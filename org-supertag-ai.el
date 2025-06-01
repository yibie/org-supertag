;;; org-supertag-ai.el --- AI workflow for org-supertag -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: yibie <yibie@outlook.com>
;; Keywords: org-mode, ai, workflow

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module implements an AI workflow engine for org-supertag based on Org Headlines.
;; Workflows are defined directly within Org files using headline properties.
;; The engine executes nodes sequentially or based on defined successor logic.
;; It integrates with org-supertag-sim-epc for LLM interactions.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element) ; Needed for property parsing
(require 'org-id)
(require 'org-supertag-behavior) ; Added require for behavior execution
(require 'org-supertag-sim-epc) ; Added require for LLM/EPC interaction

(defgroup org-supertag-ai nil
  "Customization options for org-supertag AI capabilities."
  :group 'org-supertag)

(defcustom org-supertag-ai-data-dir
  (expand-file-name "ai-data" user-emacs-directory)
  "Directory for storing AI workflow data (if needed)."
  :type 'directory
  :group 'org-supertag-ai)

;; --- Core Property Keywords ---
;; Define constants for standard property keys used in AI workflow nodes.
(defconst org-supertag-ai--prop-node-type :NODE_TYPE
  "Property key for the type of the workflow node (e.g., llm, function, behavior).")

(defconst org-supertag-ai--prop-successors :AI_SUCCESSORS
  "Property key for defining the next node(s) and transition conditions.")

(defconst org-supertag-ai--prop-prompt :AI_PROMPT
  "Property key for the prompt template (used by LLM nodes).")

(defconst org-supertag-ai--prop-model :AI_MODEL
  "Property key for specifying the LLM model.")

(defconst org-supertag-ai--prop-function :AI_FUNCTION
  "Property key for the Elisp function to call (used by function nodes).")

(defconst org-supertag-ai--prop-behavior-name :AI_BEHAVIOR_NAME
  "Property key for the behavior name to execute (used by behavior nodes).")

(defconst org-supertag-ai--prop-behavior-params :AI_BEHAVIOR_PARAMS
  "Property key for the parameters passed to a behavior.")

(defconst org-supertag-ai--prop-context-sources :AI_CONTEXT_SOURCES
  "Property key specifying where to fetch context from (previous nodes/blocks).")

(defconst org-supertag-ai--prop-output-block :AI_OUTPUT_BLOCK
  "Property key for the name of the block where the node's main output is stored.")

(defconst org-supertag-ai--prop-thoughts-block :AI_THOUGHTS_BLOCK
  "Property key for the name of the block storing intermediate thoughts or logs.")

(defconst org-supertag-ai--prop-system-prompt :AI_SYSTEM_PROMPT
  "Property key for the system prompt template (used by LLM nodes).")

;; New properties for INPUT, OUTPUT, CONDITIONAL nodes
(defconst org-supertag-ai--prop-context-key :AI_CONTEXT_KEY
  "Property key specifying the key to store the result under in the shared context (e.g., for INPUT nodes).")

(defconst org-supertag-ai--prop-template :AI_TEMPLATE
  "Property key for a template string to be rendered (e.g., for OUTPUT nodes).")

(defconst org-supertag-ai--prop-output-target :AI_OUTPUT_TARGET
  "Property key specifying where to display output (e.g., 'message', 'buffer'). Currently only 'message' supported.")

(defconst org-supertag-ai--prop-conditions :AI_CONDITIONS
  "Property key containing an Elisp expression string to evaluate for determining the action (for CONDITIONAL nodes).")

;; 4. Key Implementation Components (as per proposal)

(defun org-supertag-ai-workflow-get-node-properties (node-element-marker-pair buffer)
  "Parse and return the properties of a node identified by NODE-ELEMENT-MARKER-PAIR.
NODE-ELEMENT-MARKER-PAIR is a cons cell `(element . marker)`.
BUFFER is the buffer containing the node.

Uses `org-entry-properties` which relies on point position, hence the marker.
Returns a plist of properties or nil."
  (let* ((element (car node-element-marker-pair))
         (marker (cdr node-element-marker-pair))
         (properties nil))
    ;; Ensure we have a valid element and marker
    (unless (and element marker buffer)
      (message "Warning: Invalid input to get_node_properties: Element=%S, Marker=%S, Buffer=%S" element marker buffer)
      (cl-return-from org-supertag-ai-workflow-get-node-properties nil))

    ;; Ensure the element is a headline type just in case
    (unless (eq (org-element-type element) 'headline)
      (message "Warning: Element passed to get_node_properties is not a headline: %S" element)
      (cl-return-from org-supertag-ai-workflow-get-node-properties nil))

    (with-current-buffer buffer
      (save-excursion
        (goto-char marker) ; Go to the headline's location
        (when (org-at-heading-p)
          ;; org-entry-properties returns an alist, convert to plist
          (let ((props-alist (org-entry-properties)))
             (setq properties (cl-loop for (key . value) in props-alist
                                       collect (intern (concat ":" key)) ; Convert key string to keyword symbol
                                       collect value))))))
    properties))

;; --- Helper Functions ---

(defun org-supertag-ai--parse-string-property (prop-value-string)
  "Safely parse PROP-VALUE-STRING into an Elisp object.
Uses `read-from-string` to parse the string, intended for properties
like :AI_SUCCESSORS: or :AI_CONTEXT_SOURCES: that store lists as strings.
Returns the parsed Elisp object (e.g., a list) or nil if the string
is empty, nil, or parsing fails."
  (when (and prop-value-string (stringp prop-value-string) (not (string-empty-p prop-value-string)))
    (condition-case err
        (let ((read-object (read-from-string prop-value-string)))
          ;; read-from-string returns (object . position)
          (car read-object))
      (error
       (message "Error parsing string property '%s': %s" prop-value-string err)
       nil)))) ; Return nil on parsing error

(defun org-supertag-ai--get-block-content (node-marker buffer block-name)
  "Find a special block named BLOCK-NAME at NODE-MARKER in BUFFER and return its content using text search."
  (unless (markerp node-marker)
    (error "Invalid NODE-MARKER passed to get-block-content"))
  (unless buffer
    (error "BUFFER is required for get-block-content"))

  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (goto-char node-marker)
        (message "DEBUG GET_BLOCK_CONTENT (Text Search): Marker position: %d" (point)) ; Debug marker pos

        (when (org-at-heading-p)
          (let* ((headline-element (org-element-at-point))
                 (_ (message "DEBUG GET_BLOCK_CONTENT (Text Search): Got element type: %s, Title: %s"
                             (org-element-type headline-element)
                             (org-element-property :raw-value headline-element))) ; Debug element fetch
                 (contents-begin (org-element-property :contents-begin headline-element))
                 (contents-end (org-element-property :contents-end headline-element))
                 (region-start (or contents-begin (save-excursion (goto-char node-marker) (line-end-position))))
                 (region-end (or contents-end
                                 (save-excursion
                                   (goto-char region-start)
                                   (if (outline-next-heading) (point) (point-max)))))
                 (block-type-upcase (upcase block-name))
                 (block-begin-re (format "^[ \t]*#\\+BEGIN_%s[ \t]+%s.*?\n"
                                         block-type-upcase (regexp-quote block-name)))
                 (block-end-re (format "^[ \t]*#\\+END_%s" block-type-upcase))
                 (found-content nil)
                 (search-result-begin nil) ; Variable to store result of first search
                 (search-result-end nil)) ; Variable to store result of second search

            (message "DEBUG GET_BLOCK_CONTENT (Text Search): Calculated Search Region: %d - %d" region-start region-end)

            (narrow-to-region region-start region-end)
            (goto-char (point-min)) ; Start search from beginning of narrowed region

            ;; --- DEBUG: Search for BEGIN ---
            (setq search-result-begin (re-search-forward block-begin-re nil t))
            (message "DEBUG GET_BLOCK_CONTENT (Text Search): Result of searching for BEGIN line (re: %S): %S" block-begin-re search-result-begin)
            ;; --- END DEBUG ---

            (when search-result-begin
              (let ((content-start-pos (match-end 0)))
                 (message "DEBUG GET_BLOCK_CONTENT (Text Search): Found BEGIN at %d. Content starts at %d" (match-beginning 0) content-start-pos)

                 ;; --- DEBUG: Search for END ---
                 (setq search-result-end (re-search-forward block-end-re nil t))
                 (message "DEBUG GET_BLOCK_CONTENT (Text Search): Result of searching for END line (re: %S): %S" block-end-re search-result-end)
                 ;; --- END DEBUG ---

                 (when search-result-end
                   (let ((content-end-pos (match-beginning 0)))
                     (message "DEBUG GET_BLOCK_CONTENT (Text Search): Found END at %d. Content ends at %d" (point) content-end-pos)
                     (setq found-content (buffer-substring-no-properties content-start-pos content-end-pos))
                     (when (string-match "\\`\\(.*\\)\\(\n\\)?\\'" found-content)
                       (setq found-content (match-string 1 found-content)))
                     (message "DEBUG GET_BLOCK_CONTENT (Text Search): Extracted content length: %d" (length found-content)))))
              ) ; end let content-start-pos
            (widen)

            (message "DEBUG GET_BLOCK_CONTENT (Text Search): Returning content: %S" (if found-content "Found" "Not Found"))
            found-content))))))

;; --- Stubs for other core functions (to be implemented) ---

(defun org-supertag-ai-workflow-find-node (node-identifier &optional buffer)
  "Find the Org headline element and its marker for NODE-IDENTIFIER.
NODE-IDENTIFIER is preferably an Org ID string, but can fallback to title.
BUFFER specifies the buffer to search in (defaults to current buffer).

Returns a cons cell `(element . marker)` or nil if not found."
  (let ((buf (or buffer (current-buffer)))
        (element nil)
        (marker nil))
    (with-current-buffer buf
      (org-with-wide-buffer
       ;; 1. Try finding by Org ID first
       (condition-case nil
           (when-let* ((found-marker (org-id-find node-identifier 'marker)))
             (save-excursion ; Use save-excursion to avoid moving point permanently
               (goto-char found-marker)
               (when (org-at-heading-p)
                 (setq element (org-element-at-point))
                 (setq marker found-marker) ; Store the marker
                 (message "DEBUG FIND_NODE (ID): Found element type: %S" (org-element-type element))
                 )))
         (error nil))

       ;; 2. If not found by ID, try finding by exact title match (fallback)
       (unless element
         (save-excursion
           (goto-char (point-min))
           (while (and (not element)
                       (re-search-forward org-complex-heading-regexp nil t))
             (let* ((current-element (org-element-at-point))
                    (title (org-element-property :raw-value current-element)))
               (when (string= node-identifier title)
                 (setq element current-element)
                 (setq marker (point)) ; Use current point as marker
                 (message "DEBUG FIND_NODE (Title): Found element type: %S" (org-element-type element))
                 )))))))
    ;; Return cons cell (element . marker) or nil
    (when element (cons element marker))))

(defun org-supertag-ai-workflow-get-context (current-node-element context-sources-prop shared-context-hash buffer)
  "Get context data for the CURRENT-NODE-ELEMENT.
Reads content from blocks specified in the :AI_CONTEXT_SOURCES property
(passed as CONTEXT-SOURCES-PROP string) and merges with SHARED-CONTEXT-HASH.
Operates within the specified BUFFER."
  (let ((combined-context (if shared-context-hash
                              (copy-hash-table shared-context-hash)
                            (make-hash-table :test 'equal)))
        (context-sources (org-supertag-ai--parse-string-property context-sources-prop)))

    (when (listp context-sources)
      (dolist (source-spec context-sources)
        (when (and (consp source-spec)
                   (stringp (car source-spec))
                   (stringp (cdr source-spec)))
          (let ((source-identifier (car source-spec))
                (block-name (cdr source-spec)))
            (unless (and (stringp source-identifier) (stringp block-name))
              (message "Warning: Invalid context source spec: %S" source-spec)
              (cl-return)) ; Continue to next spec

            (if-let ((source-node-pair (org-supertag-ai-workflow-find-node source-identifier buffer)))
                (let ((source-marker (cdr source-node-pair))) ; <-- Get the marker
                  (if (markerp source-marker) ; <-- Check if marker is valid
                      ;; --- Pass marker and buffer to get-block-content ---
                      (if-let ((block-content (org-supertag-ai--get-block-content source-marker buffer block-name)))
                          (let ((context-key (format "context_%s" block-name)))
                            (puthash context-key block-content combined-context))
                        (message "Warning: Block '%s' not found in node '%s' (at marker)." block-name source-identifier))
                    (message "Warning: Invalid marker found for node '%s'." source-identifier)))
              (message "Warning: Context source node '%s' not found." source-identifier))))))

    combined-context))

(defun org-supertag-ai-workflow-write-output (node-element output-string block-name-string buffer &optional block-type-string)
  "Write OUTPUT-STRING to a named block within NODE-ELEMENT.
Operates within the specified BUFFER.
If a block with the same name exists, its entire content including
BEGIN/END markers will be replaced. Otherwise, a new block is inserted."
  (unless (eq (org-element-type node-element) 'headline)
    (error "Input NODE-ELEMENT must be of type 'headline'"))

  (let* ((buf (or buffer (current-buffer)))
         (block-type (or (and (stringp block-type-string)
                              (not (string-empty-p block-type-string))
                              block-type-string)
                         block-name-string))
         ;; Regex to find the start of the block, capturing the indent (group 1)
         ;; Removed capture group around block name as it's not needed here.
         (block-begin-re (format "^\\([ \t]*\\)#\\+BEGIN_%s[ \t]+%s"
                                 (upcase block-type) (regexp-quote block-name-string)))
         (contents-begin (org-element-property :contents-begin node-element))
         (contents-end (org-element-property :contents-end node-element))
         (found-block-begin-pos nil)
         (found-block-end-pos nil))

    (unless buf (error "Buffer argument is required for write-output"))
    (unless block-name-string (error "Block name string is required"))

    (with-current-buffer buf
      (save-excursion
        (save-restriction
          ;; Narrow to the headline's content section if it exists
          (when (and contents-begin contents-end)
            (narrow-to-region contents-begin contents-end))

          ;; Try to find existing block's BEGIN line
          (goto-char (point-min))
          (when (re-search-forward block-begin-re nil t)
            (setq found-block-begin-pos (match-beginning 0))
            ;; Capture the indent from the BEGIN line match
            (let* ((indent (match-string 1))
                   ;; Dynamically construct the END regex using the captured indent
                   (block-end-re (format "^%s#\\+END_%s"
                                         (if indent (regexp-quote indent) "") ; Quote the captured indent
                                         (upcase block-type))))
              ;; Try to find the corresponding END line after the BEGIN line
              (if (re-search-forward block-end-re nil t)
                  (setq found-block-end-pos (match-end 0))
                ;; If END marker is missing or outside region, block is considered incomplete/not found
                (message "Warning: Found BEGIN line but corresponding END line (matching indent) is missing or outside content region for node '%s'. A new block will be inserted."
                         (org-element-property :raw-value node-element))
                (setq found-block-begin-pos nil))))

          (widen) ; Widen before buffer modification

          (if found-block-begin-pos
              ;; Block found: Replace the entire block
              (progn
                (goto-char found-block-begin-pos)
                ;; Ensure end position includes potential newline
                (goto-char found-block-end-pos)
                (when (and (not (eobp)) (eq (char-after) ?\n))
                   (forward-char 1)
                   (setq found-block-end-pos (point)))

                ;; Delete the entire old block
                (delete-region found-block-begin-pos found-block-end-pos)

                ;; Insert the new block
                (goto-char found-block-begin-pos)
                 ;; Add a newline before if necessary
                (unless (or (= (point) (point-min)) (eq (char-before (point)) ?\n))
                  (insert "\n"))
                (insert (format "#+BEGIN_%s %s\n%s\n#+END_%s"
                                (upcase block-type)
                                block-name-string
                                output-string
                                (upcase block-type)))
                ;; Add a newline after if necessary
                (unless (or (eobp) (eq (char-after (point)) ?\n))
                   (insert "\n"))
                t) ; Return t on success

            ;; Block not found or incomplete: Insert new block
            (let ((insert-pos (or contents-end ; Insert at end of content
                                  (save-excursion ; Or after headline if no content
                                    (goto-char (org-element-property :begin node-element))
                                    (line-end-position)))))
              (goto-char insert-pos)
              ;; Ensure proper spacing before inserting block
              (unless (or (= insert-pos (point-min)) (looking-back "\n\n" 2))
                (insert "\n"))
              (unless (looking-back "\n" 1) (insert "\n"))
              (insert (format "#+BEGIN_%s %s\n%s\n#+END_%s\n"
                              (upcase block-type)
                              block-name-string
                              output-string
                              (upcase block-type)))
              t))))))) ;; end save-excursion, with-current-buffer

(defun org-supertag-ai--render-template (template-string context-hash)
  "Render TEMPLATE-STRING using variables from CONTEXT-HASH.
Replaces occurrences of '{{variable_name}}' with the corresponding
value found in CONTEXT-HASH. Keys in CONTEXT-HASH should be strings
matching 'variable_name'. If a variable is not found in the hash,
it's replaced with an empty string.

TEMPLATE-STRING: The string containing placeholders.
CONTEXT-HASH: A hash table where keys are variable names (strings)
              and values are the replacement strings."
  (if (not context-hash)
      template-string ; Return original if no context hash
    (let ((result template-string))
      ;; Iterate through the template string to find placeholders
      (save-match-data
        (while (string-match "{{\\([^{}[:space:]]+\\)}}" result)
          (let* ((placeholder (match-string 0 result))
                 (var-name (match-string 1 result))
                 ;; Get value from hash, default to empty string if not found or nil
                 (value (let ((raw-val (gethash var-name context-hash)))
                          (if raw-val (format "%s" raw-val) ""))))
            (setq result (replace-match value t t result)))))
      result)))

;; --- LLM Interaction Wrapper ---

(defun org-supertag-ai--invoke-llm-sync (prompt &optional system-prompt model)
  "Synchronously invoke the LLM via the EPC bridge.
Assumes the EPC server is running AND initialized by `org-supertag--enable`.
Calls the 'run_ollama' method on the Python side using `epc:call-sync`,
passing the model hint.

PROMPT: The main user prompt string.
SYSTEM-PROMPT: Optional system prompt string.
MODEL: Optional model name string. If non-nil, this will be passed
       to the Python backend to override the default model for this call.

Returns the AI response string on success.
Signals an error on failure (EPC error, AI error, server not running)."
  (org-supertag-sim-epc-log "Invoking LLM synchronously (Prompt: %s..., SystemPrompt: %s, Model: %s)..."
                           (truncate-string-to-width prompt 50)
                           (if system-prompt "yes" "no")
                           (or model "Default"))

  ;; 1. Ensure system is properly initialized
  (deferred:sync!
    (org-supertag-sim--ensure-initialized))

  ;; 2. Clean prompt and system prompt values (remove quotes)
  (let* ((clean-prompt (replace-regexp-in-string "\"" "" prompt))
         (clean-system-prompt (when system-prompt (replace-regexp-in-string "\"" "" system-prompt))))

    ;; 3. If model is provided, set it as default model before calling Ollama
    (when model
      (let ((clean-model (replace-regexp-in-string "\"" "" model)))
        (message "Setting model to: %s" clean-model)
        ;; Call set_default_model if available, otherwise log a message
        (if (org-supertag-sim-epc-verify-ollama-model clean-model)
            (message "Using model: %s" clean-model)
          (message "Warning: Model %s not verified, using default model" clean-model))))
    
    ;; 4. Prepare arguments for EPC call (only prompt and system)
    (let ((args (list clean-prompt clean-system-prompt)))
      (org-supertag-sim-epc-log "Calling EPC method 'run_ollama' with args: %S" args)

      (condition-case err
          (let ((response (epc:call-sync org-supertag-sim-epc-manager 'run_ollama args)))
            (org-supertag-sim-epc-log "Raw EPC response: %S" response)

            ;; 5. Process Response
            (if (and response (listp response) (plistp response))
                (let ((status (plist-get response :status))
                      (result (plist-get response :result))
                      (message (plist-get response :message)))
                  (if (string= status "success")
                      (progn
                        (org-supertag-sim-epc-log "LLM call successful.")
                        result)
                    ;; Check for specific error types and handle accordingly
                    (cond
                     ;; Model not found error
                     ((string-match-p "model.*not found" (or message ""))
                      (let ((fallback-models '("gemma:2b" "llama2" "mistral")))
                        (catch 'found-model
                          (dolist (fallback-model fallback-models)
                            (when (org-supertag-sim-epc-verify-ollama-model fallback-model)
                              (message "Original model not found, using fallback model: %s" fallback-model)
                              (throw 'found-model
                                     (org-supertag-ai--invoke-llm-sync prompt system-prompt fallback-model)))))))
                     ;; Service not initialized error
                     ((string-match-p "not initialized" (or message ""))
                      (message "Service not initialized, attempting to reinitialize...")
                      (org-supertag-sim-init)
                      (org-supertag-ai--invoke-llm-sync prompt system-prompt model))
                     ;; Other errors
                     (t
                      (error "LLM Error: %s" (or message "Unknown error"))))))
              (error "Invalid response format from EPC: %S" response)))
        ;; Handle EPC communication errors
        (error
         (error "EPC communication error: %s" (error-message-string err)))))))


;; --- Internal Node Type Executors (Implementations/Refined Stubs) ---

(defun org-supertag-ai--execute-llm-node (node-element context properties)
  "Execute an LLM node.
Renders prompt, calls LLM via EPC, returns action, result, and thoughts.
Returns: (list ACTION RESULT-STRING THOUGHTS-STRING)."
  (message "Executing LLM node: %s" (org-element-property :raw-value node-element))
  (let* ((prompt-template (plist-get properties org-supertag-ai--prop-prompt))
         (model (plist-get properties org-supertag-ai--prop-model))
         (system-prompt-template (plist-get properties org-supertag-ai--prop-system-prompt))
         (rendered-prompt (when prompt-template (org-supertag-ai--render-template prompt-template context)))
         (rendered-system-prompt (when system-prompt-template (org-supertag-ai--render-template system-prompt-template context)))
         (thoughts (format "--- LLM Node Execution Thoughts ---\nModel Hint: %s\nSystem Prompt Rendered:\n%s\n---\nUser Prompt Rendered:\n%s\n---"
                           (or model "Default")
                           (or rendered-system-prompt "N/A")
                           (or rendered-prompt "ERROR: Prompt template missing or empty!"))))

    (if rendered-prompt
        (condition-case err ;; This catches errors from invoke-llm-sync
            (let* ((llm-result (org-supertag-ai--invoke-llm-sync rendered-prompt rendered-system-prompt model))
                   (final-thoughts (concat thoughts (format "\nLLM Result:\n%s\n---" llm-result))))
              (list "default" llm-result final-thoughts))
          (error ;; Handle invoke-llm-sync error
           (let* ((llm-exec-error-msg (format "LLM execution failed: %s" (error-message-string err))) ; Renamed var
                  (final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" llm-exec-error-msg))))
             (message llm-exec-error-msg)
             (list "error" llm-exec-error-msg final-thoughts))))
      (let* ((prompt-error-msg (format "LLM node failed: Prompt template %s is missing or empty." org-supertag-ai--prop-prompt)) ; Renamed var
             (final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" prompt-error-msg))))
        (message prompt-error-msg)
        (list "error" prompt-error-msg final-thoughts)))))


(defun org-supertag-ai--execute-function-node (node-element context properties)
  "Execute a function node.
Calls Elisp function, returns action, result, and thoughts.
Returns: (list ACTION RESULT THOUGHTS-STRING)."
  (let* ((func-name-str (plist-get properties org-supertag-ai--prop-function))
         (node-title (org-element-property :raw-value node-element))
         (thoughts (format "--- Function Node Execution Thoughts ---\nNode: %s\nFunction Name Prop: %s"
                           node-title (or func-name-str "MISSING")))
         (action "error")
         (result nil)
         (final-thoughts thoughts))

    (if (and func-name-str (stringp func-name-str) (not (string-empty-p func-name-str)))
        (let ((func-sym (intern func-name-str)))
          (if (fboundp func-sym)
              (progn
                (setq thoughts (concat thoughts (format "\nFunction Symbol: '%s'" func-sym)))
                (message "Executing function '%s' for node '%s'" func-sym node-title)
                (condition-case err ;; Catches errors during funcall
                    (let* ((func-result-pair (funcall func-sym node-element context))
                          (func-action (car func-result-pair))
                          (func-result (cdr func-result-pair)))
                       (setq action func-action)
                       (setq result func-result)
                       (setq final-thoughts (concat thoughts (format "\nFunction Returned: (%s . %S)\n---" func-action func-result)))
                       (message "Function '%s' returned action: %s" func-sym func-action))
                  (error ;; Handle funcall error
                   (let ((func-call-error-msg (format "Error executing function %s: %s" func-sym (error-message-string err)))) ; Renamed var
                     (message func-call-error-msg)
                     (setq result func-call-error-msg) ; Store error message as result
                     (setq final-thoughts (concat thoughts (format "\nERROR executing function:\n%s\n---" func-call-error-msg)))))))
            (let ((fboundp-error-msg (format "Function '%s' not defined for node '%s'" func-sym node-title))) ; Renamed var
              (message fboundp-error-msg)
              (setq result fboundp-error-msg)
              (setq final-thoughts (concat thoughts (format "\nERROR:\nFunction not defined: %s\n---" func-sym))))))
      (let ((prop-error-msg (format ":AI_FUNCTION: property missing or empty for node '%s'" node-title))) ; Renamed var
        (message prop-error-msg)
        (setq result prop-error-msg)
        (setq final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" prop-error-msg)))))
    (list action result final-thoughts)))


(defun org-supertag-ai--execute-behavior-node (node-element context properties)
  "Execute a behavior node.
Calls behavior, returns action, result (nil), and thoughts.
Returns: (list ACTION RESULT THOUGHTS-STRING)."
  (let* ((behavior-name (plist-get properties org-supertag-ai--prop-behavior-name))
         (behavior-params-str (plist-get properties org-supertag-ai--prop-behavior-params))
         (node-title (org-element-property :raw-value node-element))
         (node-id (org-element-property :ID node-element))
         (thoughts (format "--- Behavior Node Execution Thoughts ---\nNode: %s (ID: %s)\nBehavior Name Prop: %s\nBehavior Params Prop: %s"
                           node-title (or node-id "MISSING ID")
                           (or behavior-name "MISSING") (or behavior-params-str "N/A")))
         (action "error")
         (result nil)
         (final-thoughts thoughts))

    (unless node-id
      (let ((id-error-msg (format "Behavior node '%s' is missing an :ID: property." node-title))) ; Renamed var
       (message id-error-msg)
       (setq final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" id-error-msg)))
       (cl-return-from org-supertag-ai--execute-behavior-node (list action result final-thoughts)))) ; Return using renamed var

    (if (and behavior-name (stringp behavior-name) (not (string-empty-p behavior-name)))
        (if (fboundp 'org-supertag-behavior-execute)
            (progn
              (message "Executing behavior '%s' for node '%s' (ID: %s) with params: %s"
                       behavior-name node-title node-id behavior-params-str)
              (condition-case err ;; Catches errors during behavior-execute
                  (progn
                     (org-supertag-behavior-execute node-id behavior-name behavior-params-str)
                     (setq action "default")
                     (setq final-thoughts (concat thoughts "\nExecution: Succeeded (assumed)\n---")))
                (error ;; Handle behavior execution error
                 (let ((behavior-exec-error-msg (format "Error executing behavior %s: %s" behavior-name (error-message-string err)))) ; Renamed var
                   (message behavior-exec-error-msg)
                   (setq result behavior-exec-error-msg)
                   (setq final-thoughts (concat thoughts (format "\nERROR executing behavior:\n%s\n---" behavior-exec-error-msg)))))))
          (let ((missing-func-error-msg "Function 'org-supertag-behavior-execute' not found. Is org-supertag-behavior loaded?")) ; Renamed var
            (message missing-func-error-msg)
            (setq result missing-func-error-msg)
            (setq final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" missing-func-error-msg)))))
      (let ((prop-error-msg (format ":AI_BEHAVIOR_NAME: property missing or empty for node '%s'" node-title))) ; Renamed var
        (message prop-error-msg)
        (setq result prop-error-msg)
        (setq final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" prop-error-msg)))))
    (list action result final-thoughts)))


(defun org-supertag-ai--execute-input-node (node-element context properties)
  "Execute an INPUT node.
Prompts the user for input using :AI_PROMPT:.
Returns: (list \"default\" USER-INPUT THOUGHTS-STRING)."
  (let* ((node-title (org-element-property :raw-value node-element))
         (prompt-string (plist-get properties org-supertag-ai--prop-prompt))
         (context-key (plist-get properties org-supertag-ai--prop-context-key))
         (thoughts (format "--- Input Node Execution Thoughts ---\nNode: %s\nPrompt Prop: %s\nContext Key Prop: %s"
                           node-title
                           (or prompt-string "MISSING :AI_PROMPT:")
                           (or context-key "MISSING :AI_CONTEXT_KEY: (will use :last_result only)")))
         (user-input nil)
         (final-thoughts thoughts))

    ;; Condition-case around the interactive part
    (condition-case err
        (progn
          (unless prompt-string
            (let ((prompt-missing-error-msg (format "INPUT node '%s' is missing :AI_PROMPT: property." node-title))) ; Renamed var
              (message prompt-missing-error-msg)
              (setq final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" prompt-missing-error-msg)))
              ;; Use error to signal failure to the outer condition-case if needed,
              ;; or return the error list directly. Returning seems safer here.
              (cl-return-from org-supertag-ai--execute-input-node (list "error" prompt-missing-error-msg final-thoughts))))

          ;; Use read-string to get input
          (setq user-input (read-string (format "%s: " prompt-string)))
          (setq final-thoughts (concat thoughts (format "\nUser Input Received: %s\n---" user-input)))
          ;; Return normally
          (list "default" user-input final-thoughts))
      ;; Catch potential errors from read-string or other issues
      (error
       (let ((input-error-msg (format "Error during INPUT node execution for '%s': %s" node-title (error-message-string err)))) ; Renamed var
         (message input-error-msg)
         (setq final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" input-error-msg)))
         ;; Return error list
         (list "error" input-error-msg final-thoughts))))))


(defun org-supertag-ai--execute-output-node (node-element context properties)
  "Execute an OUTPUT node.
Renders :AI_TEMPLATE: using context and displays it.
Returns: (list \"default\" RENDERED-OUTPUT THOUGHTS-STRING)."
  (let* ((node-title (org-element-property :raw-value node-element))
         (template-string (plist-get properties org-supertag-ai--prop-template))
         (output-target (or (plist-get properties org-supertag-ai--prop-output-target) "message"))
         (thoughts (format "--- Output Node Execution Thoughts ---\nNode: %s\nTemplate Prop: %s\nOutput Target Prop: %s"
                           node-title
                           (or template-string "MISSING :AI_TEMPLATE:")
                           output-target))
         (rendered-output nil)
         (final-thoughts thoughts))

    (unless template-string
      (let ((template-error-msg (format "OUTPUT node '%s' is missing :AI_TEMPLATE: property." node-title))) ; Renamed var
        (message template-error-msg)
        (setq final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" template-error-msg)))
        (cl-return-from org-supertag-ai--execute-output-node (list "error" template-error-msg final-thoughts))))

    (condition-case err ;; Catch errors during rendering or display
        (progn
          (setq rendered-output (org-supertag-ai--render-template template-string context))
          (setq final-thoughts (concat thoughts (format "\nRendered Output:\n%s\n---" rendered-output)))
          (cond
           ((string= output-target "message") (message "%s" rendered-output))
           (t (message "Warning: Unsupported output target '%s' for node '%s'. Outputting to messages." output-target node-title)
              (message "%s" rendered-output)))
          ;; Return normally
          (list "default" rendered-output final-thoughts))
      (error ;; Handle rendering/display error
       (let ((output-error-msg (format "Error during OUTPUT node execution for '%s': %s" node-title (error-message-string err)))) ; Renamed var
         (message output-error-msg)
         (setq final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" output-error-msg)))
         (list "error" output-error-msg final-thoughts))))))


(defun org-supertag-ai--execute-conditional-node (node-element context properties)
  "Execute a CONDITIONAL node.
Evaluates :AI_CONDITIONS: expression to determine the action using `funcall`.
Returns: (list EVALUATED-ACTION nil THOUGHTS-STRING)."
  (let* ((node-title (org-element-property :raw-value node-element))
         ;; Get the raw string property, e.g., "\" (if ...) \""
         (conditions-string-raw (plist-get properties org-supertag-ai--prop-conditions))
         (thoughts (format "--- Conditional Node Execution Thoughts ---\nNode: %s\nConditions Prop Raw: %s"
                           node-title
                           (or conditions-string-raw "MISSING :AI_CONDITIONS:")))
         (action "error")
         (result nil) ; Result is typically nil for conditional nodes
         (final-thoughts thoughts))

    (unless conditions-string-raw
      (let ((condition-missing-error-msg (format "CONDITIONAL node '%s' is missing :AI_CONDITIONS: property." node-title)))
        (message condition-missing-error-msg)
        (setq final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" condition-missing-error-msg)))
        (cl-return-from org-supertag-ai--execute-conditional-node (list action nil final-thoughts))))

    ;; --- DEBUG: Print context received by the function ---
    (message "DEBUG CONDITIONAL: Context received: %S" context)
    ;; --- END DEBUG ---

    (condition-case err ;; Catch errors during read or funcall
        (let* (;; First read: Convert raw property string "\" (if ...) \"" into the Lisp string "(if ...)"
               (conditions-inner-string (read conditions-string-raw))
               ;; Second read: Convert the Lisp string "(if ...)" into the Lisp list '(if ...)
               (condition-expr (read conditions-inner-string))
               ;; Construct the lambda function dynamically using the Lisp list
               (lambda-func (list 'lambda '(context) condition-expr)))

          ;; --- DEBUG: Show intermediate steps ---
          (message "DEBUG CONDITIONAL: Conditions inner string: %S" conditions-inner-string)
          (message "DEBUG CONDITIONAL: Conditions expr (list): %S" condition-expr)
          (message "DEBUG CONDITIONAL: Constructed lambda: %S" lambda-func)
          ;; --- END DEBUG ---

          ;; Call the lambda function with the current context
          (let ((call-result (funcall lambda-func context)))
            (message "DEBUG CONDITIONAL: Raw funcall result: %S" call-result) ;; <-- Debug funcall result

             (if (stringp call-result)
                 (progn
                   (setq action call-result)
                   (setq final-thoughts (concat thoughts (format "\nEvaluation Result (Action): %s\n---" action))))
               (let ((eval-type-error-msg (format "Evaluation of :AI_CONDITIONS: (via funcall) did not return a string: %S (Type: %s)"
                                                   call-result (type-of call-result))))
                 (message eval-type-error-msg)
                 (setq final-thoughts (concat thoughts (format "\nERROR:\n%s\n---" eval-type-error-msg)))
                 ;; Keep action as "error"
                 ))))
      (error ;; Handle read/funcall error
       (let ((eval-error-msg (format "Error evaluating :AI_CONDITIONS: (via funcall) for node '%s': %s" node-title (error-message-string err))))
         (message eval-error-msg)
         (setq final-thoughts (concat thoughts (format "\nERROR evaluating conditions:\n%s\n---" eval-error-msg)))
         ;; Keep action as "error"
         )))

    ;; Return action, nil result, and thoughts
    (list action result final-thoughts)))


(defun org-supertag-ai-workflow-execute-node (node-element-marker-pair shared-context-hash buffer)
  "Execute a single workflow node identified by NODE-ELEMENT-MARKER-PAIR in BUFFER."
  (let ((node-element (car node-element-marker-pair))) ; Extract element for checks and messages
    ;; --- DEBUG MESSAGE 1 ---
    (message "DEBUG EXECUTE_NODE: Received element of type: %S" (org-element-type node-element))
    ;; --- END DEBUG ---
    (unless (eq (org-element-type node-element) 'headline)
      (error "Input NODE-ELEMENT must be of type 'headline'"))
    (unless buffer (error "BUFFER argument is required for execute-node"))
    (unless node-element-marker-pair (error "NODE-ELEMENT-MARKER-PAIR is required"))

    (let* (;; Use the new function to get properties, passing the pair and buffer
           (properties (org-supertag-ai-workflow-get-node-properties node-element-marker-pair buffer))
           ;; --- DEBUG MESSAGE 2 ---
           (_ (message "DEBUG EXECUTE_NODE: Properties for node '%s' (using marker): %S"
                       (org-element-property :raw-value node-element) ; Still use element for title
                       properties))
           ;; --- END DEBUG ---
           (node-type-str (plist-get properties org-supertag-ai--prop-node-type))
           (context-sources-str (plist-get properties org-supertag-ai--prop-context-sources))
           ;; Pass element to get-context, it doesn't need the marker
           (current-context (org-supertag-ai-workflow-get-context
                             node-element context-sources-str shared-context-hash buffer))
           (node-type (when node-type-str (intern (upcase node-type-str))))
           ;; Get the raw context key string (e.g., "\"user_age\"")
           (context-key-raw (plist-get properties org-supertag-ai--prop-context-key))
           ;; Parse the raw key string to get the actual key (e.g., "user_age")
           (context-key (when (and context-key-raw (stringp context-key-raw))
                          (condition-case inner-err
                              (read context-key-raw) ; Read "\"user_age\"" -> "user_age"
                            (error
                             (message "Warning: Could not read context key string: %s. Error: %s" context-key-raw (error-message-string inner-err))
                             nil))))
           ;; --- DEBUG: Show raw and parsed context key ---
           (_ (when context-key-raw (message "DEBUG EXECUTE_NODE: Raw context key: %S, Parsed context key: %S" context-key-raw context-key)))
           ;; --- END DEBUG ---
           (exec-result-list
            (pcase node-type
              ;; Pass element, context, and properties to executors
              ('LLM (org-supertag-ai--execute-llm-node node-element current-context properties))
              ('FUNCTION (org-supertag-ai--execute-function-node node-element current-context properties))
              ('BEHAVIOR (org-supertag-ai--execute-behavior-node node-element current-context properties))
              ('INPUT (org-supertag-ai--execute-input-node node-element current-context properties))
              ('OUTPUT (org-supertag-ai--execute-output-node node-element current-context properties))
              ('CONDITIONAL (org-supertag-ai--execute-conditional-node node-element current-context properties))
              (_ (let* ((unknown-type-error-msg (format "Unknown or missing node type '%s' for node %s"
                                                        node-type-str (org-element-property :raw-value node-element)))
                        (thoughts (format "--- Error Node Execution Thoughts ---\n%s\n---" unknown-type-error-msg)))
                           (message unknown-type-error-msg)
                           (list "error" unknown-type-error-msg thoughts)))))
           (action (nth 0 exec-result-list))
           (result (nth 1 exec-result-list))
           (thoughts-string (nth 2 exec-result-list))
           (output-block-name (plist-get properties org-supertag-ai--prop-output-block))
           (thoughts-block-name (plist-get properties org-supertag-ai--prop-thoughts-block))
           ;; Use the *parsed* context-key below
           ;; (context-key is already defined above)
           (updated-shared-context (if shared-context-hash
                                       (copy-hash-table shared-context-hash)
                                     (make-hash-table :test 'equal))))

      ;; Pass element to write-output, it doesn't need the marker
      (when (and output-block-name result (stringp result))
        (org-supertag-ai-workflow-write-output node-element result output-block-name buffer))
      (when (and thoughts-block-name thoughts-string (stringp thoughts-string) (not (string-empty-p thoughts-string)))
        (org-supertag-ai-workflow-write-output node-element thoughts-string thoughts-block-name buffer))

      ;; Update shared context (using element where needed)
      (puthash "last_result" result updated-shared-context)
      ;; Use the PARSED context-key here
      (when (and context-key (stringp context-key) (not (string-empty-p context-key)))
         (puthash context-key result updated-shared-context))
      (puthash "last_action" action updated-shared-context)
      (puthash "current_node_title" (org-element-property :raw-value node-element) updated-shared-context)
      ;; Get ID from reliable properties plist now
      (puthash "current_node_id" (plist-get properties :ID) updated-shared-context)

      (cons action updated-shared-context))))

(defun org-supertag-ai-workflow-run (start-node-identifier &optional initial-context buffer)
  "Run the AI workflow starting from START-NODE-IDENTIFIER.

INITIAL-CONTEXT: An optional hash-table for initial shared state.
BUFFER: The buffer containing the workflow definition (defaults to current).

When called interactively, prompts for the START-NODE-IDENTIFIER.

Returns the final shared-context hash-table after the workflow completes or stops."
  (interactive "sStart Node ID or Title: ")
  
  ;; Ensure EPC service is properly initialized
  (when (featurep 'org-supertag-sim-epc)
    (unless (org-supertag-sim-epc-server-running-p)
      (org-supertag-sim-epc-start-server)
      (sit-for 1))
    (when (org-supertag-sim-epc-server-running-p)
      (condition-case err
          (progn
            (org-supertag-sim-epc-init)
            (message "EPC service initialized successfully"))
        (error
         (message "Warning: Failed to initialize EPC service: %s" (error-message-string err))))))

  (let* ((buf (or buffer (current-buffer)))
         (shared-context (if initial-context
                             (copy-hash-table initial-context)
                           (make-hash-table :test 'equal)))
         (current-node-pair (org-supertag-ai-workflow-find-node start-node-identifier buf))
         (max-steps 100)
         (steps 0))

    (unless current-node-pair
      (error "Workflow start node '%s' not found in buffer %s" start-node-identifier (buffer-name buf)))

    (message "DEBUG RUN: Found start node pair. Element Type: %S" (org-element-type (car current-node-pair)))
    (message "Starting AI Workflow from node: %s in buffer %s" start-node-identifier (buffer-name buf))

    (while (and current-node-pair (< steps max-steps))
      (setq steps (1+ steps))
      (message "Executing step %d: Node '%s'"
               steps (org-element-property :raw-value (car current-node-pair)))

      (let* ((result-pair (org-supertag-ai-workflow-execute-node current-node-pair shared-context buf))
             (action (car result-pair))
             (next-node-id nil)
             (next-node-pair nil))

        (setq shared-context (cdr result-pair))

        (if (string= action "error")
            (progn
              (message "Error occurred during node execution. Stopping workflow.")
              (setq current-node-pair nil)) ; Stop loop

          ;; --- Handle successor finding logic ---
          (let* ((properties (org-supertag-ai-workflow-get-node-properties current-node-pair buf))
                 (successors-str (plist-get properties org-supertag-ai--prop-successors))
                 (successors (org-supertag-ai--parse-string-property successors-str)))

            (message "DEBUG RUN LOOP (Successor Logic): Parsed successors value: %S, Type: %s" successors (type-of successors))
            (message "DEBUG RUN LOOP (Successor Logic): Checking if successors is listp: %S" (listp successors))

            ;; 1. Find next node ID from successors
            (when (listp successors)
              (let ((found-pair nil))
                ;; Try specific action
                (message "DEBUG RUN LOOP (Successor Logic): Trying cl-assoc with action='%s', successors=%S" action successors)
                (condition-case inner-err1
                    (setq found-pair (cl-assoc action successors :test #'string=)) 
                  (error (message "ERROR in cl-assoc (action '%s'): %s" action (error-message-string inner-err1))))
                (message "DEBUG RUN LOOP (Successor Logic): Result for action '%s': %S" action found-pair)
                (when found-pair (setq next-node-id (cdr found-pair)))
                ;; Try "default"
                (unless next-node-id 
                  (message "DEBUG RUN LOOP (Successor Logic): Trying cl-assoc with key='default', successors=%S" successors) 
                  (condition-case inner-err2
                      (setq found-pair (cl-assoc "default" successors :test #'string=)) 
                    (error (message "ERROR in cl-assoc (key 'default'): %s" (error-message-string inner-err2)))) 
                  (message "DEBUG RUN LOOP (Successor Logic): Result for 'default': %S" found-pair) 
                  (when found-pair (setq next-node-id (cdr found-pair))))
                ;; Try "*"
                (unless next-node-id 
                  (message "DEBUG RUN LOOP (Successor Logic): Trying cl-assoc with key='*', successors=%S" successors) 
                  (condition-case inner-err3
                      (setq found-pair (cl-assoc "*" successors :test #'string=)) 
                    (error (message "ERROR in cl-assoc (key '*'): %s" (error-message-string inner-err3)))) 
                  (message "DEBUG RUN LOOP (Successor Logic): Result for '*': %S" found-pair) 
                  (when found-pair (setq next-node-id (cdr found-pair))))
                )) ; End inner let

            ;; 2. Find the next node pair using the ID
            (if next-node-id
                (progn
                  (setq next-node-pair (org-supertag-ai-workflow-find-node next-node-id buf))
                  (unless next-node-pair (message "Error: Successor node with ID '%s' not found. Stopping workflow." next-node-id)))
              ;; 3. Fallback: Try finding next sibling
              (progn
                 (message "Info: No successor defined for action '%s'. Trying next sibling." action)
                 ;; Use org-element-properties to get the next sibling
                 (let ((next-sibling-element (org-element-property :sibling (car current-node-pair))))
                   (when (and next-sibling-element (eq (org-element-type next-sibling-element) 'headline))
                     (let ((sibling-begin (org-element-property :begin next-sibling-element)))
                       (when sibling-begin
                         (setq next-node-pair (cons next-sibling-element sibling-begin)))))))
                 (unless next-node-pair (message "Info: No suitable next sibling found or defined successor. Workflow ends.")))

            ;; Update current node pair for the next iteration
            (setq current-node-pair next-node-pair)
            (when next-node-pair (message "DEBUG RUN: Found next node pair. Element Type: %S" (org-element-type (car next-node-pair)))))
          ))) ; End if/else for action error

    ;; End of while loop

    (message "DEBUG RUN: About to check steps. Value: %S" steps)

    (when (>= steps max-steps)
      (message "Workflow stopped: Maximum execution steps (%d) reached." max-steps))

    (message "AI Workflow finished in buffer %s." (buffer-name buf))
    shared-context))


;; --- Workflow Design Helpers ---

(defun org-supertag-ai--fetch-available-models ()
  "Fetch the list of available Ollama models via EPC.
Returns a list of model name strings on success, or signals an error
if the EPC server is unreachable or the backend fails to get models."
  (org-supertag-sim-epc-log "Fetching available Ollama models via EPC...")
  ;; 1. Ensure EPC Server is ready
  (condition-case err
      (progn
        (unless (org-supertag-sim-epc-server-running-p)
          (error "SimTag EPC server is not running or failed to start.")))
    (error (error "Failed to ensure EPC server environment: %s" (error-message-string err))))

  ;; 2. Call the new EPC method 'get_available_models' synchronously
  (condition-case err
      (let ((response (epc:call-sync org-supertag-sim-epc-manager 'get_available_models nil))) ; No arguments needed
        (org-supertag-sim-epc-log "Raw EPC response for models: %S" response)

        ;; 3. Process Response
        (if (and response (listp response) (plistp response))
            (let ((status (plist-get response :status))
                  (result (plist-get response :result)) ; Should be the list of model names
                  (message (plist-get response :message)))
              (if (string= status "success")
                  (if (listp result) ; Verify the result is a list
                      (progn
                        (org-supertag-sim-epc-log "Successfully fetched %d models." (length result))
                        result) ; Return the list of model names
                    (error "Invalid result format received for models: Expected list, got %S" result))
                  (error "Error fetching models from backend: %s" (or message "Unknown error."))))
          (error "Invalid response format received from EPC for models: %S" response)))
    ;; Handle EPC communication errors
    (error (error "EPC communication error while fetching models: %s" (error-message-string err)))))

(defun org-supertag-ai-select-model ()
  "Interactively select an available Ollama model.
Fetches the list of models from the running Ollama service via the
SimTag EPC server and presents them for selection in the minibuffer.
Inserts the selected model name into the current buffer at point."
  (interactive)
  (condition-case err
      ;; Fetch the list of models first
      (let* ((available-models (org-supertag-ai--fetch-available-models))
             (selected-model nil))
        (if available-models
            ;; Use completing-read for selection
            (setq selected-model (completing-read "Select Ollama Model: "
                                                  available-models ; Candidates
                                                  nil ; Predicate
                                                  t ; Require match
                                                  nil ; Initial input
                                                  nil ; History var
                                                  (car available-models))) ; Default value
          (message "No available models fetched from Ollama service."))

        ;; Insert the selected model if one was chosen
        (if (and selected-model (stringp selected-model) (not (string-empty-p selected-model)))
            (progn
              (insert selected-model)
              (message "Inserted model: %s" selected-model))
          (message "No model selected.")))
    ;; Catch errors during fetching or selection
    (error
     (message "Error selecting model: %s" (error-message-string err)))))

;; --- Convert Markdown to Org ---
;; Inspired by the Ollama-Buddy: https://github.com/captainflasmr/ollama-buddy/blob/main/ollama-buddy-core.el
(defun org-supertag-ai--convert-md-to-org (md-string)
  "Convert a Markdown string MD-STRING to Org format string.
Handles common Markdown elements like headers, lists, bold, italics,
links, inline code, code blocks, and horizontal rules.

Inspired by the logic in `ollama-buddy--md-to-org-convert-region`."
  (if (not (and md-string (stringp md-string) (not (string-empty-p md-string))))
      ;; Return empty string if input is invalid
      ""
    (with-temp-buffer
      (insert md-string)
      ;; Perform conversions within the temporary buffer
      (goto-char (point-min))

      ;; --- Conversion logic (adapted from ollama-buddy) ---

      ;; 1. Protect code blocks
      (let ((code-blocks nil)
            (counter 0)
            block-start block-end lang content placeholder)
        (save-match-data
          (goto-char (point-min)) ; Ensure search starts from beginning
          ;; Regex slightly adjusted for potentially missing language and newline variations
          (while (re-search-forward "```\\([^\n`]*?\\)\\(?:\n\\|\\s-*$\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
            (setq lang (or (match-string 1) "") ; Handle potentially nil language
                  content (match-string 3) ; Content is group 3
                  block-start (match-beginning 0)
                  block-end (match-end 0)
                  placeholder (format "CODE_BLOCK_PLACEHOLDER_%d_AI" counter)) ; Unique placeholder

            (push (list placeholder lang content) code-blocks)
            (delete-region block-start block-end)
            (goto-char block-start)
            (insert placeholder)
            (setq counter (1+ counter)))))

      ;; 2. Apply Markdown to Org transformations
      ;; Lists: Translate `-`, `*`, or `+` lists
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t)
          (replace-match (concat (match-string 1) "- \\2"))))

      ;; Bold: `**bold**` -> `*bold*`
      (save-match-data
        (goto-char (point-min))
        ;; Match non-space character at boundaries
        (while (re-search-forward "\\*\\*\\([^[:space:]]\\(?:.\\|\n\\)*?[^[:space:]]\\)\\*\\*" nil t)
          (replace-match "*\\1*")))

      ;; Italics: `_italic_` or `*italic*` -> `/italic/`
      ;; Note: Simple `*italic*` conversion might conflict with bold if not careful.
      ;; Let's prioritize `_italic_` first.
      (save-match-data
        (goto-char (point-min))
        ;; Match non-space boundary for _italic_
        (while (re-search-forward "\\([[:space:]]\\|^\\)_\\([^[:space:]]\\(?:.\\|\n\\)*?[^[:space:]]\\)_\\([[:space:]]\\|$\\)" nil t)
          (replace-match "\\1/\\2/\\3")))
      ;; Consider adding conversion for *italic* if needed, carefully avoiding bold conflict

      ;; Links: `[text](url)` -> `[[url][text]]`
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
          (replace-match "[[\\2][\\1]]")))

      ;; Inline code: `code` -> `=code=`
      (save-match-data
        (goto-char (point-min))
        ;; Match non-backtick content
        (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
          (replace-match "=\\1=")))

      ;; Horizontal rules: `---` or `***` -> `-----`
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*\\(?:-{3,}\\|\\*{3,}\\)[ \t]*$" nil t)
          (replace-match "-----")))

      ;; Images: `![alt text](url)` -> `[[url]]` (Org usually displays images directly from links)
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward "!\\[.*?\\](\\([^)]+\\))" nil t)
          (replace-match "[[\\1]]")))

      ;; Headers: `# header` -> `* header` (Only converts level 1-6)
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward "^\\(#{1,6}\\) \\(.*\\)$" nil t)
          (replace-match (concat (make-string (length (match-string 1)) ?*) " " (match-string 2)))))

      ;; Remove potential Markdown blockquote leaders "> "
      (save-match-data
         (goto-char (point-min))
         (while (re-search-forward "^> \\(.*\\)$" nil t)
           (replace-match "\\1")))


      ;; 3. Restore code blocks with proper Org syntax
      (save-match-data
        (dolist (block (nreverse code-blocks))
          (let ((placeholder (nth 0 block))
                (lang (nth 1 block)) ; Language might be empty string
                (content (nth 2 block)))
            (goto-char (point-min))
            (when (search-forward placeholder nil t)
              ;; Ensure newline before/after block if needed
              (unless (or (bobp) (eq (char-before) ?\n)) (insert "\n"))
              (replace-match (format "#+begin_src%s\n%s\n#+end_src"
                                     (if (string-empty-p lang) "" (concat " " lang)) ; Add space only if lang exists
                                     content)
                             t t)
              (unless (or (eobp) (eq (char-after) ?\n)) (insert "\n"))))))

      ;; Return the converted buffer content
      (buffer-string))))

(provide 'org-supertag-ai)
;;; org-supertag-ai.el ends here
