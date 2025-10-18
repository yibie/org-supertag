;;; org-supertag/services/capture.el --- Capture services for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides reusable capture services for the Org-Supertag system,
;; focusing on standalone capture functionality.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'supertag-ops-node)
(require 'supertag-ops-tag)
(require 'supertag-ops-field)
(require 'supertag-services-ui)
(require 'supertag-services-query)
(require 'supertag-services-sync)

;;; --- Sync Helper Function ---

(defun supertag-services-sync-file (file)
  "Synchronize a single file with the supertag store.
This is a simple wrapper around the core sync functionality."
  (let ((counters '(:nodes-created 0 :nodes-updated 0 :nodes-deleted 0)))
    (supertag-sync--process-single-file file counters)
    (supertag-sync-garbage-collect-orphaned-nodes)))

;;; --- Core Capture Helper Functions ---

(defun supertag-capture-interactive-headline ()
  "Interactively build a headline with inline tags and return details.
Prompts user for title and allows multi-select of tags.
Returns a plist with :headline string and :tags list."
  (let* ((title (read-string "Node title: "))
         (all-tags (mapcar #'car (supertag-query :tags)))
         (selected-tags (completing-read-multiple "Select tags (comma separated, optional): " all-tags)))
    (list :headline (if selected-tags
                        (format "%s %s" title (mapconcat (lambda (tag) (concat "#" tag)) selected-tags " "))
                      title)
          :tags selected-tags)))

;;; --- Node Enrichment Functions ---

(defun supertag-capture--get-fields-for-tags (selected-tags)
  "Get all unique fields for selected tags, including inherited fields.
Returns list of field plists."
  (cl-loop for tag-id in selected-tags
           for fields = (supertag-tag-get-all-fields tag-id)
           when fields append fields into all-fields
           finally return (cl-delete-duplicates all-fields :key #'(lambda (f) (plist-get f :name)) :test #'equal)))

(defun supertag-capture--prompt-for-field-values (fields)
  "Prompt user for values for the given fields.
Returns alist of (field-name . value)."
  (cl-loop for field in fields
           for field-name = (plist-get field :name)
           for value = (read-string (format "Value for %s: " field-name))
           unless (string-empty-p value)
           collect (cons field-name value)))

(defun supertag-capture-enrich-node (node-id)
  "Interactively enrich a node with field values based on its tags.
NODE-ID is the ID of the node to enrich.
This function correctly uses the Tag -> Field -> Value data model."
  (cl-block supertag-capture-enrich-node
    (let ((node-tags (plist-get (supertag-node-get node-id) :tags)))
      (unless node-tags
        (message "Node has no tags to provide fields.")
        (return-from supertag-capture-enrich-node))

      (while t
        (let ((tag-id (completing-read "Select a tag to add field from (or empty to finish): "
                                       (cons "" node-tags) nil t)))
          (when (string-empty-p tag-id)
            (message "Node enrichment complete for %s" node-id)
            (return-from supertag-capture-enrich-node))

          (let* ((fields (supertag-tag-get-all-fields tag-id))
                 (field-names (mapcar #'(lambda (f) (plist-get f :name)) fields))
                 (prompt-choices (append field-names '("Create New Field")))
                 (choice (completing-read (format "Select field for tag '%s': " tag-id)
                                          prompt-choices nil t)))
            (cond
             ((string= choice "Create New Field")
              (when-let ((new-field-def (supertag-ui-create-field-definition)))
                (supertag-tag-add-field tag-id new-field-def)
                (let* ((field-name (plist-get new-field-def :name))
                       (value (supertag-ui-read-field-value new-field-def nil)))
                  (unless (or (null value) (string-empty-p value))
                    (supertag-field-set node-id tag-id field-name value)
                    (message "Set %s/%s -> %s" tag-id field-name value)))))
             (choice
              (let* ((field-def (cl-find-if (lambda (f) (string= (plist-get f :name) choice)) fields))
                     (current-value (supertag-field-get node-id tag-id choice))
                     (new-value (supertag-ui-read-field-value field-def current-value)))
                (unless (or (null new-value) (equal new-value current-value))
                  (supertag-field-set node-id tag-id choice new-value)
                  (message "Set %s/%s -> %s" tag-id choice new-value)))))))))))

;;; --- Dynamic Capture Template System ---

(defcustom supertag-capture-templates nil
  "User-defined dynamic capture templates for Org-Supertag.
Each template is a list: (KEY DESCRIPTION PLIST)
- KEY: A short string to identify the template (e.g., \"t\").
- DESCRIPTION: A string describing what the template does.
- PLIST: A property list containing:
  - :file: The target file path for the capture.
  - :node-spec: A list of specifications for each part of the node.

Each item in :node-spec is a plist:
(:part PART-TYPE :get GET-SPEC)
- :part: The part of the node to generate (:title, :tags, :body, :fields).
- :get: A list describing how to generate the content.

The :get list is structured as (GENERATOR-TYPE ...ARGS).
Valid GENERATOR-TYPEs are:
- :static VALUE: Use the static VALUE directly.
- :prompt PROMPT-STRING &rest PROPS: Prompt the user for input.
- :clipboard: Use the content of the clipboard.
- :region: Use the content of the active region.
- :region-or-clipboard: Use region, or fall back to clipboard.
- :template-string TEMPLATE: Use a string with placeholders like %clipboard, %date.
- :function FUNC: Call the function FUNC to get the content.

Example:
'((\"t\" \"A dynamic task\"
   :file \"~/org/tasks.org\"
   :node-spec
   '((:part :title :get (:prompt \"Task: \"))
     (:part :tags  :get (:prompt \"Tags: \" :initial-input \"task,\"))
     (:part :body  :get (:region-or-clipboard)))))"
  :type '(repeat (list (string :tag "Key")
                       (string :tag "Description")
                       (plist :tag "Properties")))
  :group 'org-supertag)

;; --- Generator Implementations ---

(defun supertag-capture--get-from-static (args)
  "Generator: Return the static value from ARGS."
  (car args))

(defun supertag-capture--get-from-prompt (args)
  "Generator: Prompt user for input.
ARGS can be (PROMPT-STRING &key :initial-input)."
  (let ((prompt (car args))
        (props (cdr args)))
    (read-string prompt (plist-get props :initial-input))))

(defun supertag-capture--get-from-clipboard ()
  "Generator: Get content from clipboard."
  (current-kill 0))

(defun supertag-capture--get-from-region ()
  "Generator: Get content from active region."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (user-error "Cannot get content from region: region is not active")))

(defun supertag-capture--get-from-region-or-clipboard ()
  "Generator: Get content from region, or clipboard if region is not active."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (current-kill 0)))

(defun supertag-capture--get-from-template-string (args)
  "Generator: Process a template string with placeholders.
ARGS is a list containing the template string.

Supported placeholders:
- %date: Current date (YYYY-MM-DD)
- %time: Current time (HH:MM)
- %datetime: Current date and time (YYYY-MM-DD HH:MM)
- %timestamp: Full timestamp (YYYY-MM-DD HH:MM:SS)
- %week: Current week number (W##)
- %month: Current month name (January, February, etc.)
- %year: Current year (YYYY)
- %user: Current user login name
- %fullname: Current user full name
- %hostname: System hostname
- %filename: Current buffer filename (if any)
- %filepath: Current buffer file path (if any)
- %directory: Current buffer directory (if any)
- %current-node-title: Title of current node (if at heading)
- %current-node-id: ID of current node (if at heading)
- %current-tags: Tags of current node (if at heading)
- %clipboard: Clipboard content
- %random: Random 4-digit number
- %uuid: Generate a new UUID"
  (let ((template (car args))
        (current-time (current-time)))
    ;; Time-related variables
    (setq template (replace-regexp-in-string "%date" (format-time-string "%Y-%m-%d" current-time) template t t))
    (setq template (replace-regexp-in-string "%time" (format-time-string "%H:%M" current-time) template t t))
    (setq template (replace-regexp-in-string "%datetime" (format-time-string "%Y-%m-%d %H:%M" current-time) template t t))
    (setq template (replace-regexp-in-string "%timestamp" (format-time-string "%Y-%m-%d %H:%M:%S" current-time) template t t))
    (setq template (replace-regexp-in-string "%week" (format-time-string "W%U" current-time) template t t))
    (setq template (replace-regexp-in-string "%month" (format-time-string "%B" current-time) template t t))
    (setq template (replace-regexp-in-string "%year" (format-time-string "%Y" current-time) template t t))
    
    ;; User information
    (setq template (replace-regexp-in-string "%user" (or (getenv "USER") (getenv "USERNAME") "unknown") template t t))
    (setq template (replace-regexp-in-string "%fullname" (or (user-full-name) "Unknown User") template t t))
    
    ;; System information
    (setq template (replace-regexp-in-string "%hostname" (or (system-name) "localhost") template t t))
    
    ;; File context
    (setq template (replace-regexp-in-string "%filename"
                                           (or (when (buffer-file-name) (file-name-nondirectory (buffer-file-name))) "")
                                           template t t))
    (setq template (replace-regexp-in-string "%filepath" (or (buffer-file-name) "") template t t))
    (setq template (replace-regexp-in-string "%directory"
                                           (or (when (buffer-file-name) (file-name-directory (buffer-file-name))) "")
                                           template t t))
    
    ;; Current node context (if at heading)
    (setq template (replace-regexp-in-string "%current-node-title"
                                           (or (when (org-at-heading-p) (org-get-heading t t)) "")
                                           template t t))
    (setq template (replace-regexp-in-string "%current-node-id"
                                           (or (when (org-at-heading-p) (org-id-get)) "")
                                           template t t))
    (setq template (replace-regexp-in-string "%current-tags"
                                           (or (when (org-at-heading-p)
                                                 (let ((node-id (org-id-get)))
                                                   (when node-id
                                                     (let ((node (supertag-node-get node-id)))
                                                       (when node
                                                         (mapconcat 'identity (plist-get node :tags) ", "))))))
                                               "")
                                           template t t))
    
    ;; Content and utility
    (setq template (replace-regexp-in-string "%clipboard" (or (current-kill 0) "") template t t))
    (setq template (replace-regexp-in-string "%random" (format "%04d" (random 10000)) template t t))
    (setq template (replace-regexp-in-string "%uuid" (org-id-new) template t t))
    
    ;; Interactively fill placeholders like %^{Prompt}
    (while (string-match "%^{\\([^}]*\\)}" template)
      (let* ((prompt (match-string 1 template))
             (user-input (read-string (concat prompt " "))))
        (setq template (replace-match user-input t t template 0))))

    ;; Handle cursor position marker %? by removing it
    (setq template (replace-regexp-in-string "%\\?" "" template t t))
    template))

(defun supertag-capture--get-from-tags-prompt (args)
  "Generator: Prompt for tags with completion on existing tags."
  (let* ((prompt (car args))
         (props (cdr args))
         (initial-input (plist-get props :initial-input))
         (all-tags (mapcar #'car (supertag-query :tags))))
    (completing-read-multiple prompt all-tags nil nil initial-input)))

(defun supertag-capture--get-from-function (args)
  "Generator: Call a function to get content.
ARGS is a list containing the function symbol."
  (let ((func (car args)))
    (if (fboundp func)
        (funcall func)
      (error "Template function not found: %S" func))))

(defun supertag-capture--generate-get-spec (text)
  "From a TEXT string, generate the appropriate ':get' spec.
- If it's a pure prompt, generate '(:prompt ...)'.
- If it contains any placeholders, generate '(:template-string ...)'.
- Otherwise, generate '(:static ...)'. "
  (let ((prompt-only-re "^%^{\\([^}]*\\)}$"))
    (cond
     ;; Case 1: Pure prompt, e.g., "%^{Task Title}"
     ((string-match prompt-only-re text)
      `(:prompt ,(match-string 1 text)))
     ;; Case 2: Contains any placeholder, e.g., "Task for %date"
     ((string-match "%" text)
      `(:template-string ,text))
     ;; Case 3: Just a static string
     (t `(:static ,text)))))

;; --- Dispatcher ---

(defun supertag-capture--get-from-static-grouped (args)
  "Generator: Process statically grouped fields.
ARGS is a list of (TAG-NAME (FIELD-SPEC...)).
Expands it into a flat list of full field specifications."
  (cl-loop for group in (car args)
           for tag-name = (car group)
           for field-specs = (cdr group)
           append (mapcar (lambda (spec)
                            ;; spec is like '(:field "status" :value "todo")
                            (append `(:tag ,tag-name) spec))
                          field-specs)))

(defun supertag-capture--get-content (get-spec)
  "Dispatch to the correct generator based on GET-SPEC."
  (let ((type (car get-spec))
        (args (cdr get-spec)))
    (pcase type
      (:static (supertag-capture--get-from-static args))
      (:prompt (supertag-capture--get-from-prompt args))
      (:clipboard (supertag-capture--get-from-clipboard))
      (:region (supertag-capture--get-from-region))
      (:region-or-clipboard (supertag-capture--get-from-region-or-clipboard))
      (:template-string (supertag-capture--get-from-template-string args))
      (:function (supertag-capture--get-from-function args))
      (:tags-prompt (supertag-capture--get-from-tags-prompt args))
      (:static-grouped (supertag-capture--get-from-static-grouped args))
      ;; The :template keyword is a special form that gets pre-parsed.
      ;; It shouldn't be dispatched here, but we handle it for safety.
      (:template (supertag-capture--process-spec (supertag-capture--parse-template-string (car args))))
      (_ (error "Unknown capture generator type: %S" type)))))

;; --- Template String Parser ---

(defun supertag-capture--parse-template-string (template-string)
  "Parse a template string into a structured node-spec list (kernel IR)."
  (let* ((lines (split-string template-string "\n" t)) ; Don't keep empty lines from string end
         (headline (car lines))
         (rest-lines (cdr lines))
         (title-spec nil)
         (tags-spec nil)
         (fields '())
         (body-lines '())
         (tags '()))

    ;; 1. Parse Headline and Tags from the first line
    (when (string-match "^\\*+ \\(.*\\)" headline)
      (let* ((headline-content (string-trim (match-string 1 headline)))
             (raw-title-template (replace-regexp-in-string "\\s-?#\\w[-_[:alnum:]]*" "" headline-content)))
        (setq tags (supertag-transform-extract-inline-tags headline-content))
        (setq title-spec `(:part :title :get ,(supertag-capture--generate-get-spec (string-trim raw-title-template))))
        (when tags
          (setq tags-spec `(:part :tags :get (:static ,tags))))))

    ;; 2. Parse Fields and Body from the rest of the lines
    (dolist (line rest-lines)
      (if (string-match "^\\s-*- \\([^:]+\\):\\s-*\\(.*\\)" line)
          (let* ((key (match-string 1 line))
                 (value-template (match-string 2 line)))
            (push `(:field ,(string-trim key) :get ,(supertag-capture--generate-get-spec (string-trim value-template))) fields))
        (push line body-lines)))

    ;; 3. Assemble the final spec list
    (let* ((body-template (string-join (nreverse body-lines) "\n"))
           (spec (list title-spec tags-spec)))
      (when fields
        (let ((first-tag (car tags)))
          (when first-tag
            (push `(:part :fields :get (:static-grouped ((,first-tag ,@(nreverse fields)))))
                  spec))))
      (unless (string-blank-p body-template)
        (push `(:part :body :get ,(supertag-capture--generate-get-spec body-template)) spec))
      (delq nil spec))))

;; --- Processor & Executor ---

(defun supertag-capture--process-spec (node-spec)
  "Process a NODE-SPEC list and return a plist of generated data."
  (let ((title "") (tags '()) (body "") (fields '()))
    (dolist (spec node-spec)
      (let ((part-type (plist-get spec :part))
            (get-spec (plist-get spec :get)))
        (pcase part-type
          (:title
           (message "DEBUG: Matched :title. Calling get-content...")
           (setq title (supertag-capture--get-content get-spec))
           (message "DEBUG: Got title: %S" title))
          (:tags
           (message "DEBUG: Matched :tags. Calling get-content...")
           (setq tags (let ((tags-val (supertag-capture--get-content get-spec)))
                        (if (listp tags-val)
                            tags-val
                          (split-string tags-val "[,;]" t "[ \t\n\r]+"))))
           (message "DEBUG: Got tags: %S" tags))
          (:body
           (message "DEBUG: Matched :body. Calling get-content...")
           (setq body (supertag-capture--get-content get-spec))
           (message "DEBUG: Got body: %S" body))
          (:fields
           (message "DEBUG: Matched :fields. Calling get-content...")
           (setq fields (supertag-capture--get-content get-spec))
           (message "DEBUG: Got fields: %S" fields)))
        (message "DEBUG: Finished processing one spec item.")))
    (message "DEBUG: --- Exiting process-spec ---")
    `(:title ,title :tags ,tags :body ,body :fields ,fields)))

(defun supertag-capture--insert-node-into-buffer (buffer position level title tags body new-node-id)
  "Correctly insert a new Org node into BUFFER at POSITION.
This function enforces the correct order: headline, properties, body."
  (with-current-buffer buffer
    (save-excursion
      (goto-char position)
      (unless (or (bobp) (looking-back "\n" 1)) (insert "\n")) ; Ensure blank line before
      ;; 1. Insert headline
      (insert (supertag--render-org-headline level title tags (buffer-file-name buffer) nil))
      ;; 2. Insert properties drawer (must come immediately after headline)
      (insert ":PROPERTIES:\n:ID: " new-node-id "\n:END:\n")
      ;; 3. Insert body content after the properties drawer
      (unless (string-empty-p body)
        (insert "\n" body))
      (save-buffer))))

(defun supertag-capture--execute (target-file processed-data)
  "Execute the capture by writing data to file and syncing.
TARGET-FILE is the path to the destination file.
PROCESSED-DATA is the plist from --process-spec."
  (message "DEBUG: Entering supertag-capture--execute")
  (unless target-file
    (user-error "TARGET-FILE cannot be nil"))
  (unless (file-exists-p target-file)
    (user-error "Target file does not exist: %s" target-file))
    (let* ((title (plist-get processed-data :title))
           (tags (plist-get processed-data :tags))
           (body (plist-get processed-data :body))
           (field-settings (plist-get processed-data :fields))
           ;; Get location
           (insert-info (supertag-ui-select-insert-position target-file))
           (insert-pos (plist-get insert-info :position))
           (insert-level (plist-get insert-info :level)))
    (unless insert-info
      (user-error "No valid insert position selected"))

    ;; Side Effect 1: Write to file
      (let ((new-node-id (org-id-new)))
        (supertag-capture--insert-node-into-buffer
         (find-file-noselect target-file)
         insert-pos insert-level title tags body new-node-id)

      ;; Side Effect 2:  Sync and Enrich
      (let ((node-id new-node-id))
      (when node-id
        ;; Instead of a full, heavy-weight sync, perform a direct,
        ;; surgical creation of the node in the database. This is much
        ;; more efficient and avoids confusing logs.
        (supertag-node-create (list :id node-id
                                    :title title
                                    :tags tags
                                    :file target-file))
        (message "Node %s created in %s" node-id (file-name-nondirectory target-file))

        ;; Set field values from template
        (when field-settings
          (dolist (f-spec field-settings)
            (let* ((f-tag (plist-get f-spec :tag))
                   (f-field (plist-get f-spec :field))
                   (f-get (plist-get f-spec :get))
                   (f-value (if f-get
                                (supertag-capture--get-content f-get)
                              (plist-get f-spec :value))))
              (when (and f-tag f-field f-value) (supertag-field-set node-id f-tag f-field f-value) (message "Set field: %s/%s -> %s" f-tag f-field f-value))))))))))



(provide 'supertag-services-capture)

;;; supertag-services-capture.el ends here
