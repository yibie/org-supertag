;;; org-supertag/services/capture.el --- Capture services for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides reusable capture services for the Org-Supertag system,
;; focusing on standalone capture functionality.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-capture)
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
    (list :headline title
          :tags selected-tags)))

;;; --- Node Enrichment Functions ---

(defun supertag-capture--get-fields-for-tags (selected-tags)
  "Get all unique fields for selected tags, including inherited fields.
Returns list of field plists."
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (dolist (tag-id selected-tags)
      (let ((fields (supertag-tag-get-all-fields tag-id)))
        (dolist (field fields)
          (let* ((fid (or (plist-get field :id)
                          (plist-get field :name)))
                 (slug (and fid (supertag-sanitize-field-id fid)))
                 (dedupe-key (if supertag-use-global-fields
                                 slug
                               (plist-get field :name))))
            (when (and dedupe-key (not (gethash dedupe-key seen)))
              (puthash dedupe-key t seen)
              (push field result))))))
    (nreverse result)))

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
        (cl-return-from supertag-capture-enrich-node))

      (while t
        (let ((tag-id (completing-read "Select a tag to add field from (or empty to finish): "
                                       (cons "" node-tags) nil t)))
          (when (string-empty-p tag-id)
            (message "Node enrichment complete for %s" node-id)
            (cl-return-from supertag-capture-enrich-node))

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
                        (if supertag-use-global-fields
                            (let ((fid (supertag-sanitize-field-id field-name)))
                              (when fid
                                (supertag-node-set-global-field node-id fid value)))
                          (supertag-field-set node-id tag-id field-name value))
                        (message "Set %s/%s -> %s" tag-id field-name value)))))
             (choice
              (let* ((field-def (cl-find-if (lambda (f) (string= (plist-get f :name) choice)) fields))
                     (current-value (supertag-field-get-with-default node-id tag-id choice))
                     (new-value (supertag-ui-read-field-value field-def current-value)))
                (unless (or (null new-value) (equal new-value current-value))
                  (if supertag-use-global-fields
                      (let ((fid (supertag-sanitize-field-id choice)))
                        (when fid
                          (supertag-node-set-global-field node-id fid new-value)))
                    (supertag-field-set node-id tag-id choice new-value))
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
  "Generator: Return the static value from ARGS.
Special case: when ARGS contains tags and position, return both."
  (if (and (cadr args) (memq (cadr args) '(:before-title :after-title)))
      ;; Special case: (tags position) for tag positioning
      args
    ;; Normal case: just return the static value
    (car args)))

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
        ;; Use replace-regexp-in-string instead of replace-match for better handling of Unicode
        (setq template (replace-regexp-in-string (concat "%^{\\(" (regexp-quote prompt) "\\)}")
                                               user-input
                                               template
                                               t
                                               t))))

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
  "Parse a template string into a structured node-spec list (kernel IR).
Prioritizes title prompts over body content for better UX."
  (let* ((lines (split-string template-string "\n" t)) ; Don't keep empty lines from string end
         (headline (car lines))
         (rest-lines (cdr lines))
         (title-spec nil)
         (tags-spec nil)
         (tag-position nil)  ; Track tag position: :before-title, :after-title, or nil
         (fields '())
         (body-lines '())
         (tags '()))

    ;; 1. Parse Headline and Tags from the first line
    (when (string-match "^\\*+ \\(.*\\)" headline)
      (let* ((headline-content (string-trim (match-string 1 headline))))
        ;; Extract tags and determine their position
        (setq tags (supertag-transform-extract-inline-tags headline-content))

        ;; Determine tag position based on where tags occur
        (setq tag-position (when tags
                             (if (string-match-p "\\`#\\w" headline-content)
                                 :before-title
                               :after-title)))

        ;; Extract title template by removing tags
        (let* ((raw-title-template
                (replace-regexp-in-string "\\(?:^\\|\\s-\\)#\\w[-_[:alnum:]]*" "" headline-content))
               (clean-title-template
                (string-trim (replace-regexp-in-string "\\s-\\{2,\\}" " " raw-title-template))))
          (setq title-spec `(:part :title :get ,(supertag-capture--generate-get-spec clean-title-template)))
          (when tags
            (setq tags-spec `(:part :tags :get (:static ,tags ,tag-position)))))))

    ;; 2. Parse Fields and Body from the rest of the lines
    (dolist (line rest-lines)
      (if (string-match "^\\s-*- \\([^:]+\\):\\s-*\\(.*\\)" line)
          (let* ((key (match-string 1 line))
                 (value-template (match-string 2 line)))
            (push `(:field ,(string-trim key) :get ,(supertag-capture--generate-get-spec (string-trim value-template))) fields))
        (push line body-lines)))

    ;; 3. Assemble the final spec list with priority order
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

(defun supertag-capture--normalize-tag-position (position)
  "Normalize POSITION for tag placement within a captured headline.
Accepts user-facing values like 'beginning/'end or string equivalents, as
well as internal keywords such as :before-title. Returns a keyword that is
compatible with `supertag--render-org-headline'."
  (let* ((normalized (if (stringp position) (downcase (string-trim position)) nil))
         (pos (cond
               ((stringp position)
                (intern (if (and normalized (string-prefix-p ":" normalized))
                            normalized
                          normalized)))
               (t position))))
    (cond
     ((memq pos '(beginning before-title :before-title)) :before-title)
     ((or (null pos) (memq pos '(end after-title :after-title))) nil)
     (t pos))))

(defun supertag-capture--process-spec (node-spec)
  "Process a NODE-SPEC list and return a plist of generated data."
  (let ((title "") (tags '()) (body "") (fields '()) (tag-position nil))
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
           (let ((tags-result (supertag-capture--get-content get-spec)))
             (setq tags (if (listp tags-result)
                           (car tags-result)  ; Extract tags list from (tags position)
                         tags-result))
             (setq tag-position (when (listp tags-result)
                                  (cadr tags-result)))  ; Extract position from (tags position)
             (when (and (stringp tags-result) (not (listp tags-result)))
               ;; Handle case where tags might be a string from old format
               (setq tags (split-string tags-result "[,;]" t "[ \t\n\r]+")))
             (message "DEBUG: Got tags: %S, position: %S" tags tag-position)))
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
    `(:title ,title :tags ,tags :body ,body :fields ,fields :tag-position ,tag-position)))

;;; --- NEW: Target Resolution Engine ---

(defun supertag-capture--select-headline-interactively (file)
  "Interactively select a headline from FILE and return its end position.
This function provides a completing-read interface for choosing a target."
  (unless (file-exists-p file)
    (user-error "Target file for interactive selection does not exist: %s" file))
  (with-current-buffer (find-file-noselect file)
    (let* ((headlines (org-map-entries (lambda () (cons (org-get-heading t t) (point))) t))
           (choice (completing-read "Select target headline: " headlines nil t)))
      (when-let ((marker (cdr (assoc choice headlines))))
        (goto-char marker)
        (org-end-of-subtree)
        (point)))))

(defun supertag-capture--resolve-target-location (plist)
  "Resolve a capture PLIST and return (list BUFFER POSITION LEVEL).
Handles various org-capture-style targets using information from PLIST."
  (let* ((target-spec (plist-get plist :target))
         (file-fallback (plist-get plist :file))
         (spec (or target-spec (when file-fallback `(file+interactive ,file-fallback))))
         (type (car spec))
         (args (cdr spec))
         (file (car args)))
    (unless spec
      (user-error "Capture template has no :target or :file defined"))

    ;; Derive file for targets that don't specify one directly.
    (cond
     ((eq type 'here)
      (setq file (or file (buffer-file-name))))
     ((eq type 'id)
      (setq file (org-id-find-id-file (car args)))
      (unless file
        (user-error "Cannot resolve ID %s to a file" (car args)))))

    (unless (or (not file) (file-exists-p file))
      (user-error "Target file does not exist: %s" file))

    (let ((buffer (if file (find-file-noselect file) (current-buffer))))
      (with-current-buffer buffer
        (pcase type
          ('file+headline
           (let ((headline (cadr args)))
             (goto-char (point-min))
             (unless (org-find-exact-headline-in-buffer headline)
               (user-error "Headline not found: %s" headline))
             (org-end-of-subtree)
             (list buffer (point) (1+ (org-outline-level)))))

          ('file+olp+datetree
           (let* ((olp (cadr args))
                  (tree-type (plist-get plist :tree-type 'day)))
             (goto-char (point-min))
             (when (and olp (not (org-find-olp olp)))
               (user-error "Outline path not found: %s" olp))
             (let ((pos (org-datetree-find-capture-location tree-type)))
               (goto-char pos)
               (list buffer pos (org-outline-level)))))

          ('file+interactive
           (let* ((info (supertag-ui-select-insert-position file))
                  (pos (plist-get info :position))
                  (level (or (plist-get info :level) (org-outline-level))))
             (unless info
               (user-error "Insert position selection cancelled"))
             (goto-char pos)
             (list buffer pos level)))

          ('id
           (let* ((id (car args))
                  (marker (org-id-find id 'marker)))
             (unless marker
               (user-error "Cannot find entry with ID: %s" id))
             (org-with-point-at marker
               (org-end-of-subtree)
               (list (current-buffer) (point) (1+ (org-outline-level))))))

          ('here
           (list buffer (point) (org-outline-level)))

          (_ (user-error "Unsupported :target type: %S" type)))))))

(defun supertag-capture--insert-node-into-buffer (buffer position level title tags body new-node-id &optional tag-position)
  "Insert a new Org node into BUFFER at POSITION and return metadata.
This function enforces the correct order: headline, properties, body.
TAG-POSITION determines where tags are placed in the headline."
  (with-current-buffer buffer
    (save-excursion
      (goto-char position)
      (unless (or (bobp) (looking-back "\n" 1))
        (insert "\n"))
      (let* ((headline-start (point))
             (normalized-tag-pos (supertag-capture--normalize-tag-position tag-position))
             marker)
        ;; 1. Insert headline with dynamic tag positioning
        (insert (supertag--render-org-headline
                 level title tags (buffer-file-name buffer) nil
                 nil normalized-tag-pos))
        ;; Create a marker that points to the start of the new headline
        (setq marker (copy-marker headline-start t))
        ;; 2. Insert properties drawer (must come immediately after headline)
        (insert ":PROPERTIES:\n:ID: " new-node-id "\n:END:\n")
        ;; 3. Insert body content after the properties drawer
        (unless (string-empty-p body)
          (insert "\n" body))
        (save-buffer)
        (list :id new-node-id :marker marker)))))

;;; --- Core Finalization API ---

(defgroup supertag-capture nil
  "Capture-related configuration and integration for Org-Supertag."
  :group 'org-supertag)

(defun supertag-capture-finalize-node-at-point (&optional field-specs explicit-node-id)
  "Finalize current Org headline as a Supertag node.

Ensures the node has a stable ID, syncs it into the Supertag store,
and applies FIELD-SPECS using the Tag/Field/Value data model.

FIELD-SPECS is a list of plists like:
  (:tag TAG-ID :field FIELD-NAME :value VALUE)

When EXPLICIT-NODE-ID is non-nil, it is enforced as the node ID and
registered via `org-id-add-location'.  Otherwise, `org-id-get-create'
is used to obtain or create an ID."
  (interactive)
  (unless (org-before-first-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (let* ((current-id (org-entry-get nil "ID"))
             (node-id (or explicit-node-id current-id (org-id-get-create))))
        (when explicit-node-id
          (unless (string= explicit-node-id current-id)
            (org-entry-put nil "ID" explicit-node-id))
          (setq node-id explicit-node-id))
        (when (and node-id (buffer-file-name))
          (org-id-add-location node-id (buffer-file-name)))
        (supertag-node-sync-at-point)
        (when field-specs
          (supertag-field-set-many node-id field-specs))
        node-id))))

;;; --- org-capture Integration Layer ---

(defcustom supertag-org-capture-auto-enable nil
  "When non-nil, enable Supertag integration with `org-capture'.

This is a convenience toggle.  You can also call
`supertag-enable-org-capture-integration' and
`supertag-disable-org-capture-integration' manually."
  :type 'boolean
  :group 'supertag-capture)

(defun supertag-org-capture-after-finalize ()
  "Attach Supertag metadata for org-capture entries that opt in.

Templates can opt in by adding `:supertag t' to their entry in
`org-capture-templates'.  Optional `:supertag-template' can be a
list of plists of the form:

  (:tag TAG-ID :field FIELD-NAME :value VALUE)

These will be applied via `supertag-field-set-many'.

Additionally, templates can request a follow-up move using
`supertag-move-node' or `supertag-move-node-and-link' by setting
`:supertag-move' in the template plist:

- :supertag-move t                ; use `supertag-move-node'
- :supertag-move 'node            ; same as t
- :supertag-move 'link            ; use `supertag-move-node-and-link'
- :supertag-move 'within-target   ; move within the capture target file only

You can also enable an interactive Supertag tag prompt after
capture by setting `:supertag-tags-prompt' to non-nil in the
template plist.  This uses the same tag completion source as the
capture DSL."
  (when (and (boundp 'org-capture-plist)
             (plist-get org-capture-plist :supertag))
    (let* ((marker (and (boundp 'org-capture-last-stored-marker)
                        org-capture-last-stored-marker))
           (field-specs (plist-get org-capture-plist :supertag-template))
           (move-spec (plist-get org-capture-plist :supertag-move))
           (tags-prompt (plist-get org-capture-plist :supertag-tags-prompt)))
      (when (markerp marker)
        (with-current-buffer (marker-buffer marker)
          (when (buffer-live-p (current-buffer))
            (goto-char marker)
            (progn
              (let ((node-id (supertag-capture-finalize-node-at-point field-specs)))
                ;; Optional tags prompt using Supertag tag completion
                (when (and tags-prompt node-id)
                  (let* ((chosen
                          (supertag-capture--get-from-tags-prompt
                           (list "Supertag tags (comma separated): ")))
                         (sanitized (mapcar #'supertag-sanitize-tag-name chosen))
                         (unique-tags (cl-delete-duplicates sanitized :test #'string=)))
                    (when unique-tags
                      ;; Update DB tags (create tag entities if needed)
                      (dolist (tag-id unique-tags)
                        (supertag-ops-add-tag-to-node node-id tag-id :create-if-needed t))
                      ;; Update inline #tag on the Org headline
                      (org-back-to-heading t)
                      (let* ((title (org-get-heading t t t t))
                             (bare-title
                              (string-trim
                               (replace-regexp-in-string
                                "\\(?:^\\|\\s-\\)#\\w[-_[:alnum:]]*" "" title)))
                             (tag-string
                              (mapconcat (lambda (t) (concat "#" t))
                                         unique-tags " "))
                             (new-title
                              (string-trim
                               (if (string-empty-p bare-title)
                                   tag-string
                                 (concat bare-title " " tag-string)))))
                        (org-edit-headline new-title)
                        (supertag-node-sync-at-point)))))
                  (when move-spec
                    (let* ((raw-move move-spec)
                           ;; Normalize move-spec:
                           ;; - (quote foo) -> foo
                           ;; - \"link\" / \"within-target\" -> symbol
                           ;; - symbols remain unchanged
                           (normalized
                            (cond
                             ((and (consp raw-move) (eq (car raw-move) 'quote))
                              (cadr raw-move))
                             ((stringp raw-move)
                              (intern (downcase raw-move)))
                             (t raw-move))))
                      (pcase normalized
                        ;; Move within the current capture target file:
                        ;; re-use `supertag-move-node' but skip the file prompt.
                        ((or 'within-target :within-target)
                         (let ((current-file (buffer-file-name)))
                           (when (and current-file (fboundp 'supertag-move-node))
                             (cl-letf (((symbol-function 'read-file-name)
                                        (lambda (&rest _ignore) current-file)))
                               (call-interactively #'supertag-move-node)))))
                        ;; Move to another file and leave a link behind
                        ((or 'link :link)
                         (when (fboundp 'supertag-move-node-and-link)
                           (call-interactively #'supertag-move-node-and-link)))
                        ;; Default: full `supertag-move-node' UI (file + position)
                        (_
                         (when (fboundp 'supertag-move-node)
                           (call-interactively #'supertag-move-node))))))
              (message "[supertag] org-capture integration: node finalized%s"
                       (if move-spec " and moved" ""))))))))))

(defun supertag-enable-org-capture-integration ()
  "Enable Supertag integration with `org-capture'."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook
            #'supertag-org-capture-after-finalize)
  (setq supertag-org-capture-auto-enable t)
  (message "[supertag] org-capture integration enabled"))

(defun supertag-disable-org-capture-integration ()
  "Disable Supertag integration with `org-capture'."
  (interactive)
  (remove-hook 'org-capture-after-finalize-hook
               #'supertag-org-capture-after-finalize)
  (setq supertag-org-capture-auto-enable nil)
  (message "[supertag] org-capture integration disabled"))

(when supertag-org-capture-auto-enable
  (supertag-enable-org-capture-integration))


;;; --- REWRITTEN: Main Workflow ---

;;;###autoload
(defun supertag-capture-with-template (&optional template-key)
  "A self-contained capture system with advanced targeting and dynamic templates."
  (interactive)
  (unless supertag-capture-templates
    (user-error "No templates defined. Please set `supertag-capture-templates'"))
  (let* ((template (if template-key
                       (assoc template-key supertag-capture-templates)
                     (let* ((completion-alist
                             (mapcar (lambda (tpl)
                                       (cons (format "%s - %s" (car tpl) (cadr tpl)) tpl))
                                     supertag-capture-templates))
                            (selected-display (completing-read "Choose template: "
                                                               (mapcar #'car completion-alist)
                                                               nil t)))
                       (cdr (assoc selected-display completion-alist))))))
    (unless template
      (user-error "Template not found"))
    (pcase-let* ((`(,key ,description . ,plist) template)
                 (raw-node-spec (plist-get plist :node-spec))
                 (node-spec (if (and (consp raw-node-spec)
                                     (eq (car raw-node-spec) :template))
                                (supertag-capture--parse-template-string (cadr raw-node-spec))
                              raw-node-spec)))
      (unless node-spec
        (user-error "Template '%s' has no :node-spec" description))
      (ignore key) ; keep API parity even if KEY is unused for now
    ;; 1. Resolve target location using the enhanced engine
      ;; 1. Gather user input before choosing insertion target
      (let* ((processed-data (supertag-capture--process-spec node-spec))
             (title (plist-get processed-data :title))
             (tags (plist-get processed-data :tags))
             (body (plist-get processed-data :body))
             (field-settings (plist-get processed-data :fields))
             (tag-position (plist-get processed-data :tag-position))
             ;; 2. Resolve target location after prompts are complete
             (location-info (supertag-capture--resolve-target-location plist))
             (buffer (nth 0 location-info))
             (position (nth 1 location-info))
             (level (nth 2 location-info))
             (new-node-id (org-id-new))
             (insert-result (supertag-capture--insert-node-into-buffer
                             buffer position level title tags body new-node-id tag-position))
             (node-marker (plist-get insert-result :marker))
             (resolved-fields
              (when field-settings
                (cl-loop for f-spec in field-settings append
                         (let* ((tag-id (plist-get f-spec :tag))
                                (field-name (plist-get f-spec :field))
                                (getter (plist-get f-spec :get))
                                (explicit-specified (plist-member f-spec :value))
                                (explicit (plist-get f-spec :value))
                                (value nil)
                                (provided nil))
                           (cond
                            (getter
                             (setq value (supertag-capture--get-content getter))
                             (setq provided t))
                            (explicit-specified
                             (setq value explicit)
                             (setq provided t)))
                           (cond
                            ((and provided
                                  (listp value)
                                  (not (stringp value))
                                  (cl-every (lambda (item)
                                              (and (plist-get item :tag)
                                                   (plist-get item :field)))
                                            value))
                             (cl-loop for nested in value
                                      for n-tag = (plist-get nested :tag)
                                      for n-field = (plist-get nested :field)
                                      for n-value = (plist-get nested :value)
                                      when (and n-tag n-field
                                                (not (and (stringp n-value)
                                                          (string-empty-p n-value))))
                                      collect (list :tag n-tag :field n-field :value n-value)))
                            ((and provided tag-id field-name
                                  (not (and (stringp value) (string-empty-p value))))
                             (list (list :tag tag-id :field field-name :value value)))
                            (t nil)))))))
          (org-with-point-at node-marker
            (supertag-capture-finalize-node-at-point resolved-fields new-node-id))
          (message "Capture successful for template: %s" description)))))

(provide 'supertag-services-capture)

;;; supertag-services-capture.el ends here
