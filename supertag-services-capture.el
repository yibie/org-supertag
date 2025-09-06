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
           finally return (delete-dups all-fields :key (lambda (f) (plist-get f :name)) :test #'equal)))

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
  (let ((continue t)
        (node-tags (plist-get (supertag-node-get node-id) :tags)))
    (unless node-tags
      (message "Node has no tags to provide fields.")
      (cl-return-from supertag-capture-enrich-node))
    
    (while continue
      (let* ((tag-id (completing-read "Select a tag to add field from (or empty to finish): "
                                      (cons "" node-tags) nil nil)))
        (if (string-empty-p tag-id)
            (setq continue nil)
          (let* ((fields (supertag-tag-get-all-fields tag-id))
                 (field-names (mapcar (lambda (f) (plist-get f :name)) fields)))
            (unless fields
              (message "Tag '%s' has no fields defined." tag-id)
              (cl-return-from supertag-capture-enrich-node))
            (let* ((field-name (completing-read (format "Select field for tag '%s': " tag-id) field-names nil t)))
              (when field-name
                (let ((value (read-string (format "Value for %s/%s: " tag-id field-name))))
                  (unless (string-empty-p value)
                    (supertag-field-set node-id tag-id field-name value)
                    (message "Set %s/%s -> %s" tag-id field-name value)))))))))
    (message "Node enrichment complete for %s" node-id)))

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
    (read-string prompt nil nil (plist-get props :initial-input))))

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
    
    template))

(defun supertag-capture--get-from-function (args)
  "Generator: Call a function to get content.
ARGS is a list containing the function symbol."
  (let ((func (car args)))
    (if (fboundp func)
        (funcall func)
      (error "Template function not found: %S" func))))

;; --- Dispatcher ---

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
      (_ (error "Unknown capture generator type: %S" type)))))

;; --- Processor & Executor ---

(defun supertag-capture--process-spec (node-spec)
  "Process a NODE-SPEC list and return a plist of generated data."
  (let ((title "") (tags '()) (body "") (fields '()))
    (dolist (spec node-spec)
      (let ((part-type (plist-get spec :part))
            (get-spec (plist-get spec :get)))
        (pcase part-type
          (:title (setq title (supertag-capture--get-content get-spec)))
          (:tags (let ((tags-val (supertag-capture--get-content get-spec)))
                   (setq tags (if (listp tags-val) tags-val (list tags-val)))))
          (:body (setq body (supertag-capture--get-content get-spec)))
          (:fields (setq fields (supertag-capture--get-content get-spec))))))
    `(:title ,title :tags ,tags :body ,body :fields ,fields)))

(defun supertag-capture--execute (target-file processed-data)
  "Execute the capture by writing data to file and syncing.
TARGET-FILE is the path to the destination file.
PROCESSED-DATA is the plist from --process-spec."
  (let* ((title (plist-get processed-data :title))
         (tags (plist-get processed-data :tags))
         (body (plist-get processed-data :body))
         (field-settings (plist-get processed-data :fields))
         ;; Assemble the node string
         (tags-str (if tags (mapconcat (lambda (tag) (concat "#" tag)) tags " ") ""))
         (full-title (if (string-empty-p tags-str) title (format "%s %s" title tags-str)))
         ;; Get location
         (insert-info (supertag-ui-select-insert-position target-file))
         (insert-pos (plist-get insert-info :position))
         (insert-level (plist-get insert-info :level)))
    (unless insert-info
      (user-error "No valid insert position selected"))

    ;; Side Effect 1: Write to file
    (let ((new-node-id (org-id-new)))
      (with-current-buffer (find-file-noselect target-file)
        (save-excursion
          (goto-char insert-pos)
          (unless (or (bobp) (looking-back "\n" 1)) (insert "\n"))
          (insert (make-string insert-level ?*) " " full-title "\n")
          (unless (string-empty-p body)
            (insert body "\n"))
          (insert "\n:PROPERTIES:\n:ID: " new-node-id "\n:END:\n")
          (save-buffer)))

      ;; Side Effect 2: Sync and Enrich
      (let ((node-id new-node-id))
      (when node-id
        (supertag-services-sync-file target-file)
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
              (when (and f-tag f-field f-value)
                (supertag-field-set node-id f-tag f-field f-value)
                (message "Set field: %s/%s -> %s" f-tag f-field f-value)))))

        ;; Interactive enrichment
        (when (y-or-n-p "Enrich node with more fields? ")
          (supertag-capture-enrich-node node-id)))))))

;; --- Orchestrator ---

(defun supertag-capture-with-template (&optional template-key)
  "Capture using a dynamic template.
If TEMPLATE-KEY is not provided, prompts for one."
  (interactive)
  (unless supertag-capture-templates
    (user-error "No templates defined. Please set `supertag-capture-templates'"))

  ;; 1. Select Template
  (let* ((template-alist supertag-capture-templates)
         (key (or template-key
                  (completing-read "Template: "
                                   (mapcar (lambda (t) (cons (car t) (cadr t))) template-alist)
                                   nil t)))
         (template-data (cdr (assoc key template-alist)))
         (target-file (plist-get template-data :file))
         (node-spec (plist-get template-data :node-spec)))
    (unless template-data
      (user-error "Template doesn't exist: %s" key))

    ;; 2. Process spec into data (Call Processor)
    (let ((processed-data (supertag-capture--process-spec node-spec)))
      ;; 3. Execute capture with data (Call Executor)
      (supertag-capture--execute target-file processed-data))))

(provide 'supertag-services-capture)

;;; supertag-services-capture.el ends here
