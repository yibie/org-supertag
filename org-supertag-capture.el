;;; org-supertag-capture.el --- Capture system for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a flexible capture system for org-supertag nodes with:
;; - Template-based capture configuration
;; - Quick capture with default settings
;; - Flexible manual capture
;; - Smart heading level detection
;; - Multiple capture sources (region, line, manual input)
;; - Location selection framework
;; - Node movement and copying utilities

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'org-supertag-node)

;;------------------------------------------------------------------------------
;; Configuration Variables
;;------------------------------------------------------------------------------

(defcustom org-supertag-capture-sync-fields-to-properties nil
  "If non-nil, sync captured fields to the Org node's PROPERTIES drawer.
When this is enabled, any field captured via a template with
':type :field' will be written to both the org-supertag
database and the visual :PROPERTIES: drawer in the Org file.

When disabled (default), fields are only written to the
org-supertag database, keeping the Org file cleaner."
  :type 'boolean
  :group 'org-supertag)

(defcustom org-supertag-capture-templates nil
  "A list of templates for `org-supertag-capture`.
This variable defines the available templates for the native
org-supertag capture system. Each template is a plist that
describes how a new node should be created.

  (setq org-supertag-capture-templates
        '((\"t\" \"Simple Task to End of File\"
           :file \"~/org/tasks.org\"
           :level 1
           :position :end ;; Simple, direct insertion
           :content
           '(...))
          (\"f\" \"Flexible Capture\"
           :file \"~/org/inbox.org\"
           :level :auto
           :position :prompt ;; This will trigger the location selector
           :content
           '(...))
          ))"
  :type '(repeat
          (list :tag "Template"
                (string :tag "Key")
                (plist :tag "Plist")))
  :group 'org-supertag)

(defcustom org-supertag-capture-default-template "default"
  "Default template key for quick capture operations."
  :type 'string
  :group 'org-supertag)
  

(defun org-supertag-capture--get-from-static (source-plist)
  "Return the static value from a SOURCE-PLIST."
  (plist-get source-plist :value))

(defun org-supertag-capture--get-from-prompt (source-plist)
  "Prompt user for input and return it."
  (let ((prompt (plist-get source-plist :prompt-text)))
    (read-string prompt)))

(defun org-supertag-capture--get-from-clipboard ()
  "Return content from the clipboard."
  (current-kill 0))

(defun org-supertag-capture--get-from-region ()
  "Return the active region's content, or empty string."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

(defun org-supertag-capture--get-from-function (source-plist)
  "Execute a function and return its value.
The function can be passed arguments from the template."
  (let ((func (plist-get source-plist :function-name))
        (args (plist-get source-plist :args)))
    (if (functionp func)
        (apply func args)
      (error "Invalid function in capture template: %S" func))))

(defun org-supertag-capture--get-content-from-source (source-plist)
  "Dispatch to the correct generator based on the :source key in SOURCE-PLIST."
  (let ((source-type (plist-get source-plist :source)))
    (pcase source-type
      (:static
       (org-supertag-capture--get-from-static source-plist))
      (:prompt
       (org-supertag-capture--get-from-prompt source-plist))
      (:clipboard
       (org-supertag-capture--get-from-clipboard))
      (:region
       (org-supertag-capture--get-from-region))
      (:function
       (org-supertag-capture--get-from-function source-plist))
      (_
       (error "Unknown capture source type: %S" source-type)))))

;;------------------------------------------------------------------------------
;; Core Template Engine
;;------------------------------------------------------------------------------

(defun org-supertag-capture--process-template (template)
  "Process a capture TEMPLATE plist and return the processed data.

The function returns a list containing two elements:
1. A string to be inserted into the Org file (node-string).
2. A list of fields to be set in the database (fields-to-set),
   where each element is a cons cell of (FIELD-NAME . FIELD-VALUE).

This function iterates through the ':content' blocks of the
template and dispatches them to content/data generators."
  (let ((node-string-parts '())
        (fields-to-set '())
        (level (or (plist-get template :level) 1)))
    (dolist (content-block (plist-get template :content))
      (let ((type (plist-get content-block :type))
            (value-plist (plist-get content-block :value)))
        (pcase type
          (:headline
           (let ((headline-text (org-supertag-capture--get-content-from-source content-block)))
             (push (format "%s %s" (make-string level ?*) headline-text) node-string-parts)))
          (:body
           (let ((body-text (org-supertag-capture--get-content-from-source content-block)))
             (push body-text node-string-parts)))
          (:field
           (let ((field-name (plist-get content-block :name))
                 (field-value (org-supertag-capture--get-content-from-source value-plist)))
             (push (cons field-name field-value) fields-to-set))))))
    ;; Return the assembled node string and the list of fields to be set.
    ;; The list of parts is reversed to maintain the order from the template.
    (list (mapconcat #'identity (nreverse node-string-parts) "\n")
          fields-to-set)))

;;------------------------------------------------------------------------------
;; Template Processing Functions
;;------------------------------------------------------------------------------

(defun org-supertag-capture--build-location-from-template (template)
  "Build location specification from TEMPLATE configuration."
  (let* ((file (plist-get template :file))
         (position-type (plist-get template :position))
         (level-spec (plist-get template :level))
         (target-file (if file
                         (expand-file-name file)
                       (read-file-name "Target file: ")))
         (position-data (org-supertag-capture--resolve-template-position target-file position-type))
         (level (org-supertag-capture--resolve-template-level target-file position-data level-spec)))
    
    (list :file target-file
          :position position-data
          :level level
          :template template)))

(defun org-supertag-capture--resolve-template-position (target-file position-type)
  "Resolve POSITION-TYPE for TARGET-FILE into position data."
  (with-current-buffer (find-file-noselect target-file)
    (cond
     ((eq position-type :end)
      `(:type :end :data ,(point-max)))
     ((eq position-type :content-start)
      `(:type :content-start :data ,(org-supertag-find-file-content-start)))
     ((eq position-type :prompt)
      (plist-get (org-supertag--select-insert-position-with-level target-file nil) :position))
     (t
      `(:type :end :data ,(point-max))))))

(defun org-supertag-capture--resolve-template-level (target-file position-data level-spec)
  "Resolve LEVEL-SPEC for TARGET-FILE and POSITION-DATA into heading level."
  (cond
   ((numberp level-spec) level-spec)
   ((eq level-spec :auto)
    (with-current-buffer (find-file-noselect target-file)
      (let ((pos (plist-get position-data :data)))
        (save-excursion
          (goto-char pos)
          (cond
           ((eq (plist-get position-data :type) :content-start) 1)
           ((eq (plist-get position-data :type) :end)
            (if (re-search-backward "^\\(\\*+\\)" nil t)
                (length (match-string 1))
              1))
           (t 1))))))
   (t 1)))

;;------------------------------------------------------------------------------
;; Template Management Functions
;;------------------------------------------------------------------------------

(defun org-supertag-capture-add-template (key description file position level)
  "Add a new capture template.
KEY: Template identifier
DESCRIPTION: Human readable description
FILE: Target file path (nil for prompt)
POSITION: Insert position (:end, :content-start, :prompt)
LEVEL: Heading level (number or :auto)"
  (interactive
   (list (read-string "Template key: ")
         (read-string "Description: ")
         (let ((file (read-file-name "Target file (empty for prompt): " nil nil nil "")))
           (if (string-empty-p file) nil file))
         (intern (completing-read "Position: " '(":end" ":content-start" ":prompt") nil t))
         (let ((level (read-string "Level (number or 'auto'): " "auto")))
           (if (string= level "auto") :auto (string-to-number level)))))
  
  (let ((new-template (list key :description description
                           :file file :position position :level level)))
    (setq org-supertag-capture-templates
          (cons new-template
                (assoc-delete-all key org-supertag-capture-templates)))
    (message "Added template '%s': %s" key description)))

(defun org-supertag-capture-list-templates ()
  "List all available capture templates."
  (interactive)
  (with-output-to-temp-buffer "*Org-Supertag Capture Templates*"
    (princ "Org-Supertag Capture Templates\n")
    (princ "==============================\n\n")
    (dolist (template org-supertag-capture-templates)
      (let* ((key (car template))
             (props (cdr template))
             (desc (plist-get props :description))
             (file (plist-get props :file))
             (pos (plist-get props :position))
             (level (plist-get props :level)))
        (princ (format "Key: %s\n" key))
        (princ (format "  Description: %s\n" desc))
        (princ (format "  File: %s\n" (or file "[prompt user]")))
        (princ (format "  Position: %s\n" pos))
        (princ (format "  Level: %s\n\n" level))))))

(defun org-supertag-capture-set-default-template (template-key)
  "Set the default template for quick capture."
  (interactive
   (list (completing-read "Select default template: "
                         (mapcar #'car org-supertag-capture-templates)
                         nil t)))
  (setq org-supertag-capture-default-template template-key)
  (message "Default capture template set to: %s" template-key))
  
;;------------------------------------------------------------------------------
;; Location Selector Framework
;;------------------------------------------------------------------------------

(defun org-supertag-find-file-content-start ()
  "Find the position where actual content should start in current buffer.
This skips over the org-mode file header (#+TITLE:, #+AUTHOR:, etc.)
and returns the position where new content should be inserted."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (looking-at "^#\\+\\|^$"))
      (forward-line 1))
    (point)))

(defun org-supertag--select-location (&optional context-options)
  "Unified location and file selector for org-supertag.

CONTEXT-OPTIONS is an optional plist with:
  :prompt - Custom prompt message (default: \"Select location:\")
  :allow-create - Whether to allow file creation (default: t)
  :auto-level - Whether to automatically calculate heading level (default: t)

Returns:
  A plist with keys:
    :file - Target file path
    :position - Position specification '(:type type :data data)
    :level - Heading level for creation (auto-calculated)
    :context - Additional context information"
  (let* ((prompt (or (plist-get context-options :prompt) "Select location:"))
         (allow-create (or (plist-get context-options :allow-create) t))
         (auto-level (or (plist-get context-options :auto-level) t))
         (target-file (read-file-name
                       (if allow-create "Target file: " "Target file (existing): ")
                       nil nil (not allow-create)))
         (position-and-level (org-supertag--select-insert-position-with-level target-file auto-level)))
    
    (list :file target-file
          :position (plist-get position-and-level :position)
          :level (plist-get position-and-level :level)
          :context context-options)))

(defun org-supertag--select-insert-position-with-level (target-file auto-level)
  "Select insertion position within TARGET-FILE and calculate appropriate level.
Returns plist with :position and :level keys."
  (with-current-buffer (find-file-noselect target-file)
    (org-with-wide-buffer
     (let* ((headlines (org-map-entries
                       (lambda ()
                         (let* ((title (org-get-heading t t t t))
                                (level (org-outline-level))
                                (olp (org-get-outline-path))
                                (display (if olp
                                           (format "%s / %s"
                                                  (mapconcat 'identity olp " / ")
                                                  title)
                                         title)))
                           (list display (point) level)))))
            (options (append
                     '("File start" "File end")
                     (when headlines
                       '("Under heading" "After heading"))))
            (choice (completing-read "Insert position: " options nil t)))
       
       (cond
        ((string= choice "File start")
         (let ((pos (org-supertag-find-file-content-start)))
           `(:position (:type :content-start :data ,pos)
             :level ,(if auto-level 1 nil))))
        
        ((string= choice "File end")
         (let* ((pos (point-max))
                (level (if auto-level
                          (save-excursion
                            (goto-char (point-max))
                            (if (re-search-backward "^\\(\\*+\\)" nil t)
                                (length (match-string 1))
                              1))
                        nil)))
           `(:position (:type :end :data ,pos)
             :level ,level)))
        
        ((string= choice "Under heading")
         (let* ((selected (completing-read
                          "Select parent heading: "
                          (mapcar #'car headlines) nil t))
                (headline-info (cl-find selected headlines :key #'car :test #'string=))
                (parent-pos (nth 1 headline-info))
                (parent-level (nth 2 headline-info)))
           (goto-char parent-pos)
           (org-end-of-subtree t)
           (let ((insert-pos (point))
                 (child-level (if auto-level (1+ parent-level) nil)))
             `(:position (:type :under-heading :data ,insert-pos)
               :level ,child-level))))
        
        ((string= choice "After heading")
         (let* ((selected (completing-read
                          "Insert after heading: "
                          (mapcar #'car headlines) nil t))
                (headline-info (cl-find selected headlines :key #'car :test #'string=))
                (sibling-pos (nth 1 headline-info))
                (sibling-level (nth 2 headline-info)))
           (goto-char sibling-pos)
           (org-end-of-subtree t)
           (let ((insert-pos (point))
                 (same-level (if auto-level sibling-level nil)))
             `(:position (:type :after-heading :data ,insert-pos)
               :level ,same-level))))
        
        (t
         `(:position (:type :end :data ,(point-max))
           :level ,(if auto-level 1 nil))))))))

;;------------------------------------------------------------------------------
;; User-Facing Command and Integration (Phase 4)
;;------------------------------------------------------------------------------

(defun org-supertag-capture--write-field-to-property-drawer (field-name value)
  "Write a single FIELD-NAME and VALUE to the PROPERTIES drawer at point."
  (org-entry-put nil field-name value))

(defun org-supertag-capture--execute (template)
  "The core execution engine for capturing a node.
Takes a fully formed TEMPLATE plist and performs all necessary
actions: processing, inserting, syncing, and setting fields."
  (when template
    (let* ((processed-data (org-supertag-capture--process-template template))
           (node-string (car processed-data))
           (fields-to-set (cadr processed-data))
           (location (org-supertag-capture--get-location-from-template template))
           (target-file (plist-get location :file))
           (insert-pos (plist-get (plist-get location :position) :data))
           (node-id nil))

      ;; 1. Insert the node text into the target file
      (with-current-buffer (find-file-noselect target-file)
        (save-excursion
          (goto-char insert-pos)
          (unless (bolp) (insert "\n"))
          (insert node-string)
          (insert "\n")

          ;; 2. Sync the new entry to create the node and get its ID
          (org-back-to-heading t)
          (setq node-id (org-supertag-node-sync-at-point))

          ;; 3. Process all fields
          (when node-id
            (dolist (field fields-to-set)
              (let ((name (car field))
                    (value (cdr field)))
                ;; 3a. Always write to the database
                (org-supertag-field-set-value node-id name value)

                ;; 3b. Conditionally write to PROPERTIES drawer
                (when org-supertag-capture-sync-fields-to-properties
                  (org-supertag-capture--write-field-to-property-drawer name value))))
            (message "Node %s captured with %d fields." node-id (length fields-to-set)))
          (unless node-id
            (error "Failed to create or sync node after insertion.")))
        (save-buffer)))))

(defun org-supertag-capture--get-location-from-template (template)
  "Get location details from a TEMPLATE plist."
  (let ((position-type (plist-get template :position)))
    (if (eq position-type :prompt)
        (org-supertag--select-location `(:prompt "Select capture location..." :file ,(plist-get template :file)))
      (list :file (expand-file-name (plist-get template :file))
            :position `(:type ,(or position-type :end) :data ,(with-current-buffer (find-file-noselect (plist-get template :file)) (point-max)))))))

;;;###autoload
(defun org-supertag-capture-template ()
  "Create a new node by selecting a template."
  (interactive)
  (let* ((template-key (completing-read "Choose capture template: "
                                        (mapcar #'car org-supertag-capture-templates)))
         (template (cdr (assoc template-key org-supertag-capture-templates))))
    (org-supertag-capture--execute template)))

;;;###autoload
(defun org-supertag-capture-direct ()
  "Create a new node directly with interactive prompts."
  (interactive)
  (let* ((title (read-string "Node Title: "))
         (body (read-string "Node Body (optional): "))
         (location (org-supertag--select-location '(:prompt "Select capture location...")))
         (virtual-template `(:level ,(plist-get location :level)
                             :file ,(plist-get location :file)
                             :position ,(plist-get location :position)
                             :content ((:type :headline :source :static :value ,title)
                                       (:type :body :source :static :value ,body)))))
    (org-supertag-capture--execute virtual-template)))

(provide 'org-supertag-capture)

;;; org-supertag-capture.el ends here