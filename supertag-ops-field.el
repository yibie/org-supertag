;;; org-supertag/ops/field.el --- Field operations for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides standardized operations for managing field values
;; associated with nodes and tags in the Org-Supertag data-centric architecture.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'supertag-core-store)
(require 'supertag-core-schema)
(require 'supertag-core-transform)
(require 'supertag-core-scan)
(require 'supertag-ops-tag)

(defcustom supertag-debug-log-field-events nil
  "When non-nil, log detailed field mutation events and automation processing.
Useful for diagnosing field value loss or unexpected overwrites."
  :type 'boolean
  :group 'org-supertag)

(defconst supertag-field--missing (list :supertag-field-missing)
  "Sentinel used to detect missing field values.")

(defconst supertag-field--node-missing (list :supertag-field-node-missing)
  "Sentinel used to detect missing node field tables.")

(defconst supertag-field--tag-missing (list :supertag-field-tag-missing)
  "Sentinel used to detect missing tag field tables.")

(defun supertag-field--list-to-hash (data)
  "Convert DATA (alist or plist) into a hash table."
  (let ((table (ht-create)))
    (cond
     ((null data)
      table)
     ;; Alist: ((\"field\" . value) ...)
     ((and (listp data) (consp (car data)))
      (dolist (cell data table)
        (puthash (car cell) (cdr cell) table)))
     ;; Flat key/value list: (\"field\" value \"other\" value)
     ((and (listp data) (zerop (% (length data) 2)))
      (let ((cursor data))
        (while cursor
          (let ((key (pop cursor))
                (value (pop cursor)))
            (puthash key value table)))))
     (t
      ;; Fallback: store under :value to avoid dropping data.
      (puthash :value data table)))
    table))

(defun supertag-field--ensure-node-table (node-id &optional create)
  "Return hash table for NODE-ID in the :fields collection.
When CREATE is non-nil, allocate and store a new table if absent."
  (let* ((fields-root (supertag-store-get-collection :fields))
         (existing (gethash node-id fields-root supertag-field--node-missing)))
    (cond
     ((eq existing supertag-field--node-missing)
      (when create
        (let ((table (ht-create)))
          (puthash node-id table fields-root)
          table)))
     ((hash-table-p existing) existing)
     ((listp existing)
      (let ((table (supertag-field--list-to-hash existing)))
        (puthash node-id table fields-root)
        table))
     ((null existing)
      (when create
        (let ((table (ht-create)))
          (puthash node-id table fields-root)
          table)))
     (t
      (when create
        (let ((table (ht-create)))
          (puthash node-id table fields-root)
          table))))))

(defun supertag-field--ensure-tag-table (node-table tag-id &optional create)
  "Return hash table for TAG-ID inside NODE-TABLE.
When CREATE is non-nil, allocate and store a new table if absent."
  (let ((existing (gethash tag-id node-table supertag-field--tag-missing)))
    (cond
     ((eq existing supertag-field--tag-missing)
      (when create
        (let ((table (ht-create)))
          (puthash tag-id table node-table)
          table)))
     ((hash-table-p existing) existing)
     ((listp existing)
      (let ((table (supertag-field--list-to-hash existing)))
        (puthash tag-id table node-table)
        table))
     ((null existing)
      (when create
        (let ((table (ht-create)))
          (puthash tag-id table node-table)
          table)))
     (t
      (when create
        (let ((table (ht-create)))
          (puthash tag-id table node-table)
          table))))))

;; Note: Change notifications are now handled by the unified commit system
;; (supertag-ops-commit). No need for separate notification handling.

;;; --- Field Operations ---

;; 4.1 Field Value Operations

(defun supertag-field-set (node-id tag-id field-name value)
  "Set the value of a tag field for a node using the unified commit system.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field.
VALUE is the field value.
Returns the updated field value."
  (let* ((old-raw (supertag-field-get node-id tag-id field-name supertag-field--missing))
         (old-value (if (eq old-raw supertag-field--missing) nil old-raw))
         (event-old (when (not (eq old-raw supertag-field--missing))
                      (list :node-id node-id :tag-id tag-id :field-name field-name :value old-value)))
         (event-new (list :node-id node-id :tag-id tag-id :field-name field-name :value value)))
    (if (and (not (eq old-raw supertag-field--missing))
             (equal old-value value))
        (progn
          (when supertag-debug-log-field-events
            (message "supertag-field-set SKIP %s/%s/%s unchanged=%S"
                     node-id tag-id field-name value))
          value)
      (progn
        (supertag-ops-commit
         :operation :field-set
         :collection :fields
         :id node-id
         :path (list :fields node-id tag-id field-name)
         :previous event-old
         :new event-new
         :context (list :node-id node-id :tag-id tag-id :field-name field-name)
         :perform (lambda ()
                    (let* ((node-table (supertag-field--ensure-node-table node-id t))
                           (tag-table (supertag-field--ensure-tag-table node-table tag-id t)))
                      (puthash field-name value tag-table)
                      (when supertag-debug-log-field-events
                        (message "supertag-field-set WRITE %s/%s/%s old=%S new=%S"
                                 node-id tag-id field-name old-value value))
                      event-new))))
        value)))

(defun supertag-field-get (node-id tag-id field-name &optional default)
  "Get the value of a tag field for a node.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field.
DEFAULT is the default value to return if the field does not exist.
Returns the field value, or DEFAULT if it does not exist."
  (let ((node-table (supertag-field--ensure-node-table node-id)))
    (if (not node-table)
        default
      (let ((tag-table (supertag-field--ensure-tag-table node-table tag-id)))
        (if (not tag-table)
            default
          (let ((value (gethash field-name tag-table supertag-field--missing)))
            (if (eq value supertag-field--missing) default value)))))))

(defun supertag-field-rename (tag-id old-name new-name)
  "Rename a field on TAG-ID from OLD-NAME to NEW-NAME across schema and data.
Signals an error if the source field is missing or the target already exists.
All node field values are migrated atomically inside a transaction."
  (unless (and (stringp tag-id) (not (string-empty-p tag-id)))
    (error "Invalid tag id: %S" tag-id))
  (unless (and (stringp old-name) (not (string-empty-p old-name)))
    (error "Invalid old field name: %S" old-name))
  (unless (and (stringp new-name) (not (string-empty-p new-name)))
    (error "Invalid new field name: %S" new-name))
  (when (string= old-name new-name)
    (cl-return-from supertag-field-rename
      (list :status :skipped :reason "Names are identical")))
  (let ((existing (supertag-tag-get-field tag-id old-name))
        (target (supertag-tag-get-field tag-id new-name)))
    (unless existing
      (error "Field '%s' not found on tag '%s'" old-name tag-id))
    (when target
      (error "Field '%s' already exists on tag '%s'" new-name tag-id)))
  (cl-labels
      ((rewrite-field-name (value)
         (cond
          ((and (stringp value) (string= value old-name)) new-name)
          ((keywordp value)
           (let ((name (substring (symbol-name value) 1)))
             (if (string= name old-name)
                 (intern (concat ":" new-name))
               value)))
          ((symbolp value)
           (let ((name (symbol-name value)))
             (if (string= name old-name)
                 (intern new-name)
               value)))
          (t value)))
       (ensure-plist (data)
         (cond
          ((hash-table-p data)
           (let (plist)
             (maphash (lambda (k v)
                        (setq plist (plist-put plist k v)))
                      data)
             plist))
          ((listp data) (copy-tree data))
          (t data))))
    (supertag-with-transaction
      ;; Update tag definition (rebuilds schema cache internally).
      (supertag-tag-rename-field tag-id old-name new-name)
      ;; Migrate stored values for each node using this tag.
      (let ((sentinel supertag-field--missing))
        (dolist (node-id (supertag-index-get-nodes-by-tag tag-id))
          (let ((value (supertag-field-get node-id tag-id old-name sentinel)))
            (unless (eq value sentinel)
              (let ((existing-new (supertag-field-get node-id tag-id new-name sentinel)))
                (when (not (eq existing-new sentinel))
                  (error "Cannot rename: node %s already has value for field '%s' on tag '%s'"
                         node-id new-name tag-id)))
              (supertag-field-set node-id tag-id new-name value)
              (supertag-field-remove node-id tag-id old-name)))))
      ;; Update relation metadata that references this field.
      (let ((relations (supertag-store-get-collection :relations)))
        (maphash
         (lambda (rel-id rel-data)
           (let* ((rel-plist (ensure-plist rel-data))
                  (updated nil))
             ;; Update :sync-fields list.
             (when-let ((fields (plist-get rel-plist :sync-fields)))
               (let ((new-fields (mapcar #'rewrite-field-name fields)))
                 (unless (equal new-fields fields)
                   (setq rel-plist (plist-put rel-plist :sync-fields new-fields))
                   (setq updated t))))
             ;; Update :rollup-field if present.
             (when-let ((rollup (plist-get rel-plist :rollup-field)))
               (let ((new-rollup (rewrite-field-name rollup)))
                 (unless (equal new-rollup rollup)
                   (setq rel-plist (plist-put rel-plist :rollup-field new-rollup))
                   (setq updated t))))
             ;; Update nested :props keys that may reference field names.
             (when-let ((props (plist-get rel-plist :props)))
               (let* ((props-plist (ensure-plist props))
                      (props-updated nil))
                 (dolist (key '(:from-property :to-property :rollup-field))
                   (when (plist-member props-plist key)
                     (let* ((val (plist-get props-plist key))
                            (new-val (rewrite-field-name val)))
                       (unless (equal new-val val)
                         (setq props-plist (plist-put props-plist key new-val))
                         (setq props-updated t)))))
                 (when props-updated
                   (setq rel-plist (plist-put rel-plist :props props-plist))
                   (setq updated t))))
             (when updated
               (supertag-store-put-entity :relations rel-id rel-plist t))))
         relations))
      (list :status :renamed :tag-id tag-id :from old-name :to new-name))))

(defun supertag-field-get-with-default (node-id tag-id field-name)
  "Get field value for NODE-ID/TAG-ID/FIELD-NAME, falling back to schema default.
Returns the stored field value when present. If the field is unset, returns
the :default from the field definition, evaluating functional defaults when
necessary. Signals nil when the field definition cannot be found."
  (let ((value (supertag-field-get node-id tag-id field-name supertag-field--missing)))
    (if (eq value supertag-field--missing)
        (when-let ((field-def (supertag-tag-get-field tag-id field-name)))
          (let ((default (plist-get field-def :default)))
            (if (functionp default) (funcall default) default)))
      value)))

(defun supertag-field-remove (node-id tag-id field-name)
  "Remove the value of a tag field for a node using the unified commit system.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field.
Returns the removed field value."
  (let* ((old-raw (supertag-field-get node-id tag-id field-name supertag-field--missing))
         (old-value (if (eq old-raw supertag-field--missing) nil old-raw))
         (event-old (when (not (eq old-raw supertag-field--missing))
                      (list :node-id node-id :tag-id tag-id :field-name field-name :value old-value)))
         (event-new (list :node-id node-id :tag-id tag-id :field-name field-name :value nil)))
    (if (eq old-raw supertag-field--missing)
        nil
      ;; Use unified commit system for field operations
      (progn
        (supertag-ops-commit
         :operation :field-remove
         :collection :fields
         :id node-id
         :path (list :fields node-id tag-id field-name)
         :previous event-old
         :new event-new
         :context (list :node-id node-id :tag-id tag-id :field-name field-name)
         :perform (lambda ()
                    (let* ((node-table (supertag-field--ensure-node-table node-id))
                           (tag-table (and node-table (supertag-field--ensure-tag-table node-table tag-id))))
                      (when (and node-table tag-table)
                        (remhash field-name tag-table))
                      event-old))))
        old-value)))

;; 4.2 Field Validation and Normalization

(defun supertag-field-validate (tag-id field-name value)
  "Validate a field value against the tag's field definition.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field.
VALUE is the value to validate.
Returns t if validation passes, otherwise nil."
  (let ((field-def (supertag-tag-get-field tag-id field-name)))
    (unless field-def
      (error "Field '%s' not defined for tag '%s'." field-name tag-id))

    (let ((type (plist-get field-def :type))
          (options (plist-get field-def :options))
          (validator (plist-get field-def :validator)))
      (and
       ;; Type check
       (supertag--convert-type value type) ; Will signal error if type conversion fails
       ;; Options check
       (or (null options) (member value options))
       ;; Custom validator
       (or (null validator) (funcall validator value))))))

(defun supertag-field-normalize (tag-id field-name value)
  "Normalize a field value according to the tag's field definition.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field.
VALUE is the value to normalize.
Returns the normalized value."
  (let ((field-def (supertag-tag-get-field tag-id field-name)))
    (unless field-def
      (error "Field '%s' not defined for tag '%s'." field-name tag-id))

    (let ((type (plist-get field-def :type))
          (default-value (plist-get field-def :default)))
      (cond
       ;; Apply default value if value is nil and default is specified
       ((and (null value) default-value)
        (if (functionp default-value) (funcall default-value) default-value))
       ;; Convert type
       (type (supertag--convert-type value type))
       ;; Otherwise, return as is
       (t value)))))

;; 4.3 Interactive Field Definition Utilities

(defun supertag-field-read-date-value (&optional prompt)
  "Interactive helper to read a date value with user-friendly options.
PROMPT is the optional prompt string to display.
Returns a date string in a format supported by supertag--convert-to-timestamp."
  (let* ((prompt (or prompt "Enter date: "))
         (choices '("today" "tomorrow" "yesterday"
                   "+1 day" "+3 days" "+7 days" "+1 week" "+1 month"
                   "-1 day" "-3 days" "-7 days" "-1 week" "-1 month"
                   "Use org-read-date (calendar picker)"
                   "Enter custom format"))
         (choice (completing-read
                 (concat prompt "(choose option or type directly): ")
                 choices nil nil)))
    (cond
     ;; User selected a predefined option
     ((member choice choices)
      (cond
       ((string= choice "Use org-read-date (calendar picker)")
        ;; Use org-mode's built-in date picker
        (require 'org)
        (format-time-string "%Y-%m-%d" (org-read-date t t)))
       ((string= choice "Enter custom format")
        ;; Let user enter custom format with help
        (read-string
         "Enter date (formats: 2024-01-15, today, +3 days): "))
       (t choice))) ; Return the predefined choice directly
     ;; User typed something directly
     (t choice))))

(defun supertag-field-read-timestamp-value (&optional prompt)
  "Interactive helper to read a timestamp value (usually auto-generated).
PROMPT is the optional prompt string to display.
For timestamp fields, usually auto-generation is preferred."
  (let* ((prompt (or prompt "Set timestamp: "))
         (choices '("now (current time)"
                   "Use org-read-date (specific date & time)"
                   "Enter ISO format (2024-01-15 14:30)"
                   "Enter custom format"))
         (choice (completing-read
                 (concat prompt "(choose option): ")
                 choices nil t)))
    (cond
     ((string= choice "now (current time)")
      "now")
     ((string= choice "Use org-read-date (specific date & time)")
      ;; Use org-mode's built-in date picker with time
      (require 'org)
      (org-read-date t t nil "Select date and time: "))
     ((string= choice "Enter ISO format (2024-01-15 14:30)")
      (read-string "Enter timestamp (YYYY-MM-DD HH:MM): "))
     ((string= choice "Enter custom format")
      (read-string "Enter timestamp (formats: now, 2024-01-15 14:30): "))
     (t choice))))

(defun supertag-field-read-type-with-options (current-type)
  "Interactively read a field type and options for :options type.
CURRENT-TYPE is the current type of the field (used as default).
Returns a cons cell (TYPE . OPTIONS) where OPTIONS is a list for
:options type or nil for other types."
  (let* ((type-descriptions '((:string . "string - Plain text")
                              (:number . "number - Numeric value")
                              (:integer . "integer - Whole number")
                              (:boolean . "boolean - True/False")
                              (:date . "date - User-input date (supports: 2024-01-15, today, +3 days)")
                              (:timestamp . "timestamp - Auto-generated timestamp (created/modified time)")
                              (:options . "options - Multiple choice")
                              (:url . "url - Web address")
                              (:email . "email - Email address")
                              (:tag . "tag - Tag reference(s)")))
         (type-choices (mapcar (lambda (type)
                                 (let ((desc (alist-get type type-descriptions)))
                                   (or desc (symbol-name type))))
                               supertag-field-types))
         (current-type-desc (alist-get current-type type-descriptions))
         (type-str-selected (completing-read "Field type: " type-choices nil t current-type-desc))
         ;; Extract the actual type symbol from the selection
         (new-type (or (car (rassoc type-str-selected type-descriptions))
                       (cl-find-if (lambda (type-sym)
                                     (string= (symbol-name type-sym) type-str-selected))
                                   supertag-field-types)
                       current-type))) ; fallback to current type
    (if (eq new-type :options)
        (let* ((options-input (read-string "Options (comma separated): "))
               (options-list (split-string options-input "," t "[ \t\n\r]+")))
          (cons new-type options-list))
      (cons new-type nil))))

(defun supertag-field-read-value-with-type-assistance (field-type &optional prompt current-value)
  "Read a field value with type-specific assistance.
FIELD-TYPE is the field type (e.g., :timestamp, :boolean, :options).
PROMPT is the optional prompt string.
CURRENT-VALUE is the current value (for editing).
Returns the user input appropriate for the field type."
  (let ((prompt (or prompt (format "Enter %s value: " (substring (symbol-name field-type) 1)))))
    (pcase field-type
      (:timestamp (supertag-field-read-timestamp-value prompt))
      (:boolean (if (y-or-n-p (or prompt "Enable this option? ")) "true" "false"))
      (:date (supertag-field-read-date-value prompt))
      (:options (read-string prompt current-value)) ; Could be enhanced further
      (:integer (read-string prompt (if current-value (format "%s" current-value) "")))
      (:number (read-string prompt (if current-value (format "%s" current-value) "")))
      (_ (read-string prompt current-value)))))

(provide 'supertag-ops-field)
