;;; org-supertag-field.el --- Field system for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Core functionality for field system:
;; - Field type definitions
;; - Field value access
;; - Org integration
;; - Field synchronization

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-supertag-db)

;;------------------------------------------------------------------------------
;; Field Value Database Operations
;;------------------------------------------------------------------------------

(defun org-supertag-field-db-set-value (node-id field-name value tag-id)
  "Set field value to database.
NODE-ID: Node identifier
FIELD-NAME: Field name
VALUE: Field value
TAG-ID: Associated tag identifier"
  (let ((link-id (format ":node-field:%s->%s" node-id field-name))
        (props (list :from node-id
                    :to field-name
                    :tag-id tag-id
                    :value value
                    :created-at (current-time))))
    (puthash link-id props org-supertag-db--link)))

(defun org-supertag-field-db-get-value (node-id field-name tag-id)
  "Get field value from database.
NODE-ID: Node identifier
FIELD-NAME: Field name
TAG-ID: Associated tag identifier"
  (let* ((link-id (format ":node-field:%s->%s" node-id field-name))
         (link (gethash link-id org-supertag-db--link)))
    (when (and link 
               (equal (plist-get link :tag-id) tag-id))
      (plist-get link :value))))

(defun org-supertag-field-db-delete-value (node-id field-name tag-id)
  "Delete field value from database.
NODE-ID: Node identifier
FIELD-NAME: Field name
TAG-ID: Associated tag identifier"
  (let ((link-id (format ":node-field:%s->%s" node-id field-name)))
    (remhash link-id org-supertag-db--link)))

;;----------------------------------------------------------------------
;; Field Value Entity Operations
;;----------------------------------------------------------------------

(defun org-supertag-field-set-value (field-def value node-id tag-id)
  "Set field value.
FIELD-DEF: Field definition
VALUE: Field value
NODE-ID: Node identifier
TAG-ID: Associated tag identifier"
  (condition-case err
      (let* ((field-name (plist-get field-def :name))
             (field-type (plist-get field-def :type))
             (type-def (org-supertag-get-field-type field-type))
             (processed-value (if type-def
                                (progn
                                  (org-supertag-field-validate field-def value)
                                  (if-let ((formatter (plist-get type-def :formatter)))
                                      (funcall formatter value field-def)
                                    value))
                              value)))
        (org-supertag-field-db-set-value node-id field-name processed-value tag-id)
        (when-let ((pos (condition-case nil
                           (org-id-find node-id t)
                         (error nil))))
          (condition-case sync-err
              (org-with-point-at pos
                (org-entry-put nil field-name processed-value))
            (error
             (message "Failed to sync field value to org buffer: %s"
                     (error-message-string sync-err))))))
    (error
     (message "Error in field set-value operation: %s"
              (error-message-string err))
     (signal (car err) (cdr err)))))

(defun org-supertag-field-validate (field value)
  "Validate field value.
FIELD: Field definition
VALUE: Value to validate
Returns t if valid, otherwise throws error"
  (let* ((name (plist-get field :name))
         (type (plist-get field :type))
         (required (plist-get field :required))
         (type-def (org-supertag-get-field-type type))
         (validator (plist-get type-def :validator)))
    (when (and required (null value))
      (error "Field '%s' is required" name))
    (if (null value)
        t
      (progn
        (when (and (eq type 'string) (not (stringp value)))
          (error "Field '%s' requires string type, got %S" name (type-of value)))
        (when validator
          (condition-case err
              (unless (if (eq type 'options)
                         (funcall validator value field)
                       (funcall validator value))
                (error "Value '%s' does not meet %s type requirements" value type))
            (error
             (error "Error validating field '%s': %s" name (error-message-string err)))))
        t))))

(defun org-supertag-field-get-value (field-def node-id tag-id)
  "Get field value.
FIELD-DEF: field definition
NODE-ID: node ID
TAG-ID: tag ID"
  (let* ((field-name (plist-get field-def :name))
         (type (plist-get field-def :type))
         (type-spec (org-supertag-get-field-type type))
         (value (org-supertag-field-db-get-value node-id field-name tag-id)))
    (when (and value
               (plist-get type-spec :formatter))
      (setq value (funcall (plist-get type-spec :formatter) value field-def)))
    value))

(defun org-supertag-field-remove-value (field-def node-id tag-id)
  "Remove field value.
FIELD-DEF: field definition
NODE-ID: node ID
TAG-ID: tag ID"
  (let ((field-name (plist-get field-def :name)))
    (org-supertag-db-remove-link :node-field node-id field-name
                                `(:tag-id ,tag-id))
    (org-with-point-at (org-id-find node-id t)
      (org-entry-delete nil field-name))))

(defun org-supertag-field-batch-set-value (field-def-list value-list node-id tag-id)
  "Set multiple field values in batch.
FIELD-DEF-LIST: List of field definitions to set
VALUE-LIST: List of values corresponding to the fields
NODE-ID: ID of the target node
TAG-ID: ID of the associated tag"
  (cl-loop for field-def in field-def-list
           for value in value-list
           do (org-supertag-field-set-value field-def value node-id tag-id)))

(defun org-supertag-field-find-nodes (field-name value &optional tag-id)
  "Find nodes that have a specific field value.
FIELD-NAME: Name of the field to search
VALUE: Value to match against
TAG-ID: Optional tag ID to limit search scope

Returns a list of nodes that match the criteria."
  (let ((links (org-supertag-db-get-links :node-field)))
    (cl-remove-if-not
     (lambda (link)
       (and (equal (car link) field-name)  ; Match field name
            (equal (plist-get (cadr link) :value) value)  ; Match value
            (or (null tag-id)  ; If no tag ID specified
                (equal (plist-get (cadr link) :tag-id) tag-id)))) ; Match tag ID
     links)))

;;----------------------------------------------------------------------
;; Field Type System
;;----------------------------------------------------------------------

(defvar org-supertag-field-types
  '((string . (:validator org-supertag-validate-string
               :formatter org-supertag-format-string
               :reader org-supertag-read-string-field
               :description "Plain text"))
    (options . (:validator org-supertag-validate-options
                :formatter org-supertag-format-options
                :reader org-supertag-read-options-field
                :description "Options"))
    (number . (:validator org-supertag-validate-number
               :formatter org-supertag-format-number
               :reader org-supertag-read-number-field
               :description "Number"))
    (date . (:validator org-supertag-validate-date
             :formatter org-supertag-format-date
             :reader org-supertag-read-date-field
             :description "Date"))
    (timestamp . (:validator org-supertag-validate-timestamp
             :formatter org-supertag-format-timestamp
             :reader org-supertag-read-timestamp-field
             :description "Timestamp"))
    (reference . (:validator org-supertag-validate-reference
                  :formatter org-supertag-format-reference
                  :reader org-supertag-read-reference-field
                  :description "Reference"))
    (tags . (:validator org-supertag-validate-tags
             :formatter org-supertag-format-tags
             :reader org-supertag-read-tags-field
             :description "Tags"))
    (list . (:validator org-supertag-validate-list
             :formatter org-supertag-format-list
             :reader org-supertag-read-list-field
             :description "List"))
    (range . (:validator org-supertag-validate-range
              :formatter org-supertag-format-range
              :reader org-supertag-read-range-field
              :description "Number Range")))
  "Field type definition.
Each type is a cons cell, car is the type name (symbol), cdr is the type definition plist, containing:
- :validator   Function to validate field value
- :formatter   Function to format field value
- :reader      Function to read field value
- :description Description of the field type")

(defun org-supertag-get-field-types ()
  "Get all supported field types.
Return a list of ((display-name . type-symbol) ...)."
  (mapcar (lambda (type-def)
            (let* ((type-name (car type-def))
                   (type-spec (cdr type-def))
                   (description (plist-get type-spec :description)))
              (cons (or description (symbol-name type-name))
                    type-name)))
          org-supertag-field-types))

(defun org-supertag-get-field-type (type)
  "Get field type definition.
TYPE is the type symbol.
Return the type definition plist, containing :validator, :formatter, :reader and :description."
  (message "Debug - Getting field type: %S" type)
  (let ((type-def (alist-get type org-supertag-field-types)))
    (message "Debug - Field type definition: %S" type-def)
    type-def))

;;----------------------------------------------------------------------
;; Field Value Validators and Formatters
;;----------------------------------------------------------------------

(defun org-supertag-validate-string (value)
  "Validate string value.
VALUE: Value to validate.
Return t if VALUE is nil or a non-empty string."
  (or (null value)                    ; Allow nil value
      (and (stringp value)           ; Must be string if not nil
           (not (string-empty-p (string-trim value))))))

(defun org-supertag-format-string (value field)
  "Format string VALUE.
VALUE is the value to format.
FIELD is the field definition."
  (message "Debug - Formatting string value: %S" value)
  (when value
    (string-trim value)))

(defun org-supertag-validate-date (value)
  "Validate date VALUE.
VALUE is the value to validate."
  (message "Debug - Validating date value: %S" value)
  (condition-case err
      (progn
        ;; Handle time value returned by org-read-date
        (when (listp value)
          (setq value (format-time-string "%Y-%m-%d" value)))
        ;; Try parsing if string
        (when (stringp value)
          (org-parse-time-string value))
        t)
    (error
     (message "Date validation error: %S" err)
     nil)))

(defun org-supertag-format-date (value field)
  "Format date value.
VALUE is the value to format.
FIELD is the field definition."
  (message "Debug - Formatting date value: %S" value)
  (condition-case err
      (cond
       ;; Handle time value from org-read-date
       ((listp value)
        (format-time-string "%Y-%m-%d" value))
       ;; Handle string format
       ((stringp value)
        (let ((time (org-parse-time-string value)))
          (format-time-string "%Y-%m-%d" (apply #'encode-time time))))
       ;; Return original for other cases
       (t value))
    (error
     (message "Date formatting error: %S" err)
     value)))

(defun org-supertag-validate-timestamp (value field-def)
  "Validate timestamp value.
VALUE: Timestamp value to validate.
FIELD-DEF: Field definition."
  (condition-case nil
      (when value
        (org-parse-time-string value)  ; Try parsing time string
        t)
    (error nil)))

(defun org-supertag-format-timestamp (value field-def)
  "Format timestamp value.
VALUE: Timestamp value to format.
FIELD-DEF: Field definition."
  (when value
    (let ((time (org-parse-time-string value)))
      (format-time-string "%Y-%m-%d %H:%M" 
                         (apply #'encode-time time)))))

(defun org-supertag-validate-email (value)
  "Validate email VALUE."
  (and (stringp value)
       (string-match-p "^[^@]+@[^@]+\\.[^@]+$" value)))

(defun org-supertag-format-email (value)
  "Format email VALUE."
  (string-trim value))

(defun org-supertag-validate-url (value)
  "Validate URL VALUE."
  (and (stringp value)
       (string-match-p "^https?://" value)))

(defun org-supertag-format-url (value)
  "Format URL VALUE."
  (string-trim value))

(defun org-supertag-validate-reference (value)
  "Validate reference value.
VALUE should be a valid org-id."
  (message "Debug - Validating reference value: %S" value)
  (when value
    (let ((node-ids (org-supertag-get-all-node-ids)))
      (message "Debug - Available node IDs: %S" node-ids)
      (member value node-ids))))

(defun org-supertag-format-reference (value field)
  "Format reference value.
VALUE is the value to format.
FIELD is the field definition."
  (message "Debug - Formatting reference value: %S" value)
  (when value
    (let ((node-ids (org-supertag-get-all-node-ids)))
      (if (member value node-ids)
          (org-with-point-at (org-id-find value t)
            (org-get-heading t t t t))
        value))))

(defun org-supertag-validate-options (value field)
  "Validate options value.
VALUE is the value to validate.
FIELD is the field definition containing :options."
  (let ((options (plist-get field :options)))
    (message "Debug - Validating options: value=%S, options=%S" value options)
    (and (stringp value) (member value options))))

(defun org-supertag-format-options (value field)
  "Format options value.
VALUE is the value to format.
FIELD is the field definition."
  (message "Debug - Formatting options value: %S" value)
  (when value
    (string-trim value)))

(defun org-supertag-validate-number (value)
  "Validate numeric value.
VALUE can be a number or numeric string."
  (or (numberp value)
      (and (stringp value)
           (string-match-p "^[0-9.]+$" value))))

(defun org-supertag-format-number (value field)
  "Format numeric value.
VALUE can be a number or numeric string.
FIELD is the field definition."
  (message "Debug - Formatting number value: %S" value)
  (when value
    (if (numberp value)
        (number-to-string value)
      (if (string-match-p "^[0-9.]+$" value)
          value
        (number-to-string (string-to-number value))))))

(defun org-supertag-validate-list (value)
  "Validate list value.
VALUE is the value to validate."
  (message "Debug - Validating list value: %S" value)
  (or (listp value)
      (and (stringp value)
           (string-match-p "^\\[.*\\]$" value))))

(defun org-supertag-format-list (value field)
  "Format list value.
VALUE is the value to format.
FIELD is the field definition."
  (message "Debug - Formatting list value: %S" value)
  (cond
   ((listp value)
    (format "%S" value))
   ((stringp value)
    (if (string-match-p "^\\[.*\\]$" value)
        value
      (format "[%s]" value)))
   (t (format "[%s]" value))))

(defun org-supertag-read-list-field (prompt)
  "Read list field value.
PROMPT is the prompt message."
  (message "Debug - Reading list field with prompt: %s" prompt)
  (let* ((input (read-string (format "%s (comma separated): " prompt)))
         (values (split-string input "," t "[ \t\n\r]+")))
    (message "Debug - List field input: %S -> %S" input values)
    values))

(defun org-supertag-validate-range (value)
  "Validate range value.
VALUE should be in 'min-max' format."
  (when value  ; Allow nil value
    (condition-case nil
        (let* ((parts (split-string value "-"))
               (min (string-to-number (car parts)))
               (max (string-to-number (cadr parts))))
          (and (= (length parts) 2)     ; Must have two parts
               (numberp min)             ; Min must be number
               (numberp max)             ; Max must be number
               (< min max)))            ; Min must be less than max
      (error nil))))

(defun org-supertag-format-range (value field-def)
  "Format range value.
VALUE is the range value.
FIELD-DEF is the field definition."
  value)  ; Return value as-is

(defun org-supertag-read-range-field (field-def)
  "Read range value.
FIELD-DEF is the field definition."
  (let ((value (read-string 
                (format "Enter range (format: min-max): "))))
    (if (org-supertag-validate-range value)
        value
      (error "Invalid range format, use 'min-max' format like '1-10'"))))

;;----------------------------------------------------------------------
;; Read Field Value
;;----------------------------------------------------------------------

(defun org-supertag-field-read-value (field)
  "Read field value.
FIELD is the field definition."
  (let* ((name (plist-get field :name))
         (type (plist-get field :type))
         (type-def (org-supertag-get-field-type type))
         (reader (plist-get type-def :reader))
         (formatter (plist-get type-def :formatter))
         (required (plist-get field :required))
         (options (plist-get field :options)))
    ;; 1. Ensure reader exists
    (unless reader
      (error "Field type %s has no reader function" type))
    ;; 2. Read value
    (catch 'done
      (while t
        (condition-case err
            (let* ((input-value
                    (if (eq type 'options)
                        (funcall reader name options)
                      (funcall reader name)))
                   (typed-value (org-supertag-field--convert-value type input-value)))
              ;; 3. Validate and format
              (if (org-supertag-field-validate field typed-value)
                  (throw 'done 
                         (if formatter
                             (funcall formatter typed-value field)
                           typed-value))
                (when (or required
                         (y-or-n-p (format "Field %s validation failed. Retry? " name)))
                  (sit-for 1))))
          (error
           (let ((err-msg (error-message-string err)))
             (message "Error - Processing field %s: %s" name err-msg)
             (when (or required
                      (y-or-n-p (format "Error processing field %s. Retry? " name)))
               (sit-for 1)))))))))

(defun org-supertag-field--convert-value (type value)
  "Convert value to specified type.
TYPE: Target type.
VALUE: Value to convert."
  (let ((type-spec (org-supertag-get-field-type type)))
    (when-let ((formatter (plist-get type-spec :formatter)))
      (funcall formatter value nil))))

(defun org-supertag-read-string-field (prompt)
  "Read string field value.
PROMPT is the prompt message."
  (message "Debug - Reading string field with prompt: %s" prompt)
  (let* ((raw-input (read-string (format "%s: " prompt)))
         (trimmed-input (string-trim raw-input))
         (result (if (string-empty-p trimmed-input)
                    nil
                  trimmed-input)))
    (message "Debug - String field: raw-input=%S, trimmed=%S, result=%S"
             raw-input trimmed-input result)
    result))

(defun org-supertag-read-date-field (prompt &optional default)
  "Read date field value.
PROMPT is the prompt message.
DEFAULT is the default value."
  (let* ((input (org-read-date nil t nil prompt nil default))
         (formatted-date (format-time-string "%Y-%m-%d" input)))
    (message "Debug - Date input from org-read-date: %S -> %S" input formatted-date)
    formatted-date))

(defun org-supertag-read-timestamp-field (prompt)
  "Read timestamp field value.
PROMPT is the prompt message."
  (let* ((time (org-read-date t t))  ; Use org-mode time reader with time
         (ts (org-timestamp-from-time time t)))  ; Convert to org timestamp
    (org-timestamp-format ts "%Y-%m-%d %H:%M")))  ; Format to standard format

(defun org-supertag-read-email-field (prompt &optional default)
  "Read email field value.
PROMPT is the prompt message.
DEFAULT is the default value."
  (let ((input (read-string (format "%s (example@domain.com)%s: "
                                   prompt
                                   (if default
                                       (format " (default: %s)" default)
                                     ""))
                           nil nil default)))
    (if (org-supertag-validate-email input)
        input
      (progn
        (message "Invalid email address, please try again")
        (sit-for 1)
        (org-supertag-read-email-field prompt default)))))

(defun org-supertag-read-url-field (prompt &optional default)
  "Read URL field value.
PROMPT is the prompt message
DEFAULT is the default value"
  (let ((input (read-string (format "%s (https://example.com)%s: "
                                   prompt
                                   (if default
                                       (format " (default: %s)" default)
                                     ""))
                           nil nil default)))
    (if (org-supertag-validate-url input)
        input
      (progn
        (message "Invalid URL, please try again")
        (sit-for 1)
        (org-supertag-read-url-field prompt default)))))

(defun org-supertag-read-reference-field (prompt)
  "Read reference field value.
PROMPT is the prompt message"
  (message "Debug - Reading reference field...")
  (let ((node-ids (org-supertag-get-all-node-ids)))
    (if node-ids
        (let* ((nodes-with-titles 
                (mapcar (lambda (id)
                         (cons (org-with-point-at (org-id-find id t)
                                (org-get-heading t t t t))
                               id))
                       node-ids))
               (choice (completing-read prompt (mapcar #'car nodes-with-titles) nil t)))
          (cdr (assoc choice nodes-with-titles)))
      (user-error "No nodes available for reference"))))

(defun org-supertag-read-options-field (prompt options)
  "Read options field value.
PROMPT is the prompt message
OPTIONS is the list of available options"
  (let ((input (completing-read (format "%s (%s): " 
                                      prompt 
                                      (mapconcat #'identity options "/"))
                              options
                              nil t)))
    (if (member input options)
        input
      (progn
        (message "Please select from the given options")
        (sit-for 1)
        (org-supertag-read-options-field prompt options)))))

(defun org-supertag-read-number-field (prompt)
  "Read numeric field value.
PROMPT is the prompt message"
  (let ((input-str (read-string (format "%s: " prompt))))
    ;; Validate input contains only digits and decimal point
    (if (string-match-p "^[0-9.]+$" input-str)
        input-str  ; Return number as string
      (progn
        (message "Please enter a valid number")
        (sit-for 1)
        (org-supertag-read-number-field prompt)))))

;;----------------------------------------------------------------------
;; Preset Field
;;----------------------------------------------------------------------

(defcustom org-supertag-preset-fields
  '(("Priority" 
     :type options
     :values ("P1" "P2" "P3" "P4")
     :description "Task Priority")
    ("Status" 
     :type options
     :values ("To Do" "In Progress" "Done" "On Hold")
     :description "Task Status")
    ("Deadline" 
     :type date
     :description "Task Deadline")
    ("Progress" 
     :type number
     :min 0
     :max 100
     :description "Progress Bar")
    ("Who" 
     :type string
     :description "Who")
    ("Tags" 
     :type list
     :separator ","
     :description "Keywords or Tags")
    ("URL" 
     :type url
     :description "Related Links")
    ("Rating" 
     :type number
     :min 1 
     :max 5
     :description "Five-star Rating"))
  "Preset field definition list.
Each preset field is a (name . props) pair, where:
- name is the field name
- props is the field properties plist"
  :type '(repeat (cons string plist))
  :group 'org-supertag)

(defun org-supertag--parse-preset-field (field-def)
  "Parse preset field definition.
FIELD-DEF is the preset field definition"
  (let* ((name (car field-def))
         (props (cdr field-def)))
    (append 
     (list :name name
           :type (plist-get props :type))
     ;; 可选属性
     (when-let ((desc (plist-get props :description)))
       (list :description desc))
     (when-let ((values (plist-get props :values)))
       (list :options values))
     (when-let ((min (plist-get props :min)))
       (list :min min))
     (when-let ((max (plist-get props :max)))
       (list :max max))
     (when-let ((sep (plist-get props :separator)))
       (list :separator sep)))))

(defun org-supertag-get-preset-field (name)
  "Get preset field definition.
NAME is the field name"
  (when-let ((field-def (assoc name org-supertag-preset-fields)))
    (org-supertag--parse-preset-field field-def)))

(defun org-supertag-format-preset-field (preset)
  "Format preset field options.
PRESET is the preset field definition"
  (let ((name (car preset))
        (desc (plist-get (cdr preset) :description)))
    (format "- %s%s"
            name
            (if desc (format " (%s)" desc) ""))))

(provide 'org-supertag-field)
;;; org-supertag-field.el ends here
