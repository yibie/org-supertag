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
  "Set field value for node.
FIELD-DEF is the field definition
VALUE is the field value
NODE-ID is the target node ID
TAG-ID is the tag ID"
  (condition-case err
      (let* ((field-name (plist-get field-def :name))
             (field-type (plist-get field-def :type))
             (type-def (org-supertag-get-field-type field-type))
             (processed-value
              (progn
                (message "Debug - Setting field: name=%S, type=%S, initial-value=%S" 
                         field-name field-type value)
                (if (and type-def value)
                    (progn
                      (message "Debug - Processing value with type-def: %S" type-def)
                      (when-let ((validator (plist-get type-def :validator)))
                        (message "Debug - Validating with %S" validator)
                        (unless (funcall validator value)
                          (error "Value validation failed for %s: %S" field-name value)))
                      (if-let ((formatter (plist-get type-def :formatter)))
                          (progn
                            (message "Debug - Formatting with %S" formatter)
                            (funcall formatter value field-def))
                        value))
                  (progn
                    (message "Debug - Using raw value (no processing): %S" value)
                    value)))))
        
        (message "Debug - Final processed value: %S" processed-value)
        
        (when (eq field-type 'tag-reference)
          (message "Debug - Processing tag-reference: value=%S, processed=%S" 
                   value processed-value)
          (when-let ((old-value (org-supertag-field-get-value field-def node-id tag-id)))
            (org-supertag-node-db-remove-reference node-id old-value))
          (when processed-value
            (org-supertag-node-db-add-reference node-id processed-value)))
        
        (org-supertag-field-db-set-value node-id field-name processed-value tag-id)
        
        (when-let ((pos (condition-case nil
                           (org-id-find node-id t)
                         (error nil))))
          (org-with-point-at pos
            (org-set-property field-name processed-value))))
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
    (message "Debug - Validating field: name=%S, type=%S, value=%S" name type value)
    
    (when (and required (null value))
      (error "Field '%s' is required" name))
    
    (if (null value)
        t
      (progn
        (when (and (eq type 'string) (not (stringp value)))
          (error "Field '%s' requires string type, got %S" name (type-of value)))
        
        (when validator
          (condition-case err
              (if (not (funcall validator value))
                (error "Value '%s' does not meet validation requirements" value))
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
    (tag-reference . (:validator org-supertag-validate-tag-reference
                  :reader org-supertag-read-tag-reference-field
                  :description "Node Reference"))
    (list . (:validator org-supertag-validate-list
             :formatter org-supertag-format-list
             :reader org-supertag-read-list-field
             :description "List"))
    (range . (:validator org-supertag-validate-range
              :formatter org-supertag-format-range
              :reader org-supertag-read-range-field
              :description "Number Range"))
    (behavior . (:validator org-supertag-validate-behavior
                 :formatter org-supertag-format-behavior
                 :reader org-supertag-read-behavior-field
                 :description "Tag Behavior")))
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
VALUE can be either:
- An org timestamp string (e.g. \"[2025-01-14 Tue]\" or \"<2025-01-14 Tue>\")
- A time value from org-read-date
- A standard date string (e.g. \"2025-01-14\")"
  (message "Debug - Validating date value: %S" value)
  (condition-case err
      (cond
       ((and (stringp value)
             (string-match org-ts-regexp0 value))
        t)
       ((listp value)
        t)
       ((stringp value)
        (org-parse-time-string value)
        t)
       (t nil))
    (error
     (message "Date validation error: %S" err)
     nil)))

(defun org-supertag-format-date (value &optional _field)
  "Format date value to org active timestamp format.
VALUE can be either:
- An org timestamp string (e.g. \"[2025-01-14 Tue]\" or \"<2025-01-14 Tue>\")
- A time value from org-read-date
- A standard date string (e.g. \"2025-01-14\")"
  (message "Debug - Formatting date value: %S" value)
  (condition-case err
      (cond
       ((and (stringp value)
             (string-match org-ts-regexp0 value))
        (if (string-prefix-p "[" value)
            (concat "<" (substring value 1 -1) ">")
          value))
       ((listp value)
        (format-time-string "<%Y-%m-%d %a>" value))
       ((stringp value)
        (let ((time (org-parse-time-string value)))
          (format-time-string "<%Y-%m-%d %a>"
                            (apply #'encode-time time time))))
       (t value))
    (error
     (message "Date formatting error: %S" err)
     value)))

(defun org-supertag-validate-timestamp (value &optional _field-def)
  "Validate timestamp value.
VALUE can be either:
- An org timestamp string (e.g. \"[2025-01-14 Tue 10:30]\" or \"<2025-01-14 Tue 10:30>\")
- A time value from org-read-date
- A standard timestamp string (e.g. \"2025-01-14 10:30\")"
  (condition-case err
      (cond
       ((and (stringp value)
             (string-match org-ts-regexp value))
        t)
       ((listp value)
        t)
       ((stringp value)
        (org-parse-time-string value)
        t)
       (t nil))
    (error nil)))

(defun org-supertag-format-timestamp (value field-def)
  "Format timestamp value to org inactive timestamp format.
VALUE can be either:
- An org timestamp string (e.g. \"[2025-01-14 Tue 10:30]\" or \"<2025-01-14 Tue 10:30>\")
- A time value from org-read-date
- A standard timestamp string (e.g. \"2025-01-14 10:30\")"
  (when value
    (condition-case err
        (cond
         ((and (stringp value)
               (string-match org-ts-regexp value))
          (if (string-prefix-p "<" value)
              (concat "[" (substring value 1 -1) "]")
            value))
         ((listp value)
          (format-time-string "[%Y-%m-%d %a %H:%M]" value))
         ((stringp value)
          (let ((time (org-parse-time-string value)))
            (format-time-string "[%Y-%m-%d %a %H:%M]"
                              (apply #'encode-time time))))
         (t value))
      (error value))))

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

(defun org-supertag-validate-tag-reference (value)
  "Validate tag reference value.
VALUE should be a node ID (string) or nil, or an org-mode link format."
  (message "Debug - Validating tag reference: %S" value)
  (or (null value)  ; 允许 nil 值
      (and (stringp value)
           (let ((node-id (cond
                          ;; org-mode 链接格式：[[id:NODE-ID][TITLE]]
                          ((string-match "\\[\\[id:\\([^]]+\\)\\]" value)
                           (match-string 1 value))
                          ;; 纯 ID
                          (t value))))
             (message "Debug - Checking if node exists: %S" node-id)
             (let ((exists (org-supertag-node-db-exists-p node-id)))
               (message "Debug - Node exists? %S" exists)
               exists)))))


(defun org-supertag-field-get-reference-value (node-id tag-id field)
  "Get referenced field value and type.
NODE-ID: Current node ID
TAG-ID: Current tag ID
FIELD: Field definition containing tag reference"
  (when-let* ((ref-value (org-supertag-field-get-value field node-id tag-id))
              (ref-tag-id (plist-get ref-value :tag-id))
              (ref-field-name (plist-get ref-value :field))
              (ref-tag (org-supertag-tag-get ref-tag-id)))
    (list :field ref-field-name
          :value (org-supertag-field-get-value ref-field-name node-id ref-tag-id))))

(defun org-supertag-validate-options (value &optional field-def)
  "Validate options value.
VALUE is the value to validate.
FIELD-DEF is the optional field definition containing :options."
  (let ((options (when field-def (plist-get field-def :options))))
    (message "Debug - Validating options: value=%S, options=%S" value options)
    (cond
     ((null options) 
      (stringp value))
     (t (and (stringp value)
             (member value options))))))

(defun org-supertag-format-options (value field)
  "Format options value.
VALUE is the value to format.
FIELD is the field definition.

Returns a trimmed string if VALUE is a string, otherwise returns nil."
  (message "Debug - Formatting options value: %S" value)
  (when (and value (stringp value))  ; Only process if value is a non-nil string
    (string-trim value)))

(defun org-supertag-validate-number (value)
  "Validate numeric value.
VALUE can be a number or numeric string."
  (or (numberp value)
      (and (stringp value)
           (string-match-p "^[0-9.]+$" value))))

(defun org-supertag-format-number (value &optional _field)
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

(defun org-supertag-format-list (value &optional _field)
  "Format list value.
VALUE is the value to format.
FIELD is the field definition."
  (message "Debug - Formatting list value: %S" value)
  (cond
   ((null value) "[]")
   ((listp value)
    (format "[%s]" 
            (mapconcat (lambda (item) 
                        (format "\"%s\"" item))
                      value ",")))
   ((stringp value)
    (if (string-match-p "^\\[.*\\]$" value)
        (let ((json-array-type 'list)
              (json-false nil))
          (condition-case err
              (let ((parsed (json-read-from-string value)))
                (org-supertag-format-list parsed))
            (error value)))  ; If parsing fails, return original
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

;;---------------------------------------------------------------------------
;; Get Field Initial Value
;;---------------------------------------------------------------------------

(defun org-supertag-field-get-initial-value (field)
  "Get initial value for FIELD."
  (let ((field-type (plist-get field :type))
        (field-value (plist-get field :value))
        (field-options (plist-get field :options)))
    (message "Getting initial value for field: %S" field)
    (let ((initial-value
           (cond
            ((eq field-type 'behavior)
             field-value)
            ((and (eq field-type 'options) field-options)
             (car field-options))
            ((eq field-type 'date)
             (format-time-string "%Y-%m-%d"))
            ((eq field-type 'string)
             " ")  
            (t nil))))
      (message "Initial value for field %s (%s): %S" 
               (plist-get field :name)
               field-type
               initial-value)
      initial-value)))

;;------------------------------------------------------------------------------
;; Behavior Field Type
;;------------------------------------------------------------------------------

(defun org-supertag-validate-behavior (value)
  "Validate behavior field value.
VALUE should be a plist with :trigger and either :action or :style."
  (and (plistp value)
       (plist-member value :trigger)
       (memq (plist-get value :trigger) 
             '(:on-add :on-remove :on-change :on-schedule :always))
       (or (functionp (plist-get value :action))
           (plistp (plist-get value :style)))))

(defun org-supertag-format-behavior (value &optional _field)
  "Format behavior field value for display.
VALUE can be either:
- A string (behavior name)
- A behavior plist

Format example:
- Behavior[@important]
- Behavior[:on-add +action +style:📦]
- Behavior[:always +action]"
  (cond
   ((stringp value)
    (if-let ((behavior (gethash value org-supertag-behavior-registry)))
        (let ((trigger (plist-get behavior :trigger))
              (has-action (plist-get behavior :action))
              (style (plist-get behavior :style)))
          (format "Behavior[%s%s%s]"
                  (or trigger "nil")
                  (if has-action " +action" "")
                  (if style 
                      (format " +style:%s" 
                              (or (plist-get style :prefix) ""))
                    "")))
      (format "Behavior[%s]" value)))
   
   ((and (listp value) (keywordp (car value)))
    (let ((trigger (plist-get value :trigger))
          (has-action (plist-get value :action))
          (style (plist-get value :style)))
      (format "Behavior[%s%s%s]"
              (or trigger "nil")
              (if has-action " +action" "")
              (if style 
                  (format " +style:%s" 
                          (or (plist-get style :prefix) ""))
                ""))))
   
   (t "Behavior[nil]")))

(defun org-supertag-read-behavior-field (prompt &optional initial)
  "Read behavior field value.
PROMPT is the prompt string
INITIAL is the initial value."
  (let* ((trigger (intern 
                  (completing-read 
                   "Trigger: "
                   '("on-add" "on-remove" "on-change" "on-schedule" "always")
                   nil t nil nil "on-add")))
         (has-action (y-or-n-p "Add action? "))
         (action (when has-action
                  (read-from-minibuffer "Action (lambda form): "
                                      "(lambda (node-id)\n  )")))
         (has-style (y-or-n-p "Add style? "))
         (style (when has-style
                 (list :face (read-from-minibuffer "Face properties: "
                                                  "(:foreground \"blue\")")
                       :prefix (read-string "Prefix: " "📋")))))
    (list :trigger trigger
          :action (when has-action (eval (read action)))
          :style style)))

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
    (unless reader
      (error "Field type %s has no reader function" type))
    (when (and (eq type 'options) (not options))
      (let* ((options-input (read-string (format "%s options (comma separated): " name)))
             (options-values (split-string options-input "," t "[ \t\n\r]+")))
        (setq options options-values)
        (setq field (plist-put field :options options-values))))
    (catch 'done
      (while t
        (condition-case err
            (let* ((input-value
                    (if (eq type 'options)
                        (funcall reader name options)
                      (funcall reader name)))
                   (typed-value (org-supertag-field--convert-value type input-value)))
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
  (message "Debug - Converting value: type=%S, value=%S" type value)
  (let ((type-spec (org-supertag-get-field-type type)))
    (message "Debug - Type spec: %S" type-spec)
    (cond
     ((null value)
      (message "Debug - Value is nil, returning nil")
      nil)
     ((eq type 'tag-reference)
      (message "Debug - Processing tag-reference value: %S" value)
      (let* ((node-id (cond
                      ((stringp value) value)
                      ((listp value) (car value))
                      (t (format "%s" value))))
             (node (org-supertag-db-get node-id)))
        (if node
            (let ((title (or (plist-get node :title)
                           (format "Node %s" node-id))))
              (format "[[id:%s][%s]]" node-id title))
          ;; If the node is not found, return the original ID
          node-id)))
     (t
      (if-let ((formatter (plist-get type-spec :formatter)))
          (progn
            (message "Debug - Using formatter: %S" formatter)
            (funcall formatter value nil))
        (progn
          (message "Debug - No formatter found, using raw value")
          value))))))

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
  "Read date field value using org-mode's date reader.
PROMPT is the prompt message.
DEFAULT is the default value."
  (let* ((input (org-read-date nil t nil prompt nil default))
         (formatted-date (format-time-string "<%Y-%m-%d %a>" input)))
    (message "Debug - Date input from org-read-date: %S -> %S" input formatted-date)
    formatted-date))

(defun org-supertag-read-timestamp-field (prompt)
  "Read timestamp field value using org-mode's date reader.
PROMPT is the prompt message."
  (let* ((time (org-read-date t t))  ; Use org-mode time reader with time
         (formatted-ts (format-time-string "[%Y-%m-%d %a %H:%M]" time)))
    formatted-ts))

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

(defun org-supertag-read-options-field (prompt options)
  "Read options field value.
PROMPT is the prompt message
OPTIONS is the list of available options."
  (unless options
    (error "Options type field requires predefined options"))
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
    (if (string-match-p "^[0-9.]+$" input-str)
        input-str  ; Return number as string
      (progn
        (message "Please enter a valid number")
        (sit-for 1)
        (org-supertag-read-number-field prompt)))))

;;----------------------------------------------------------------------
;; Field Modification
;;----------------------------------------------------------------------

(defun org-supertag-tag-modify-field (tag-id field-name)
  "Modify field definition for TAG-ID's FIELD-NAME.
This will clear all existing values of this field across all nodes."
  (interactive
   (let* ((node-id (org-id-get))
          (tags (org-supertag-node-get-tags node-id))
          (tag-id (completing-read "Select tag: " tags nil t))
          (tag (org-supertag-tag-get tag-id))
          (fields (plist-get tag :fields))
          (field-name (completing-read 
                      "Select field to modify: "
                      (mapcar (lambda (field)
                              (plist-get field :name))
                             fields)
                      nil t)))
     (list tag-id field-name)))
  
  (let* ((tag (org-supertag-tag-get tag-id))
         (fields (plist-get tag :fields))
         (field (cl-find field-name fields
                        :key (lambda (f) (plist-get f :name))
                        :test #'equal))
         (current-type (plist-get field :type))
         (type-choices
          (mapcar (lambda (type-def)
                   (let ((type-name (car type-def))
                         (type-spec (cdr type-def)))
                     (format "%s - %s"
                             (symbol-name type-name)
                             (plist-get type-spec :description))))
                 org-supertag-field-types))
         (new-type-choice
          (completing-read 
           (format "Current type is %s. Select new type: " current-type)
           type-choices
           nil t nil nil
           (symbol-name current-type)))
         (new-type (intern (car (split-string new-type-choice " - "))))
         (new-name (read-string 
                   (format "Field name (current: %s): " field-name)
                   field-name)))
    
    (when (yes-or-no-p 
           (format "Modifying field will clear all existing values. Continue? "))
      (let* ((new-field (list :name new-name
                             :type new-type))
             (new-fields (mapcar 
                         (lambda (f)
                           (if (equal (plist-get f :name) field-name)
                               new-field
                             f))
                         fields))
             (new-tag (plist-put (copy-sequence tag) 
                                :fields new-fields)))
        
        (org-supertag-db-add tag-id new-tag)
        
        (let ((nodes (org-supertag-tag-get-nodes tag-id)))
          (dolist (node-id nodes)
            (org-supertag-field-remove-value field node-id tag-id)))
        
        (message "Field '%s' modified. You may now set new values." 
                 new-name)))))

(provide 'org-supertag-field)
;;; org-supertag-field.el ends here
