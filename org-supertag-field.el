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
(require 'org-supertag-util)

;;------------------------------------------------------------------------------
;; Field Value Database Operations
;;------------------------------------------------------------------------------

(defun org-supertag-field-set-value (node-id field-name value &optional tag-id)
  "Set a field value for a given node in the database.
This function creates or updates the node-field link.
If VALUE is a list, it will be serialized into a comma-separated string.
If VALUE is nil or an empty string, the field is removed."
  ;;(message "DEBUG: set-value node-id=%S field-name=%S value=%S (type=%S)" node-id field-name value (type-of value))
  (let ((link-id (format ":node-field:%s->%s" node-id field-name))
        (final-value (if (listp value)
                         (mapconcat #'identity value ",")
                       value)))
    ;;(message "DEBUG: set-value final-value=%S (type=%S)" final-value (type-of final-value))
    (if (and final-value (not (string-empty-p final-value)))
        (let ((props (list :type :node-field
                           :from node-id
                           :to field-name
                           :value final-value
                           :created-at (current-time))))
          (when tag-id
            (setq props (plist-put props :tag-id tag-id)))
          (puthash link-id props org-supertag-db--link))
      ;; If value is nil or empty, remove the link.
      (remhash link-id org-supertag-db--link))
    ;; Mark the database as dirty and schedule a save.
    (org-supertag-db--mark-dirty)
    (org-supertag-db--schedule-save)))

(defun org-supertag-field-get-value (node-id field-name &optional tag-id)
  "Get field value from database.
If TAG-ID is provided, it will ensure the field belongs to that tag."
  (let* ((link-id (format ":node-field:%s->%s" node-id field-name))
         (link (gethash link-id org-supertag-db--link)))
    (when (and link
               ;; If tag-id is provided, match it. If not, ignore tag-id.
               (or (not tag-id) (equal (plist-get link :tag-id) tag-id)))
      (plist-get link :value))))

(defun org-supertag-field-delete-value (node-id field-name &optional tag-id)
  "Delete field value from database.
If TAG-ID is provided, it will only delete if the field belongs to that tag."
  (let* ((link-id (format ":node-field:%s->%s" node-id field-name))
         (link (gethash link-id org-supertag-db--link)))
    (when (and link
               (or (not tag-id) (equal (plist-get link :tag-id) tag-id)))
      (remhash link-id org-supertag-db--link)
      (org-supertag-db--mark-dirty)
      (org-supertag-db--schedule-save))))


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
    (url . (:validator org-supertag-validate-url
            :formatter org-supertag-format-url
            :reader org-supertag-read-url-field
            :description "URL"))
    (email . (:validator org-supertag-validate-email
              :formatter org-supertag-format-email
              :reader org-supertag-read-email-field
              :description "Email"))
    (tag . (:validator org-supertag-validate-tag
            :formatter org-supertag-format-tag
            :reader org-supertag-read-multiple-tags-field
            :description "Tag Reference"))
    ;; (range . (:validator org-supertag-validate-range
    ;;           :formatter org-supertag-format-range
    ;;           :reader org-supertag-read-range-field
    ;;           :description "Number Range"))
)
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
  ;;(message "Debug - Getting field type: %S" type)
  (let ((type-def (alist-get type org-supertag-field-types)))
    ;;(message "Debug - Field type definition: %S" type-def)
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
  ;;(message "Debug - Formatting string value: %S" value)
  (when value
    (string-trim value)))

(defun org-supertag-validate-date (value)
  "Validate date VALUE.
VALUE can be either:
- An org timestamp string (e.g. \"[2025-01-14 Tue]\" or \"<2025-01-14 Tue>\")
- A time value from org-read-date
- A standard date string (e.g. \"2025-01-14\")"
  ;;(message "Debug - Validating date value: %S" value)
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
  ;;(message "Debug - Formatting date value: %S" value)
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

(defun org-supertag-format-email (value &optional _field)
  "Format email VALUE.
VALUE is the email value to format.
FIELD is the field definition (optional)."
  (when (and value (stringp value))
    (string-trim value)))

(defun org-supertag-validate-url (value)
  "Validate URL VALUE."
  (and (stringp value)
       (string-match-p "^https?://" value)))

(defun org-supertag-format-url (value &optional _field)
  "Format URL VALUE.
VALUE is the URL value to format.
FIELD is the field definition (optional)."
  (when (and value (stringp value))
    (string-trim value)))

(defun org-supertag-validate-tag (value &optional _field)
  "Validate tag VALUE. Can be a single tag string or a list of tags.
VALUE can contain any characters except colons and whitespace."
  (if (listp value)
      ;; If it's a list, validate every element
      (cl-every (lambda (v) (org-supertag-validate-tag v)) value)
    ;; If it's a single value, perform the original validation
    (when value
      (and (stringp value)
           (not (string-empty-p (string-trim value)))
           (not (string-match-p "[:[:space:]]" (string-trim value)))))))

;; format tag field, support multiple values serialization
(defun org-supertag-format-tag (value &optional field-def)
  "Format tag value for storage. If multiple, serialize as comma-separated string."
  (let ((is-multiple (and field-def (plist-get field-def :multiple))))
    (cond
     ((and is-multiple (listp value))
      (mapconcat #'identity value ","))
     ((listp value)
      (car value))
     (t value))))

(defun org-supertag-validate-options (value &optional field-def)
  "Validate options value.
VALUE is the value to validate.
FIELD-DEF is the optional field definition containing :options."
  (let ((options (when field-def (plist-get field-def :options))))
    ;;(message "Debug - Validating options: value=%S, options=%S" value options)
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
  ;;(message "Debug - Formatting options value: %S" value)
  (when (and value (stringp value))  ; Only process if value is a non-nil string
    (string-trim value)))

(defun org-supertag-validate-number (value &optional _field)
  "Validate numeric value.
VALUE can be a number or numeric string."
  (or (numberp value)
      (and (stringp value)
           (string-match-p "^[0-9.]+$" value))))

(defun org-supertag-format-number (value &optional _field)
  "Format numeric value.
VALUE can be a number or numeric string.
FIELD is the field definition."
  ;;(message "Debug - Formatting number value: %S" value)
  (when value
    (if (numberp value)
        (number-to-string value)
      (if (string-match-p "^[0-9.]+$" value)
          value
        (number-to-string (string-to-number value))))))



;; (defun org-supertag-validate-range (value)
;;   "Validate range value.
;; VALUE should be in 'min-max' format."
;;   (when value  ; Allow nil value
;;     (condition-case nil
;;         (let* ((parts (split-string value "-"))
;;                (min (string-to-number (car parts)))
;;                (max (string-to-number (cadr parts))))
;;           (and (= (length parts) 2)     ; Must have two parts
;;                (numberp min)             ; Min must be number
;;                (numberp max)             ; Max must be number
;;                (< min max)))            ; Min must be less than max
;;       (error nil))))

;; (defun org-supertag-format-range (value &optional _field)
;;   "Format range VALUE.
;; VALUE should be in 'min-max' format.
;; FIELD is the field definition (optional)."
;;   (when (and value (stringp value))
;;     (string-trim value)))


;;---------------------------------------------------------------------------
;; Get Field Initial Value
;;---------------------------------------------------------------------------

(defun org-supertag-field-get-initial-value (field)
  "Get initial value for FIELD.
Only returns a value if explicitly specified in the field definition."
  (let ((field-type (plist-get field :type))
        (field-value (plist-get field :value))
        (field-default (plist-get field :default)))
    ;;(message "Getting initial value for field: %S" field)
    (let ((initial-value
           (cond
            ;; If there's an explicit default value, use it
            (field-default field-default)
            ;; If there's a value in the field definition, use it
            (field-value field-value)
            ;; Otherwise return nil
            (t nil))))
      (message "Initial value for field %s (%s): %S" 
               (plist-get field :name)
               field-type
               initial-value)
      initial-value)))



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
              (let ((validator (plist-get type-def :validator)))
                (if (or (not validator) 
                        (not (fboundp validator))
                        (funcall validator typed-value field))
                    (throw 'done 
                           (if formatter
                               (funcall formatter typed-value field)
                             typed-value))
                  (when (or required
                           (y-or-n-p (format "Field %s validation failed. Retry? " name)))
                    (sit-for 1)))))
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
  ;;(message "Debug - Converting value: type=%S, value=%S" type value)
  (let ((type-spec (org-supertag-get-field-type type)))
    ;;(message "Debug - Type spec: %S" type-spec)
    (if (null value)
        (progn
          (message "Debug - Value is nil, returning nil")
          nil)
      (if-let* ((formatter (plist-get type-spec :formatter)))
          (progn
            (message "Debug - Using formatter: %S" formatter)
            (funcall formatter value nil))
        (progn
          (message "Debug - No formatter found, using raw value")
          value)))))

(defun org-supertag-field--validate-and-format-value (value field-def)
  "Private helper to validate and format a VALUE based on FIELD-DEF."
  (let* ((type (plist-get field-def :type))
         (type-spec (org-supertag-get-field-type type))
         (validator (plist-get type-spec :validator))
         (formatter (plist-get type-spec :formatter)))
    (when (or (not validator) (funcall validator value))
      (if formatter
          (funcall formatter value field-def)
        value))))


;;----------------------------------------------------------------------
;; Field Value Readers
;;----------------------------------------------------------------------

(defun org-supertag-read-string-field (prompt current-value &optional _field-def)
  "Read a string value from the user."
  (read-string prompt current-value))

(defun org-supertag-read-number-field (prompt current-value &optional _field-def)
  "Read a number value from the user."
  (read-string prompt current-value))

(defun org-supertag-read-date-field (prompt current-value &optional _field-def)
  "Read a date value from the user."
  (let ((date (org-read-date nil t nil prompt
                             (when (and current-value (not (string-empty-p current-value)))
                               current-value))))
    (format-time-string "<%Y-%m-%d %a>" date)))

(defun org-supertag-read-timestamp-field (prompt current-value &optional _field-def)
  "Read a timestamp value from the user."
  (let ((date-time (org-read-date t t nil prompt
                                  (when (and current-value (not (string-empty-p current-value)))
                                    current-value))))
    (format-time-string "[%Y-%m-%d %a %H:%M]" date-time)))

(defun org-supertag-read-options-field (prompt current-value field-def)
  "Read an options value from the user."
  (let ((options (plist-get field-def :options)))
    (completing-read prompt options nil t current-value)))


(defun org-supertag-read-url-field (prompt current-value &optional _field-def)
  "Read a URL from the user."
  (read-string prompt current-value))

(defun org-supertag-read-email-field (prompt current-value &optional _field-def)
  "Read an email from the user."
  (read-string prompt current-value))

;; support multiple tag values
(defun org-supertag-read-multiple-tags-field (prompt current-values &optional _field-def)
  "Read multiple tag references from the user，support comma separated and multi-segment completion."
  (let* ((tag-names (org-supertag-get-all-tags))
         (crm-separator ",")
         (input (let ((crm-separator crm-separator))
                  (completing-read-multiple
                   prompt tag-names nil nil
                   (when (listp current-values)
                     (mapconcat #'identity current-values ", ")))))
         )
    (mapcar #'string-trim input)))

;; get field value and deserialize (multiple values return list)
(defun org-supertag-field-get-value-for-display (node-id field-def)
  "Get a field's value, deserializing it if it is a tag field (always split for tag type)."
  (let* ((field-name (plist-get field-def :name))
         (tag-id (plist-get field-def :tag-id))
         (field-type (plist-get field-def :type))
         (raw-value (org-supertag-field-get-value node-id field-name tag-id)))
    ;;(message "DEBUG: get-value node-id=%S field-name=%S field-type=%S raw-value=%S (type=%S)"
             node-id field-name field-type raw-value (type-of raw-value))
    (if (and (eq field-type 'tag) raw-value (stringp raw-value))
        (let ((result (split-string raw-value "," t)))
          ;;(message "DEBUG: get-value split result: %S" result)
          result)
      raw-value))


;;----------------------------------------------------------------------
;; High-Level Interactive Field Editor
;;----------------------------------------------------------------------

(defun org-supertag-field-read-and-validate-value (field-def &optional current-value)
  "Interactively read and validate a field value based on its definition.
Handles single values, multiple values (for tags), and dynamic creation of new tags."
  (let* ((field-name (plist-get field-def :name))
         (field-type (plist-get field-def :type))
         (is-multiple (and (eq field-type 'tag) (plist-get field-def :multiple)))
         (type-info (or (org-supertag-get-field-type field-type) (org-supertag-get-field-type 'string)))
         (reader-fn (plist-get type-info :reader))
         (prompt (format "Edit %s (%s%s): "
                         field-name
                         (or (plist-get type-info :description) "Value")
                         (if is-multiple ", separate with commas" "")))
         new-value-raw
         validated-value
         valid-input-p)

    (unless reader-fn
      (error "No reader function found for field type %s" field-type))

    (while (not valid-input-p)
      (setq new-value-raw
            (funcall reader-fn prompt current-value field-def))
      (if (null new-value-raw)
          (progn (setq validated-value nil) (setq valid-input-p t))
        (if (eq field-type 'tag)
            ;; --- Multi-value Tag Logic ---
            (let* ((input-tags new-value-raw)
                   (all-tags (org-supertag-get-all-tags))
                   (final-tags '())
                   (all-valid t))
              (dolist (tag-str input-tags)
                (if (member tag-str all-tags)
                    (push tag-str final-tags)
                  (if (y-or-n-p (format "Tag '%s' does not exist. Create it? " tag-str))
                      (progn
                        (org-supertag-tag--create tag-str)
                        (push tag-str final-tags))
                    (progn
                      (message "Aborted. Please re-enter all required tags.")
                      (setq all-valid nil)))))
              (when all-valid
                (setq validated-value (nreverse final-tags))
                (setq valid-input-p t)))
          ;; --- 其它类型 ---
          (let ((validated-single-value (org-supertag-field--validate-and-format-value new-value-raw field-def)))
            (if validated-single-value
                (progn
                  (setq validated-value validated-single-value)
                  (setq valid-input-p t))
              (message "Invalid value. Please try again.")
              (sit-for 1))))))
    ;;(message "DEBUG: field-read-and-validate-value returning %S (type=%S)" validated-value (type-of validated-value))
    validated-value))

;;----------------------------------------------------------------------
;; Field Definition Management
;;----------------------------------------------------------------------

(defun org-supertag-tag-modify-field (tag-id field-name)
  "Modify field definition for TAG-ID's FIELD-NAME.
This will clear all existing values of this field across all nodes."
  (interactive
   (let* ((node-id (org-id-get))
          (tags (org-supertag-node-get-tags node-id))
          (tag-id (completing-read "Select tag: " tags nil t))
          ;; Use the new function to get all fields, including inherited ones
          (fields (org-supertag-get-all-fields-for-tag tag-id))
          (field-name (completing-read 
                      "Select field to modify: "
                      (mapcar (lambda (field)
                              (plist-get field :name))
                             fields)
                      nil t)))
     (list tag-id field-name)))
  
  (let* ((tag (org-supertag-tag-get tag-id))
         (fields (org-supertag-get-all-fields-for-tag tag-id)) ;; Get all fields for modification context
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
            (org-supertag-field-delete-value node-id field-name tag-id)))
        
        (message "Field '%s' modified. You may now set new values." 
                 new-name)))))

(defun org-supertag-field-get-tag-id (field)
  (getf field :tag-id))

(defun org-supertag-field-edit-cancel ()
  "Cancel editing fields."
  (when (get-buffer "*Org Supertag Fields*")
    (kill-buffer "*Org Supertag Fields*")))

(defun org-supertag-field-edit-save (fields)
  "Save the edited fields."
  (dolist (field fields)
    (let ((name (org-supertag-field-get-name field))
          (value (org-supertag-field-get-value field)))
      (org-supertag-field-set-value (org-id-get-create) name value (org-supertag-field-get-tag-id field))))
  (org-supertag-field-edit-cancel)
  (message "Fields saved."))

(defun org-supertag-field--get-existing-tags ()
  "Get all existing tags for completion.
Returns a list of tag names."
  (let (tags)
    (maphash (lambda (id props)
               (let ((type (plist-get props :type)))
                 (when (eq type :tag)
                   (let ((name (or (plist-get props :name) id)))
                     (push name tags)))))
             org-supertag-db--object)
    (sort tags #'string<)))





(provide 'org-supertag-field)

;;; org-supertag-field.el ends here
