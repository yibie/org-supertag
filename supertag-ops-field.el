;;; org-supertag/ops/field.el --- Field operations for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides standardized operations for managing field values
;; associated with nodes and tags in the Org-Supertag data-centric architecture.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store)    ; For supertag-get
(require 'supertag-core-schema)   ; For supertag--convert-type
(require 'supertag-core-transform) ; For supertag-transform
(require 'supertag-ops-tag)  ; For supertag-tag-get-field

;;; --- Field Operations ---

;; 4.1 Field Value Operations

(defun supertag-field-set (node-id tag-id field-name value)
  "Set the value of a tag field for a node.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field.
VALUE is the field value.
Returns the updated field value."
  ;; (message "DEBUG-1: supertag-field-set called. Node: %s, Field: %s, Value: %s" node-id field-name value)
  (supertag-transform
   (list :fields node-id tag-id field-name)
   (lambda (_) value)))

(defun supertag-field-get (node-id tag-id field-name &optional default)
  "Get the value of a tag field for a node.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field.
DEFAULT is the default value to return if the field does not exist.
Returns the field value, or DEFAULT if it does not exist."
  (supertag-get (list :fields node-id tag-id field-name) default))

(defun supertag-field-remove (node-id tag-id field-name)
  "Remove the value of a tag field for a node.
NODE-ID is the unique identifier of the node.
TAG-ID is the unique identifier of the tag.
FIELD-NAME is the name of the field.
Returns the removed field value."
  (supertag-transform
   (list :fields node-id tag-id field-name)
   (lambda (_) nil))) ; Setting value to nil effectively removes it

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