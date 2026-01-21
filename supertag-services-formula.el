;;; supertag-services-formula.el --- Formula evaluation service -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides a safe environment for evaluating user-defined
;; formula expressions within Org-Supertag.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; --- Formula Context Functions ---

(defun supertag-formula-get-property (entity-data prop-key)
  "Helper function for formulas to get a property from ENTITY-DATA.
This function is exposed within the formula evaluation sandbox."
  (plist-get entity-data prop-key))

(defun supertag-formula-days-until (date-list)
  "Helper function for formulas to calculate days until a date.
DATE-LIST is a list like (year month day)."
  (when (and (listp date-list) (= (length date-list) 3))
    (let* ((year (nth 0 date-list))
           (month (nth 1 date-list))
           (day (nth 2 date-list))
           (target-time (encode-time 0 0 0 day month year)) ; sec, min, hour are 0
           (current-time (current-time))
           (diff-seconds (time-to-seconds (time-subtract target-time current-time))))
      (floor (/ diff-seconds 86400.0))))) ; 86400 seconds in a day

;; Add more safe helper functions here as needed, e.g., supertag-formula-format-date, etc.

;; --- Formula Evaluation Sandbox ---

(defun supertag-formula--normalize-key (key)
  "Normalize KEY into a keyword when possible."
  (cond
   ((keywordp key) key)
   ((symbolp key) (intern (concat ":" (symbol-name key))))
   ((stringp key)
    (let ((s (string-trim key)))
      (if (string-prefix-p ":" s)
          (intern s)
        (intern (concat ":" s)))))
   (t nil)))

(defun supertag-formula--lookup (entity-data key &optional field-getter)
  "Lookup KEY for ENTITY-DATA, optionally using FIELD-GETTER.

Resolution order:
1) ENTITY-DATA :properties plist
2) ENTITY-DATA top-level plist
3) FIELD-GETTER (tag-field/global-field) when provided"
  (let* ((k (supertag-formula--normalize-key key))
         (props (plist-get entity-data :properties))
         (v (when (and k (plistp props))
              (plist-get props k))))
    (cond
     ((not (null v)) v)
     ((and k (plist-member entity-data k)) (plist-get entity-data k))
     ((functionp field-getter) (funcall field-getter key))
     (t nil))))

(defun supertag-formula--literal (value)
  "Convert VALUE to a readable literal for embedding into a formula."
  (cond
   ((null value) "nil")
   ((or (numberp value) (eq value t)) (format "%S" value))
   ((stringp value) (prin1-to-string value))
   ((listp value) (prin1-to-string value))
   (t (prin1-to-string value))))

(defun supertag-formula--expand-placeholders (formula-string entity-data &optional field-getter)
  "Expand {{...}} placeholders in FORMULA-STRING using ENTITY-DATA/FIELD-GETTER.

Placeholder content accepts:
- `:prop` (keyword-like)
- `prop`  (will be treated as `:prop` for lookup)
Values are embedded as Emacs Lisp literals."
  (let ((expanded formula-string))
    (while (string-match "{{\\([^}]+\\)}}" expanded)
      (let* ((raw (match-string 1 expanded))
             (key (string-trim raw))
             (val (supertag-formula--lookup entity-data key field-getter))
             (lit (supertag-formula--literal val)))
        (setq expanded (replace-match lit t t expanded))))
    expanded))

(defun supertag-formula-evaluate (formula-string entity-data &optional field-getter)
  "Evaluate FORMULA-STRING for ENTITY-DATA.

Supports {{...}} placeholders which are expanded before evaluation.
Also exposes a limited set of helper functions:
- (get-property :key)  ; reads from ENTITY-DATA (and FIELD-GETTER when provided)
- (days-until '(YYYY MM DD))"
  (unless (and (stringp formula-string) (not (string-empty-p formula-string)))
    (error "FORMULA-STRING must be a non-empty string"))
  (let* ((expanded (supertag-formula--expand-placeholders formula-string entity-data field-getter))
         (sexp (condition-case _err
                   (read expanded)
                 (error nil)))
         (get-property-fn (lambda (prop-key)
                            (supertag-formula--lookup entity-data prop-key field-getter))))
    (unless sexp
      (error "Invalid formula: %S" formula-string))
    (cl-letf (((symbol-function 'get-property) get-property-fn)
              ((symbol-function 'days-until) #'supertag-formula-days-until))
      (eval sexp))))

(provide 'supertag-services-formula)
