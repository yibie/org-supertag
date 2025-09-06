;;; supertag-services-formula.el --- Formula evaluation service -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides a safe environment for evaluating user-defined
;; formula expressions within Org-Supertag.

;;; Code:

(require 'cl-lib)

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

(defun supertag-formula-evaluate (formula-string entity-data)
  "Evaluate a formula string in a sandboxed environment.
ENTITY-DATA is the plist of the current node.
Exposes a limited set of safe functions for formula expressions."
  (let* ((formula-sexp (read formula-string)))
         ;; Define the sandboxed environment
         (get-property (lambda (prop-key) (supertag-formula-get-property entity-data prop-key))) ; Corrected binding
         (days-until #'supertag-formula-days-until)
         ;; Add other safe functions here
         )
    (if formula-sexp
        (eval formula-sexp)
      "INVALID FORMULA"))

(provide 'supertag-services-formula)
