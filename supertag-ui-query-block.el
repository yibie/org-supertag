;;; supertag-ui-query-block.el --- S-expression query blocks for Org Babel -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides S-expression query block functionality for Org Babel.
;; Core responsibilities:
;; - S-expression parsing and AST generation
;; - Org Babel integration (org-babel-execute:org-supertag-query-block)
;; - Table formatting for query results
;; - Block insertion commands
;;
;; This module follows the "good taste" principle: single responsibility,
;; no special cases, clean data flow.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'supertag-services-query) ; For the core query engine

;;; --- Table Formatting ---

(defun supertag-query-block--format-table (headers data)
  "Format DATA into an Org table string with HEADERS and basic alignment."
  (require 'org-table)
  (with-temp-buffer
    (org-mode)
    (insert "| " (mapconcat #'identity headers " | ") " |\n")
    (insert "|-" (mapconcat (lambda (h) (make-string (length h) ?-)) headers "-|-") "-|\n")
    (dolist (row data)
      (insert "| " (mapconcat #'identity row " | ") " |\n"))
    (org-table-align)
    (buffer-string)))

(defun supertag-query-block--get-node-field-value (node-id field-name)
  "Get the value of FIELD-NAME for NODE-ID from database field relationships."
  (let ((node-data (supertag-get (list :nodes node-id))))
    (when node-data
      (let ((properties (plist-get node-data :properties)))
        (when properties
          (plist-get properties (intern (concat ":" field-name))))))))

;;; --- S-expression Query Block Functions ---

(defun supertag-insert-query-block ()
  "Insert an S-expression query block for Org Babel."
  (interactive)
  (let* ((query (read-string "Query S-expression: "))
         (block-template "#+BEGIN_SRC org-supertag-query-block :results raw\n%s\n#+END_SRC"))
    (unless (string-empty-p query)
      (insert (format block-template query)))))

(defun org-babel-execute:org-supertag-query-block (body params)
  "Execute an org-supertag-query-block and return results as an Org table.
BODY is the S-expression query string.
PARAMS are the babel parameters (not used currently).
This is the primary function for S-expression query blocks."
  (let* ((query-str (string-trim body))
         (query-sexp (car (read-from-string query-str)))
         (ast (supertag-query--parse-sexp query-sexp))
         (node-ids (supertag-query--execute-ast ast))
         (field-keys (supertag-query--get-fields-from-ast ast))
         (headers (append '("Node" "Tags") field-keys))
         (nodes (delq nil (mapcar (lambda (id)
                                    (supertag-get (list :nodes id)))
                                  node-ids)))
         (table-data (mapcar (lambda (node)
                               (let* ((id (plist-get node :id))
                                      (title (or (plist-get node :title) "Untitled"))
                                      (tags (plist-get node :tags)))
                                 (append (list (format "[[id:%s][%s]]" id title)
                                               (if (and tags (listp tags)) 
                                                   (mapconcat #'identity tags ", ") 
                                                 ""))
                                         (mapcar (lambda (key)
                                                   (let ((val (supertag-query-block--get-node-field-value id key)))
                                                     (if val (format "%s" val) "")))
                                                 field-keys))))
                             nodes)))
    (if (not nodes)
        "No results found."
      (supertag-query-block--format-table headers table-data))))

;;; --- Initialization and Configuration ---

;; Org Babel registration - new language name
(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages '(org-supertag-query-block . t))
  (add-to-list 'org-babel-default-header-args '(org-supertag-query-block . ((:results . "raw")))))

;; Backward compatibility alias for the old function name
(defalias 'org-babel-execute:org-supertag-query 'org-babel-execute:org-supertag-query-block
  "Backward compatibility alias for the renamed S-expression query block function.")

(provide 'supertag-ui-query-block)

;;; supertag-ui-query-block.el ends here