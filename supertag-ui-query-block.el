;;; supertag-ui-query-block.el --- S-expression query blocks for Org Babel -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides S-expression query block functionality for Org, in
;; two flavors that share a single "query string + params -> table string"
;; core:
;;
;; 1. Org Babel blocks (`org-babel-execute:org-supertag-query-block'):
;;
;;      #+BEGIN_SRC org-supertag-query-block :results raw :sort modified :order desc :limit 20 :columns "status priority"
;;      (and (tag "project") (after "-30d"))
;;      #+END_SRC
;;
;;    Re-run with the usual babel keys (\\[org-ctrl-c-ctrl-c] on the block).
;;
;; 2. Dataview-style dynamic blocks (`org-dblock-write:supertag-query'),
;;    which auto-refresh like any other Org dynamic block:
;;
;;      #+BEGIN: supertag-query :query "(and (tag \"project\") (after \"-30d\"))" :sort modified :order desc :limit 20 :columns ("status" "priority")
;;      #+END:
;;
;;    Refresh with \\[org-ctrl-c-ctrl-c] on the block, `org-dblock-update',
;;    or `org-update-all-dblocks'. Insert one with
;;    `supertag-insert-query-dblock'.
;;
;; Both flavors accept the same optional result-control params, all of
;; which are no-ops when omitted (existing babel blocks keep behaving
;; exactly as before):
;;
;;   :sort    title | created | modified | a field name.
;;            Field (and "created"/"modified") sorts compare numerically
;;            when both values parse as numbers, otherwise string-compare
;;            ("created"/"modified" compare as Emacs time values instead).
;;            Nodes missing the sort key sort last, regardless of :order.
;;   :order   asc (default) | desc.
;;   :limit   a positive integer, applied after sorting.
;;   :columns an explicit list of field names for extra table columns,
;;            overriding the fields auto-derived from the query's
;;            (field ...) clauses. The "Node" and "Tags" columns are
;;            always present regardless.
;;
;; Malformed queries and invalid params never signal into the org-babel or
;; org-dblock machinery: they render as a single-line "Error: ..." string
;; in place of the table.
;;
;; This module follows the "good taste" principle: single responsibility,
;; no special cases, clean data flow.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'supertag-services-query) ; For the core query engine
(require 'supertag-ops-node)

;;; --- Table Formatting ---

(defun supertag-query-block--format-table (headers data)
  "Format DATA into an Org table string with HEADERS and basic alignment."
  (require 'org-table)
  (with-temp-buffer
    (org-mode)
    ;; Ensure tab-width is 8 as required by org-current-text-column
    (setq-local tab-width 8)
    (insert "| " (mapconcat #'identity headers " | ") " |\n")
    (insert "|-" (mapconcat (lambda (h) (make-string (length h) ?-)) headers "-|-") "-|\n")
    (dolist (row data)
      (insert "| " (mapconcat #'identity row " | ") " |\n"))
    (org-table-align)
    (buffer-string)))

;;; --- Shared Core: query string + params -> table string ---

(defun supertag-query-block--parse-columns (columns)
  "Normalize the :columns param COLUMNS into a list of field-name strings.
Accepts a list of strings/symbols, a single symbol, a space/comma
separated string, or nil (meaning \"no override\")."
  (cond
   ((null columns) nil)
   ((stringp columns)
    (let ((trimmed (string-trim columns)))
      (unless (string-empty-p trimmed)
        (split-string trimmed "[, \f\t\n\r\v]+" t))))
   ((symbolp columns) (list (symbol-name columns)))
   ((listp columns)
    (mapcar (lambda (c)
              (cond ((stringp c) c)
                    ((symbolp c) (symbol-name c))
                    (t (format "%s" c))))
            columns))
   (t (error "Invalid :columns value: %S" columns))))

(defun supertag-query-block--normalize-sort-key (sort)
  "Normalize the :sort param SORT into a field-key string, or nil."
  (cond
   ((null sort) nil)
   ((stringp sort) (let ((trimmed (string-trim sort)))
                      (unless (string-empty-p trimmed) trimmed)))
   ((symbolp sort) (symbol-name sort))
   (t (error "Invalid :sort value: %S" sort))))

(defun supertag-query-block--normalize-order (order)
  "Normalize the :order param ORDER into the symbol `asc' or `desc'."
  (cond
   ((null order) 'asc)
   ((memq order '(asc desc)) order)
   ((and (stringp order) (member (downcase (string-trim order)) '("asc" "desc")))
    (intern (downcase (string-trim order))))
   (t (error "Invalid :order value: %S (expected asc or desc)" order))))

(defun supertag-query-block--normalize-limit (limit)
  "Normalize the :limit param LIMIT into a positive integer, or nil."
  (cond
   ((null limit) nil)
   ((and (integerp limit) (> limit 0)) limit)
   ((and (stringp limit)
         (string-match-p "\\`[ \t]*[0-9]+[ \t]*\\'" limit)
         (> (string-to-number limit) 0))
    (string-to-number limit))
   (t (error "Invalid :limit value: %S (expected a positive integer)" limit))))

(defun supertag-query-block--sort-value (node-id node key)
  "Return the raw sort value for NODE-ID/NODE for normalized sort KEY."
  (cond
   ((string= key "title") (plist-get node :title))
   ((string= key "created") (plist-get node :created-at))
   ((string= key "modified") (plist-get node :modified-at))
   (t (supertag-query--get-node-field-value node-id key))))

(defun supertag-query-block--numeric (value)
  "Return VALUE as a number if it is one, or a numeric-looking string. Else nil."
  (cond
   ((numberp value) value)
   ((and (stringp value)
         (string-match-p "\\`[ \t]*-?[0-9]+\\(\\.[0-9]+\\)?[ \t]*\\'" value))
    (string-to-number value))
   (t nil)))

(defun supertag-query-block--value< (a b)
  "Return non-nil if sort value A sorts before sort value B.
Numeric comparison when both parse as numbers; Emacs-time comparison when
both look like Emacs time values (as used by :created-at/:modified-at);
string comparison otherwise."
  (let ((na (supertag-query-block--numeric a))
        (nb (supertag-query-block--numeric b)))
    (cond
     ((and na nb) (< na nb))
     ((and (consp a) (consp b) (integerp (car a)) (integerp (car b)))
      (time-less-p a b))
     (t (string< (format "%s" a) (format "%s" b))))))

(defun supertag-query-block--apply-sort (nodes sort-key order)
  "Sort NODES (list of node plists) by SORT-KEY (string or nil) per ORDER.
Nodes missing the sort key are always placed last, regardless of ORDER."
  (if (null sort-key)
      nodes
    (let (with-key without-key)
      (dolist (n nodes)
        (let ((v (supertag-query-block--sort-value (plist-get n :id) n sort-key)))
          (if v (push (cons n v) with-key) (push n without-key))))
      (setq with-key (nreverse with-key)
            without-key (nreverse without-key))
      (setq with-key (sort with-key (lambda (a b) (supertag-query-block--value< (cdr a) (cdr b)))))
      (when (eq order 'desc) (setq with-key (nreverse with-key)))
      (append (mapcar #'car with-key) without-key))))

(defun supertag-query-block--row (node columns)
  "Build one Org table row (list of cell strings) for NODE and COLUMNS."
  (let* ((id (plist-get node :id))
         (title (or (plist-get node :title) "Untitled"))
         (tags (plist-get node :tags)))
    (append (list (supertag-node-format-link id title)
                  (if (and tags (listp tags))
                      (mapconcat #'identity tags ", ")
                    ""))
            (mapcar (lambda (key)
                      (let ((val (supertag-query--get-node-field-value id key)))
                        (if val (format "%s" val) "")))
                    columns))))

(defun supertag-query-block--headers-and-rows (query-str opts)
  "Execute QUERY-STR (an S-expression query string) with OPTS.
OPTS is a plist with optional :sort, :order, :limit, :columns keys, using
the same semantics documented at the top of this file.
Returns (HEADERS . ROWS). Signals an error on malformed input; callers
that must never signal should go through `supertag-query-block--render'."
  (let* ((query-sexp (car (read-from-string (string-trim query-str))))
         (ast (supertag-query--parse-sexp query-sexp))
         (node-ids (supertag-query--execute-ast ast))
         (auto-fields (supertag-query--get-fields-from-ast ast))
         (columns (or (supertag-query-block--parse-columns (plist-get opts :columns))
                      auto-fields))
         (sort-key (supertag-query-block--normalize-sort-key (plist-get opts :sort)))
         (order (supertag-query-block--normalize-order (plist-get opts :order)))
         (limit (supertag-query-block--normalize-limit (plist-get opts :limit)))
         (nodes (delq nil (mapcar #'supertag-node-get node-ids)))
         (nodes (supertag-query-block--apply-sort nodes sort-key order)))
    (when limit
      (setq nodes (cl-subseq nodes 0 (min limit (length nodes)))))
    (cons (append '("Node" "Tags") columns)
          (mapcar (lambda (node) (supertag-query-block--row node columns)) nodes))))

(defun supertag-query-block--render (query-str opts)
  "Render QUERY-STR/OPTS to a table string, or a one-line \"Error: ...\" string.
Never signals: this is the entry point both the babel executor and the
dynamic-block writer call, so a malformed query s-expression or an
invalid param (unknown sort key, bad :order/:limit/:columns value, etc.)
must never propagate into org-babel or org-dblock machinery."
  (condition-case err
      (let* ((result (supertag-query-block--headers-and-rows query-str opts))
             (headers (car result))
             (rows (cdr result)))
        (if (null rows)
            "No results found."
          (supertag-query-block--format-table headers rows)))
    (error (format "Error: %s" (error-message-string err)))))

;;; --- S-expression Query Block Functions (Org Babel) ---

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
PARAMS are the babel header args. All are optional and, when omitted,
produce exactly the previous behavior:

  :sort NAME    title | created | modified | a field name. To pass a
                literal string instead of a bare symbol, quote it, e.g.
                :sort \"priority\".
  :order asc|desc
  :limit N      a positive integer.
  :columns \"f1 f2\"  or  :columns \\='(\"f1\" \"f2\")
                A space/comma separated string is the simplest form; a
                quoted Lisp list also works (header-arg values starting
                with \"(\" are `eval'd by Org, so an unquoted list would
                be evaluated as a function call).

See the file commentary for full semantics (missing sort keys sort
last, numeric vs. string vs. Emacs-time comparison, etc.). Malformed
queries or invalid params render as a one-line error string instead of
signaling."
  (supertag-query-block--render
   body
   (list :sort (cdr (assq :sort params))
         :order (cdr (assq :order params))
         :limit (cdr (assq :limit params))
         :columns (cdr (assq :columns params)))))

;;; --- Dataview-style Dynamic Block ---

(defun org-dblock-write:supertag-query (params)
  "Render a Supertag S-expression query as a refreshable Org dynamic block.
PARAMS is the plist Org parses from the #+BEGIN: line, e.g.:

  #+BEGIN: supertag-query :query \"(and (tag \\\"project\\\") (after \\\"-30d\\\"))\" \\
:sort modified :order desc :limit 20 :columns (\"status\" \"priority\")
  #+END:

Recognized keys (all but :query are optional):
  :query   (required) an S-expression query string, same syntax as the
           `org-supertag-query-block' babel language.
  :sort    title | created | modified | a field name (bare symbol or
           string).
  :order   asc (default) | desc.
  :limit   a positive integer, applied after sorting.
  :columns an explicit list of field names, e.g. (\"status\" \"priority\"),
           overriding the fields auto-derived from the query. \"Node\"
           and \"Tags\" columns are always present.

Refresh with \\[org-ctrl-c-ctrl-c] on the block, `org-dblock-update', or
`org-update-all-dblocks'. Never signals: a malformed :query or an
invalid param renders as a one-line error string instead of a table."
  (let* ((query-str (plist-get params :query))
         (text
          (if (not (and query-str (stringp query-str)
                        (not (string-empty-p (string-trim query-str)))))
              "Error: supertag-query dynamic block requires a :query string."
            (supertag-query-block--render
             query-str
             (list :sort (plist-get params :sort)
                   :order (plist-get params :order)
                   :limit (plist-get params :limit)
                   :columns (plist-get params :columns))))))
    ;; `org-prepare-dblock' already positioned point on a fresh blank line
    ;; immediately followed by the #+END: line's own newline, so the
    ;; inserted text must NOT end in a trailing newline of its own.
    (insert (string-remove-suffix "\n" text))))

;;;###autoload
(defun supertag-insert-query-dblock ()
  "Insert a `supertag-query' dynamic block and run its first update.
Prompts for an S-expression query, e.g.:

  (and (tag \"project\") (after \"-30d\"))

The inserted block can be hand-edited afterward to add :sort, :order,
:limit, or :columns keys (see `org-dblock-write:supertag-query' for
their syntax). Refresh anytime with \\[org-ctrl-c-ctrl-c] on the block,
`org-dblock-update', or `org-update-all-dblocks'."
  (interactive)
  (let ((query (read-string
                "Query s-expression (e.g. (and (tag \"project\") (after \"-30d\"))): ")))
    (unless (string-empty-p (string-trim query))
      (org-create-dblock (list :name "supertag-query" :query query))
      (org-update-dblock))))

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
