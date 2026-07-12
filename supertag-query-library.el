;;; supertag-query-library.el --- Saved queries, a guided builder, and a syntax reference -*- lexical-binding: t; -*-

;;; Commentary:
;; This file makes the Org-Supertag S-expression query language (defined in
;; `supertag-services-query.el', see `supertag-query--parse-sexp' and
;; `supertag-query--execute-ast') learnable and reusable, without changing
;; the engine or the query-block/dynamic-block UI layer:
;;
;; - Saved queries: `supertag-query-save', `supertag-query-run-saved',
;;   `supertag-query-insert-saved', backed by the `supertag-query-saved'
;;   defcustom.
;; - A guided builder: `supertag-query-build' assembles a query S-expression
;;   interactively, with tag/field completion from live data, then lets you
;;   copy it, insert it as a block, run it, or save it as a named query.
;; - A quick reference: `supertag-query-describe-syntax'.
;;
;; This file never reimplements query parsing or execution -- it always
;; calls into `supertag-query--parse-sexp', `supertag-query--execute-ast',
;; `supertag-query--get-fields-from-ast', and `supertag-query--get-node-field-value'
;; from `supertag-services-query.el'.
;;
;; See doc/QUERY.md for the full user-facing grammar reference.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'supertag-services-query)
(require 'supertag-ops-node)

(defgroup supertag-query-library nil
  "Saved queries, guided builder, and syntax reference for Org-Supertag queries."
  :group 'org-supertag)

;;; --- Saved queries ---------------------------------------------------

(defcustom supertag-query-saved nil
  "Alist of saved Org-Supertag queries.
Each element is (NAME . QUERY-STRING).  NAME is a human-readable label
you choose; QUERY-STRING is the textual S-expression accepted by
`supertag-query-sexp', e.g. \"(tag \\\"project\\\")\" or
\"(and (tag \\\"task\\\") (not (field \\\"status\\\" \\\"done\\\")))\".
See doc/QUERY.md for the full grammar."
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Query S-expression"))
  :group 'supertag-query-library)

(defvar supertag-query-library--last-query nil
  "The most recent query string this library ran, inserted, or built.
Session-only; used as a prompt default so you don't have to retype a
query you just tried.")

;;; --- Small helpers -----------------------------------------------------

(defun supertag-query-library--customize-save-possible-p ()
  "Return non-nil if `customize-save-variable' can write to disk.
Mirrors the precondition `supertag-setup--customize-save-possible-p'
applies in supertag-setup.el: customizations cannot be saved when Emacs
was started without loading an init file (e.g. `emacs -q'), even if
`custom-file' happens to be set, so we also require `user-init-file' to
be non-nil."
  (and user-init-file
       (let ((file (or custom-file user-init-file)))
         (and (stringp file)
              (not (string-empty-p file))
              (if (file-exists-p file)
                  (file-writable-p file)
                (let ((dir (file-name-directory file)))
                  (and dir (file-writable-p dir))))))))

(defun supertag-query-library--default-query-text ()
  "Return a sensible default query string for prompts.
Prefers the active region, then falls back to the last query this
library ran, inserted, or built."
  (cond
   ((use-region-p)
    (string-trim (buffer-substring-no-properties (region-beginning) (region-end))))
   (supertag-query-library--last-query)
   (t "")))

(defun supertag-query-library--read-query-sexp (query-string)
  "Parse QUERY-STRING into a query S-expression, signaling a clear error.
Uses the real reader and the real parser (`supertag-query--parse-sexp')
so an invalid query is rejected the same way the engine would reject it."
  (let (sexp)
    (condition-case err
        (setq sexp (car (read-from-string query-string)))
      (error (user-error "Could not read query `%s': %s"
                          query-string (error-message-string err))))
    (condition-case err
        (progn (supertag-query--parse-sexp sexp) sexp)
      (error (user-error "Invalid query `%s': %s"
                          query-string (error-message-string err))))))

(defun supertag-query-library--saved-query-string (name)
  "Return the saved query string for NAME, or signal a user-error."
  (or (cdr (assoc name supertag-query-saved))
      (user-error "No saved query named `%s'" name)))

(defun supertag-query-library--completing-read-saved (prompt)
  "Read a saved query name with PROMPT, annotated with its query text."
  (unless supertag-query-saved
    (user-error "No saved queries yet -- use `supertag-query-save' first"))
  (let ((collection
         (lambda (str pred action)
           (if (eq action 'metadata)
               '(metadata (annotation-function . supertag-query-library--annotate-saved))
             (complete-with-action action (mapcar #'car supertag-query-saved) str pred)))))
    (completing-read prompt collection nil t)))

(defun supertag-query-library--annotate-saved (name)
  "Return an annotation string showing the query text saved under NAME."
  (let ((query (cdr (assoc name supertag-query-saved))))
    (if query (format "  --  %s" query) "")))

(defun supertag-query-library--dynamic-block-command ()
  "Return the query-block layer's dynamic-block insert command, if defined.
The dynamic block form (`#+BEGIN: supertag-query ...', an Org \"dblock\")
is being added to supertag-ui-query-block.el independently of this file,
so we only detect its insert command by name pattern (matching either
\"dblock\" or \"dynamic-block\", since Org's own convention is \"dblock\")
rather than hard-depending on a specific name. Returns nil if no such
command is loaded yet."
  (let (found)
    (mapatoms
     (lambda (sym)
       (when (and (not found)
                  (commandp sym)
                  (string-match-p "\\`supertag" (symbol-name sym))
                  (string-match-p "d\\(ynamic-\\)?block" (symbol-name sym))
                  (string-match-p "insert" (symbol-name sym)))
         (setq found sym))))
    found))

;;; --- Rendering query results --------------------------------------------

(defun supertag-query-library--render-results (query-sexp)
  "Run QUERY-SEXP and return a read-only buffer showing the results.
Node titles are shown as Org links, alongside a Tags column and one
column per field mentioned in QUERY-SEXP.  This reuses the query
engine's own AST parser/executor/field-extractor; it does not
reimplement parsing or execution."
  (let* ((ast (supertag-query--parse-sexp query-sexp))
         (node-ids (supertag-query--execute-ast ast))
         (field-keys (supertag-query--get-fields-from-ast ast))
         (nodes (delq nil (mapcar #'supertag-node-get node-ids)))
         (buf (get-buffer-create "*Supertag Saved Query*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (setq-local tab-width 8)
        (insert (format "#+TITLE: Supertag Query Results\n# Query: %S\n\n" query-sexp))
        (if (not nodes)
            (insert "No results found.\n")
          (let ((headers (append '("Node" "Tags") field-keys)))
            (insert "| " (mapconcat #'identity headers " | ") " |\n")
            (insert "|-" (mapconcat (lambda (h) (make-string (length h) ?-)) headers "-|-") "-|\n")
            (dolist (node nodes)
              (let* ((id (plist-get node :id))
                     (title (or (plist-get node :title) "Untitled"))
                     (tags (plist-get node :tags))
                     (row (append
                           (list (supertag-node-format-link id title)
                                 (if (and tags (listp tags)) (mapconcat #'identity tags ", ") ""))
                           (mapcar (lambda (key)
                                     (let ((val (supertag-query--get-node-field-value id key)))
                                       (if val (format "%s" val) "")))
                                   field-keys))))
                (insert "| " (mapconcat #'identity row " | ") " |\n")))
            (require 'org-table)
            (org-table-align)))
        (goto-char (point-min))
        (view-mode 1)))
    buf))

;;; --- Saved-query commands ------------------------------------------------

;;;###autoload
(defun supertag-query-save (query name)
  "Save QUERY (an S-expression string) under NAME in `supertag-query-saved'.
Interactively, QUERY defaults to the active region, or to the last
query this library ran/inserted/built.  Persists permanently via
`customize-save-variable' when a writable custom-file/init file is
available; otherwise the query is kept for this session only and the
user is told so, mirroring the behaviour of
`supertag-setup--persist' in supertag-setup.el."
  (interactive
   (let* ((default (supertag-query-library--default-query-text))
          (query (read-string "Query S-expression: " default))
          (name (read-string "Save as name: ")))
     (list query name)))
  (when (string-empty-p (string-trim name))
    (user-error "Query name cannot be empty"))
  ;; Validate before saving so a typo is never persisted silently.
  (supertag-query-library--read-query-sexp query)
  (let* ((existing (assoc name supertag-query-saved))
         (updated (if existing
                      (progn (setcdr existing query) supertag-query-saved)
                    (append supertag-query-saved (list (cons name query))))))
    (setq supertag-query-saved updated)
    (setq supertag-query-library--last-query query)
    (if (supertag-query-library--customize-save-possible-p)
        (progn
          (customize-save-variable 'supertag-query-saved updated)
          (message "Saved query `%s' permanently to %s"
                   name (abbreviate-file-name (or custom-file user-init-file))))
      (message "Saved query `%s' for this session only (no writable custom-file/init file found)."
               name))
    updated))

;;;###autoload
(defun supertag-query-run-saved (name)
  "Run the saved query NAME and show its results in a temp buffer."
  (interactive (list (supertag-query-library--completing-read-saved "Run saved query: ")))
  (let* ((query-string (supertag-query-library--saved-query-string name))
         (query-sexp (supertag-query-library--read-query-sexp query-string)))
    (setq supertag-query-library--last-query query-string)
    (pop-to-buffer (supertag-query-library--render-results query-sexp))))

;;;###autoload
(defun supertag-query-insert-saved (name)
  "Insert the saved query NAME as a query block at point.
Offers the Org Babel form (`#+BEGIN_SRC org-supertag-query-block ...')
by default.  If the query-block layer's dynamic-block insert command
(the `#+BEGIN: supertag-query ...' form) is loaded, also offer that
form -- see supertag-ui-query-block.el for its docstring and exact
parameters (:sort/:order/:limit/:columns and friends)."
  (interactive (list (supertag-query-library--completing-read-saved "Insert saved query: ")))
  (let* ((query-string (supertag-query-library--saved-query-string name))
         (dynamic-cmd (supertag-query-library--dynamic-block-command))
         (choice (if dynamic-cmd
                     (completing-read "Insert as: " '("Babel block" "Dynamic block")
                                       nil t nil nil "Babel block")
                   "Babel block")))
    (setq supertag-query-library--last-query query-string)
    (if (and dynamic-cmd (equal choice "Dynamic block"))
        (progn
          (kill-new query-string)
          (message "Query copied to kill-ring (yank it if %s prompts for one); invoking it now."
                   dynamic-cmd)
          (call-interactively dynamic-cmd))
      (insert (format "#+BEGIN_SRC org-supertag-query-block :results raw\n%s\n#+END_SRC\n"
                       query-string)))))

;;; --- Guided builder -------------------------------------------------------

(defconst supertag-query-library--operators
  '(("tag"     . "(tag NAME) -- nodes carrying tag NAME")
    ("field"   . "(field KEY VALUE) -- nodes whose field KEY equals VALUE")
    ("term"    . "(term WORD) -- full-text search over title/content")
    ("after"   . "(after DATE) -- nodes dated after DATE")
    ("before"  . "(before DATE) -- nodes dated before DATE")
    ("between" . "(between START END) -- nodes dated between START and END"))
  "Leaf query operators offered by `supertag-query-build', with descriptions.")

(defun supertag-query-library--completing-read-operator (prompt)
  "Read a leaf operator name with PROMPT, annotated with its meaning."
  (let ((collection
         (lambda (str pred action)
           (if (eq action 'metadata)
               '(metadata (annotation-function . supertag-query-library--annotate-operator))
             (complete-with-action action (mapcar #'car supertag-query-library--operators) str pred)))))
    (completing-read prompt collection nil t)))

(defun supertag-query-library--annotate-operator (op)
  "Return an annotation string describing operator OP."
  (let ((desc (cdr (assoc op supertag-query-library--operators))))
    (if desc (format "  --  %s" desc) "")))

(defun supertag-query-library--live-tag-names ()
  "Return known tag names from live data, or nil if unavailable.
Mirrors the (mapcar #\\='car (supertag-query :tags)) pattern used
throughout supertag-ui-commands.el."
  (ignore-errors (mapcar #'car (supertag-query :tags))))

(defun supertag-query-library--live-field-names ()
  "Return known global field names from live data, or nil if unavailable."
  (ignore-errors
    (delete-dups
     (delq nil
           (mapcar (lambda (pair)
                     (or (plist-get (cdr pair) :name)
                         (and (car pair) (format "%s" (car pair)))))
                   (supertag-query :field-definitions))))))

(defun supertag-query-library--read-tag-name ()
  "Read a tag name, completing against live tags when available."
  (let ((tags (supertag-query-library--live-tag-names)))
    (if tags
        (completing-read "Tag: " tags nil nil)
      (read-string "Tag: "))))

(defun supertag-query-library--read-field-name ()
  "Read a field name, completing against live field definitions when available."
  (let ((fields (supertag-query-library--live-field-names)))
    (if fields
        (completing-read "Field: " fields nil nil)
      (read-string "Field: "))))

(defun supertag-query-library--read-date (prompt)
  "Read a date string for PROMPT, validated with the engine's own parser.
Accepts \"now\", an absolute \"YYYY-MM-DD\" date, or a relative offset
like \"-7d\", \"+2w\", \"-1m\", \"1y\" (an offset without a sign means
+, i.e. the future)."
  (let ((s (read-string (format "%s (now / YYYY-MM-DD / -7d / +2w): " prompt))))
    (unless (supertag-query--resolve-date-string s)
      (user-error "Unrecognized date `%s' -- use now, YYYY-MM-DD, or [+-]Nd/w/m/y" s))
    s))

(defun supertag-query-library--make-condition (op &rest args)
  "Construct and validate a leaf condition sexp for operator OP and ARGS.
OP is a string or symbol naming one of the leaf operators in
`supertag-query-library--operators'.  The resulting sexp is passed
through `supertag-query--parse-sexp' so an arity mistake (e.g. the
wrong number of ARGS) surfaces the same error the query engine itself
would raise, instead of silently building a bad query.  This is the
pure assembly step used by both the interactive builder and its
tests."
  (let ((sexp (cons (if (stringp op) (intern op) op) args)))
    (supertag-query--parse-sexp sexp)
    sexp))

(defun supertag-query-library--combine-conditions (combinator expr next)
  "Return the sexp combining EXPR and NEXT with COMBINATOR.
COMBINATOR is \"and\"/\"or\" (a string) or the symbol `and'/`or'."
  (let ((sexp (list (if (stringp combinator) (intern combinator) combinator) expr next)))
    (supertag-query--parse-sexp sexp)
    sexp))

(defun supertag-query-library--build-condition ()
  "Interactively build one leaf query condition and return it as a sexp."
  (let ((op (supertag-query-library--completing-read-operator "Condition operator: ")))
    (pcase op
      ("tag" (supertag-query-library--make-condition op (supertag-query-library--read-tag-name)))
      ("field" (let ((key (supertag-query-library--read-field-name)))
                 (supertag-query-library--make-condition
                  op key (read-string (format "Value for field `%s': " key)))))
      ("term" (supertag-query-library--make-condition op (read-string "Search term: ")))
      ("after" (supertag-query-library--make-condition
                op (supertag-query-library--read-date "After date")))
      ("before" (supertag-query-library--make-condition
                 op (supertag-query-library--read-date "Before date")))
      ("between" (supertag-query-library--make-condition
                  op
                  (supertag-query-library--read-date "Start date")
                  (supertag-query-library--read-date "End date")))
      (_ (user-error "Unknown operator `%s'" op)))))

(defun supertag-query-library--present-built-query (expr)
  "Preview built query EXPR and offer copy/insert/run/save follow-up actions."
  (let* ((text (prin1-to-string expr))
         (action (completing-read
                  (format "Query: %s -- action: " text)
                  '("Copy to kill-ring" "Insert block" "Run now" "Save as named query")
                  nil t)))
    (setq supertag-query-library--last-query text)
    (pcase action
      ("Copy to kill-ring"
       (kill-new text)
       (message "Copied to kill-ring: %s" text))
      ("Insert block"
       (insert (format "#+BEGIN_SRC org-supertag-query-block :results raw\n%s\n#+END_SRC\n" text)))
      ("Run now"
       (pop-to-buffer (supertag-query-library--render-results expr)))
      ("Save as named query"
       (supertag-query-save text (read-string "Save as name: "))))
    expr))

;;;###autoload
(defun supertag-query-build ()
  "Interactively assemble an Org-Supertag query S-expression.
Prompts for a leaf condition (tag/field/term/after/before/between),
then repeatedly offers to combine it with another condition using AND
or OR, and finally offers to wrap the whole thing in NOT.  Tag and
field names are completed from live data when possible.  When done,
previews the resulting S-expression and offers to copy it, insert it
as a block, run it immediately, or save it as a named query."
  (interactive)
  (let ((expr (supertag-query-library--build-condition))
        (continue t))
    (while continue
      (let ((again (completing-read "Add another condition? "
                                     '("no" "and" "or") nil t nil nil "no")))
        (if (member again '("no" ""))
            (setq continue nil)
          (let ((next (supertag-query-library--build-condition)))
            (setq expr (supertag-query-library--combine-conditions again expr next))))))
    (when (y-or-n-p "Wrap the whole query in NOT? ")
      (setq expr (list 'not expr)))
    (supertag-query-library--present-built-query expr)))

;;; --- Quick reference -------------------------------------------------------

(defconst supertag-query-library--syntax-reference-text
  "Org-Supertag Query Language -- Quick Reference
================================================

Combinators
  (and COND...)          all of COND... must match
  (or COND...)           any of COND... must match
  (not COND)             COND must not match

Leaf conditions
  (tag NAME)             nodes carrying tag NAME
  (field KEY VALUE)      nodes whose field KEY equals VALUE (exact match)
  (term WORD)            substring search over node title/content
  (after DATE)           nodes dated after DATE
  (before DATE)          nodes dated before DATE
  (between START END)    nodes dated between START and END

Date formats (DATE / START / END above)
  \"now\"                 the current moment
  \"YYYY-MM-DD\"          an absolute date
  \"-7d\", \"+2w\", \"-1m\", \"1y\"
                        a relative offset: [+-]N followed by d/w/m/y
                        (day/week/month/year).  No sign means +
                        (the future); m is approximated as 30 days,
                        y as 365.25 days.

NAME/KEY/VALUE/WORD must be strings (double-quoted); bare symbols are
also accepted for NAME/KEY/VALUE but numbers are not -- write
\"5\", not 5.

Examples (simple to complex)
  (tag \"project\")
  (field \"status\" \"active\")
  (term \"meeting\")
  (and (tag \"task\") (not (field \"status\" \"done\")))
  (or (tag \"work\") (tag \"personal\"))
  (and (tag \"task\") (after \"-7d\"))
  (and (or (tag \"work\") (tag \"project\"))
       (not (field \"status\" \"completed\"))
       (after \"2025-01-01\"))

See doc/QUERY.md for the full reference, composition examples, where
queries can be used (babel blocks, dynamic blocks, saved queries, the
guided builder), and troubleshooting."
  "Reference text shown by `supertag-query-describe-syntax'.
Kept in sync with doc/QUERY.md by hand; doc/QUERY.md is the fuller
version of this same content.")

;;;###autoload
(defun supertag-query-describe-syntax ()
  "Show a quick reference for the Org-Supertag query language."
  (interactive)
  (let ((buf (get-buffer-create "*Supertag Query Syntax*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert supertag-query-library--syntax-reference-text)
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buf)))

(provide 'supertag-query-library)

;;; supertag-query-library.el ends here
