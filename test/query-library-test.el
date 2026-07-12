;;; query-library-test.el --- ERT tests for supertag-query-library.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for the query learnability/reusability layer added in
;; supertag-query-library.el:
;;   1. The builder's pure S-expression assembly helpers
;;      (`supertag-query-library--make-condition' and
;;      `supertag-query-library--combine-conditions'), validated against the
;;      real parser (`supertag-query--parse-sexp').
;;   2. Saved-query alist round-trip through a temporary custom-file.
;;   3. The `*Supertag Query Syntax*' quick-reference buffer renders.
;;   4. Running a saved query renders node links/tags via the real query
;;      engine (`supertag-query--parse-sexp' / `supertag-query--execute-ast'),
;;      against an isolated in-memory store.
;;
;; None of these tests touch the user's real init file or `~/.emacs.d'.
;;
;; Run:
;;   ./test/run-tests.sh query-library
;;   emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/query-library-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-services-query)
(require 'supertag-query-library)

;;; --- 1. Builder assembly helpers -----------------------------------------

(ert-deftest supertag-query-library-test-make-condition-builds-valid-leaf-sexps ()
  "`supertag-query-library--make-condition' builds sexps the parser accepts."
  (should (equal (supertag-query-library--make-condition "tag" "project")
                 '(tag "project")))
  (should (equal (supertag-query-library--make-condition "field" "status" "active")
                 '(field "status" "active")))
  (should (equal (supertag-query-library--make-condition "term" "meeting")
                 '(term "meeting")))
  (should (equal (supertag-query-library--make-condition "after" "-7d")
                 '(after "-7d")))
  (should (equal (supertag-query-library--make-condition "between" "-7d" "now")
                 '(between "-7d" "now")))
  ;; Every produced sexp must also be independently accepted by the real
  ;; parser (this is implicit in --make-condition, but assert it explicitly
  ;; so a future refactor can't silently skip validation).
  (dolist (sexp (list '(tag "project") '(field "status" "active")
                       '(term "meeting") '(after "-7d") '(between "-7d" "now")))
    (should (supertag-query--parse-sexp sexp))))

(ert-deftest supertag-query-library-test-make-condition-rejects-bad-arity ()
  "Wrong argument counts surface the parser's own error, not a silent build."
  (should-error (supertag-query-library--make-condition "tag" "a" "b"))
  (should-error (supertag-query-library--make-condition "field" "only-one-arg"))
  (should-error (supertag-query-library--make-condition "between" "-7d")))

(ert-deftest supertag-query-library-test-combine-conditions-wraps-and-validates ()
  "`supertag-query-library--combine-conditions' nests conditions under and/or."
  (let* ((c1 (supertag-query-library--make-condition "tag" "task"))
         (c2 (supertag-query-library--make-condition "tag" "work"))
         (and-sexp (supertag-query-library--combine-conditions "and" c1 c2))
         (or-sexp (supertag-query-library--combine-conditions "or" c1 c2)))
    (should (equal and-sexp '(and (tag "task") (tag "work"))))
    (should (equal or-sexp '(or (tag "task") (tag "work"))))
    ;; Combinator can also be a symbol, not just a string.
    (should (equal (supertag-query-library--combine-conditions 'and c1 c2) and-sexp))
    ;; Nesting multiple combine steps (left-associative, as the interactive
    ;; loop in `supertag-query-build' does) still parses.
    (let ((nested (supertag-query-library--combine-conditions
                   "or" and-sexp (supertag-query-library--make-condition "term" "x"))))
      (should (equal nested '(or (and (tag "task") (tag "work")) (term "x"))))
      (should (supertag-query--parse-sexp nested)))))

;;; --- 2. Saved-query round trip through a temp custom-file ----------------

(defmacro supertag-query-library-test--with-temp-custom-file (&rest body)
  "Run BODY with `custom-file'/`user-init-file' pointed at a fresh temp file.
`supertag-query-saved' is reset to nil beforehand and restored afterwards,
and the temp file is removed afterwards.  This never touches the user's
real init file."
  (declare (indent 0))
  `(let* ((tmp-file (make-temp-file "supertag-query-library-test" nil ".el"))
          (custom-file tmp-file)
          (user-init-file tmp-file)
          (saved-backup supertag-query-saved))
     (with-temp-file tmp-file (insert ";; empty custom-file for tests\n"))
     (unwind-protect
         (progn
           (setq supertag-query-saved nil)
           ,@body)
       (setq supertag-query-saved saved-backup)
       (ignore-errors (delete-file tmp-file)))))

(ert-deftest supertag-query-library-test-save-persists-to-custom-file ()
  "Saving a query updates the alist in-session and writes it to disk."
  (supertag-query-library-test--with-temp-custom-file
    (should (supertag-query-library--customize-save-possible-p))
    (supertag-query-save "(tag \"project\")" "my-projects")
    (should (equal (cdr (assoc "my-projects" supertag-query-saved))
                    "(tag \"project\")"))
    ;; The value must actually have been written to disk, not just held in
    ;; the session variable.
    (let ((on-disk (with-temp-buffer
                      (insert-file-contents custom-file)
                      (buffer-string))))
      (should (string-match-p "my-projects" on-disk))
      (should (string-match-p "tag \\\\\"project" on-disk)))
    ;; Saving again under the same name updates rather than duplicates.
    (supertag-query-save "(tag \"projects-v2\")" "my-projects")
    (should (= 1 (length supertag-query-saved)))
    (should (equal (cdr (assoc "my-projects" supertag-query-saved))
                    "(tag \"projects-v2\")"))))

(ert-deftest supertag-query-library-test-save-rejects-empty-name-and-bad-query ()
  "Saving validates both the name and that the query actually parses."
  (supertag-query-library-test--with-temp-custom-file
    (should-error (supertag-query-save "(tag \"x\")" ""))
    (should-error (supertag-query-save "(tag \"x\")" "   "))
    (should-error (supertag-query-save "(bogus-operator \"x\")" "broken"))
    (should (null supertag-query-saved))))

;;; --- 3. Quick-reference buffer --------------------------------------------

(ert-deftest supertag-query-library-test-describe-syntax-renders-buffer ()
  "`supertag-query-describe-syntax' renders a read-only reference buffer."
  (unwind-protect
      (progn
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
            (supertag-query-describe-syntax)))
        (let ((buf (get-buffer "*Supertag Query Syntax*")))
          (should buf)
          (with-current-buffer buf
            (should (derived-mode-p 'special-mode))
            (should buffer-read-only)
            (let ((text (buffer-string)))
              (should (string-match-p "(tag NAME)" text))
              (should (string-match-p "(field KEY VALUE)" text))
              (should (string-match-p "(between START END)" text))
              (should (string-match-p "doc/QUERY.md" text))))))
    (ignore-errors (kill-buffer "*Supertag Query Syntax*"))))

;;; --- 4. Running a saved query against an isolated store ------------------

(defmacro supertag-query-library-test--with-isolated-store (&rest body)
  "Run BODY with `supertag--store' rebound to a fresh, empty store."
  (declare (indent 0))
  `(let ((supertag--store nil))
     (supertag--ensure-store)
     ,@body))

(ert-deftest supertag-query-library-test-run-saved-renders-results-buffer ()
  "Running a saved query shows matching nodes as links with a Tags column."
  (supertag-query-library-test--with-isolated-store
    (puthash "n1"
             (list :id "n1" :title "Write report" :tags '("task" "work")
                   :link-type "id")
             (supertag-store-get-collection :nodes))
    (puthash "n2"
             (list :id "n2" :title "Buy groceries" :tags '("errand")
                   :link-type "id")
             (supertag-store-get-collection :nodes))
    (supertag-query-library-test--with-temp-custom-file
      (supertag-query-save "(tag \"task\")" "my-tasks")
      (unwind-protect
          (let ((buf (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
                       (supertag-query-run-saved "my-tasks")
                       (get-buffer "*Supertag Saved Query*"))))
            (should buf)
            (with-current-buffer buf
              (let ((text (buffer-string)))
                (should (string-match-p "Write report" text))
                (should (string-match-p "task" text))
                (should-not (string-match-p "Buy groceries" text)))))
        (ignore-errors (kill-buffer "*Supertag Saved Query*"))))))

(provide 'query-library-test)

;;; query-library-test.el ends here
