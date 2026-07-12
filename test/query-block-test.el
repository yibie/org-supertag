;;; query-block-test.el --- ERT tests for supertag-ui-query-block.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for the query-block upgrade on the hardening/p0-p2
;; branch: the shared babel/dynamic-block rendering core in
;; `supertag-ui-query-block.el', its :sort/:order/:limit/:columns result
;; controls, and its "never signal, render an error string instead"
;; guarantee.
;;
;; Every test runs inside an isolated temp directory with a freshly reset
;; in-memory store; none of them ever touch the user's real `~/.emacs.d'.
;;
;; Run:
;;   ./test/run-tests.sh query
;;   emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/query-block-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-ops-node)
(require 'supertag-ops-tag)
(require 'supertag-ops-field)
(require 'supertag-services-query)
(require 'supertag-ui-query-block)

;;; --- Shared helpers ---

(defmacro query-block-test--with-clean-store (&rest body)
  "Run BODY with a clean isolated store in a temp directory."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "supertag-query-block-test" t))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag--store nil)
          (supertag--store-origin nil))
     (unwind-protect
         (progn
           (supertag--ensure-store)
           ,@body)
       (ignore-errors (delete-directory tmp t)))))

(defun query-block-test--time (day-offset)
  "Return a distinct Emacs time value, DAY-OFFSET days after a fixed epoch."
  (seconds-to-time (+ 1700000000 (* day-offset 86400))))

(defun query-block-test--make-fixture ()
  "Populate the current store with 10 nodes across 2 tags.
Six nodes are tagged \"project\" with a numeric \"priority\" field and
distinct :created-at timestamps; four are tagged \"area\" (no field).
Returns the list of created node ids, in creation order."
  (supertag-tag-create (list :id "project" :name "project"))
  (supertag-tag-create (list :id "area" :name "area"))
  (let (ids)
    (cl-loop
     for i from 1 to 6
     for priority in '(5 3 9 1 7 2)
     do (let* ((id (format "proj-%d" i))
               (node (supertag-node-create
                      (list :id id
                            :title (format "Node %d" i)
                            :file "/tmp/query-block-test.org"
                            :created-at (query-block-test--time i)))))
          (supertag-node-add-tag id "project")
          (supertag-field-set id "project" "priority" priority)
          (push id ids)))
    (cl-loop
     for i from 7 to 10
     do (let* ((id (format "area-%d" i))
               (node (supertag-node-create
                      (list :id id
                            :title (format "Node %d" i)
                            :file "/tmp/query-block-test.org"
                            :created-at (query-block-test--time i)))))
          (supertag-node-add-tag id "area")
          (push id ids)))
    (nreverse ids)))

(defun query-block-test--link-count (table)
  "Count how many node-link cells (\"[[id:...\") appear in TABLE."
  (let ((count 0) (start 0))
    (while (string-match "\\[\\[id:" table start)
      (setq count (1+ count) start (match-end 0)))
    count))

;;; --- 1. Babel executor: existing behavior guard ---

(ert-deftest query-block-babel-tag-query-basic ()
  "A plain tag query returns a table with exactly the tagged nodes."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (let ((table (org-babel-execute:org-supertag-query-block
                  "(tag \"project\")" nil)))
      (should (stringp table))
      (should (= 6 (query-block-test--link-count table)))
      (dolist (i '(1 2 3 4 5 6))
        (should (string-match-p (format "Node %d" i) table)))
      (dolist (i '(7 8 9 10))
        (should-not (string-match-p (format "Node %d\\b" i) table)))
      (should (string-match-p "|[ \t]*Node[ \t]*|[ \t]*Tags[ \t]*|" table)))))

;;; --- 2. :limit / :order / :sort ---

(ert-deftest query-block-babel-limit-truncates ()
  ":limit truncates the result rows after sorting."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (let ((table (org-babel-execute:org-supertag-query-block
                  "(tag \"project\")"
                  '((:sort . "priority") (:order . "asc") (:limit . 3)))))
      (should (= 3 (query-block-test--link-count table)))
      ;; Ascending by priority: 1 (Node 4), 2 (Node 6), 3 (Node 2).
      (should (string-match-p "Node 4" table))
      (should (string-match-p "Node 6" table))
      (should (string-match-p "Node 2" table))
      (should-not (string-match-p "Node 1\\b" table))
      (should-not (string-match-p "Node 3\\b" table))
      (should-not (string-match-p "Node 5\\b" table)))))

(ert-deftest query-block-babel-sort-numeric-field-orders-numerically ()
  ":sort on a numeric field orders rows numerically, not lexically."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (let* ((table (org-babel-execute:org-supertag-query-block
                   "(tag \"project\")"
                   '((:sort . "priority") (:order . "asc"))))
           (pos-4 (string-match "Node 4" table))
           (pos-6 (string-match "Node 6" table))
           (pos-2 (string-match "Node 2" table))
           (pos-1 (string-match "Node 1" table))
           (pos-5 (string-match "Node 5" table))
           (pos-3 (string-match "Node 3" table)))
      ;; priorities: N4=1, N6=2, N2=3, N1=5, N5=7, N3=9 -- ascending order.
      (should (< pos-4 pos-6 pos-2 pos-1 pos-5 pos-3)))))

(ert-deftest query-block-babel-order-desc-reverses ()
  ":order desc reverses the sorted order."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (let* ((table (org-babel-execute:org-supertag-query-block
                   "(tag \"project\")"
                   '((:sort . "priority") (:order . "desc"))))
           (pos-3 (string-match "Node 3" table))
           (pos-1 (string-match "Node 1" table))
           (pos-4 (string-match "Node 4" table)))
      ;; Descending: Node 3 (priority 9) first, Node 4 (priority 1) last.
      (should (< pos-3 pos-1 pos-4)))))

;;; --- 3. :columns override ---

(ert-deftest query-block-babel-columns-overrides-auto-columns ()
  ":columns overrides the fields auto-derived from the query AST."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (let ((table (org-babel-execute:org-supertag-query-block
                  "(tag \"project\")"
                  '((:columns . ("priority"))))))
      (should (string-match-p "|[ \t]*Node[ \t]*|[ \t]*Tags[ \t]*|[ \t]*priority[ \t]*|" table))
      ;; The priority values themselves should show up as cell contents.
      (should (string-match-p "5" table))
      (should (string-match-p "9" table)))))

;;; --- 4. Dynamic block ---

(ert-deftest query-block-dblock-writer-inserts-table ()
  "`org-dblock-write:supertag-query' renders a table via `org-dblock-update'."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (with-temp-buffer
      (org-mode)
      (insert "#+BEGIN: supertag-query :query \"(tag \\\"project\\\")\"\n#+END:\n")
      (goto-char (point-min))
      (org-update-dblock)
      (let ((text (buffer-string)))
        (should (= 6 (query-block-test--link-count text)))
        (should (string-match-p "Node 1" text))
        (should (string-match-p (regexp-quote "#+BEGIN: supertag-query") text))
        (should (string-match-p "#\\+END:" text))))))

(ert-deftest query-block-dblock-writer-honors-params ()
  "The dynamic block honors :sort/:order/:limit/:columns like the babel path."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (with-temp-buffer
      (org-mode)
      (insert "#+BEGIN: supertag-query :query \"(tag \\\"project\\\")\" :sort priority :order desc :limit 2 :columns (\"priority\")\n#+END:\n")
      (goto-char (point-min))
      (org-update-dblock)
      (let ((text (buffer-string)))
        (should (= 2 (query-block-test--link-count text)))
        (should (string-match-p "Node 3" text))
        (should (string-match-p "priority" text))))))

;;; --- 5. Errors never signal ---

(ert-deftest query-block-malformed-query-renders-error-no-signal ()
  "A malformed query s-expression renders a one-line error, never signals."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (let ((table (org-babel-execute:org-supertag-query-block
                  "(this-is-not-an-operator 1 2)" nil)))
      (should (stringp table))
      (should (string-prefix-p "Error:" table)))))

(ert-deftest query-block-unparseable-sexp-renders-error-no-signal ()
  "An unparseable s-expression string renders a one-line error, never signals."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (let ((table (org-babel-execute:org-supertag-query-block
                  "(tag \"project\"" nil))) ; missing closing paren
      (should (stringp table))
      (should (string-prefix-p "Error:" table)))))

(ert-deftest query-block-invalid-order-renders-error-no-signal ()
  "An invalid :order value renders a one-line error, never signals."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (let ((table (org-babel-execute:org-supertag-query-block
                  "(tag \"project\")" '((:order . "sideways")))))
      (should (stringp table))
      (should (string-prefix-p "Error:" table)))))

(ert-deftest query-block-dblock-malformed-query-renders-error-no-signal ()
  "The dynamic block renders a one-line error for a malformed query too."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (with-temp-buffer
      (org-mode)
      (insert "#+BEGIN: supertag-query :query \"(bogus-op)\"\n#+END:\n")
      (goto-char (point-min))
      (org-update-dblock)
      (let ((text (buffer-string)))
        (should (string-match-p "Error:" text))))))

(ert-deftest query-block-dblock-missing-query-renders-error-no-signal ()
  "The dynamic block renders a one-line error when :query is absent."
  (query-block-test--with-clean-store
    (query-block-test--make-fixture)
    (with-temp-buffer
      (org-mode)
      (insert "#+BEGIN: supertag-query\n#+END:\n")
      (goto-char (point-min))
      (org-update-dblock)
      (let ((text (buffer-string)))
        (should (string-match-p "Error:" text))))))

(provide 'query-block-test)

;;; query-block-test.el ends here
