
;;; supertag-persistence-test.el --- ERT tests for persistence -*- lexical-binding: t; -*-

;;; Commentary:
;; Run:
;;   M-x eval-buffer
;;   M-x ert RET supertag-persistence-

;;; Code:

(require 'ert)
(require 'ht)

(require 'supertag-core-store)
(require 'supertag-core-persistence)

(defun supertag-persistence-test--make-store-with-nodes (ids)
  "Return a minimal store hash table with IDS inserted into :nodes."
  (let ((store (ht-create))
        (nodes (ht-create)))
    (dolist (id ids)
      (puthash id (list :id id :type :node :title "t" :file "/tmp/f") nodes))
    (puthash :nodes nodes store)
    store))

(defun supertag-persistence-test--write-db (file store)
  "Write STORE into FILE using the same print settings as persistence."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-escape-nonascii t)
          (print-length nil)
          (print-level nil)
          (print-circle t))
      (prin1 store (current-buffer)))))

(defun supertag-persistence-test--write-invalid-db (file)
  "Write an invalid lisp payload into FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert ")")))

(defmacro supertag-persistence-test--with-temp-env (&rest body)
  "Run BODY with persistence paths redirected into a temp directory."
  (declare (indent 0))
  `(let* ((tmp (file-name-as-directory (make-temp-file "supertag-persist-test" t)))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag-sync-state-file (expand-file-name "sync-state.el" tmp)))
     (setq supertag--store nil)
     (setq supertag--store-origin nil)
     ,@body))

(ert-deftest supertag-persistence-load-store-basic ()
  (supertag-persistence-test--with-temp-env
    (supertag-persistence-test--write-db
     supertag-db-file
     (supertag-persistence-test--make-store-with-nodes '("A" "B")))
    (supertag-load-store)
    (should (hash-table-p supertag--store))
    (should (= 2 (hash-table-count (gethash :nodes supertag--store))))
    (should (equal (plist-get supertag--store-origin :status) :ok))
    (should (equal (plist-get supertag--store-origin :loaded-from)
                   (expand-file-name supertag-db-file)))))

(ert-deftest supertag-persistence-load-store-failover-keeps-working ()
  (supertag-persistence-test--with-temp-env
    (let ((bad (expand-file-name "supertag-db-9999-99-99.el" supertag-data-directory)))
      (supertag-persistence-test--write-invalid-db bad)
      (supertag-persistence-test--write-db
       supertag-db-file
       (supertag-persistence-test--make-store-with-nodes '("A")))
      ;; Pass an explicit bad candidate; loader should record failure and fall back.
      (supertag-load-store bad)
      (should (= 1 (hash-table-count (gethash :nodes supertag--store))))
      (should (equal (plist-get supertag--store-origin :loaded-from)
                     (expand-file-name supertag-db-file)))
      (let* ((failures (plist-get supertag--store-origin :load-failures))
             (bad-entry (assoc (expand-file-name bad) failures)))
        (should bad-entry)
        (should (stringp (cdr bad-entry)))))))

(ert-deftest supertag-persistence-load-store-prefers-explicit-file ()
  "An explicit DB file must take priority over configured and snapshot paths."
  (supertag-persistence-test--with-temp-env
    (let ((explicit (expand-file-name "supertag-db-9999-99-99.el"
                                      supertag-data-directory)))
      (supertag-persistence-test--write-db
       supertag-db-file
       (supertag-persistence-test--make-store-with-nodes '("CONFIGURED")))
      (supertag-persistence-test--write-db
       explicit
       (supertag-persistence-test--make-store-with-nodes '("EXPLICIT")))
      (supertag-load-store explicit)
      (should (equal (plist-get supertag--store-origin :loaded-from) explicit))
      (let ((nodes (gethash :nodes supertag--store)))
        (should (gethash "EXPLICIT" nodes))
        (should-not (gethash "CONFIGURED" nodes))))))

(ert-deftest supertag-persistence-load-store-supports-print-circle ()
  (supertag-persistence-test--with-temp-env
    ;; Ensure the DB contains #n=/#n# so that `read-circle` is required.
    (let* ((id (copy-sequence "ID-1"))
           (nodes (ht-create))
           (store (ht-create)))
      ;; Use the same string object both as key and as :id to trigger print-circle references.
      (puthash id (list :id id :type :node :title "t" :file "/tmp/f") nodes)
      (puthash :nodes nodes store)
      (supertag-persistence-test--write-db supertag-db-file store)
      (with-temp-buffer
        (insert-file-contents supertag-db-file)
        (should (string-match-p "#[0-9]+=" (buffer-string))))
      (supertag-load-store)
      (should (= 1 (hash-table-count (gethash :nodes supertag--store))))
      (let* ((loaded-nodes (gethash :nodes supertag--store))
             (node (gethash id loaded-nodes)))
        (should (equal (plist-get node :id) id))))))

(ert-deftest supertag-persistence-set-db-file-compiles-without-config-guard ()
  "The persistence module must compile independently of the config guard macro."
  (let* ((source-file (or (locate-library "supertag-core-persistence.el")
                          (locate-library "supertag-core-persistence")))
         (definition
          (with-temp-buffer
            (insert-file-contents source-file)
            (re-search-forward
             "^(defun supertag--persistence--set-db-file\\_>")
            (goto-char (match-beginning 0))
            (read (current-buffer))))
         (had-macro (fboundp 'supertag-config-guard--with-allow))
         (saved-macro (and had-macro
                           (symbol-function 'supertag-config-guard--with-allow)))
         compiled-setter)
    (unwind-protect
        (progn
          (when had-macro
            (fmakunbound 'supertag-config-guard--with-allow))
          (let ((byte-compile-warnings nil))
            (setq compiled-setter
                  (byte-compile (cons 'lambda (cddr definition)))))
          (fset 'supertag-config-guard--with-allow
                (cons 'macro (lambda (&rest body) (cons 'progn body))))
          (let ((supertag-db-file nil))
            (funcall compiled-setter "/tmp/supertag-regression.db")
            (should (equal supertag-db-file "/tmp/supertag-regression.db"))))
      (if had-macro
          (fset 'supertag-config-guard--with-allow saved-macro)
        (fmakunbound 'supertag-config-guard--with-allow)))))

(provide 'supertag-persistence-test)

;;; supertag-persistence-test.el ends here
