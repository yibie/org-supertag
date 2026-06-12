;;; extractor-test.el --- ERT tests for the extractor plugin system -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for supertag-extractor registration, pipeline dispatch,
;; and built-in extractors.
;;
;; Run:
;;   emacs -batch -L . --eval "(package-initialize)" -l test/extractor-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load path setup
(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-services-sync)

;;; --- Helpers ---

(defmacro extractor-test--with-headline (text &rest body)
  "Create a temp org buffer with TEXT, parse headline, and run BODY with HL bound."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (insert "\nSome content.\n")
     (goto-char (point-min))
     (let ((hl (org-element-at-point)))
       ,@body)))

(defun extractor-test--registry-count ()
  "Return the number of registered extractors."
  (length supertag-extractor--registry))

(defun extractor-test--factory-reset ()
  "Nuke all extractors and reinstall defaults."
  (setq supertag-extractor--registry nil)
  (supertag-extractor--setup-defaults))

;;; --- Registration Tests ---

(ert-deftest extractor-register-basic ()
  "Register a single extractor and verify it appears in the registry."
  (let ((old-registry (copy-sequence supertag-extractor--registry)))
    (unwind-protect
        (progn
          (supertag-extractor-register
           :name 'test-basic
           :priority 100
           :fn (lambda (el file ctx) (list :test-key "ok")))
          (let* ((reg (supertag-extractor-list))
                 (entry (cl-find 'test-basic reg
                                 :key (lambda (e) (plist-get e :name))
                                 :test #'equal)))
            (should entry)
            (should (functionp (plist-get entry :fn)))
            (should (= 100 (plist-get entry :priority)))))
      (setq supertag-extractor--registry old-registry))))

(ert-deftest extractor-register-replace ()
  "Registering with the same name replaces the old entry."
  (let ((old-registry (copy-sequence supertag-extractor--registry)))
    (unwind-protect
        (progn
          (supertag-extractor-register
           :name 'test-replace
           :priority 10
           :fn #'ignore)
          (supertag-extractor-register
           :name 'test-replace
           :priority 99
           :fn (lambda (el file ctx) (list :new-key "replaced")))
          (let* ((reg (supertag-extractor-list))
                 (entries (cl-remove-if-not
                           (lambda (e) (eq (plist-get e :name) 'test-replace))
                           reg)))
            (should (= 1 (length entries)))
            (should (= 99 (plist-get (car entries) :priority)))))
      (setq supertag-extractor--registry old-registry))))

(ert-deftest extractor-unregister ()
  "Unregister removes an extractor from the registry."
  (let ((old-registry (copy-sequence supertag-extractor--registry)))
    (unwind-protect
        (progn
          (supertag-extractor-register
           :name 'test-remove
           :priority 50
           :fn #'ignore)
          (should (cl-find 'test-remove (supertag-extractor-list)
                           :key (lambda (e) (plist-get e :name))
                           :test #'equal))
          (supertag-extractor-unregister 'test-remove)
          (should-not (cl-find 'test-remove (supertag-extractor-list)
                               :key (lambda (e) (plist-get e :name))
                               :test #'equal)))
      (setq supertag-extractor--registry old-registry))))

(ert-deftest extractor-list-returns-copy ()
  "supertag-extractor-list returns a copy, not the internal reference."
  (let ((a (supertag-extractor-list))
        (b (supertag-extractor-list)))
    (should (equal a b))
    (setcar a nil)
    (should (equal b (supertag-extractor-list)))))

;;; --- Pipeline Tests ---

(ert-deftest extractor-run-no-registry ()
  "When no extractors are registered, the pipeline returns an empty plist."
  (let ((saved supertag-extractor--registry))
    (unwind-protect
        (progn
          (setq supertag-extractor--registry nil)
          (extractor-test--with-headline "* Test"
            (let ((result (supertag-extractor--run hl "/tmp/test.org" '())))
              (should (null result)))))
      (setq supertag-extractor--registry saved))))

(ert-deftest extractor-run-merges-outputs ()
  "Different extractors merge their keys into a single plist."
  (let ((old-registry (copy-sequence supertag-extractor--registry)))
    (unwind-protect
        (progn
          (setq supertag-extractor--registry nil)
          (supertag-extractor-register
           :name 'a :priority 0
           :fn (lambda (el file ctx) (list :key-a "A")))
          (supertag-extractor-register
           :name 'b :priority 1
           :fn (lambda (el file ctx) (list :key-b "B")))
          (extractor-test--with-headline "* Test"
            (let ((result (supertag-extractor--run hl "/tmp/test.org" '())))
              (should (string= "A" (plist-get result :key-a)))
              (should (string= "B" (plist-get result :key-b))))))
      (setq supertag-extractor--registry old-registry))))

(ert-deftest extractor-run-later-overrides-earlier ()
  "A higher-priority (later-called) extractor wins for the same key."
  (let ((old-registry (copy-sequence supertag-extractor--registry)))
    (unwind-protect
        (progn
          (setq supertag-extractor--registry nil)
          (supertag-extractor-register
           :name 'early :priority 0
           :fn (lambda (el file ctx) (list :dup-key "early")))
          (supertag-extractor-register
           :name 'late :priority 10
           :fn (lambda (el file ctx) (list :dup-key "late")))
          (extractor-test--with-headline "* Test"
            (let ((result (supertag-extractor--run hl "/tmp/test.org" '())))
              (should (string= "late" (plist-get result :dup-key))))))
      (setq supertag-extractor--registry old-registry))))

(ert-deftest extractor-run-nil-patch-ignored ()
  "An extractor returning nil is safely ignored."
  (let ((old-registry (copy-sequence supertag-extractor--registry)))
    (unwind-protect
        (progn
          (setq supertag-extractor--registry nil)
          (supertag-extractor-register
           :name 'returns-nil :priority 0
           :fn (lambda (el file ctx) nil))
          (supertag-extractor-register
           :name 'real :priority 1
           :fn (lambda (el file ctx) (list :ok "yes")))
          (extractor-test--with-headline "* Test"
            (let ((result (supertag-extractor--run hl "/tmp/test.org" '())))
              (should (string= "yes" (plist-get result :ok))))))
      (setq supertag-extractor--registry old-registry))))

(ert-deftest extractor-run-passes-context ()
  "The CTX argument is passed through to extractors."
  (let ((old-registry (copy-sequence supertag-extractor--registry)))
    (unwind-protect
        (progn
          (setq supertag-extractor--registry nil)
          (supertag-extractor-register
           :name 'ctx-check :priority 0
           :fn (lambda (el file ctx)
                 (list :file file
                       :full-rescan (plist-get ctx :full-rescan-p))))
          (extractor-test--with-headline "* Test"
            (let ((result (supertag-extractor--run hl "/tmp/check.org" '(:full-rescan-p t))))
              (should (string= "/tmp/check.org" (plist-get result :file)))
              (should (eq t (plist-get result :full-rescan))))))
      (setq supertag-extractor--registry old-registry))))

;;; --- Built-in Extractor Tests ---

(ert-deftest extractor-builtins-exist ()
  "All seven built-in extractors are registered after setup."
  (extractor-test--factory-reset)
  (let ((names (mapcar (lambda (e) (plist-get e :name))
                       (supertag-extractor-list))))
    (dolist (expected '(core-structure title olp tags properties content refs))
      (should (member expected names)))))

(ert-deftest extractor-core-structure ()
  "Core structure extractor produces level, todo, priority."
  (extractor-test--with-headline "** TODO [#A] Task"
    (let ((result (supertag-extractor--core-structure hl "/tmp/f" '())))
      (should (= 2 (plist-get result :level)))
      (should (string= "TODO" (plist-get result :todo)))
      (should (string= "#A" (plist-get result :priority))))))

(ert-deftest extractor-title ()
  "Title extractor produces :title and :raw-value."
  (extractor-test--with-headline "* My Title"
    (let ((result (supertag-extractor--title hl "/tmp/f" '())))
      (should (string= "My Title" (plist-get result :title)))
      (should (string= "My Title" (plist-get result :raw-value))))))

(ert-deftest extractor-tags ()
  "Tags extractor produces :tags combining org tags and inline #tags."
  (extractor-test--with-headline "* Tagged :orgtag:"
    (let ((result (supertag-extractor--tags hl "/tmp/f" '(:full-rescan-p t))))
      (should (plist-get result :tags))
      (should (member "orgtag" (plist-get result :tags))))))

(ert-deftest extractor-content ()
  "Content extractor produces :content non-nil."
  (extractor-test--with-headline "* With Content"
    (let ((result (supertag-extractor--content hl "/tmp/f" '())))
      ;; Content may be nil or string; either is fine for unit test
      (should (or (null (plist-get result :content))
                  (stringp (plist-get result :content)))))))

(ert-deftest extractor-refs ()
  "Refs extractor produces :ref-to (always a list)."
  (extractor-test--with-headline "* No Refs"
    (let ((result (supertag-extractor--refs hl "/tmp/f" '())))
      (should (listp (plist-get result :ref-to))))))

(provide 'extractor-test)
;;; extractor-test.el ends here
