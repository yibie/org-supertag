;;; cl-block-regression-test.el --- guard cl-return-from usage -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for "Symbol's value as variable is void:
;; --cl-block-FN--" errors.  `cl-return-from FN' only works inside the
;; implicit block created by `cl-defun FN' (or an explicit `cl-block
;; FN').  Inside a plain `defun' it compiles to a throw against a
;; lexical catch tag that was never bound, and errors at runtime.
;;
;; Run:
;;   emacs -Q --batch -L . -l test/cl-block-regression-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)

(defconst supertag-cl-block-test--root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root.")

(defun supertag-cl-block-test--files ()
  "All elisp files in the repository that must respect the cl-block rule."
  (directory-files-recursively supertag-cl-block-test--root "\\.el\\'"))

(defun supertag-cl-block-test--scan (form blocks context report)
  "Walk FORM reporting `cl-return-from' calls with no enclosing block.
BLOCKS is the list of block-name symbols lexically in scope.
CONTEXT is the innermost enclosing function name, for reporting.
REPORT is called with CONTEXT and the offending block name.
Block names of nil are skipped: they belong to macro-generated blocks
\(`cl-dolist' and friends) invisible before macroexpansion."
  (when (consp form)
    (pcase form
      (`(quote . ,_) nil)
      (`(defun ,(and name (pred symbolp)) ,_args . ,body)
       ;; Plain defun creates no block; outer blocks stay lexically visible.
       (dolist (f body)
         (supertag-cl-block-test--scan f blocks name report)))
      (`(cl-defun ,(and name (pred symbolp)) ,_args . ,body)
       (dolist (f body)
         (supertag-cl-block-test--scan f (cons name blocks) name report)))
      (`(cl-block ,(and name (pred symbolp)) . ,body)
       (dolist (f body)
         (supertag-cl-block-test--scan f (cons name blocks) context report)))
      (`(cl-return-from ,(and name (pred symbolp)) . ,rest)
       (unless (or (null name) (memq name blocks))
         (funcall report context name))
       (dolist (f rest)
         (supertag-cl-block-test--scan f blocks context report)))
      (_
       (while (consp form)
         (supertag-cl-block-test--scan (car form) blocks context report)
         (setq form (cdr form)))))))

(ert-deftest supertag-cl-return-from-requires-block ()
  "Every `cl-return-from FN' sits inside `cl-defun FN' or `cl-block FN'."
  (let ((offenders '()))
    (dolist (file (supertag-cl-block-test--files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((report (lambda (context name)
                        (push (format "%s: %s returns from unestablished block %s"
                                      (file-name-nondirectory file)
                                      context name)
                              offenders))))
          (condition-case err
              (while t
                (supertag-cl-block-test--scan (read (current-buffer))
                                              nil nil report))
            (end-of-file nil)
            (error (push (format "%s: unreadable: %s"
                                 (file-name-nondirectory file)
                                 (error-message-string err))
                         offenders))))))
    (should (equal (nreverse offenders) '()))))

(ert-deftest supertag-sync-verify-file-nodes-skips-when-guarded ()
  "Byte-compiled `supertag-sync--verify-file-nodes' returns nil when
destructive sync is disallowed, instead of signalling a void
--cl-block-...-- variable (the async worker crash)."
  (let ((fn-form nil))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "supertag-services-sync.el"
                         supertag-cl-block-test--root))
      (goto-char (point-min))
      (condition-case nil
          (while (not fn-form)
            (let ((form (read (current-buffer))))
              (when (and (memq (car-safe form) '(defun cl-defun))
                         (eq (cadr form) 'supertag-sync--verify-file-nodes))
                (setq fn-form form))))
        (end-of-file nil)))
    (should fn-form)
    ;; Scope both the guard stub and the definition under test so the
    ;; test leaves no redefinitions behind in a live session.
    (cl-letf (((symbol-function 'supertag-sync--allow-destructive-p)
               (lambda () nil))
              ((symbol-function 'supertag-sync--verify-file-nodes) nil))
      (eval fn-form t)
      (let ((byte-compile-warnings nil))
        (byte-compile 'supertag-sync--verify-file-nodes))
      (should (null (supertag-sync--verify-file-nodes
                     "/tmp/supertag-cl-block-test-nonexistent.org"
                     (list :nodes-deleted 0)))))))

(provide 'cl-block-regression-test)
;;; cl-block-regression-test.el ends here
