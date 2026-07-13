;;; sync-worker-regression-test.el --- sync worker regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavioral regressions for the guarded sync worker: early return from
;; byte-compiled code and restart-safe deferred heading deletion.
;;
;; Run:
;;   emacs -Q --batch -L . --eval "(package-initialize)" \
;;     -l test/sync-worker-regression-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)
(require 'supertag-services-sync)

(defconst supertag-sync-worker-test--root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root.")

(ert-deftest supertag-sync-verify-file-nodes-skips-when-guarded ()
  "Byte-compiled `supertag-sync--verify-file-nodes' returns nil when
destructive sync is disallowed, instead of signalling a void
--cl-block-...-- variable (the async worker crash)."
  (let ((fn-form nil))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "supertag-services-sync.el"
                         supertag-sync-worker-test--root))
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

(ert-deftest supertag-sync-deferred-deletion-retries-after-restart ()
  "A guarded deletion remains retryable when another node also changes.
The deferred-files table is session-local, so persisted sync state must keep
the old mtime until destructive cleanup is allowed."
  (let* ((file (make-temp-file "supertag-deferred-" nil ".org" "* Keep\n"))
         (state-table (make-hash-table :test 'equal))
         (supertag-sync--state (list :sync-state state-table))
         (supertag-sync--deferred-files (make-hash-table :test 'equal))
         (mtime (file-attribute-modification-time (file-attributes file)))
         (state-saved nil))
    (unwind-protect
        (progn
          (puthash file
                   (list :mtime (time-subtract mtime (seconds-to-time 60))
                         :size 1 :content-hash "old" :hash-algo 'sha1)
                   state-table)
          (cl-letf (((symbol-function 'supertag-sync--allow-destructive-p)
                     (lambda () nil))
                    ((symbol-function 'supertag-sync--parse-file-header)
                     (lambda () nil))
                    ((symbol-function 'supertag--parse-org-nodes-from-current-buffer)
                     (lambda (_file)
                       (list (list :id "keep" :file file :level 1
                                   :title "new"))))
                    ((symbol-function 'supertag-sync--upsert-file-node)
                     (lambda (&rest _) nil))
                    ((symbol-function 'supertag-find-nodes-by-file)
                     (lambda (_file)
                       (list (cons "gone" (list :id "gone" :file file :level 1))
                             (cons "keep" (list :id "keep" :file file :level 1
                                                :title "old")))))
                    ((symbol-function 'supertag-node-changed-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'supertag--merge-node-properties)
                     (lambda (new _old) new))
                    ((symbol-function 'supertag-db-add-with-hash)
                     (lambda (&rest _) nil))
                    ((symbol-function 'supertag-node-mark-deleted-from-file)
                     (lambda (&rest _)
                       (ert-fail "destructive deletion ran while guarded")))
                    ((symbol-function 'supertag-sync-save-state)
                     (lambda () (setq state-saved t))))
            (supertag-sync--async-processor file))
          (should state-saved)
          (should (gethash file supertag-sync--deferred-files))
          ;; Restart drops only the in-memory retry marker.
          (clrhash supertag-sync--deferred-files)
          (cl-letf (((symbol-function 'supertag-sync--in-sync-scope-p)
                     (lambda (_file) t)))
            (should (equal (list file) (supertag-get-modified-files)))))
      (ignore-errors (delete-file file)))))

(provide 'sync-worker-regression-test)
;;; sync-worker-regression-test.el ends here
