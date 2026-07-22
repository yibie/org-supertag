;;; sync-worker-regression-test.el --- sync worker regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavioral regressions for the guarded sync worker: early return from
;; byte-compiled code and deferred heading deletion retries after restart or
;; async worker failure.
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

(ert-deftest supertag-sync-full-rescan-parses-unchanged-files ()
  "A full rescan reparses files even when their content hash is unchanged."
  (let* ((file (make-temp-file "supertag-full-rescan-" nil ".org" "* Note\n"))
         (state-table (make-hash-table :test 'equal))
         (supertag-sync--state (list :sync-state state-table))
         (supertag-sync--deferred-files (make-hash-table :test 'equal))
         (supertag-sync--is-full-rescan-p t)
         (parsed nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert-file-contents file)
            (puthash file
                     (list :content-hash (secure-hash 'sha1 (current-buffer)))
                     state-table))
          (cl-letf (((symbol-function 'supertag-sync--allow-destructive-p)
                     (lambda () t))
                    ((symbol-function 'supertag-sync--parse-file-header)
                     (lambda () nil))
                    ((symbol-function 'supertag--parse-org-nodes-from-current-buffer)
                     (lambda (_file) (setq parsed t) nil))
                    ((symbol-function 'supertag-sync--upsert-file-node)
                     (lambda (&rest _) nil))
                    ((symbol-function 'supertag-find-nodes-by-file)
                     (lambda (_file) nil)))
            (supertag-sync--process-single-file
             file (list :nodes-created 0 :nodes-updated 0 :nodes-deleted 0)))
          (should parsed))
      (ignore-errors (delete-file file)))))

(ert-deftest supertag-sync-deferred-file-requeues-after-worker-error ()
  "A failed worker does not leave a deferred file permanently queued."
  (let* ((file (make-temp-file "supertag-deferred-worker-" nil ".org" "* Keep\n"))
         (directory (file-name-directory file))
         (supertag-sync--state
          (list :sync-state (make-hash-table :test 'equal)))
         (supertag-sync--deferred-files (make-hash-table :test 'equal))
         (supertag-async--queue nil)
         (supertag-async--timer nil)
         (supertag-async--processor-fn
          (lambda (_file) (error "deliberate worker failure")))
         (supertag-async-batch-size 1)
         (supertag-sync-quiet-when-idle t))
    (unwind-protect
        (progn
          (puthash file :pending supertag-sync--deferred-files)
          (cl-letf (((symbol-function 'supertag-sync--effective-directories)
                     (lambda () (list directory)))
                    ((symbol-function 'supertag-sync--snapshot-build)
                     (lambda () (list :status 'complete :files (list file))))
                    ((symbol-function 'supertag-get-modified-files)
                     (lambda () nil))
                    ((symbol-function 'supertag-sync--snapshot-files-to-remove)
                     (lambda (_files) nil))
                    ((symbol-function 'supertag-sync--snapshot-new-files)
                     (lambda (_files) nil))
                    ((symbol-function 'supertag-sync--in-sync-scope-p)
                     (lambda (_file) t))
                    ((symbol-function 'supertag-sync-garbage-collect-orphaned-nodes)
                     (lambda () nil))
                    ((symbol-function 'supertag-async--ensure-timer)
                     (lambda () nil)))
            (supertag-sync--check-and-sync-guarded)
            (should (equal supertag-async--queue (list file)))
            (supertag-async--worker)
            (should (null supertag-async--queue))
            (supertag-sync--check-and-sync-guarded)
            (should (equal supertag-async--queue (list file)))))
      (ignore-errors (delete-file file)))))

(ert-deftest supertag-sync-validate-nodes-keeps-legacy-file-nodes ()
  "Validate legacy file nodes without identity metadata by file existence.
Such nodes predate `:link-type', so a live file is the only safe evidence.
A deleted-file node and a heading whose ID is absent are still orphaned."
  (let* ((file (make-temp-file "supertag-validate-" nil ".org"
                               "#+title: ai\n* Heading\nno id drawer here\n"))
         (gone-file (concat (make-temp-name
                             (expand-file-name "supertag-validate-gone-"
                                               temporary-file-directory))
                            ".org"))
         (marked '()))
    (unwind-protect
        (cl-letf (((symbol-function 'supertag-traverse-nodes)
                   (lambda (fn)
                     (funcall fn "FILE-NODE-UUID"
                              (list :id "FILE-NODE-UUID" :type :node
                                    :level 0 :file file :title "ai"))
                     (funcall fn "FILE-NODE-GONE"
                              (list :id "FILE-NODE-GONE" :type :node
                                    :level 0 :file gone-file :title "gone-file"))
                     (funcall fn "MISSING-HEADING"
                              (list :id "MISSING-HEADING" :type :node
                                    :level 1 :file file :title "gone"))))
                  ((symbol-function 'supertag-node-mark-deleted-from-file)
                   (lambda (id) (push id marked))))
          (supertag-sync-validate-nodes)
          ;; File node with a live file is kept; the deleted-file node and the
          ;; genuinely missing heading are orphaned.
          (should-not (member "FILE-NODE-UUID" marked))
          (should (member "FILE-NODE-GONE" marked))
          (should (member "MISSING-HEADING" marked))
          (should (= (length marked) 2)))
      (ignore-errors (delete-file file)))))

(defun supertag-sync-worker-test--identity-replacement
    (link-type contents old-id new-id)
  "Verify identity replacement for LINK-TYPE using CONTENTS and node IDs."
  (let* ((tmp (make-temp-file "supertag-validate-identity-" t))
         (file (expand-file-name "note.org" tmp))
         (supertag-data-directory tmp)
         (supertag-db-file (expand-file-name "supertag-db.el" tmp))
         (supertag--store nil)
         (counters (list :nodes-deleted 0)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert contents))
          (supertag--ensure-store)
          (supertag-node-create
           (list :id old-id :type :node :level 0 :link-type link-type
                 :file file :title "Old"))
          (supertag-node-create
           (list :id new-id :type :node :level 0 :link-type link-type
                 :file file :title "New"))
          (supertag-sync-validate-nodes counters)
          (should-not (plist-get (supertag-node-get old-id) :file))
          (should (equal (plist-get (supertag-node-get new-id) :file) file))
          (should (equal (car (supertag-find-file-node file)) new-id))
          (should (= (plist-get counters :nodes-deleted) 1)))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest supertag-sync-validate-nodes-orphans-replaced-org-id ()
  "A file node stops owning a file after its top-level Org ID changes."
  (supertag-sync-worker-test--identity-replacement
   'id ":PROPERTIES:\n:ID: new-file-id\n:END:\n#+TITLE: Note\n"
   "old-file-id" "new-file-id"))

(ert-deftest supertag-sync-validate-nodes-orphans-replaced-denote-id ()
  "A file node stops owning a file after its Denote identifier changes."
  (supertag-sync-worker-test--identity-replacement
   'denote "#+TITLE: Note\n#+IDENTIFIER: new-denote-id\n"
   "old-denote-id" "new-denote-id"))

(provide 'sync-worker-regression-test)
;;; sync-worker-regression-test.el ends here
