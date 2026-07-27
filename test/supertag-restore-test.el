;;; supertag-restore-test.el --- ERT tests for supertag-restore's pure parts -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the snapshot helpers and the destructive restore path, including
;; legacy summaries, downgrade restores, pre-restore recovery points, and
;; multi-instance locking.
;;
;; Every test runs inside an isolated temp directory; none of them ever
;; touch the user's real `~/.emacs.d'.
;;
;; Run:
;;   emacs -batch -L . -L test --eval "(package-initialize)" \
;;     -l ert -l test/supertag-restore-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ht)
(require 'cl-lib)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-core-persistence)

;;; --- Helpers ---

(defmacro supertag-restore-test--with-temp-dir (var &rest body)
  "Bind VAR to a fresh temp directory for BODY, removed afterwards."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "supertag-restore-test" t))))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defun supertag-restore-test--touch (dir name &optional time)
  "Create an empty snapshot file NAME under DIR, stamped with TIME.
TIME defaults to the current time. Writes a minimal, readable store so
`supertag--restore-snapshot-summary' has something to parse."
  (let ((file (expand-file-name name dir)))
    (with-temp-file file
      (let ((store (ht-create))
            (nodes (ht-create)))
        (puthash "n1" (list :id "n1" :type :node) nodes)
        (puthash :nodes nodes store)
        (puthash :version "6.0.0" store)
        (let ((print-length nil) (print-level nil) (print-circle t))
          (prin1 store (current-buffer)))))
    (when time
      (set-file-times file time))
    file))

(defun supertag-restore-test--make-store (ids &optional version root-key)
  "Return a minimal store containing IDS under ROOT-KEY.
VERSION defaults to `supertag-data-version' and ROOT-KEY to :nodes."
  (let ((store (ht-create))
        (nodes (ht-create)))
    (dolist (id ids)
      (puthash id (list :id id :type :node :title id) nodes))
    (puthash (or root-key :nodes) nodes store)
    (puthash :version (or version supertag-data-version) store)
    store))

(defun supertag-restore-test--write-store (file store)
  "Write STORE to FILE in the legacy single-sexp format."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-length nil)
          (print-level nil)
          (print-circle t))
      (prin1 store (current-buffer)))))

(defun supertag-restore-test--read-file (file)
  "Return FILE's literal contents."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defmacro supertag-restore-test--with-temp-env (&rest body)
  "Run BODY with persistence redirected to an isolated temp directory."
  (declare (indent 0))
  `(supertag-restore-test--with-temp-dir tmp
     (let ((supertag-data-directory tmp)
           (supertag-db-file (expand-file-name "supertag-db.el" tmp))
           (supertag-db-backup-directory (expand-file-name "backups" tmp))
           (supertag-db-lock t)
           (supertag-db-auto-migrate t)
           (supertag-db-verify-after-save t)
           (supertag--store nil)
           (supertag--store-origin nil)
           (supertag--db-lock-conflict nil)
           (supertag--db-locked-file nil))
       (unwind-protect
           (progn ,@body)
         (supertag--db-release-lock)))))

(defun supertag-restore-test--run-command (snapshot)
  "Run `supertag-restore', selecting SNAPSHOT and confirming the restore."
  (let ((name (file-name-nondirectory snapshot)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt labels &rest _)
                 (or (cl-find-if (lambda (label)
                                   (string-match-p (regexp-quote name) label))
                                 labels)
                     (ert-fail (format "Snapshot label not found: %s" name)))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (supertag-restore))))

;;; --- Classification ---

(ert-deftest supertag-restore-test-kind-classifies-all-three-patterns ()
  (should (eq 'daily (supertag--restore-snapshot-kind "supertag-db-2026-07-20.el")))
  (should (eq 'premigrate (supertag--restore-snapshot-kind "supertag-db-premigrate-4-0-0-20260720-101500.el")))
  (should (eq 'preformat6 (supertag--restore-snapshot-kind "supertag-db-preformat6-20260720-101500.el")))
  (should (eq 'prerestore (supertag--restore-snapshot-kind "supertag-db-prerestore-20260720-101500-abc123.el"))))

(ert-deftest supertag-restore-test-kind-rejects-unrelated-files ()
  (should (null (supertag--restore-snapshot-kind "supertag-db.el")))
  (should (null (supertag--restore-snapshot-kind "supertag-db.el.tmp123")))
  (should (null (supertag--restore-snapshot-kind "sync-state.el")))
  (should (null (supertag--restore-snapshot-kind "supertag-db-not-a-date.el"))))

(ert-deftest supertag-restore-test-kind-label-covers-known-kinds ()
  (should (equal "daily" (supertag--restore-snapshot-kind-label 'daily)))
  (should (equal "pre-migration" (supertag--restore-snapshot-kind-label 'premigrate)))
  (should (equal "pre-format6" (supertag--restore-snapshot-kind-label 'preformat6)))
  (should (equal "pre-restore" (supertag--restore-snapshot-kind-label 'prerestore))))

;;; --- Enumeration + sorting ---

(ert-deftest supertag-restore-test-list-finds-all-kinds-sorted-newest-first ()
  (supertag-restore-test--with-temp-dir dir
    (let* ((now (current-time))
           (older (time-subtract now (seconds-to-time 200)))
           (oldest (time-subtract now (seconds-to-time 400))))
      (supertag-restore-test--touch dir "supertag-db-2026-07-18.el" oldest)
      (supertag-restore-test--touch dir "supertag-db-premigrate-4-0-0-20260719-000000.el" older)
      (supertag-restore-test--touch dir "supertag-db-preformat6-20260720-000000.el" now)
      (supertag-restore-test--touch dir "supertag-db-prerestore-20260720-000000-abc123.el"
                                    (time-add now (seconds-to-time 100)))
      ;; Unrelated file in the same directory must not show up as a snapshot.
      (supertag-restore-test--touch dir "sync-state.el" now)
      (let ((snapshots (supertag--restore-snapshot-list dir)))
        (should (= 4 (length snapshots)))
        (should (equal '(prerestore preformat6 premigrate daily)
                        (mapcar (lambda (s) (plist-get s :kind)) snapshots)))))))

(ert-deftest supertag-restore-test-list-nil-for-missing-directory ()
  (should (null (supertag--restore-snapshot-list "/nonexistent/dir/for/restore/test"))))

(ert-deftest supertag-restore-test-list-nil-for-empty-directory ()
  (supertag-restore-test--with-temp-dir dir
    (should (null (supertag--restore-snapshot-list dir)))))

;;; --- Summary + labeling ---

(ert-deftest supertag-restore-test-summary-reads-nodes-and-version ()
  (supertag-restore-test--with-temp-dir dir
    (let* ((file (supertag-restore-test--touch dir "supertag-db-2026-07-20.el")))
      (let ((summary (supertag--restore-snapshot-summary file)))
        (should (= 1 (plist-get summary :nodes)))
        (should (= 0 (plist-get summary :tags)))
        (should (equal "6.0.0" (plist-get summary :version)))))))

(ert-deftest supertag-restore-test-summary-normalizes-legacy-root-keys ()
  (supertag-restore-test--with-temp-dir dir
    (let* ((file (expand-file-name "supertag-db-premigrate-5-0-0-legacy.el" dir))
           (store (supertag-restore-test--make-store '("n1" "n2") "5.0.0" 'nodes))
           (tags (ht-create)))
      (puthash "t1" (list :id "t1" :type :tag) tags)
      (puthash "tags" tags store)
      (supertag-restore-test--write-store file store)
      (let ((summary (supertag--restore-snapshot-summary file)))
        (should (= 2 (plist-get summary :nodes)))
        (should (= 1 (plist-get summary :tags)))
        (should (equal "5.0.0" (plist-get summary :version)))))))

(ert-deftest supertag-restore-test-describe-includes-kind-and-node-count ()
  (supertag-restore-test--with-temp-dir dir
    (supertag-restore-test--touch dir "supertag-db-2026-07-20.el")
    (let* ((snapshots (supertag--restore-snapshot-list dir))
           (label (supertag--restore-snapshot-describe (car snapshots))))
      (should (string-match-p "daily" label))
      (should (string-match-p "1" label))
      (should (string-match-p "supertag-db-2026-07-20\\.el" label)))))

;;; --- Destructive restore path ---

(ert-deftest supertag-restore-test-dirty-store-gets-unique-recovery-snapshot ()
  (supertag-restore-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (supertag-restore-test--write-store
     supertag-db-file (supertag-restore-test--make-store '("disk")))
    (supertag-load-store)
    (setq supertag--store (supertag-restore-test--make-store '("memory-1" "memory-2")))
    (supertag-mark-dirty)
    ;; An existing daily backup must not suppress the per-restore recovery point.
    (supertag-restore-test--write-store
     (supertag-get-backup-filename (format-time-string "%Y-%m-%d"))
     (supertag-restore-test--make-store '("daily")))
    (let ((snapshot (expand-file-name
                     "supertag-db-preformat6-20260720-101500.el"
                     supertag-db-backup-directory)))
      (supertag-restore-test--write-store
       snapshot (supertag-restore-test--make-store '("restored")))
      (supertag-restore-test--run-command snapshot)
      (let ((recovery (directory-files supertag-db-backup-directory t
                                       "\\`supertag-db-prerestore-.*\\.el\\'")))
        (should (= 1 (length recovery)))
        (should (= 2 (plist-get (supertag--restore-snapshot-summary (car recovery))
                                :nodes)))))))

(ert-deftest supertag-restore-test-lock-conflict-refuses-before-replacement ()
  (supertag-restore-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (supertag-restore-test--write-store
     supertag-db-file (supertag-restore-test--make-store '("live")))
    (supertag-load-store)
    (let* ((before (supertag-restore-test--read-file supertag-db-file))
           (snapshot (expand-file-name
                      "supertag-db-2026-07-20.el"
                      supertag-db-backup-directory)))
      (supertag-restore-test--write-store
       snapshot (supertag-restore-test--make-store '("snapshot")))
      (should-error
       (cl-letf (((symbol-function 'supertag--db-acquire-lock)
                  (lambda () (setq supertag--db-lock-conflict "other@host.123"))))
         (supertag-restore-test--run-command snapshot))
       :type 'user-error)
      (should (equal before (supertag-restore-test--read-file supertag-db-file))))))

(ert-deftest supertag-restore-test-keeps-lock-through-reload ()
  (supertag-restore-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (supertag-restore-test--write-store
     supertag-db-file (supertag-restore-test--make-store '("live")))
    (supertag-load-store)
    (let ((snapshot (expand-file-name
                     "supertag-db-2026-07-20.el"
                     supertag-db-backup-directory))
          (release-count 0))
      (supertag-restore-test--write-store
       snapshot (supertag-restore-test--make-store '("snapshot")))
      (cl-letf (((symbol-function 'supertag--db-release-lock)
                 (lambda () (cl-incf release-count))))
        (supertag-restore-test--run-command snapshot))
      (should (= 0 release-count))
      (should (eq t (file-locked-p supertag-db-file))))))

(ert-deftest supertag-restore-test-downgrade-snapshot-is-not-auto-migrated ()
  (supertag-restore-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (supertag-restore-test--write-store
     supertag-db-file (supertag-restore-test--make-store '("live")))
    (supertag-load-store)
    (let* ((snapshot (expand-file-name
                      "supertag-db-premigrate-5-0-0-20260720-101500.el"
                      supertag-db-backup-directory))
           (old-store (supertag-restore-test--make-store '("old") "5.0.0")))
      (supertag-restore-test--write-store snapshot old-store)
      (let ((snapshot-bytes (supertag-restore-test--read-file snapshot)))
        (supertag-restore-test--run-command snapshot)
        (should (equal "5.0.0" (supertag--get-data-version supertag--store)))
        (should (equal snapshot-bytes
                       (supertag-restore-test--read-file supertag-db-file)))))))

(provide 'supertag-restore-test)

;;; supertag-restore-test.el ends here
