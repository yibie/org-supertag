;;; persistence-hardening-test.el --- ERT tests for persistence-hardening features -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for the persistence-hardening work on the
;; hardening/p0-p2 branch:
;;   1. Atomic DB save (temp file + verify + rename).
;;   2. Multi-instance advisory locking.
;;   3. Auto-migration on load, with pre-migration snapshots.
;;   4. `supertag-doctor' batch report rendering.
;;
;; Every test runs inside an isolated temp directory; none of them
;; ever touch the user's real `~/.emacs.d'.
;;
;; Run:
;;   ./test/run-tests.sh persist
;;   emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/persistence-hardening-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ht)
(require 'cl-lib)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-core-persistence)
(require 'supertag-doctor)

;;; --- Shared helpers ---

(defun supertag-hardening-test--make-store (ids &optional version)
  "Return a minimal store hash table with IDS inserted into :nodes.
When VERSION is non-nil, also stamp the store's :version key with it
so migration tests can seed an out-of-date store."
  (let ((store (ht-create))
        (nodes (ht-create)))
    (dolist (id ids)
      (puthash id (list :id id :type :node :title "t" :file "/tmp/f") nodes))
    (puthash :nodes nodes store)
    (when version
      (puthash :version version store))
    store))

(defun supertag-hardening-test--write-store-file (file store)
  "Write STORE into FILE using the same print settings as persistence."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-escape-nonascii t)
          (print-length nil)
          (print-level nil)
          (print-circle t))
      (prin1 store (current-buffer)))))

(defun supertag-hardening-test--read-file-bytes (file)
  "Return the literal contents of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun supertag-hardening-test--tmp-residues (dir)
  "Return files under DIR whose name still looks like a save temp file."
  (directory-files dir nil "\\.tmp"))

(defmacro supertag-hardening-test--with-temp-env (&rest body)
  "Run BODY with persistence state redirected into an isolated temp dir.
Rebinds `supertag-data-directory', `supertag-db-file',
`supertag-db-backup-directory', and the relevant persistence defcustoms
and internal state variables, so tests never touch the real
`~/.emacs.d'. The temp directory is removed afterwards."
  (declare (indent 0))
  `(let* ((tmp (file-name-as-directory (make-temp-file "supertag-hardening-test" t)))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag-db-verify-after-save t)
          (supertag-db-lock t)
          (supertag-db-auto-migrate t)
          (supertag--store nil)
          (supertag--store-origin nil)
          (supertag--db-lock-conflict nil)
          (supertag--db-locked-file nil))
     (unwind-protect
         (progn ,@body)
       (supertag--db-release-lock)
       (ignore-errors (delete-directory tmp t)))))

;;; --- 1. Atomic save ---

(ert-deftest supertag-hardening-test-atomic-save-leaves-no-temp-residue ()
  "A successful atomic save leaves the DB file in place and no .tmp litter."
  (supertag-hardening-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (setq supertag--store (supertag-hardening-test--make-store '("A" "B" "C")))
    (supertag--persistence-write-store-atomically supertag-db-file)
    (should (file-exists-p supertag-db-file))
    (should (file-readable-p supertag-db-file))
    (let* ((loaded (supertag--persistence--try-read-store supertag-db-file))
           (nodes (gethash :nodes loaded)))
      (should (hash-table-p nodes))
      (should (= 3 (hash-table-count nodes))))
    (should (null (supertag-hardening-test--tmp-residues
                   (file-name-directory supertag-db-file))))))

(ert-deftest supertag-hardening-test-atomic-save-failure-preserves-old-db ()
  "If `rename-file' fails, the previous DB file is left untouched."
  (supertag-hardening-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (supertag-hardening-test--write-store-file
     supertag-db-file (supertag-hardening-test--make-store '("OLD")))
    (let ((original-bytes (supertag-hardening-test--read-file-bytes supertag-db-file)))
      (setq supertag--store (supertag-hardening-test--make-store '("OLD" "NEW" "NEWER")))
      (should-error
       (cl-letf (((symbol-function 'rename-file)
                  (lambda (&rest _args) (error "simulated rename failure"))))
         (supertag--persistence-write-store-atomically supertag-db-file)))
      (should (equal original-bytes
                     (supertag-hardening-test--read-file-bytes supertag-db-file)))
      (should (null (supertag-hardening-test--tmp-residues
                     (file-name-directory supertag-db-file)))))))

(ert-deftest supertag-hardening-test-verify-mismatch-aborts-save ()
  "A post-write verification mismatch aborts the save and keeps the old DB."
  (supertag-hardening-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (supertag-hardening-test--write-store-file
     supertag-db-file (supertag-hardening-test--make-store '("OLD")))
    (let ((original-bytes (supertag-hardening-test--read-file-bytes supertag-db-file)))
      (setq supertag--store (supertag-hardening-test--make-store '("OLD" "NEW")))
      ;; Force the live node count used by the verification step to disagree
      ;; with what was actually written to the temp file, without touching
      ;; the write path itself.
      (should-error
       (cl-letf (((symbol-function 'supertag--count-nodes)
                  (lambda () 999)))
         (supertag--persistence-write-store-atomically supertag-db-file)))
      (should (equal original-bytes
                     (supertag-hardening-test--read-file-bytes supertag-db-file)))
      (should (null (supertag-hardening-test--tmp-residues
                     (file-name-directory supertag-db-file)))))))

;;; --- 2. Multi-instance locking ---

(ert-deftest supertag-hardening-test-lock-conflict-blocks-save ()
  "A foreign lock artifact is detected as a conflict and blocks saving."
  (supertag-hardening-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (supertag-hardening-test--write-store-file
     supertag-db-file (supertag-hardening-test--make-store '("A")))
    (let* ((lock-file (expand-file-name
                        (concat ".#" (file-name-nondirectory supertag-db-file))
                        (file-name-directory supertag-db-file))))
      ;; Emacs advisory lock artifacts are dangling symlinks whose target
      ;; encodes "user@host.pid[:boot]"; not every filesystem supports
      ;; symlinks, so skip rather than fail when this one doesn't.
      (skip-unless
       (ignore-errors
         (make-symbolic-link "otheruser@otherhost.999999:12345" lock-file)
         t))
      (unwind-protect
          (progn
            (let ((owner (file-locked-p supertag-db-file)))
              (should (stringp owner))
              (supertag--db-acquire-lock)
              (should (equal supertag--db-lock-conflict owner))
              (let ((reasons (supertag--persistence-guard-violations)))
                (should (cl-find-if
                         (lambda (r)
                           (string-match-p "locked by another Emacs instance" r))
                         reasons)))))
        (ignore-errors (delete-file lock-file))))))

(ert-deftest supertag-hardening-test-lock-acquire-and-release-roundtrip ()
  "With no conflicting lock, acquire takes the lock and release frees it."
  (supertag-hardening-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (supertag-hardening-test--write-store-file
     supertag-db-file (supertag-hardening-test--make-store '("A")))
    (should (null (file-locked-p supertag-db-file)))
    (supertag--db-acquire-lock)
    (should (null supertag--db-lock-conflict))
    (should (eq t (file-locked-p supertag-db-file)))
    (supertag--db-release-lock)
    (should (null (file-locked-p supertag-db-file)))
    (should (null supertag--db-locked-file))))

;;; --- 3. Auto-migration ---

(ert-deftest supertag-hardening-test-auto-migrate-stamps-version-and-snapshots-once ()
  "Loading a stale store auto-migrates it once and snapshots exactly once."
  (supertag-hardening-test--with-temp-env
    (supertag-hardening-test--write-store-file
     supertag-db-file
     (supertag-hardening-test--make-store '("A") "4.0.0"))
    ;; The generic save guard also refuses to save when it thinks the
    ;; sync-state layer hasn't been loaded for this vault (a check that is
    ;; unrelated to migration itself, and always fires in an isolated test
    ;; that never loads the sync module). Neutralize just that unrelated
    ;; guard so this test can observe auto-migration's own behavior: that
    ;; the migrated store is actually persisted to disk.
    (cl-letf (((symbol-function 'supertag--persistence--expected-sync-state-file)
               (lambda () nil)))
      (supertag-load-store)
      (should (equal (gethash :version supertag--store) supertag-data-version))
      (let ((snapshots (directory-files supertag-db-backup-directory nil "premigrate")))
        (should (= 1 (length snapshots))))
      ;; The migrated version must actually have been persisted to disk,
      ;; otherwise every subsequent load would re-run the migration.
      (let* ((on-disk (supertag--persistence--try-read-store supertag-db-file)))
        (should (equal (gethash :version on-disk) supertag-data-version)))
      (supertag-load-store)
      (should (equal (gethash :version supertag--store) supertag-data-version))
      (let ((snapshots (directory-files supertag-db-backup-directory nil "premigrate")))
        (should (= 1 (length snapshots)))))))

(ert-deftest supertag-hardening-test-auto-migrate-disabled-leaves-version ()
  "With auto-migrate disabled, the stale version is left as-is on load."
  (supertag-hardening-test--with-temp-env
    (let ((supertag-db-auto-migrate nil))
      (supertag-hardening-test--write-store-file
       supertag-db-file
       (supertag-hardening-test--make-store '("A") "4.0.0"))
      (supertag-load-store)
      (should (equal (gethash :version supertag--store) "4.0.0"))
      (let ((snapshots (and (file-directory-p supertag-db-backup-directory)
                             (directory-files supertag-db-backup-directory nil "premigrate"))))
        (should (= 0 (length snapshots)))))))

;;; --- 4. Doctor ---

(ert-deftest supertag-hardening-test-doctor-batch-report-renders-sections ()
  "`supertag-doctor' in report-only mode renders all seven report sections."
  (supertag-hardening-test--with-temp-env
    (setq supertag--store (supertag-hardening-test--make-store '("A")))
    (let* ((buf (supertag-doctor t))
           (text (with-current-buffer buf (buffer-string))))
      (should (string-match-p "1\\. Database Files" text))
      (should (string-match-p "2\\. Guards" text))
      (should (string-match-p "3\\. Lock" text))
      (should (string-match-p "4\\. Version" text))
      (should (string-match-p "5\\. Integrity" text))
      (should (string-match-p "6\\. Backups" text))
      (should (string-match-p "7\\. Presence" text)))))

;;; --- 5. Cross-machine presence (advisory) ---

(defun supertag-hardening-test--write-presence-file (host seconds-ago &optional pid)
  "Write a presence JSON file for HOST whose `updatedAt' is SECONDS-AGO in the past.
Mirrors the on-disk shape written by `supertag--presence-write', without
depending on it, so these tests can independently pin down the exact
timestamp under test. Returns the presence file path."
  (let* ((file (supertag--presence-file))
         (ts (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                  (time-subtract (current-time) (seconds-to-time seconds-ago))
                                  t)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (json-encode (list (cons 'host host)
                                  (cons 'updatedAt ts)
                                  (cons 'pid (or pid 12345))))))
    file))

(ert-deftest supertag-hardening-test-presence-foreign-fresh-is-active ()
  "A foreign host's presence written moments ago is reported as active."
  (supertag-hardening-test--with-temp-env
    (supertag-hardening-test--write-presence-file "other-machine" 5)
    (let ((foreign (supertag--presence-foreign-active-p)))
      (should (equal foreign "other-machine")))
    ;; Guard-style check: callers branch on truthiness of the return value.
    (should (if (supertag--presence-foreign-active-p) t nil))))

(ert-deftest supertag-hardening-test-presence-foreign-stale-is-nil ()
  "A foreign host's presence written 10 minutes ago is stale, not active."
  (supertag-hardening-test--with-temp-env
    (supertag-hardening-test--write-presence-file "other-machine" 600)
    (should (null (supertag--presence-foreign-active-p)))))

(ert-deftest supertag-hardening-test-presence-own-host-is-nil ()
  "This host's own (fresh) presence claim never counts as foreign."
  (supertag-hardening-test--with-temp-env
    (supertag-hardening-test--write-presence-file (system-name) 5)
    (should (null (supertag--presence-foreign-active-p)))))

(ert-deftest supertag-hardening-test-presence-write-creates-valid-json ()
  "`supertag--presence-write' produces a parseable file naming this host."
  (supertag-hardening-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (supertag--presence-write)
    (let ((file (supertag--presence-file)))
      (should (file-exists-p file))
      (let* ((data (json-read-file file))
             (host (cdr (assq 'host data)))
             (updated-at (cdr (assq 'updatedAt data))))
        (should (equal host (system-name)))
        (should (stringp updated-at))
        (should (ignore-errors (parse-iso8601-time-string updated-at)))))))

(ert-deftest supertag-hardening-test-presence-write-is-atomic ()
  "`supertag--presence-write' leaves no `.tmp' residue next to the DB file."
  (supertag-hardening-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (supertag--presence-write)
    (should (null (supertag-hardening-test--tmp-residues
                   (file-name-directory (supertag--presence-file)))))))

(provide 'persistence-hardening-test)

;;; persistence-hardening-test.el ends here
