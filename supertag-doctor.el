;;; supertag-doctor.el --- Health check and repair for Org-Supertag -*- lexical-binding: t; -*-

;; Keywords: convenience

;;; Commentary:

;; `supertag-doctor' is an interactive health-check command for the
;; Org-Supertag persistence layer.  It inspects the on-disk database
;; file(s), in-memory store guards, lock state, data version, and
;; referential integrity, then renders a report to the
;; "*Supertag Doctor*" buffer.
;;
;; Unless called with a prefix argument (or running in batch mode via
;; `noninteractive'), it will additionally offer to run a series of
;; known-safe repair commands, prompting with `y-or-n-p' before each
;; one, and appending what was run to the report buffer.
;;
;; Every check is written to degrade gracefully: if a given helper
;; function or variable is not available (e.g. because a module has
;; not been loaded, or was renamed upstream), the corresponding line
;; reports "n/a (helper not available)" instead of erroring, so the
;; command is always safe to run.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-persistence)
(require 'supertag-core-store)

;; Sync-layer repair helper. Not hard-required so this file keeps a
;; minimal dependency footprint; guarded with `fboundp' at call time.
(declare-function supertag-sync-cleanup-database "supertag-services-sync")

(defgroup supertag-doctor nil
  "Health check and repair tools for Org-Supertag."
  :group 'org-supertag)

(defconst supertag-doctor--buffer-name "*Supertag Doctor*"
  "Name of the buffer used to render the doctor report.")

;;; --- Small helpers ---

(defun supertag-doctor--insert-header (title)
  "Insert a section header for TITLE into the current buffer."
  (insert "\n" title "\n")
  (insert (make-string (string-width title) ?=) "\n"))

(defun supertag-doctor--na ()
  "Standard text used when a helper/variable is not available."
  "n/a (helper not available)")

(defun supertag-doctor--format-time (time)
  "Format TIME (an Emacs time value) as a human readable string."
  (if time
      (condition-case nil
          (format-time-string "%Y-%m-%d %H:%M:%S" time)
        (error "n/a (unparseable time)"))
    "n/a"))

(defun supertag-doctor--file-size (file)
  "Return size in bytes of FILE, or nil if it does not exist."
  (when (and (stringp file) (file-exists-p file) (not (file-directory-p file)))
    (file-attribute-size (file-attributes file))))

;;; --- Report sections ---

(defun supertag-doctor--section-database-files ()
  "Insert the \"Database Files\" section into the current buffer."
  (supertag-doctor--insert-header "1. Database Files")
  (if (not (fboundp 'supertag--persistence--db-file-candidates))
      (insert (supertag-doctor--na) "\n")
    (let ((candidates (supertag--persistence--db-file-candidates)))
      (insert (format "Candidate DB files (%d):\n" (length candidates)))
      (dolist (c candidates)
        (let ((size (supertag-doctor--file-size c)))
          (insert (format "  - %s%s\n" c
                          (if size (format " (%d bytes)" size) " (missing)")))))))
  (insert "\n")
  (let* ((active (and (boundp 'supertag-db-file) supertag-db-file))
         (active-size (and active (supertag-doctor--file-size active)))
         (in-memory-count (and (fboundp 'supertag--count-nodes)
                                (supertag--count-nodes))))
    (insert (format "Active supertag-db-file: %s\n" (or active (supertag-doctor--na))))
    (insert (format "  Size: %s\n"
                    (cond ((null active) (supertag-doctor--na))
                          (active-size (format "%d bytes" active-size))
                          (t "n/a (file missing)"))))
    (if (and active (fboundp 'supertag--persistence--try-read-store)
             (file-exists-p active) (not (file-directory-p active)))
        (condition-case err
            (let* ((data (supertag--persistence--try-read-store active))
                   (nodes-key (and (hash-table-p data) (gethash :nodes data)))
                   (on-disk-count (if (hash-table-p nodes-key) (hash-table-count nodes-key) 0)))
              (insert "  Parse check: OK (readable Lisp data)\n")
              (insert (format "  On-disk node count: %d\n" on-disk-count)))
          (error
           (insert (format "  Parse check: FAILED (%s)\n" (error-message-string err)))
           (insert (format "  On-disk node count: %s\n" (supertag-doctor--na)))))
      (insert (format "  Parse check: %s\n"
                      (if (fboundp 'supertag--persistence--try-read-store)
                          "n/a (no readable file)"
                        (supertag-doctor--na))))
      (insert (format "  On-disk node count: %s\n" (supertag-doctor--na))))
    (insert (format "  In-memory node count: %s\n"
                    (if in-memory-count in-memory-count (supertag-doctor--na))))))

(defun supertag-doctor--section-guards ()
  "Insert the \"Guards\" section into the current buffer."
  (supertag-doctor--insert-header "2. Guards")
  (insert (format "Guard violations: %s\n"
                  (if (fboundp 'supertag--persistence-guard-violations)
                      (let ((reasons (supertag--persistence-guard-violations)))
                        (if reasons (mapconcat #'identity reasons "; ") "none"))
                    (supertag-doctor--na))))
  (insert (format "Store origin: %s\n"
                  (if (boundp 'supertag--store-origin)
                      (format "%S" supertag--store-origin)
                    (supertag-doctor--na))))
  (insert (format "Dirty flag: %s\n"
                  (cond ((fboundp 'supertag-dirty-p) (if (supertag-dirty-p) "dirty" "clean"))
                        ((boundp 'supertag-db--dirty) (if supertag-db--dirty "dirty" "clean"))
                        (t (supertag-doctor--na))))))

(defun supertag-doctor--section-lock ()
  "Insert the \"Lock\" section into the current buffer."
  (supertag-doctor--insert-header "3. Lock")
  (let ((active (and (boundp 'supertag-db-file) supertag-db-file)))
    (cond
     ((not active)
      (insert (supertag-doctor--na) "\n"))
     ((not (fboundp 'file-locked-p))
      (insert "n/a (helper not available: file-locked-p)\n"))
     (t
      (let ((status (ignore-errors (file-locked-p active))))
        (insert
         (format "Lock status for %s: %s\n"
                 active
                 (cond
                  ((null status) "not locked")
                  ((eq status t) "locked by this Emacs session")
                  ((stringp status) (format "locked by another owner: %s" status))
                  (t (format "%S" status))))))))))

(defun supertag-doctor--section-version ()
  "Insert the \"Version\" section into the current buffer."
  (supertag-doctor--insert-header "4. Version")
  (let* ((target (and (boundp 'supertag-data-version) supertag-data-version))
         (store-loaded (and (boundp 'supertag--store) (hash-table-p supertag--store)))
         (current (cond
                   ((and (fboundp 'supertag--get-data-version) store-loaded)
                    (supertag--get-data-version supertag--store))
                   ((not store-loaded) "n/a (store not loaded)")
                   (t (supertag-doctor--na)))))
    (insert (format "Target version (supertag-data-version): %s\n" (or target (supertag-doctor--na))))
    (insert (format "Store version: %s\n" current))
    (when (and target store-loaded (stringp current))
      (insert (format "Match: %s\n" (if (string= target current) "yes" "NO - mismatch"))))))

(defun supertag-doctor--section-integrity ()
  "Insert the \"Integrity\" section into the current buffer."
  (supertag-doctor--insert-header "5. Integrity")
  (insert (format "Tag reference validation: %s\n"
                  (if (fboundp 'supertag--validate-tag-references)
                      (if (supertag--validate-tag-references) "PASSED" "FAILED (see *Messages*)")
                    (supertag-doctor--na))))
  (if (not (fboundp 'supertag-store-get-collection))
      (insert (format "Nodes missing :type: %s\n" (supertag-doctor--na)))
    (let ((nodes-table (supertag-store-get-collection :nodes))
          (missing 0)
          (total 0))
      (if (not (hash-table-p nodes-table))
          (insert "Nodes missing :type: n/a (nodes collection unavailable)\n")
        (maphash (lambda (_id data)
                   (cl-incf total)
                   (unless (and data (plist-get data :type))
                     (cl-incf missing)))
                 nodes-table)
        (insert (format "Nodes missing :type: %d of %d\n" missing total))))))

(defun supertag-doctor--section-backups ()
  "Insert the \"Backups\" section into the current buffer."
  (supertag-doctor--insert-header "6. Backups")
  (let ((dir (and (boundp 'supertag-db-backup-directory) supertag-db-backup-directory)))
    (insert (format "Backup directory: %s\n" (or dir (supertag-doctor--na))))
    (if (not (and dir (file-directory-p dir)))
        (insert "  (directory does not exist)\n")
      (let* ((files (directory-files dir t "^supertag-db-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.el$"))
             (dated (delq nil
                          (mapcar (lambda (f)
                                    (let ((attrs (ignore-errors (file-attributes f))))
                                      (when attrs (cons f (nth 5 attrs)))))
                                  files)))
             (sorted (sort dated (lambda (a b) (time-less-p (cdr b) (cdr a)))))
             (newest (car sorted)))
        (insert (format "  Backup count: %d\n" (length dated)))
        (if newest
            (insert (format "  Newest backup: %s (%s)\n"
                            (car newest)
                            (supertag-doctor--format-time (cdr newest))))
          (insert "  Newest backup: none found\n"))))
    (insert (format "Retention (supertag-db-backup-keep-days): %s\n"
                    (if (boundp 'supertag-db-backup-keep-days)
                        (format "%s days" supertag-db-backup-keep-days)
                      (supertag-doctor--na))))))

(defun supertag-doctor--section-presence ()
  "Insert the \"Presence\" section into the current buffer.
Reports the advisory cross-machine presence file (see
`supertag--presence-file' in supertag-core-persistence.el): its path,
whether it exists, the host and age of the last claim, and a verdict of
own / foreign-active / foreign-stale / unavailable."
  (supertag-doctor--insert-header "7. Presence")
  (if (not (fboundp 'supertag--presence-file))
      (insert (supertag-doctor--na) "\n")
    (let ((file (supertag--presence-file))
          (enabled (if (boundp 'supertag-presence-enable) supertag-presence-enable t)))
      (insert (format "Presence enabled (supertag-presence-enable): %s\n"
                      (if enabled "yes" "no")))
      (insert (format "Presence file: %s\n" (or file (supertag-doctor--na))))
      (cond
       ((not file)
        (insert "  Exists: n/a (supertag-db-file unset)\n"))
       ((not (file-exists-p file))
        (insert "  Exists: no\n"))
       (t
        (insert "  Exists: yes\n")
        (let* ((data (and (fboundp 'supertag--presence-read)
                          (ignore-errors (supertag--presence-read))))
               (host (and data (cdr (assq 'host data))))
               (updated-at (and data (cdr (assq 'updatedAt data))))
               (parsed (and (stringp updated-at)
                           (fboundp 'parse-iso8601-time-string)
                           (ignore-errors (parse-iso8601-time-string updated-at))))
               (age (and parsed (round (float-time (time-subtract (current-time) parsed))))))
          (insert (format "  Host: %s\n" (or host (supertag-doctor--na))))
          (insert (format "  Updated at: %s\n" (or updated-at (supertag-doctor--na))))
          (insert (format "  Age: %s\n" (if age (format "%d seconds" age) (supertag-doctor--na))))
          (insert
           (format
            "  Verdict: %s\n"
            (cond
             ((not (stringp host)) "n/a (unparseable presence file)")
             ((string= host (system-name)) "own (claimed by this host)")
             ((null age) "n/a (unparseable updatedAt timestamp)")
             ((and (boundp 'supertag-presence-stale-seconds)
                   (< age supertag-presence-stale-seconds))
              (format "FOREIGN, ACTIVE (%s, %ds ago; stale threshold %ds)"
                      host age supertag-presence-stale-seconds))
             (t
              (format "foreign, stale (%s, %ds ago)" host age)))))))))))

(defun supertag-doctor--build-report ()
  "Erase the current buffer and insert the full doctor report."
  (erase-buffer)
  (insert (format "Supertag Doctor Report - %s\n"
                  (format-time-string "%Y-%m-%d %H:%M:%S")))
  (insert (make-string 72 ?-) "\n")
  (supertag-doctor--section-database-files)
  (supertag-doctor--section-guards)
  (supertag-doctor--section-lock)
  (supertag-doctor--section-version)
  (supertag-doctor--section-integrity)
  (supertag-doctor--section-backups)
  (supertag-doctor--section-presence)
  (insert "\n"))

;;; --- Repairs ---

(defconst supertag-doctor--repair-commands
  '((supertag-db-migrate-and-normalize
     . "Run data migrations and normalization (supertag-db-migrate-and-normalize)")
    (supertag-db-purge-invalid-nodes
     . "Purge invalid/ghost nodes (supertag-db-purge-invalid-nodes)")
    (supertag-db-purge-duplicate-tags
     . "Purge duplicate tags (supertag-db-purge-duplicate-tags)")
    (supertag-sync-cleanup-database
     . "Validate nodes and garbage-collect orphans (supertag-sync-cleanup-database)"))
  "Repair commands offered by `supertag-doctor', in run order.")

(defun supertag-doctor--run-repairs ()
  "Offer to run each available repair command, appending results to the buffer."
  (supertag-doctor--insert-header "Repairs")
  (dolist (entry supertag-doctor--repair-commands)
    (let ((fn (car entry))
          (desc (cdr entry)))
      (cond
       ((not (fboundp fn))
        (insert (format "- SKIPPED (unavailable): %s\n" desc)))
       ((y-or-n-p (format "Run repair: %s? " desc))
        (insert (format "- RUNNING: %s\n" desc))
        (condition-case err
            (progn (funcall fn)
                   (insert "  -> done\n"))
          (error
           (insert (format "  -> ERROR: %s\n" (error-message-string err))))))
       (t
        (insert (format "- SKIPPED (declined): %s\n" desc))))))
  (cond
   ((not (fboundp 'supertag-save-store))
    (insert "- SKIPPED (unavailable): Save store (supertag-save-store)\n"))
   ((y-or-n-p "Save store now (supertag-save-store)? ")
    (insert "- RUNNING: Save store (supertag-save-store)\n")
    (condition-case err
        (progn (supertag-save-store)
               (insert "  -> done\n"))
      (error
       (insert (format "  -> ERROR: %s\n" (error-message-string err))))))
   (t
    (insert "- SKIPPED (declined): Save store (supertag-save-store)\n"))))

;;; --- Entry point ---

;;;###autoload
(defun supertag-doctor (&optional report-only)
  "Run health checks on the Org-Supertag database and report to a buffer.

Produces a report in the \"*Supertag Doctor*\" buffer covering
database files, guards, lock state, data version, integrity, and
backups.

With a prefix argument REPORT-ONLY (or when running in batch mode,
see `noninteractive'), only the report is produced and no repairs are
offered. Otherwise, after the report, a series of known-safe repair
commands are offered one at a time via `y-or-n-p', skipping any that
are not currently available (not `fboundp')."
  (interactive "P")
  (let* ((report-only (or report-only noninteractive))
         (buf (get-buffer-create supertag-doctor--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (supertag-doctor--build-report)
        (unless report-only
          (supertag-doctor--run-repairs)))
      (goto-char (point-min)))
    (unless noninteractive
      (display-buffer buf))
    buf))

(provide 'supertag-doctor)

;;; supertag-doctor.el ends here
