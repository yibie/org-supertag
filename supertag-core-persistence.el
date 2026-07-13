;;; org-supertag/supertag-persistence.el --- Data persistence for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides functions for persisting the Org-Supertag
;; in-memory store to a file and loading it back.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'json) ; For presence-file encode/decode
(require 'parse-time) ; For parse-iso8601-time-string, used by presence
(require 'supertag-core-notify) ; For supertag-subscribe and supertag-emit-event
(require 'supertag-core-store) ; For supertag--store
(require 'supertag-core-index) ; For relation index rebuild after load
(require 'supertag-core-transform) ; For supertag-with-transaction (real per-entity rollback)

;;; --- Persistence Configuration ---
;; Note: supertag-data-directory is defined in org-supertag.el
;; This is a fallback definition in case this module is loaded independently
(defvar supertag-data-directory
  (expand-file-name "org-supertag/" user-emacs-directory)
  "Directory for storing Org-Supertag data.
This is a fallback definition. The primary definition is in org-supertag.el.")

(defvar supertag--config-guard-allow)

(defconst supertag-data-version "5.0.0"
  "Current data format version.
Used for data format compatibility checks and automatic migration.")

(defun supertag-data-file (filename)
  "Get full path for data file.
FILENAME is relative to `supertag-data-directory`."
  (expand-file-name filename supertag-data-directory))

(defcustom supertag-db-file
  (supertag-data-file "supertag-db.el")
  "Database file path."
  :type 'file
  :group 'org-supertag)

(defcustom supertag-db-backup-directory
  (supertag-data-file "backups")
  "Directory for database backups."
  :type 'directory
  :group 'org-supertag)

(defcustom supertag-db-auto-save-interval 300
  "Auto-save interval in seconds.
Set to nil to disable auto-save."
  :type '(choice (const :tag "Disable" nil)
                (integer :tag "Interval (seconds)"))
  :group 'org-supertag)

(defcustom supertag-db-backup-interval 86400
  "Daily backup interval in seconds (default: 24 hours).
Set to nil to disable daily backups."
  :type '(choice (const :tag "Disable" nil)
                (integer :tag "Interval (seconds)"))
  :group 'org-supertag)

(defcustom supertag-db-backup-keep-days 3
  "Number of days to keep daily backups.
Older backups will be automatically cleaned up."
  :type 'integer
  :group 'org-supertag)

(defcustom supertag-db-verify-after-save t
  "When non-nil, verify the database file after saving.
The freshly written file is re-read and its :nodes collection count is
compared against the in-memory store before the previous database file
is replaced. On mismatch or read error, the write is aborted and the
previous database file is left untouched."
  :type 'boolean
  :group 'org-supertag)

(defcustom supertag-db-lock t
  "When non-nil, protect the database from concurrent multi-instance access.
Uses Emacs' built-in advisory file locking (`lock-file', `unlock-file',
`file-locked-p') on `supertag-db-file'. When another Emacs instance already
holds the lock, this session records the conflict in
`supertag--db-lock-conflict' and refuses to save the database until the
other instance releases the lock (or `supertag-db-retry-lock' is used once
it has exited)."
  :type 'boolean
  :group 'org-supertag)

(defcustom supertag-db-auto-migrate t
  "When non-nil, automatically migrate an out-of-date database after load.
After `supertag-load-store' successfully loads `supertag-db-file', if the
loaded store's :version does not match `supertag-data-version', this session
runs `supertag-db-migrate-and-normalize' automatically instead of requiring
the user to invoke it by hand (see `supertag--maybe-auto-migrate').

A timestamped pre-migration snapshot of the database file is written to
`supertag-db-backup-directory' before migrating. When nil, out-of-date
databases are left as-is after loading; migrate manually with
\\[supertag-db-migrate-and-normalize]."
  :type 'boolean
  :group 'org-supertag)

(defcustom supertag-presence-enable t
  "When non-nil, write and check an advisory presence file for cross-machine
awareness.

Org-SuperTag's database is a single serialized file. Users who sync it via
Dropbox/iCloud/etc. get that sync service's \"whole file, last writer wins,
no warning\" semantics — running Emacs against the same synced database on
two machines at once can silently discard one side's edits. This is NOT a
lock (a sync service's minutes-scale propagation delay means it cannot
physically be one); it is a best-effort, advisory heads-up: a small JSON
file recording which host last touched the database, and when, is written
next to `supertag-db-file' on load and periodically while this session
runs. When another host's presence looks recently active, loading warns
loudly. See README \"Syncing across machines\" for the supported
single-writer workflow this is meant to nudge users toward."
  :type 'boolean
  :group 'org-supertag)

(defcustom supertag-presence-stale-seconds 300
  "Age in seconds beyond which a foreign presence record is ignored.
A presence record written by another host more than this many seconds ago
is treated as stale — that machine is presumed no longer actively editing —
and `supertag--presence-foreign-active-p' returns nil for it."
  :type 'integer
  :group 'org-supertag)

(defvar supertag-db--auto-save-timer nil
  "Timer for auto-save.")

(defvar supertag-db--backup-timer nil
  "Timer for daily backup.")

(defvar supertag-db--dirty nil
  "Flag indicating if database has unsaved changes.")

(defvar supertag-db--last-backup-date nil
  "Date of last backup in YYYY-MM-DD format.")

(defvar supertag--store-origin nil
  "Metadata about the loaded store and its originating persistence state.")

(defvar supertag--db-lock-conflict nil
  "Non-nil when another Emacs instance holds the DB lock.
Holds the owner description string returned by `file-locked-p' (for example
\"user@host.12345:1698765432\") for whichever file `supertag--db-acquire-lock'
last checked. While non-nil, this session refuses to save the database (see
`supertag--persistence-guard-violations'). Cleared automatically once the
lock is acquired or the other instance's lock is found to be gone.")

(defvar supertag--db-locked-file nil
  "File path this Emacs instance currently holds the advisory lock for, or nil.
Tracked separately from `supertag-db-file' so that switching vaults (which
reassigns `supertag-db-file' before the old lock is released) still releases
the correct file's lock.")

;;; --- Multi-instance DB Locking ---

(defun supertag--db-acquire-lock ()
  "Acquire the advisory lock on `supertag-db-file' for this Emacs instance.
When `supertag-db-lock' is enabled and `supertag-db-file' is set, checks
`file-locked-p' on it: if another Emacs instance already holds the lock
\(i.e. `file-locked-p' returns a string, not t), records the owner in
`supertag--db-lock-conflict' and warns that this session will not save the
database until the conflict clears. Otherwise, clears any previous conflict
and calls `lock-file' — locally binding `create-lockfiles' to t, since
`lock-file' is a no-op when that variable is nil. Any error signaled while
locking is caught and reported via `message' but never propagated, so a
locking problem can never break DB loading."
  (when (and supertag-db-lock
             (stringp supertag-db-file)
             (> (length supertag-db-file) 0))
    (let ((owner (file-locked-p supertag-db-file)))
      (if (stringp owner)
          (progn
            (setq supertag--db-lock-conflict owner)
            (message "Supertag: database %s is locked by another Emacs instance (%s); this session will NOT save until the lock is released. Run M-x supertag-db-retry-lock once the other instance has exited."
                     (abbreviate-file-name supertag-db-file) owner))
        (setq supertag--db-lock-conflict nil)
        (condition-case err
            (let ((create-lockfiles t))
              (lock-file supertag-db-file)
              (setq supertag--db-locked-file supertag-db-file))
          (error
           (message "Supertag: failed to acquire lock on %s: %s (continuing without a lock)"
                    (abbreviate-file-name supertag-db-file)
                    (error-message-string err))))))))

(defun supertag--db-release-lock ()
  "Release the advisory DB lock held by this Emacs instance, if any.
Safe no-op when no lock is currently held (`supertag--db-locked-file' is
nil). Any error from `unlock-file' is ignored, since a failed unlock must
never interrupt shutdown or vault switching."
  (when supertag--db-locked-file
    (ignore-errors (unlock-file supertag--db-locked-file))
    (setq supertag--db-locked-file nil))
  (setq supertag--db-lock-conflict nil))

(defun supertag-db-retry-lock ()
  "Retry acquiring the DB lock after a previously detected conflict.
Useful once the other Emacs instance holding the lock on `supertag-db-file'
has exited: re-checks `file-locked-p' and, if the lock is now free (or
already held by this instance), calls `supertag--db-acquire-lock' to take
it over so saves can resume."
  (interactive)
  (supertag--db-acquire-lock)
  (if supertag--db-lock-conflict
      (message "Supertag: database %s is still locked by another Emacs instance (%s)."
               (abbreviate-file-name supertag-db-file) supertag--db-lock-conflict)
    (message "Supertag: database lock acquired for %s."
             (abbreviate-file-name supertag-db-file))))

;;; --- Cross-machine Presence (advisory; S0 of the git-sync hardening plan) ---
;;
;; `supertag--db-acquire-lock' above only ever sees *this machine's* other
;; Emacs instances (`lock-file' writes a local symlink next to the DB file,
;; which most sync services do not even propagate reliably, and certainly
;; not promptly). It cannot detect a second machine editing the same
;; Dropbox/iCloud-synced database. Presence closes that visibility gap with
;; an ordinary, sync-friendly JSON file instead of a lock primitive: it is
;; written periodically and read on load, purely advisory, and never blocks
;; a save the way a lock conflict does.

(defvar supertag--presence-write-failed nil
  "Non-nil once a presence-file write has failed and been warned about.
Keeps `supertag--presence-write' from spamming a `display-warning' on every
auto-save timer tick after the first failure — the underlying condition
(for example an unwritable data directory) is unlikely to resolve itself
between ticks, so warn once per session and go quiet.")

(defun supertag--presence-file ()
  "Return the path of the advisory cross-machine presence file, or nil.
The file lives NEXT TO `supertag-db-file' (same directory) rather than in a
dedicated state directory, because that shared directory is exactly what
sync services like Dropbox/iCloud propagate for the users this feature is
for. For local-only users, an extra small file there is harmless.
Returns nil when `supertag-db-file' is unset or empty."
  (when (and (stringp supertag-db-file) (> (length supertag-db-file) 0))
    (expand-file-name "supertag-presence.json"
                       (file-name-directory supertag-db-file))))

(defun supertag--presence-write ()
  "Best-effort, atomic write of this session's presence claim.
Writes `{\"host\": ..., \"updatedAt\": ..., \"pid\": ...}' (via
`json-encode') to `supertag--presence-file', using the same temp-file +
`rename-file' pattern as `supertag--persistence-write-store-atomically' so a
concurrent reader never observes a half-written file. `updatedAt' is an
ISO 8601 UTC timestamp.

Never signals: `supertag-presence-enable' nil is a no-op, a nil
`supertag--presence-file' (unset `supertag-db-file') is a no-op, and any
other error is caught and reported via `display-warning' at most once per
session (see `supertag--presence-write-failed') rather than propagated —
a presence-file problem must never break a save or a load."
  (when supertag-presence-enable
    (condition-case err
        (let ((file (supertag--presence-file)))
          (when file
            (let* ((dir (file-name-directory file))
                   (payload (json-encode
                             (list (cons 'host (system-name))
                                   (cons 'updatedAt
                                         (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                              nil t))
                                   (cons 'pid (emacs-pid)))))
                   (temp-file (make-temp-file (concat file ".tmp")))
                   (success nil))
              (unless (file-exists-p dir)
                (make-directory dir t))
              (unwind-protect
                  (progn
                    (with-temp-buffer
                      (set-buffer-file-coding-system 'utf-8-unix)
                      (insert payload)
                      (write-region (point-min) (point-max) temp-file nil 'silent))
                    (rename-file temp-file file t)
                    (setq success t))
                (unless success
                  (ignore-errors (delete-file temp-file)))))))
      (error
       (unless supertag--presence-write-failed
         (setq supertag--presence-write-failed t)
         (display-warning
          'org-supertag
          (format "Supertag: failed to write cross-machine presence file: %s"
                  (error-message-string err))
          :warning))))))

(defun supertag--presence-read ()
  "Return the parsed presence file as an alist, or nil on any error.
Parses `supertag--presence-file' via `json-read-file'. Returns nil when
`supertag-db-file' is unset, the presence file does not exist, or it fails
to parse as JSON — callers must treat nil as \"no usable presence
information\", never as an error."
  (let ((file (supertag--presence-file)))
    (when (and file (file-exists-p file))
      (condition-case nil
          (json-read-file file)
        (error nil)))))

(defun supertag--presence-foreign-active-p ()
  "Return the foreign host string when another machine's presence is active.
Non-nil only when all of the following hold: the presence file exists and
parses, its recorded `host' differs from `(system-name)', its `updatedAt'
parses as ISO 8601 (via `parse-iso8601-time-string'), and that timestamp is
within `supertag-presence-stale-seconds' of now. Returns nil when the file
is missing/unparseable, records this host, or is stale (older than the
threshold)."
  (let* ((data (supertag--presence-read))
         (host (and data (cdr (assq 'host data))))
         (updated-at (and data (cdr (assq 'updatedAt data)))))
    (when (and (stringp host)
               (not (string= host (system-name)))
               (stringp updated-at))
      (let ((parsed (ignore-errors (parse-iso8601-time-string updated-at))))
        (when parsed
          (let ((age (float-time (time-subtract (current-time) parsed))))
            (when (< age supertag-presence-stale-seconds)
              host)))))))

(defun supertag--presence-check-and-claim ()
  "Warn about a recently-active foreign presence, then claim this host's.
Meant to run right after a successful `supertag-load-store'. When
`supertag--presence-foreign-active-p' reports another host was recently
active on this database, shows a loud, actionable `display-warning' (not a
`message': a status-bar message is too easy to miss at exactly the moment
this matters, right after opening a database another machine may still be
writing to). Afterwards — whether or not a warning was shown — writes this
session's own presence via `supertag--presence-write', claiming the
database for this host going forward."
  (when supertag-presence-enable
    (let ((foreign-host (supertag--presence-foreign-active-p)))
      (when foreign-host
        (let* ((data (supertag--presence-read))
               (updated-at (and data (cdr (assq 'updatedAt data))))
               (parsed (and (stringp updated-at)
                            (ignore-errors (parse-iso8601-time-string updated-at))))
               (age (and parsed (round (float-time (time-subtract (current-time) parsed))))))
          (display-warning
           'org-supertag
           (format "SUPERTAG: ANOTHER MACHINE MAY STILL BE EDITING THIS DATABASE.

Host %s was active on this database %s ago (%s).

This database file has no merge support: if you keep editing on both
machines at the same time, whichever one saves LAST WINS and the other
machine's changes are silently discarded — there will be no error, no
conflict marker, just quietly lost work.

If you are done editing on %s, this warning is safe to ignore.
Otherwise, quit Emacs there before continuing to edit here.

See README \"Syncing across machines\" for the supported workflow."
                   foreign-host
                   (if age (format "%d second%s" age (if (= age 1) "" "s")) "recently")
                   (abbreviate-file-name supertag-db-file)
                   foreign-host)
           :warning))))
    (supertag--presence-write)))

(defun supertag--presence-release ()
  "Best-effort delete of this host's own presence claim.
Meant to run on `kill-emacs-hook'. Deletes `supertag--presence-file' ONLY
when it still names this host (`(system-name)') — if another, newer machine
has since overwritten it with its own claim, that claim is left alone,
since deleting it would erase real presence information that other host's
own load-time check depends on. Any error is ignored: a failed delete must
never interrupt shutdown."
  (when supertag-presence-enable
    (ignore-errors
      (let* ((file (supertag--presence-file))
             (data (and file (file-exists-p file) (supertag--presence-read)))
             (host (and data (cdr (assq 'host data)))))
        (when (and file (stringp host) (string= host (system-name)))
          (delete-file file))))))

;;; --- Backup Functions ---

(defun supertag-get-backup-filename (date-str)
  "Generate backup filename for given DATE-STR in YYYY-MM-DD format."
  (expand-file-name
   (format "supertag-db-%s.el" date-str)
   supertag-db-backup-directory))

(defun supertag-create-daily-backup ()
  "Create a daily backup of the database if needed.
Returns t if backup was created, nil if not needed."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (backup-file (supertag-get-backup-filename today)))
    (if (file-exists-p backup-file)
        nil
      (when (file-exists-p supertag-db-file)
        (supertag-persistence-ensure-data-directory)
        (copy-file supertag-db-file backup-file)
        (setq supertag-db--last-backup-date today)
        (message "Daily backup created: %s" backup-file)
        t))))

(defun supertag-cleanup-old-backups ()
  "Remove backup files older than `supertag-db-backup-keep-days` days."
  (when (file-exists-p supertag-db-backup-directory)
    (let* ((cutoff-time (time-subtract (current-time)
                                      (days-to-time supertag-db-backup-keep-days)))
           (backup-files (directory-files supertag-db-backup-directory t
                                         "^supertag-db-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.el$"))
           (removed-count 0))
      (dolist (backup-file backup-files)
        (let ((file-time (nth 5 (file-attributes backup-file))))
          (when (time-less-p file-time cutoff-time)
            (delete-file backup-file)
            (cl-incf removed-count)
            (message "Removed old backup: %s" backup-file))))
      (when (> removed-count 0)
        (message "Cleaned up %d old backup files" removed-count)))))

(defun supertag-backup-database-now ()
  "Force create a backup immediately and clean up old backups.
This function can be called interactively by users."
  (interactive)
  (supertag-create-daily-backup)
  (supertag-cleanup-old-backups))

(defun supertag-check-daily-backup ()
  "Check if daily backup is needed and create one if necessary."
  (let ((today (format-time-string "%Y-%m-%d")))
    (unless (string= today supertag-db--last-backup-date)
      (when (supertag-create-daily-backup)
        (supertag-cleanup-old-backups)))))

;;; --- Persistence Functions ---

(defun supertag-mark-dirty ()
  "Mark database as having unsaved changes."
  (setq supertag-db--dirty t))

(defun supertag-clear-dirty ()
  "Clear database unsaved changes flag."
  (setq supertag-db--dirty nil))

(defun supertag-dirty-p ()
  "Check if database has unsaved changes."
  supertag-db--dirty)

(defun supertag--count-nodes ()
  "Return the number of node entries in the store."
  (let ((nodes-table (supertag-store-get-collection :nodes)))
    (if (hash-table-p nodes-table)
        (hash-table-count nodes-table)
      0)))

(defun supertag--count-field-values ()
  "Return the total number of field values stored."
  (let ((root (supertag-store-get-collection :field-values))
        (count 0))
    (when (hash-table-p root)
      (maphash
       (lambda (_node-id node-table)
         (when (hash-table-p node-table)
           (cl-incf count (hash-table-count node-table))))
       root))
    count))

(defun supertag--persistence--normalize-path (path)
  "Normalize PATH for comparison, or nil when PATH is invalid."
  (when (and (stringp path) (> (length path) 0))
    (expand-file-name path)))

(defun supertag--persistence--expected-sync-state-file ()
  "Return expected sync-state path derived from current data directory."
  (when (and (boundp 'supertag-data-directory)
             (stringp supertag-data-directory)
             (> (length supertag-data-directory) 0))
    (expand-file-name "sync-state.el"
                      (file-name-as-directory
                       (expand-file-name supertag-data-directory)))))

(defun supertag--persistence--data-dir ()
  "Return normalized `supertag-data-directory`, or nil when unset."
  (when (and (boundp 'supertag-data-directory)
             (stringp supertag-data-directory)
             (> (length supertag-data-directory) 0))
    (file-name-as-directory
     (expand-file-name supertag-data-directory))))

(defun supertag--persistence--default-db-file ()
  "Return the default DB file path derived from `supertag-data-directory`."
  (let ((dir (supertag--persistence--data-dir)))
    (when dir
      (expand-file-name "supertag-db.el" dir))))

(defun supertag--persistence--newest-db-snapshot (&optional dir)
  "Return newest DB snapshot file under DIR (or `supertag-data-directory`).

This is a best-effort fallback for legacy filenames like `supertag-db-YYYY-MM-DD.el`
or files with a `.db` extension that still contain an Emacs-lisp printed store."
  (let* ((dir (or dir (supertag--persistence--data-dir))))
    (when (and dir (file-directory-p dir))
      (let* ((candidates (directory-files dir t "^supertag-db-.*\\.\\(el\\|db\\)$" t))
             (dated (delq nil
                          (mapcar (lambda (path)
                                    (let ((attrs (ignore-errors (file-attributes path))))
                                      (when attrs
                                        (cons path (nth 5 attrs)))))
                                  candidates)))
             (sorted (sort dated (lambda (a b) (time-less-p (cdr b) (cdr a))))))
        (car (car sorted))))))

(defun supertag--persistence--db-file-candidates (&optional file)
  "Return a de-duplicated list of DB file candidates for FILE/current config."
  (let* ((explicit (supertag--persistence--normalize-path file))
         (configured (supertag--persistence--normalize-path supertag-db-file))
         (default (supertag--persistence--default-db-file))
         (legacy (let ((dir (supertag--persistence--data-dir)))
                   (when dir
                     (expand-file-name "supertag-db.db" dir))))
         (snapshot (supertag--persistence--newest-db-snapshot)))
    (cl-delete-duplicates (delq nil (list explicit configured default legacy snapshot))
                          :test #'string=
                          :from-end t)))

(defun supertag--persistence--pick-readable-file (paths)
  "Return the first readable regular file in PATHS, or nil."
  (cl-loop for path in paths
           for expanded = (and (stringp path)
                               (> (length path) 0)
                               (ignore-errors (expand-file-name path)))
           when (and expanded
                     (file-exists-p expanded)
                     (not (file-directory-p expanded))
                     (file-readable-p expanded))
           return expanded))

(defun supertag--persistence--set-db-file (path)
  "Set `supertag-db-file` to PATH, respecting config guard when available."
  (when (and (stringp path) (> (length path) 0))
    (let ((supertag--config-guard-allow t))
      (setq supertag-db-file path))))

;;; --- S2: Canonical, deterministic, line-per-entity serialization ---
;;
;; Design goal (see .phrase/phases/phase-git-sync-20260713/PLAN.md "S2 规范化
;; 序列化"): same logical store content must produce byte-identical output on
;; any machine, and a single-field change on one entity must show up as a
;; single-line `git diff'. This is a hard prerequisite for S3's git merge
;; driver, which parses this exact line format.
;;
;; FORMAT (frozen once tests pass — S3 depends on it):
;;
;;   ;; -*- mode: lisp-data; coding: utf-8-unix -*-
;;   ;; supertag-db canonical format 1
;;   (:version "5.0.0")                              <- root scalars, one line
;;   (:collection :nodes :id "node-a" :data (...))    <- one line per entity
;;   (:collection :nodes :id "node-b" :data (...))
;;   (:collection :tags :id "tag-x" :data (...))
;;
;; - Root keys of the store are split into "collections" (hash-table valued
;;   — printed one entity per line, collections ordered alphabetically by
;;   keyword name, entities within a collection ordered by `string<' of their
;;   id's string form) and "scalars" (everything else, e.g. :version —
;;   merged into a single sorted plist line).
;; - Every plist appearing inside entity :data is printed with its keys
;;   sorted alphabetically, RECURSIVELY into nested plists. Ordinary
;;   (non-plist) lists are walked but never reordered — only their elements
;;   are recursively canonicalized, preserving original element order.
;; - PRE-CHECK FINDING (hash tables nested in entity data): the store is NOT
;;   uniformly plists-all-the-way-down. `supertag-store-get-collection
;;   :fields' (legacy three-level node -> tag -> field nesting, see
;;   `supertag--normalize-fields-collection' above) and `:field-values'
;;   (two-level node -> field nesting, see `supertag-store-put-field-value'
;;   in supertag-core-store.el) are populated by direct hash-table puts that
;;   bypass `supertag-store-put-entity'/`supertag--normalize-entity', so the
;;   ENTITY VALUE for those two collections is itself a hash table, not a
;;   plist. Rather than assert "always a plist" and error on this (as a
;;   naive first cut might), `supertag--persistence--canonicalize-value'
;;   below handles hash tables found ANYWHERE in entity data generically: it
;;   freezes them into a sorted, re-readable `(:supertag-hash-table ...)'
;;   marker form (see that function and `supertag--persistence--thaw-value'
;;   for the inverse). This also means no assumption is silently made about
;;   which collections may contain nested hash tables — any hash table
;;   anywhere in the tree is handled the same way.
;; - PRE-CHECK FINDING (cross-entity shared structure): a probe over the
;;   10k-node fixture in test/perf-benchmark.el found ~20,000 `eq'-shared
;;   cons cells — but ALL of it traced back to that fixture building a
;;   single `field-defs' list once and reusing the SAME object across all 50
;;   synthetic tags' :fields slot (the fixture docstring says outright it
;;   "bypass[es] the ops/commit layer entirely"). Real write paths
;;   (e.g. `supertag-tag-create' in supertag-ops-tag.el) go through
;;   `supertag--deep-copy-plist' specifically to avoid this kind of aliasing.
;;   Regardless of which case applies, per-entity independent `prin1' (no
;;   `print-circle' spanning multiple lines) makes cross-entity sharing a
;;   pure non-issue for correctness: each entity's data is fully
;;   materialized on its own line, so shared structure just gets printed
;;   (and, after a load, held) as separate `equal'-but-not-`eq' copies. No
;;   coercion-layer surgery was needed in supertag-core-store.el.
;; - `print-circle' is still bound around each per-line `prin1' call, which
;;   protects against a genuinely self-referential value WITHIN one entity's
;;   own data (distinct from cross-entity sharing above); Emacs builds a
;;   fresh circular-reference table for every top-level `prin1' call, so
;;   binding it once around the whole write (rather than re-binding inside
;;   the loop) still gives each line independent, correct handling.

(defconst supertag--persistence-canonical-format-header
  ";; -*- mode: lisp-data; coding: utf-8-unix -*-\n;; supertag-db canonical format 1\n"
  "Two-line header written at the top of every canonical-format DB file.")

(defconst supertag--persistence--hash-marker :supertag-hash-table
  "Marker keyword identifying a frozen hash table in canonical output.
See `supertag--persistence--canonicalize-value' and
`supertag--persistence--thaw-value'.")

(defun supertag--persistence--sort-key (key)
  "Return a string used to order KEY (an entity id or hash-table key).
Coerces KEY to a string form equivalent to `format \"%s\"' so ids/keys of
different Lisp types (strings, keywords, numbers, symbols) still sort
consistently and deterministically against each other. Special-cased for
the two overwhelmingly common cases — a plain string (most entity ids)
returned as-is, and a symbol/keyword (all plist keys) via `symbol-name'
— to skip `format''s general parsing machinery, since this runs on every
entity id and every plist key in the whole store."
  (cond
   ((stringp key) key)
   ((symbolp key) (symbol-name key))
   (t (format "%s" key))))

(defun supertag--persistence--sort-pairs-by-key (pairs)
  "Return PAIRS (each a cons `(KEY . VALUE)') sorted by KEY's sort key.
A Schwartzian transform: `supertag--persistence--sort-key' is computed
exactly ONCE per pair up front, rather than repeatedly inside the sort
comparator. This matters at the DB's real scale — computing it inside the
comparator costs O(n log n) `format' calls (one measured run over 5k
entities alone made the canonical writer ~14x slower than a plain
`prin1' dump, almost entirely `format' overhead in comparators; this
transform brought it back under the 2x perf-guard budget in
test/canonical-serialization-test.el)."
  (mapcar #'cdr
          (sort (mapcar (lambda (pair)
                          (cons (supertag--persistence--sort-key (car pair)) pair))
                        pairs)
                (lambda (a b) (string< (car a) (car b))))))

(defun supertag--persistence--plist-p (value)
  "Conservatively detect whether VALUE looks like a plist.
Returns non-nil only for a proper, non-empty, even-length list whose
element at every even (0-based) index is a keyword. This deliberately
excludes nil/empty lists, improper (dotted) lists, and lists of
non-keyword-prefixed items — e.g. `:tag-field-associations' values (an
ordinary list of association plists) do not themselves satisfy this
predicate, so they are walked element-by-element but never key-sorted or
reordered as a whole.

This predicate alone is used only by callers outside the hot
canonicalization path (e.g. the canonical-format reader, which calls it
at most once per line); `supertag--persistence--canonicalize-value' below
uses the fused `supertag--persistence--plist-pairs-or-nil' instead, which
detects AND collects in one pass — see that function's docstring for why."
  (let ((len (proper-list-p value)))
    (and len
         (> len 0)
         (cl-evenp len)
         (cl-loop for cell on value by #'cddr
                  always (keywordp (car cell))))))

(defun supertag--persistence--plist-pairs-or-nil (value)
  "Return VALUE's `(SORT-KEY KEY . RAW-VALUE)' triples if it is a plist, else nil.
A fused, single-pass replacement for calling
`supertag--persistence--plist-p' (itself a full traversal) and THEN
separately collecting key/value pairs (a second full traversal): this
walks VALUE exactly once, bailing out immediately on the first sign it
is not a plist (an odd remaining length, or a non-keyword key), and
precomputes each key's `supertag--persistence--sort-key' along the way so
the caller's `sort' never needs to recompute it. This is on the hottest
path in the file — `supertag--persistence--canonicalize-value' runs it on
every cons it visits — and collapsing several O(n) passes into one made a
measurable difference on the 5k-node perf-guard benchmark in
test/canonical-serialization-test.el (originally ~14x the cost of a
plain `prin1' dump; the budget is 2x)."
  (let ((cursor value) (pairs nil) (ok t))
    (while (and ok (consp cursor))
      (let ((k (car cursor)))
        (if (and (keywordp k) (consp (cdr cursor)))
            (progn
              (push (cons (supertag--persistence--sort-key k) (cons k (cadr cursor))) pairs)
              (setq cursor (cddr cursor)))
          (setq ok nil))))
    (and ok (null cursor) pairs)))

(defsubst supertag--persistence--canonicalize-maybe-atom (value)
  "Like `supertag--persistence--canonicalize-value', but inlined and with a
fast exit for anything that cannot possibly need canonicalizing: only
conses, hash tables, and vectors are ever restructured, so the
overwhelmingly common case of a plain leaf value (string/number/keyword/
symbol/nil — most values in most plists) skips the real function call
entirely. Being a `defsubst', this check itself is inlined at every call
site rather than adding another call frame. This one change measurably
mattered on the 10k-node fixture used by the perf-guard test."
  (if (or (consp value) (hash-table-p value) (vectorp value))
      (supertag--persistence--canonicalize-value value)
    value))

(defun supertag--persistence--rebuild-sorted-plist (pairs)
  "Rebuild a flat, sorted, canonicalized plist from PAIRS.
PAIRS is the `(SORT-KEY KEY . RAW-VALUE)' triple list returned by
`supertag--persistence--plist-pairs-or-nil'.

Builds one small `(KEY VALUE)' list per pair and splices them together
with `nconc' (each pair still in its own freshly-consed 2-element list,
so this is safe) rather than a single shared `push'/`nreverse'
accumulator — pushing both KEY and VALUE onto one accumulator and
reversing once at the end does NOT recover the right order: reversing
the whole flat sequence also swaps each pair's internal key/value
positions, not just the pairs' relative order. (Caught by
test/canonical-serialization-test.el's sorted-invariants test.)"
  (setq pairs (sort pairs (lambda (a b) (string< (car a) (car b)))))
  (apply #'nconc
         (mapcar (lambda (p)
                   (list (cadr p) (supertag--persistence--canonicalize-maybe-atom (cddr p))))
                 pairs)))

(defun supertag--persistence--freeze-hash-table (table)
  "Return a canonical, sorted, re-readable form of hash table TABLE.
The result is `(:supertag-hash-table ((KEY . VALUE) ...))' with entries
sorted by `supertag--persistence--sort-key' on KEY and VALUE canonicalized
recursively. `supertag--persistence--thaw-value' reverses this back into an
actual (`equal'-test) hash table on load. Sorting a hash table's entries
for deterministic output is NOT the same thing as reordering an ordinary
list's elements (which `supertag--persistence--plist-p'/canonicalize-value
never do) — a hash table has no inherent element order to preserve in the
first place."
  (let (pairs)
    (maphash (lambda (k v) (push (cons (supertag--persistence--sort-key k) (cons k v)) pairs))
             table)
    (setq pairs (sort pairs (lambda (a b) (string< (car a) (car b)))))
    (list supertag--persistence--hash-marker
          (mapcar (lambda (p) (cons (cadr p) (supertag--persistence--canonicalize-maybe-atom (cddr p))))
                  pairs))))

(defun supertag--persistence--canonicalize-value (value)
  "Return VALUE with nested hash tables frozen and plist keys sorted.
Recurses into plists (sorting keys), ordinary lists (preserving element
order), improper/dotted conses, and vectors. Atoms are returned unchanged.
See the commentary above `supertag--persistence-canonical-format-header'
for why this is safe with respect to cross-entity shared structure."
  (cond
   ((hash-table-p value)
    (supertag--persistence--freeze-hash-table value))
   ((consp value)
    (let ((plist-pairs (supertag--persistence--plist-pairs-or-nil value)))
      (if plist-pairs
          (supertag--persistence--rebuild-sorted-plist plist-pairs)
        (let ((len (proper-list-p value)))
          (if len
              (mapcar #'supertag--persistence--canonicalize-maybe-atom value)
            (cons (supertag--persistence--canonicalize-maybe-atom (car value))
                  (supertag--persistence--canonicalize-maybe-atom (cdr value))))))))
   ((vectorp value)
    (apply #'vector
           (mapcar #'supertag--persistence--canonicalize-maybe-atom (append value nil))))
   (t value)))

(defun supertag--persistence--thaw-value (value)
  "Inverse of `supertag--persistence--canonicalize-value'.
Rebuilds an `equal'-test hash table from any
`(:supertag-hash-table ((KEY . VALUE) ...))' marker found anywhere in
VALUE (at any nesting depth), recursing into plists/lists/vectors exactly
like the canonicalizer. Everything else is returned unchanged."
  (cond
   ((and (consp value)
         (eq (car value) supertag--persistence--hash-marker)
         (consp (cdr value))
         (null (cddr value)))
    (let ((table (ht-create)))
      (dolist (pair (cadr value))
        (puthash (car pair) (supertag--persistence--thaw-value (cdr pair)) table))
      table))
   ((null value) nil)
   ((vectorp value)
    (apply #'vector (mapcar #'supertag--persistence--thaw-value (append value nil))))
   ((consp value)
    (let ((len (proper-list-p value)))
      (if len
          (mapcar #'supertag--persistence--thaw-value value)
        (cons (supertag--persistence--thaw-value (car value))
              (supertag--persistence--thaw-value (cdr value))))))
   (t value)))

(defun supertag--persistence--write-canonical-store (store buffer)
  "Insert the canonical, deterministic serialization of STORE into BUFFER.
STORE must be a hash table (the shape `supertag--store' always has after
`supertag--ensure-store'). See the format commentary above
`supertag--persistence-canonical-format-header'."
  (unless (hash-table-p store)
    (error "supertag--persistence--write-canonical-store: STORE must be a hash table, got: %S"
           store))
  (let (collections scalars)
    (maphash (lambda (k v)
               (when (eq k :collection)
                 (error "supertag--persistence--write-canonical-store: store has a root key literally named :collection, which collides with the canonical entity-line marker"))
               (if (hash-table-p v)
                   (push (cons k v) collections)
                 (push (cons k v) scalars)))
             store)
    (with-current-buffer buffer
      (insert supertag--persistence-canonical-format-header)
      (let ((print-escape-nonascii t)
            (print-length nil)
            (print-level nil)
            (print-circle t))
        ;; --- Root scalars: one sorted plist line ---
        (when scalars
          (setq scalars (supertag--persistence--sort-pairs-by-key scalars))
          (prin1 (apply #'append
                        (mapcar (lambda (pair)
                                  (list (car pair)
                                        (supertag--persistence--canonicalize-value (cdr pair))))
                                scalars))
                 buffer)
          (insert "\n"))
        ;; --- Collections, alphabetical order; entities sorted by id ---
        (setq collections (supertag--persistence--sort-pairs-by-key collections))
        (dolist (coll collections)
          (let ((coll-key (car coll))
                entries)
            ;; Precompute each id's sort key inline during the `maphash' walk
            ;; (rather than a separate pass afterward) — this list is
            ;; potentially the largest in the whole store (e.g. :nodes), so
            ;; folding pair-collection and sort-key computation into one
            ;; pass matters here more than anywhere else in this file.
            (maphash (lambda (id data)
                       (push (cons (supertag--persistence--sort-key id) (cons id data)) entries))
                     (cdr coll))
            (setq entries (sort entries (lambda (a b) (string< (car a) (car b)))))
            (setq entries (mapcar #'cdr entries))
            (dolist (entry entries)
              ;; The entity's own :id field is very often `eq' to the id used
              ;; as its collection key (both usually trace back to the same
              ;; string object a caller built once and reused for both the
              ;; hash key and the `:id' plist slot). Printed with a shared
              ;; `print-circle' scope across the WHOLE envelope form below,
              ;; that would emit `#1="node-a" ... :id #1#' instead of the
              ;; plain `"node-a"' the format spec shows — still perfectly
              ;; readable, but needlessly noisy and an unnecessary deviation
              ;; from the frozen example. A cheap defensive copy of the
              ;; envelope id (strings only; other id types such as keywords
              ;; or fixnums are immutable/interned and print-circle does not
              ;; number them anyway) sidesteps this without touching
              ;; `:data' itself or giving up `print-circle' as a guard
              ;; against genuine self-reference elsewhere in the entity.
              (let ((envelope-id (if (stringp (car entry))
                                     (copy-sequence (car entry))
                                   (car entry))))
                (prin1 (list :collection coll-key
                             :id envelope-id
                             :data (supertag--persistence--canonicalize-value (cdr entry)))
                       buffer))
              (insert "\n"))))))))

(defun supertag--persistence--read-canonical-forms ()
  "Read a canonical-format DB from the current buffer.
Point must be at (or before) the first non-comment sexp. Returns a fresh
hash table shaped like `supertag--coerce-store-table' would produce:
collection keywords mapped to hash tables of id -> data, plus any root
scalar keys (e.g. :version) merged in directly."
  (let ((store (ht-create))
        (read-circle t)
        (keep-reading t))
    (while keep-reading
      (let ((form (condition-case nil
                      (read (current-buffer))
                    (end-of-file (setq keep-reading nil) nil))))
        (when keep-reading
          (cond
           ((and (consp form) (eq (car form) :collection))
            (let* ((coll (plist-get form :collection))
                   (id (plist-get form :id))
                   (data (supertag--persistence--thaw-value (plist-get form :data)))
                   (bucket (or (gethash coll store)
                               (let ((ht (ht-create)))
                                 (puthash coll ht store)
                                 ht))))
              (puthash id data bucket)))
           ((supertag--persistence--plist-p form)
            (cl-loop for (k v) on form by #'cddr
                     do (puthash k (supertag--persistence--thaw-value v) store)))
           (t
            (error "supertag-db canonical format: unrecognized top-level form: %S" form))))))
    store))

(defun supertag--persistence--skip-leading-comments-and-whitespace ()
  "Move point in the current buffer past leading whitespace/`;'-comment lines."
  (goto-char (point-min))
  (while (progn
           (skip-chars-forward " \t\r\n")
           (looking-at-p ";"))
    (forward-line 1)))

(define-error 'supertag-persistence-conflict-markers-error
  "Supertag: file contains unresolved git merge conflict markers")

(defun supertag--persistence--buffer-has-conflict-markers-p ()
  "Return non-nil if the current buffer contains unresolved git conflict markers.
Looks for a line beginning with any of the three literal git conflict
marker prefixes (`<<<<<<<', `=======', `>>>>>>>') -- the shape git itself
leaves behind in a file when a merge (or the S2-format degradation path:
git's own default line-oriented text merge, run because no semantic merge
driver was configured for this clone -- see `supertag-git-check' and the
Commentary in supertag-git.el) collides on the very same line/entity.
Checked BEFORE any attempt to `read' the buffer as Lisp, since a
conflict-marked file is not valid Lisp in either on-disk format and would
otherwise merely surface as an opaque `read' error indistinguishable from
any other corruption."
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward "^<<<<<<< " nil t)
        (progn (goto-char (point-min)) (re-search-forward "^=======$" nil t))
        (progn (goto-char (point-min)) (re-search-forward "^>>>>>>> " nil t)))))

(defun supertag--persistence--try-read-store (path)
  "Return store data read from PATH.

Detects which of the two on-disk formats PATH uses by looking at the
first non-comment, non-whitespace character: `(' means the S2 canonical,
line-per-entity format (see `supertag--persistence-canonical-format-header'
commentary); anything else (in practice always `#', from the legacy
`prin1' of the store hash table directly) falls back to the original
single-sexp read. This build reads either format with zero migration
step required: old on-disk databases keep loading exactly as before, and
the very next save always writes canonical format from then on.

Before attempting either read, refuses PATH outright (signaling
`supertag-persistence-conflict-markers-error', a distinguishable
condition rather than a bare `error') if it contains unresolved git merge
conflict markers -- see
`supertag--persistence--buffer-has-conflict-markers-p'. This is
deliberately checked ahead of parsing rather than left to surface as a
generic `read' failure, so the message can name the actual fix
(`supertag-git-setup' / `git checkout --merge') instead of an opaque
\"end of file during parsing\". Callers (`supertag-load-store') treat this
exactly like any other unreadable candidate -- pushed onto that
function's per-candidate failures list, never silently swallowed into an
apparently-successful empty read -- see that function's commentary for
why this specific failure mode cannot lead to a destructive save later.

Signals an error if the file cannot be read or parsed."
  (with-temp-buffer
    (insert-file-contents path)
    (when (supertag--persistence--buffer-has-conflict-markers-p)
      (signal 'supertag-persistence-conflict-markers-error
              (list (format "%s contains unresolved git merge conflict markers (<<<<<<< / ======= / >>>>>>>) and cannot be loaded as a database. This almost always means a merge ran without the semantic merge driver configured for THIS clone -- run `M-x supertag-git-setup' to configure it (see supertag-git.el), then resolve this file with `git checkout --merge %s' (re-triggering the driver) or by hand, before reloading."
                            (abbreviate-file-name path) path))))
    (supertag--persistence--skip-leading-comments-and-whitespace)
    (if (eq (char-after) ?\()
        (supertag--persistence--read-canonical-forms)
      (progn
        (goto-char (point-min))
        (let ((read-circle t))
          (read (current-buffer)))))))

(defun supertag--persistence--canonicalize-store-root (store)
  "Normalize STORE root keys to canonical keyword collections."
  (when (hash-table-p store)
    (dolist (spec '((:nodes nodes "nodes")
                    (:tags tags "tags")
                    (:relations relations "relations")
                    (:embeds embeds "embeds")
                    (:fields fields "fields")
                    (:field-definitions field-definitions "field-definitions")
                    (:tag-field-associations tag-field-associations "tag-field-associations")
                    (:field-values field-values "field-values")
                    (:meta meta "meta")))
      (let ((canonical (car spec))
            (aliases (cdr spec)))
        (when (and (not (ht-contains? store canonical)))
          (catch 'moved
            (dolist (alias aliases)
              (when (ht-contains? store alias)
                (puthash canonical (gethash alias store) store)
                (remhash alias store)
                (throw 'moved t))))))))
  store)

(defun supertag--record-store-origin (status &optional context)
  "Record metadata about the current in-memory store origin."
  (setq supertag--store-origin
        (append
         (list :status status
               :db-file supertag-db-file
               :data-directory supertag-data-directory
               :sync-state-file (when (boundp 'supertag-sync-state-file)
                                  supertag-sync-state-file)
               :sync-state-source (when (boundp 'supertag-sync--state-source)
                                    supertag-sync--state-source)
               :sync-directories (when (boundp 'org-supertag-sync-directories)
                                   org-supertag-sync-directories)
               :active-sync-directory (when (boundp 'org-supertag-active-sync-directory)
                                        org-supertag-active-sync-directory)
               :nodes-count (supertag--count-nodes)
               :field-values-count (supertag--count-field-values)
               :captured-at (current-time))
         context)))

(defun supertag--persistence-guard-violations (&optional file)
  "Return a list of reasons to refuse saving the store."
  (let* ((origin supertag--store-origin)
         (db-file (or file supertag-db-file))
         (state-file (supertag--persistence--expected-sync-state-file))
         (state-source (when (boundp 'supertag-sync--state-source)
                         supertag-sync--state-source))
         (origin-status (plist-get origin :status))
         (origin-db (plist-get origin :db-file))
         (origin-state (plist-get origin :sync-state-file))
         (origin-field-count (plist-get origin :field-values-count))
         (current-field-count (supertag--count-field-values))
         (current-nodes (supertag--count-nodes))
         (reasons '()))
    (unless origin
      (push "store origin missing (store not loaded)" reasons))
    (let ((db-now (supertag--persistence--normalize-path db-file))
          (db-origin (supertag--persistence--normalize-path origin-db)))
      (when (and db-now db-origin (not (string= db-now db-origin)))
        (push "db-file mismatch (manual switch detected)" reasons)))
    (let ((state-now (supertag--persistence--normalize-path state-file))
          (state-origin (supertag--persistence--normalize-path origin-state)))
      (when (and state-now state-origin (not (string= state-now state-origin)))
        (push "sync-state file mismatch (manual switch detected)" reasons)))
    (let ((source-now (supertag--persistence--normalize-path state-file))
          (source-loaded (supertag--persistence--normalize-path state-source)))
      (cond
       ((and source-now (not source-loaded))
        (push "sync-state not loaded for current vault" reasons))
       ((and source-now source-loaded (not (string= source-now source-loaded)))
        (push "sync-state not loaded for current vault" reasons))))
    (when (memq origin-status '(:failed :empty-file))
      (push (format "last load status %s" origin-status) reasons))
    (when (and (numberp origin-field-count)
               (> origin-field-count 0)
               (= current-field-count 0)
               (> current-nodes 0))
      (push "field-values dropped to 0 (possible data loss)" reasons))
    (when supertag-db-lock
      (let ((live-owner (file-locked-p db-file)))
        (cond
         ((stringp live-owner)
          (setq supertag--db-lock-conflict live-owner)
          (push (format "database locked by another Emacs instance (%s)" live-owner) reasons))
         (supertag--db-lock-conflict
          ;; We previously recorded a conflict, but `file-locked-p' no longer
          ;; reports another owner for this file — the other instance likely
          ;; exited. Retry taking over the lock for this session.
          (supertag--db-acquire-lock)
          (when supertag--db-lock-conflict
            (push (format "database locked by another Emacs instance (%s)" supertag--db-lock-conflict) reasons))))))
    (nreverse reasons)))

(defun supertag--persistence-refuse-save (reasons)
  "Refuse saving and explain the correct flow."
  (let ((msg (format "Supertag refused to save: %s. Proper flow: use M-x supertag-vault-activate to switch vaults and reload state/store before saving."
                     (mapconcat #'identity reasons "; "))))
    (user-error "%s" msg)))

(defun supertag-persistence-ensure-data-directory ()
  "Ensure database and backup directories exist."
  (let ((db-dir (file-name-directory supertag-db-file)))
    ;; 1. Ensure database directory exists
    (unless (file-exists-p db-dir)
      (make-directory db-dir t))
    ;; 2. Ensure backup directory exists
    (unless (file-exists-p supertag-db-backup-directory)
      (make-directory supertag-db-backup-directory t))
    ;; 3. Verify directory creation
    (unless (and (file-exists-p db-dir)
                 (file-exists-p supertag-db-backup-directory))
      (error "Failed to create required directories: %s or %s"
             db-dir supertag-db-backup-directory))))

;;; --- Persistence Functions ---

(defun supertag--normalize-fields-collection ()
  "Normalize the :fields collection to ensure proper nested hash table structure.
The :fields collection has a three-level structure:
  :fields -> node-id -> tag-id -> field-name -> value
This function ensures all levels are proper hash tables."
  (let* ((fields-root (supertag-store-get-collection :fields))
         (normalized-count 0))
    (when (hash-table-p fields-root)
      (maphash
       (lambda (node-id node-data)
         ;; Ensure node-level is a hash table
         (unless (hash-table-p node-data)
           (let ((node-table (ht-create)))
             ;; Convert node-data to hash table if it's a list
             (when (listp node-data)
               (let ((cursor node-data))
                 (while cursor
                   (let ((tag-id (pop cursor))
                         (tag-data (pop cursor)))
                     (when tag-id
                       (puthash tag-id tag-data node-table))))))
             (puthash node-id node-table fields-root)
             (setq node-data node-table)
             (cl-incf normalized-count)))

         ;; Ensure tag-level is a hash table
         (when (hash-table-p node-data)
           (maphash
            (lambda (tag-id tag-data)
              (unless (hash-table-p tag-data)
                (let ((tag-table (ht-create)))
                  ;; Convert tag-data to hash table if it's a list
                  (when (listp tag-data)
                    (let ((cursor tag-data))
                      (while cursor
                        (let ((field-name (pop cursor))
                              (field-value (pop cursor)))
                          (when field-name
                            (puthash field-name field-value tag-table))))))
                  (puthash tag-id tag-table node-data)
                  (cl-incf normalized-count))))
            node-data)))
       fields-root)
      (when (> normalized-count 0)
        (message "Normalized %d field structures in :fields collection." normalized-count)
        (supertag-mark-dirty)))))


(defun supertag--persistence-write-store-atomically (file)
  "Write `supertag--store' to FILE atomically.

A temporary file is created in the same directory as FILE (so the
final `rename-file' is atomic on the same filesystem), the in-memory
store is serialized into it using the S2 canonical, deterministic,
line-per-entity format (see `supertag--persistence--write-canonical-store'
and the format commentary above
`supertag--persistence-canonical-format-header'), and — when
`supertag-db-verify-after-save' is non-nil — the temp file is re-read the
same way the loader does (`supertag--persistence--try-read-store', which
understands both the canonical and legacy formats) and its :nodes count
is compared against the in-memory store's :nodes count before it
replaces FILE.

On any failure, including a verification mismatch or read error, the
temp file is removed, an error is signaled, and FILE is left
untouched."
  (let ((temp-file (make-temp-file (concat file ".tmp")))
        (success nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-file-coding-system 'utf-8-unix) ; Ensure UTF-8 encoding
            (supertag--persistence--write-canonical-store supertag--store (current-buffer))
            (let ((write-region-inhibit-fsync nil))
              (write-region (point-min) (point-max) temp-file nil 'silent)))
          (when supertag-db-verify-after-save
            (let (verify-count)
              (condition-case err
                  (let* ((verify-data (supertag--persistence--try-read-store temp-file))
                         (verify-nodes (and (hash-table-p verify-data)
                                            (gethash :nodes verify-data))))
                    (setq verify-count (if (hash-table-p verify-nodes)
                                           (hash-table-count verify-nodes)
                                         0)))
                (error
                 (error "Supertag save verification failed to read %s: %s"
                        temp-file (error-message-string err))))
              (let ((live-count (supertag--count-nodes)))
                (unless (= verify-count live-count)
                  (error "Supertag save verification mismatch for %s: wrote %d nodes, expected %d"
                         file verify-count live-count)))))
          (when (file-exists-p file)
            (set-file-modes temp-file (file-modes file)))
          (rename-file temp-file file t)
          (setq success t))
      (unless success
        (ignore-errors (delete-file temp-file))))))

(defun supertag-save-store (&optional file)
  "Save the current `supertag--store` to a file.
FILE is the optional file path. Defaults to `supertag-db-file`.

This is also the function `supertag-setup-auto-save' and
`supertag-schedule-save' hand to their timers, so it doubles as the
presence heartbeat: `supertag--presence-write' below runs unconditionally
on every call — including timer ticks where the store turns out not to be
dirty and nothing else in this function does any work — so a foreign
machine's `supertag--presence-foreign-active-p' check sees this host as
recently active for as long as this session keeps running."
  (interactive)
  (supertag--presence-write)
  (let* ((file-to-save (or file supertag-db-file))
         (reasons (supertag--persistence-guard-violations file-to-save))
         (interactive-call (called-interactively-p 'any)))
    (cond
     ;; ponytail: timer-driven calls must not raise; only interactive
     ;; invocations get the loud error. Otherwise a stalled init (e.g.
     ;; failed `require`) leaves the auto-save timer barking forever.
     ((and reasons interactive-call)
      (supertag--persistence-refuse-save reasons))
     (reasons
      (message "Supertag auto-save skipped: %s"
               (mapconcat #'identity reasons "; ")))
     (t
    (supertag-persistence-ensure-data-directory) ; Ensure directory exists before saving
    (when (supertag-dirty-p) ; Only save if dirty
      ;; Safety guard: avoid overwriting a non-trivial on-disk DB with an empty in-memory store
      (let* ((nodes-table (ignore-errors (supertag-store-get-collection :nodes)))
             (live-node-count (and (hash-table-p nodes-table)
                                   (hash-table-count nodes-table)))
             (existing-file-p (file-exists-p file-to-save))
             (existing-size (when existing-file-p (file-attribute-size (file-attributes file-to-save))))
             ;; Treat DB file larger than 1KB as "non-trivial" by default
             (non-trivial-file (and existing-size (> existing-size 1024))))
        (if (and non-trivial-file
                 (numberp live-node-count)
                 (= live-node-count 0))
            (message "Protective skip: Live DB has 0 nodes while on-disk DB looks non-trivial (%s bytes). Skipping save to avoid data loss."
                     existing-size)
          (supertag--persistence-write-store-atomically file-to-save)
          (supertag-clear-dirty)
          (supertag--record-store-origin :ok)
          ;; Re-claim presence after a successful save too, not just on the
          ;; unconditional heartbeat write above — keeps the recorded
          ;; `updatedAt' as fresh as possible right when real writes happen.
          (supertag--presence-write)
          ;; Check if daily backup is needed after successful save
          (supertag-check-daily-backup))))))))

(defun supertag-db-migrate-and-normalize ()
  "Run all data migrations and normalizations on the loaded store.
This includes version migrations, field structure normalization,
and legacy field name migrations. This function should be called
manually after loading a database from an older version or when
data corruption is suspected."
  (interactive)
  (if (hash-table-p supertag--store)
      (progn
        (message "Starting database migration and normalization...")

        ;; --- Data version check and automatic migration ---
        ;; `supertag--run-migrations' returns non-nil (and stamps :version)
        ;; whenever it performs a migration, even when no :nodes/:fields
        ;; content actually changed. That must still mark the store dirty,
        ;; otherwise a pure version bump is never persisted and migration
        ;; would silently re-run on every subsequent load.
        (when (supertag--run-migrations supertag--store)
          (supertag-mark-dirty))

        ;; --- Normalize :fields collection structure ---
        (supertag--normalize-fields-collection)

        ;; --- Automatic Field Migration ---
        (let* ((nodes-table (supertag-store-get-collection :nodes))
               (migrated-count 0))
          (maphash
           (lambda (key value)
             (when (and value (eq (plist-get value :type) :node))
               (let ((needs-migration nil)
                     (migrated-value (copy-sequence value)))
                 ;; Migrate :file-path to :file
                 (when (and (plist-get value :file-path)
                            (not (plist-get value :file)))
                   (setq migrated-value (plist-put migrated-value :file (plist-get value :file-path)))
                   (setq needs-migration t))
                 ;; Migrate :pos to :position
                 (when (and (plist-get value :pos)
                            (not (plist-get value :position)))
                   (setq migrated-value (plist-put migrated-value :position (plist-get value :pos)))
                   (setq needs-migration t))
                 ;; Update if migration was needed
                 (when needs-migration
                   (supertag-store-put-entity :nodes key migrated-value t)
                   (cl-incf migrated-count)))))
           nodes-table)
          (when (> migrated-count 0)
            (message "Migrated %d nodes with legacy field names (:file-path -> :file, :pos -> :position)."
                     migrated-count)
            (supertag-mark-dirty)))

        (message "Database migration and normalization complete.")
        (when (supertag-dirty-p)
          (message "Changes were made. Saving database...")
          (supertag-save-store)))
    (message "Database not loaded. Please load the database first.")))

  ;; --- Automatic Purge of Invalid Nodes (example, kept commented) ---
  ;; (let* ((nodes-table (supertag-store-get-collection :nodes))
  ;;        (keys-to-remove '())
  ;;        (purged-count 0)
  ;;        (initial-count (hash-table-count nodes-table)))
  ;;   (maphash
  ;;    (lambda (key value)
  ;;      (unless (and value (plist-get value :type))
  ;;        (push key keys-to-remove))))
  ;;    nodes-table)
  ;;   (when keys-to-remove
  ;;     (dolist (key keys-to-remove)
  ;;       (supertag-store-remove-entity :nodes key)
  ;;       (cl-incf purged-count))
  ;;     (message "Purged %d invalid/ghost entries from database." purged-count)
  ;;     (supertag-mark-dirty)))

(defun supertag--maybe-auto-migrate ()
  "Automatically migrate the just-loaded store when its version is stale.
Called from `supertag-load-store' right after a successful load and lock
acquisition. Does nothing unless all of the following hold:
- `supertag-db-auto-migrate' is non-nil;
- `supertag--store' is a loaded hash table;
- `(supertag--get-data-version supertag--store)' differs from
  `supertag-data-version' (a missing/nil stored version already reads back
  as the old default version, so it counts as a mismatch too).

If this session recorded a multi-instance lock conflict
(`supertag--db-lock-conflict'), migration is skipped with a message instead:
a read-only session (another Emacs instance holds the write lock) must never
rewrite the database.

Before migrating, when `supertag-db-file' exists on disk, a pre-migration
snapshot is copied into `supertag-db-backup-directory' as
\"supertag-db-premigrate-<OLDVER>-<TIMESTAMP>.el\" (old version sanitized for
filename use). Note that this filename pattern intentionally does not match
the daily-backup cleanup regex used by `supertag-cleanup-old-backups'
\(\"^supertag-db-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.el$\"), so these
pre-migration snapshots are never automatically deleted. If the database
file does not exist on disk yet, the snapshot step is skipped but migration
still proceeds.

Errors signaled by `supertag-db-migrate-and-normalize' are caught here and
reported loudly rather than re-signaled: the loaded store is left in place
(so it stays usable) and the message tells the user to run
\\[supertag-db-migrate-and-normalize] by hand."
  (when (and supertag-db-auto-migrate
             (hash-table-p supertag--store))
    (let ((old-version (supertag--get-data-version supertag--store)))
      (unless (string= old-version supertag-data-version)
        (if supertag--db-lock-conflict
            (message "Supertag: skipping auto-migration of %s (locked by another Emacs instance: %s). Stored data version %s is out of date; run M-x supertag-db-migrate-and-normalize once the lock is released."
                     (abbreviate-file-name supertag-db-file)
                     supertag--db-lock-conflict
                     old-version)
          (let ((snapshot-file nil))
            (when (file-exists-p supertag-db-file)
              (condition-case err
                  (let* ((sanitized-version
                          (replace-regexp-in-string "[^A-Za-z0-9]+" "-" old-version))
                         (existing
                          (and (file-directory-p supertag-db-backup-directory)
                               (directory-files
                                supertag-db-backup-directory t
                                (format "\\`supertag-db-premigrate-%s-.*\\.el\\'"
                                        (regexp-quote sanitized-version))))))
                    (supertag-persistence-ensure-data-directory)
                    ;; A snapshot for this same source version may already
                    ;; exist when an earlier auto-migration ran but its save
                    ;; was skipped by a persistence guard (so the on-disk DB
                    ;; is still at OLD-VERSION). Reuse it instead of writing
                    ;; another copy of the same pre-migration state — with a
                    ;; per-second timestamp a same-second rerun would even
                    ;; silently overwrite the first snapshot.
                    (if existing
                        (progn
                          (setq snapshot-file (car existing))
                          (message "Supertag: reusing existing pre-migration snapshot %s"
                                   (abbreviate-file-name snapshot-file)))
                      (setq snapshot-file
                            (expand-file-name
                             (format "supertag-db-premigrate-%s-%s.el"
                                     sanitized-version
                                     (format-time-string "%Y%m%d-%H%M%S"))
                             supertag-db-backup-directory))
                      (copy-file supertag-db-file snapshot-file t)
                      (message "Supertag: pre-migration snapshot saved to %s"
                               (abbreviate-file-name snapshot-file))))
                (error
                 (setq snapshot-file nil)
                 (message "Supertag: failed to create pre-migration snapshot (%s); proceeding with auto-migration anyway."
                          (error-message-string err)))))
            (condition-case err
                (progn
                  (supertag-db-migrate-and-normalize)
                  (message "Supertag: auto-migrated database from version %s to %s.%s"
                           old-version supertag-data-version
                           (if snapshot-file
                               (format " Pre-migration snapshot: %s" (abbreviate-file-name snapshot-file))
                             ""))
                  ;; The save inside `supertag-db-migrate-and-normalize' goes
                  ;; through `supertag-save-store' and is therefore subject to
                  ;; ALL persistence guards. Early in startup the sync-state
                  ;; guard can legitimately skip it, leaving the migrated
                  ;; store in memory only. That is safe (the auto-save timer
                  ;; persists it once the guards clear), but the user should
                  ;; know the on-disk file is still the old version for now.
                  (when (supertag-dirty-p)
                    (message "Supertag: migrated store not yet written to disk (a save guard deferred it); it will be persisted by the next successful save.")))
              (error
               (message "Supertag: AUTO-MIGRATION FAILED (%s -> %s): %s. Database left loaded but NOT migrated.%s Run M-x supertag-db-migrate-and-normalize manually to retry."
                        old-version supertag-data-version
                        (error-message-string err)
                        (if snapshot-file
                            (format " A pre-migration snapshot is available at %s."
                                    (abbreviate-file-name snapshot-file))
                          " No pre-migration snapshot was created."))))))))))

(defun supertag-load-store (&optional file)
  "Load data into supertag--store from a file.
This function loads and coerces the persisted store data, but does not
run version migrations. For migrations/normalization, use the command
`supertag-db-migrate-and-normalize` after loading.
FILE is the optional file path. Defaults to supertag-db-file."
  (interactive)
  (let* ((candidates (supertag--persistence--db-file-candidates file))
         (file-to-load nil)
         (load-status nil)
         (failures '()))
    ;; Release any lock held for a previously loaded DB file (e.g. when
    ;; switching vaults) before possibly loading a different one below.
    (supertag--db-release-lock)
    ;; Ensure directory exists before loading (best-effort; does not depend on DB presence).
    (ignore-errors (supertag-persistence-ensure-data-directory))

    ;; Do not rely on a pre-check alone: try reading candidates until one succeeds.
    (dolist (candidate candidates)
      (let ((expanded (and (stringp candidate)
                           (> (length candidate) 0)
                           (ignore-errors (expand-file-name candidate)))))
        (when (and expanded
                   (file-exists-p expanded)
                   (not (file-directory-p expanded))
                   (file-readable-p expanded)
                   (null file-to-load))
          (condition-case err
              (let* ((loaded-data (supertag--persistence--try-read-store expanded))
                     (coerced (supertag--coerce-store-table loaded-data)))
                (setq file-to-load expanded)
                (setq supertag--store (supertag--persistence--canonicalize-store-root coerced))
                (supertag--ensure-store)
                (setq load-status :ok))
            (error
             (push (cons expanded (error-message-string err)) failures))))))

    (if (and file-to-load (eq load-status :ok))
        (progn
          (supertag--persistence--set-db-file file-to-load)
          (supertag-clear-dirty)
          (supertag-index-rebuild-relations)
          (supertag--record-store-origin :ok
                                         (list :loaded-from file-to-load
                                               :load-candidates candidates
                                               :load-failures (nreverse failures)))
          ;; The "run migration manually" hint is stale once auto-migration is
          ;; enabled and able to run. Only surface it when auto-migrate is off,
          ;; or when it is on but a version mismatch will be skipped anyway
          ;; (this session holds no write lock on `supertag-db-file', so
          ;; `supertag--maybe-auto-migrate' will refuse to touch the store).
          (let* ((version-mismatch
                  (not (string= (supertag--get-data-version supertag--store)
                                supertag-data-version)))
                 (migration-will-be-skipped
                  (and version-mismatch
                       supertag-db-auto-migrate
                       (stringp (file-locked-p file-to-load))))
                 (show-manual-hint
                  (or (not supertag-db-auto-migrate) migration-will-be-skipped)))
            (message "Database loaded from %s.%s"
                     (abbreviate-file-name file-to-load)
                     (if show-manual-hint
                         " For migrations, run M-x supertag-db-migrate-and-normalize."
                       "")))
          (supertag--db-acquire-lock)
          (supertag--presence-check-and-claim)
          (supertag--maybe-auto-migrate))
      (setq supertag--store (ht-create))
      (setq failures (nreverse failures))
      ;; `failures' is only ever non-nil here when at least one candidate
      ;; FILE EXISTED and failed to parse (the dolist above only attempts a
      ;; read, and thus only ever pushes onto `failures', when
      ;; `file-exists-p' held -- see the loop's `when' guard above) -- as
      ;; opposed to the genuinely-fresh-vault case where no candidate file
      ;; exists at all. That distinction matters: a parse failure (in
      ;; particular the git-conflict-markers case detected by
      ;; `supertag--persistence--try-read-store') must never be reported or
      ;; recorded the same way as an intentionally-new, empty vault, because
      ;; `supertag--persistence-guard-violations' -- consulted by every
      ;; subsequent `supertag-save-store' call, interactive or timer-driven
      ;; -- already refuses to save whenever the recorded origin `:status'
      ;; is `:failed' (that check has existed since this function's
      ;; :status-plist convention was introduced, but nothing previously
      ;; ever actually produced `:failed'). Recording `:failed' here means
      ;; that even after the user later creates brand-new nodes in this
      ;; now-"empty" in-memory store (which would otherwise defeat the
      ;; separate byte-size-based "Protective skip" guard in
      ;; `supertag-save-store', since that guard only fires when the live
      ;; node count is still exactly 0), any save is blocked at the
      ;; `supertag--persistence-guard-violations' check -- run BEFORE that
      ;; node-count guard is ever reached -- until the user goes through
      ;; the proper recovery flow this module's `supertag--persistence-refuse-save'
      ;; message already points at. The on-disk file (still containing the
      ;; real, conflict-marked or otherwise corrupt data) is therefore never
      ;; at risk of being silently overwritten by this fresh empty store.
      (setq load-status (if failures :failed :new))
      (supertag-clear-dirty)
      (supertag--record-store-origin load-status (list :loaded-from nil
                                                       :load-candidates candidates
                                                       :load-failures failures))
      (if failures
          (message "Supertag: FAILED to load the database -- %d candidate(s) existed but could not be parsed (%s). Initialized an EMPTY in-memory store as a placeholder; saving is BLOCKED (see M-x supertag-doctor / M-x supertag-git-setup) until this is resolved and the store is reloaded -- your on-disk data has NOT been modified. candidates=%S"
                   (length failures)
                   (mapconcat (lambda (f) (format "%s: %s" (abbreviate-file-name (car f)) (cdr f)))
                              failures "; ")
                   (mapcar #'abbreviate-file-name candidates))
        (message "Initialized empty Org-Supertag store (no readable DB found; candidates=%S)."
                 (mapcar #'abbreviate-file-name candidates))))
        ;; Rebuild global field caches if the feature is loaded and enabled.
        (when (fboundp 'supertag--maybe-rebuild-global-field-caches)
          (supertag--maybe-rebuild-global-field-caches))))

(defun supertag-schedule-save ()
  "Schedule a delayed save.
Waits for 2 seconds of idle time before saving to avoid frequent saves."
  (when supertag-db--auto-save-timer
    (cancel-timer supertag-db--auto-save-timer))
  (setq supertag-db--auto-save-timer
        (run-with-idle-timer 2 nil #'supertag-save-store)))

(defun supertag-setup-auto-save ()
  "Set up auto-save timer."
  (when (and supertag-db-auto-save-interval
             (null supertag-db--auto-save-timer))
    (setq supertag-db--auto-save-timer
          (run-with-timer supertag-db-auto-save-interval
                         supertag-db-auto-save-interval
                         #'supertag-save-store))))

(defun supertag-setup-daily-backup ()
  "Set up daily backup timer."
  (when (and supertag-db-backup-interval
             (null supertag-db--backup-timer))
    (setq supertag-db--backup-timer
          (run-with-timer supertag-db-backup-interval
                         supertag-db-backup-interval
                         #'supertag-backup-database-now))))

(defun supertag-cleanup-auto-save ()
  "Clean up auto-save timer."
  (when supertag-db--auto-save-timer
    (cancel-timer supertag-db--auto-save-timer)
    (setq supertag-db--auto-save-timer nil)))

(defun supertag-cleanup-daily-backup ()
  "Clean up daily backup timer."
  (when supertag-db--backup-timer
    (cancel-timer supertag-db--backup-timer)
    (setq supertag-db--backup-timer nil)))

(defun supertag-setup-all-timers ()
  "Set up both auto-save and daily backup timers."
  (supertag-setup-auto-save)
  (supertag-setup-daily-backup))

(defun supertag-cleanup-all-timers ()
  "Clean up all persistence-related timers."
  (supertag-cleanup-auto-save)
  (supertag-cleanup-daily-backup))

;;; --- Event Subscription ---

(defun supertag-persistence--handle-store-changed (path old-value new-value)
  "Handle store-changed events.
This function is called when the store is updated.
PATH, OLD-VALUE, and NEW-VALUE describe the change."
  (supertag-mark-dirty) ; Mark database as dirty
  (supertag-schedule-save)) ; Schedule a delayed save

;; Subscribe to store-changed events
(supertag-subscribe :store-changed #'supertag-persistence--handle-store-changed)

(defun supertag-db-purge-duplicate-tags ()
  "Interactively scan the :tags collection and remove duplicate tags.
Keeps the tag with the most complete data (most fields defined).
Merges relations from duplicate tags to the kept tag."
  (interactive)
  (let* ((tags-table (supertag-store-get-collection :tags))
         (name-to-tags (make-hash-table :test 'equal))
         (duplicates-found 0)
         (tags-removed 0))

    (if (not (hash-table-p tags-table)) ; defensive, though collection is always hash-table
        (message "Tags collection is missing or invalid; nothing to purge.")

      ;; Step 1: Group tags by name
      (maphash (lambda (tag-id tag-data)
                 (when (and tag-data (plist-get tag-data :name))
                   (let ((tag-name (plist-get tag-data :name)))
                     (unless (gethash tag-name name-to-tags)
                       (puthash tag-name '() name-to-tags))
                     (puthash tag-name
                             (cons (cons tag-id tag-data) (gethash tag-name name-to-tags))
                             name-to-tags))))
               tags-table)

      ;; Step 2: Find and resolve duplicates
      (maphash (lambda (tag-name tag-list)
                 (when (> (length tag-list) 1)
                   (cl-incf duplicates-found)
                   (message "Found %d duplicate tags for name '%s': %s"
                           (length tag-list) tag-name
                           (mapcar #'car tag-list))

                   ;; Choose the "best" tag (with most fields or first created)
                   (let* ((sorted-tags (sort tag-list
                                           (lambda (a b)
                                             (let ((fields-a (length (or (plist-get (cdr a) :fields) '())))
                                                   (fields-b (length (or (plist-get (cdr b) :fields) '()))))
                                               (> fields-a fields-b)))))
                          (keeper (car sorted-tags))
                          (duplicates (cdr sorted-tags))
                          (keeper-id (car keeper)))

                    (message "Keeping tag '%s', removing duplicates: %s"
                            keeper-id (mapcar #'car duplicates))

                    ;; Remove duplicate tags
                    (dolist (duplicate duplicates)
                      (let ((duplicate-id (car duplicate)))
                        (supertag-store-remove-entity :tags duplicate-id)
                        (cl-incf tags-removed))))))
               name-to-tags)

      (if (> duplicates-found 0)
          (progn
            (message "Duplicate tag cleanup complete. %d tag names had duplicates, %d duplicate tags removed."
                    duplicates-found tags-removed)
            (supertag-save-store)
            (message "Database saved."))
        (message "No duplicate tags found. Database is clean.")))))

(defun supertag-db-purge-invalid-nodes ()
  "Interactively scan the :nodes collection and remove entries with invalid data.
An entry is considered invalid if its value is nil or it's not a valid plist
with a :type property.

This version is tolerant when the :nodes collection is missing or not yet
initialized: it will treat that case as an empty collection and exit cleanly."
  (interactive)
  (let* ((nodes-table (supertag-store-get-collection :nodes))
         (keys-to-remove '())
         (total-keys 0))
    ;; If nodes-table is missing or not a hash table, log and skip the purge.
    (if (not (hash-table-p nodes-table))
        (message "Nodes collection is missing or invalid; nothing to purge.")
      ;; else proceed with scanning and purging
      (setq total-keys (hash-table-count nodes-table))
      (message "Scanning %d total entries in nodes table..." total-keys)

      ;; First, identify all keys with invalid values
      (maphash (lambda (key value)
                 (unless (and value (plist-get value :type))
                   (push key keys-to-remove)))
               nodes-table)

      ;; Then, remove them
      (if keys-to-remove
          (progn
            (message "Found %d invalid entries to purge. Purging..." (length keys-to-remove))
            (dolist (key keys-to-remove)
              ;; Use the new, explicit delete function
              (supertag-store-remove-entity :nodes key))
            (message "Purging complete. Saving database...")
            (supertag-save-store)
            (message "Database saved. %d entries remain." (hash-table-count (supertag-store-get-collection :nodes))))
        (message "No invalid entries found. Database is clean.")))))

(defun supertag-db-inspect-file ()
  "Inspect the database file and report its structure.
Useful for diagnosing why nodes aren't loading properly."
  (interactive)
  (let* ((candidates (supertag--persistence--db-file-candidates nil))
         (file-to-inspect (or (supertag--persistence--pick-readable-file candidates)
                              supertag-db-file)))
    (if (not (and file-to-inspect (file-exists-p file-to-inspect)))
        (message "Database file does not exist. Candidates: %S"
                 (mapcar #'abbreviate-file-name candidates))
      (with-temp-buffer
        (insert-file-contents file-to-inspect)
      (goto-char (point-min))
      (condition-case err
          (let* ((read-circle t)
                 (data (read (current-buffer)))
                 (is-hash (hash-table-p data))
                 (nodes-key (if is-hash (gethash :nodes data) nil))
                 (nodes-count (if (hash-table-p nodes-key)
                                  (hash-table-count nodes-key)
                                0))
                 (sample-nodes '())
                 (nodes-without-type 0)
                 (nodes-with-type 0))

            (with-output-to-temp-buffer "*Supertag DB Inspection*"
              (princ "=== Database File Inspection ===\n\n")
              (princ (format "File: %s\n" file-to-inspect))
              (princ (format "File size: %d bytes\n"
                             (file-attribute-size (file-attributes file-to-inspect))))
              (princ (format "Data is hash-table: %s\n" is-hash))
              (princ (format "Nodes collection exists: %s\n" (if nodes-key "YES" "NO")))
              (princ (format "Nodes collection is hash-table: %s\n" (hash-table-p nodes-key)))
              (princ (format "Node count in file: %d\n\n" nodes-count))

              (when (hash-table-p nodes-key)
                (princ "=== Node Analysis ===\n")
                (maphash (lambda (id node-data)
                           (if (plist-get node-data :type)
                               (cl-incf nodes-with-type)
                             (cl-incf nodes-without-type))
                           (when (< (length sample-nodes) 3)
                             (push (cons id node-data) sample-nodes)))
                         nodes-key)

                (princ (format "Nodes with :type property: %d\n" nodes-with-type))
                (princ (format "Nodes WITHOUT :type property: %d\n\n" nodes-without-type))

                (when sample-nodes
                  (princ "=== Sample Nodes ===\n")
                  (dolist (sample (reverse sample-nodes))
                    (let ((id (car sample))
                          (data (cdr sample)))
                      (princ (format "\nNode ID: %s\n" id))
                      (princ (format "Has :type: %s\n" (if (plist-get data :type) "YES" "NO")))
                      (princ (format "Has :title: %s\n" (if (plist-get data :title) "YES" "NO")))
                      (princ (format "Has :file: %s\n" (if (plist-get data :file) "YES" "NO")))
                      (princ (format "Properties: %S\n" (let ((props '()))
                                                           (cl-loop for (k v) on data by #'cddr
                                                                    do (push k props))
                                                           (nreverse props)))))))

                (when (> nodes-without-type 0)
                  (princ "\n=== WARNING ===\n")
                  (princ (format "%d nodes are missing the :type property!\n" nodes-without-type))
                  (princ "These nodes will be automatically purged during load.\n")
                  (princ "This may be why your database appears empty after loading.\n\n")
                  (princ "Possible causes:\n")
                  (princ "1. Data was created with an older version\n")
                  (princ "2. Manual editing of the database file\n")
                  (princ "3. Incomplete migration\n\n")
                  (princ "Solution: Run M-x supertag-sync-full-rescan to rebuild the database.\n")))))
        (error
         (message "Error reading database file: %s" (error-message-string err))))))))

;;; --- Time Format Standardization ---

(defun supertag-current-time ()
  "Get the standardized current time.
Returns Emacs standard time format (high low micro pico)."
  (current-time))

(defun supertag-time-equal (time1 time2)
  "Safe time comparison function.
TIME1 and TIME2 should be in Emacs time format.
Returns t if times are equal, otherwise returns nil."
  (and (supertag--validate-time time1)
       (supertag--validate-time time2)
       (equal time1 time2)))

(defun supertag--validate-time (time-value)
  "Validate that the time value is in valid Emacs time format.
TIME-VALUE should be a four-element list (high low micro pico)."
  (and (listp time-value)
       (= (length time-value) 4)
       (cl-every #'integerp time-value)))

;;; --- Data Version Management ---

(defun supertag--get-data-version (data)
  "Extract version information from the data store.
DATA should be the main data storage hash table.
Returns the version string, or a default old version if not found."
  (if (hash-table-p data)
      (or (gethash :version data) "4.0.0")  ; Default old version
    "4.0.0"))

(defun supertag--set-data-version (data version)
  "Set version information in the data store.
DATA should be the main data storage hash table.
VERSION is the version string to set."
  (when (hash-table-p data)
    (puthash :version version data)))

(defun supertag--run-migrations (data)
  "Run data migrations based on version number.
DATA is the data store to migrate.
Automatically detects version and executes necessary migration steps.
Returns t if migration was performed, nil otherwise."
  (let ((current-version (supertag--get-data-version data)))
    (unless (string= current-version supertag-data-version)
      ;; Execute version-specific migrations
      (cond
       ;; Migrate from 4.x to 5.0.0
       ((string-prefix-p "4." current-version)
        (message "Migrating data from version %s to %s..." current-version supertag-data-version)
        (supertag--migrate-4x-to-5x data)
        (message "Data migration completed to version %s" supertag-data-version))

       ;; Other version migrations can be added here
       (t
        (when (not (string= current-version supertag-data-version))
          (message "Warning: Unknown data version %s, setting to current version %s"
                   current-version supertag-data-version))))

      ;; Update version number
      (supertag--set-data-version data supertag-data-version)
      t)))

(defun supertag--migrate-4x-to-5x (data)
  "Migrate from version 4.x to 5.0.0.
Main changes include data format standardization and field name normalization."
  ;; Specific migration steps can be added here
  ;; For example, field renaming, data format conversion, etc.

  ;; Example: Ensure all time fields use standard format
  (let ((nodes-table (gethash :nodes data)))
    (when (hash-table-p nodes-table)
      (maphash (lambda (node-id node-data)
                 (when (plist-get node-data :type)
                   ;; Ensure time fields exist and are correctly formatted
                   (unless (plist-get node-data :created-at)
                     (setq node-data (plist-put node-data :created-at (supertag-current-time))))
                   (unless (plist-get node-data :modified-at)
                     (setq node-data (plist-put node-data :modified-at (supertag-current-time))))

                   ;; Update node data
                   (puthash node-id node-data nodes-table)))
               nodes-table))))

;;; --- Data Backup and Transaction Safety Mechanisms ---

(defun supertag-backup-store ()
  "Create a deep backup of the data store.
Returns a complete copy of the current supertag--store."
  (when (hash-table-p supertag--store)
    (let ((backup (make-hash-table :test (hash-table-test supertag--store)
                                   :size (hash-table-size supertag--store))))
      (maphash (lambda (key value)
                 (puthash key
                         (if (hash-table-p value)
                             ;; Deep copy nested hash tables
                             (let ((nested-copy (make-hash-table :test (hash-table-test value)
                                                               :size (hash-table-size value))))
                               (maphash (lambda (k v) (puthash k v nested-copy)) value)
                               nested-copy)
                           ;; For other types, copy directly (plists, etc.)
                           (copy-sequence value))
                         backup))
               supertag--store)
      backup)))

(defun supertag-restore-store (backup-data)
  "Restore data store from backup.
BACKUP-DATA should be a backup created by `supertag-backup-store'."
  (when (and backup-data (hash-table-p backup-data))
    (setq supertag--store backup-data)
    (supertag-clear-dirty)
    (message "Data store restored from backup")))

;; `supertag--with-transaction' used to be a *real* transaction (unlike the
;; formerly-fake `supertag-with-transaction' in supertag-core-transform.el)
;; but paid for correctness with a full-store deep copy on every use — too
;; heavy for routine mutations. Now that `supertag-with-transaction' does
;; real per-entity rollback (see `supertag--transaction-record-old-value')
;; at a fraction of the cost, this macro is a thin obsolete alias so existing
;; callers (e.g. `supertag-migrate-tag-ids') keep working unchanged.
;; `supertag-backup-store'/`supertag-restore-store' are kept as standalone
;; utilities (doctor/migration full-snapshot use), just no longer wired into
;; the routine transaction path.
(define-obsolete-function-alias 'supertag--with-transaction
  'supertag-with-transaction
  "org-supertag 5.10 (S1 transaction hardening)")

(defun supertag--validate-tag-references ()
  "Validate tag reference consistency.
Check that all tags referenced by nodes exist in the tag collection.
Returns t if all references are valid, otherwise returns nil."
  (let ((tags-table (supertag-store-get-collection :tags))
        (nodes-table (supertag-store-get-collection :nodes))
        (valid-p t)
        (error-count 0))

    (when (and (hash-table-p tags-table) (hash-table-p nodes-table))
      (maphash (lambda (node-id node-data)
                 (when (eq (plist-get node-data :type) :node)
                   (let ((node-tags (plist-get node-data :tags)))
                     (when node-tags
                       (dolist (tag-id node-tags)
                         (unless (gethash tag-id tags-table)
                           (setq valid-p nil)
                           (cl-incf error-count)
                           (message "Invalid tag reference: node %s references non-existent tag %s"
                                   node-id tag-id)))))))
               nodes-table))

    (if valid-p
        (message "Tag reference validation passed")
      (message "Tag reference validation failed with %d errors" error-count))

    valid-p))

(provide 'supertag-core-persistence)

;;; supertag-core-persistence.el ends here
