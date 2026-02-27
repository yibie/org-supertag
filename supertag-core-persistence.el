;;; org-supertag/supertag-persistence.el --- Data persistence for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides functions for persisting the Org-Supertag
;; in-memory store to a file and loading it back.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'supertag-core-notify) ; For supertag-subscribe and supertag-emit-event
(require 'supertag-core-store) ; For supertag--store

;;; --- Persistence Configuration ---
;; Note: supertag-data-directory is defined in org-supertag.el
;; This is a fallback definition in case this module is loaded independently
(defvar supertag-data-directory
  (expand-file-name "org-supertag/" user-emacs-directory)
  "Directory for storing Org-Supertag data.
This is a fallback definition. The primary definition is in org-supertag.el.")

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
                          :test #'string=)))

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
    (if (fboundp 'supertag-config-guard--with-allow)
        (supertag-config-guard--with-allow
          (setq supertag-db-file path))
      (setq supertag-db-file path))))

(defun supertag--persistence--try-read-store (path)
  "Return store data read from PATH.

Signals an error if the file cannot be read or parsed."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((read-circle t))
      (read (current-buffer)))))

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


(defun supertag-save-store (&optional file)
  "Save the current `supertag--store` to a file.
FILE is the optional file path. Defaults to `supertag-db-file`."
  (interactive)
  (let ((file-to-save (or file supertag-db-file)))
    (let ((reasons (supertag--persistence-guard-violations file-to-save)))
      (when reasons
        (supertag--persistence-refuse-save reasons)))
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
          (with-temp-file file-to-save
            (set-buffer-file-coding-system 'utf-8-unix) ; Ensure UTF-8 encoding
            (let ((print-escape-nonascii t)  ; Correctly handle non-ASCII characters
                  (print-length nil)         ; Unlimited print length
                  (print-level nil)          ; No limit print level
                  (print-circle t))          ; Handle circular structures
              (prin1 supertag--store (current-buffer))))
          (supertag-clear-dirty)
          (supertag--record-store-origin :ok)
          ;; Check if daily backup is needed after successful save
          (supertag-check-daily-backup))))))

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
        (supertag--run-migrations supertag--store)

        ;; --- Normalize :fields collection structure ---
        (supertag--normalize-fields-collection))
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
    (supertag-save-store))

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
          (supertag--record-store-origin :ok
                                         (list :loaded-from file-to-load
                                               :load-candidates candidates
                                               :load-failures (nreverse failures)))
          (message "Database loaded from %s. For migrations, run M-x supertag-db-migrate-and-normalize."
                   (abbreviate-file-name file-to-load)))
      (setq supertag--store (ht-create))
      (setq load-status :new)
      (supertag-clear-dirty)
      (supertag--record-store-origin load-status (list :loaded-from nil
                                                       :load-candidates candidates
                                                       :load-failures (nreverse failures)))
      (message "Initialized empty Org-Supertag store (no readable DB found; candidates=%S)."
               (mapcar #'abbreviate-file-name candidates)))
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

(defconst supertag-data-version "5.0.0"
  "Current data format version.
Used for data format compatibility checks and automatic migration.")

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

(defmacro supertag--with-transaction (&rest body)
  "Transaction execution wrapper.
Creates a data backup before executing BODY, and automatically restores it if execution fails.
Ensures atomicity of data operations."
  `(let ((backup (supertag-backup-store))
         (success nil))
     (unwind-protect
         (progn
           ,@body
           (setq success t))
       (unless success
         (when backup
           (supertag-restore-store backup)
           (message "Transaction failed, data restored from backup"))))))

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
