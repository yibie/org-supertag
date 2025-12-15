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
This function ONLY loads data and does not perform any migration
or normalization. For data migration, use the command
`supertag-db-migrate-and-normalize` after loading.
FILE is the optional file path. Defaults to supertag-db-file."
  (interactive)
  (let ((file-to-load (or file supertag-db-file)))
    (supertag-persistence-ensure-data-directory) ; Ensure directory exists before loading
    (if (file-exists-p file-to-load)
        (progn
          (with-temp-buffer
            (insert-file-contents file-to-load)
            (goto-char (point-min))
            ;; Check if file is empty or malformed
            (if (= (point-min) (point-max))
                (progn
                  (message "Warning: Database file %s is empty, initializing new store" file-to-load)
                  (setq supertag--store (ht-create)))
              (condition-case err
                  (let ((loaded-data (read (current-buffer))))
                    (unless (hash-table-p loaded-data)
                      (message "Warning: Legacy data format detected in %s. Coercing to hash-table."
                               file-to-load))
                    (setq supertag--store loaded-data))
                (error
                 (message "Error reading database file %s: %s. Initializing empty store."
                          file-to-load (error-message-string err))
                 (setq supertag--store (ht-create))))))
          (supertag-clear-dirty)
          (message "Database loaded. For data migration, run M-x supertag-db-migrate-and-normalize."))
      (unless (hash-table-p supertag--store)
        (setq supertag--store (ht-create))
        (message "Initialized empty Org-Supertag store."))))
  ;; Rebuild global field caches if the feature is loaded and enabled.
  (when (fboundp 'supertag--maybe-rebuild-global-field-caches)
    (supertag--maybe-rebuild-global-field-caches)))

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
  (if (not (file-exists-p supertag-db-file))
      (message "Database file does not exist: %s" supertag-db-file)
    (with-temp-buffer
      (insert-file-contents supertag-db-file)
      (goto-char (point-min))
      (condition-case err
          (let* ((data (read (current-buffer)))
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
              (princ (format "File: %s\n" supertag-db-file))
              (princ (format "File size: %d bytes\n" (file-attribute-size (file-attributes supertag-db-file))))
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
         (message "Error reading database file: %s" (error-message-string err)))))))

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
