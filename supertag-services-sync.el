;;; org-supertag/services/sync.el --- Synchronization mechanism for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements the synchronization mechanism for the Org-Supertag
;; data-centric architecture. It handles importing data from Org files into the
;; central store and exporting data from the store back to Org files.

;;; Code:


(require 'cl-lib)
(require 'subr-x)
(require 'ht)
(require 'org-element) ; For parsing Org files
(require 'org-id)     ; For generating Org IDs
(require 'supertag-core-store)
(require 'supertag-core-schema)
(require 'supertag-core-transform)
(require 'supertag-core-state) ; For supertag-with-transaction
(require 'supertag-ops-node) ; For supertag-node-create
(require 'supertag-ops-batch) ; For supertag-batch-create
(require 'supertag-services-query) ; For supertag-find-nodes-by-file
(require 'supertag-core-persistence) ; For supertag-data-directory
(require 'supertag-ops-tag) ; For supertag-tag-create
(require 'supertag-ops-relation) ; For supertag-relation-create, supertag-relation-find-between

;;; Customization (from org-supertag-old/org-supertag-sync.el)

(defgroup supertag-sync nil
  "Synchronization settings for Org-Supertag."
  :group 'supertag)

(defcustom supertag-sync-state-file
  (expand-file-name "sync-state.el" supertag-data-directory) ; Use new data directory
  "File to store sync state data."
  :type 'file
  :group 'supertag-sync)

(defcustom supertag-sync-auto-interval 900
  "Interval in seconds for automatic synchronization."
  :type 'integer
  :group 'supertag-sync)

(defcustom supertag-sync-idle-delay 1.0
  "Seconds of idle time required before automatic sync runs."
  :type 'number
  :group 'supertag-sync)

(defcustom org-supertag-sync-directories nil
  "List of directories to monitor for automatic synchronization.
Each entry should be an absolute path. Subdirectories will also be monitored.
If nil, no automatic synchronization will occur."
  :type '(repeat directory)
  :group 'supertag-sync)

(defcustom supertag-sync-exclude-directories nil
  "List of directories to exclude from synchronization.
Takes precedence over `org-supertag-sync-directories`."
  :type '(repeat directory)
  :group 'supertag-sync)

(defcustom supertag-sync-file-pattern "\.org$"
  "Regular expression for matching files to synchronize."
  :type 'string
  :group 'supertag-sync)


(defcustom supertag-sync-hash-props
  '(:raw-value :tags :todo-type :priority)
  "Properties to include when calculating node hash values."
  :type '(repeat symbol)
  :group 'supertag-sync)

(defcustom supertag-sync-auto-create-node nil
  "Whether to automatically create nodes for headings during sync.
When enabled, any heading without an ID will get one automatically.
Note: This can interfere with embed block synchronization, so it's disabled by default."
  :type 'boolean
  :group 'supertag-sync)

(defcustom supertag-sync-node-creation-level 1
  "Minimum heading level for automatic node creation.
Only headings at this level or deeper will be considered for node creation."
  :type 'integer
  :group 'supertag-sync)

;; Safety guards against accidental mass-deletion/data loss
(defcustom supertag-sync-orphan-grace-seconds 3600
  "Grace period in seconds before deleting orphaned nodes.
A node must remain orphaned (its :file property nil) for at least this long
before garbage collection can remove it."
  :type 'integer
  :group 'supertag-sync)

(defcustom supertag-sync-max-delete-ratio 0.5
  "Maximum allowed ratio of nodes to delete in a single GC pass.
If the fraction of candidate orphan deletions exceeds this ratio of total nodes,
the deletion pass is aborted to prevent accidental mass deletion."
  :type 'number
  :group 'supertag-sync)

(defcustom supertag-sync-max-delete-count 1000
  "Maximum number of nodes allowed to be deleted in a single GC pass.
If candidate deletions exceed this number, the deletion pass is aborted."
  :type 'integer
  :group 'supertag-sync)

;;; Auto-start configuration (safer defaults to reduce user setup)

(defcustom supertag-sync-auto-start t
  "Automatically start Org-Supertag auto-sync after Emacs startup.
Start is delayed and retried until sync directories are available
to avoid race conditions at early startup."
  :type 'boolean
  :group 'supertag-sync)

(defcustom supertag-sync-auto-start-initial-delay 3
  "Seconds to wait after startup before the first auto-start attempt."
  :type 'integer
  :group 'supertag-sync)

(defcustom supertag-sync-auto-start-retry-interval 5
  "Seconds between auto-start retry attempts when directories are not yet available."
  :type 'integer
  :group 'supertag-sync)

(defcustom supertag-sync-auto-start-max-retries 24
  "Maximum number of auto-start retries before giving up.
With the default interval, this caps retries to about 2 minutes."
  :type 'integer
  :group 'supertag-sync)

(defvar supertag-sync--auto-start-timer nil
  "Internal timer used for deferred auto-start of the sync worker.")

(defvar supertag-sync--auto-start-retries-left 0
  "Internal counter for remaining auto-start retries.")

;; Tag write style for rendering headlines
(defcustom supertag-tag-style 'inline
  "Style to write tags when generating or inserting Org headlines.
Supported values:
- 'inline  => Title with inline #tags.
- 'org     => Title with org native :tag: syntax.
- 'both    => Combine both inline and org native forms.
- 'auto    => Heuristic; currently defaults to 'inline."
  :type '(choice (const :tag "Inline #tags" inline)
                 (const :tag "Org :tag:" org)
                 (const :tag "Both" both)
                 (const :tag "Auto" auto))
  :group 'supertag-sync)

;; Legacy tag handling policy
(defcustom supertag-sync-legacy-tags-policy 'read-only
  "How to handle legacy org native :tag: found in headlines.
Supported values:
- 'read-only   => Read and import to DB, do not modify files (default).
- 'lazy-convert => When touching a headline, convert :tag: to inline #tags.
- 'preserve    => Always preserve :tag: in files.
- 'ignore      => Do not read or import :tag: into the database."
  :type '(choice (const :tag "Read only" read-only)
                 (const :tag "Lazy convert on edit" lazy-convert)
                 (const :tag "Preserve in files" preserve)
                 (const :tag "Ignore" ignore))
  :group 'supertag-sync)

;;; Variables

(defvar supertag-sync--state (make-hash-table :test 'equal)
  "Track file modification states.
Key: file path
Value: last sync time")

(defvar supertag-sync--internal-modifications (make-hash-table :test 'equal)
  "Track files modified internally by Supertag code.
Key: file path (absolute)
Value: timestamp of last internal modification.
This is used to distinguish internal modifications (by automation/UI) from
external modifications (by user/other tools), preventing unnecessary re-sync.")

(defvar supertag-sync--is-full-rescan-p nil
  "Dynamically bound to t during a full rescan.
This allows special behavior, like one-time import of legacy tags.")

;;; Helper functions for accessing sync state data

(defun supertag--mark-internal-modification (file)
  "Mark FILE as internally modified by Supertag.
FILE should be an absolute path. This function records the current time
to prevent sync from re-parsing the file we just modified."
  (when file
    (let ((abs-file (expand-file-name file)))
      (puthash abs-file (current-time) supertag-sync--internal-modifications))))

(defun supertag--is-internal-modification-p (file)
  "Check if FILE was recently modified internally by Supertag.
Returns t if the file's modification time is within 1 second of the last
internal modification timestamp, indicating this save is from Supertag code."
  (when file
    (let* ((abs-file (expand-file-name file))
           (last-internal (gethash abs-file supertag-sync--internal-modifications))
           (file-mtime (when (file-exists-p abs-file)
                        (file-attribute-modification-time (file-attributes abs-file)))))
      (and last-internal
           file-mtime
           ;; If file mtime is within 2 seconds after internal modification, skip sync
           (time-less-p file-mtime (time-add last-internal 2))))))

(defun supertag-sync--get-state-table ()
  "Get the actual state hash table from supertag-sync--state.
Handles both old format (direct hash table) and new format (plist with :sync-state key)."
  (cond
   ((hash-table-p supertag-sync--state)
    ;; Old format: direct hash table
    supertag-sync--state)
   ((and (listp supertag-sync--state) (plist-get supertag-sync--state :sync-state))
    ;; New format: plist with :sync-state key
    (plist-get supertag-sync--state :sync-state))
   (t
    ;; Fallback: create empty hash table
    (let ((new-table (make-hash-table :test 'equal)))
      (setq supertag-sync--state new-table)
      new-table))))

(defun supertag-sync--ensure-state-format ()
  "Ensure supertag-sync--state is in the correct format for current code.
If it's a plist, extract the hash table. If it's already a hash table, keep it."
  (when (and (listp supertag-sync--state)
             (plist-get supertag-sync--state :sync-state))
    (setq supertag-sync--state (plist-get supertag-sync--state :sync-state))))


;;; --- Sync Mechanism ---

;; Core Functions - File State Tracking

(defun supertag-sync--in-sync-scope-p (file)
  "Check if FILE is within synchronization scope.
Returns t if file should be synchronized based on configured directories.
If no directories are configured, returns t for all org files."
  (when (and file (file-exists-p file))
    (let* ((expanded-file (expand-file-name file))
           (file-dir (file-name-directory expanded-file))
           (excluded (and supertag-sync-exclude-directories
                         (cl-some (lambda (dir)
                                   (let ((expanded-exclude-dir (expand-file-name dir)))
                                     (string-prefix-p expanded-exclude-dir file-dir)))
                                 supertag-sync-exclude-directories)))
           (included (if org-supertag-sync-directories
                        (cl-some (lambda (dir)
                                  (let ((expanded-dir (expand-file-name dir)))
                                    (string-prefix-p expanded-dir file-dir)))
                                org-supertag-sync-directories)
                      t)))
      (let ((result (and included
                         (not excluded)
                         (string-match-p supertag-sync-file-pattern file))))
        ;; Enhanced debug logging for troubleshooting
        (when (not result)
          (message "DEBUG-SCOPE: File %s REJECTED - exists: %s, included: %s, excluded: %s, pattern-match: %s"
                   file
                   (file-exists-p file)
                   included
                   excluded
                   (string-match-p supertag-sync-file-pattern file))
          (message "DEBUG-SCOPE: expanded-file: %s" expanded-file)
          (message "DEBUG-SCOPE: file-dir: %s" file-dir)
          (message "DEBUG-SCOPE: sync-directories: %S" org-supertag-sync-directories)
          (when org-supertag-sync-directories
            (dolist (dir org-supertag-sync-directories)
              (let ((expanded-dir (expand-file-name dir)))
                (message "DEBUG-SCOPE: Checking dir %s (expanded: %s) - prefix match: %s"
                         dir
                         expanded-dir
                         (string-prefix-p expanded-dir file-dir))))))
        result))))

(defun supertag-scan-sync-directories (&optional all-files-p)
  "Scan sync directories for org files.
If ALL-FILES-P is non-nil, return all files in scope.
Otherwise, returns a list of new files that are not yet in sync state."
  (let ((files nil)
        (state-table (supertag-sync--get-state-table)))
    (if (not org-supertag-sync-directories)
        (message "WARNING: org-supertag-sync-directories is not configured. No files will be synced.")
      (dolist (dir org-supertag-sync-directories)
        (when (file-exists-p dir)
          (let ((dir-files (directory-files-recursively
                           dir supertag-sync-file-pattern t)))
            (dolist (file dir-files)
              (when (and (file-regular-p file)
                         (supertag-sync--in-sync-scope-p file)
                         (or all-files-p
                             (not (gethash file state-table))))
                (push file files)))))))
    files))

(defun supertag-sync-update-state (file)
  "Update sync state for FILE."
  (when (file-exists-p file)
    (let ((state-table (supertag-sync--get-state-table)))
      (puthash file
               (file-attribute-modification-time
                (file-attributes file))
               state-table))))

(defun supertag-sync-check-state (file)
  "Check if FILE needs synchronization.
Returns t if file has been modified since last sync."
  (let ((state-table (supertag-sync--get-state-table)))
    (when-let* ((state (gethash file state-table))
                (last-sync state)
                (mtime (file-attribute-modification-time
                       (file-attributes file))))
      (time-less-p last-sync mtime))))

(defun supertag-get-modified-files ()
  "Get list of files that need synchronization.
Returns files that have been modified since last sync."
  (let ((files nil)
        (state-table (supertag-sync--get-state-table)))
    (maphash
     (lambda (file state)
       (when (and (file-exists-p file)
                  (supertag-sync--in-sync-scope-p file)
                  (supertag-sync-check-state file))
         (push file files)))
     state-table)
    files))

;; --- State Management ---

(defun supertag-sync-import-file (file)
  "Import data from FILE into the store.
Reads the file, parses Org nodes, and creates/updates them using hybrid architecture.
Returns a list of imported/updated node data."
  (let ((nodes (supertag--parse-org-nodes file))
        (imported-nodes '()))
    ;; Process each node with hybrid architecture (strict validation + direct storage)
    (dolist (node-props nodes)
      (let ((imported-node (supertag-node-create node-props)))
        (push imported-node imported-nodes)))
    (nreverse imported-nodes)))

(defun supertag-sync-export-file (file)
  "Export data from the store to FILE.
Finds all nodes associated with FILE, generates Org content,
and writes it to the file.
Returns a list of exported node data."
  (let* ((nodes (supertag-find-nodes-by-file file))
         (node-data (mapcar #'cdr nodes)) ; Extract only the node data from (id . data) pairs
         (org-content (supertag--generate-org-content node-data)))
    (with-temp-file file (insert org-content))
    node-data))

  (defun supertag--generate-org-content (nodes)
  "Helper function to generate Org content from node plists.
NODES is a list of node plists.
Returns a string containing the Org content.
Respects `supertag-tag-style` configuration for tag formatting."
    (with-temp-buffer
      (dolist (node nodes)
        (let* ((title (plist-get node :title))
               (tags (plist-get node :tags))
               (level (or (plist-get node :level) 1))
               (content (or (plist-get node :content) ""))
               (id (plist-get node :id))
               (file (plist-get node :file))
               ;; Use the configured tag style
               (tag-style (supertag--resolve-tag-style node file))
               (tags-part (supertag--format-tags-by-style tags tag-style)))
          ;; Reconstruct the node with configured tag formatting
          (insert (format "%s %s%s\n"
                          (make-string level ?*)
                          title
                          tags-part))
          (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n" id))
          ;; Insert content
          (when content
            (insert content))
          (unless (or (string-empty-p content) (string-suffix-p "\n" content))
            (insert "\n"))))
      (buffer-string)))

(defun supertag-sync-save-state ()
  "Save sync state to file."
  (supertag-sync--ensure-state-format)
  (with-temp-file supertag-sync-state-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 supertag-sync--state (current-buffer)))))

(defun supertag-sync-load-state ()
  "Load sync state from file.
If file doesn't exist, initialize empty state.
Returns the loaded or initialized sync state."
  (condition-case err
      (let ((result
             (if (file-exists-p supertag-sync-state-file)
                 (with-temp-buffer
                   (insert-file-contents supertag-sync-state-file)
                   (goto-char (point-min))
                   ;; Check if file is empty
                   (if (= (point-min) (point-max))
                       (progn
                         (message "Warning: Sync state file is empty, initializing new state")
                         (setq supertag-sync--state (make-hash-table :test 'equal)))
                     (condition-case read-err
                         (progn
                           (setq supertag-sync--state (read (current-buffer)))
                           (message "Loaded sync state with %d entries"
                                    (if (hash-table-p supertag-sync--state)
                                        (hash-table-count supertag-sync--state)
                                      0))
                           ;; Ensure the loaded state is in the correct format
                           (supertag-sync--ensure-state-format)
                           supertag-sync--state)
                       (error
                        (message "Error reading sync state: %s" (error-message-string read-err))
                        (message "Initializing new sync state")
                        (setq supertag-sync--state (make-hash-table :test 'equal))
                        supertag-sync--state))))
               ;; Initialize empty state if file doesn't exist
               (progn
                 (message "Sync state file does not exist, initializing empty state")
                 (setq supertag-sync--state (make-hash-table :test 'equal))
                 ;; Save the initial state
                 (supertag-sync-save-state)
                 supertag-sync--state))))
        result)
    (error
     (message "Critical error loading sync state: %s" (error-message-string err))
     (message "Initializing fresh sync state")
     (setq supertag-sync--state (make-hash-table :test 'equal))
     supertag-sync--state)))

;; --- Check and sync ---

(defun supertag-sync-ensure-directories ()
  "Ensure sync directories are properly configured."
  (unless org-supertag-sync-directories
    (message "Warning: `org-supertag-sync-directories` is not set. Auto-sync will not occur.")))
    
(defvar supertag-sync--timer nil
  "Timer for periodic sync checks.")

(defvar supertag-sync--idle-dispatch nil
  "Idle timer used to defer sync execution until Emacs is idle.")

(defun supertag-sync--cancel-idle-dispatch ()
  "Cancel any pending idle dispatch for the sync worker."
  (when (timerp supertag-sync--idle-dispatch)
    (cancel-timer supertag-sync--idle-dispatch))
  (setq supertag-sync--idle-dispatch nil))

(defun supertag-sync--queue-idle-dispatch ()
  "Schedule sync execution for the next idle period."
  (unless (timerp supertag-sync--idle-dispatch)
    (setq supertag-sync--idle-dispatch
          (run-with-idle-timer
           (max supertag-sync-idle-delay 0)
           nil
           #'supertag-sync--run-idle-dispatch))))

(defun supertag-sync--run-idle-dispatch ()
  "Run the sync worker after idle delay."
  (setq supertag-sync--idle-dispatch nil)
  (when (fboundp 'supertag-sync--check-and-sync)
    (supertag-sync--check-and-sync)))

;;; Auto-start manager -------------------------------------------------

(defun supertag-sync--dirs-ready-p ()
  "Return non-nil when all configured sync directories exist and are accessible."
  (and org-supertag-sync-directories
       (cl-every #'file-directory-p org-supertag-sync-directories)))

(defun supertag-sync--cancel-auto-start ()
  "Cancel any pending auto-start timer."
  (when (timerp supertag-sync--auto-start-timer)
    (cancel-timer supertag-sync--auto-start-timer))
  (setq supertag-sync--auto-start-timer nil)
  (setq supertag-sync--auto-start-retries-left 0))

(defun supertag-sync--auto-start-tick ()
  "Auto-start attempt: start sync when directories are ready, otherwise retry."
  (cond
   ((not supertag-sync-auto-start)
    (supertag-sync--cancel-auto-start))
   ((supertag-sync--dirs-ready-p)
    (supertag-sync--cancel-auto-start)
    (message "Supertag: directories ready; starting auto-sync")
    (supertag-sync-start-auto-sync))
   ((<= supertag-sync--auto-start-retries-left 0)
    (supertag-sync--cancel-auto-start)
    (message "Supertag: auto-sync not started; directories unavailable"))
   (t
    (setq supertag-sync--auto-start-retries-left (1- supertag-sync--auto-start-retries-left)))))

(defun supertag-sync-schedule-auto-start ()
  "Schedule deferred auto-start of auto-sync with retries until directories are ready."
  (when supertag-sync-auto-start
    (supertag-sync--cancel-auto-start)
    (setq supertag-sync--auto-start-retries-left supertag-sync-auto-start-max-retries)
    (setq supertag-sync--auto-start-timer
          (run-with-timer
           (max 0 supertag-sync-auto-start-initial-delay)
           (max 1 supertag-sync-auto-start-retry-interval)
           #'supertag-sync--auto-start-tick))))


;; (defun supertag-sync-emergency-recovery ()
;;   "Emergency recovery function to clean up all sync-related timers and state.
;; Use this when experiencing persistent timer-related errors."
;;   (interactive)
;;   (message "Starting emergency recovery for sync system...")
  
;;   ;; Cancel our timer
;;   (when (timerp supertag-sync--timer)
;;     (cancel-timer supertag-sync--timer)
;;     (setq supertag-sync--timer nil)
;;     (message "Canceled supertag-sync--timer"))
  
;;   ;; Clean up any other timers that might be calling our function
;;   (let ((all-timers (timer-list))
;;         (cleaned-count 0))
;;     (dolist (timer all-timers)
;;       (when (and (timerp timer)
;;                  (or (equal (timer--function timer) 'supertag-sync--check-and-sync)
;;                      (and (listp (timer--function timer))
;;                           (equal (car (timer--function timer)) 'lambda))))
;;         (cancel-timer timer)
;;         (cl-incf cleaned-count)))
;;     (when (> cleaned-count 0)
;;       ;;(message "Cleaned up %d additional timers" cleaned-count))
;;     )
  
;;   ;; Reset sync state
;;   (setq supertag-sync--state (make-hash-table :test 'equal))
;;   (message "Reset sync state")
  
;;   ;; Verify function is defined
;;   (if (fboundp 'supertag-sync--check-and-sync)
;;       (message "Function supertag-sync--check-and-sync is properly defined")
;;     (message "WARNING: Function supertag-sync--check-and-sync is NOT defined")) 
;;   (message "Emergency recovery completed. You can now try M-x supertag-sync-start-auto-sync"))

;;; Core Functions - Node Hash Support (from org-supertag-old/org-supertag-sync.el)

(defun supertag-node-hash (node)
  "Calculate hash value for NODE.
Includes the node's ID to ensure absolute uniqueness of the state fingerprint."
  (let* ((id (or (plist-get node :id) "")) ; Ensure ID is part of the hash
         (raw-value (or (plist-get node :raw-value) ""))
         (content (or (plist-get node :content) "")) ; Include content in hash
         (tags (let ((tag-list (plist-get node :tags)))
                 (if (listp tag-list)
                     (mapconcat #'identity (sort (copy-sequence tag-list) #'string<) "|")
                   (or tag-list ""))))
         (todo-type (or (plist-get node :todo) "")) ; Use :todo for schema consistency
         (priority (or (plist-get node :priority) ""))
         (properties (let ((props-plist (plist-get node :properties)))
                       (if (plistp props-plist)
                           (let (props-alist)
                             ;; Convert plist to alist for safe sorting.
                             (cl-loop for (k v) on props-plist by #'cddr
                                      when k ; Handle odd-length or malformed plists
                                      do (push (cons k v) props-alist))

                             ;; Sort the alist by key (keys are keywords).
                             (setq props-alist (sort props-alist (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))

                             ;; Create the flattened string representation.
                             (mapconcat (lambda (pair)
                                          ;; Format value as empty string if it's nil.
                                          (format "%s=%s" (car pair) (or (cdr pair) "")))
                                        props-alist
                                        "|"))
                         ""))))
    (secure-hash 'sha1 (format "%s|%s|%s|%s|%s|%s|%s" id raw-value content tags todo-type priority properties))))

(defun supertag-node-mark-deleted-from-file (id)
  "Mark a node as deleted from its file by setting its :file property to nil.
This does not remove the node from the store immediately."
  ;;(message "DEBUG: supertag-node-mark-deleted-from-file called for ID: %S" id)
  (supertag-node-update
   id
   (lambda (node)
     (when node
       (let ((modified (copy-sequence node)))
         (plist-put modified :file nil)
         ;; Record when the node first became orphaned to allow a grace period
         (plist-put modified :orphaned-at (supertag-current-time)))))))

(defun supertag-db-add-with-hash (id props &optional counters)
  "Add node with ID and PROPS to database, including hash value.
This function also handles tag creation and relations.
COUNTERS is an optional plist for tracking statistics."
  (let ((node-hash (supertag-node-hash props)))
    ;; Ensure :id, :type and :hash are added to props while preserving existing fields
    (let ((node-props (plist-put props :id id)))
      (setq node-props (plist-put node-props :type :node))
      (setq node-props (plist-put node-props :hash node-hash))
      ;; Process tags only when actually saving the node
      (supertag--process-node-tags node-props)
      ;; Process reference relations when actually saving the node
      (when counters
        (let ((current-refs (plist-get node-props :ref-to)))
          ;; Clean up orphaned references first
          (supertag--cleanup-orphaned-references id current-refs counters)
          ;; Then process current references
          (supertag--process-node-references node-props counters)))
      ;; If this node comes from a file (i.e., has :file), clear any orphan marker
      (when (plist-get node-props :file)
        (setq node-props (plist-put node-props :orphaned-at nil)))
      (supertag-node-create node-props))))

(defun supertag-node-changed-p (old-node new-node)
  "Compare OLD-NODE and NEW-NODE to detect changes.
If OLD-NODE doesn't have a hash value, calculate it on the fly."
  (let ((old-hash (or (plist-get old-node :hash)
                      (supertag-node-hash old-node)))
        (new-hash (or (plist-get new-node :hash)
                      (supertag-node-hash new-node))))
    (not (string= old-hash new-hash))))

(defun supertag--merge-node-properties (new-props old-props)
  "Merge NEW-PROPS from file with OLD-PROPS from database.
NEW-PROPS is the source of truth for file-based properties.
OLD-PROPS is the source of truth for database-only fields."
  (let ((merged-props (copy-sequence new-props))
        (standard-keys '(:id :title :raw-value :tags :properties :ref-to :file :content :level :todo :priority :scheduled :deadline :position :pos :hash :type)))
    (cl-loop for (key value) on old-props by #'cddr
             do (unless (member key standard-keys)
                  (plist-put merged-props key value)))
    merged-props))

(defun supertag-sync--process-single-file (file counters)
     "Process a single FILE for synchronization.
   COUNTERS is a plist for tracking :nodes-created, :nodes-updated, and :nodes-deleted."
     ;; (message "Syncing file: %s" file)
     (let* ((current-nodes-in-file (make-hash-table :test 'equal))
            (nodes-from-file (supertag--parse-org-nodes file))
            (existing-nodes-in-store (supertag-find-nodes-by-file file)))

       ;; (message "DEBUG-PROCESS: Parser found %d nodes in file." (length nodes-from-file))
       ;; (message "DEBUG-PROCESS: DB query found %d existing nodes for this file." (length existing-nodes-in-store))

       ;; Populate current-nodes-in-file hash table for quick lookup
       (dolist (node-props nodes-from-file)
         (puthash (plist-get node-props :id) node-props current-nodes-in-file))

       ;; Process existing nodes in store for this file
       (dolist (existing-node-pair existing-nodes-in-store)
        (let* ((id (car existing-node-pair))
               (old-node-props (cdr existing-node-pair))
               (new-node-props (gethash id current-nodes-in-file)))
           (cond
            ((null new-node-props)
             (supertag-node-mark-deleted-from-file id)
             (setf (plist-get counters :nodes-deleted) (1+ (or (plist-get counters :nodes-deleted) 0))))
            ((supertag-node-changed-p old-node-props new-node-props)
             ;; (message "DEBUG-PROCESS: Node %s CHANGED. Old hash: %s, New hash: %s"
             ;;          id
             ;;          (or (plist-get old-node-props :hash) (supertag-node-hash old-node-props))
             ;;          (supertag-node-hash new-node-props))
             (let ((merged-props (supertag--merge-node-properties new-node-props old-node-props)))
               (supertag-db-add-with-hash id merged-props counters))
             (setf (plist-get counters :nodes-updated) (1+ (or (plist-get counters :nodes-updated) 0))))
            (t
             ;; (message "DEBUG-PROCESS: Node %s NOT changed. Old hash: %s, New hash: %s"
             ;;          id
             ;;          (or (plist-get old-node-props :hash) (supertag-node-hash old-node-props))
             ;;          (supertag-node-hash new-node-props))
             ))
           ;; This is now inside the let* block, fixing the scope bug.
           (remhash id current-nodes-in-file)))

       ;; (message "DEBUG-PROCESS: Found %d new nodes to create." (hash-table-count current-nodes-in-file))

       ;; Process new nodes from file
       (maphash (lambda (id new-node-props)
                  ;; (message "DEBUG-PROCESS: Creating new node with ID: %s" id)
                  (supertag-db-add-with-hash id new-node-props counters)
                  (setf (plist-get counters :nodes-created) (1+ (or (plist-get counters :nodes-created) 0))))
                current-nodes-in-file)

       ;; Update sync state for the file
       (supertag-sync-update-state file)
       ;; (message "DEBUG-PROCESS: Finished processing file: %s" file)
       ))


(defun supertag-sync--verify-file-nodes (file counters)
  "Verify that nodes in the database still exist in the file.
This function checks if nodes associated with FILE still exist in the file.
If a node exists in the database but not in the file, it's marked as orphaned.
FILE is the file path to verify.
COUNTERS is a plist for tracking :nodes-created, :nodes-updated, and :nodes-deleted."
  (let* ((file-exists (file-exists-p file))
         (current-nodes-in-file (make-hash-table :test 'equal))
         (nodes-from-file (when file-exists
                            (supertag--parse-org-nodes file)))
         (existing-nodes-in-store (supertag-find-nodes-by-file file)))

    (if file-exists
        (progn
          ;; Populate current-nodes-in-file hash table for quick lookup
          (dolist (node-props nodes-from-file)
            (puthash (plist-get node-props :id) node-props current-nodes-in-file))

          ;; Process existing nodes in store for this file
          (dolist (existing-node-pair existing-nodes-in-store)
            (let* ((id (car existing-node-pair))
                   (old-node-props (cdr existing-node-pair))
                   (new-node-props (gethash id current-nodes-in-file)))
              ;; If node exists in store but not in file, mark it as orphaned
              (when (null new-node-props)
                (let ((db-node (supertag-node-get id)))
                  (when (and db-node
                             (let ((db-node-file (plist-get db-node :file)))
                               (and db-node-file
                                    (string= db-node-file file))))
                    (supertag-node-mark-deleted-from-file id)
                    (setf (plist-get counters :nodes-deleted)
                          (1+ (plist-get counters :nodes-deleted)))))))))
      ;; File doesn't exist: mark all its nodes as orphaned
      (dolist (existing-node-pair existing-nodes-in-store)
        (let* ((id (car existing-node-pair))
               (db-node (supertag-node-get id)))
          (when (and db-node
                     (let ((db-node-file (plist-get db-node :file)))
                       (and db-node-file
                            (string= db-node-file file))))
            (supertag-node-mark-deleted-from-file id)
            (setf (plist-get counters :nodes-deleted)
                  (1+ (plist-get counters :nodes-deleted)))))))))


(defun supertag-sync--check-and-sync ()
  "Check and synchronize modified files.
  This is the main sync function called periodically."
  ;;(message "DEBUG: supertag-sync--check-and-sync function called at %s" (current-time))
  
  ;; Pre-check: Warn if sync directories are not configured
  (unless org-supertag-sync-directories
    (message "WARNING: org-supertag-sync-directories is not configured. Sync will not run.")
    (cl-return-from supertag-sync--check-and-sync nil))
  
  ;; CRITICAL FIX: Check if database is empty but sync-state has files
  ;; This indicates data loss - warn user and suggest recovery
  (let* ((nodes-table (supertag-store-get-collection :nodes))
         (db-empty (= (hash-table-count nodes-table) 0))
         (state-table (supertag-sync--get-state-table))
         (state-count (hash-table-count state-table)))
    (when (and db-empty (> state-count 0))
      (message "WARNING: Database is empty but %d files are tracked in sync-state." state-count)
      (message "This suggests data was lost. Please run: M-x supertag-sync-full-rescan")))
  
  (supertag-with-transaction
    (let ((files-to-remove nil)
          (state-changed nil)
          (modified-files (supertag-get-modified-files))
          ;; Use a plist to hold counters, making it easy to pass around.
          (counters '(:nodes-created 0 :nodes-updated 0 :nodes-deleted 0 :references-created 0 :references-deleted 0))
          ;; Track all files that have been processed to identify orphaned files
          (processed-files (make-hash-table :test 'equal)))

      ;; (message "DEBUG: Modified files found: %S" modified-files)
      ;; (message "DEBUG: Sync state hash table size: %d" (hash-table-count supertag-sync--state))
      ;; 1. Cleanup Sync State - Remove files that are no longer in scope
      ;; CRITICAL FIX: Only clean up files if sync directories are configured
      ;; If not configured, skip cleanup to prevent accidental data loss
      (when org-supertag-sync-directories
        (let ((state-table (supertag-sync--get-state-table)))
          (maphash (lambda (file _state)
                     (let ((file-exists (file-exists-p file))
                           (in-scope (supertag-sync--in-sync-scope-p file)))
                       (when (or (not file-exists)
                                 (not in-scope))
                         (push file files-to-remove))))
                   state-table)))

      (when files-to-remove
        (setq state-changed t)
        (let ((state-table (supertag-sync--get-state-table)))
          (dolist (file files-to-remove)
            (let ((file-exists-before (file-exists-p file)))
              (remhash file state-table)
              ;; If file is out of scope but still exists, we just untrack it.
              ;; We DO NOT delete its nodes. They become "unmanaged".
              ;; If the file truly doesn't exist, then we can clean up its nodes.
              (unless (file-exists-p file)
                (supertag-sync--verify-file-nodes file counters))))))

      ;; 2. Scan for New Files
      (let ((new-files (supertag-scan-sync-directories)))
        (when new-files
          (setq modified-files
                (cl-union modified-files new-files :test #'string=))))

      ;; 3. Process Modified Files
      (when modified-files
        (let ((sorted-files (sort modified-files ; Remove debug message
                                  (lambda (a b)
                                    (time-less-p
                                     (file-attribute-modification-time (file-attributes a))
                                     (file-attribute-modification-time (file-attributes b)))))))
          (dolist (file sorted-files)
            (supertag-sync--process-single-file file counters)
            (setq state-changed t)
            ;; Mark this file as processed
            (puthash file t processed-files))))

      ;; 4. Process All Files in Sync Scope to Identify Orphaned Files
      ;; FIX: Get ALL files from directories, not just from sync state
      (let ((all-files-in-scope (supertag-scan-sync-directories t)))
        ;; Process files that haven't been processed yet (i.e., not modified)
        (dolist (file all-files-in-scope)
          (unless (gethash file processed-files)
            ;;(message "DEBUG: Processing unmodified file for orphan check: %s" file)
            (supertag-sync--verify-file-nodes file counters)
            ;; Also ensure the file is in sync state
            (let ((state-table (supertag-sync--get-state-table)))
              (unless (gethash file state-table)
                (supertag-sync-update-state file)
                (setq state-changed t))))))

      (when state-changed
        (supertag-sync-save-state))

      ;; 5. Perform deep validation to catch any remaining zombie nodes
      ;; This is the robust check inspired by the old system.
      (supertag-sync-validate-nodes counters)

      ;; Report results with enhanced diagnostics
      (let ((refs-created (or (plist-get counters :references-created) 0))
            (refs-deleted (or (plist-get counters :references-deleted) 0))
            (total-changes (+ (plist-get counters :nodes-created)
                             (plist-get counters :nodes-updated)
                             (plist-get counters :nodes-deleted))))
        (message "File Sync Completed: %d nodes created, %d updated, %d deleted, %d refs created, %d refs deleted."
                 (plist-get counters :nodes-created)
                 (plist-get counters :nodes-updated)
                 (plist-get counters :nodes-deleted)
                 refs-created
                 refs-deleted)
        
        ;; Provide diagnostic hints if no changes were detected
        (when (and (= total-changes 0)
                   (not modified-files))
          (supertag--diagnose-empty-sync))))

    ;; Run garbage collection outside of transaction to ensure immediate persistence
    (supertag-sync-garbage-collect-orphaned-nodes)))

;;; --- Enhanced Hash Table Traversal Utilities ---

(defun supertag-traverse-collection (collection-path callback)
  "Traverse a collection in the nested hash table at COLLECTION-PATH.
CALLBACK is a function that receives (id value) pairs.
Returns a list of results from CALLBACK."
  (let* ((path (if (listp collection-path) collection_path (list collection_path)))
         (key (car path))
         (collection (and key (supertag-store-get-collection key)))
        (results '()))
    (when (hash-table-p collection)
      (maphash (lambda (id value)
                 (push (funcall callback id value) results))
               collection))
    (nreverse results)))

(defun supertag-traverse-nodes (callback)
  "Traverse all nodes in the store.
CALLBACK is a function that receives (id node-data) pairs.
Returns a list of results from CALLBACK."
  (let ((nodes-collection (supertag-store-get-collection :nodes))
        (results '())
        (total-nodes 0)
        (valid-nodes 0))
    (when (hash-table-p nodes-collection)
      (maphash (lambda (id node-data)
                 (cl-incf total-nodes)
                 (when node-data)
                 (when (and node-data (plist-get node-data :type))
                   (cl-incf valid-nodes)
                   (push (funcall callback id node-data) results)))
               nodes-collection))
    (nreverse results)))

(defun supertag-find-nodes-by-condition (condition-fn)
  "Find all nodes that satisfy CONDITION-FN.
CONDITION-FN is a function that receives (id node-data) and returns t if the node should be included.
Returns a list of (id . node-data) pairs."
  (supertag-traverse-nodes
   (lambda (id node-data)
     (when (funcall condition-fn id node-data)
       (cons id node-data)))))

(defun supertag-sync-garbage-collect-orphaned-nodes ()
  "Scan the store for nodes marked as orphaned (:file nil) and delete them safely.
Applies a grace period and mass-deletion guardrails to prevent accidental data loss."
  (let ((candidate-ids '())
        (deleted-count 0)
        (total-nodes 0)
        (nodes-with-file 0)
        (nodes-without-file 0)
        (now (current-time)))
    ;; Collect IDs of orphaned nodes that exceeded grace period
    (supertag-traverse-nodes
     (lambda (id node)
       (cl-incf total-nodes)
       (let ((file-prop (plist-get node :file)))
         (if file-prop
             (cl-incf nodes-with-file)
           (cl-incf nodes-without-file)))
       (when (and (eq (plist-get node :type) :node)
                  (null (plist-get node :file))
                  (stringp id)
                  (not (string= id "")))
        (let* ((orphaned-at (plist-get node :orphaned-at))
               ;; Compute age safely; if ORPHANED-AT is invalid, ignore-errors returns nil
               (age (ignore-errors (float-time (time-subtract now orphaned-at)))))
           (when (and age (>= age (or supertag-sync-orphan-grace-seconds 0)))
             (push id candidate-ids))))))

    ;; Mass-deletion guardrails
    (let* ((candidate-count (length candidate-ids))
           (ratio (if (> total-nodes 0)
                      (/ (float candidate-count) (float total-nodes))
                    0.0))
           (ratio-cap (or supertag-sync-max-delete-ratio 1.0))
           (count-cap (or supertag-sync-max-delete-count most-positive-fixnum)))
      (when (and (> candidate-count 0)
                 (or (> ratio ratio-cap)
                     (> candidate-count count-cap)))
        (message (concat "GC aborted: candidate orphan deletions (%d/%d, %.2f%%) exceed safety caps. "
                         "Adjust `supertag-sync-max-delete-ratio`/`supertag-sync-max-delete-count` if intentional.")
                 candidate-count total-nodes (* ratio 100))
        (setq candidate-ids '())))

    ;; Delete eligible orphaned nodes outside of transaction for immediacy
    (dolist (id candidate-ids)
      (let ((node (supertag-node-get id)))
        (when (and node (null (plist-get node :file)))
          (supertag-node-delete id)
          (cl-incf deleted-count))))

    (when (> deleted-count 0)
      (supertag-save-store))
    deleted-count))

(defun supertag-sync--id-exists-in-file-p (id file)
  "Check if a node ID exists in the specified FILE.
ID is the node ID string. FILE is the absolute path.
Returns t if the node ID is found, nil otherwise."
  (and id
       file
       (file-exists-p file)
       (with-temp-buffer
         (insert-file-contents-literally file)
         (goto-char (point-min))
         (re-search-forward (concat ":ID:[ \t]+" (regexp-quote id)) nil t))))

(defun supertag-sync-validate-nodes (&optional counters)
  "Validate all nodes in the database against their source files.
This function iterates through all nodes in the store and checks if they
still exist in their corresponding files. If not, they are marked as
orphaned (by setting :file to nil) to be garbage collected later.
COUNTERS is a plist for tracking changes."
  (supertag-traverse-nodes
   (lambda (id node)
     (let ((file (plist-get node :file))
           (type (plist-get node :type)))
       ;; Only check nodes that are supposed to be in a file, and are of type :node.
       ;; If :file is already nil, it's already an orphan.
       (when (and file (eq type :node))
         (unless (supertag-sync--id-exists-in-file-p id file)
           (supertag-node-mark-deleted-from-file id)
           (when counters
             (setf (plist-get counters :nodes-deleted) (1+ (plist-get counters :nodes-deleted))))))))))
  

;; --- Org Parser ---
(defun supertag--parse-properties (properties-plist)
  "Parse a properties plist from org-element into an alist."
  (let ((node-props '()))
    (when properties-plist
      (let ((props (copy-list properties-plist)))
        (while props
          (let* ((key-str (plist-get props :key))
                 (key (intern (concat ":" key-str)))
                 (value (plist-get props :value)))
            (push (cons key value) node-props))
          (setq props (cddr props)))))
    (nreverse node-props)))

(defun supertag--extract-refs (elements)
  "Extract id: links from a list of org elements."
  (let ((refs '()))
    (when elements
      (org-element-map elements 'link
        (lambda (link)
          (when (and (equal (org-element-property :type link) "id")
                     (org-uuidgen-p (org-element-property :path link)))
            (push (org-element-property :path link) refs)))))
    (nreverse refs)))

(defun supertag--extract-inline-tags-from-string (content-string)
  "Extract all tags from a CONTENT-STRING using a regex."
  (let ((tags '()))
    (when content-string
      (with-temp-buffer
        (insert content-string)
        (goto-char (point-min))
        (while (re-search-forward "#\\([a-zA-Z0-9][-_a-zA-Z0-9]*\\)" nil t)
             (push (match-string 1) tags))))
    (nreverse tags)))

(defun supertag--strip-inline-tags (title)
  "Remove inline #tags from TITLE while preserving surrounding text.
Collapses extra spaces produced by tag removal and trims leading/trailing
whitespace."
  (when title
    (let* ((without-tags (replace-regexp-in-string
                          "#[a-zA-Z0-9][-_a-zA-Z0-9]*\\(?:[ \t]+\\)?"
                          "" title))
           (collapsed (replace-regexp-in-string "[ \t]+" " " without-tags)))
      (string-trim collapsed))))

  (defun supertag--extract-inline-tags (elements)
  "Extract all tags from org ELEMENTS.
ELEMENTS can be a list of org elements or a single element.
If ELEMENTS is a string, extract tags directly from it."
  (let ((tags '()))
    (cond
     ((stringp elements)
      (setq tags (supertag--extract-inline-tags-from-string elements)))
     (elements
      (org-element-map elements '(paragraph plain-text)
        (lambda (element)
          (let ((content (org-element-property :value element)))
            (when content
              (setq tags (append tags (supertag--extract-inline-tags-from-string content)))))))))
      (cl-delete-duplicates tags :test #'equal)))

  (defun supertag--extract-org-headline-tags (headline)
    "Extract org native tags (:tag:) from HEADLINE element.
Return a list of tag strings, or an empty list if none."
    (let ((tags (org-element-property :tags headline)))
      (when tags
        (cl-remove-if (lambda (s) (or (null s) (string-empty-p s)))
                      (mapcar #'identity tags)))))

  (defun supertag--merge-and-sanitize-tags (tags-1 tags-2)
    "Merge two tag lists and sanitize names.
Returns a de-duplicated list preserving order preference of TAGS-1."
    (let* ((sanitize #'(lambda (s) (and s (supertag-sanitize-tag-name s))))
           (a (delq nil (mapcar sanitize tags-1)))
           (b (delq nil (mapcar sanitize tags-2)))
           (seen (make-hash-table :test 'equal))
           (out '()))
      (dolist (tag a)
        (unless (gethash tag seen)
          (push tag out)
          (puthash tag tag seen)))
      (dolist (tag b)
        (unless (gethash tag seen)
          (push tag out)
          (puthash tag tag seen)))
      (nreverse out)))

  (defun supertag--resolve-tag-style (&optional node file)
    "Resolve write style for tags for NODE/FILE context.
Currently returns `supertag-tag-style`, using 'inline when value is 'auto."
    (let ((style supertag-tag-style))
      (if (eq style 'auto) 'inline style)))

  (defun supertag--format-tags-by-style (tags style)
    "Return a string representing TAGS according to STYLE.
Result includes a leading space when non-empty, else an empty string."
    (let* ((inline-part (when tags (mapconcat (lambda (tag) (concat "#" tag)) tags " ")))
           (org-part (when tags (concat ":" (mapconcat #'identity tags ":") ":"))))
      (pcase style
        ('inline (if inline-part (concat " " inline-part) ""))
        ('org    (if org-part    (concat " " org-part)    ""))
        ('both   (cond
                  ((and inline-part org-part) (concat " " inline-part " " org-part))
                  (inline-part (concat " " inline-part))
                  (org-part (concat " " org-part))
                  (t "")))
        (_ (if inline-part (concat " " inline-part) "")))))

  (defun supertag--render-org-headline (level title tags file node &optional style tag-position)
    "Render an Org headline line given LEVEL, TITLE and TAGS.
Returns a single line string ending with a newline.
TAG-POSITION can be :before-title, :after-title, or nil (default after title)."
    (let* ((resolved (or style (supertag--resolve-tag-style node file)))
           (stars (make-string (max 1 (or level 1)) ?*))
           (tags-part (when tags (supertag--format-tags-by-style tags resolved))))
      (cond
       ;; Tags before title: * #tag1 #tag2 Title
       ((eq tag-position :before-title)
        (format "%s%s %s\n" stars (or tags-part "") title))
       ;; Tags after title (default): * Title #tag1 #tag2
       (t
        (format "%s %s%s\n" stars title (or tags-part ""))))))

  (defun supertag--apply-legacy-tags-policy (buffer beg end tags)
    "Apply legacy tags policy within BUFFER on region [BEG, END].
If `supertag-sync-legacy-tags-policy' is 'lazy-convert, remove trailing
org native :tag: from headline line and ensure inline-tags exist.
Returns non-nil when a modification was performed."
    (when (eq supertag-sync-legacy-tags-policy 'lazy-convert)
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (goto-char (point-min))
            (when (looking-at "^\*+ .*")
              (let ((changed nil))
                ;; Remove trailing :tag: block
                (when (re-search-forward "\s-+:[^\n:]+:" (line-end-position) t)
                  (replace-match "")
                  (setq changed t))
                ;; Ensure inline #tags present if TAGS provided
                (when (and tags (> (length tags) 0))
                  (end-of-line)
                  (insert (supertag--format-tags-by-style tags 'inline))
                  (setq changed t))
                changed)))))))


(defun supertag--create-tag-entities (tag-names)
  "Create tag entities for TAG-NAMES and return their IDs.
Ensures tags are created only once and returns existing tag IDs.
IMPORTANT: This function NEVER modifies existing tags - it only creates new ones."
  (let ((tag-ids '()))
    (dolist (tag-name tag-names)
      (let* ((sanitized-name (supertag-sanitize-tag-name tag-name))
             (tag-id sanitized-name) 
             (existing-tag (supertag-tag-get tag-id)))
        ;; CRITICAL: Only create if tag doesn't exist
        ;; Never modify existing tags to preserve their field definitions
        (unless existing-tag
          (supertag-tag-create (list :id tag-id :name sanitized-name)))
        (push tag-id tag-ids)))
    (nreverse tag-ids)))

(defun supertag--create-node-tag-relations (node-id tag-ids)
  "Create node-tag relations for NODE-ID and TAG-IDS.
Relation creation function now has built-in duplicate checking."
  (dolist (tag-id tag-ids)
    ;; supertag-relation-create now handles duplicate checking internally
    (supertag-relation-create
     (list :type :node-tag
           :from node-id
           :to tag-id
           :created-at (current-time)))))

(defun supertag--process-node-tags (node-data)
  "Process tags for a node, creating tag entities and relations only when necessary.
NODE-DATA is the node plist containing tag information.
This function is called only when a node is actually being created or updated."
  (let ((node-id (plist-get node-data :id))
        (all-tags (plist-get node-data :tags)))
    (when (and node-id all-tags)
      ;; Create tag entities only if they don't exist
      (let ((tag-ids (supertag--create-tag-entities all-tags)))
        ;; Create node-tag relations only if they don't exist
        (supertag--create-node-tag-relations node-id tag-ids)))))


(defun supertag--process-node-references (node-data counters)
  "Process reference relations for a node, creating reference relations as needed.
NODE-DATA is the node plist containing reference information.
COUNTERS is a plist for tracking relation statistics.
This function is called only when a node is actually being created or updated."
  (let ((node-id (plist-get node-data :id))
        (ref-to-list (plist-get node-data :ref-to)))
    (when (and node-id ref-to-list)
      ;; Process each reference
      (dolist (target-id ref-to-list)
        (when (and (stringp target-id) (not (string-empty-p target-id)))
          ;; Check if target node exists in the store
          (let ((target-node (supertag-node-get target-id)))
            (if target-node
                (progn
                  ;; Create reference relation using unified service (handles duplicates + backlinks)
                  (let ((existing-relations (supertag-relation-find-between node-id target-id :reference)))
                    (unless existing-relations
                      (when (supertag-relation-add-reference node-id target-id)
                        (setf (plist-get counters :references-created)
                              (1+ (or (plist-get counters :references-created) 0)))))))
              ;; Target node doesn't exist yet - this is normal during batch sync
              ;; (message "DEBUG: Target node %s not found yet, reference relation will be created when target is processed." target-id)
              )))))))

(defun supertag--cleanup-orphaned-references (node-id current-refs counters)
  "Clean up orphaned reference relations for a node.
NODE-ID is the node's ID.
CURRENT-REFS is the current list of references from the file.
COUNTERS is a plist for tracking relation statistics."
  (let ((existing-relations (supertag-relation-find-by-from node-id :reference)))
    (dolist (relation existing-relations)
      (let ((target-id (plist-get relation :to)))
        ;; If this relation's target is not in current refs, delete it
        (unless (member target-id current-refs)
          (message "DEBUG: Removing orphaned reference relation (%s -> %s)." node-id target-id)
          (supertag-relation-delete (plist-get relation :id))
          (setf (plist-get counters :references-deleted)
                (1+ (or (plist-get counters :references-deleted) 0))))))))

(defun supertag--extract-outline-path (headline)
  "Extract the outline path (olp) for HEADLINE.
Returns a list of ancestor titles from root to current headline (inclusive).
For example: (\"Top Level\" \"Second Level\" \"Current Headline\")
The titles remove inline #tags as well as TODO keywords and org :tags:."
  (let ((path '())
        (current headline))
    ;; Traverse up the hierarchy collecting titles
    (while current
      (when (eq (org-element-type current) 'headline)
        (let* ((raw-title (org-element-property :raw-value current))
               (todo-keyword (org-element-property :todo-keyword current))
               ;; Clean title: remove TODO keyword, org :tags:, and inline #tags
               (cleaned-title
                (when raw-title
                  (let ((title raw-title))
                    (when todo-keyword
                      (setq title (replace-regexp-in-string
                                   (concat "^" (regexp-quote todo-keyword) "\\s-+")
                                   "" title)))
                    ;; Remove org native :tags: at the end
                    (setq title (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*$" "" title))
                    (supertag--strip-inline-tags title)))))
          (when cleaned-title (push cleaned-title path))))
      ;; Move to parent element
      (setq current (org-element-property :parent current))
      ;; Stop if we've reached the document root
      (when (or (not current)
                (eq (org-element-type current) 'org-data))
        (setq current nil)))
    path))

(defun supertag--extract-node-own-content (headline contents-begin contents-end)
  "Extract only the content that belongs to HEADLINE, excluding sub-headlines.
HEADLINE is the org-element headline object.
CONTENTS-BEGIN and CONTENTS-END are the content boundaries from org-element.
Returns a string containing only the node's own content."
  (if (not (and contents-begin contents-end (> contents-end contents-begin)))
      ""
    (save-excursion
      (goto-char contents-begin)
      (let ((current-level (org-element-property :level headline))
            (content-end contents-end)
            (search-pos contents-begin))
        ;; Use a safer approach: find first child headline
        (goto-char contents-begin)
        (when (re-search-forward (format "^\\*\\{%d,\\} " (1+ current-level)) contents-end t)
          ;; Found a child headline at deeper level
          (setq content-end (line-beginning-position)))
        ;; Extract content from contents-begin to the adjusted content-end
        (buffer-substring-no-properties contents-begin content-end)))))

  (defun supertag--convert-element-to-node-plist (headline file &optional migration-mode)
  "Convert a headline ELEMENT from org-element into a node plist.
This is the core reusable parser for a single headline.
NOTE: This function only parses data, it does NOT create tag entities or relations.
MIGRATION-MODE when t, only processes nodes with existing IDs (no auto-generation)."
  (let* ((id (org-element-property :ID headline))
         (contents-begin (org-element-property :contents-begin headline))
         (contents-end (org-element-property :contents-end headline))
         (original-raw-title (org-element-property :raw-value headline))
         (todo-keyword (org-element-property :todo-keyword headline))
         ;; Clean title: remove TODO keyword, :tags:, and inline #tags while keeping display text
         (cleaned-title
          (when original-raw-title
            (let ((title original-raw-title))
              (when todo-keyword
                (setq title (replace-regexp-in-string
                             (concat "^" (regexp-quote todo-keyword) "\\s-+")
                             "" title)))
              (setq title (replace-regexp-in-string ":[[:alnum:]_@#%]+:" "" title))
              (setq title (supertag--strip-inline-tags title))
              (string-trim title))))
         (final-title (if (or (null cleaned-title) (string-empty-p cleaned-title))
                          original-raw-title
                        cleaned-title))
         ;; Extract outline path (olp) - list of ancestor titles from root to current
         (olp (supertag--extract-outline-path headline))
         (headline-tags (supertag--extract-inline-tags original-raw-title))
         (content-tags (supertag--extract-inline-tags (org-element-contents headline)))
         (org-native-tags (if supertag-sync--is-full-rescan-p
                              (or (supertag--extract-org-headline-tags headline) '())
                            '()))
         (all-tags (supertag--merge-and-sanitize-tags
                   (cl-union headline-tags content-tags :test #'equal)
                     org-native-tags))
         (properties (supertag--parse-properties (org-element-property :properties headline)))
         (refs-to (supertag--extract-refs
                   (when contents-begin
                     (org-element-contents headline)))))
    ;; Handle ID generation based on mode
    (let ((final-id (if migration-mode
                        ;; Migration mode: only use existing IDs
                        id
                      ;; Normal mode: generate ID if auto-create is enabled
                      (or id (when supertag-sync-auto-create-node
                               (org-id-new))))))
      ;; Only create node if we have a valid ID
      (when final-id
        ;; NOTE: Tag creation and relationship establishment have been moved to the
        ;; supertag--process-node-tags function, so these actions are only performed
        ;; when a node actually needs to be created or updated.

        (list :id final-id
             :title (or final-title "Untitled Node")
             :raw-value final-title ;; Use final title for hashing
             :tags all-tags
             :properties properties
             :ref-to (cl-delete-duplicates refs-to :test #'equal)
            :file file
            :olp olp ;; Add outline path
            :content (let ((raw-content (if (and contents-begin contents-end)
                                             ;; Extract only content up to first child headline
                                             (supertag--extract-node-own-content headline contents-begin contents-end)
                                           "")))
                       ;; Aggressively remove any properties drawers found in the content area.
                       ;; This is necessary because drawers in the content area are parsed as
                       ;; plain paragraphs, so they cannot be filtered by element type.
                       (replace-regexp-in-string ":PROPERTIES:\n\\(.\\|\n\\)*?:END:\n?"
                                                 "" raw-content))
            :level (org-element-property :level headline)
           :todo (org-element-property :todo-keyword headline)
           :priority (let ((p (org-element-property :priority headline))) (and p (format "#%c" p)))
           :scheduled (let ((ts (org-element-property :scheduled headline))) (and ts (org-element-interpret-data ts)))
           :deadline (let ((ts (org-element-property :deadline headline))) (and ts (org-element-interpret-data ts)))
           :position (org-element-property :begin headline)
           :pos (org-element-property :begin headline))))))

(defun supertag--map-headlines (parsed-ast file &optional migration-mode)
  "Map over headlines in PARSED-AST and parse them into nodes.
MIGRATION-MODE when t, only processes nodes with existing IDs."
  (let (nodes)
    (org-element-map parsed-ast 'headline
      (lambda (headline)
        (let ((node (supertag--convert-element-to-node-plist headline file migration-mode)))
          (when node (push node nodes)))))
    (nreverse nodes)))

;;;###autoload
(defun supertag--parse-org-nodes (file &optional migration-mode)
  "Parse the org file and return a list of nodes. Entry point.
This function IGNORES content inside #+begin_embed blocks.
Uses a temporary buffer with minimal side effects to avoid interfering with other packages.
MIGRATION-MODE when t, only processes nodes with existing IDs."
  (unless (file-exists-p file)
    (error "File does not exist: %s" file))
  (with-temp-buffer
    ;; Disable hooks and modes that might interfere
    (let ((inhibit-modification-hooks t)
          (org-mode-hook nil)
          (org-inhibit-startup t)
          (org-agenda-inhibit-startup t))
      ;; Ensure tab-width is 8 as required by org-current-text-column
      (setq-local tab-width 8)
      (insert-file-contents file)
      (goto-char (point-min))
      ;; Pre-process to remove content of embed blocks before parsing
      (while (re-search-forward "^#\\+begin_embed:.*$" nil t)
        (let ((start (match-end 0)))
          (when (re-search-forward "^#\\+end_embed" nil t)
            (delete-region start (match-beginning 0)))))
      (goto-char (point-min))
      ;; Parse without triggering org-mode initialization
      (let ((parsed-ast (org-element-parse-buffer)))
        (supertag--map-headlines parsed-ast file migration-mode)))))

;;;------------------------------------------------------------------
;;; Supertag Sync Auto Star or Stop
;;;------------------------------------------------------------------
;;;###autoload
(defun supertag-sync-start-auto-sync (&optional interval)
  "Start automatic synchronization with INTERVAL seconds.
If INTERVAL is nil, use `supertag-sync-auto-interval`."
  (interactive)
  
  ;; Safety check: ensure function is defined before setting timer
  (unless (fboundp 'supertag-sync--check-and-sync)
    (error "supertag-sync--check-and-sync function is not defined. Cannot start auto-sync."))
  
  ;; Cancel existing timer
  (when supertag-sync--timer
    (message "DEBUG: Canceling existing timer")
    (cancel-timer supertag-sync--timer)
    (setq supertag-sync--timer nil))

  ;; Clear pending idle work to avoid duplicates
  (supertag-sync--cancel-idle-dispatch)

  ;; Ensure store is initialized before starting auto-sync
  (unless (hash-table-p supertag--store)
    (setq supertag--store (ht-create)))
  ;; Start new timer with safety wrapper (fixed interval, not idle)
  (setq supertag-sync--timer
        (run-with-timer
         2 ; Start first sync after a short 2-second delay
         (or interval supertag-sync-auto-interval) ; Then, repeat at the configured interval
         (lambda ()
           "Safe wrapper for scheduling sync during idle periods."
           (supertag-sync--queue-idle-dispatch)))))

(defun supertag-sync-stop-auto-sync ()
  "Stop automatic synchronization."
  (interactive)
  (when supertag-sync--timer
    (cancel-timer supertag-sync--timer)
    (setq supertag-sync--timer nil)
    (message "Auto-sync stopped"))
  (supertag-sync--cancel-idle-dispatch))

;;;###autoload
(defun supertag-sync-full-rescan ()
  "Force a full rescan of every file currently managed by Supertag sync.
When called interactively, display a summary report and return a plist
describing the work that was performed."
  (interactive)
  (let* ((state-table (supertag-sync--get-state-table))
         (files (cl-delete-duplicates
                 (append (supertag-scan-sync-directories t)
                         (let (state-files)
                           (when (hash-table-p state-table)
                             (maphash (lambda (file _)
                                        (push file state-files))
                                      state-table))
                           state-files))
                 :test #'string-equal))
         (files (cl-remove-if-not
                 (lambda (file)
                   (and (stringp file)
                        (file-regular-p file)
                        (supertag-sync--in-sync-scope-p file)))
                 files))
         (processed 0)
         (counters '(:nodes-created 0 :nodes-updated 0 :nodes-deleted 0
                     :references-created 0 :references-deleted 0))
         (supertag-sync--is-full-rescan-p t))
    (supertag-with-transaction
      (dolist (file files)
        (condition-case err
            (progn
              (supertag-sync--process-single-file file counters)
              (cl-incf processed))
          (error
           (message "Supertag rescan skipped %s: %s" file (error-message-string err)))))
      (supertag-sync-validate-nodes counters))
    (supertag-sync-save-state)
    (let* ((gc-count (supertag-sync-garbage-collect-orphaned-nodes))
           (result (list :files-processed processed
                         :nodes-created (plist-get counters :nodes-created)
                         :nodes-updated (plist-get counters :nodes-updated)
                         :nodes-deleted (plist-get counters :nodes-deleted)
                         :references-created (plist-get counters :references-created)
                         :references-deleted (plist-get counters :references-deleted)
                         :garbage-collected gc-count)))
      (when (called-interactively-p 'interactive)
        (message "Supertag rescan: %d files processed, %d created, %d updated, %d deleted, %d refs created, %d refs deleted, %d GC."
                 processed
                 (plist-get counters :nodes-created)
                 (plist-get counters :nodes-updated)
                 (plist-get counters :nodes-deleted)
                 (plist-get counters :references-created)
                 (plist-get counters :references-deleted)
                 gc-count))
      result)))

;;;-------------------------------------------------------------------
;;; Database Cleanup
;;;-------------------------------------------------------------------

;;;###autoload
(defun supertag-sync-cleanup-database ()
  "Perform database maintenance by validating nodes and garbage collecting orphaned nodes.
This command runs two key maintenance functions in sequence:
1. `supertag-sync-validate-nodes': Validates all nodes against their source files
   and marks any "zombie nodes" (nodes in database but not in files) as orphaned.
2. `supertag-sync-garbage-collect-orphaned-nodes': Deletes all nodes marked as
   orphaned, including zombie nodes and nodes with nil file properties.

This is a safe operation that helps maintain database integrity."
  (interactive)
  (message "Starting database cleanup...")
  
  ;; Step 1: Validate all nodes and mark zombies as orphaned
  (let ((counters '(:nodes-deleted 0)))
    (supertag-sync-validate-nodes counters)
    (message "Node validation complete. %d nodes marked as orphaned." 
             (plist-get counters :nodes-deleted))
    
    ;; Step 2: Garbage collect all orphaned nodes
    (let ((deleted-count (supertag-sync-garbage-collect-orphaned-nodes)))
      (message "Database cleanup complete. %d orphaned nodes deleted." deleted-count))))



;;;-------------------------------------------------------------------
;;; Node-Based Real-time Sync
;;;-------------------------------------------------------------------

(defun supertag-sync--run-on-save ()
  "Hook function to run single-node sync after saving a buffer.
This function distinguishes between internal modifications (by Supertag) and
external modifications (by user/other tools) to avoid unnecessary re-parsing."
  ;; Only run for org-mode buffers that are part of the sync scope
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (supertag-sync--in-sync-scope-p (buffer-file-name)))
    (let ((file (buffer-file-name)))
      ;; Check if this is an internal modification
      (if (supertag--is-internal-modification-p file)
          ;; Internal modification: skip sync, memory is already up-to-date
          (progn
            (message "Supertag: Skip sync for internal modification: %s" (file-name-nondirectory file))
            ;; Update sync state to prevent periodic sync from re-syncing
            (supertag-sync-update-state file))
        ;; External modification: sync from file to memory
        (let ((counters '(:nodes-created 0 :nodes-updated 0 :nodes-deleted 0 :references-created 0 :references-deleted 0))
              (inhibit-modification-hooks t))
          (message "Supertag: Sync external modification: %s" (file-name-nondirectory file))
          ;; Process the file within a transaction for atomicity
          (supertag-with-transaction
            (supertag-sync--process-single-file file counters))
          ;; Immediately run garbage collection after processing
          (supertag-sync-garbage-collect-orphaned-nodes))))))
        

(defun supertag-sync-setup-realtime-hooks ()
  "Add hooks for real-time node synchronization."
  (add-hook 'after-save-hook #'supertag-sync--run-on-save nil t))

(defun supertag--parse-node-at-point ()
  "Parse the Org heading at point and return its property list.
This version manually extracts the subtree to bypass the org-element
cache, ensuring the current, unsaved buffer state is parsed.
Uses minimal side effects to avoid interfering with other packages."
  (when (org-at-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (let* ((begin (point))
             (end (save-excursion (org-end-of-subtree t t) (point)))
             (subtree-text (buffer-substring-no-properties begin end))
             (current-file (buffer-file-name)))
        (with-temp-buffer
          ;; Disable hooks that might interfere
          (let ((org-mode-hook nil)
                (org-inhibit-startup t)
                (org-agenda-inhibit-startup t)
                (inhibit-modification-hooks t))
            (insert subtree-text)
            (org-mode)
            ;; Ensure tab-width is 8 as required by org-current-text-column
            (setq-local tab-width 8)
            (let ((ast (org-element-parse-buffer)))
              ;; The AST of the subtree will have one top-level headline
              (when (and (eq (org-element-type ast) 'org-data)
                         (org-element-contents ast))
                (let ((headline-element (car (org-element-contents ast))))
                  (when (eq (org-element-type headline-element) 'headline)
                    (supertag--convert-element-to-node-plist headline-element current-file)))))))))))

;;;###autoload
(defun supertag-node-sync-at-point ()
  "Re-sync the node at point with the database.
Parses the current state of the headline and updates the store."
  (when (org-at-heading-p)
    (let ((props (supertag--parse-node-at-point)))
      (when props
        (supertag-node-create props)))))

;;;###autoload
(defun supertag-migrate-org-files-to-database (path &optional counters allow-no-id)
  "One-time migration function to import nodes and tags from Org files 
into the database.
PATH can be a file or a directory path. If it is a directory, all .org files 
will be processed recursively.

COUNTERS is an optional plist for tracking migration statistics.
ALLOW-NO-ID when t, also processes nodes without existing IDs (generates temporary IDs).
Returns a plist containing summary information.

This is a one-time operation for initializing user data when first using org-supertag.
It will create entities of type :node and :tag, and establish relations between them."
  (let* ((counters (or counters (list :files-processed 0
                                     :nodes-created 0
                                     :tags-created 0
                                     :relations-created 0
                                     :errors 0)))
         (org-files (if (file-directory-p path)
                        (supertag--find-org-files path)
                      (list path)))
         (all-nodes '())
         (all-tags '()))
    
    (message "Migrating org files to database...")
    (message "Found %d org files" (length org-files))
    
    ;; First phase: Parse all files, collect nodes and tags
    (dolist (file org-files)
      (condition-case err
          (progn
            (message "Parsing file: %s" file)
            (let ((nodes (supertag--parse-org-nodes file (not allow-no-id)))) ; migration-mode = not allow-no-id
              ;; When allow-no-id is nil: only nodes with existing IDs are returned
              ;; When allow-no-id is t: all nodes are returned (including auto-generated IDs)
              (setq all-nodes (append all-nodes nodes))
              ;; Collect tags from valid nodes
              (dolist (node nodes)
                (let ((node-tags (plist-get node :tags)))
                  (when node-tags
                    (setq all-tags (append all-tags node-tags))))))
            (setf (plist-get counters :files-processed)
                  (1+ (plist-get counters :files-processed)))))
        (error
         (message "Failed to parse file %s: %s" file (error-message-string err))
         (setf (plist-get counters :errors)
               (1+ (plist-get counters :errors))))))
    
    ;; Remove duplicate tags
    (setq all-tags (cl-delete-duplicates all-tags :test #'equal))
    
    (message "Parsing completed: %d nodes, %d unique tags" 
             (length all-nodes) (length all-tags))
    
    ;; Second phase: Create tag entities
    (message "Creating tag entities...")
    (let ((tag-ids (supertag--create-tag-entities all-tags)))
      (setf (plist-get counters :tags-created) (length tag-ids)))
    
    ;; Third phase: Create node entities and relations
    (message "Creating node entities and relations...")
    (dolist (node all-nodes)
      (condition-case err
          (progn
            ;; Create node
            (supertag-node-create node)
            (setf (plist-get counters :nodes-created)
                  (1+ (plist-get counters :nodes-created)))
            
            ;; Create node-tag relations
            (let ((node-id (plist-get node :id))
                  (node-tags (plist-get node :tags)))
              (when (and node-id node-tags)
                (let ((tag-ids (mapcar #'supertag-sanitize-tag-name node-tags)))
                  (supertag--create-node-tag-relations node-id tag-ids)
                  (setf (plist-get counters :relations-created)
                        (+ (plist-get counters :relations-created)
                           (length tag-ids)))))))
        (error
         (message "Failed to create node %s: %s" (plist-get node :id) (error-message-string err))
         (setf (plist-get counters :errors)
               (1+ (plist-get counters :errors))))))
    
    ;; Return statistics
    (message "Migration completed!")
    (message "Statistics: files=%d, nodes=%d, tags=%d, relations=%d, errors=%d"
             (plist-get counters :files-processed)
             (plist-get counters :nodes-created)
             (plist-get counters :tags-created)
             (plist-get counters :relations-created)
             (plist-get counters :errors))
    
    counters)

(defun supertag--find-org-files (directory)
  "Recursively find all .org files in DIRECTORY.
DIRECTORY is the directory path to search.
Returns a list of .org file paths."
  (let ((org-files '()))
    (dolist (file (directory-files-recursively directory "\\.org$"))
      (when (file-readable-p file)
        (push file org-files)))
    (nreverse org-files)))
    
(defun supertag--diagnose-empty-sync ()
  "Diagnose why sync found no files to process.
Provides helpful hints to the user about configuration issues."
  (let ((sync-dirs org-supertag-sync-directories)
        (state-table (supertag-sync--get-state-table))
        (state-count (if (hash-table-p (supertag-sync--get-state-table))
                         (hash-table-count (supertag-sync--get-state-table))
                       0)))
    
    (cond
     ;; Case 1: No sync directories configured
     ((null sync-dirs)
      (message "DIAGNOSTIC: No sync directories configured. Set org-supertag-sync-directories."))
     
     ;; Case 2: Sync directories don't exist
     ((not (cl-some #'file-directory-p sync-dirs))
      (message "DIAGNOSTIC: None of the configured sync directories exist:")
      (dolist (dir sync-dirs)
        (message "  - %s [%s]" dir (if (file-exists-p dir) "exists but not a directory" "does not exist"))))
     
     ;; Case 3: Directories exist but contain no matching files
     ((= state-count 0)
      (message "DIAGNOSTIC: Sync directories exist but no .org files found or tracked:")
      (dolist (dir sync-dirs)
        (when (file-directory-p dir)
          (let ((org-files (directory-files-recursively dir "\\.org$" nil)))
            (message "  - %s: %d .org files found" dir (length org-files))
            (when (= (length org-files) 0)
              (message "    Hint: Check if directory contains .org files"))))))
     
     ;; Case 4: Files tracked but all up-to-date
     (t
      (message "DIAGNOSTIC: %d files tracked." state-count)))))

(provide 'supertag-services-sync)
