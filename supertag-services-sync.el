;;; org-supertag/services/sync.el --- Synchronization mechanism for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements the synchronization mechanism for the Org-Supertag
;; data-centric architecture. It handles importing data from Org files into the
;; central store and exporting data from the store back to Org files.

;;; Code:


(require 'cl-lib)
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

;;; Variables

(defvar supertag-sync--state (make-hash-table :test 'equal)
  "Track file modification states.
Key: file path
Value: last sync time")

;;; Helper functions for accessing sync state data

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

;;; --- Internal Helper ---

;; ID generation is now handled by supertag-id-utils.el

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
      (and included
           (not excluded)
           (string-match-p supertag-sync-file-pattern file)))))

(defun supertag-scan-sync-directories (&optional all-files-p)
  "Scan sync directories for org files.
If ALL-FILES-P is non-nil, return all files in scope.
Otherwise, returns a list of new files that are not yet in sync state."
  (let ((files nil)
        (state-table (supertag-sync--get-state-table)))
    (dolist (dir org-supertag-sync-directories)
      (when (file-exists-p dir)
        (let ((dir-files (directory-files-recursively
                         dir supertag-sync-file-pattern t)))
          (dolist (file dir-files)
            (when (and (file-regular-p file)
                       (supertag-sync--in-sync-scope-p file)
                       (or all-files-p
                           (not (gethash file state-table))))
              (push file files))))))
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
Returns a string containing the Org content."
  (with-temp-buffer
    (dolist (node nodes)
      (let* ((title (plist-get node :title))
             (tags (plist-get node :tags))
             (level (or (plist-get node :level) 1))
             (content (or (plist-get node :content) ""))
             (id (plist-get node :id)))
        ;; Reconstruct the node with proper formatting.
        (insert (format "%s %s%s\n"
                        (make-string level ?*)
                        title
                        (if tags (concat " :" (mapconcat #'identity tags ":") ":") "")))
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
If file doesn't exist, initialize empty state."
  (if (file-exists-p supertag-sync-state-file)
      (with-temp-buffer
        (insert-file-contents supertag-sync-state-file)
        (setq supertag-sync--state
              (read (current-buffer)))
        ;; Ensure the loaded state is in the correct format
        (supertag-sync--ensure-state-format))
    ;; Initialize empty state if file doesn't exist
    (setq supertag-sync--state (make-hash-table :test 'equal))
    ;; Save the initial state
    (supertag-sync-save-state)))

;; --- Check and sync ---

(defun supertag-sync-ensure-directories ()
  "Ensure sync directories are properly configured."
  (unless org-supertag-sync-directories
    (message "Warning: `org-supertag-sync-directories` is not set. Auto-sync will not occur.")))
    
(defvar supertag-sync--timer nil
  "Timer for periodic sync checks.")


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
  (when-let ((node (supertag-get (list :nodes id))))
    ;;(message "DEBUG: supertag-node-mark-deleted-from-file: Node file before update: %S" (plist-get node :file))
    (let ((modified-node (plist-put node :file nil)))
      ;; 直接更新现有节点，而不是"创建"新节点
      ;; 这避免了产生混乱的debug输出和file: nil的节点创建消息
      (supertag-store-direct-set :nodes id modified-node))
    ;;(message "DEBUG: supertag-node-mark-deleted-from-file: Node file after update (should be nil): %S" (plist-get (supertag-get (list :nodes id)) :file))
    ))

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
      (supertag-node-create node-props))))

(defun supertag-node-changed-p (old-node new-node)
  "Compare OLD-NODE and NEW-NODE to detect changes.
If OLD-NODE doesn't have a hash value, calculate it on the fly."
  (let ((old-hash (or (plist-get old-node :hash)
                      (supertag-node-hash old-node)))
        (new-hash (or (plist-get new-node :hash)
                      (supertag-node-hash new-node))))
    (not (string= old-hash new-hash))))

(defun supertag-sync--process-single-file (file counters)
  "Process a single FILE for synchronization.
COUNTERS is a plist for tracking :nodes-created, :nodes-updated, and :nodes-deleted."
  (message "Syncing file: %s" file)
  (let* ((current-nodes-in-file (make-hash-table :test 'equal))
         (nodes-from-file (supertag--parse-org-nodes file))
         (existing-nodes-in-store (supertag-find-nodes-by-file file)))

    (message "DEBUG-PROCESS: Parser found %d nodes in file." (length nodes-from-file))
    (message "DEBUG-PROCESS: DB query found %d existing nodes for this file." (length existing-nodes-in-store))

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
          (setf (plist-get counters :nodes-deleted) (1+ (plist-get counters :nodes-deleted))))
         ((supertag-node-changed-p old-node-props new-node-props)
          (supertag-db-add-with-hash id new-node-props counters)
          (setf (plist-get counters :nodes-updated) (1+ (plist-get counters :nodes-updated)))))
        (remhash id current-nodes-in-file)))

    (message "DEBUG-PROCESS: Found %d new nodes to create." (hash-table-count current-nodes-in-file))

    ;; Process new nodes from file
    (maphash (lambda (id new-node-props)
               (message "DEBUG-PROCESS: Creating new node with ID: %s" id)
               (supertag-db-add-with-hash id new-node-props counters)
               (setf (plist-get counters :nodes-created) (1+ (plist-get counters :nodes-created))))
             current-nodes-in-file)

    ;; Update sync state for the file
    (supertag-sync-update-state file)
    (message "DEBUG-PROCESS: Finished processing file: %s" file)))

(defun supertag-sync--verify-file-nodes (file counters)
  "Verify that nodes in the database still exist in the file.
This function checks if nodes associated with FILE still exist in the file.
If a node exists in the database but not in the file, it's marked as orphaned.
FILE is the file path to verify.
COUNTERS is a plist for tracking :nodes-created, :nodes-updated, and :nodes-deleted."
  ;;(message "DEBUG: Verifying nodes for file: %s" file)
  (let* ((current-nodes-in-file (make-hash-table :test 'equal))
         (nodes-from-file (when (file-exists-p file)
                           (supertag--parse-org-nodes file)))
         (existing-nodes-in-store (supertag-find-nodes-by-file file)))

    ;; If file doesn't exist, mark all its nodes as orphaned
    (unless (file-exists-p file)
      (message "DEBUG: File %s no longer exists, marking all its nodes as orphaned" file)
      (dolist (existing-node-pair existing-nodes-in-store)
        (let* ((id (car existing-node-pair))
               (db-node (supertag-get (list :nodes id))))
          (when (and db-node
                     (let ((db-node-file (plist-get db-node :file)))
                       (and db-node-file
                            (string= (expand-file-name db-node-file) (expand-file-name file)))))
            ;;(message "DEBUG: Marking node %s as orphaned because file %s no longer exists" id file)
            (supertag-node-mark-deleted-from-file id)
            (setf (plist-get counters :nodes-deleted) (1+ (plist-get counters :nodes-deleted))))))
      (return-from supertag-sync--verify-file-nodes))

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
          (let ((db-node (supertag-get (list :nodes id))))
            ;;(message "DEBUG: supertag-sync--verify-file-nodes: Node %s exists in DB but not in file %s. DB node file: %S" id file (plist-get db-node :file))
            (when (and db-node
                       (let ((db-node-file (plist-get db-node :file)))
                         (and db-node-file
                              (string= (expand-file-name db-node-file) (expand-file-name file)))))
              ;;(message "DEBUG: Node %s deleted from file %s. Marking as orphaned." id file)
              (supertag-node-mark-deleted-from-file id)
              (setf (plist-get counters :nodes-deleted)
		    (1+ (plist-get counters :nodes-deleted))))))))))


(defun supertag-sync--check-and-sync ()
  "Check and synchronize modified files.
  This is the main sync function called periodically."
  ;;(message "DEBUG: supertag-sync--check-and-sync function called at %s" (current-time))
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
      (let ((state-table (supertag-sync--get-state-table)))
        (maphash (lambda (file _state)
                   (when (or (not (file-exists-p file))
                             (not (supertag-sync--in-sync-scope-p file)))
                     (push file files-to-remove)))
                 state-table))

      (when files-to-remove
        (setq state-changed t)
        (let ((state-table (supertag-sync--get-state-table)))
          (dolist (file files-to-remove)
            (message "Removing file from sync state (file %s or out of scope): %s"
                     (if (file-exists-p file) "exists but out of scope" "doesn't exist")
                     file)
            (remhash file state-table)
            ;; Also clean up any nodes associated with these files
            (supertag-sync--verify-file-nodes file counters))))

      ;; 2. Scan for New Files
      (let ((new-files (supertag-scan-sync-directories)))
        (when new-files
          (setq state-changed t) ; Remove debug message
          (dolist (file new-files)
            (supertag-sync-update-state file))))
      (when state-changed
        (supertag-sync-save-state))

      ;; 3. Process Modified Files
      (when modified-files
        (let ((sorted-files (sort modified-files ; Remove debug message
                                  (lambda (a b)
                                    (time-less-p
                                     (file-attribute-modification-time (file-attributes a))
                                     (file-attribute-modification-time (file-attributes b)))))))
          (dolist (file sorted-files)
            (supertag-sync--process-single-file file counters)
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

      ;; Report results ; Remove debug message
      (let ((refs-created (or (plist-get counters :references-created) 0))
            (refs-deleted (or (plist-get counters :references-deleted) 0)))
        (message "File Sync Completed: %d nodes created, %d updated, %d deleted, %d refs created, %d refs deleted."
                 (plist-get counters :nodes-created)
                 (plist-get counters :nodes-updated)
                 (plist-get counters :nodes-deleted)
                 refs-created
                 refs-deleted)))

    ;; Run garbage collection outside of transaction to ensure immediate persistence
    (supertag-sync-garbage-collect-orphaned-nodes)))

;;; --- Enhanced Hash Table Traversal Utilities ---

(defun supertag-traverse-collection (collection-path callback)
  "Traverse a collection in the nested hash table at COLLECTION-PATH.
CALLBACK is a function that receives (id value) pairs.
Returns a list of results from CALLBACK."
  (let ((collection (supertag-get collection-path))
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
  (let ((nodes-collection (supertag-get (list :nodes)))
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
  "Scan the store for nodes marked as orphaned (:file nil) and delete them.
This function performs the actual deletion of orphaned nodes."
  (let ((orphaned-ids nil)
        (deleted-count 0)
        (total-nodes 0)
        (nodes-with-file 0)
        (nodes-without-file 0))
    ;; Collect IDs of orphaned nodes using enhanced traversal
    (setq orphaned-ids
          (supertag-traverse-nodes
           (lambda (id node)
             (cl-incf total-nodes)
             (let ((file-prop (plist-get node :file)))
               (if file-prop
                   (cl-incf nodes-with-file)
                 (cl-incf nodes-without-file))
               (when (and (eq (plist-get node :type) :node)
                          (null (plist-get node :file))
                          (stringp id)
                          (not (string= id "")))
                 id)))))

    ;; Delete orphaned nodes outside of transaction to ensure immediate persistence
    (when orphaned-ids
      (dolist (id orphaned-ids)
        (let ((node (supertag-get (list :nodes id))))
          (when node
            ;; Double-check that this is indeed an orphaned node
            (when (and (eq (plist-get node :type) :node)
                       (null (plist-get node :file)))
              (supertag-update (list :nodes id) nil)
              (cl-incf deleted-count))))))

    ;; Force immediate save after garbage collection
    (when (> deleted-count 0)
      (supertag-save-store))

    (message "Orphaned node garbage collection complete. %d nodes deleted." deleted-count)

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
  (message "DEBUG: Starting deep validation of all nodes against files...")
  (supertag-traverse-nodes
   (lambda (id node)
     (let ((file (plist-get node :file)))
       ;; Only check nodes that are supposed to be in a file.
       ;; If :file is already nil, it's already an orphan.
       (when file
         (unless (supertag-sync--id-exists-in-file-p id file)
           (message "DEBUG: Validation found zombie node! ID: %s, File: %s" id file)
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
  "Extract all #tags from a CONTENT-STRING using a regex."
  (let ((tags '()))
    (when content-string
      (with-temp-buffer
        (insert content-string)
        (goto-char (point-min))
        (while (re-search-forward "#\\(\\w[-_[:alnum:]]*\\)" nil t)
          (push (match-string 1) tags))))
    (nreverse tags)))

(defun supertag--extract-inline-tags (elements)
  "Extract all #tags from org ELEMENTS.
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

(defun supertag--create-tag-entities (tag-names)
  "Create tag entities for TAG-NAMES and return their IDs.
Ensures tags are created only once and returns existing tag IDs."
  (let ((tag-ids '()))
    (dolist (tag-name tag-names)
      (let* ((sanitized-name (supertag-sanitize-tag-name tag-name))
             (tag-id sanitized-name)  ; 直接使用清理后的名称作为ID
             (existing-tag (supertag-tag-get tag-id)))
        (if existing-tag
            (message "DEBUG: Tag '%s' already exists, reusing." tag-id)
          (progn
            (message "DEBUG: Creating new tag '%s'." tag-id)
            (supertag-tag-create (list :id tag-id :name sanitized-name))))
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
          (let ((target-node (supertag-get (list :nodes target-id))))
            (if target-node
                (progn
                  ;; Create reference relation (supertag-relation-create handles duplicates)
                  (let ((existing-relations (supertag-relation-find-between node-id target-id :reference)))
                    (if existing-relations
                        (message "DEBUG: Reference relation (%s -> %s) already exists, skipping." node-id target-id)
                      (progn
                        (message "DEBUG: Creating new reference relation (%s -> %s)." node-id target-id)
                        (supertag-relation-create
                         (list :type :reference
                               :from node-id
                               :to target-id
                               :created-at (current-time)))
                        (setf (plist-get counters :references-created)
                              (1+ (or (plist-get counters :references-created) 0)))))))
              ;; Target node doesn't exist yet - this is normal during batch sync
              (message "DEBUG: Target node %s not found yet, reference relation will be created when target is processed." target-id))))))))

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

(defun supertag--convert-element-to-node-plist (headline file)
  "Convert a headline ELEMENT from org-element into a node plist.
This is the core reusable parser for a single headline.
NOTE: This function only parses data, it does NOT create tag entities or relations."
  (let* ((id (org-element-property :ID headline))
         (contents-begin (org-element-property :contents-begin headline))
         (contents-end (org-element-property :contents-end headline))
         (section-elements (org-element-contents headline))
         (original-raw-title (org-element-property :raw-value headline))
         ;; Defensively clean title of both #tags and :tags: to prevent duplication.
         (cleaned-raw-title (string-trim (replace-regexp-in-string ":[[:alnum:]_@#%]+:" "" (replace-regexp-in-string
"#\\w[-_[:alnum:]]*" "" original-raw-title))))
         (headline-tags (supertag--extract-inline-tags original-raw-title))
         (content-tags (supertag--extract-inline-tags section-elements))
         (all-tags (cl-union headline-tags content-tags :test #'equal))
         (properties (supertag--parse-properties (org-element-property :properties headline)))
         (refs-to (supertag--extract-refs
                   (when contents-begin
                     (org-element-contents headline)))))
    ;; Generate ID only if auto-create is enabled and no ID exists
    (let ((final-id (or id
                        (when supertag-sync-auto-create-node
                          (org-id-new)))))
      ;; Only create node if we have a valid ID (either existing or auto-generated)
      (when final-id
        ;; NOTE: 标签创建和关系建立移到了 supertag--process-node-tags 函数中
        ;; 这样只有在节点真正需要创建或更新时才会执行标签操作
        
        (list :id final-id
            :title (or cleaned-raw-title "Untitled Node")
            :raw-value cleaned-raw-title ;; Use cleaned title for hashing
            :tags all-tags
            :properties properties
            :ref-to (cl-delete-duplicates refs-to :test #'equal)
            :file file
            :content (let ((raw-content (if (and contents-begin contents-end)
                                             (buffer-substring-no-properties contents-begin contents-end)
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

(defun supertag--map-headlines (parsed-ast file)
  "Map over headlines in PARSED-AST and parse them into nodes."
  (let (nodes)
    (org-element-map parsed-ast 'headline
      (lambda (headline)
        (let ((node (supertag--convert-element-to-node-plist headline file)))
          (when node (push node nodes)))))
    (nreverse nodes)))

;;;###autoload
(defun supertag--parse-org-nodes (file)
  "Parse the org file and return a list of nodes. Entry point.
This function IGNORES content inside #+begin_embed blocks."
  (unless (file-exists-p file)
    (error "File does not exist: %s" file))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    ;; Pre-process to remove content of embed blocks before parsing
    (while (re-search-forward "^#\\+begin_embed:.*$" nil t)
      (let ((start (match-end 0)))
        (when (re-search-forward "^#\\+end_embed" nil t)
          (delete-region start (match-beginning 0)))))
    (goto-char (point-min)) ; Go back to beginning for parsing
    (let ((parsed-ast (org-element-parse-buffer)))
      (supertag--map-headlines parsed-ast file))))

;;;------------------------------------------------------------------
;;; Supertag Sync Auto Star or Stop
;;;------------------------------------------------------------------
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

  ;; Ensure store is initialized before starting auto-sync
  (unless (hash-table-p supertag--store)
    (setq supertag--store (ht-create)))
  ;; Start new timer with safety wrapper (fixed interval, not idle)
  (setq supertag-sync--timer
        (run-with-timer
         2 ; Start first sync after a short 2-second delay
         (or interval supertag-sync-auto-interval) ; Then, repeat at the configured interval
         (lambda ()
            "Safe wrapper for sync function with error handling."
            (when (fboundp 'supertag-sync--check-and-sync)
              (supertag-sync--check-and-sync))))))

(defun supertag-sync-stop-auto-sync ()
  "Stop automatic synchronization."
  (interactive)
  (when supertag-sync--timer
    (cancel-timer supertag-sync--timer)
    (setq supertag-sync--timer nil)
    (message "Auto-sync stopped")))

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
  "Hook function to run single-node sync after saving a buffer."
  ;; We only run this for org-mode buffers that are part of the sync scope.
  (when (and (derived-mode-p 'org-mode) ; Remove debug message
             (buffer-file-name)
             (supertag-sync--in-sync-scope-p (buffer-file-name)))
    ;; Create a dummy counters plist, as the function expects one.
    (let ((counters '(:nodes-created 0 :nodes-updated 0 :nodes-deleted 0 :references-created 0 :references-deleted 0)))
      ;; Process the file within a transaction for atomicity
      (supertag-with-transaction
        (supertag-sync--process-single-file (buffer-file-name) counters))
      ;; Immediately run garbage collection after processing
      (let ((deleted-count (supertag-sync-garbage-collect-orphaned-nodes))
            (refs-created (or (plist-get counters :references-created) 0))
            (refs-deleted (or (plist-get counters :references-deleted) 0)))
        (message "Real-time sync for %s: %d created, %d updated, %d marked, %d purged, %d refs (+%d/-%d)."
                 (file-name-nondirectory (buffer-file-name))
                 (plist-get counters :nodes-created)
                 (plist-get counters :nodes-updated)
                 (plist-get counters :nodes-deleted)
                 deleted-count
                 (+ refs-created refs-deleted)
                 refs-created
                 refs-deleted)))))

(defun supertag-sync-setup-realtime-hooks ()
  "Add hooks for real-time node synchronization."
  (add-hook 'after-save-hook #'supertag-sync--run-on-save nil t))

(defun supertag--parse-node-at-point ()
  "Parse the Org heading at point and return its property list."
  (when (org-at-heading-p)
    (let ((headline (org-element-at-point)))
      (supertag--convert-element-to-node-plist headline (buffer-file-name)))))
      
(provide 'supertag-services-sync)
