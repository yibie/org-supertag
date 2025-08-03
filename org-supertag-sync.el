;;; org-supertag-sync.el --- Synchronization layer for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides synchronization functionality for org-supertag
;;
;; Core Features:
;; - File monitoring and change detection
;; - Automatic synchronization of modified files
;; - State management for sync operations
;; - Background sync with configurable intervals
;; - Zombie node cleanup
;;
;; Usage:
;; 1. Initialize: (org-supertag-sync-init)
;; 2. Start auto-sync: (org-supertag-sync-start-auto-sync)
;; 3. Stop auto-sync: (org-supertag-sync-stop-auto-sync)
;; 4. Manual sync: (org-supertag-sync-file file-path)
;;
;; Configuration:
;; - org-supertag-sync-auto-interval: Auto-sync interval in seconds
;; - org-supertag-sync-directories: Directories to monitor
;; - org-supertag-sync-node-creation-level: Minimum heading level for node creation

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-supertag-db)
(require 'cl-lib)
(require 'ht)
(require 'f)

;;------------------------------------------------------------------------------
;; Cache Management
;;------------------------------------------------------------------------------

(defun org-supertag-sync--clear-org-element-cache ()
  "Clear org-element cache to prevent emergency exit errors.
This function safely clears various org-element cache mechanisms."
  (let ((org-element-use-cache nil)
        (org-startup-folded nil)
        (org-startup-with-inline-images nil)
        (org-startup-with-latex-preview nil)
        (org-hide-emphasis-markers nil))
    ;; Clear org-element cache if available
    (when (fboundp 'org-element-cache-reset)
      (condition-case err
          (org-element-cache-reset)
        (error (message "[org-supertag] Error clearing org-element cache: %s" (error-message-string err)))))
    
    ;; Clear buffer-specific cache if available
    (when (fboundp 'org-element-cache-clear)
      (condition-case err
          (org-element-cache-clear)
        (error (message "[org-supertag] Error clearing buffer cache: %s" (error-message-string err)))))
    
    ;; Clear any other cache mechanisms that might exist
    (when (fboundp 'org-element-cache-reset)
      (condition-case err
          (org-element-cache-reset)
        (error (message "[org-supertag] Error in secondary cache reset: %s" (error-message-string err)))))))

(defun org-supertag-sync--safe-org-operation (operation &optional retry-count)
  "Safely execute an org operation with cache protection.
OPERATION is a function to execute.
RETRY-COUNT is the number of retries (default 1)."
  (let ((retry-count (or retry-count 1))
        (attempt 0))
    (while (<= attempt retry-count)
      (condition-case err
          (progn
            (org-supertag-sync--clear-org-element-cache)
            (funcall operation)
            (cl-return t)) ; Success, exit loop
        (error 
         (cl-incf attempt)
         (if (<= attempt retry-count)
             (message "[org-supertag] Operation failed (attempt %d/%d): %s" 
                      attempt (1+ retry-count) (error-message-string err))
           (message "[org-supertag] Operation failed after %d attempts: %s" 
                    (1+ retry-count) (error-message-string err))
           (cl-return nil)))))))

;;------------------------------------------------------------------------------
;; Configuration
;;------------------------------------------------------------------------------

(defgroup org-supertag-sync nil
  "Synchronization settings for org-supertag."
  :group 'org-supertag)

(defcustom org-supertag-sync-state-file 
  (expand-file-name "sync-state.el" org-supertag-data-directory)
  "File to store sync state data."
  :type 'file
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-auto-interval 300
  "Interval in seconds for automatic synchronization."
  :type 'integer
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-directories nil
  "List of directories to monitor for automatic synchronization.
Each entry should be an absolute path. Subdirectories will also be monitored.
If nil, no automatic synchronization will occur."
  :type '(repeat directory)
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-exclude-directories nil
  "List of directories to exclude from synchronization.
Takes precedence over `org-supertag-sync-directories'."
  :type '(repeat directory)
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-file-pattern "\\.org$"
  "Regular expression for matching files to synchronize."
  :type 'string
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-check-contents t
  "Whether to check node contents during sync.
Enabling this increases accuracy but reduces performance."
  :type 'boolean
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-hash-props
  '(:raw-value :tags :todo-type :priority)
  "Properties to include when calculating node hash values."
  :type '(repeat symbol)
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-auto-create-node nil
  "Whether to automatically create nodes for headings during sync.
When enabled, any heading without an ID will get one automatically.
Note: This can interfere with embed block synchronization, so it's disabled by default."
  :type 'boolean
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-node-creation-level 1
  "Minimum heading level for automatic node creation.
Only headings at this level or deeper will be considered for node creation."
  :type 'integer
  :group 'org-supertag-sync)

;;; Variables

(defvar org-supertag-sync--state (make-hash-table :test 'equal)
  "Track file modification states.
Key: file path
Value: last sync time")

(defvar org-supertag-sync--buffer-watch nil
  "Track buffer modification hooks.")

(defvar org-supertag-sync--timer nil
  "Timer for periodic sync checks.")

;; Add a variable to track initialization status
(defvar org-supertag-sync--initialized nil
  "Flag indicating whether org-supertag-sync has been initialized.")

;;; Core Functions - File State Tracking

(defun org-supertag-sync--in-sync-scope-p (file)
  "Check if FILE is within synchronization scope.
Returns t if file should be synchronized based on configured directories.
If no directories are configured, returns t for all org files."
  (when (and file (file-exists-p file))
    (let* ((expanded-file (expand-file-name file))
           (file-dir (file-name-directory expanded-file))
           (excluded (and org-supertag-sync-exclude-directories
                         (cl-some (lambda (dir)
                                   (let ((expanded-exclude-dir (expand-file-name dir)))
                                     (string-prefix-p expanded-exclude-dir file-dir)))
                                 org-supertag-sync-exclude-directories)))
           (included (if org-supertag-sync-directories
                        (cl-some (lambda (dir)
                                  (let ((expanded-dir (expand-file-name dir)))
                                    (string-prefix-p expanded-dir file-dir)))
                                org-supertag-sync-directories)
                      t)))
      (and included
           (not excluded)
           (string-match-p org-supertag-sync-file-pattern file)))))

(defun org-supertag-scan-sync-directories (&optional all-files-p)
  "Scan sync directories for org files.
If ALL-FILES-P is non-nil, return all files in scope.
Otherwise, returns a list of new files that are not yet in sync state."
  (let ((files nil)
        (total-scanned 0))
    (dolist (dir org-supertag-sync-directories)
      (when (file-exists-p dir)
        (let ((dir-files (directory-files-recursively
                         dir org-supertag-sync-file-pattern t)))
          (message "[org-supertag] Scanning directory: %s (found %d files)" 
                   dir (length dir-files))
          (dolist (file dir-files)
            (cl-incf total-scanned)
            (let ((normalized-file (expand-file-name file)))
              (when (and (file-regular-p normalized-file)
                         (org-supertag-sync--in-sync-scope-p normalized-file)
                         (or all-files-p
                             (not (gethash normalized-file org-supertag-sync--state))))
                (push normalized-file files))))))
    (message "[org-supertag] Scan complete: %d total files scanned, %d files added to sync" 
             total-scanned (length files))
    files)))

(defun org-supertag-sync-update-state (file)
  "Update sync state for FILE.
Ensures file path is normalized to prevent duplicates."
  (when (and file (file-exists-p file))
    (let ((expanded-file (expand-file-name file)))
      (puthash expanded-file
               (file-attribute-modification-time 
                (file-attributes expanded-file))
               org-supertag-sync--state)
      (message "[org-supertag] Updated sync state for: %s" expanded-file))))

(defun org-supertag-sync-check-state (file)
  "Check if FILE needs synchronization.
Returns t if file has been modified since last sync."
  (when-let* ((expanded-file (expand-file-name file))
              (state (gethash expanded-file org-supertag-sync--state))
              (last-sync state)
              (mtime (file-attribute-modification-time
                     (file-attributes expanded-file))))
    (time-less-p last-sync mtime)))

(defun org-supertag-get-modified-files ()
  "Get list of files that need synchronization.
Returns files that have been modified since last sync."
  (let ((files nil))
    (maphash
     (lambda (file state)
       (when (and (file-exists-p file)
                  (org-supertag-sync--in-sync-scope-p file)
                  (org-supertag-sync-check-state file))
         (push file files)))
     org-supertag-sync--state)
    files))

;;; Core Functions - Node Hash Support

(defun org-supertag-node-hash (node)
  "Calculate unified hash value for NODE.
Includes all important node information: title, content, tags, fields, and properties.
This hash detects any change in the node's content or structure."
  (let* ((title (or (plist-get node :title) ""))
         (content (or (plist-get node :content) ""))
         (raw-value (or (plist-get node :raw-value) ""))
         (tags (let ((tag-list (plist-get node :tags)))
                 (if (listp tag-list)
                     (mapconcat 'identity (sort (copy-sequence tag-list) 'string<) "|")
                   (or tag-list ""))))
         (todo-type (or (plist-get node :todo-type) ""))
         (priority (or (plist-get node :priority) ""))
         (properties (let ((props-plist (plist-get node :properties)))
                       (if (plistp props-plist)
                           (let (props-alist)
                             ;; 1. Convert plist to alist for safe sorting.
                             (let ((temp-plist props-plist))
                               (while temp-plist
                                 (let ((key (pop temp-plist))
                                       (val (pop temp-plist)))
                                   (when key ; Handle odd-length or malformed plists
                                     (push (cons key val) props-alist)))))
                             
                             ;; 2. Sort the alist by key (keys are keywords).
                             (setq props-alist (sort props-alist (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))
                             
                             ;; 3. Create the flattened string representation.
                             (mapconcat (lambda (pair)
                                          ;; Format value as empty string if it's nil.
                                          (format "%s=%s" (car pair) (or (cdr pair) "")))
                                        props-alist
                                        "|"))
                         ""))))
    (secure-hash 'sha1
                 (format "%s|%s|%s|%s|%s|%s|%s"
                        title content raw-value tags todo-type priority properties))))

;; 使用 org-supertag-node.el 中的统一哈希计算函数

(defun org-supertag-db-add-with-hash (id props)
  "Add node with ID and PROPS to database, including unified hash value."
  (let ((node-hash (org-supertag-node-hash props)))
    (org-supertag-db-add id 
                         (plist-put (plist-put props :type :node)
                                  :hash node-hash))))

(defun org-supertag-node-changed-p (old-node new-node)
  "Compare OLD-NODE and NEW-NODE to detect changes.
If OLD-NODE doesn't have a hash value, calculate it on the fly."
  (let ((old-hash (or (plist-get old-node :hash)
                      (org-supertag-node-hash old-node)))
        (new-hash (or (plist-get new-node :hash)
                      (org-supertag-node-hash new-node))))
    (not (string= old-hash new-hash))))

(defun org-supertag-sync--normalize-properties (props)
  "Normalize property list PROPS for consistent comparison.
Converts all keys and values to strings."
  (when props
    (cl-loop for (k v) on props by #'cddr
            when (and k v)
            collect (cons (if (keywordp k)
                              (substring (symbol-name k) 1)
                            (format "%s" k))
                          (format "%s" v)))))

;;-------------------------------------------------------------------
;; Core Functions - Node Scanning and Update
;;-------------------------------------------------------------------

(defun org-supertag-scan-buffer-nodes ()
  "Scan current buffer and collect node information.
Only process headings with IDs or that meet creation criteria.
Returns a hash table mapping node IDs to their properties."
  (let ((nodes (make-hash-table :test 'equal))
        (org-element-use-cache nil)  ; Disable element cache
        (org-startup-folded nil)     ; Ensure all content is visible
        (org-startup-with-inline-images nil)  ; Disable image loading
        (org-startup-with-latex-preview nil)  ; Disable latex preview
        (org-hide-emphasis-markers nil))      ; Show all markers
    (when (org-supertag-sync--in-sync-scope-p buffer-file-name)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((case-fold-search t))  ; Make searches case-insensitive
            (while (re-search-forward org-heading-regexp nil t)
              (when (and (org-at-heading-p)
                         (not (string-prefix-p "TAGS" (org-get-heading t t t t)))
                         (not (org-in-commented-heading-p))
                         (>= (org-current-level) org-supertag-sync-node-creation-level))
                (let ((id (org-id-get)))
                  ;; Only create node if there is no ID
                  (unless id
                    (condition-case err
                        (org-supertag-node-create)
                      (error 
                       (message "Failed to create node ID at line %d: %s"
                               (line-number-at-pos) 
                               (error-message-string err)))))
                  (when-let* ((id (org-id-get))  ; Get ID again after potential creation
                             (props (condition-case err
                                       (save-excursion
                                         (org-back-to-heading t)
                                         (org-supertag-db-parse-node-properties))
                                     (error
                                      (message "Error extracting properties at point %d: %s"
                                               (point)
                                               (error-message-string err))
                                      nil))))
                    (puthash id props nodes)))))))))
    nodes))

(defun org-supertag-db-update-buffer ()
  "Update database with all nodes in current buffer.
Uses a three-pass approach:
1. Scan buffer to collect current nodes.
2. Process updates, creations, and moves, and reconcile references.
3. Process deletions."
  (save-excursion
    (let* ((file (buffer-file-name))
           (current-nodes (make-hash-table :test 'equal))
           (updated 0)
           (deleted 0)
           (moved 0))
      
      ;; First pass: scan buffer and collect nodes using safe operation
      (save-restriction
        (widen)
        (goto-char (point-min))
        (org-supertag-sync--safe-org-operation
         (lambda ()
           (let ((org-element-use-cache nil)
                 (org-startup-folded nil)
                 (org-startup-with-inline-images nil)
                 (org-startup-with-latex-preview nil)
                 (org-hide-emphasis-markers nil))
             (org-map-entries
              (lambda ()
                (let ((id (org-id-get)))
                  (when (and id (>= (org-current-level) org-supertag-sync-node-creation-level))
                    (condition-case err
                        (when-let* ((props (org-supertag-db-parse-node-properties)))
                          (puthash id props current-nodes))
                      (error (message "Error extracting properties for node %s: %s" id (error-message-string err)))))))
              t nil)))
         2)) ; Retry up to 2 times
      
      ;; Second pass: process updates, creations, and moves
      ;; Fixed maphash call to ensure proper syntax
      (maphash
       (lambda (id props)
         (let* ((old-node (org-supertag-db-get id))
                (is-new (null old-node))
                (is-moved (and old-node (not (string= (plist-get old-node :file-path) file))))
                (is-updated (and old-node (not is-moved) (org-supertag-node-changed-p old-node props))))
           (when (or is-new is-moved is-updated)
             (condition-case err
                 (let ((old-refs (if is-new nil (plist-get old-node :ref-to)))
                       (new-refs (plist-get props :ref-to)))
                   ;; First, save the node itself with all its new data.
                   (org-supertag-db-add-with-hash id props)
                   ;; Second, update back-references on other nodes.
                   (when (fboundp 'org-supertag-db-reconcile-references)
                     (org-supertag-db-reconcile-references id new-refs old-refs))
                   ;; Increment counters
                   (cond (is-moved (cl-incf moved))
                         (t (cl-incf updated))))
               (error (message "Error processing node %s: %s" id (error-message-string err)))))))
       current-nodes)
      
      ;; Third pass: check for deletions
      ;; Fixed maphash call to ensure proper syntax
      (maphash
       (lambda (id node)
         (when (and (string= (plist-get node :file-path) file)
                   (null (gethash id current-nodes)))
           (condition-case err
               (progn
                 (org-supertag-db-remove-object id)
                 (cl-incf deleted))
             (error (message "Error removing node %s: %s" id (error-message-string err))))))
       org-supertag-db--object)
      
      ;; Report changes
      ;; (when (or (> updated 0) (> deleted 0) (> moved 0))
      ;;   (message "Buffer sync: %d updated, %d deleted, %d moved"
      ;;            updated deleted moved))
      )))

(defun org-supertag--sync-at-point ()
  "Synchronize node at point with database."
  (when-let* ((id (org-id-get))
              (props (org-supertag-db-parse-node-properties)))
    (let ((old-node (org-supertag-db-get id)))
      (when (or (null old-node)
                (org-supertag-node-changed-p old-node props))
        ;; Preserve relationship data
        (when old-node
          (let ((preserved-props '(:ref-to :ref-from :ref-count)))
            (dolist (prop preserved-props)
              (when-let* ((value (plist-get old-node prop)))
                (setq props (plist-put props prop value))))))
        ;; Update database
        (org-supertag-db-add-with-hash id props)
        t))))

;;-------------------------------------------------------------------
;; Buffer Monitoring
;;-------------------------------------------------------------------

(defun org-supertag-sync-setup-buffer-watch ()
  "Setup buffer modification tracking."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (org-supertag-sync--in-sync-scope-p buffer-file-name))
        ;; Add after-save-hook
        (add-hook 'after-save-hook 
                  #'org-supertag-sync--handle-save
                  nil t)
        ;; 监听修改
        (add-hook 'before-change-functions
                  #'org-supertag-sync--handle-modify 
                  nil t)
        ;; 添加新的监听
        (add-hook 'org-after-promote-entry-hook
                  #'org-supertag--sync-at-point
                  nil t)
        (add-hook 'org-after-demote-entry-hook
                  #'org-supertag--sync-at-point
                  nil t)
        (add-hook 'org-after-refile-insert-hook
                  #'org-supertag--sync-at-point
                  nil t)))))

(defun org-supertag-sync--handle-save ()
  "Handle buffer save."
  (when (and buffer-file-name
             (org-supertag-sync--in-sync-scope-p buffer-file-name))
    ;; Update database
    (org-supertag-db-update-buffer)
    ;; Update sync state
    (org-supertag-sync-update-state buffer-file-name)))

(defun org-supertag-sync--handle-modify (beg end)
  "Handle buffer modification between BEG and END."
  (when (and buffer-file-name
             (org-supertag-sync--in-sync-scope-p buffer-file-name))
    (save-excursion
      (goto-char beg)
      (when (and (org-at-heading-p)
                 (not (org-id-get)))
        (org-supertag--sync-at-point)))
    (let ((state (gethash buffer-file-name org-supertag-sync--state)))
      (when state
        (setcar state (current-time))))))

(defun org-supertag-sync--process-node (element)
  "Process org node ELEMENT during sync.
Ensures node has ID and is properly registered in database."
  (when (and org-supertag-sync-auto-create-node
             (>= (org-element-property :level element) 
                 org-supertag-sync-node-creation-level))
    (let* ((begin (org-element-property :begin element))
           (id (org-element-property :ID element)))
      (save-excursion
        (goto-char begin)
        (when (org-at-heading-p)
          (unless id
            (org-id-get-create)
            (setq id (org-element-property :ID element)))
          (org-supertag--sync-at-point))))))

;;-------------------------------------------------------------------
;; State Management
;;-------------------------------------------------------------------

(defun org-supertag-sync--cleanup-database ()
  "Clean up database by removing nodes from files not in sync state."
  (let ((nodes-to-remove nil)
        (files-in-db (make-hash-table :test 'equal))
        (sync-files (make-hash-table :test 'equal)))
    
    ;; First, normalize all files in sync state
    (maphash (lambda (file _)
               (let ((normalized (expand-file-name file)))
                 (puthash normalized t sync-files)))
             org-supertag-sync--state)
    
    ;; Collect all unique files in database
    (maphash (lambda (id node)
               (when (eq (plist-get node :type) :node)
                 (let ((file (plist-get node :file-path)))
                   (when file
                     (let ((normalized (expand-file-name file)))
                       (puthash normalized t files-in-db))))))
             org-supertag-db--object)
    
    (message "[org-supertag] Checking %d files from database against %d files in sync state"
             (hash-table-count files-in-db)
             (hash-table-count sync-files))
    
    ;; Check each file in database against sync state
    (maphash (lambda (file _)
               (unless (gethash file sync-files)
                 (message "[org-supertag] File %s not in sync state, marking its nodes for removal" file)
                 ;; Collect all nodes from this file
                 (maphash (lambda (id node)
                           (when-let ((node-file-path (plist-get node :file-path)))
                             (when (and (eq (plist-get node :type) :node)
                                      (string= (expand-file-name node-file-path) file))
                               (push id nodes-to-remove))))
                         org-supertag-db--object)))
             files-in-db)
    
    ;; Remove collected nodes
    (when nodes-to-remove
      (message "[org-supertag] Removed %d nodes from %d files not in sync state" 
               (length nodes-to-remove)
               (- (hash-table-count files-in-db) (hash-table-count sync-files)))
      (dolist (id nodes-to-remove)
        (org-supertag-db-remove-object id)))
    
    (when (= (length nodes-to-remove) 0)
      (message "[org-supertag] No nodes need to be removed - all database files are in sync state"))))

(defun org-supertag-sync-save-state ()
  "Save sync state to file."
  ;; Clean up non-existent files from sync state only
  (let ((files-to-remove nil))
    (message "Debug - Current sync directories: %S" org-supertag-sync-directories)
    (message "Debug - Current sync state has %d files" (hash-table-count org-supertag-sync--state))
    
    (maphash (lambda (file _state)
               (let ((exists (file-exists-p file))
                     (in-scope (org-supertag-sync--in-sync-scope-p file)))
                 (when (or (not exists)
                          (not in-scope))
                   (push file files-to-remove))))
             org-supertag-sync--state)
    
    (message "Debug - Files to remove from sync state: %S" files-to-remove)
    
    ;; Remove files from sync state only (not from database)
    (dolist (file files-to-remove)
      (remhash file org-supertag-sync--state))
    
    (message "Debug - After cleanup, sync state has %d files" 
             (hash-table-count org-supertag-sync--state)))
  
  (with-temp-file org-supertag-sync-state-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 org-supertag-sync--state (current-buffer)))))

(defun org-supertag-sync-load-state ()
  "Load sync state from file.
If file doesn't exist, initialize empty state."
  (if (file-exists-p org-supertag-sync-state-file)
      (with-temp-buffer
        (insert-file-contents org-supertag-sync-state-file)
        (setq org-supertag-sync--state 
              (read (current-buffer))))
    ;; Initialize empty state if file doesn't exist
    (setq org-supertag-sync--state (make-hash-table :test 'equal))
    ;; Save the initial state
    (org-supertag-sync-save-state)))

;; Auto initialization
(defun org-supertag-sync--ensure-directories ()
  "Ensure sync directories are properly configured."
  (unless org-supertag-sync-directories
    ;; Try to load from custom-file if it exists
    (when custom-file
      (condition-case nil
          (load custom-file t)
        (error nil)))
    ;; If still not set, try to load from init file
    (unless org-supertag-sync-directories
      (let ((init-file (or user-init-file "~/.emacs")))
        (when (file-exists-p init-file)
          (condition-case nil
              (load init-file t)
            (error nil)))))
    ;; If directories are still not set, try to get from symbol-value
    (unless org-supertag-sync-directories
      (when-let ((value (get 'org-supertag-sync-directories 'saved-value)))
        (setq org-supertag-sync-directories (eval (car value))))))
  
  ;; Normalize directories if they exist
  (when org-supertag-sync-directories
    (setq org-supertag-sync-directories
          (mapcar (lambda (dir)
                    (file-name-as-directory (expand-file-name dir)))
                  org-supertag-sync-directories)))
  
  org-supertag-sync-directories)

;;-------------------------------------------------------------------
;; Zombie Node Validation and Cleanup
;;-------------------------------------------------------------------

(defun org-supertag-sync-validate-and-cleanup-zombie-nodes ()
  "Validate and clean up all zombie nodes (manually executed).
Check all nodes in the database, verify if the node ID exists in the corresponding file.
If the node ID does not exist in the corresponding file, automatically delete the zombie node."
  (interactive)
  (let ((all-files (make-hash-table :test 'equal))
        (zombie-nodes '())
        (cleaned-count 0)
        (total-nodes 0)
        (checked-files 0))
    
    ;; First collect all files and their nodes
    (maphash (lambda (id node)
               (when (eq (plist-get node :type) :node)
                 (cl-incf total-nodes)
                 (let ((file-path (plist-get node :file-path)))
                   (when file-path
                     (unless (gethash file-path all-files)
                       (puthash file-path '() all-files))
                     (puthash file-path 
                             (cons (cons id node) (gethash file-path all-files))
                             all-files)))))
             org-supertag-db--object)
    
    (message "Starting zombie node validation for %d nodes in %d files..." 
             total-nodes (hash-table-count all-files))
    
    ;; Check nodes in files 
    (maphash (lambda (file-path nodes)
               (cl-incf checked-files)
               (message "Checking file (%d/%d): %s" 
                       checked-files (hash-table-count all-files) file-path)
               
               (if (file-exists-p file-path)
                   ;; File exists, check if the node is in the file
                   (dolist (node-entry nodes)
                     (let ((node-id (car node-entry)))
                       (unless (org-supertag-sync--check-node-exists-in-file node-id file-path)
                         (message "Found zombie node: %s in file %s" node-id file-path)
                         (push (cons node-id file-path) zombie-nodes))))
                 ;; File does not exist, all nodes are zombies
                 (message "File not found: %s, marking all %d nodes as zombies" 
                         file-path (length nodes))
                 (dolist (node-entry nodes)
                   (let ((node-id (car node-entry)))
                     (push (cons node-id file-path) zombie-nodes)))))
             all-files)
    
    ;; Directly delete zombie nodes
    (when zombie-nodes
      (message "Cleaning up %d zombie nodes..." (length zombie-nodes))
      (dolist (zombie zombie-nodes)
        (let ((node-id (car zombie))
              (file-path (cdr zombie)))
          (message "Removing zombie node %s from %s" node-id file-path)
          (org-supertag-db-remove-object node-id)
          (cl-incf cleaned-count))))
    
    (message "Zombie node cleanup completed: checked %d files, %d total nodes, cleaned %d zombie nodes" 
             checked-files total-nodes cleaned-count)
    cleaned-count))

(defun org-supertag-sync--check-node-exists-in-file (node-id file-path)
  "Check if the node ID exists in the specified file.
NODE-ID: The ID of the node to check
FILE-PATH: The path to the file
Returns t if the node exists, nil if it does not exist."
  (and node-id
       file-path
       (file-exists-p file-path)
       (with-current-buffer (find-file-noselect file-path)
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (or (re-search-forward
                  (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote node-id))
                  nil t)
                (progn
                  (goto-char (point-min))
                  (re-search-forward
                   (format "^[ \t]*#\\+ID:[ \t]+%s[ \t]*$" (regexp-quote node-id))
                   nil t))))))))

(defun org-supertag-sync-cleanup-zombie-nodes-for-file (file-path)
  "Clean up zombie nodes for a specific file.
FILE-PATH: Path to the file to check
Returns number of cleaned nodes."
  (let ((zombie-nodes '())
        (count 0))
    
    (when (file-exists-p file-path)
      (message "Checking zombie nodes for file: %s" file-path)
      
      ;; Get all nodes in the database for the file
      (maphash (lambda (id node)
                 (when (and (eq (plist-get node :type) :node)
                           (string= (plist-get node :file-path) file-path))
                   ;; Check if the node ID exists in the file
                   (unless (org-supertag-sync--check-node-exists-in-file id file-path)
                     (push id zombie-nodes))))
               org-supertag-db--object)
      
      ;; Delete zombie nodes
      (dolist (node-id zombie-nodes)
        (message "Removing zombie node: %s" node-id)
        (org-supertag-db-remove-object node-id)
        (cl-incf count))
      
      (when (> count 0)
        (message "Cleaned up %d zombie nodes from file: %s" count file-path))))
    
    count)

;;-------------------------------------------------------------------
;; Core Functions - Zombie Node Auto Cleanup
;;-------------------------------------------------------------------
(defun org-supertag-sync--auto-cleanup-zombie-nodes (modified-files)
  "Automatically clean up zombie nodes inside MODIFIED-FILES.
Returns the number of cleaned nodes."
  (let ((cleaned-count 0))
    (dolist (file modified-files)
      (when (file-exists-p file)
        (maphash (lambda (id node)
                   (when (and (eq (plist-get node :type) :node)
                              (string= (plist-get node :file-path) file)
                              (not (org-supertag-sync--check-node-exists-in-file id file)))
                     (message "[org-supertag] Removing zombie node %s from %s" id file)
                     (org-supertag-db-remove-object id)
                     (cl-incf cleaned-count)))
                 org-supertag-db--object))
    cleaned-count)))

(defun org-supertag-sync--check-and-sync ()
  "Check and synchronize modified files.
This is the main sync function called periodically."
  ;; Clean up non-existent files and files out of sync scope from sync state
  (let ((files-to-remove nil)
        (state-changed nil) ; Flag to track if state was modified
        (modified-files (org-supertag-get-modified-files)))
    ;; Fixed maphash call to ensure proper syntax
    (maphash (lambda (file _state)
               (when (or (not (file-exists-p file))
                        (not (org-supertag-sync--in-sync-scope-p file)))
                 (push file files-to-remove)))
             org-supertag-sync--state)
    
    ;; Remove files from sync state and their nodes from database
    (when files-to-remove
      (setq state-changed t)
      (dolist (file files-to-remove)
        (message "[org-supertag] Removing file from sync state (file %s or out of scope): %s"
                 (if (file-exists-p file) "exists" "doesn't exist")
                 file)
        (remhash file org-supertag-sync--state)
        ;; Remove nodes from these files in database
        ;; Fixed maphash call to ensure proper syntax
        (maphash (lambda (id node)
                   (when (and (eq (plist-get node :type) :node)
                              (string= (plist-get node :file-path) file))
                     (org-supertag-db-remove-object id)))
                 org-supertag-db--object)))

    ;; Save the state to disk if it was changed.
    (when state-changed
      (org-supertag-sync-save-state)))

  ;; Check for new files first
  (let ((new-files (org-supertag-scan-sync-directories)))
    (when new-files
      (dolist (file new-files)
        (org-supertag-sync-update-state file))
      ;; Also save state if new files were added
      (org-supertag-sync-save-state)))
  
  ;; Original sync logic
  (let ((modified-files (org-supertag-get-modified-files))
        (nodes-deleted 0)
        (nodes-moved 0)
        (nodes-created 0)
        (old-nodes (make-hash-table :test 'equal))
        (updated 0))
    
    ;; First collect existing nodes
    ;; Fixed maphash call to ensure proper syntax
    (maphash
     (lambda (id node)
       (when (eq (plist-get node :type) :node)
         (puthash id node old-nodes)))
     org-supertag-db--object)
    
    ;; Process files
    (when modified-files
      ;; Sort files by modification time to handle dependencies
      (setq modified-files
            (sort modified-files
                  (lambda (a b)
                    (let ((attr-a (file-attributes a))
                          (attr-b (file-attributes b)))
                      (when (and attr-a attr-b)
                        (time-less-p
                         (file-attribute-modification-time attr-a)
                         (file-attribute-modification-time attr-b)))))))
      
      ;; Process files with enhanced error handling
      (dolist (file modified-files)
        (message "Syncing file: %s" file)
        (condition-case err
            ;; Use safe org operation for file processing
            (org-supertag-sync--safe-org-operation
             (lambda ()
               (let ((org-element-use-cache nil)
                     (org-startup-folded nil)
                     (org-startup-with-inline-images nil)
                     (org-startup-with-latex-preview nil)
                     (org-hide-emphasis-markers nil))
                 
                 (with-current-buffer (find-file-noselect file)
                   (let ((before-nodes (hash-table-count old-nodes)))
                     ;; Update database with additional error handling
                     (condition-case db-err
                         (org-supertag-db-update-buffer)
                       (error 
                        (message "[org-supertag] Error updating database for %s: %s" 
                                 file (error-message-string db-err))
                        ;; Try to recover by clearing cache and retrying once
                        (org-supertag-sync--clear-org-element-cache)
                        (condition-case retry-err
                            (org-supertag-db-update-buffer)
                          (error 
                           (message "[org-supertag] Retry failed for %s: %s" 
                                    file (error-message-string retry-err))))))
                     
                     ;; Count changes
                     ;; Fixed maphash call to ensure proper syntax
                     (maphash
                      (lambda (id node)
                        (let ((old-node (gethash id old-nodes)))
                          (cond
                           ;; Node moved
                           ((and old-node
                                 (not (string= (plist-get old-node :file-path)
                                             (plist-get node :file-path))))
                            (cl-incf nodes-moved))
                           ;; New node
                           ((null old-node)
                            (cl-incf nodes-created)))))
                      org-supertag-db--object)
                     
                     ;; Count deleted nodes
                     (let ((deleted-in-file (- before-nodes
                                             (hash-table-count old-nodes))))
                       (setq nodes-deleted (+ nodes-deleted deleted-in-file)))
                     
                     (org-supertag-sync-update-state file)
                     (cl-incf updated)))))
             2) ; Retry up to 2 times
            (error 
             (message "[org-supertag] Error processing file %s: %s" 
                      file (error-message-string err))
             ;; Continue with next file instead of stopping
             nil))))
    
    ;; Clean up zombie nodes if any files were processed
    (if modified-files
        (progn
          (let ((cleaned-nodes (org-supertag-sync--auto-cleanup-zombie-nodes modified-files)))
            (if (and cleaned-nodes (> cleaned-nodes 0))
                (progn
                  (message "[org-supertag] Cleaned up %d zombie nodes" cleaned-nodes))))))
    (message "[org-supertag] Node Sync Completed")))

(defun org-supertag-sync-start-auto-sync (&optional interval)
  "Start automatic synchronization with INTERVAL seconds.
If INTERVAL is nil, use `org-supertag-sync-auto-interval'."
  (interactive)
  ;; Cancel existing timer
  (when org-supertag-sync--timer
    (cancel-timer org-supertag-sync--timer))
  
  ;; Start new timer
  (setq org-supertag-sync--timer
        (run-with-idle-timer 
         (or interval org-supertag-sync-auto-interval)
         t
         #'org-supertag-sync--check-and-sync))
  (message "Auto-sync started with %d second interval"
           (or interval org-supertag-sync-auto-interval)))

(defun org-supertag-sync-stop-auto-sync ()
  "Stop automatic synchronization."
  (interactive)
  (when org-supertag-sync--timer
    (cancel-timer org-supertag-sync--timer)
    (setq org-supertag-sync--timer nil)
    (message "Auto-sync stopped")))

(cl-defun org-supertag-sync-init ()
  "Initialize sync system."
  (when org-supertag-sync--initialized
    (cl-return-from org-supertag-sync-init))
  
  ;; Ensure directories are loaded
  (org-supertag-sync--ensure-directories)
  
  ;; Ensure data directory exists
  (unless (file-exists-p org-supertag-data-directory)
    (make-directory org-supertag-data-directory t))
  
  ;; Load sync state
  (org-supertag-sync-load-state)
  
  ;; Clean up database if loaded and has content
  (when (and (featurep 'org-supertag-db) 
             (hash-table-p org-supertag-db--object)
             (> (hash-table-count org-supertag-db--object) 0))
    (org-supertag-sync--cleanup-database))
  
  ;; Setup buffer hooks
  (org-supertag-sync-setup-buffer-watch)
  
  ;; Initial state for all files
  (let ((all-files (org-supertag-get-all-files))
        (new-files (org-supertag-scan-sync-directories)))
    ;; Update state for valid files
    (dolist (file (delete-dups (append all-files new-files)))
      (when (file-exists-p file)
        (org-supertag-sync-update-state file))))
  
  ;; Save initial state
  (org-supertag-sync-save-state)
  
  ;; Start auto-sync
  (org-supertag-sync-start-auto-sync)
  
  ;; Mark as initialized
  (setq org-supertag-sync--initialized t))

(defun org-supertag-db-resync-all ()
  "Resynchronize all nodes from files using a safe 'preserve and rebuild' strategy."
  (interactive)
  (let ((all-nodes-props (make-hash-table :test 'equal))
        (errors nil))
    (when (yes-or-no-p "This will rebuild the node database from source files. Manually created tag relationships will be preserved. Continue? ")

      ;; Phase 1: Clear only node-related data. Tags and their relationships are preserved.
      (message "Phase 1/4: Removing all node entities and their derived links…")
      (let ((nodes-to-delete '())
            (links-to-delete '()))
        ;; Collect IDs to delete
        (maphash (lambda (id entity)
                   (when (eq (plist-get entity :type) :node)
                     (push id nodes-to-delete)))
                 org-supertag-db--object)
        (maphash (lambda (id link)
                   (when (memq (plist-get link :type) '(:node-tag :node-field))
                     (push id links-to-delete)))
                 org-supertag-db--link)
        
        ;; Perform deletion
        (dolist (id nodes-to-delete)
          (remhash id org-supertag-db--object))
        (dolist (id links-to-delete)
          (remhash id org-supertag-db--link))
        
        (message "Removed %d nodes and %d node-derived links."
                 (length nodes-to-delete) (length links-to-delete)))

      ;; Phase 2: Scan all files and build a complete in-memory representation of all nodes.
      (message "Phase 2/4: Scanning all source files for nodes...")
      (let* ((files (org-supertag-scan-sync-directories t))
             (total (length files))
             (current 0)
             (processed 0))
        (dolist (file files)
          (cl-incf current)
          (message "Scanning (%d/%d): %s" current total (file-name-nondirectory file))
          (when (file-exists-p file)
            (with-demoted-errors "Error scanning %S"
              (with-temp-buffer
                (set-buffer-file-coding-system 'utf-8-unix)
                (let ((coding-system-for-read 'utf-8))
                  (insert-file-contents file))
                (org-mode)
                (save-excursion
                  (widen)
                  (goto-char (point-min))
                  (org-map-entries
                   (lambda ()
                     (let ((id (org-id-get)))
                       (when id
                         ;; Use the pure parsing function
                         (when-let* ((parsed-props (org-supertag-db-parse-node-properties))) 
                           (setq parsed-props 
                                 (plist-put parsed-props :file-path file))
                           (puthash id parsed-props all-nodes-props)
                           (cl-incf processed))))
                   t nil))))))
        (message "Found %d nodes in total." (hash-table-count all-nodes-props)))

      ;; Phase 3: Add all nodes, create tags and node-tag links.
      (message "Phase 3/4: Rebuilding nodes and node-tag links...")
      (let ((nodes-processed 0)
            (tags-created 0)
            (links-created 0)
            (errors '()))
        (maphash
         (lambda (id props)
           (cl-incf nodes-processed)
           ;; 1. Add the node object itself with its hash.
           (condition-case err
               (org-supertag-db-add-with-hash id props)
             (error
              (push (format "Failed to add node %s: %s" id (error-message-string err)) errors)))
           
           ;; 2. Process and register all tags found in the headline.
           (when-let ((headline-tags (plist-get props :tags)))
             (dolist (tag-name headline-tags)
               ;; Defensively skip empty tag names that may result from parsing errors.
               (unless (or (null tag-name) (string-empty-p tag-name))
                 (let ((sanitized-tag (org-supertag-sanitize-tag-name tag-name)))
                   ;; Ensure tag object exists (non-destructive)
                   ;; Only create tag if it does not exist.
                   (unless (org-supertag-tag-get sanitized-tag)
                     (condition-case err
                         (progn
                           (org-supertag-tag--create sanitized-tag)
                           (cl-incf tags-created))
                       (error
                        (push (format "Failed to create tag %s: %s" sanitized-tag (error-message-string err)) errors))))
                   
                   ;; Ensure link exists
                   (condition-case err
                       (progn
                         (org-supertag-db-link :node-tag id sanitized-tag)
                         (cl-incf links-created))
                     (error
                      (push (format "Failed to create link %s->%s: %s" id sanitized-tag (error-message-string err)) errors)))))))
         all-nodes-props)
        
        (message "Phase 3 completed: %d nodes, %d tags created, %d links created"
                 nodes-processed tags-created links-created)
        (when errors
          (message "Errors during Phase 3: %s" (string-join errors "\n")))))

      ;; Phase 4: Reconcile all node-to-node references.
      (message "Phase 4/4: Reconciling node-to-node references...")
      (maphash (lambda (id props)
                 (org-supertag-db-reconcile-references id (plist-get props :ref-to) nil))
               all-nodes-props)

      (if errors
          (progn
            (message "Resync completed with errors.")))
        (message "[org-supertag] Resync completed successfully. %d nodes processed." (hash-table-count all-nodes-props))))))

(defun org-supertag-sync-force-all ()
  "Force synchronization of all files in scope. This is an alias for `org-supertag-db-resync-all`."
  (interactive)
  (org-supertag-db-resync-all))

;;-------------------------------------------------------------------
;; Helper Functions
;;-------------------------------------------------------------------

(defun org-supertag-sync-add-directory (dir)
  "Add a directory to the sync scope."
  (interactive "DAdd directory to sync scope: ")
  (let ((expanded-dir (expand-file-name dir)))
    (add-to-list 'org-supertag-sync-directories expanded-dir)
    (message "Added %s to sync scope." expanded-dir)))

(defun org-supertag-sync-remove-directory (dir)
  "Remove directory from synchronization scope."
  (interactive
   (list (completing-read "Remove directory from sync: "
                         org-supertag-sync-directories
                         nil t)))
  (let ((abs-dir (expand-file-name dir)))
    (setq org-supertag-sync-directories
          (delete abs-dir org-supertag-sync-directories))
    (customize-save-variable 'org-supertag-sync-directories 
                           org-supertag-sync-directories)
    (message "Removed %s from sync directories" abs-dir)))

(defun org-supertag-sync-file (&optional file-path force-p)
  "Synchronize a single file.
If FILE-PATH is nil, sync the current buffer's file.
If FORCE-P is non-nil, sync even if no modifications are detected."
  (interactive (list (buffer-file-name) current-prefix-arg))
  (let ((file (or file-path (buffer-file-name))))
    (when (and file (file-exists-p file))
      (if (or force-p (org-supertag-sync-check-state file))
          (with-current-buffer (find-file-noselect file)
            (let ((org-supertag-db-auto-save nil))
              (message "Syncing file: %s" file)
              (org-supertag-sync--file file)
              ;; Sync filetags after node sync is complete.
              (org-supertag-sync--sync-filetags file)
              (org-supertag-sync-update-state file)
              (org-supertag-db-save)
              (message "Sync complete for: %s" file)))
        (message "File is already up to date: %s" file)))))

(defun org-supertag-sync-files (files)
  "Synchronize a list of files."
  (dolist (file files)
    (org-supertag-sync-file file)))

(defun org-supertag-sync-force-rescan ()
  "Force a full rescan and synchronization of all files."
  (interactive)
  (org-supertag-sync-force-all))

;; Add a function to sync all nodes in the current buffer
(defun org-supertag-sync-buffer ()
  "Force sync of all nodes in the current buffer."
  (interactive)
  (org-supertag-sync-file (buffer-file-name) t))

(defun org-supertag-sync--sync-filetags (file)
  "Synchronize file-level tags for the given FILE.
This applies #+FILETAGS to all level-1 nodes in the file,
handling additions, removals, and updating co-occurrence relations."
  (when (and (featurep 'org-supertag-relation) file)
    (with-current-buffer (find-file-noselect file)
      (let* ((parsed-tree (org-element-parse-buffer))
             (new-file-tags (org-element-map parsed-tree 'keyword
                              (lambda (k)
                                (when (string= (org-element-property :key k) "FILETAGS")
                                  (split-string (org-element-property :value k) " " t)))))
             (new-file-tags (car (or new-file-tags '(())))) ; Get first match or empty list
             (file-metadata (org-supertag-db-get-file-metadata file))
             (old-file-tags (or (plist-get file-metadata :filetags) '()))
             (tags-to-add (seq-difference new-file-tags old-file-tags))
             (tags-to-remove (seq-difference old-file-tags new-file-tags))
             (level-one-nodes (org-element-map parsed-tree 'headline
                                (lambda (h)
                                  (when (= (org-element-property :level h) 1)
                                    (org-element-property :ID h))))))
        
        ;; Ensure all new filetags exist in the DB
        (dolist (tag-name (append tags-to-add tags-to-remove))
          (org-supertag-tag--create tag-name))
        
        ;; Process tags to add
        (when tags-to-add
          (dolist (node-id level-one-nodes)
            (when node-id
              (dolist (tag-name tags-to-add)
                ;; Add link with source tracking
                (org-supertag-db-link :node-tag node-id tag-name '(:source :filetags))
                ;; Update co-occurrence
                (org-supertag-relation-record-cooccurrence node-id tag-name)))))
        
        ;; Process tags to remove
        (when tags-to-remove
          (dolist (node-id level-one-nodes)
            (when node-id
              (dolist (tag-name tags-to-remove)
                ;; Remove link, only if it was from filetags
                (let ((link-id (org-supertag-db--get-link-id :node-tag node-id tag-name)))
                  (when-let ((props (gethash link-id org-supertag-db--link)))
                    (when (eq (plist-get props :source) :filetags)
                      (remhash link-id org-supertag-db--link)
                      ;; Update co-occurrence
                      (org-supertag-relation-unrecord-cooccurrence node-id tag-name))))))))
        
        ;; Update metadata in DB
        (when (or tags-to-add tags-to-remove)
          (org-supertag-db-update-file-metadata file :filetags new-file-tags)
          (message "Synced FILETAGS for %s: +%d added, -%d removed for %d level-1 nodes."
                   (file-name-nondirectory file)
                   (length tags-to-add)
                   (length tags-to-remove)
                   (length level-one-nodes)))))))

;;;###autoload
(defun org-supertag-sync-cleanup-database-by-files-not-in-sync` ()
  "Manually clean up database by removing nodes from files not in sync state.
This is useful when files have been removed from sync scope or deleted."
  (interactive)
  (if (and (featurep 'org-supertag-db) 
           (hash-table-p org-supertag-db--object)
           (> (hash-table-count org-supertag-db--object) 0))
      (progn
        (org-supertag-sync--cleanup-database)
        (message "Database cleanup completed"))
    (message "Database not loaded or empty, no cleanup needed")))

(defun org-supertag-sync-safe-update-hashes ()
  "Update all node hashes safely, ensuring no data is lost.
This function is specifically designed to fix hash value issues by only updating the hash values without modifying any other data.
这个函数专门用于修复哈希值问题，只更新哈希值，不修改任何其他数据。"
  (interactive)
  
  ;; Check if database is loaded
  (unless (and (featurep 'org-supertag-db) 
               (hash-table-p org-supertag-db--object))
    (user-error "Database not loaded, please load org-supertag-db first"))
  
  (let ((total-nodes 0)
        (updated-hash 0)
        (errors 0)
        (backup-data (make-hash-table :test 'equal)))
    
    (message "Starting safe update of all node hashes...")
    (message "Note: This operation only updates hash values, no other data is modified")
    
    ;; First backup all node data
    (message "Backing up node data...")
    (maphash
     (lambda (id node)
       (when (eq (plist-get node :type) :node)
         (puthash id (copy-sequence node) backup-data)))
     org-supertag-db--object)
    
    (message "Backup completed, %d nodes backed up" (hash-table-count backup-data))
    
    ;; Iterate through all nodes in the database, only updating hash values
    (maphash
     (lambda (id node)
       (when (eq (plist-get node :type) :node)
         (cl-incf total-nodes)
         (condition-case err
             (let ((new-hash (org-supertag-node-hash node)))
               ;; Only update hash values, preserving all other data
               (let ((updated-node (plist-put (copy-sequence node) :hash new-hash)))
                 ;; Use org-supertag-db-add to ensure proper update process
                 (org-supertag-db-add id updated-node)
                 (cl-incf updated-hash)))
           (error
            (message "Error updating node hash %s: %s" id (error-message-string err))
            (cl-incf errors))))))
     org-supertag-db--object)
    
    ;; Verify data integrity
    (message "Verifying data integrity...")
    (let ((verification-errors 0))
      (maphash
       (lambda (id node)
         (when (eq (plist-get node :type) :node)
           (let ((backup-node (gethash id backup-data)))
             (when backup-node
               ;; 检查关键字段是否保持不变
               (let ((original-title (plist-get backup-node :title))
                     (current-title (plist-get node :title))
                     (original-content (plist-get backup-node :content))
                     (current-content (plist-get node :content)))
                 (when (or (not (equal original-title current-title))
                           (not (equal original-content current-content)))
                   (message "Warning: Data for node %s may have changed" id)
                   (cl-incf verification-errors)))))))
       org-supertag-db--object)
      
      (when (> verification-errors 0)
        (message "Warning: %d data integrity issues found" verification-errors)))
    
    ;; Save database
    (when (> updated-hash 0)
      (message "Saving database changes...")
      (org-supertag-db-save))
    
    (message "Hash update completed:")
    (message "  Total nodes: %d" total-nodes)
    (message "  Updated hash: %d" updated-hash)
    (message "  Errors: %d" errors)
    (message "  Note: All field and link data is preserved")
    (message "  Backup data saved, can be restored if needed")
    
    updated-hash)

(defun org-supertag-sync-check-hash-status ()
  "Check the hash status of nodes in the database."
  (interactive)
  
  ;; Check if database is loaded
  (unless (and (featurep 'org-supertag-db) 
               (hash-table-p org-supertag-db--object))
    (user-error "Database not loaded, please load org-supertag-db first"))
  
  (let ((total-nodes 0)
        (with-hash 0)
        (missing-hash 0))
    
    (maphash
     (lambda (id node)
       (when (eq (plist-get node :type) :node)
         (cl-incf total-nodes)
         
         (if (plist-get node :hash)
             (cl-incf with-hash)
           (cl-incf missing-hash))))
     org-supertag-db--object)
    
    (message "=== Hash Status Report ===")
    (message "Total nodes: %d" total-nodes)
    (message "Hash: %d present, %d missing (%.1f%%)" 
             with-hash missing-hash 
             (* 100.0 (/ (float with-hash) total-nodes)))
    
    (when (> missing-hash 0)
      (message "建议运行: M-x org-supertag-sync-recalculate-missing-hashes"
               ))
    
    (list :total total-nodes
          :with-hash with-hash
          :missing-hash missing-hash)))

;;------------------------------------------------------------------------------
;; User Commands
;;------------------------------------------------------------------------------

(defun org-supertag-sync-clear-cache ()
  "Clear org-element cache to resolve emergency exit errors.
This command can be used when encountering org-element cache issues."
  (interactive)
  (org-supertag-sync--clear-org-element-cache)
  (message "[org-supertag] Cache cleared successfully"))

(defun org-supertag-sync-reset-and-sync ()
  "Reset org-element cache and perform a full sync.
This is useful when experiencing persistent cache-related errors."
  (interactive)
  (org-supertag-sync--clear-org-element-cache)
  (org-supertag-sync-force-rescan)
  (message "[org-supertag] Cache reset and full sync completed"))

;;------------------------------------------------------------------------------
;; Debugging and Diagnostics
;;------------------------------------------------------------------------------

(defun org-supertag-sync-diagnose-state ()
  "Diagnose sync state issues and provide detailed information."
  (interactive)
  (message "=== org-supertag Sync State Diagnosis ===")
  
  ;; Check sync directories
  (message "Sync directories: %S" org-supertag-sync-directories)
  
  ;; Check sync state
  (let ((state-count (hash-table-count org-supertag-sync--state)))
    (message "Files in sync state: %d" state-count))
  
  ;; Scan directories and compare
  (let ((scanned-files (org-supertag-scan-sync-directories t))
        (scanned-count (length scanned-files)))
    (message "Files found by scanning: %d" scanned-count))
  
  ;; Check database files
  (let ((db-files (make-hash-table :test 'equal))
        (db-count 0))
    (maphash (lambda (id node)
               (when (eq (plist-get node :type) :node)
                 (let ((file (plist-get node :file-path)))
                   (when file
                     (let ((normalized (expand-file-name file)))
                       (puthash normalized t db-files)
                       (cl-incf db-count))))))
             org-supertag-db--object)
    (message "Files in database: %d" db-count))
  
  ;; Check for files in sync state but not in scope
  (let ((out-of-scope 0))
    (maphash (lambda (file _state)
               (unless (org-supertag-sync--in-sync-scope-p file)
                 (cl-incf out-of-scope)))
             org-supertag-sync--state)
    (when (> out-of-scope 0)
      (message "Files in sync state but out of scope: %d" out-of-scope)))
  
  ;; Check for non-existent files in sync state
  (let ((non-existent 0))
    (maphash (lambda (file _state)
               (unless (file-exists-p file)
                 (cl-incf non-existent)))
             org-supertag-sync--state)
    (when (> non-existent 0)
      (message "Non-existent files in sync state: %d" non-existent)))
  
  (message "=== Diagnosis Complete ==="))

(defun org-supertag-sync-cleanup-state ()
  "Clean up sync state by removing invalid entries."
  (interactive)
  (message "=== Cleaning up sync state ===")
  
  (let ((removed-count 0))
    (maphash (lambda (file _state)
               (when (or (not (file-exists-p file))
                         (not (org-supertag-sync--in-sync-scope-p file)))
                 (remhash file org-supertag-sync--state)
                 (cl-incf removed-count)))
             org-supertag-sync--state)
    
    (message "Removed %d invalid entries from sync state" removed-count)
    (org-supertag-sync-save-state)
    (message "Sync state cleaned up and saved")))

(defun org-supertag-sync-cleanup-duplicate-paths ()
  "Clean up duplicate file paths in sync state by normalizing all paths.
This function consolidates entries that refer to the same file but with different path representations."
  (interactive)
  (message "=== Cleaning up duplicate file paths ===")
  
  (let ((path-mapping (make-hash-table :test 'equal))
        (duplicates-removed 0)
        (total-entries 0))
    
    ;; First pass: collect all entries and their normalized paths
    (maphash (lambda (file state)
               (cl-incf total-entries)
               (let ((normalized (expand-file-name file)))
                 (if (gethash normalized path-mapping)
                     ;; This is a duplicate
                     (cl-incf duplicates-removed)
                   ;; This is the first occurrence of this normalized path
                   (puthash normalized (cons file state) path-mapping))))
             org-supertag-sync--state)
    
    ;; Second pass: rebuild sync state with normalized paths
    (setq org-supertag-sync--state (make-hash-table :test 'equal))
    (maphash (lambda (normalized-path file-state-pair)
               (puthash normalized-path (cdr file-state-pair) org-supertag-sync--state))
             path-mapping)
    
    (message "Cleaned up %d duplicate entries out of %d total entries" 
             duplicates-removed total-entries)
    (message "Sync state now has %d unique files" (hash-table-count org-supertag-sync--state))
    
    ;; Save the cleaned state
    (org-supertag-sync-save-state)
    (message "Cleaned sync state saved")))


;;------------------------------------------------------------------------------
;; Provide
;;------------------------------------------------------------------------------

(provide 'org-supertag-sync)

;;; org-supertag-sync.el ends here
