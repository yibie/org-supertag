;;; org-supertag-sync.el --- File synchronization for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides file synchronization functionality for org-supertag:
;; - File state tracking using modification time
;; - Node change detection using hash values
;; - Automatic synchronization
;; - Conflict detection
;; - State persistence
;; - Error recovery

;; Configuration auto sync directories in init.el
;; (setq org-supertag-sync-auto-interval 300)

;; Configuration sync files scope in init.el
;; (setq org-supertag-sync-directories
;;       '("~/org/work"
;;         "~/org/projects"))
;;
;; If you want to exclude some directories in sync, you can set it like this:
;; (setq org-supertag-sync-exclude-directories
;;       '("~/org/work/archive"
;;         "~/org/projects/temp"))

;; Or use command to add
;; (org-supertag-sync-add-directory "~/org/personal")

;; View current configuration
;; (org-supertag-sync-list-directories)

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-supertag-db)
(require 'cl-lib)

;;; Customization

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

(defcustom org-supertag-sync-check-contents nil
  "Whether to check node contents during sync.
Enabling this increases accuracy but reduces performance."
  :type 'boolean
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-hash-props
  '(:raw-value :tags :todo-type :priority)
  "Properties to include when calculating node hash values."
  :type '(repeat symbol)
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-auto-create-node t
  "Whether to automatically create nodes for headings during sync.
When enabled, any heading without an ID will get one automatically."
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
  (let ((files nil))
    (dolist (dir org-supertag-sync-directories)
      (when (file-exists-p dir)
        (let ((dir-files (directory-files-recursively
                         dir org-supertag-sync-file-pattern t)))
          (dolist (file dir-files)
            (when (and (file-regular-p file)
                       (org-supertag-sync--in-sync-scope-p file)
                       (or all-files-p
                           (not (gethash file org-supertag-sync--state))))
              (push file files))))))
    files))

(defun org-supertag-sync-update-state (file)
  "Update sync state for FILE."
  (when (file-exists-p file)
    (puthash file
             (file-attribute-modification-time 
              (file-attributes file))
             org-supertag-sync--state)))

(defun org-supertag-sync-check-state (file)
  "Check if FILE needs synchronization.
Returns t if file has been modified since last sync."
  (when-let* ((state (gethash file org-supertag-sync--state))
              (last-sync state)
              (mtime (file-attribute-modification-time
                     (file-attributes file))))
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
  "Calculate hash value for NODE.
Only includes stable properties, excludes position information."
  (let* ((raw-value (or (plist-get node :raw-value) ""))
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
                 (format "%s|%s|%s|%s|%s"
                        raw-value tags todo-type priority properties))))

(defun org-supertag-node-content-hash (node)
  "Calculate hash value for NODE's content.
This is optional and only used when content checking is enabled."
  (when org-supertag-sync-check-contents
    (when-let* ((beg (plist-get node :contents-begin))
                (end (plist-get node :contents-end))
                (content (buffer-substring-no-properties beg end)))
      (secure-hash 'sha1 content))))

(defun org-supertag-db-add-with-hash (id props)
  "Add node with ID and PROPS to database, including hash value."
  (let ((node-hash (org-supertag-node-hash props)))
    (when-let ((content-hash (org-supertag-node-content-hash props)))
      (setq props (plist-put props :content-hash content-hash)))
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
    (or (not (string= old-hash new-hash))
        (when org-supertag-sync-check-contents
          (let ((old-content-hash (or (plist-get old-node :content-hash)
                                     (org-supertag-node-content-hash old-node)))
                (new-content-hash (org-supertag-node-content-hash new-node)))
            (and old-content-hash new-content-hash
                 (not (string= old-content-hash new-content-hash))))))))

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
      
      ;; First pass: scan buffer and collect nodes
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((org-element-use-cache nil)
              (org-startup-folded nil)
              (org-startup-with-inline-images nil)
              (org-startup-with-latex-preview nil)
              (org-hide-emphasis-markers nil))
          (condition-case err
              (org-map-entries
               (lambda ()
                 (let ((id (org-id-get)))
                   (when (and id (>= (org-current-level) org-supertag-sync-node-creation-level))
                     (condition-case err2
                         (when-let* ((props (org-supertag-db-parse-node-properties)))
                           (puthash id props current-nodes))
                       (error (message "Error extracting properties for node %s: %s" id (error-message-string err2)))))))
               t nil)
            (error (message "Error scanning buffer: %s" (error-message-string err))))))
      
      ;; Second pass: process updates, creations, and moves
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
                           (when (and (eq (plist-get node :type) :node)
                                    (string= (expand-file-name (plist-get node :file-path)) file))
                             (push id nodes-to-remove)))
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
                 org-supertag-db--object)))
    cleaned-count))

(defun org-supertag-sync--check-and-sync ()
  "Check and synchronize modified files.
This is the main sync function called periodically."
  ;; Clean up non-existent files and files out of sync scope from sync state
  (let ((files-to-remove nil))
    (maphash (lambda (file _state)
               (when (or (not (file-exists-p file))
                        (not (org-supertag-sync--in-sync-scope-p file)))
                 (push file files-to-remove)))
             org-supertag-sync--state)
    
    ;; Remove files from sync state and their nodes from database
    (dolist (file files-to-remove)
      (message "[org-supertag] Removing file from sync state (file %s or out of scope): %s"
               (if (file-exists-p file) "exists" "doesn't exist")
               file)
      (remhash file org-supertag-sync--state)
      ;; Remove nodes from these files in database
      (maphash (lambda (id node)
                 (when (and (eq (plist-get node :type) :node)
                           (string= (plist-get node :file-path) file))
                   (org-supertag-db-remove-object id)))
               org-supertag-db--object)))

  ;; Check for new files first
  (let ((new-files (org-supertag-scan-sync-directories)))
    (when new-files
      (dolist (file new-files)
        (org-supertag-sync-update-state file))))
  
  ;; Original sync logic
  (let ((modified-files (org-supertag-get-modified-files))
        (nodes-deleted 0)
        (nodes-moved 0)
        (nodes-created 0)
        (old-nodes (make-hash-table :test 'equal))
        (updated 0))
    
    ;; First collect existing nodes
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
                    (time-less-p
                     (file-attribute-modification-time
                      (file-attributes a))
                     (file-attribute-modification-time
                      (file-attributes b))))))
      
      ;; Process files without error catching
      (dolist (file modified-files)
        (message "Syncing file: %s" file)
        (with-current-buffer (find-file-noselect file)
          (let ((before-nodes (hash-table-count old-nodes)))
            ;; Update database
            (org-supertag-db-update-buffer)
            ;; Count changes
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
    
    ;; Cleanup zombie nodes after processing modified files
    (when modified-files
     ;; (message "[org-supertag] Checking for zombie nodes in modified files...")
      (let ((cleaned-nodes (org-supertag-sync--auto-cleanup-zombie-nodes modified-files)))
        (when (> cleaned-nodes 0)
          (message "[org-supertag] Cleaned up %d zombie nodes" cleaned-nodes))))
    
    ;; Report results
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
      (maphash
       (lambda (id props)
         ;; 1. Add the node object itself with its hash.
         (org-supertag-db-add-with-hash id props)
         ;; 2. Process and register all tags found in the headline.
         (when-let ((headline-tags (plist-get props :tags)))
           (dolist (tag-name headline-tags)
             ;; Defensively skip empty tag names that may result from parsing errors.
             (unless (or (null tag-name) (string-empty-p tag-name))
               (let ((sanitized-tag (org-supertag-sanitize-tag-name tag-name)))
                 ;; Ensure tag object exists (non-destructive)
                 ;; Only create tag if it does not exist.
                 (unless (org-supertag-tag-get sanitized-tag)
                   (org-supertag-tag--create sanitized-tag))
                 ;; Ensure link exists
                 (org-supertag-db-link "HAS_TAG" id sanitized-tag))))))
       all-nodes-props)

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
  (org-supertag-sync-all t))

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
                (org-supertag-db-link "HAS_TAG" node-id tag-name '(:source :filetags))
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
(defun org-supertag-sync-cleanup-database ()
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

(defun org-supertag-sync--diagnose-parse-error (file)
  "Diagnose and attempt to fix org-element parsing errors in FILE."
  (condition-case err
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (let ((org-element-use-cache nil)
                  (case-fold-search t)
                  (problematic-regions '()))
              ;; First pass: try to identify problematic regions
              (while (re-search-forward org-heading-regexp nil t)
                (when (org-at-heading-p)
                  (let ((pos (point))
                        (heading (org-get-heading t t t t)))
                    (condition-case err
                        (progn
                          (org-back-to-heading t)
                          (let ((element (org-element-at-point)))
                            (unless (and element
                                       (org-element-property :raw-value element))
                              (push (list pos heading "Invalid element structure") problematic-regions))))
                      (error
                       (push (list pos heading (error-message-string err)) problematic-regions))))))
              
              ;; Report findings
              (when problematic-regions
                (with-current-buffer (get-buffer-create "*Org Parse Diagnosis*")
                  (erase-buffer)
                  (insert (format "Parse diagnosis for %s\n\n" file))
                  (dolist (region (nreverse problematic-regions))
                    (let ((pos (nth 0 region))
                          (heading (nth 1 region))
                          (error-msg (nth 2 region)))
                      (insert (format "Position %d: %s\n  Error: %s\n\n"
                                    pos heading error-msg))))
                  (display-buffer (current-buffer))))
              
              ;; Return number of problems found
              (length problematic-regions))))
    (error
     (message "Error diagnosing file %s: %s" file (error-message-string err))
     -1))))

;;;###autoload
(defun org-supertag-sync-test-auto-id-creation ()
  "Test automatic ID creation for headings in current buffer.
Scans current buffer and attempts to create IDs for all headings
that meet the criteria. Reports results."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be in an org-mode buffer"))
  
  (let ((created-count 0)
        (existing-count 0)
        (failed-count 0)
        (errors '()))
    
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (while (re-search-forward org-heading-regexp nil t)
            (when (and (org-at-heading-p)
                       (not (string-prefix-p "TAGS" (org-get-heading t t t t)))
                       (not (org-in-commented-heading-p))
                       (>= (org-current-level) org-supertag-sync-node-creation-level))
              (let ((id (org-id-get))
                    (heading (org-get-heading t t t t))
                    (line (line-number-at-pos)))
                (cond
                 ;; Already has ID
                 (id
                  (cl-incf existing-count)
                  (message "Line %d: %s [ID exists: %s]" line heading id))
                 ;; Try to create ID
                 (t
                  (condition-case err
                      (progn
                        (org-supertag-node-create)
                        (let ((new-id (org-id-get)))
                          (if new-id
                              (progn
                                (cl-incf created-count)
                                (message "Line %d: %s [ID created: %s]" line heading new-id))
                            (cl-incf failed-count)
                            (push (list line heading "ID creation returned nil") errors)
                            (message "Line %d: %s [ID creation failed: returned nil]" line heading))))
                    (error
                     (cl-incf failed-count)
                     (let ((err-msg (error-message-string err)))
                       (push (list line heading err-msg) errors)
                       (message "Line %d: %s [Error: %s]" line heading err-msg)))))))))))
    
    ;; Report results
    (message "\n=== Auto ID Creation Test Results ===")
    (message "Total headings processed: %d" (+ created-count existing-count failed-count))
    (message "IDs created: %d" created-count)
    (message "IDs already existed: %d" existing-count)
    (message "Failures: %d" failed-count)
    
    (when errors
      (message "\nErrors encountered:")
      (dolist (error errors)
        (message "  Line %d (%s): %s" (nth 0 error) (nth 1 error) (nth 2 error))))
    
    ;; Show results in a buffer if there were errors
    (when errors
      (with-current-buffer (get-buffer-create "*Org Supertag ID Test Results*")
        (erase-buffer)
        (insert "Auto ID Creation Test Results\n")
        (insert (format "===============================\n\n"))
        (insert (format "Total headings: %d\n" (+ created-count existing-count failed-count)))
        (insert (format "IDs created: %d\n" created-count))
        (insert (format "IDs existing: %d\n" existing-count))
        (insert (format "Failures: %d\n\n" failed-count))
        
        (when errors
          (insert "Errors:\n")
          (dolist (error errors)
            (insert (format "Line %d: %s\n  Error: %s\n\n" 
                           (nth 0 error) (nth 1 error) (nth 2 error)))))
        (display-buffer (current-buffer))))
    
    (list :created created-count :existing existing-count :failed failed-count :errors errors)))

;;; Debug Functions for Hash Mismatch Issues

(defun org-supertag-debug-hash-differences ()
  "Debug function to check hash calculation differences.
This helps identify why nodes are being marked as 'updated' when they shouldn't be."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be in an org-mode buffer"))
  
  (let ((problematic-nodes '())
        (total-checked 0)
        (hash-mismatches 0))
    
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (let ((id (org-id-get)))
             (when id
               (cl-incf total-checked)
               (let* ((old-node (org-supertag-db-get id))
                      (new-props (org-supertag-extract-node-props)))
                 (when (and old-node new-props)
                   (let ((old-hash (or (plist-get old-node :hash)
                                      (org-supertag-node-hash old-node)))
                         (new-hash (org-supertag-node-hash new-props)))
                     (unless (string= old-hash new-hash)
                       (cl-incf hash-mismatches)
                       ;; Detailed comparison
                       (let ((differences '()))
                         (dolist (prop '(:raw-value :tags :todo-type :priority))
                           (let ((old-val (plist-get old-node prop))
                                 (new-val (plist-get new-props prop)))
                             (unless (equal old-val new-val)
                               (push (list prop 
                                          :old old-val 
                                          :new new-val) differences))))
                         (push (list :id id
                                   :title (plist-get new-props :title)
                                   :old-hash old-hash
                                   :new-hash new-hash
                                   :differences differences
                                   :old-has-hash (not (null (plist-get old-node :hash))))
                               problematic-nodes))))))))
         t nil)))
    
    ;; Report results
    (with-current-buffer (get-buffer-create "*Org Supertag Hash Debug*")
      (erase-buffer)
      (insert (format "Hash Debugging Results\n"))
      (insert (format "======================\n\n"))
      (insert (format "Total nodes checked: %d\n" total-checked))
      (insert (format "Hash mismatches: %d\n\n" hash-mismatches))
      
      (if (= hash-mismatches 0)
          (insert "✅ No hash mismatches found! All nodes have consistent hashes.\n")
        (progn
          (insert "❌ Found hash mismatches in the following nodes:\n\n")
          (dolist (node (reverse problematic-nodes))
            (insert (format "📝 Node: %s\n" (plist-get node :title)))
            (insert (format "   ID: %s\n" (plist-get node :id)))
            (insert (format "   Old Hash: %s\n" (plist-get node :old-hash)))
            (insert (format "   New Hash: %s\n" (plist-get node :new-hash)))
            (insert (format "   Old node had hash: %s\n" 
                           (if (plist-get node :old-has-hash) "Yes" "No")))
            (let ((diffs (plist-get node :differences)))
              (if diffs
                  (progn
                    (insert "   Differences:\n")
                    (dolist (diff diffs)
                      (insert (format "     %s:\n" (car diff)))
                      (insert (format "       Old: %S\n" (plist-get diff :old)))
                      (insert (format "       New: %S\n" (plist-get diff :new)))))
                (insert "   No property differences found (unexpected!)\n")))
            (insert "\n"))))
      (display-buffer (current-buffer)))
    
    (message "Hash debug completed: %d/%d nodes have hash mismatches" 
             hash-mismatches total-checked)
    
    (list :total total-checked 
          :mismatches hash-mismatches 
          :problematic-nodes problematic-nodes))))

(defun org-supertag-debug-properties-stability ()
  "Debug function to test properties extraction stability."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be in an org-mode buffer"))
  
  (when-let* ((id (org-id-get))
              (title (org-get-heading t t t t)))
    (message "Testing properties stability for: %s" title)
    
    ;; Extract properties multiple times
    (let ((extractions '()))
      (dotimes (i 3)
        (let ((props (org-supertag-extract-node-props)))
          (push (list :attempt (1+ i)
                     :properties (plist-get props :properties)
                     :hash (org-supertag-node-hash props))
                extractions)))
      
      (with-current-buffer (get-buffer-create "*Org Supertag Properties Debug*")
        (erase-buffer)
        (insert (format "Properties Stability Test for: %s\n" title))
        (insert (format "ID: %s\n\n" id))
        
        (let ((all-hashes (mapcar (lambda (e) (plist-get e :hash)) extractions))
              (all-properties (mapcar (lambda (e) (plist-get e :properties)) extractions))))
          
          (if (= (length (delete-dups (copy-sequence all-hashes))) 1)
              (insert "✅ Hash is stable across multiple extractions\n")
            (insert "❌ Hash varies across extractions!\n"))
          
          (insert "\nDetailed Results:\n")
          (dolist (extraction extractions)
            (insert (format "\nAttempt %d:\n" (plist-get extraction :attempt)))
            (insert (format "  Hash: %s\n" (plist-get extraction :hash)))
            (insert (format "  Properties count: %d\n" 
                           (length (plist-get extraction :properties))))
            (let ((props (plist-get extraction :properties)))
              (when props
                (insert "  Properties:\n")
                (dolist (prop props)
                  (insert (format "    %s: %s\n" (car prop) (cdr prop))))))))
        
        (display-buffer (current-buffer))))))

;;; Hash Stability Fix Functions

(defun org-supertag-fix-missing-hashes ()
  "Fix all nodes in database that don't have hash values.
This should resolve the 'false positive updates' issue."
  (interactive)
  (let ((fixed-count 0)
        (total-nodes 0))
    
    (maphash
     (lambda (id node)
       (when (eq (plist-get node :type) :node)
         (cl-incf total-nodes)
         (unless (plist-get node :hash)
           ;; Node doesn't have a hash, calculate and save it
           (let ((node-hash (org-supertag-node-hash node)))
             (org-supertag-db-add id 
                                  (plist-put node :hash node-hash))
             (cl-incf fixed-count)))))
     org-supertag-db--object)
    
    (message "Fixed %d/%d nodes missing hash values" fixed-count total-nodes)
    fixed-count))

(defun org-supertag-normalize-all-hashes ()
  "Recalculate all node hashes using the improved hash function.
This ensures all nodes use the same hash calculation method."
  (interactive)
  (when (yes-or-no-p "This will recalculate all node hashes. Continue? ")
    (let ((updated-count 0)
          (total-nodes 0))
      
      (maphash
       (lambda (id node)
         (when (eq (plist-get node :type) :node)
           (cl-incf total-nodes)
           (let ((new-hash (org-supertag-node-hash node)))
             ;; Always update with new hash to ensure consistency
             (org-supertag-db-add id 
                                  (plist-put node :hash new-hash))
             (cl-incf updated-count)))))
       org-supertag-db--object)
    
    (message "Normalized hashes for %d nodes" updated-count)
    updated-count))

(provide 'org-supertag-sync)

;;; org-supertag-sync.el ends here
