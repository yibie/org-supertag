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
  '(:raw-value :tags :todo-type :priority :properties)
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

;;; Core Functions - Node Hash Support

(defun org-supertag-node-hash (node)
  "Calculate hash value for NODE.
Only includes stable properties, excludes position information."
  (secure-hash 'sha1
               (format "%s%s%s%s%s"
                      (plist-get node :raw-value)    ; title
                      (plist-get node :tags)         ; tags
                      (plist-get node :todo-type)    ; TODO state
                      (plist-get node :priority)     ; priority
                      (plist-get node :properties)))) ; property drawer

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
                  ;; 只在没有 ID 时创建节点
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
                                         (let ((element (org-element-at-point)))
                                           (org-supertag-extract-node-props)))
                                     (error
                                      (message "Error extracting properties at point %d: %s"
                                               (point)
                                               (error-message-string err))
                                      nil))))
                    (puthash id props nodes)))))))))
    nodes))

(defun org-supertag-extract-node-props ()
  "Extract properties of node at point.
Returns a plist of node properties or nil if not at a valid node."
  (when (org-at-heading-p)
    (condition-case err
        (let* ((element (save-excursion
                         (org-back-to-heading t)
                         (let ((org-element-use-cache nil))
                           (org-element-at-point))))
               (raw-value (org-element-property :raw-value element)))
          (unless raw-value
            (error "Failed to extract raw value from heading"))
          (let ((props (list :type :node
                            :id (org-id-get)
                            :title raw-value
                            :raw-value raw-value
                            :tags (org-element-property :tags element)
                            :todo-type (org-element-property :todo-type element)
                            :priority (org-element-property :priority element)
                            :properties (condition-case nil
                                          (org-entry-properties nil 'all)
                                        (error nil))
                            :file-path (buffer-file-name)
                            :level (or (org-element-property :level element) 1)
                            :pos (or (org-element-property :begin element) (point))
                            :begin (or (org-element-property :begin element) (point))
                            :contents-begin (org-element-property :contents-begin element)
                            :contents-end (org-element-property :contents-end element)
                            :olp (condition-case nil
                                    (org-get-outline-path t)
                                  (error nil)))))
            ;; Validate required properties
            (cl-loop for prop in '(:id :title :file-path :type)
                     for value = (plist-get props prop)
                     unless value do
                     (error "Missing required property %s" prop))
            props))
      (error
       (message "Error extracting node properties: %s" (error-message-string err))
       nil))))

(defun org-supertag-db-update-buffer ()
  "Update database with all nodes in current buffer.
Uses a two-pass approach:
1. Scan buffer to collect current nodes
2. Process updates, moves, and deletions"
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
        (let ((org-element-use-cache nil)  ; Disable element cache
              (org-startup-folded nil)      ; Ensure all content is visible
              (org-startup-with-inline-images nil)  ; Disable image loading
              (org-startup-with-latex-preview nil)  ; Disable latex preview
              (org-hide-emphasis-markers nil))      ; Show all markers
          (condition-case err
              (org-map-entries
               (lambda ()
                 (let ((id (org-id-get)))
                   (when (and id
                            (>= (org-current-level) org-supertag-sync-node-creation-level))
                     (condition-case err2
                         (when-let* ((props (org-supertag-extract-node-props)))
                           (puthash id props current-nodes))
                       (error
                        (message "Error extracting properties for node %s: %s"
                                id (error-message-string err2)))))))
               t nil)
            (error
             (message "Error scanning buffer: %s" (error-message-string err))))))
      
      ;; Second pass: process updates and moves
      (maphash
       (lambda (id props)
         (let* ((old-node (org-supertag-db-get id))
                (old-file (and old-node (plist-get old-node :file-path))))
           (condition-case err
               (cond
                ;; Node moved from another file
                ((and old-node
                      (not (string= old-file file)))
                 (let ((preserved-props '(:ref-to :ref-from :ref-count)))
                   (dolist (prop preserved-props)
                     (when-let* ((value (plist-get old-node prop)))
                       (setq props (plist-put props prop value)))))
                 (org-supertag-db-add-with-hash id props)
                 (cl-incf moved))
                ;; Node updated in same file
                ((and old-node
                      (org-supertag-node-changed-p old-node props))
                 (let ((preserved-props '(:ref-to :ref-from :ref-count)))
                   (dolist (prop preserved-props)
                     (when-let* ((value (plist-get old-node prop)))
                       (setq props (plist-put props prop value)))))
                 (org-supertag-db-add-with-hash id props)
                 (cl-incf updated))
                ;; New node
                ((null old-node)
                 (org-supertag-db-add-with-hash id props)
                 (cl-incf updated)))
             (error
              (message "Error processing node %s: %s"
                       id (error-message-string err))))))
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
             (error
              (message "Error removing node %s: %s"
                       id (error-message-string err))))))
       org-supertag-db--object)
      
      ;; Report changes
      (when (or (> updated 0) (> deleted 0) (> moved 0))
        (message "Buffer sync: %d updated, %d deleted, %d moved"
                 updated deleted moved)))))

(defun org-supertag--sync-at-point ()
  "Synchronize node at point with database."
  (when-let* ((id (org-id-get))
              (props (org-supertag-extract-node-props)))
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

(defun org-supertag-sync--maybe-auto-init ()
  "Maybe initialize sync system automatically.
Only initialize if auto-sync is enabled and not already initialized."
  (when (and (not org-supertag-sync--initialized)  ; Not already initialized
             (org-supertag-sync--ensure-directories)  ; Ensure directories are loaded
             (not org-supertag-sync--timer)) ; Not already running
    (org-supertag-sync-init)))

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
    ;; Check for parsing errors in all files
    (dolist (file (append all-files new-files))
      (when (and (file-exists-p file)
                 (> (org-supertag-sync--diagnose-parse-error file) 0))
        (message "Warning: Found parsing issues in %s" file)))
    ;; Update state for valid files
    (dolist (file (append all-files new-files))
      (when (file-exists-p file)
        (org-supertag-sync-update-state file))))
  
  ;; Save initial state
  (org-supertag-sync-save-state)
  
  ;; Start auto-sync
  (org-supertag-sync-start-auto-sync)
  
  ;; Mark as initialized
  (setq org-supertag-sync--initialized t))

;; Initialize when appropriate
(add-hook 'after-init-hook #'org-supertag-sync--maybe-auto-init)
(with-eval-after-load 'org-supertag
  (org-supertag-sync--maybe-auto-init))
(when (featurep 'org-supertag)
  (org-supertag-sync--maybe-auto-init))

;;-------------------------------------------------------------------
;; Automatic Synchronization
;;-------------------------------------------------------------------

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

(defun org-supertag-scan-sync-directories ()
  "Scan sync directories for new org files.
Returns a list of new files that are not yet in sync state."
  (let ((new-files nil))
    (dolist (dir org-supertag-sync-directories)
      (when (file-exists-p dir)
        (let ((dir-files (directory-files-recursively 
                         dir org-supertag-sync-file-pattern t)))
          (dolist (file dir-files)
            (when (and (file-regular-p file)
                      (org-supertag-sync--in-sync-scope-p file)
                      (not (gethash file org-supertag-sync--state)))
              (push file new-files))))))
    new-files))

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
        (errors nil)
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
      
      ;; Process files
      (dolist (file modified-files)
        (condition-case err
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
                (cl-incf updated)))
          (error
           (push (cons file (error-message-string err))
                 errors)))))
    
    ;; Report results
    (if errors
        (progn
          (message "Sync completed with errors: %d files updated, %d errors"
                   updated (length errors))
          (with-current-buffer (get-buffer-create "*Org Supertag Sync Errors*")
            (erase-buffer)
            (insert "Force Synchronization Errors:\n\n")
            (dolist (err errors)
              (insert (format "File: %s\nError: %s\n\n"
                             (car err) (cdr err))))
            (display-buffer (current-buffer))))
      (message "[org-supertag] Node Sync Completed"))))

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

(defun org-supertag-sync-force-all ()
  "Force synchronization of all files in scope."
  (interactive)
  (let* ((files (org-supertag-get-all-files))
         (total (length files))
         (batch-size 10)  ; Process 10 files at a time
         (current 0)
         (updated 0)
         (errors nil)
         ;; save original values 
         (org-startup-with-latex-preview nil)
         (org-startup-folded nil)
         (org-startup-with-inline-images nil)
         (org-startup-indented nil)
         (org-hide-block-startup nil)
         (org-hide-drawer-startup nil)
         (org-startup-align-all-tables nil)
         ;; disable org-element parsing
         (org-element-use-cache nil)
         ;; disable auto collect keywords
         (org--collect-keywords-cache (make-hash-table :test 'equal))
         ;; 保存节点关系数据
         (preserved-data (make-hash-table :test 'equal))
         ;; 保存所有非节点实体
         (preserved-entities (make-hash-table :test 'equal))
         ;; 保存所有链接数据
         (preserved-links (make-hash-table :test 'equal)))
    
    ;; 保存所有节点的关系数据
    (maphash
     (lambda (id node)
       (when (eq (plist-get node :type) :node)
         (let ((rel-data (list :ref-to (plist-get node :ref-to)
                              :ref-from (plist-get node :ref-from)
                              :ref-count (plist-get node :ref-count))))
           (puthash id rel-data preserved-data))))
     org-supertag-db--object)
    
    (maphash
     (lambda (id entity)
       (let ((entity-type (plist-get entity :type)))
         (when (and entity-type (not (eq entity-type :node)))
           (puthash id (copy-sequence entity) preserved-entities))))
     org-supertag-db--object)
    
    ;; Confirm with user if too many files
    (when (and (> total 100)
               (not (yes-or-no-p 
                     (format "About to process %d files. Continue? " total))))
      (user-error "Aborted force sync"))
    
    ;; Process files in batches
    (while files
      ;; Take next batch
      (let ((batch (seq-take files batch-size)))
        (setq files (seq-drop files batch-size))
        
        ;; Process batch
        (dolist (file batch)
          (setq current (1+ current))
          
          (when (and (file-exists-p file)
                    (org-supertag-sync--in-sync-scope-p file))
            (condition-case err
                (let ((buf (find-file-noselect file nil nil nil)))
                  (with-current-buffer buf
                    ;; disable some features that may cause problems
                    (setq-local org-startup-with-latex-preview nil
                              org-startup-folded nil
                              org-startup-with-inline-images nil
                              org-startup-indented nil
                              org-hide-block-startup nil
                              org-hide-drawer-startup nil
                              org-startup-align-all-tables nil
                              org-element-use-cache nil)
                    ;; ensure buffer is in correct mode
                    (let ((org-mode-hook nil)
                          (org-set-regexps-and-options-hook nil)
                          (org-startup-options-hook nil))
                      (delay-mode-hooks
                        (org-mode)))
                    ;; ensure buffer is fully loaded
                    (widen)
                    (goto-char (point-min))
                    ;; disable some features that may interfere
                    (let ((org-fold-core-style 'overlays)
                          (org-element-use-cache nil)
                          (org-startup-folded nil))
                      (let ((current-nodes (org-supertag-scan-buffer-nodes)))
                        (maphash
                         (lambda (id props)
                           (when-let* ((rel-data (gethash id preserved-data)))
                             (dolist (prop '(:ref-to :ref-from :ref-count))
                               (when-let* ((value (plist-get rel-data prop)))
                                 (setq props (plist-put props prop value)))))
                           (org-supertag-db-add-with-hash id props))
                         current-nodes))
                      (org-supertag-sync-update-state file))
                    ;; Save buffer if modified
                    (when (buffer-modified-p)
                      (basic-save-buffer)))
                  ;; Kill buffer
                  (kill-buffer buf)
                  (cl-incf updated))
              (error
               (push (cons file (error-message-string err))
                     errors))))
          
          ;; Add delay between files
          (sit-for 0.1))
        
        ;; Allow interruption between batches
        (when (input-pending-p)
          (when (yes-or-no-p "Interrupt force sync? ")
            (user-error "Force sync interrupted at file %d/%d" 
                       current total)))))
    
         ;; 恢复所有非节点实体
     (maphash
      (lambda (id entity)
        (org-supertag-db-add id entity))
      preserved-entities)
     
     ;; 恢复所有链接数据
     (maphash
      (lambda (link-id link-data)
        (puthash link-id link-data org-supertag-db--link))
      preserved-links)
    
    ;; Report results
    (if errors
        (progn
          (message "Force sync completed with errors: %d files updated, %d errors"
                   updated (length errors))
          (with-current-buffer (get-buffer-create "*Org Supertag Sync Errors*")
            (erase-buffer)
            (insert "Force Synchronization Errors:\n\n")
            (dolist (err errors)
              (insert (format "File: %s\nError: %s\n\n"
                             (car err) (cdr err))))
            (display-buffer (current-buffer))))
      (message "[org-supertag] Node Sync Completed"))))

;;-------------------------------------------------------------------
;; Error Recovery
;;-------------------------------------------------------------------

(defun org-supertag-sync-recover ()
  "Recover from sync errors.
1. Stop auto-sync
2. Reset state
3. Rebuild from files
4. Rescan all nodes"
  (interactive)
  ;; 1. Stop auto-sync
  (when org-supertag-sync--timer
    (cancel-timer org-supertag-sync--timer))
  
  ;; 2. Reset state
  (clrhash org-supertag-sync--state)
  
  ;; 3. Rebuild file state
  (let ((errors nil))
    (dolist (file (org-supertag-get-all-files))
      (when (file-exists-p file)
        (condition-case err
            (progn
              ;; Update file state
              (org-supertag-sync-update-state file)
              ;; Scan nodes in file
              (with-current-buffer (find-file-noselect file)
                (org-supertag-db-update-buffer)))
          (error
           (push (cons file (error-message-string err))
                 errors)))))
    
    ;; Report errors if any
    (when errors
      (with-current-buffer (get-buffer-create "*Org Supertag Sync Errors*")
        (erase-buffer)
        (insert "Recovery Errors:\n\n")
        (dolist (err errors)
          (insert (format "File: %s\nError: %s\n\n"
                         (car err) (cdr err))))
        (display-buffer (current-buffer)))))
  
  ;; 4. Save recovered state
  (org-supertag-sync-save-state)
  
  ;; 5. Restart auto-sync
  (org-supertag-sync-start-auto-sync)
  (message "Recovery completed"))

(defun org-supertag-db-resync-all ()
  "Resynchronize all nodes in database while preserving relationships.
Uses ID-based scanning to ensure reliability."
  (interactive)
  (when (yes-or-no-p "This will update all nodes. Continue? ")
    (let ((updated 0)
          (errors nil)
          (preserved-data (make-hash-table :test 'equal))
          (preserved-entities (make-hash-table :test 'equal))
          (preserved-links (make-hash-table :test 'equal)))
      
      ;; 1. First preserve all relationship data
      (maphash
       (lambda (id node)
         (when (eq (plist-get node :type) :node)
           (let ((rel-data (list :ref-to (plist-get node :ref-to)
                                :ref-from (plist-get node :ref-from)
                                :ref-count (plist-get node :ref-count))))
             (puthash id rel-data preserved-data))))
       org-supertag-db--object)
      
             ;; 1.5. 保存所有非节点实体
       (maphash
        (lambda (id entity)
          (let ((entity-type (plist-get entity :type)))
            (when (and entity-type (not (eq entity-type :node)))
              (puthash id (copy-sequence entity) preserved-entities))))
        org-supertag-db--object)
       
       ;; 1.6. 保存所有链接数据
       (maphash
        (lambda (link-id link-data)
          (puthash link-id (copy-sequence link-data) preserved-links))
        org-supertag-db--link)
      
      ;; 2. Scan all files and update nodes
      (dolist (file (org-supertag-get-all-files))
        (when (file-exists-p file)
          (condition-case err
              (with-current-buffer (find-file-noselect file)
                (let ((current-nodes (org-supertag-scan-buffer-nodes)))
                  (maphash
                   (lambda (id props)
                     ;; Restore relationship data if exists
                     (when-let* ((rel-data (gethash id preserved-data)))
                       (dolist (prop '(:ref-to :ref-from :ref-count))
                         (when-let* ((value (plist-get rel-data prop)))
                           (setq props (plist-put props prop value)))))
                     ;; Update node with hash
                     (org-supertag-db-add-with-hash id props)
                     (cl-incf updated))
                   current-nodes)))
            (error
             (push (cons file (error-message-string err))
                   errors)))))
      
             ;; 2.5. 恢复所有非节点实体
       (maphash
        (lambda (id entity)
          (org-supertag-db-add id entity))
        preserved-entities)
       
       ;; 2.6. 恢复所有链接数据
       (maphash
        (lambda (link-id link-data)
          (puthash link-id link-data org-supertag-db--link))
        preserved-links)
      
      ;; 3. Report results
      (if errors
          (progn
            (message "Resync completed with errors: %d nodes updated, %d errors"
                     updated (length errors))
            (with-current-buffer (get-buffer-create "*Org Supertag Sync Errors*")
              (erase-buffer)
              (insert "Resync Errors:\n\n")
              (dolist (err errors)
                (insert (format "File: %s\nError: %s\n\n"
                               (car err) (cdr err))))
              (display-buffer (current-buffer))))
        (message "Successfully resynced %d nodes while preserving relationships"
                 updated))
      updated)))

;;-------------------------------------------------------------------
;; Helper Functions
;;-------------------------------------------------------------------

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

(defun org-supertag-sync-add-directory (dir)
  "Add directory to synchronization scope."
  (interactive "DAdd directory to sync: ")
  (let ((normalized-dir (org-supertag-sync--normalize-directory dir)))
    (unless (member normalized-dir org-supertag-sync-directories)
      (push normalized-dir org-supertag-sync-directories)
      (customize-save-variable 'org-supertag-sync-directories 
                             org-supertag-sync-directories)
      (message "Added %s to sync directories" normalized-dir))))

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
          (length problematic-regions))))))

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
                       (message "Line %d: %s [Error: %s]" line heading err-msg))))))))))))
    
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
        (insert "===============================\n\n")
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

(provide 'org-supertag-sync)

;;; org-supertag-sync.el ends here
