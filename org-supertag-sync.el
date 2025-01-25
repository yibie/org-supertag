;;; org-supertag-sync.el --- File synchronization for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides file synchronization functionality for org-supertag:
;; - File state tracking
;; - Buffer modification monitoring
;; - Automatic synchronization
;; - Conflict detection
;; - State persistence
;; - Error recovery
;; - Node analysis
;; - Customizable sync settings

;; Configuration in init.el
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

;;; Customization

(defgroup org-supertag-sync nil
  "Synchronization settings for org-supertag."
  :group 'org-supertag)

(defcustom org-supertag-sync-state-file 
  (expand-file-name "org-supertag-sync-state.el" org-supertag-data-directory)
  "File to store sync state data.
Stores the file in the org-supertag data directory."
  :type 'file
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-auto-interval 30
  "Interval in seconds for automatic synchronization."
  :type 'integer
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-directories nil
  "List of directories to monitor for automatic synchronization.
Each entry should be an absolute path. Subdirectories will also be monitored.
If nil, no automatic synchronization will occur.
Example: '(\"~/org/work\" \"~/org/personal\")"
  :type '(repeat directory)
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-exclude-directories nil
  "List of directories to exclude from synchronization.
Takes precedence over `org-supertag-sync-directories'.
Useful for excluding specific subdirectories."
  :type '(repeat directory)
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-file-pattern "\\.org$"
  "Regular expression for matching files to synchronize."
  :type 'string
  :group 'org-supertag-sync)

;;; Variables

(defvar org-supertag-sync--state (make-hash-table :test 'equal)
  "Track file modification states.
Key: file path
Value: (mtime . hash)")

(defvar org-supertag-sync--buffer-watch nil
  "Track buffer modification hooks.")

(defvar org-supertag-sync--timer nil
  "Timer for periodic sync checks.")

;;; Core Functions

(defun org-supertag-sync-file-hash (file)
  "Calculate hash of FILE content."
  (if (file-exists-p file)
      (secure-hash 'sha1 
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
    nil))

(defun org-supertag-sync-update-state (file)
  "Update sync state for FILE."
  (when (file-exists-p file)
    (let ((mtime (file-attribute-modification-time 
                  (file-attributes file)))
          (hash (org-supertag-sync-file-hash file)))
      (puthash file (cons mtime hash) org-supertag-sync--state))))

(defun org-supertag-sync-check-state (file)
  "Check consistency state of FILE.
Returns one of:
- :consistent    - file content matches database state
- :needs-update  - file exists but content differs from database
- :missing       - file no longer exists"
  (when-let* ((state (gethash file org-supertag-sync--state))
              (old-hash (cdr state)))
    (if (file-exists-p file)
        (let ((new-hash (org-supertag-sync-file-hash file)))
          (if (string= old-hash new-hash)
              :consistent        
            :needs-update))     
      :missing)))              

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
                  #'org-supertag-node-sync-at-point
                  nil t)
        (add-hook 'org-after-demote-entry-hook
                  #'org-supertag-node-sync-at-point
                  nil t)
        (add-hook 'org-after-refile-insert-hook
                  #'org-supertag-node-sync-at-point
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
    ;; 检查修改是否涉及标题
    (save-excursion
      (goto-char beg)
      (when (and (org-at-heading-p)
                 (not (org-id-get)))
        ;; 如果是新标题且没有 ID，创建节点
        (org-supertag-node-sync-at-point)))
    ;; 更新同步状态
    (let ((state (gethash buffer-file-name org-supertag-sync--state)))
      (when state
        (setcar state (current-time))))))

(defun org-supertag-sync--process-node (element)
  "Process org node ELEMENT during sync.
Ensures node has ID and is properly registered in database."
  (let* ((begin (org-element-property :begin element))
         (id (org-element-property :ID element)))
    (save-excursion
      (goto-char begin)
      (when (org-at-heading-p)
        ;; If no ID, create one
        (unless id
          (org-id-get-create)
          (setq id (org-element-property :ID element)))
        ;; Sync to database
        (org-supertag-node-sync-at-point)))))

(defun org-supertag-db-update-buffer ()
  "Update database with all nodes in current buffer.
Ensures all headings have IDs and are properly registered."
  (save-excursion
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((parse-tree (org-element-parse-buffer 'element))
           (count 0))
       ;; 处理所有标题
       (org-element-map parse-tree 'headline
         (lambda (element)
           (when (org-supertag-sync--process-node element)
             (cl-incf count)))
         nil nil 'headline)
       (when (> count 0)
         (message "Updated %d nodes in buffer" count))))))

;;-------------------------------------------------------------------
;; State Management
;;-------------------------------------------------------------------

(defun org-supertag-sync-save-state ()
  "Save sync state to file."
  (with-temp-file org-supertag-sync-state-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 org-supertag-sync--state (current-buffer)))))

(defun org-supertag-sync-load-state ()
  "Load sync state from file."
  (when (file-exists-p org-supertag-sync-state-file)
    (with-temp-buffer
      (insert-file-contents org-supertag-sync-state-file)
      (setq org-supertag-sync--state 
            (read (current-buffer))))))

;;-------------------------------------------------------------------
;; Automatic Synchronization
;;-------------------------------------------------------------------

(defun org-supertag-sync-check-all ()
  "Check sync state of all files.
Returns a plist containing files grouped by their sync state:
- :needs-update - files that need database update
- :missing      - files that no longer exist
- :unknown      - files with uncertain state"
  (let ((needs-update nil)
        (missing nil)
        (unknown nil))
    (maphash
     (lambda (file _state)
       (when-let ((status (org-supertag-sync-check-state file)))
         (pcase status
           (:needs-update (push file needs-update))
           (:missing (push file missing))
           (:unknown (push file unknown)))))
     org-supertag-sync--state)
    (list :needs-update (nreverse needs-update)
          :missing (nreverse missing)
          :unknown (nreverse unknown))))

(defun org-supertag-sync--check-and-sync ()
  "Check and sync files based on their consistency state."
  (let* ((states (org-supertag-sync-check-all))
         (updated-files nil))
    ;; Only process files in sync scope
    (dolist (file (plist-get states :needs-update))
      (when (and (file-exists-p file)
                 (org-supertag-sync--in-sync-scope-p file))
        (condition-case err
            (with-current-buffer (find-file-noselect file)
              (org-supertag-db-update-buffer)
              (org-supertag-sync-update-state file)
              (push file updated-files)
              (message "Successfully synchronized %s" file))
          (error
           (message "Error updating %s: %s" file (error-message-string err))))))
    
    ;; Report remaining issues
    (let* ((final-states (org-supertag-sync-check-all))
           (remaining-issues
            (append (plist-get final-states :needs-update)
                   (plist-get final-states :missing)
                   (plist-get final-states :unknown))))
      (when remaining-issues
        (message "Files still needing attention:\n%s"
                 (mapconcat
                  (lambda (file)
                    (let ((status (org-supertag-sync-check-state file)))
                      (format "  %s (%s)" file status)))
                  remaining-issues
                  "\n"))))))

(defun org-supertag-sync-start-auto-sync (&optional interval)
  "Start auto-sync timer with INTERVAL seconds."
  (when org-supertag-sync--timer
    (cancel-timer org-supertag-sync--timer))
  (setq org-supertag-sync--timer
        (run-with-idle-timer 
         (or interval org-supertag-sync-auto-interval) t
         #'org-supertag-sync--check-and-sync)))

;;-------------------------------------------------------------------
;; Error Recovery
;;-------------------------------------------------------------------

(defun org-supertag-sync-recover ()
  "Recover from sync errors.
1. Stop auto-sync
2. Reset state
3. Rebuild from files"
  (interactive)
  ;; 1. Stop auto-sync
  (when org-supertag-sync--timer
    (cancel-timer org-supertag-sync--timer))
  
  ;; 2. Reset state
  (clrhash org-supertag-sync--state)
  
  ;; 3. Rebuild state from files
  (dolist (file (org-supertag-get-all-files))
    (when (file-exists-p file)
      (condition-case err
          (org-supertag-sync-update-state file)
        (error
         (message "Error processing %s: %s" 
                  file (error-message-string err))))))
  
  ;; 4. Save recovered state
  (org-supertag-sync-save-state)
  
  ;; 5. Restart auto-sync
  (org-supertag-sync-start-auto-sync))

;;; Initialization

(defun org-supertag-sync-init ()
  "Initialize sync system."
  ;; Ensure data directory exists
  (unless (file-exists-p org-supertag-data-directory)
    (make-directory org-supertag-data-directory t))
  
  ;; Load saved state if exists
  (org-supertag-sync-load-state)
  ;; Setup buffer hooks
  (org-supertag-sync-setup-buffer-watch)
  ;; Initial state for all files
  (dolist (file (org-supertag-get-all-files))
    (when (file-exists-p file)
      (org-supertag-sync-update-state file)))
  ;; Start auto-sync
  (org-supertag-sync-start-auto-sync))

;;; Manual Operations

(defun org-supertag-sync-force-all ()
  "Force sync all files."
  (interactive)
  (dolist (file (org-supertag-get-all-files))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-supertag-db-update-buffer)
        (org-supertag-sync-update-state file))))
  (message "All files synced"))

(defun org-supertag-db-resync-all ()
  "Resynchronize all nodes in database while preserving relationships.
Only updates node content and basic properties, preserving:
- Tag relationships
- Node references
- Other metadata"
  (interactive)
  (when (yes-or-no-p "This will update all node contents. Continue? ")
    (let ((updated 0))
      (maphash
       (lambda (id props)
         (when (eq (plist-get props :type) :node)
           (when-let* ((file (plist-get props :file-path))
                      (pos (plist-get props :pos))
                      (exists (file-exists-p file)))
             ;; Save existing relationship data
             (let ((ref-to (plist-get props :ref-to))
                   (ref-from (plist-get props :ref-from))
                   (ref-count (plist-get props :ref-count)))
               ;; Update node content
               (with-current-buffer (find-file-noselect file)
                 (save-excursion
                   (goto-char pos)
                   (when-let ((new-props (org-supertag-db--parse-node-at-point)))
                     ;; Restore relationship data
                     (setq new-props
                           (plist-put
                            (plist-put
                             (plist-put new-props :ref-to ref-to)
                             :ref-from ref-from)
                            :ref-count ref-count))
                     ;; Update database
                     (org-supertag-db-add id new-props)
                     (cl-incf updated))))))))
       org-supertag-db--object)
      (message "Resynced %d nodes while preserving relationships" updated)
      updated)))

(defun org-supertag-sync-analyze-file (file)
  "Analyze consistency between file nodes and database for FILE.
When used interactively, prompts to select a file.
Compares nodes in file with their database records and reports:
- Matching nodes (in sync)
- Modified nodes (need update)
- Missing nodes (in file but not in db)
- Orphaned nodes (in db but not in file)"
  (interactive
   (list (completing-read "Select file to analyze: "
                         (org-supertag-get-all-files)
                         nil t)))
  (when (file-exists-p file)
    (let ((file-nodes nil)
          (db-nodes nil)
          (matching nil)
          (modified nil)
          (missing nil)
          (orphaned nil))
      ;; Collect node information from file
      (with-current-buffer (find-file-noselect file)
        (when (derived-mode-p 'org-mode)  
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (when-let* ((element (org-element-at-point))
                         (id (org-element-property :ID element))
                         (title (org-element-property :raw-value element))
                         (level (org-element-property :level element)))
                (push (list :id id :title title :level level) file-nodes))))))
      
      ;; Collect node information from database
      (maphash
       (lambda (id props)
         (when (and (eq (plist-get props :type) :node)
                   (string= (plist-get props :file-path) file))
           (push (list :id id 
                      :title (plist-get props :title)
                      :level (plist-get props :level))
                 db-nodes)))
       org-supertag-db--object)

      ;; Analyze node status
      (dolist (file-node file-nodes)
        (let* ((id (plist-get file-node :id))
               (db-node (cl-find-if (lambda (n) 
                                     (string= (plist-get n :id) id))
                                   db-nodes)))
          (if db-node
              (if (and (string= (plist-get file-node :title)
                               (plist-get db-node :title))
                      (= (plist-get file-node :level)
                         (plist-get db-node :level)))
                  (push file-node matching)
                (push file-node modified))
            (push file-node missing))))

      ;; Find orphaned nodes (in database but not in file)
      (dolist (db-node db-nodes)
        (unless (cl-find-if (lambda (n) 
                             (string= (plist-get n :id)
                                     (plist-get db-node :id)))
                           file-nodes)
          (push db-node orphaned)))

      ;; Display analysis results
      (with-current-buffer (get-buffer-create "*Org Supertag Node Analysis*")
        (erase-buffer)
        (insert (format "Node consistency analysis for %s:\n\n" file))
        
        ;; Display matching nodes
        (when matching
          (insert "=== Matching Nodes ===\n")
          (dolist (node matching)
            (insert (format "%s %s (%s)\n"
                          (make-string (plist-get node :level) ?*)
                          (plist-get node :title)
                          (plist-get node :id)))))
        
        ;; Display modified nodes
        (when modified
          (insert "\n=== Modified Nodes (need update) ===\n")
          (dolist (node modified)
            (insert (format "%s %s (%s)\n"
                          (make-string (plist-get node :level) ?*)
                          (plist-get node :title)
                          (plist-get node :id)))))
      
        ;; Display missing nodes
        (when missing
          (insert "\n=== Missing Nodes (in file only) ===\n")
          (dolist (node missing)
            (insert (format "%s %s (%s)\n"
                          (make-string (plist-get node :level) ?*)
                          (plist-get node :title)
                          (plist-get node :id)))))
        
        ;; Display orphaned nodes
        (when orphaned
          (insert "\n=== Orphaned Nodes (in database only) ===\n")
          (dolist (node orphaned)
            (insert (format "%s %s (%s)\n"
                          (make-string (plist-get node :level) ?*)
                          (plist-get node :title)
                          (plist-get node :id)))))
        
        (display-buffer (current-buffer))))))

;;-------------------------------------------------------------------
;; Helper Functions
;;-------------------------------------------------------------------

(defun org-supertag-sync--in-sync-scope-p (file)
  "Check if FILE is within synchronization scope.
Returns t if file should be synchronized based on configured directories."
  (when (and file (file-exists-p file))
    (let* ((file-dir (file-name-directory (expand-file-name file)))
           (excluded (cl-some (lambda (dir)
                               (string-prefix-p 
                                (expand-file-name dir) file-dir))
                             org-supertag-sync-exclude-directories))
           (included (and org-supertag-sync-directories
                         (cl-some (lambda (dir)
                                   (string-prefix-p 
                                    (expand-file-name dir) file-dir))
                                 org-supertag-sync-directories))))
      (and (not excluded)
           (or (null org-supertag-sync-directories) ; if not set, sync all
               included)
           (string-match-p org-supertag-sync-file-pattern file)))))

(defun org-supertag-sync-add-directory (dir)
  "Add directory to synchronization scope."
  (interactive "DAdd directory to sync: ")
  (let ((abs-dir (expand-file-name dir)))
    (unless (member abs-dir org-supertag-sync-directories)
      (push abs-dir org-supertag-sync-directories)
      (customize-save-variable 'org-supertag-sync-directories 
                             org-supertag-sync-directories)
      (message "Added %s to sync directories" abs-dir))))

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

(defun org-supertag-sync-list-directories ()
  "Display current synchronization directories."
  (interactive)
  (with-current-buffer (get-buffer-create "*Org Supertag Sync Dirs*")
    (erase-buffer)
    (insert "=== Synchronized Directories ===\n\n")
    (if org-supertag-sync-directories
        (dolist (dir org-supertag-sync-directories)
          (insert (format "- %s\n" dir)))
      (insert "No directories configured (global sync enabled)\n"))
    (when org-supertag-sync-exclude-directories
      (insert "\n=== Excluded Directories ===\n\n")
      (dolist (dir org-supertag-sync-exclude-directories)
        (insert (format "- %s\n" dir))))
    (display-buffer (current-buffer))))

(provide 'org-supertag-sync)

;;; org-supertag-sync.el ends here