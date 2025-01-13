;;; org-supertag-sync.el --- File synchronization for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides file synchronization functionality for org-supertag:
;; - File state tracking
;; - Buffer modification monitoring
;; - Automatic synchronization
;; - Conflict detection
;; - State persistence
;; - Error recovery

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-supertag-db)

;;; Customization

(defgroup org-supertag-sync nil
  "Synchronization settings for org-supertag."
  :group 'org-supertag)

(defcustom org-supertag-sync-state-file 
  (expand-file-name "org-supertag-sync-state.el" user-emacs-directory)
  "File to store sync state data."
  :type 'file
  :group 'org-supertag-sync)

(defcustom org-supertag-sync-auto-interval 30
  "Interval in seconds for automatic synchronization."
  :type 'integer
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
  "Check if FILE needs sync.
Returns:
- nil if no sync needed
- t if sync needed
- 'conflict if conflict detected"
  (when-let* ((state (gethash file org-supertag-sync--state))
              (old-mtime (car state))
              (old-hash (cdr state))
              (new-mtime (file-attribute-modification-time
                         (file-attributes file)))
              (new-hash (org-supertag-sync-file-hash file)))
    (cond
     ;; No change
     ((and (time-equal-p old-mtime new-mtime)
           (string= old-hash new-hash))
      nil)
     ;; Changed but consistent
     ((and (time-less-p old-mtime new-mtime)
           (not (string= old-hash new-hash)))
      t)
     ;; Potential conflict
     (t 'conflict))))

;;; Buffer Monitoring

(defun org-supertag-sync-setup-buffer-watch ()
  "Setup buffer modification tracking."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (string-match-p "\\.org$" buffer-file-name))
        ;; Add after-save-hook
        (add-hook 'after-save-hook 
                  #'org-supertag-sync--handle-save
                  nil t)
        ;; Track modifications
        (add-hook 'before-change-functions
                  #'org-supertag-sync--handle-modify 
                  nil t)))))

(defun org-supertag-sync--handle-save ()
  "Handle buffer save."
  (when-let ((file buffer-file-name))
    ;; Update database
    (org-supertag-db-update-buffer)
    ;; Update sync state
    (org-supertag-sync-update-state file)))

(defun org-supertag-sync--handle-modify (_beg _end)
  "Handle buffer modification."
  (when-let ((file buffer-file-name))
    ;; Mark as modified
    (let ((state (gethash file org-supertag-sync--state)))
      (when state
        (setcar state (current-time))))))

;;; State Management

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

;;; Automatic Synchronization

(defun org-supertag-sync-check-all ()
  "Check sync state of all files.
Returns list of files needing sync."
  (let (needs-sync conflicts)
    (maphash
     (lambda (file _state)
       (when (file-exists-p file)
         (let ((status (org-supertag-sync-check-state file)))
           (cond
            ((eq status t)
             (push file needs-sync))
            ((eq status 'conflict)
             (push file conflicts))))))
     org-supertag-sync--state)
    (cons needs-sync conflicts)))

(defun org-supertag-sync--check-and-sync ()
  "Check and sync changed files."
  (let* ((status (org-supertag-sync-check-all))
         (needs-sync (car status))
         (conflicts (cdr status)))
    ;; Handle needed syncs
    (dolist (file needs-sync)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-supertag-db-update-buffer)
          (org-supertag-sync-update-state file))))
    ;; Report conflicts
    (when conflicts
      (message "Sync conflicts in files: %S" conflicts))))

(defun org-supertag-sync-start-auto-sync (&optional interval)
  "Start auto-sync timer with INTERVAL seconds."
  (when org-supertag-sync--timer
    (cancel-timer org-supertag-sync--timer))
  (setq org-supertag-sync--timer
        (run-with-idle-timer 
         (or interval org-supertag-sync-auto-interval) t
         #'org-supertag-sync--check-and-sync)))

;;; Error Recovery

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

(provide 'org-supertag-sync)

;;; org-supertag-sync.el ends here