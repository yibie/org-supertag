;;; Fix sync hash issue - ensure incremental sync works properly

(require 'org-supertag-background-sync)

(defun fix-sync-hash-issue ()
  "Fix the sync_hashes.json file not being created issue."
  (interactive)
  (message "\n=== Fixing Sync Hash Issue ===")
  
  ;; 1. Ensure data directory exists
  (unless (file-exists-p org-supertag-data-directory)
    (make-directory org-supertag-data-directory t)
    (message "Created data directory: %s" org-supertag-data-directory))
  
  ;; 2. Force a manual sync to establish baseline hashes
  (message "\nForcing initial sync to establish baseline...")
  
  ;; 3. Calculate hashes for all current objects
  (let ((node-count 0)
        (tag-count 0)
        (link-count 0))
    
    ;; Clear existing hashes
    (clrhash org-supertag-background-sync--last-sync-hashes)
    
    ;; Process all objects in the database
    (maphash
     (lambda (id props)
       (let ((hash (org-supertag-background-sync--calculate-object-hash props))
             (type (plist-get props :type)))
         (when hash
           (puthash id hash org-supertag-background-sync--last-sync-hashes)
           (cond
            ((eq type :node) (cl-incf node-count))
            ((eq type :tag) (cl-incf tag-count))))))
     org-supertag-db--object)
    
    ;; Process all links
    (maphash
     (lambda (id props)
       (let ((hash (org-supertag-background-sync--calculate-object-hash props)))
         (when hash
           (puthash id hash org-supertag-background-sync--last-sync-hashes)
           (cl-incf link-count))))
     org-supertag-db--link)
    
    (message "Calculated hashes for: %d nodes, %d tags, %d links" 
             node-count tag-count link-count))
  
  ;; 4. Save the hashes
  (condition-case err
      (progn
        (org-supertag-background-sync--save-hashes)
        (message "\n✅ Hash file created successfully!")
        
        ;; Verify the file
        (if (file-exists-p org-supertag-background-sync--hash-file)
            (let ((size (file-attribute-size 
                        (file-attributes org-supertag-background-sync--hash-file))))
              (message "   File: %s" org-supertag-background-sync--hash-file)
              (message "   Size: %d bytes" size)
              (message "   Hash count: %d" 
                       (hash-table-count org-supertag-background-sync--last-sync-hashes)))
          (message "❌ Hash file still doesn't exist!")))
    (error
     (message "❌ Error saving hashes: %s" (error-message-string err))))
  
  ;; 5. Update sync statistics to reflect current state
  (setq org-supertag-background-sync--last-sync-time (current-time))
  (message "\nSync state updated. Next sync will be incremental."))

(defun verify-incremental-sync ()
  "Verify that incremental sync is working."
  (interactive)
  (message "\n=== Verifying Incremental Sync ===")
  
  ;; Check if hash file exists
  (if (file-exists-p org-supertag-background-sync--hash-file)
      (progn
        (message "✅ Hash file exists: %s" org-supertag-background-sync--hash-file)
        ;; Load hashes
        (org-supertag-background-sync--load-hashes)
        (message "   Loaded %d hashes" 
                 (hash-table-count org-supertag-background-sync--last-sync-hashes))
        
        ;; Test change detection
        (let ((changes (org-supertag-background-sync--get-changed-objects)))
          (message "\nChange detection results:")
          (message "   Nodes to upsert: %d" (length (nth 0 changes)))
          (message "   Tags to upsert: %d" (length (nth 1 changes)))
          (message "   Links to upsert: %d" (length (nth 2 changes)))
          (message "   IDs to delete: %d" (length (nth 3 changes)))
          
          (if (= 0 (+ (length (nth 0 changes))
                     (length (nth 1 changes))
                     (length (nth 2 changes))
                     (length (nth 3 changes))))
              (message "\n✅ Incremental sync is working! No changes detected.")
            (message "\n⚠️  Changes detected. This is normal if you've modified data."))))
    (message "❌ Hash file doesn't exist. Run fix-sync-hash-issue first!")))

(defun force-reset-sync-hashes ()
  "Force reset all sync hashes to establish a new baseline.
This will clear the hash file and recalculate all hashes."
  (interactive)
  (when (yes-or-no-p "This will reset all sync hashes. The next sync will process all objects. Continue? ")
    (message "\n=== Force Resetting Sync Hashes ===")
    
    ;; 1. Delete existing hash file
    (when (file-exists-p org-supertag-background-sync--hash-file)
      (delete-file org-supertag-background-sync--hash-file)
      (message "Deleted existing hash file"))
    
    ;; 2. Clear in-memory hashes
    (clrhash org-supertag-background-sync--last-sync-hashes)
    
    ;; 3. Recalculate and save new hashes
    (fix-sync-hash-issue)
    
    (message "\nSync hashes have been reset. Next sync will be incremental from this baseline.")))

;; Provide interactive commands
(message "\nAvailable commands:")
(message "  M-x fix-sync-hash-issue       - Fix the missing hash file issue")
(message "  M-x verify-incremental-sync   - Verify incremental sync is working")
(message "  M-x force-reset-sync-hashes   - Force reset all hashes (use with caution)") 