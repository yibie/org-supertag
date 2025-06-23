;;; fix-sync-cleanup-issue.el --- Complete solution to fix sync cleanup issues

(require 'org-supertag-sync)
(require 'org-supertag-db)
(require 'org-supertag-background-sync)

(defun fix-sync-cleanup-issue ()
  "Complete solution to fix sync cleanup issues.
This function will:
1. Stop auto-sync to prevent further damage
2. Clean up problematic nodes in the database (nodes with nil file-path)
3. Rescan sync directories and rebuild the database
4. Rebuild the sync hash baseline
5. Restart the sync system"
  (interactive)
  (when (yes-or-no-p "This will completely rebuild the database and sync state. Are you sure you want to continue?")
    (message "\n=== Starting sync cleanup issue fix ===")

    ;; 1. Stop auto-sync
    (message "\n1. Stopping auto-sync...")
    (when (boundp 'org-supertag-sync--timer)
      (when org-supertag-sync--timer
        (cancel-timer org-supertag-sync--timer)
        (setq org-supertag-sync--timer nil)))
    (message "   ✓ Auto-sync stopped")

    ;; 2. Clean up corrupted nodes
    (message "\n2. Cleaning up corrupted nodes (file-path is nil)...")
    (let ((cleaned-count 0)
          (nodes-to-clean '()))
      (maphash (lambda (id node)
                 (when (and (eq (plist-get node :type) :node)
                           (null (plist-get node :file-path)))
                   (push id nodes-to-clean)))
               org-supertag-db--object)
      (dolist (id nodes-to-clean)
        (org-supertag-db-remove-object id)
        (cl-incf cleaned-count))
      (message "   ✓ Cleaned up %d corrupted nodes" cleaned-count))

    ;; 3. Clear and rebuild sync state
    (message "\n3. Rebuilding sync state...")
    (clrhash org-supertag-sync--state)
    (message "   ✓ Sync state cleared")

    ;; 4. Rescan sync directories
    (message "\n4. Rescanning sync directories...")
    (let ((total-files 0)
          (processed-files 0)
          (errors '()))

      ;; Scan all configured sync directories
      (dolist (dir org-supertag-sync-directories)
        (when (file-exists-p dir)
          (message "   Scanning directory: %s" dir)
          (let ((files (directory-files-recursively dir "\\.org$")))
            (cl-incf total-files (length files))
            (dolist (file files)
              (condition-case err
                  (when (org-supertag-sync--in-sync-scope-p file)
                    ;; Update sync state
                    (org-supertag-sync-update-state file)
                    ;; Parse nodes in the file
                    (with-current-buffer (find-file-noselect file)
                      (let ((node-count 0))
                        (org-map-entries
                         (lambda ()
                           (when (org-id-get)
                             (condition-case parse-err
                                 (when-let* ((props (org-supertag-db--parse-node-at-point)))
                                   (org-supertag-db-add (plist-get props :id) props)
                                   (cl-incf node-count))
                               (error
                                (message "Warning: Error parsing node in %s: %s"
                                         file (error-message-string parse-err))))))
                         t nil)
                        (message "     Processed %d nodes" node-count)))
                    (cl-incf processed-files))
                (error
                 (push (cons file (error-message-string err)) errors)
                 (message "Error: Error processing file %s: %s" file (error-message-string err))))))))

      (message "   ✓ Scan complete: %d files, %d files processed" total-files processed-files)
      (when errors
        (message "   ⚠️  %d files failed to process" (length errors))))

    ;; 5. Save sync state
    (message "\n5. Saving sync state...")
    (org-supertag-sync-save-state)
    (message "   ✓ Sync state saved: %d files" (hash-table-count org-supertag-sync--state))

    ;; 6. Rebuild hash baseline
    (message "\n6. Rebuilding sync hash baseline...")
    (when (fboundp 'org-supertag-background-sync-rebuild-baseline)
      (condition-case err
          (progn
            (org-supertag-background-sync-rebuild-baseline)
            (message "   ✓ Hash baseline rebuilt"))
        (error
         (message "   ⚠️  Hash baseline rebuild failed: %s" (error-message-string err)))))

    ;; 7. Verify fix results
    (message "\n7. Verifying fix results...")
    (let ((node-count 0)
          (valid-nodes 0)
          (invalid-nodes 0))
      (maphash (lambda (id node)
                 (when (eq (plist-get node :type) :node)
                   (cl-incf node-count)
                   (if (plist-get node :file-path)
                       (cl-incf valid-nodes)
                     (cl-incf invalid-nodes))))
               org-supertag-db--object)
      (message "   Database status:")
      (message "     Total nodes: %d" node-count)
      (message "     Valid nodes: %d" valid-nodes)
      (message "     Invalid nodes: %d" invalid-nodes)
      (message "     Sync state files: %d" (hash-table-count org-supertag-sync--state)))

    ;; 8. Restart sync
    (message "\n8. Restarting sync system...")
    (when (fboundp 'org-supertag-sync-start-auto-sync)
      (org-supertag-sync-start-auto-sync))
    (message "   ✓ Sync system restarted")

    (message "\n=== Fix complete ===")
    (message "Suggestions:")
    (message "- Run (debug-sync-cleanup-issue) to verify status again")
    (message "- Run (org-supertag-background-sync-status) to check sync status")
    (message "- If issues persist, manual inspection of specific org files may be needed")))

(defun fix-cleanup-nil-file-paths ()
  "Specifically cleans up nodes with nil file-path.
This is a safe cleanup operation, only removing clearly corrupted data."
  (interactive)
  (let ((cleaned-count 0)
        (nodes-to-clean '()))
    (message "\n=== Cleaning up nodes with nil file-path ===")

    ;; 1. Find all nodes with nil file-path
    (maphash (lambda (id node)
               (when (and (eq (plist-get node :type) :node)
                         (null (plist-get node :file-path)))
                 (push (cons id node) nodes-to-clean)))
             org-supertag-db--object)

    (if (null nodes-to-clean)
        (message "✓ No nodes with nil file-path found")
      (progn
        (message "Found %d nodes with nil file-path:" (length nodes-to-clean))
        (dolist (entry nodes-to-clean)
          (let ((id (car entry))
                (node (cdr entry)))
            (message "  - %s (Title: %s)" id (plist-get node :title))))

        (when (yes-or-no-p (format "Delete these %d corrupted nodes?" (length nodes-to-clean)))
          (dolist (entry nodes-to-clean)
            (let ((id (car entry)))
              (org-supertag-db-remove-object id)
              (cl-incf cleaned-count)))
          (message "✓ Cleaned up %d corrupted nodes" cleaned-count))))

    cleaned-count))

(defun fix-prevent-auto-cleanup ()
  "Temporarily disable auto-cleanup to prevent further node loss."
  (interactive)
  (message "\n=== Disabling auto-cleanup function ===")

  ;; Stop auto-sync
  (when (boundp 'org-supertag-sync--timer)
    (when org-supertag-sync--timer
      (cancel-timer org-supertag-sync--timer)
      (setq org-supertag-sync--timer nil)
      (message "✓ Auto-sync stopped")))

  ;; Temporarily redefine cleanup functions to no-ops
  (fset 'org-supertag-sync--cleanup-database
        (lambda ()
          (message "Cleanup function has been temporarily disabled to prevent data loss")))

  (fset 'org-supertag-sync-validate-and-cleanup-zombie-nodes
        (lambda ()
          (message "Zombie node cleanup function has been temporarily disabled")
          0))

  (message "✓ Cleanup function has been temporarily disabled")
  (message "Note: To re-enable, you need to reload org-supertag-sync.el"))

(defun fix-restore-from-files ()
  "Rebuild the entire database from the file system.
This is the safest recovery method, directly rereading all data from org files."
  (interactive)
  (when (yes-or-no-p "This will clear the database and rebuild from org files. Are you sure you want to continue?")
    (message "\n=== Rebuilding database from files ===")

    ;; 1. Backup current database
    (message "\n1. Backing up current database...")
    (when (fboundp 'org-supertag-db-backup)
      (org-supertag-db-backup))

    ;; 2. Clear database (preserve non-node data)
    (message "\n2. Clearing node data...")
    (let ((preserved-count 0)
          (removed-count 0))
      (maphash (lambda (id entity)
                 (let ((type (plist-get entity :type)))
                   (if (eq type :node)
                       (progn
                         (ht-remove! org-supertag-db--object id)
                         (cl-incf removed-count))
                     (cl-incf preserved-count))))
               (ht-copy org-supertag-db--object))
      (message "   ✓ Removed %d nodes, preserved %d other entities" removed-count preserved-count))

    ;; 3. Rescan and import all nodes
    (message "\n3. Rescanning org files...")
    (let ((total-nodes 0)
          (error-files '()))
      (dolist (dir org-supertag-sync-directories)
        (when (file-exists-p dir)
          (message "   Scanning directory: %s" dir)
          (let ((files (directory-files-recursively dir "\\.org$")))
            (dolist (file files)
              (condition-case err
                  (when (org-supertag-sync--in-sync-scope-p file)
                    (with-current-buffer (find-file-noselect file)
                      (let ((file-nodes 0))
                        (org-map-entries
                          (lambda ()
                            (when (org-id-get)
                              (condition-case err
                                  (when-let* ((props (org-supertag-db--parse-node-at-point)))
                                    (org-supertag-db-add (plist-get props :id) props)
                                    (cl-incf file-nodes)
                                    (cl-incf total-nodes))
                                (error
                                 (let ((id (org-id-get)))
                                   (message "Error processing node %s in %s: %s"
                                           id file (error-message-string err)))))))
                          t nil)
                        (when (> file-nodes 0)
                          (message "     %s: %d nodes" file file-nodes)))))
                (error
                 (push file error-files)
                 (message "     Error: %s - %s" file (error-message-string err))))))))

      (message "\n✓ Rebuild complete:")
      (message "   Total nodes: %d" total-nodes)
      (when error-files
        (message "   Error files: %d" (length error-files))))

    ;; 4. Rebuild sync state and hash baseline
    (message "\n4. Rebuilding sync infrastructure...")
    (org-supertag-sync-save-state)
    (when (fboundp 'org-supertag-background-sync-rebuild-baseline)
      (org-supertag-background-sync-rebuild-baseline))

    (message "\n=== Rebuild complete ===")
    (message "Database completely rebuilt from org files")))

(defun test-node-creation ()
  "Test if node creation meets requirements."
  (interactive)
  (when (and (derived-mode-p 'org-mode) (org-at-heading-p))
    (condition-case err
        (if-let* ((props (org-supertag-db--parse-node-at-point)))
            (progn
              (message "✓ Node parsed successfully!")
              (message "  ID: %s" (plist-get props :id))
              (message "  Title: %s" (plist-get props :title))
              (message "  File: %s" (plist-get props :file-path))
              (message "  Level: %s" (plist-get props :level))
              (message "  Position: %s" (plist-get props :pos))
              (message "  Outline path: %S" (plist-get props :olp))

              ;; Test adding to database
              (org-supertag-db-add (plist-get props :id) props)
              (message "✓ Node added to database successfully!")
              props)
          (message "❌ Could not parse current node (ID might be missing)"))
      (error
       (message "❌ Node processing failed: %s" (error-message-string err))
       nil))))

(provide 'fix-sync-cleanup-issue)