;;; org-supertag-migration.el --- Migrate standard Org tags to supertags -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides functionality to migrate standard Org mode tags (e.g., :tag:)
;; to org-supertag format (e.g., :#tag:) within the configured sync directories.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-supertag-sync) ; Need access to sync directories and helpers
(require 'cl-lib) ; For cl-lib functions like cl-loop, cl-incf

(defun org-supertag--get-files-in-scope ()
  "Return a list of all .org files within the sync scope.
Respects `org-supertag-sync-directories', `org-supertag-sync-exclude-directories',
and `org-supertag-sync-file-pattern'."
  (let ((files nil))
    (dolist (dir org-supertag-sync-directories files)
      (when (file-directory-p dir)
        (let* ((abs-dir (expand-file-name dir))
               (dir-files (directory-files-recursively abs-dir org-supertag-sync-file-pattern t)))
          (dolist (file dir-files)
            (when (and (file-regular-p file)
                       (string= (file-name-extension file) "org") ; Ensure it's an org file
                       (org-supertag-sync--in-sync-scope-p file)) ; Check includes/excludes
              (push file files)))))))
  (delete-dups files)) ; Return unique list

(defun org-supertag--collect-standard-tags (files)
  "Scan FILES and return a list of unique standard Org tags (without '#')."
  (let ((standard-tags (make-hash-table :test 'equal))
        (org-element-use-cache nil) ; Ensure fresh parsing
        (errors nil))
    (dolist (file files)
      (condition-case err
          (with-current-buffer (find-file-noselect file nil t) ; Visit quietly
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward org-heading-regexp nil t)
                (when (org-at-heading-p)
                  (dolist (tag (org-get-tags))
                    (unless (string-prefix-p "#" tag)
                      (puthash tag t standard-tags)))))))
        (error
         (push (format "Error scanning %s: %s" file (error-message-string err)) errors))))
    (when errors
      (message "Warnings during tag collection:\n%s" (mapconcat #'identity errors "\n")))
    (hash-table-keys standard-tags)))

(defun org-supertag-migrate-standard-tags-to-supertag ()
  "Migrate standard Org tags (:tag:) to supertags (:#tag:) globally.

Scans files defined in `org-supertag-sync-directories`, respecting
exclusions. Prompts the user to select which standard tags to
migrate.

WARNING: This function modifies Org files in place. BACK UP YOUR
FILES before proceeding."
  (interactive)

  (unless (and org-supertag-sync-directories (listp org-supertag-sync-directories))
    (user-error "Please configure `org-supertag-sync-directories` first."))

  (let* ((files (org-supertag--get-files-in-scope))
         (standard-tags (org-supertag--collect-standard-tags files)))

    (unless standard-tags
      (message "No standard Org tags found in the sync scope.")
      (cl-return-from org-supertag-migrate-standard-tags-to-supertag))

    (let* ((tags-to-migrate (completing-read-multiple
                             "Select standard tags to migrate to :#tag: format (SPC to mark, RET to confirm): "
                             standard-tags
                             nil t))) ; require-match=t

      (unless tags-to-migrate
        (message "No tags selected for migration. Aborting.")
        (cl-return-from org-supertag-migrate-standard-tags-to-supertag))

      (message "Selected tags for migration: %s" (mapconcat #'identity tags-to-migrate ", "))

      (unless (yes-or-no-p (format "WARNING: This will modify %d files potentially.\nMigrate %d selected tags (%s) to :#tag: format?\n*** PLEASE BACK UP YOUR FILES FIRST! *** Proceed? "
                                   (length files)
                                   (length tags-to-migrate)
                                   (mapconcat #'identity tags-to-migrate ", ")))
        (message "Migration aborted by user.")
        (cl-return-from org-supertag-migrate-standard-tags-to-supertag))

      ;; --- Execution Phase ---
      (let ((modified-files-count 0)
            (migrated-instances-count 0)
            (errors nil)
            (tags-to-migrate-hash (make-hash-table :test 'equal)))
        ;; Populate hash for quick lookup
        (dolist (tag tags-to-migrate) (puthash tag t tags-to-migrate-hash))

        (message "Starting migration...")
        (cl-loop for file in files
                 for idx from 1
                 do
                 (message "Processing file %d/%d: %s" idx (length files) (file-name-nondirectory file))
                 (condition-case err
                     (let ((buffer-modified nil))
                       (with-current-buffer (find-file-noselect file nil nil) ; Need to modify
                         (save-excursion
                           (goto-char (point-min))
                           ;; Use org-scan-tags for robust heading iteration and modification
                           (org-scan-tags
                            (lambda ()
                              (let* ((current-tags (org-get-tags))
                                     (new-tags nil)
                                     (changed-p nil))
                                (dolist (tag current-tags)
                                  (if (gethash tag tags-to-migrate-hash)
                                      (progn
                                        (push (concat "#" tag) new-tags)
                                        (cl-incf migrated-instances-count)
                                        (setq changed-p t))
                                    (push tag new-tags))) ; Keep existing #tags or non-selected tags
                                (when changed-p
                                  (org-set-tags (nreverse new-tags)) ; org-set-tags modifies buffer
                                  (setq buffer-modified t))))
                            ;; Scope: only tags in headings
                            '(:scope headline :target plain))
                           (when buffer-modified
                             (basic-save-buffer)
                             (cl-incf modified-files-count))))
                       ;; Kill buffer after processing
                       (unless (or (eq (current-buffer) (get-buffer file)) ; Don't kill if it was already open interactively
                                   (memq (current-buffer) (buffer-list))) ; Check if buffer still exists
                         (kill-buffer (current-buffer))))
                   (error
                    (push (format "Error processing %s: %s" file (error-message-string err)) errors))))

        ;; --- Reporting Phase ---
        (message "Migration complete.")
        (message "Migrated %d tag instances for %d selected tags across %d files."
                 migrated-instances-count
                 (length tags-to-migrate)
                 modified-files-count)
        (when errors
          (warn "Encountered %d errors during migration:" (length errors))
          (dolist (err-msg errors) (message "- %s" err-msg))
          (message "Please check the affected files manually."))))))

(provide 'org-supertag-migration)

;;; org-supertag-migration.el ends here 