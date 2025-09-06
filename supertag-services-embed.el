;;; supertag-services-embed.el --- DEBUGGING VERSION for DB-driven embed services -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-element)
(require 'supertag-core-store)
(require 'supertag-ops-node)
(require 'supertag-ops-embed)
(require 'supertag-ui-embed)
(require 'supertag-services-sync) 

;;; --- Core DB-driven Sync Logic (with extensive debugging) ---

(defun supertag-services-embed--update-node-in-db (source-id new-embed-content)
  "Update a node's content in the database from an embed block.
Title is NOT updated from the embed block in this new architecture."
  (message "DEBUG: (2a) Running update-node-in-db for %s" source-id)
  (when-let ((old-node-data (supertag-get (list :nodes source-id))))
    (let ((updated-node-data (cl-copy-list old-node-data)))
      ;; The entire block content is the new node content.
      (setf (plist-get updated-node-data :content) new-embed-content)
      ;; Use the existing transaction-safe operation to update the node
      (supertag-node-create updated-node-data)
      (message "DEBUG: (2c) DB update called via supertag-node-create."))
    (message "DEBUG: ERROR - Could not find node %s in DB during update." source-id)))

(defun supertag-services-embed--render-node-to-file (source-id)
  "Renders a node from the database to its source file."
  (message "DEBUG: (3a) Running render-node-to-file for %s" source-id)
  (if-let* ((node-data (supertag-get (list :nodes source-id)))
            (file-path (plist-get node-data :file)))
      (if (and file-path (file-exists-p file-path))
          (with-current-buffer (find-file-noselect file-path)
            (save-excursion
              (goto-char (point-min))
              (if (re-search-forward (format ":ID:[ \t]+%s" (regexp-quote source-id)) nil t)
                  (progn
                    (message "DEBUG: (3b) Found ID in file %s. Rendering..." file-path)
                    (org-back-to-heading t)
                    (let* ((element (org-element-at-point))
                           (start (org-element-property :begin element))
                           (end (org-element-property :end element)))
                      (delete-region start end)
                      (goto-char start)
                      (insert (supertag--generate-org-content (list node-data)))
                      (save-buffer)
                      (message "DEBUG: (3c) Buffer for %s saved." file-path)))
                (message "DEBUG: ERROR - Could not find ID %s in file %s" source-id file-path))))
        (message "DEBUG: ERROR - File path '%s' for node %s not found or invalid." file-path source-id))
    (message "DEBUG: ERROR - Could not find node %s in DB during render." source-id)))      

(defun supertag-embed-sync-modified-blocks ()
  "Sync all modified embed blocks in current buffer back to their sources."
  (message "DEBUG: (1a) Running supertag-embed-sync-modified-blocks on %s" (buffer-name))
  (save-excursion
    (goto-char (point-min))
    (let ((synced-count 0))
      (while (re-search-forward "^#\\+begin_embed:\\s-+\\([a-zA-Z0-9-]+\\)" nil t)
        (let* ((source-id (match-string 1))
               (inner-region (supertag-ui-embed-get-inner-block-region source-id))
               (current-content (and inner-region
                                     (buffer-substring-no-properties
                                      (car inner-region) (cdr inner-region)))))
          (message "DEBUG: (1b) Found embed block for ID %s" source-id)
          (if current-content
              (progn
                (message "DEBUG: (2) Calling DB update...")
                (supertag-services-embed--update-node-in-db source-id current-content)
                (message "DEBUG: (3) Calling render-to-file...")
                (supertag-services-embed--render-node-to-file source-id)
                (setq synced-count (1+ synced-count)))
            (message "DEBUG: (1c) No content found in embed block. Skipping."))))
      (if (> synced-count 0)
          (message "DEBUG: (4) Sync process finished for %d embed block(s)." synced-count)
        (message "DEBUG: (4) No embed blocks found or synced in this run."))
      synced-count)))

;;; --- Refresh Logic (Remains largely the same) ---

(defun supertag-services-embed-refresh-block (source-id)
  "Refresh the content of an embed block for the given source ID."
  (let ((refreshed nil))
    (when-let ((block-region (supertag-ui-embed-find-block-by-source source-id)))
      (let* ((inner-start (save-excursion (goto-char (car block-region)) (forward-line 1) (point)))
             (inner-end (save-excursion (goto-char (cdr block-region)) (re-search-backward "^#\\+end_embed" (car block-region) t) (point)))
             (new-content (supertag-ui-embed-generate-node-content source-id)))
        (when (and new-content (>= inner-end inner-start))
          (delete-region inner-start inner-end)
          (goto-char inner-start)
          (insert new-content)
          (unless (string-suffix-p "\n" new-content)
            (insert "\n"))
          (setq refreshed t))))
    refreshed))

(defun supertag-services-embed-refresh-all ()
  "Refresh all embed blocks in the current buffer."
  (interactive)
  (let ((refreshed-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+begin_embed:\\s-+\\([a-zA-Z0-9-]+\\)" nil t)
        (let ((source-id (match-string 1)))
          (when (supertag-services-embed-refresh-block source-id)
            (setq refreshed-count (1+ refreshed-count))))))
    (when (called-interactively-p 'interactive)
      (message "Refreshed %d embed blocks in current buffer" refreshed-count))
    refreshed-count))

;;; --- Save Hooks ---

(defun supertag-services-embed-on-source-save ()
  "Handle source file saves - refresh all embeds that reference this file."
  (let ((current-file (buffer-file-name)))
    (when current-file
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (and (buffer-file-name)
                       (not (string= (buffer-file-name) current-file)))
              (let ((source-ids (supertag-ops-embed-find-by-source-file current-file)))
                (dolist (source-id source-ids)
                  (supertag-services-embed-refresh-block source-id))))))))))

;;; --- System Integration ---

(defun supertag-services-embed-init ()
  "Initialize the embed services with save-based synchronization."
  (when (null supertag--store)
    (require 'supertag-core-persistence)
    (supertag-load-store)
    (message "Store loaded for embed services"))
  ;; Add save hooks for synchronization
  ;; This hook now handles the DB update and write-back to source file.
  (add-hook 'after-save-hook #'supertag-embed-sync-modified-blocks)
  ;; This hook handles refreshing embeds when a source file is saved directly.
  (add-hook 'after-save-hook #'supertag-services-embed-on-source-save)
  (message "DB-driven embed services initialized"))

(defun supertag-services-embed-cleanup ()
  "Cleanup embed services."
  (remove-hook 'after-save-hook #'supertag-embed-sync-modified-blocks)
  (remove-hook 'after-save-hook #'supertag-services-embed-on-source-save)
  (message "Embed services cleaned up"))

;; Note: Initialization is handled by the main org-supertag system
;; Do not auto-initialize to avoid circular dependencies

(provide 'supertag-services-embed)
;;; supertag-services-embed.el ends here
