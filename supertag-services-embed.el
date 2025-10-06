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
  ;;(message "DEBUG: (2a) Running update-node-in-db for %s" source-id)
  (when-let ((old-node-data (supertag-get (list :nodes source-id))))
    (let ((updated-node-data (cl-copy-list old-node-data)))
      ;; The entire block content is the new node content.
      (setf (plist-get updated-node-data :content) new-embed-content)
      ;; Use the existing transaction-safe operation to update the node
      (supertag-node-create updated-node-data)
      ;;(message "DEBUG: (2c) DB update called via supertag-node-create.")
      )
    ;;(message "DEBUG: ERROR - Could not find node %s in DB during update." source-id)
    ))

(defun supertag-services-embed--render-node-to-file (source-id)
  "Renders a node's CONTENT from the database to its source file.
Only updates the content section, preserving the headline and properties.
Uses find-file-noselect with minimal side effects."
  (when-let* ((node-data (supertag-get (list :nodes source-id)))
              (file-path (plist-get node-data :file)))
    (when (file-exists-p file-path)
      ;; Use find-file-noselect but suppress hooks that might interfere
      (let* ((inhibit-modification-hooks t)
             (target-buffer (let ((find-file-hook nil)        ; Disable file hooks
                                  (after-find-file nil))       ; Disable after-find-file
                              (find-file-noselect file-path))))
        (with-current-buffer target-buffer
          (when (eq major-mode 'org-mode)
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward (format ":ID:[ \t]+%s" (regexp-quote source-id)) nil t)
              ;; Find the end of the properties drawer
              (let ((props-end (save-excursion
                                 (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                                   (forward-line 1)
                                   (point)))))
                (when props-end
                  ;; Find the end of this node's content (before next heading or end of buffer)
                  (org-back-to-heading t)
                  (let* ((element (org-element-at-point))
                         (node-end (org-element-property :end element))
                         (new-content (plist-get node-data :content)))
                    ;; Delete old content (from end of properties to end of node)
                    (delete-region props-end node-end)
                    ;; Insert new content
                    (goto-char props-end)
                    (when (and new-content (not (string-empty-p new-content)))
                      (insert new-content)
                      (unless (string-suffix-p "\n" new-content)
                        (insert "\n")))
                    (save-buffer))))))))))))

(defun supertag-embed-sync-modified-blocks ()
  "Sync all modified embed blocks in current buffer back to their sources.
This function is designed to minimize interference with other packages."
  (let ((inhibit-modification-hooks t))  ; Prevent other hooks from firing
    (save-excursion
      (goto-char (point-min))
      (let ((synced-count 0))
        (while (re-search-forward "^#\\+begin_embed:\\s-+\\([a-zA-Z0-9-]+\\)" nil t)
          (let* ((source-id (match-string 1))
                 (inner-region (supertag-ui-embed-get-inner-block-region source-id))
                 (current-content (and inner-region
                                       (buffer-substring-no-properties
                                        (car inner-region) (cdr inner-region)))))
            (when current-content
              (supertag-services-embed--update-node-in-db source-id current-content)
              (supertag-services-embed--render-node-to-file source-id)
              (setq synced-count (1+ synced-count)))))
        synced-count))))

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
  "Handle source file saves - refresh all embeds that reference this file.
This function runs OUTSIDE of inhibit-modification-hooks to allow buffer updates."
  ;;(message "DEBUG-EMBED: [HOOK] on-source-save called for file: %s" (buffer-file-name))
  (let ((current-file (buffer-file-name)))
    (when current-file
      ;;(message "DEBUG-EMBED: Checking %d buffers for embed blocks" (length (buffer-list)))
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            ;;(message "DEBUG-EMBED: Checking buffer: %s, file: %s" (buffer-name) (buffer-file-name))
            (when (and (buffer-file-name)
                       (not (string= (buffer-file-name) current-file)))
              ;;(message "DEBUG-EMBED: Calling find-by-source-file for: %s" current-file)
              (let ((source-ids (supertag-ops-embed-find-by-source-file current-file)))
                ;;(message "DEBUG-EMBED: Found %d source-ids: %S" (length source-ids) source-ids)
                (when source-ids
                  ;;(message "DEBUG-EMBED: Refreshing %d embed blocks in %s" (length source-ids) (buffer-name))
                  (dolist (source-id source-ids)
                    ;;(message "DEBUG-EMBED: Refreshing block for source-id: %s" source-id)
                    (supertag-services-embed-refresh-block source-id)))))))))))

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
