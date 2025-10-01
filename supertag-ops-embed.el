;;; supertag-ops-embed.el --- Embed operations for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides embed-related operations for the org-supertag system.
;; It handles finding embed blocks by source files and other embed operations.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store)
(require 'supertag-services-query)

(defun supertag-ops-embed-find-by-source-file (source-file)
  "Find all embed blocks that reference nodes from SOURCE-FILE.
Returns a list of source-ids (node IDs) that have embed blocks referencing them.
This function searches all open buffers for embed blocks that reference nodes from the source file."
  (let ((source-ids '())
        (nodes-in-file nil))
    (when (and source-file (file-exists-p source-file))
      ;; First, find all nodes that belong to this source file
      (let ((file-nodes (supertag-find-nodes-by-file source-file)))
        (setq nodes-in-file (mapcar #'car file-nodes))) ; Extract node IDs
      
      ;; Then, search all open buffers for embed blocks referencing these nodes
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (buffer-file-name)
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "^#\\+begin_embed:\\s-+\\([a-zA-Z0-9-]+\\)" nil t)
                  (let ((embed-id (match-string 1)))
                    ;; Check if this embed-id is one of the nodes from source-file
                    (when (member embed-id nodes-in-file)
                      (push embed-id source-ids)))))))))
      
      ;; Return unique list of source-ids
      (cl-delete-duplicates source-ids :test #'equal))))

(provide 'supertag-ops-embed)
;;; supertag-ops-embed.el ends here