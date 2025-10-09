  `emacs-lisp
  ;;; supertag-service-org.el --- Org Buffer Interaction Service -- lexical-binding: t; --

  ;;; Commentary:
  ;; This module provides high-level functions that correctly synchronize
  ;; changes by using robust, ID-based node location instead of stale
  ;; character positions.

  (require 'cl-lib)
  (require 'org)
  (require 'org-id) ;; Required for org-id-goto
  (require 'supertag-core-store)
  (require 'supertag-services-sync)
  (require 'supertag-ops-tag)
  (require 'supertag-ops-relation)
  (require 'supertag-ops-field)
  (require 'supertag-view-helper)

  (defun supertag-service-org--with-node-buffer (node-id func)
    "Find the buffer and position for NODE-ID using robust org-id-goto
  and execute FUNC there.
  Uses save-window-excursion to avoid disrupting user's view."
    (let* ((node-info (supertag-get `(:nodes ,node-id)))
           (file-path (plist-get node-info :file)))
      (when (and file-path (file-exists-p file-path))
        (save-window-excursion
          (let ((buffer (find-file-noselect file-path)))
            (with-current-buffer buffer
              (save-excursion
                ;; DO NOT trust the :position property. It can be stale.
                ;; Instead, use the canonical way to find a node by its ID.
                (org-id-goto node-id)
                (when (org-at-heading-p)
                  (funcall func)))))))))

  (defun supertag-service-org--update-buffer-and-resync (node-id buffer-update-func)
    "Generic function to run a buffer-updating function and then trigger a resync."
    (supertag-service-org--with-node-buffer node-id
      (lambda ()
        (funcall buffer-update-func)
        ;; Sync to update memory from the modified buffer
        (when (fboundp 'supertag-node-sync-at-point)
          (supertag-node-sync-at-point))
        (save-buffer)
        ;; Mark as internal modification AFTER saving
        (when (buffer-file-name)
          (supertag--mark-internal-modification (buffer-file-name))))))

  (defun supertag-service-org-set-todo-state (node-id state)
    "Sets the TODO state for NODE-ID in the buffer and triggers a resync."
    (supertag-service-org--update-buffer-and-resync node-id
      (lambda () (org-todo state))))

  (defun supertag-service-org-add-tag (node-id tag-name)
    "Adds #TAG-NAME text to the headline for NODE-ID and triggers a resync."
    (supertag-service-org--update-buffer-and-resync node-id
      (lambda ()
        (end-of-line)
        (insert (concat " #" tag-name)))))

  (defun supertag-service-org-remove-tag (node-id tag-name)
    "Removes #TAG-NAME text from the headline for NODE-ID and triggers a resync."
    (supertag-service-org--update-buffer-and-resync node-id
      (lambda ()
        (let ((tag-regexp (concat "\\s-?#" (regexp-quote tag-name) "\\b")))
          (when (re-search-forward tag-regexp (line-end-position) t)
            (replace-match ""))))))

  (provide 'supertag-service-org)
  ;;; supertag-service-org.el ends here