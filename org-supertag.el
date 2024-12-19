;; org-supertag.el --- SuperTag plugin for Org mode -*- lexical-binding: t; -*-

(require 'org-supertag-db)
(require 'org-supertag-node) 
(require 'org-supertag-field)
(require 'org-supertag-tag)
(require 'org-supertag-query)

(defgroup org-supertag nil
  "Customization options for org-supertag."
  :group 'org
  :prefix "org-supertag-")

;;;###autoload
(define-minor-mode org-supertag-mode
  "Toggle org-supertag mode."
  :init-value nil
  :lighter " SuperTag"
  :group 'org-supertag
  (if org-supertag-mode
      (org-supertag--enable)
    (org-supertag--disable)))

(defun org-supertag--enable ()
  "Enable org-supertag.
1. Ensure database directory exists
2. Initialize database
3. Setup auto-save
4. Add necessary hooks"
  ;; 1. Ensure database directory exists
  (org-supertag-db-ensure-data-directory)
  ;; 2. Initialize database
  (org-supertag-db-init)
  ;; 3. Setup auto-save
  (org-supertag-db--setup-auto-save)
  ;; 4. Add hooks
  (add-hook 'kill-emacs-hook #'org-supertag-db-save))

(defun org-supertag--disable ()
  "Disable org-supertag.
1. Save database
2. Clean up auto-save timer
3. Clear cache
4. Remove hooks"
  ;; 1. Save database
  (org-supertag-db-save)
  ;; 2. Clean up auto-save timer
  (org-supertag-db--cleanup-auto-save)
  ;; 3. Clear cache
  (org-supertag-db--cache-clear)
  ;; 4. Remove hooks
  (remove-hook 'kill-emacs-hook #'org-supertag-db-save))

(defun org-supertag-cleanup ()
  "Clean up org-supertag resources.
Used for manual cleanup or system state reset."
  (interactive)
  ;; 1. Save database
  (org-supertag-db-save)
  ;; 2. Clean up auto-save timer
  (org-supertag-db--cleanup-auto-save)
  ;; 3. Clear cache
  (org-supertag-db--cache-clear)
  ;; 4. Reset dirty data flag
  (org-supertag-db--clear-dirty))

;;;###autoload
(defun org-supertag-setup ()
  "Setup org-supertag.
Automatically enables org-supertag-mode in org-mode buffers."
  (interactive)
  (org-supertag-db-init)
  ;; Enable org-supertag-mode for all org-mode buffers
  (add-hook 'org-mode-hook #'org-supertag-mode))

(provide 'org-supertag)