;; org-supertag.el --- SuperTag plugin for Org mode -*- lexical-binding: t; -*-

(require 'org-supertag-db)
(require 'org-supertag-node) 
(require 'org-supertag-field)
(require 'org-supertag-tag)
(require 'org-supertag-query)
(require 'org-supertag-behavior)


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
  "Enable org-supertag."
  (message "\n=== Enabling org-supertag ===")
  ;; 1. Ensure database directory exists
  (org-supertag-db-ensure-data-directory)
  ;; 2. Initialize database
  (org-supertag-db-init)
  ;; 3. Setup auto-save
  (org-supertag-db--setup-auto-save)
  ;; 4. Add hooks
  (add-hook 'kill-emacs-hook #'org-supertag-db-save))

(defun org-supertag--disable ()
  "Disable org-supertag."
  (message "\n=== Disabling org-supertag ===")
  (message "DB state before disable: %S" (ht->alist org-supertag-db--object))
  
  ;; 1. Save database
  (org-supertag-db-save)
  ;; 2. Clean up auto-save timer
  (org-supertag-db--cleanup-auto-save)
  ;; 3. Clear cache
  (org-supertag-db--cache-clear)
  ;; 4. Remove hooks
  (remove-hook 'kill-emacs-hook #'org-supertag-db-save)
  
  (message "DB state after disable: %S" (ht->alist org-supertag-db--object)))

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
  "Setup org-supertag."
  (interactive)
  ;; 初始化数据库
  (org-supertag-db-init)
  ;; 初始化自定义行为文件
  (let ((custom-file (expand-file-name "org-supertag-custom-behavior.el"
                                     org-supertag-data-directory)))
    (unless (file-exists-p custom-file)
      (unless (file-exists-p org-supertag-data-directory)
        (make-directory org-supertag-data-directory t))
      
      (when-let ((template (locate-library "org-supertag-custom-behavior.el")))
        (copy-file template custom-file)
        (message "Created custom behaviors file at %s" custom-file)))
    
    ;; 加载自定义行为文件
    (when (file-exists-p custom-file)
      (load custom-file)))
  
  ;; 启用 org-supertag-mode
  (add-hook 'org-mode-hook #'org-supertag-mode))

(defun org-supertag--initialize ()
  "Initialize org-supertag system."
  (org-supertag-behavior-mode 1))

(add-hook 'org-supertag-mode-hook #'org-supertag--initialize)




(provide 'org-supertag)