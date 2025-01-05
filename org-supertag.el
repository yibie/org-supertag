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

(defun org-supertag--initialize-id-system ()
  "Initialize org-id system properly."
  (require 'org-id)
  ;; Let the org-id system handle its own initialization
  (unless (and (boundp 'org-id-locations)
               (or (hash-table-p org-id-locations)
                   (null org-id-locations)))
    ;; Ensure it is nil, allowing org-id-locations-load to handle the conversion
    (setq org-id-locations nil))
  
  ;; If the file exists and the variable is nil, load it
  (when (and (null org-id-locations)
             (file-exists-p org-id-locations-file))
    (condition-case err
        (org-id-locations-load)
      (error
       (message "Failed to load org-id-locations: %s" 
                (error-message-string err))
       (setq org-id-locations (make-hash-table :test 'equal)))))
  
  ;; Ensure there is a valid hash table in the end
  (unless (hash-table-p org-id-locations)
    (setq org-id-locations (make-hash-table :test 'equal))))

(defun org-supertag--enable ()
  "Enable org-supertag."
  ;; 1. Initialize the ID system
  (org-supertag--initialize-id-system)
  
  ;; 2. Ensure the data directory exists
  (org-supertag-db-ensure-data-directory)
  ;; 3. Initialize database
  (org-supertag-db-init)
  ;; 4. Setup auto-save
  (org-supertag-db--setup-auto-save)
  ;; 5. Add hooks
  (add-hook 'kill-emacs-hook #'org-supertag-db-save)
  (add-hook 'org-after-refile-insert-hook 
            #'org-supertag-node--after-refile-update-ids))

(defun org-supertag--disable ()
  "Disable org-supertag."
  ;; 1. Save database
  (org-supertag-db-save)
  ;; 2. Clean up auto-save timer
  (org-supertag-db--cleanup-auto-save)
  ;; 3. Clean up ID tracking
  (org-supertag-node--cleanup-id-tracking)
  ;; 4. Clear cache
  (org-supertag-db--cache-clear)
  ;; 5. Remove hooks
  (remove-hook 'kill-emacs-hook #'org-supertag-db-save)
  (remove-hook 'org-after-refile-insert-hook #'org-supertag-node--after-refile-update-ids))

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
  (org-supertag-db-init)
  (let ((custom-file (expand-file-name "org-supertag-custom-behavior.el"
                                     org-supertag-data-directory)))
    (unless (file-exists-p custom-file)
      (unless (file-exists-p org-supertag-data-directory)
        (make-directory org-supertag-data-directory t)) 
      (when-let ((template (locate-library "org-supertag-custom-behavior.el")))
        (copy-file template custom-file)
        (message "Created custom behaviors file at %s" custom-file)))
    (when (file-exists-p custom-file)
      (load custom-file)))
  (add-hook 'org-mode-hook #'org-supertag-mode))

(defun org-supertag--initialize ()
  "Initialize org-supertag system."
  ;; Enable the behavior system
  (org-supertag-behavior-mode 1))

(add-hook 'org-supertag-mode-hook #'org-supertag--initialize)




(provide 'org-supertag)
