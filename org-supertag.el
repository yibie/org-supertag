;;; org-supertag.el --- SuperTag plugin for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie
;; Keywords: org-mode, tags, metadata, workflow, automation
;; Version: 3.0.0
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; URL: https://github.com/yibie/org-supertag

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-supertag is a powerful tagging system for Org mode that extends the
;; traditional tagging capabilities with advanced features:
;;
;; Core Features:
;; - Enhanced tag management with metadata and smart completion
;; - Dynamic tag behaviors and rules
;; - Flexible tag querying with history support
;; - Comprehensive node operations (move, delete, reference)
;; - Automatic tag synchronization
;; - Custom tag behaviors support

;;; Code:

(require 'org)
(require 'org-id)
(require 'deferred)
(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-relation)
(require 'org-supertag-field)
(require 'org-supertag-tag)
(require 'org-supertag-query)
(require 'org-supertag-behavior)
(require 'org-supertag-sync)
(require 'org-supertag-luhmann)
(require 'org-supertag-view)
(require 'org-supertag-inline)
(require 'org-supertag-sim-epc)
(require 'org-supertag-sim)
(require 'org-supertag-backlink)
(require 'org-supertag-recovery)

(defgroup org-supertag nil
  "Customization options for org-supertag."
  :group 'org
  :prefix "org-supertag-")

;;;###autoload
(define-minor-mode org-supertag-mode
  "Toggle org-supertag mode."
  :init-value nil
  :lighter " ST"
  :group 'org-supertag
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c t m") 'org-supertag-relation-manage)
            (define-key map (kbd "C-c t v") 'org-supertag-view-tag)
            map)
  (if org-supertag-mode
      (org-supertag--enable)
    (org-supertag--disable)))

;; Add initialization flag
(defvar org-supertag--initialized nil
  "Flag to track if org-supertag has been initialized.")

(defun org-supertag--initialize-id-system ()
  "Initialize org-id system properly."
  (require 'org-id)
  ;; Let org-id system handle initialization itself
  (unless (and (boundp 'org-id-locations)
               (or (hash-table-p org-id-locations)
                   (null org-id-locations)))
    ;; Ensure it's nil, let org-id-locations-load handle the conversion
    (setq org-id-locations nil))
  ;; If file exists and variable is nil, load it
  (when (and (null org-id-locations)
             (file-exists-p org-id-locations-file))
    (condition-case err
        (org-id-locations-load)
      (error
       (message "Failed to load org-id-locations: %s" 
                (error-message-string err))
       (setq org-id-locations (make-hash-table :test 'equal)))))
  ;; Ensure there's a valid hash table in the end
  (unless (hash-table-p org-id-locations)
    (setq org-id-locations (make-hash-table :test 'equal))))

(defun org-supertag--enable ()
  "Enable org-supertag."
  (unless org-supertag--initialized
    ;; 1. Initialize ID system
    (org-supertag--initialize-id-system)
    ;; 2. Ensure data directory exists
    (org-supertag-db-ensure-data-directory)
    ;; 3. Initialize database
    (org-supertag-db-init)
    ;; 4. Setup auto-save
    (org-supertag-db--setup-auto-save)
    ;; 5. Initialize sync system
    (org-supertag-sync-init)
    ;; 6. Initialize similarity system and EPC service
    (when (featurep 'org-supertag-sim)
      (condition-case err
          (progn
            (require 'org-supertag-sim)
            (org-supertag-sim-init))
        (error
         (message "Failed to initialize similarity system: %s"
                  (error-message-string err)))))
    ;; 7. Add hooks
    (add-hook 'kill-emacs-hook #'org-supertag-db-save)
    ;; Mark as initialized
    (setq org-supertag--initialized t)))

(defun org-supertag--disable ()
  "Disable org-supertag."
  ;; 1. Save database
  (org-supertag-db-save)
  ;; 2. Clean up auto-save timer
  (org-supertag-db--cleanup-auto-save)
  ;; 3. Clean up sync system
  (when org-supertag-sync--timer
    (cancel-timer org-supertag-sync--timer)
    (setq org-supertag-sync--timer nil))
  ;; 4. Stop EPC server
  (when (featurep 'org-supertag-sim-epc)
    (org-supertag-sim-epc-stop-server))
  ;; 5. Clear cache
  (org-supertag-db--cache-clear)
  ;; 6. Remove hooks
  (remove-hook 'kill-emacs-hook #'org-supertag-db-save)
  ;; Reset initialization flag
  (setq org-supertag--initialized nil))

(defun org-supertag-cleanup ()
  "Clean up org-supertag resources.
Used for manual cleanup or system state reset."
  (interactive)
  ;; 1. Save database
  (org-supertag-db-save)
  ;; 2. Clean up auto-save timer
  (org-supertag-db--cleanup-auto-save)
  ;; 3. Stop EPC server
  (when (featurep 'org-supertag-sim-epc)
    (org-supertag-sim-epc-stop-server))
  ;; 4. Clear cache
  (org-supertag-db--cache-clear)
  ;; 5. Reset dirty data flag
  (org-supertag-db--clear-dirty))

;;;###autoload
(defun org-supertag-setup ()
  "Setup org-supertag."
  (interactive)
  (unless org-supertag--initialized
    (org-supertag--enable)

    ;; Load custom behaviors first, so the registry is populated
    (let ((custom-file (expand-file-name "org-supertag-custom-behavior.el"
                                       org-supertag-data-directory)))
      (unless (file-exists-p custom-file)
        (unless (file-exists-p org-supertag-data-directory)
          (make-directory org-supertag-data-directory t))
        (when-let* ((template (locate-library "org-supertag-custom-behavior.el")))
          (copy-file template custom-file)
          (message "Created custom behaviors file at %s" custom-file)))
      (when (file-exists-p custom-file)
        (load custom-file)))

    ;; Now that custom behaviors are loaded, initialize and enable the behavior system
    ;; This will trigger org-supertag-behavior--init,
    ;; which in turn calls org-supertag-behavior--setup-scheduled-behaviors
    (org-supertag-behavior-mode 1)

    ;; Add org-supertag-mode to org-mode-hook
    (add-hook 'org-mode-hook #'org-supertag-mode))
  ;; It's good practice for setup functions to signal completion or success.
  (message "org-supertag setup complete."))

(provide 'org-supertag)

;;; org-supertag.el ends here
