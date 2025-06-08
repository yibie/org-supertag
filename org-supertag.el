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
(require 'org-supertag-bridge)
(require 'org-supertag-api)
(require 'org-supertag-backlink)
(require 'org-supertag-recovery)
(require 'org-supertag-proactive-engine)
(require 'org-supertag-background-sync)
(require 'org-supertag-bridge-sync)
(require 'org-supertag-auto-tag)
(require 'org-supertag-ui)

(defgroup org-supertag nil
  "Customization options for org-supertag."
  :group 'org
  :prefix "org-supertag-")

(defcustom org-supertag-enable-auto-vectorization t
  "Whether to automatically start the vectorization system.
If non-nil, the similarity system will be initialized in the background
after Emacs becomes idle. If nil, vectorization features will only be
available after manual initialization via `org-supertag-sim-init'."
  :type 'boolean
  :group 'org-supertag)

(defcustom org-supertag-vectorization-init-delay 10
  "Delay in seconds before starting vectorization system automatically.
Only used when `org-supertag-enable-auto-vectorization' is non-nil."
  :type 'integer
  :group 'org-supertag)

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

(defvar org-supertag--vectorization-init-timer nil
  "Timer for delayed vectorization system initialization.")

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

(defun org-supertag--schedule-vectorization-init ()
  "Schedule vectorization system initialization if auto-vectorization is enabled."
  (when org-supertag-enable-auto-vectorization
    (message "Scheduling vectorization system initialization...")
    (setq org-supertag--vectorization-init-timer
          (run-with-idle-timer 
           org-supertag-vectorization-init-delay
           nil
           #'org-supertag--init-vectorization-async))))

(defun org-supertag--init-vectorization-async ()
  "Asynchronously initialize the vectorization system."
  (condition-case err
      (progn
        (require 'org-supertag-bridge)
        (org-supertag-bridge-start-process)
        ;; Python后端现在已启动，通知背景同步系统
        (org-supertag--notify-backend-ready))
    (error
     (message "Failed to initialize vectorization system: %s" 
              (error-message-string err))
     (message "You can manually initialize it later with M-x org-supertag-init-vectorization"))))

(defun org-supertag--notify-backend-ready ()
  "通知系统Python后端已就绪。"
  (when (featurep 'org-supertag-background-sync)
    ;; 如果背景同步正在等待后端，现在可以继续了
    (when (eq org-supertag-background-sync--state :waiting-backend)
      (org-supertag-background-sync--check-backend))))

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
    ;; 6. Schedule vectorization system initialization (async)
    (org-supertag--schedule-vectorization-init)
    ;; 7. Initialize background sync systems
    (require 'org-supertag-background-sync)
    (when org-supertag-background-sync-auto-start
      (org-supertag-background-sync-start))
    ;; 8. Initialize and start the main vectorization sync timer
    (require 'org-supertag-bridge-sync)
    (when (and (boundp 'org-supertag-bridge-sync-interval)
               org-supertag-bridge-sync-interval)
      (org-supertag-bridge-sync-start))
    ;; 9. Add hooks
    (add-hook 'kill-emacs-hook #'org-supertag-db-save)
    ;; Mark as initialized
    (setq org-supertag--initialized t)
    ;; 统一的初始化完成消息
    (message "Org SuperTag system initialized")))

(defun org-supertag--disable ()
  "Disable org-supertag."
  ;; 1. Cancel vectorization init timer if pending
  (when org-supertag--vectorization-init-timer
    (cancel-timer org-supertag--vectorization-init-timer)
    (setq org-supertag--vectorization-init-timer nil))
  ;; 2. Save database
  (org-supertag-db-save)
  ;; 3. Clean up auto-save timer
  (org-supertag-db--cleanup-auto-save)
  ;; 4. Clean up sync system
  (when org-supertag-sync--timer
    (cancel-timer org-supertag-sync--timer)
    (setq org-supertag-sync--timer nil))
  ;; 5. Stop background sync systems
  (when (featurep 'org-supertag-background-sync)
    (org-supertag-background-sync-stop))
  (when (featurep 'org-supertag-bridge-sync)
    (org-supertag-bridge-sync-stop))
  ;; 6. Stop Python bridge process
  (org-supertag-bridge-kill-process)
  ;; 7. Clear cache
  (org-supertag-db--cache-clear)
  ;; 8. Remove hooks
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
  ;; 3. Stop Python bridge process
  (org-supertag-bridge-kill-process)
  ;; 4. Clear cache
  (org-supertag-db--cache-clear)
  ;; 5. Reset dirty data flag
  (org-supertag-db--clear-dirty))

;;;###autoload
(defun org-supertag-init-vectorization ()
  "Manually initialize the vectorization system.
This command can be used to start the vectorization system if automatic
initialization is disabled or failed."
  (interactive)
  (if (and (boundp 'org-supertag-bridge--ready-p)
           org-supertag-bridge--ready-p)
      (message "Vectorization system is already initialized.")
    (progn
      (require 'org-supertag-bridge)
      (when (called-interactively-p 'any)
        (message "Initializing vectorization system..."))
      (condition-case err
          (progn
              (org-supertag-bridge-start-process)
            (when (called-interactively-p 'any)
              (message "Vectorization system initialized successfully."))
            ;; 通知背景同步系统
            (org-supertag--notify-backend-ready))
        (error
         (message "Failed to initialize vectorization system: %s" 
                  (error-message-string err)))))))

(defun org-supertag--setup ()
  "Internal setup function to configure hooks."
  (interactive)
  (unless (featurep 'org-supertag)
    (progn
      ;; Add org-supertag-mode to org-mode-hook
      (add-hook 'org-mode-hook #'org-supertag-mode)))
  ;; It's good practice for setup functions to signal completion or success.
  (message "org-supertag setup complete.%s" 
           (if org-supertag-mode 
               " Mode is now active." 
             " Mode is inactive.")))

;;;###autoload
(defun org-supertag-setup ()
  "Initialize and activate the Org Supertag system components.
This includes the database, the proactive engine, and the background sync."
  (interactive)
  (org-supertag-db-init)
  (org-supertag-proactive-engine-activate)
  (org-supertag-background-sync-start)
  (when (featurep 'org-supertag-auto-tag)
    (org-supertag-auto-tag-mode 1))
  (message "Org SuperTag setup complete"))


(provide 'org-supertag)

;;; org-supertag.el ends here
