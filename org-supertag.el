;;; org-supertag.el --- SuperTag plugin for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie
;; Keywords: org-mode, tags, metadata, workflow, automation
;; Version: 4.6.0
;; Package-Requires: ((emacs "28.1") (org "9.6") (deferred "0.5.1"))
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

;; Package-Requires: ((emacs "28.1") (org "9.6") (deferred "0.5.1") (epc "0.1.1"))

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
(require 'org-supertag-scheduler)
(require 'org-supertag-behavior)
(require 'org-supertag-sync)
(require 'org-supertag-view)
(require 'org-supertag-inline)
(require 'org-supertag-bridge)
(require 'org-supertag-api)
(require 'org-supertag-recovery)
(require 'org-supertag-background-sync) 
(require 'org-supertag-auto-tag)
(require 'org-supertag-completion)
(require 'org-supertag-smart-companion)
(require 'org-supertag-embed)
;;(require 'org-supertag-workbench)

(defgroup org-supertag nil
  "Customization options for org-supertag."
  :group 'org
  :prefix "org-supertag-")

(defun org-supertag--log (format-string &rest args)
  "Log a message with the '[org-supertag]' prefix."
  (message "[org-supertag] %s" (apply #'format format-string args)))

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

(defvar org-supertag-project-root
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The root directory of the org-supertag project.
This is determined dynamically based on the location of this file.")

;;;###autoload
(define-minor-mode org-supertag-mode
  "Toggle org-supertag mode."
  :init-value nil
  :lighter " ST"
  :group 'org-supertag
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
        ;; The bridge is now started by `org-supertag-setup`.
        ;; This function's role is just to notify other systems
        ;; that the vectorization part of the setup is happening.
        ;; (org-supertag-bridge-start-process) ; <<< REMOVED
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
  "Enable the core, non-network parts of org-supertag."
  (unless org-supertag--initialized
    ;; 1. Initialize ID system
    (org-supertag--initialize-id-system)
    ;; 2. Ensure data directory exists
    (org-supertag-db-ensure-data-directory)
    ;; 3. Initialize database
    (org-supertag-db-init)
    ;; 4. Initialize sync system
    (when (fboundp 'org-supertag-sync-init)
      (org-supertag-sync-init))
    ;; 5. Initialize relation module
    (org-supertag-relation-init)
    ;; 6. Setup auto-save hook for Emacs exit
    (add-hook 'kill-emacs-hook #'org-supertag-db-save)

    ;; Mark as initialized
    (setq org-supertag--initialized t)
    (org-supertag--log "Core system initialized.")))

(defun org-supertag--disable ()
  "Disable org-supertag."
  (when org-supertag--initialized
    (org-supertag--log "Disabling Org SuperTag system...")
    ;; Try to save db before shutting down
    (org-supertag-db-save)
    (remove-hook 'kill-emacs-hook #'org-supertag-db-save)

    ;; Stop all services that might have been started
    (when (fboundp 'org-supertag-auto-tag-stop-silent-scan)
      (org-supertag-auto-tag-stop-silent-scan))
    (when (fboundp 'org-supertag-background-sync-stop)
      (org-supertag-background-sync-stop))
    (when (fboundp 'org-supertag-bridge-kill-process)
        (org-supertag-bridge-kill-process))
    (setq org-supertag--initialized nil)))

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
              (org-supertag--notify-backend-ready))
        (error
         (message "Failed to initialize vectorization system: %s" 
                  (error-message-string err)))))))

(defun org-supertag--setup ()
  "Internal setup function to configure hooks."
  (interactive)
  ;; Add org-supertag-mode to org-mode-hook (always, not conditional)
  (add-hook 'org-mode-hook #'org-supertag-mode)
  ;; It's good practice for setup functions to signal completion or success.
  (when (called-interactively-p 'any)
    (message "org-supertag setup complete - org-supertag-mode added to org-mode-hook")))

;;;###autoload
(defun org-supertag-setup ()
  "Initialize the full Org SuperTag system in the correct, sequential order.
This is the recommended entry point for user configurations.
It ensures that services depending on the Python bridge only start *after*
the bridge is confirmed to be ready."
  (interactive)

  ;; 1. Enable the core mode, which handles basic, local setup.
  (org-supertag-mode 1)

  ;; 2. Enable service-specific modes. This just loads them and makes them
  ;;    available, but does not start their timers or network activities.
  (when (fboundp 'org-supertag-auto-tag-mode)
    (org-supertag-auto-tag-mode 1))
  (when (fboundp 'org-supertag-background-sync-start)
    (require 'org-supertag-background-sync))
  
  ;; 3. Add the service start functions to the 'bridge ready' hook.
  ;;    These functions will be called automatically by the bridge
  ;;    once it has successfully connected to the Python backend.
  (add-hook 'org-supertag-bridge-ready-hook #'org-supertag-background-sync-start)
  (add-hook 'org-supertag-bridge-ready-hook #'org-supertag-scheduler-start)
  (add-hook 'org-supertag-bridge-ready-hook #'org-supertag-smart-companion-setup)
  
  ;; 4. Initialize embed functionality
  (when (fboundp 'org-supertag-embed-setup)
    (org-supertag-embed-setup))
  
  ;; 4. Finally, start the Python bridge process.
  ;;    Once ready, it will trigger the hook above.
  (org-supertag-bridge-start-process)

  (message "Org SuperTag setup initiated. Dependent services will start once Python bridge is ready."))

;; Auto-setup when loading the package
;; This ensures org-supertag-mode is automatically added to org-mode-hook
(org-supertag--setup)

(add-hook 'org-supertag-db-after-load-hook #'org-supertag-behavior-setup)

(provide 'org-supertag)

;;; org-supertag.el ends here
