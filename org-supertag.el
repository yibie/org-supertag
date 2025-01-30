;;; org-supertag.el --- SuperTag plugin for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie
;; Keywords: org-mode, tags, metadata, workflow, automation
;; Version: 2.2.3
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
;;
;; New in 2.0.0:
;; - Schedule trigger and deadline management system
;;   - Cron-style scheduling for behaviors ("minute hour day month weekday")
;;     Examples: " 0 9 * * 1-5" (weekdays at 9:00)
;;              "30 * * * *" (every hour at :30)
;;   - Deadline check behaviors (@deadline-check)
;;   - Overdue and upcoming deadline handling
;; - Enhanced node movement with link preservation
;; - Improved tag change functionality
;; - Automatic file synchronization system
;;   - Real-time buffer state tracking
;;   - Conflict detection and resolution
;;   - Robust error recovery
;; - Optimized query results using ewoc
;; - Async face refresh for better performance
;;
;; Latest Updates:
;; - Smart tag completion with TAB support
;; - Enhanced node management commands
;; - Query history with customizable size
;; - Improved reference tracking system
;;
;; For detailed usage and examples, see the README.org file in the project
;; repository.

;;; Code:

(require 'org-supertag-db)
(require 'org-supertag-node) 
(require 'org-supertag-field)
(require 'org-supertag-tag)
(require 'org-supertag-query)
(require 'org-supertag-behavior)
(require 'org-supertag-sync)
(require 'org-supertag-luhmann)


(defgroup org-supertag nil
  "Customization options for org-supertag."
  :group 'org
  :prefix "org-supertag-")

;; ;; Add SuperTag menu after Tools menu
;; (define-key-after
;;   global-map
;;   [menu-bar supertag]
;;   (cons "SuperTag" (make-sparse-keymap "SuperTag"))
;;   'tools)

;; ;; Node operations
;; (define-key global-map [menu-bar supertag query]
;;   '("Query Node" . org-supertag-node-query))
;; (define-key global-map [menu-bar supertag create]
;;   '("Create Node" . org-supertag-node-create))
;; (define-key global-map [menu-bar supertag move]
;;   '("Move Node" . org-supertag-node-move-node))
;; (define-key global-map [menu-bar supertag delete]
;;   '("Delete Node" . org-supertag-node-delete))

;; ;; Separator
;; (define-key global-map [menu-bar supertag sep1]
;;   '("--"))

;; ;; System operations
;; (define-key global-map [menu-bar supertag sync]
;;   '("Sync Nodes" . org-supertag-sync-nodes))

;; ;; Information display
;; (define-key global-map [menu-bar supertag info]
;;   '("Show Node Info" . org-supertag-node-info))
;; (define-key global-map [menu-bar supertag show-all]
;;   '("Show All Nodes" . org-supertag-node-show-all))
;; (define-key global-map [menu-bar supertag show-graph]
;;   '("Show Node Graph" . org-supertag-node-show-graph))

;;;###autoload
(define-minor-mode org-supertag-mode
  "Toggle org-supertag mode."
  :init-value nil
  :lighter " SuperTag"
  :group 'org-supertag
  :keymap (make-sparse-keymap)
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
    ;; 6. Add hooks
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
  ;; 3. Clear cache
  (org-supertag-db--cache-clear)
  ;; 4. Reset dirty data flag
  (org-supertag-db--clear-dirty))

;;;###autoload
(defun org-supertag-setup ()
  "Setup org-supertag."
  (interactive)
  (unless org-supertag--initialized
    (org-supertag--enable)
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
    (add-hook 'org-mode-hook #'org-supertag-mode)))

(defun org-supertag--initialize ()
  "Initialize org-supertag system."
  ;; Enable behavior system
  (org-supertag-behavior-mode 1))

(provide 'org-supertag)

;;; org-supertag.el ends here