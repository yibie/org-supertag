;;; org-supertag.el --- SuperTag plugin for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie
;; Keywords: org-mode, tags, metadata, workflow, automation
;; Version: 5.0.0
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
;; traditional tagging capabilities with advanced features

;; Package-Requires: ((emacs "28.1") (org "9.6") (ht "2.4") (gptel "0.9.8"))

;;; Code:


(require 'cl-lib)
(require 'org)
(require 'org-id)

(defgroup org-supertag nil
  "Core configuration for Org-Supertag."
  :group 'org)

(defcustom supertag-data-directory
  (expand-file-name "org-supertag" user-emacs-directory)
  "Directory for storing Org-Supertag data."
  :type 'directory
  :group 'org-supertag)

(defcustom supertag-project-root
  (file-name-directory (file-name-directory (or load-file-name buffer-file-name)))
  "The root directory of the org-supertag project."
  :type 'directory
  :group 'org-supertag)

;; --- Core Components ---
(require 'ht) ; Ensure ht is loaded before other modules that might depend on it
(require 'supertag-core-store)
(require 'supertag-core-scan)
(require 'supertag-core-persistence)
(require 'supertag-core-schema)
(require 'supertag-core-transform)
(require 'supertag-core-notify)

;; --- Entity Operations (ops) ---
(require 'supertag-ops-node)
(require 'supertag-ops-tag)
(require 'supertag-ops-field)
(require 'supertag-ops-relation)
(require 'supertag-ops-schema)
(require 'supertag-ops-batch)
(require 'supertag-ops-embed)

;; --- Automation System ---
(require 'supertag-automation-sync)
(require 'supertag-automation)

;; --- Service Functions (services) ---
(require 'supertag-services-query)
(require 'supertag-services-sync)
(require 'supertag-services-ui)
(require 'supertag-services-capture)
(require 'supertag-services-embed)
(require 'supertag-services-scheduler)


;; --- User Interface (ui) ---
(require 'supertag-ui-commands)
(require 'supertag-ui-chat)
(require 'supertag-ui-embed)
(require 'supertag-ui-query-block)
(require 'supertag-ui-search)
(require 'supertag-ui-completion)

;; --- View ---
(require 'supertag-view-schema)
(require 'supertag-view-helper)
(require 'supertag-view-node)
(require 'supertag-view-table)
(require 'supertag-view-kanban)

;; --- RAG ---
(require 'supertag-rag)

;; --- Migration ---
(require 'supertag-migration)

;; --- Compat ---
(require 'supertag-compat)

;; --- Initialization ---
(defun supertag-init ()
 "Initialize the Org-Supertag system.
This function loads all necessary components and sets up the environment."
    (interactive)
    
    ;; Step 1: Ensure data directories exist
    (supertag-persistence-ensure-data-directory)
    
    ;; Step 2: Check critical configuration before loading data
    (supertag--check-critical-config)
    
    ;; Step 3: Load sync state
    (supertag-sync-load-state)
    
    ;; Step 4: Load data from persistent storage
    (supertag-load-store)
    
    ;; Step 5: Validate loaded data and sync directories
    (supertag--validate-initialization)
    
    ;; Step 6: Materialize tag schema cache after loading data
    (supertag-ops-schema-rebuild-cache)
    
    ;; Step 7: Set up auto-save and daily backup timers
    (supertag-setup-all-timers)
    
    ;; Step 8: Schedule safe auto-start for sync (optional, guarded)
    (when (and (boundp 'supertag-sync-auto-start)
               supertag-sync-auto-start)
      (supertag-sync-schedule-auto-start))
    
    ;; Step 9: Initialize embed services
    (when (fboundp 'supertag-services-embed-init)
      (supertag-services-embed-init))
    
    ;; Step 10: Start scheduler
    (supertag-scheduler-start)
    
    ;; Step 11: Enable completion globally
    (global-supertag-ui-completion-mode 1)

    ;; Step 12: Enable completion in already-open org buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-mode)
          (supertag-ui-completion-mode 1)))))

  ;; Optionally auto-show Node View side window and follow context
  (when (and (boundp 'supertag-view-node-auto-show)
             supertag-view-node-auto-show
             (fboundp 'supertag-view-node-ensure-shown))
    (supertag-view-node-ensure-shown))

(defun supertag--check-critical-config ()
  "Check critical configuration before initialization.
Warn user if important settings are missing or incorrect."
  ;; Check if sync directories are configured
  (unless org-supertag-sync-directories
    (display-warning 'org-supertag
                     "org-supertag-sync-directories is not configured!\n\
This means no files will be synchronized automatically.\n\
Please set this variable in your Emacs configuration, for example:\n\
  (setq org-supertag-sync-directories '(\"/path/to/your/notes\"))"
                     :warning))
  
  ;; Check if configured directories exist
  (when org-supertag-sync-directories
    (dolist (dir org-supertag-sync-directories)
      (unless (file-directory-p dir)
        (display-warning 'org-supertag
                         (format "Configured sync directory does not exist: %s\n\
Please check your org-supertag-sync-directories configuration." dir)
                         :warning)))))

(defun supertag--validate-initialization ()
  "Validate initialization state and provide helpful diagnostics."
  (let* ((store-is-valid (and (hash-table-p supertag--store)
                              (> (hash-table-count supertag--store) 0)))
         (nodes-table (when store-is-valid
                        (gethash :nodes supertag--store)))
         (node-count (if (hash-table-p nodes-table)
                         (hash-table-count nodes-table)
                       0))
         (db-file supertag-db-file)
         (db-exists (file-exists-p db-file))
         (db-size (when db-exists (file-attribute-size (file-attributes db-file)))))
    
    ;; Report database status
    (message "Database status: %s, Size: %s bytes, Nodes: %d"
             (if db-exists "exists" "NEW")
             (if db-size db-size "N/A")
             node-count)
    
    ;; Warn if database is empty but should have data
    ;; Only warn if store itself is invalid or truly empty (no collections at all)
    (when (and db-exists
               (> db-size 100)  ; Non-trivial file size
               (not store-is-valid)  ; Store is invalid or empty
               (= node-count 0))
      (display-warning 'org-supertag
                       (format "Database file exists but contains no nodes!\n\
Database: %s\n\
This may indicate:\n\
1. Database corruption or format issues\n\
2. All nodes were deleted or marked as orphaned\n\
3. Sync directories configuration changed\n\n\
Consider running: M-x supertag-sync-full-rescan" db-file)
                       :warning))
    
    ;; Suggest initial sync if database is truly empty
    (when (and (= node-count 0)
               org-supertag-sync-directories
               (cl-some #'file-directory-p org-supertag-sync-directories))
      (message "Database is empty. Consider running: M-x supertag-sync-full-rescan"))))
 
;; --- Hooks for persistence ---
(add-hook 'kill-emacs-hook #'supertag-save-store)
(add-hook 'kill-emacs-hook #'supertag-cleanup-all-timers) ; Clean up all timers on exit
(add-hook 'kill-emacs-hook #'supertag-sync-save-state) ; Save sync state on exit
(add-hook 'kill-emacs-hook #'supertag-sync-stop-auto-sync) ; Stop auto-sync on exit
(add-hook 'emacs-startup-hook #'supertag-init)
(add-hook 'org-mode-hook #'supertag-sync-setup-realtime-hooks)

(provide 'org-supertag)

;;; org-supertag.el ends here

 
