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

;; Load org-supertag after gptel is loaded
;; --- Core Components ---
(require 'supertag-core-store)
(require 'supertag-core-persistence) ; Add persistence module for data loading/saving
(require 'supertag-core-schema)
(require 'supertag-core-transform)
(require 'supertag-core-notify) ; Renamed from supertag-notify for clarity

;; --- Entity Operations (ops) ---
(require 'supertag-ops-node)
(require 'supertag-ops-tag)
(require 'supertag-ops-field)
(require 'supertag-ops-relation)
(require 'supertag-ops-batch)
(require 'supertag-ops-embed)

;; --- Automation System 2.0 ---
(require 'supertag-automation)

;; --- Performance Optimizations ---
(require 'supertag-performance-optimizer)

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

;; --- View ---
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
    (message "Initializing Org-Supertag system...")
    ;; Ensure data directories exist
    (supertag-persistence-ensure-data-directory)
    ;; Load sync state
    (supertag-sync-load-state)
    ;; Load data from persistent storage
    (supertag-load-store)
    ;; Set up auto-save
    (supertag-setup-auto-save)
    ;; Start auto-sync
    (supertag-sync-start-auto-sync)
    ;; Initialize embed services
    (when (fboundp 'supertag-services-embed-init)
      (supertag-services-embed-init))
    ;; Start scheduler
    (supertag-scheduler-start)
    ;; Add a watcher to debug unexpected changes to the store
    (add-variable-watcher 'supertag--store (lambda (sym newval op where) (debug)))
    (message "Org-Supertag system initialized."))

;; --- Hooks for persistence ---
(add-hook 'kill-emacs-hook #'supertag-save-store)
(add-hook 'kill-emacs-hook #'supertag-sync-save-state) ; Save sync state on exit
(add-hook 'kill-emacs-hook #'supertag-sync-stop-auto-sync) ; Stop auto-sync on exit
(add-hook 'after-init-hook #'supertag-init) ; Initialize on Emacs start, after basic init
(add-hook 'org-mode-hook #'supertag-sync-setup-realtime-hooks) ; Add sync hook to all org buffers

(provide 'org-supertag)

;;; org-supertag/supertag.el ends here
