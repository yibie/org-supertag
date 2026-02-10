;;; org-supertag.el --- SuperTag plugin for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie
;; Keywords: org-mode, tags, metadata, workflow, automation
;; Version: 5.7.0
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
(require 'subr-x)

(defgroup org-supertag nil
  "Core configuration for Org-Supertag."
  :group 'org)

(defcustom supertag-data-directory
  (expand-file-name "org-supertag" user-emacs-directory)
  "Directory for storing Org-Supertag data."
  :type 'directory
  :group 'org-supertag)

(defvar supertag--base-data-directory
  (file-name-as-directory (expand-file-name supertag-data-directory))
  "Base data directory for vault storage.

This stays constant even when `supertag-data-directory` is switched per vault.")

(defcustom org-supertag-active-sync-directory nil
  "Active vault root directory when `org-supertag-sync-directories` lists multiple roots.

This is only used when `org-supertag-sync-directories-mode` is `vaults`.
The value may include `~`; it will be normalized internally."
  :type '(choice (const :tag "First directory" nil)
                 directory)
  :group 'org-supertag)

(defcustom org-supertag-vault-auto-switch nil
  "When non-nil, automatically switch the active vault for Org buffers.

If enabled, entering an Org buffer will activate the matching vault (by file path),
which includes loading that vault's DB/state and restarting auto-sync for it.

Default is nil to avoid unexpected IO and model reload costs during navigation."
  :type 'boolean
  :group 'org-supertag)

(defcustom org-supertag-vault-modeline-indicator t
  "When non-nil, show the matched vault name in the mode line for Org buffers.

This does not switch the active vault; it only displays which vault the current
file belongs to (based on its path)."
  :type 'boolean
  :group 'org-supertag)

(defvar supertag-vault--current nil
  "Currently active vault plist (normalized).")

(defvar-local supertag-vault--buffer-indicator nil
  "Cached mode line indicator for the current buffer.")

(define-minor-mode supertag-vault-indicator-mode
  "Show Org-Supertag vault indicator in the mode line."
  :init-value nil
  :lighter (:eval (or supertag-vault--buffer-indicator "")))

(defvar supertag--initialized nil
  "Non-nil after `supertag-init` completes.")

(defvar supertag--config-guard-enabled nil
  "When non-nil, prevent manual runtime changes to vault persistence variables.")

(defvar supertag--config-guard-allow nil
  "When non-nil, allow guarded variable updates for vault switching.")

(defvar supertag--config-guard--reverting nil
  "Internal guard flag used while reverting blocked config changes.")

(defvar supertag--config-guard-state nil
  "Expected runtime values for vault persistence variables.")

(defun supertag-config-guard--key (symbol)
  "Return guard key for SYMBOL, or nil when untracked."
  (pcase symbol
    ('supertag-data-directory :data-directory)
    ('supertag-db-file :db-file)
    ('supertag-db-backup-directory :backup-directory)
    ('supertag-sync-state-file :sync-state-file)
    ('org-supertag-sync-directories :sync-directories)
    ('org-supertag-active-sync-directory :active-sync-directory)
    (_ nil)))

(defun supertag-config-guard--capture ()
  "Capture current vault persistence settings."
  (setq supertag--config-guard-state
        (list :data-directory supertag-data-directory
              :db-file supertag-db-file
              :backup-directory supertag-db-backup-directory
              :sync-state-file (when (boundp 'supertag-sync-state-file)
                                 supertag-sync-state-file)
              :sync-directories (when (boundp 'org-supertag-sync-directories)
                                  org-supertag-sync-directories)
              :active-sync-directory (when (boundp 'org-supertag-active-sync-directory)
                                       org-supertag-active-sync-directory))))

(defun supertag-config-guard--update (symbol newval)
  "Update guard state for SYMBOL to NEWVAL."
  (let ((key (supertag-config-guard--key symbol)))
    (when key
      (setq supertag--config-guard-state
            (plist-put supertag--config-guard-state key newval)))))

(defun supertag-config-guard--watch (symbol newval operation _where)
  "Block manual runtime changes to guarded variables."
  (when (and supertag--config-guard-enabled
             supertag--initialized
             (memq operation '(set let))
             (not supertag--config-guard--reverting))
    (let ((key (supertag-config-guard--key symbol)))
      (when key
        (if supertag--config-guard-allow
            (supertag-config-guard--update symbol newval)
          (let ((expected (plist-get supertag--config-guard-state key)))
            (unless (equal newval expected)
              (let ((supertag--config-guard--reverting t))
                (set symbol expected))
              (message "Supertag: manual config change blocked. Use M-x supertag-vault-activate to switch vaults."))))))))

(defun supertag-config-guard-enable ()
  "Enable runtime guard for vault persistence variables."
  (supertag-config-guard--capture)
  (unless supertag--config-guard-enabled
    (setq supertag--config-guard-enabled t)
    (when (fboundp 'add-variable-watcher)
      (dolist (var '(supertag-data-directory
                     supertag-db-file
                     supertag-db-backup-directory
                     supertag-sync-state-file
                     org-supertag-sync-directories
                     org-supertag-active-sync-directory))
        (add-variable-watcher var #'supertag-config-guard--watch)))))

(defmacro supertag-config-guard--with-allow (&rest body)
  "Execute BODY while allowing guarded config changes."
  (declare (indent 0))
  `(let ((supertag--config-guard-allow t))
     ,@body))

(defun supertag-vault--normalize-path (path)
  "Return a canonical directory PATH for matching and IO."
  (when (and (stringp path) (not (string-empty-p path)))
    (file-name-as-directory
     (file-truename (expand-file-name path)))))

(defun supertag-vault--sanitize-name (name)
  "Return filesystem-friendly NAME."
  (let ((s (or name "")))
    (setq s (downcase s))
    (setq s (replace-regexp-in-string "[^[:alnum:]_.-]+" "-" s))
    (setq s (replace-regexp-in-string "^-+" "" s))
    (setq s (replace-regexp-in-string "-+$" "" s))
    (if (string-empty-p s) "vault" s)))

(defun supertag-vault--id (vault)
  "Return stable identifier string for VAULT."
  (let* ((root (plist-get vault :root))
         (name (plist-get vault :name))
         (root-norm (and root (supertag-vault--normalize-path root)))
         (base (supertag-vault--sanitize-name (or name "vault")))
         (suffix (when root-norm (substring (secure-hash 'sha1 root-norm) 0 10))))
    (if suffix (format "%s-%s" base suffix) base)))

(defun supertag-vault--normalize-vault-root (root)
  "Normalize ROOT directory into a vault plist."
  (let* ((root-norm (supertag-vault--normalize-path root)))
    (when root-norm
      (let* ((name (file-name-nondirectory (directory-file-name root-norm)))
             (vault (list :name (or name "vault") :root root-norm))
             (id (supertag-vault--id vault))
             (base (or supertag--base-data-directory
                       (file-name-as-directory (expand-file-name supertag-data-directory))))
             (data-dir (file-name-as-directory
                        (expand-file-name (format "vaults/%s" id) base))))
        (list :id id :name (plist-get vault :name) :root root-norm :data-directory data-dir)))))

(defun supertag-vault--vault-mode-p ()
  "Return non-nil when sync directories are treated as separate vaults."
  (and (boundp 'org-supertag-sync-directories-mode)
       (eq org-supertag-sync-directories-mode 'vaults)))

(defun supertag-vault--normalized-vaults ()
  "Return list of normalized vaults."
  (when (and (supertag-vault--vault-mode-p) (listp org-supertag-sync-directories))
    (delq nil (mapcar #'supertag-vault--normalize-vault-root org-supertag-sync-directories))))

(defun supertag-vault--find-by-root (root)
  "Find normalized vault by ROOT directory."
  (let ((target (supertag-vault--normalize-path root)))
    (when target
      (cl-find-if (lambda (v) (string= (plist-get v :root) target))
                  (supertag-vault--normalized-vaults)))))

(defun supertag-vault--find-by-file (file)
  "Find the best matching vault for FILE (longest root prefix)."
  (when (and (stringp file) (not (string-empty-p file)))
    (let* ((file-norm (condition-case nil
                          (file-truename (expand-file-name file))
                        (error (expand-file-name file))))
           (best nil)
           (best-len -1))
      (dolist (vault (supertag-vault--normalized-vaults))
        (let* ((root (plist-get vault :root))
               (root-len (length root)))
          (when (and (string-prefix-p root file-norm)
                     (> root-len best-len))
            (setq best vault)
            (setq best-len root-len))))
      best)))

(defun supertag-vault--apply (vault)
  "Apply VAULT persistence and sync configuration without loading data."
  (supertag-config-guard--with-allow
    (let* ((data-dir (file-name-as-directory (plist-get vault :data-directory))))
      (setq supertag-data-directory data-dir)
      (setq supertag-db-file (expand-file-name "supertag-db.el" data-dir))
      (setq supertag-db-backup-directory (expand-file-name "backups" data-dir))
      (setq supertag-sync-state-file (expand-file-name "sync-state.el" data-dir))
      ;; Backup date is per-vault; reset to allow correct daily backup decisions.
      (when (boundp 'supertag-db--last-backup-date)
        (setq supertag-db--last-backup-date nil))))
  (supertag-config-guard--capture))

(defun supertag-vault--current-id ()
  "Return current vault ID or nil."
  (plist-get supertag-vault--current :id))

(defun supertag-vault--effective-root ()
  "Return the active vault root directory, or nil when not in vault mode."
  (when (supertag-vault--vault-mode-p)
    (let ((vaults (supertag-vault--normalized-vaults)))
      (cond
       ((and org-supertag-active-sync-directory
             (supertag-vault--find-by-root org-supertag-active-sync-directory))
        (plist-get (supertag-vault--find-by-root org-supertag-active-sync-directory) :root))
       ((and (consp vaults) (plist-get (car vaults) :root))
        (plist-get (car vaults) :root))
       (t nil)))))

;;;###autoload
(defun org-supertag--effective-sync-directories ()
  "Return effective sync directories for the current session.

In vault mode, returns a single-element list containing the active vault root.
Otherwise, returns `org-supertag-sync-directories` unchanged."
  (if (supertag-vault--vault-mode-p)
      (let ((root (supertag-vault--effective-root)))
        (when root (list root)))
    org-supertag-sync-directories))

(defun supertag-vault--persist-current ()
  "Persist current vault state/store best-effort."
  (ignore-errors
    (when (fboundp 'supertag-sync-save-state)
      (supertag-sync-save-state)))
  (ignore-errors
    (when (fboundp 'supertag-save-store)
      (supertag-save-store))))

(defun supertag-vault--buffer-vault-name (&optional file)
  "Return vault name that FILE belongs to, or nil."
  (let* ((file (or file (buffer-file-name)))
         (vault (and file (supertag-vault--find-by-file file))))
    (plist-get vault :name)))

(defun supertag-vault--update-buffer-indicator ()
  "Update `supertag-vault--buffer-indicator` for the current buffer."
  (setq supertag-vault--buffer-indicator nil)
  (when (and org-supertag-vault-modeline-indicator
             (listp org-supertag-sync-directories)
             (> (length org-supertag-sync-directories) 1))
    (let ((name (supertag-vault--buffer-vault-name)))
      (setq supertag-vault--buffer-indicator
            (if name
                (format " ST[%s]" name)
              " ST[-]"))))
  (force-mode-line-update))

;;;###autoload
(defun supertag-vault-activate (vault)
  "Activate VAULT (normalized plist) and load its DB/state.

This stops the current auto-sync worker (if any), switches persistence paths,
loads store/sync-state for the selected vault, and restarts auto-sync for the
active vault when `supertag-sync-auto-start` is non-nil."
  (interactive
   (let* ((vaults (supertag-vault--normalized-vaults))
          (choices (mapcar (lambda (v)
                             (format "%s  (%s)"
                                     (plist-get v :name)
                                     (abbreviate-file-name (plist-get v :root))))
                           vaults))
          (choice (completing-read "Supertag vault: " choices nil t)))
     (list (nth (cl-position choice choices :test #'string=) vaults))))
  (unless vault
    (user-error "No vault selected"))
  (unless (supertag-vault--vault-mode-p)
    (user-error "Vault switching requires `org-supertag-sync-directories-mode` set to 'vaults"))
  (unless (equal (plist-get vault :id) (supertag-vault--current-id))
    (supertag-vault--persist-current)
    (when (fboundp 'supertag-sync--cancel-auto-start)
      (ignore-errors (supertag-sync--cancel-auto-start)))
    (when (fboundp 'supertag-sync-stop-auto-sync)
      (ignore-errors (supertag-sync-stop-auto-sync)))
    (supertag-config-guard--with-allow
      (setq supertag-vault--current vault)
      (setq org-supertag-active-sync-directory (plist-get vault :root)))
    (supertag-vault--apply vault)
    (when (fboundp 'supertag-persistence-ensure-data-directory)
      (supertag-persistence-ensure-data-directory))
    (when (fboundp 'supertag-sync-load-state)
      (supertag-sync-load-state))
    (when (fboundp 'supertag-load-store)
      (supertag-load-store))
    (when (and (boundp 'supertag-sync-auto-start)
               supertag-sync-auto-start
               (fboundp 'supertag-sync-start-auto-sync))
      (ignore-errors (supertag-sync-start-auto-sync)))
    (when (fboundp 'supertag-view-node-refresh)
      (ignore-errors (supertag-view-node-refresh)))
    (message "Supertag: active vault => %s (%s)"
             (plist-get vault :name)
             (abbreviate-file-name (plist-get vault :root)))))

;;;###autoload
(defun supertag-vault-auto-activate ()
  "Update vault indicator and optionally auto-switch active vault for Org buffers."
  (when (and (listp org-supertag-sync-directories)
             (> (length org-supertag-sync-directories) 1))
    (when org-supertag-vault-modeline-indicator
      (supertag-vault-indicator-mode 1))
    (supertag-vault--update-buffer-indicator)
    (when org-supertag-vault-auto-switch
      (let ((file (buffer-file-name)))
        (when file
          (let ((vault (supertag-vault--find-by-file file)))
            (when vault
              (supertag-vault-activate vault))))))))

(defun supertag-vault--select-startup-default ()
  "Select and apply a default vault at startup (without loading)."
  (setq supertag--base-data-directory
        (or supertag--base-data-directory
            (file-name-as-directory (expand-file-name supertag-data-directory))))
  (when (and (supertag-vault--vault-mode-p)
             (listp org-supertag-sync-directories)
             (> (length org-supertag-sync-directories) 1))
    (let* ((vault (or (supertag-vault--find-by-root org-supertag-active-sync-directory)
                      (car (supertag-vault--normalized-vaults)))))
      (when vault
        (setq supertag-vault--current vault)
        (setq org-supertag-active-sync-directory (plist-get vault :root))
        (supertag-vault--apply vault)))))

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
(require 'supertag-view-framework)
;; (require 'supertag-view-examples-simple)
(require 'supertag-view-progress-dashboard)
(require 'supertag-view-effort-distribution)
(require 'supertag-view-priority-matrix)
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

    ;; Step 0: Select default vault (if configured) before any IO.
    (supertag-vault--select-startup-default)
    
    ;; Step 1: Ensure data directories exist
    (supertag-persistence-ensure-data-directory)
    
    ;; Step 2: Check critical configuration before loading data
    (supertag--check-critical-config)
    
    ;; Step 3: Load sync state
    (supertag-sync-load-state)
    
    ;; Step 4: Load data from persistent storage
    (supertag-load-store)
    (when (and (boundp 'supertag--store-origin)
               (eq (plist-get supertag--store-origin :status) :new)
               (file-exists-p supertag-db-file))
      (message "Supertag: DB exists (%s) but store origin is :new (candidates=%S); retrying."
               (abbreviate-file-name supertag-db-file)
               (mapcar #'abbreviate-file-name
                       (or (plist-get supertag--store-origin :load-candidates) '())))
      (supertag-load-store supertag-db-file))
    
    ;; Step 5: Validate loaded data and sync directories
    (supertag--validate-initialization)
    
    ;; Step 5.5: Apply user schema registrations (optional)
    (when (fboundp 'supertag-schema-apply-registrations)
      (supertag-schema-apply-registrations))

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
          (supertag-ui-completion-mode 1))))

    ;; Step 13: Enable runtime config guard after successful init
    (setq supertag--initialized t)
    (supertag-config-guard-enable))

  ;; Optionally auto-show Node View side window and follow context
  (when (and (boundp 'supertag-view-node-auto-show)
             supertag-view-node-auto-show
             (fboundp 'supertag-view-node-ensure-shown))
    (supertag-view-node-ensure-shown))

(defun supertag--check-critical-config ()
  "Check critical configuration before initialization.
Warn user if important settings are missing or incorrect."
  (unless org-supertag-sync-directories
    (display-warning 'org-supertag
                     "org-supertag-sync-directories is not configured!\n\
This means no files will be synchronized automatically.\n\
Please set this variable in your Emacs configuration, for example:\n\
  (setq org-supertag-sync-directories '(\"/path/to/your/notes\"))"
                     :warning))

  (when org-supertag-sync-directories
    (dolist (dir org-supertag-sync-directories)
      (unless (file-directory-p (expand-file-name dir))
        (display-warning 'org-supertag
                         (format "Configured sync directory does not exist: %s\n\
Please check your org-supertag-sync-directories configuration."
                                 (abbreviate-file-name (expand-file-name dir)))
                         :warning))))

  (when (supertag-vault--vault-mode-p)
    (let ((vaults (supertag-vault--normalized-vaults)))
      (when (and (> (length org-supertag-sync-directories) 1)
                 (null vaults))
        (display-warning 'org-supertag
                         "Vault mode is enabled but no valid vault roots were found.\n\
Check `org-supertag-sync-directories`."
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
(add-hook 'org-mode-hook #'supertag-vault-auto-activate)
(add-hook 'org-mode-hook #'supertag-sync-setup-realtime-hooks)

(provide 'org-supertag)

;;; org-supertag.el ends here

 
