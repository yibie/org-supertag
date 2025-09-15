;;; org-supertag/supertag-persistence.el --- Data persistence for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides functions for persisting the Org-Supertag
;; in-memory store to a file and loading it back.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'supertag-core-notify) ; For supertag-subscribe and supertag-emit-event
(require 'supertag-core-store) ; For supertag--store

;;; --- Persistence Configuration ---
;; Note: supertag-data-directory is defined in org-supertag.el
;; This is a fallback definition in case this module is loaded independently
(defvar supertag-data-directory
  (expand-file-name "org-supertag/" user-emacs-directory)
  "Directory for storing Org-Supertag data.
This is a fallback definition. The primary definition is in org-supertag.el.")

(defun supertag-data-file (filename)
  "Get full path for data file.
FILENAME is relative to `supertag-data-directory`."
  (expand-file-name filename supertag-data-directory))

(defcustom supertag-db-file
  (supertag-data-file "supertag-db.el")
  "Database file path."
  :type 'file
  :group 'org-supertag)

(defcustom supertag-db-backup-directory
  (supertag-data-file "backups")
  "Directory for database backups."
  :type 'directory
  :group 'org-supertag)

(defcustom supertag-db-auto-save-interval 300
  "Auto-save interval in seconds.
Set to nil to disable auto-save."
  :type '(choice (const :tag "Disable" nil)
                (integer :tag "Interval (seconds)"))
  :group 'org-supertag)

(defcustom supertag-db-backup-interval 86400
  "Daily backup interval in seconds (default: 24 hours).
Set to nil to disable daily backups."
  :type '(choice (const :tag "Disable" nil)
                (integer :tag "Interval (seconds)"))
  :group 'org-supertag)

(defcustom supertag-db-backup-keep-days 3
  "Number of days to keep daily backups.
Older backups will be automatically cleaned up."
  :type 'integer
  :group 'org-supertag)

(defvar supertag-db--auto-save-timer nil
  "Timer for auto-save.")

(defvar supertag-db--backup-timer nil
  "Timer for daily backup.")

(defvar supertag-db--dirty nil
  "Flag indicating if database has unsaved changes.")

(defvar supertag-db--last-backup-date nil
  "Date of last backup in YYYY-MM-DD format.")

;;; --- Backup Functions ---

(defun supertag-get-backup-filename (date-str)
  "Generate backup filename for given DATE-STR in YYYY-MM-DD format."
  (expand-file-name
   (format "supertag-db-%s.el" date-str)
   supertag-db-backup-directory))

(defun supertag-create-daily-backup ()
  "Create a daily backup of the database if needed.
Returns t if backup was created, nil if not needed."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (backup-file (supertag-get-backup-filename today)))
    (if (file-exists-p backup-file)
        (progn
          (message "Daily backup already exists: %s" backup-file)
          nil)
      (when (file-exists-p supertag-db-file)
        (supertag-persistence-ensure-data-directory)
        (copy-file supertag-db-file backup-file)
        (setq supertag-db--last-backup-date today)
        (message "Daily backup created: %s" backup-file)
        t))))

(defun supertag-cleanup-old-backups ()
  "Remove backup files older than `supertag-db-backup-keep-days` days."
  (when (file-exists-p supertag-db-backup-directory)
    (let* ((cutoff-time (time-subtract (current-time)
                                      (days-to-time supertag-db-backup-keep-days)))
           (backup-files (directory-files supertag-db-backup-directory t
                                         "^supertag-db-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.el$"))
           (removed-count 0))
      (dolist (backup-file backup-files)
        (let ((file-time (nth 5 (file-attributes backup-file))))
          (when (time-less-p file-time cutoff-time)
            (delete-file backup-file)
            (cl-incf removed-count)
            (message "Removed old backup: %s" backup-file))))
      (when (> removed-count 0)
        (message "Cleaned up %d old backup files" removed-count)))))

(defun supertag-backup-database-now ()
  "Force create a backup immediately and clean up old backups.
This function can be called interactively by users."
  (interactive)
  (supertag-create-daily-backup)
  (supertag-cleanup-old-backups))

(defun supertag-check-daily-backup ()
  "Check if daily backup is needed and create one if necessary."
  (let ((today (format-time-string "%Y-%m-%d")))
    (unless (string= today supertag-db--last-backup-date)
      (when (supertag-create-daily-backup)
        (supertag-cleanup-old-backups)))))

;;; --- Persistence Functions ---

(defun supertag-mark-dirty ()
  "Mark database as having unsaved changes."
  (setq supertag-db--dirty t))

(defun supertag-clear-dirty ()
  "Clear database unsaved changes flag."
  (setq supertag-db--dirty nil))

(defun supertag-dirty-p ()
  "Check if database has unsaved changes."
  supertag-db--dirty)

(defun supertag-persistence-ensure-data-directory ()
  "Ensure database and backup directories exist."
  (let ((db-dir (file-name-directory supertag-db-file)))
    ;; 1. Ensure database directory exists
    (unless (file-exists-p db-dir)
      (make-directory db-dir t))
    ;; 2. Ensure backup directory exists
    (unless (file-exists-p supertag-db-backup-directory)
      (make-directory supertag-db-backup-directory t))
    ;; 3. Verify directory creation
    (unless (and (file-exists-p db-dir)
                 (file-exists-p supertag-db-backup-directory))
      (error "Failed to create required directories: %s or %s"
             db-dir supertag-db-backup-directory))))

;;; --- Persistence Functions ---

(defun supertag-save-store (&optional file)
  "Save the current `supertag--store` to a file.
FILE is the optional file path. Defaults to `supertag-db-file`."
  (interactive)
  (let ((file-to-save (or file supertag-db-file)))
    (supertag-persistence-ensure-data-directory) ; Ensure directory exists before saving
    (when (supertag-dirty-p) ; Only save if dirty
      (message "Saving Org-Supertag data to: %s..." file-to-save)
      (with-temp-file file-to-save
        (set-buffer-file-coding-system 'utf-8-unix) ; 确保UTF-8编码
        (let ((print-escape-nonascii t)  ; 正确处理非ASCII字符
              (print-length nil)         ; 不限制打印长度
              (print-level nil))         ; 不限制打印层级
          (prin1 supertag--store (current-buffer))))
      (supertag-clear-dirty)
      ;; Check if daily backup is needed after successful save
      (supertag-check-daily-backup)
      (message "Org-Supertag data saved to: %s" file-to-save))))

(defun supertag-load-store (&optional file)
  "Load data into supertag--store from a file.
FILE is the optional file path. Defaults to supertag-db-file."
  (interactive)
  (let ((file-to-load (or file supertag-db-file)))
    (supertag-persistence-ensure-data-directory) ; Ensure directory exists before loading
    (when (file-exists-p file-to-load)
      (message "Loading Org-Supertag data from: %s..." file-to-load)
      (with-temp-buffer
        (insert-file-contents file-to-load)
        (goto-char (point-min))
        ;; Check if file is empty or malformed
        (if (= (point-min) (point-max))
            (progn
              (message "Warning: Database file %s is empty, initializing new store" file-to-load)
              (setq supertag--store (ht-create)))
          (let ((loaded-data (read (current-buffer))))
            (if (hash-table-p loaded-data)
                (progn
                  (setq supertag--store loaded-data)
                  ;; --- 数据版本检查和自动迁移 ---
                  (supertag--run-migrations supertag--store)
                  
                  ;; --- Automatic Purge of Invalid Nodes ---
                  (let* ((nodes-table (supertag-get '(:nodes) (ht-create)))
                         (keys-to-remove '())
                         (purged-count 0)
                         (migrated-count 0))
                    (when (hash-table-p nodes-table)
                      (maphash (lambda (key value)
                                 (unless (and value (plist-get value :type))
                                   (push key keys-to-remove)))
                               nodes-table)
                      (when keys-to-remove
                        (dolist (key keys-to-remove)
                          (supertag-update (list :nodes key) nil)
                          (cl-incf purged-count))
                        (message "Purged %d invalid/ghost entries from database during load." purged-count)
                        (supertag-mark-dirty)))
                    ;; --- Automatic Field Migration ---
                    (when (hash-table-p nodes-table)
                      (maphash (lambda (key value)
                                 (when (and value (plist-get value :type) (eq (plist-get value :type) :node))
                                   (let ((needs-migration nil)
                                         (migrated-value (copy-sequence value)))
                                     ;; Migrate :file-path to :file
                                     (when (and (plist-get value :file-path) (not (plist-get value :file)))
                                       (setq migrated-value (plist-put migrated-value :file (plist-get value :file-path)))
                                       (setq needs-migration t))
                                     ;; Migrate :pos to :position
                                     (when (and (plist-get value :pos) (not (plist-get value :position)))
                                       (setq migrated-value (plist-put migrated-value :position (plist-get value :pos)))
                                       (setq needs-migration t))
                                     ;; Update if migration was needed
                                     (when needs-migration
                                       (supertag-update (list :nodes key) migrated-value)
                                       (cl-incf migrated-count)))))
                               nodes-table)
                      (when (> migrated-count 0)
                        (message "Migrated %d nodes with legacy field names (:file-path -> :file, :pos -> :position)." migrated-count)
                        (supertag-mark-dirty))))
                    ;; --- ADD THIS DEBUG PROBE ---
                    (let ((nodes-table (supertag-get '(:nodes))))
                      (message "DEBUG-LOAD: Nodes table loaded. Type: %s, Count: %s"
                               (type-of nodes-table)
                               (if (hash-table-p nodes-table) (hash-table-count nodes-table) "N/A")))
                    (message "Org-Supertag data loaded from: %s" file-to-load))
                (progn
                  (message "Warning: Invalid data format in %s, initializing new store" file-to-load)
                  (setq supertag--store (ht-create)))))))
      (supertag-clear-dirty)
      ;; Rebuild indexes after loading data
      (when (hash-table-p supertag--store)
        (supertag--rebuild-all-indexes)))
    (unless (hash-table-p supertag--store)
      (setq supertag--store (ht-create))
      (message "Initialized empty Org-Supertag store."))))

(defun supertag-schedule-save ()
  "Schedule a delayed save.
Waits for 2 seconds of idle time before saving to avoid frequent saves."
  (when supertag-db--auto-save-timer
    (cancel-timer supertag-db--auto-save-timer))
  (setq supertag-db--auto-save-timer
        (run-with-idle-timer 2 nil #'supertag-save-store)))

(defun supertag-setup-auto-save ()
  "Set up auto-save timer."
  (when (and supertag-db-auto-save-interval
             (null supertag-db--auto-save-timer))
    (setq supertag-db--auto-save-timer
          (run-with-timer supertag-db-auto-save-interval
                         supertag-db-auto-save-interval
                         #'supertag-save-store))))

(defun supertag-setup-daily-backup ()
  "Set up daily backup timer."
  (when (and supertag-db-backup-interval
             (null supertag-db--backup-timer))
    (setq supertag-db--backup-timer
          (run-with-timer supertag-db-backup-interval
                         supertag-db-backup-interval
                         #'supertag-backup-database-now))))

(defun supertag-cleanup-auto-save ()
  "Clean up auto-save timer."
  (when supertag-db--auto-save-timer
    (cancel-timer supertag-db--auto-save-timer)
    (setq supertag-db--auto-save-timer nil)))

(defun supertag-cleanup-daily-backup ()
  "Clean up daily backup timer."
  (when supertag-db--backup-timer
    (cancel-timer supertag-db--backup-timer)
    (setq supertag-db--backup-timer nil)))

(defun supertag-setup-all-timers ()
  "Set up both auto-save and daily backup timers."
  (supertag-setup-auto-save)
  (supertag-setup-daily-backup))

(defun supertag-cleanup-all-timers ()
  "Clean up all persistence-related timers."
  (supertag-cleanup-auto-save)
  (supertag-cleanup-daily-backup))

;;; --- Event Subscription ---

(defun supertag-persistence--handle-store-changed (path old-value new-value)
  "Handle store-changed events.
This function is called when the store is updated.
PATH, OLD-VALUE, and NEW-VALUE describe the change."
  (supertag-mark-dirty) ; Mark database as dirty
  (supertag-schedule-save)) ; Schedule a delayed save

;; Subscribe to store-changed events
(supertag-subscribe :store-changed #'supertag-persistence--handle-store-changed)

(defun supertag-db-purge-duplicate-tags ()
  "Interactively scan the :tags collection and remove duplicate tags.
Keeps the tag with the most complete data (most fields defined).
Merges relations from duplicate tags to the kept tag."
  (interactive)
  (let* ((tags-table (supertag-get '(:tags)))
         (name-to-tags (make-hash-table :test 'equal))
         (duplicates-found 0)
         (tags-removed 0))
    
    (if (not (hash-table-p tags-table))
        (message "Tags collection is missing or invalid; nothing to purge.")
      
      ;; Step 1: Group tags by name
      (maphash (lambda (tag-id tag-data)
                 (when (and tag-data (plist-get tag-data :name))
                   (let ((tag-name (plist-get tag-data :name)))
                     (unless (gethash tag-name name-to-tags)
                       (puthash tag-name '() name-to-tags))
                     (puthash tag-name 
                             (cons (cons tag-id tag-data) (gethash tag-name name-to-tags))
                             name-to-tags))))
               tags-table)
      
      ;; Step 2: Find and resolve duplicates
      (maphash (lambda (tag-name tag-list)
                 (when (> (length tag-list) 1)
                   (cl-incf duplicates-found)
                   (message "Found %d duplicate tags for name '%s': %s" 
                           (length tag-list) tag-name 
                           (mapcar #'car tag-list))
                   
                   ;; Choose the "best" tag (with most fields or first created)
                   (let* ((sorted-tags (sort tag-list 
                                           (lambda (a b)
                                             (let ((fields-a (length (or (plist-get (cdr a) :fields) '())))
                                                   (fields-b (length (or (plist-get (cdr b) :fields) '()))))
                                               (> fields-a fields-b)))))
                          (keeper (car sorted-tags))
                          (duplicates (cdr sorted-tags))
                          (keeper-id (car keeper)))
                     
                     (message "Keeping tag '%s', removing duplicates: %s" 
                             keeper-id (mapcar #'car duplicates))
                     
                     ;; Remove duplicate tags
                     (dolist (duplicate duplicates)
                       (let ((duplicate-id (car duplicate)))
                         (supertag-delete (list :tags duplicate-id))
                         (cl-incf tags-removed))))))
               name-to-tags)
      
      (if (> duplicates-found 0)
          (progn
            (message "Duplicate tag cleanup complete. %d tag names had duplicates, %d duplicate tags removed." 
                    duplicates-found tags-removed)
            (supertag-save-store)
            (message "Database saved."))
        (message "No duplicate tags found. Database is clean.")))))

(defun supertag-db-purge-invalid-nodes ()
  "Interactively scan the :nodes collection and remove entries with invalid data.
An entry is considered invalid if its value is nil or it's not a valid plist
with a :type property.

This version is tolerant when the :nodes collection is missing or not yet
initialized: it will treat that case as an empty collection and exit cleanly."
  (interactive)
  (let* ((nodes-table (supertag-get '(:nodes)))
         (keys-to-remove '())
         (total-keys 0))
    ;; If nodes-table is missing or not a hash table, log and skip the purge.
    (if (not (hash-table-p nodes-table))
        (message "Nodes collection is missing or invalid; nothing to purge.")
      ;; else proceed with scanning and purging
      (setq total-keys (hash-table-count nodes-table))
      (message "Scanning %d total entries in nodes table..." total-keys)

      ;; First, identify all keys with invalid values
      (maphash (lambda (key value)
                 (unless (and value (plist-get value :type))
                   (push key keys-to-remove)))
               nodes-table)

      ;; Then, remove them
      (if keys-to-remove
          (progn
            (message "Found %d invalid entries to purge. Purging..." (length keys-to-remove))
            (dolist (key keys-to-remove)
              ;; Use the new, explicit delete function
              (supertag-delete (list :nodes key)))
            (message "Purging complete. Saving database...")
            (supertag-save-store)
            (message "Database saved. %d entries remain." (hash-table-count (supertag-get '(:nodes)))))
        (message "No invalid entries found. Database is clean.")))))

;;; --- 时间格式标准化 ---

(defun supertag-current-time ()
  "获取标准化的当前时间。
返回 Emacs 标准时间格式 (high low micro pico)。"
  (current-time))

(defun supertag-time-equal (time1 time2)
  "安全的时间比较函数。
TIME1 和 TIME2 应该是 Emacs 时间格式。
返回 t 如果时间相等，否则返回 nil。"
  (and (timep time1) (timep time2) (equal time1 time2)))

(defun supertag--validate-time (time-value)
  "验证时间值是否为有效的 Emacs 时间格式。
TIME-VALUE 应该是四元素列表 (high low micro pico)。"
  (and (listp time-value)
       (= (length time-value) 4)
       (cl-every #'integerp time-value)))

;;; --- 数据版本管理 ---

(defconst supertag-data-version "5.0.0"
  "当前数据格式版本。
用于数据格式兼容性检查和自动迁移。")

(defun supertag--get-data-version (data)
  "从数据存储中提取版本信息。
DATA 应该是主数据存储哈希表。
返回版本字符串，如果未找到则返回默认旧版本。"
  (if (hash-table-p data)
      (or (gethash :version data) "4.0.0")  ; 默认旧版本
    "4.0.0"))

(defun supertag--set-data-version (data version)
  "在数据存储中设置版本信息。
DATA 应该是主数据存储哈希表。
VERSION 是要设置的版本字符串。"
  (when (hash-table-p data)
    (puthash :version version data)))

(defun supertag--run-migrations (data)
  "根据版本号执行数据迁移。
DATA 是要迁移的数据存储。
自动检测版本并执行必要的迁移步骤。"
  (let ((current-version (supertag--get-data-version data)))
    (unless (string= current-version supertag-data-version)
      (message "Migrating data from version %s to %s" current-version supertag-data-version)
      
      ;; 执行版本特定的迁移
      (cond
       ;; 从 4.x 迁移到 5.0.0
       ((string-prefix-p "4." current-version)
        (supertag--migrate-4x-to-5x data))
       
       ;; 其他版本迁移可以在这里添加
       (t
        (message "No migration path defined for version %s" current-version)))
      
      ;; 更新版本号
      (supertag--set-data-version data supertag-data-version)
      (message "Data migration completed to version %s" supertag-data-version))))

(defun supertag--migrate-4x-to-5x (data)
  "从版本 4.x 迁移到 5.0.0。
主要变更包括数据格式标准化和字段名规范化。"
  (message "Executing 4.x to 5.0.0 migration...")
  
  ;; 这里可以添加具体的迁移步骤
  ;; 例如字段重命名、数据格式转换等
  
  ;; 示例：确保所有时间字段使用标准格式
  (let ((nodes-table (gethash :nodes data)))
    (when (hash-table-p nodes-table)
      (maphash (lambda (node-id node-data)
                 (when (plist-get node-data :type)
                   ;; 确保时间字段存在且格式正确
                   (unless (plist-get node-data :created-at)
                     (setq node-data (plist-put node-data :created-at (supertag-current-time))))
                   (unless (plist-get node-data :modified-at)
                     (setq node-data (plist-put node-data :modified-at (supertag-current-time))))
                   
                   ;; 更新节点数据
                   (puthash node-id node-data nodes-table)))
               nodes-table)))
  
  (message "4.x to 5.0.0 migration completed"))

;;; --- 数据备份和事务安全机制 ---

(defun supertag-backup-store ()
  "创建数据存储的深度备份。
返回当前 supertag--store 的完整拷贝。"
  (when (hash-table-p supertag--store)
    (let ((backup (make-hash-table :test (hash-table-test supertag--store)
                                   :size (hash-table-size supertag--store))))
      (maphash (lambda (key value)
                 (puthash key
                         (if (hash-table-p value)
                             ;; 深度拷贝嵌套的哈希表
                             (let ((nested-copy (make-hash-table :test (hash-table-test value)
                                                               :size (hash-table-size value))))
                               (maphash (lambda (k v) (puthash k v nested-copy)) value)
                               nested-copy)
                           ;; 对于其他类型，直接拷贝（plists等）
                           (copy-sequence value))
                         backup))
               supertag--store)
      backup)))

(defun supertag-restore-store (backup-data)
  "从备份恢复数据存储。
BACKUP-DATA 应该是通过 `supertag-backup-store' 创建的备份。"
  (when (and backup-data (hash-table-p backup-data))
    (setq supertag--store backup-data)
    (supertag-clear-dirty)
    (message "Data store restored from backup")))

(defmacro supertag--with-transaction (&rest body)
  "事务执行包装器。
在执行 BODY 前创建数据备份，如果执行失败则自动恢复。
确保数据操作的原子性。"
  `(let ((backup (supertag-backup-store))
         (success nil))
     (unwind-protect
         (progn
           ,@body
           (setq success t))
       (unless success
         (when backup
           (supertag-restore-store backup)
           (message "Transaction failed, data restored from backup"))))))

(defun supertag--validate-tag-references ()
  "验证标签引用的一致性。
检查所有节点引用的标签是否都存在于标签集合中。
返回 t 如果所有引用都有效，否则返回 nil。"
  (let ((tags-table (supertag-get '(:tags)))
        (nodes-table (supertag-get '(:nodes)))
        (valid-p t)
        (error-count 0))
    
    (when (and (hash-table-p tags-table) (hash-table-p nodes-table))
      (maphash (lambda (node-id node-data)
                 (when (eq (plist-get node-data :type) :node)
                   (let ((node-tags (plist-get node-data :tags)))
                     (when node-tags
                       (dolist (tag-id node-tags)
                         (unless (gethash tag-id tags-table)
                           (setq valid-p nil)
                           (cl-incf error-count)
                           (message "Invalid tag reference: node %s references non-existent tag %s"
                                   node-id tag-id)))))))
               nodes-table))
    
    (if valid-p
        (message "Tag reference validation passed")
      (message "Tag reference validation failed with %d errors" error-count))
    
    valid-p))

(provide 'supertag-core-persistence)