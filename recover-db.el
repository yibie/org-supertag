;;; recover-db.el --- Recovery script for org-supertag database -*- lexical-binding: t; -*-

(require 'org-supertag-db)
(require 'org-supertag-sync)

(defun org-supertag-recover-from-backup ()
  "Attempt to recover database from the most recent backup."
  (interactive)
  (let ((backup-dir org-supertag-db-backup-directory))
    (if (not (file-exists-p backup-dir))
        (message "备份目录不存在: %s" backup-dir)
      (let ((backup-files (directory-files backup-dir t "db-.*\\.el$")))
        (if (not backup-files)
            (message "没有找到备份文件")
          (let* ((latest-backup (car (sort backup-files 
                                         (lambda (a b)
                                           (file-newer-than-file-p a b)))))
                 (backup-size (nth 7 (file-attributes latest-backup))))
            (when (> backup-size 0)
              (when (yes-or-no-p 
                     (format "发现备份文件 %s (%d bytes)，是否恢复？" 
                            (file-name-nondirectory latest-backup)
                            backup-size))
                (condition-case err
                    (progn
                      ;; 备份当前文件（如果存在）
                      (when (file-exists-p org-supertag-db-file)
                        (let ((backup-current (format "%s.before-recovery" 
                                                     org-supertag-db-file)))
                          (copy-file org-supertag-db-file backup-current t)
                          (message "当前数据库文件已备份到: %s" backup-current)))
                      
                      ;; 恢复备份
                      (copy-file latest-backup org-supertag-db-file t)
                      
                      ;; 重新加载数据库
                      (org-supertag-db-load)
                      
                      (message "数据库恢复成功！实体数量: %d, 链接数量: %d"
                              (ht-size org-supertag-db--object)
                              (ht-size org-supertag-db--link)))
                  (error
                   (message "恢复失败: %S" err))))))))))

(defun org-supertag-rebuild-database ()
  "Rebuild database by re-scanning all org files."
  (interactive)
  (when (yes-or-no-p "这将重新构建整个数据库，是否继续？")
    (condition-case err
        (progn
          ;; 清空当前数据库
          (setq org-supertag-db--object (ht-create)
                org-supertag-db--link (ht-create))
          
          ;; 重新同步所有文件
          (message "开始重新构建数据库...")
          (org-supertag-sync-all)
          
          ;; 保存数据库
          (org-supertag-db-save)
          
          (message "数据库重建完成！实体数量: %d, 链接数量: %d"
                  (ht-size org-supertag-db--object)
                  (ht-size org-supertag-db--link)))
      (error
       (message "重建失败: %S" err)))))

(defun org-supertag-force-resync ()
  "Force resync all org files, clearing sync state."
  (interactive)
  (when (yes-or-no-p "这将强制重新同步所有文件，是否继续？")
    (condition-case err
        (progn
          ;; 清空同步状态
          (when (boundp 'org-supertag-sync--file-states)
            (setq org-supertag-sync--file-states (ht-create)))
          
          ;; 重新同步
          (message "开始强制重新同步...")
          (org-supertag-sync-all)
          
          (message "强制同步完成！"))
      (error
       (message "强制同步失败: %S" err)))))

(defun org-supertag-recover-interactive ()
  "Interactive recovery menu."
  (interactive)
  (let ((choice (read-char-choice 
                 "选择恢复方式:
1. 从备份恢复
2. 重建数据库  
3. 强制重新同步
4. 取消
请选择 (1-4): " 
                 '(?1 ?2 ?3 ?4))))
    (cond
     ((eq choice ?1) (org-supertag-recover-from-backup))
     ((eq choice ?2) (org-supertag-rebuild-database))
     ((eq choice ?3) (org-supertag-force-resync))
     ((eq choice ?4) (message "已取消"))
     (t (message "无效选择")))))

;; 提供便捷的恢复入口
(defun org-supertag-emergency-recovery ()
  "Emergency recovery for lost org-supertag data."
  (interactive)
  (message "开始紧急数据恢复...")
  
  ;; 首先运行诊断
  (load-file "diagnose-db.el")
  
  ;; 然后提供恢复选项
  (org-supertag-recover-interactive)) 