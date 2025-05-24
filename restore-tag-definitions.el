;;; restore-tag-definitions.el --- Restore tag definitions from backup -*- lexical-binding: t; -*-

(require 'org-supertag-db)

(defun org-supertag-scan-backup-for-tags ()
  "Scan backup files for tag definitions."
  (interactive)
  (let ((backup-dir org-supertag-db-backup-directory)
        (found-tags '())
        (scanned-files 0))
    
    (if (not (file-exists-p backup-dir))
        (message "备份目录不存在: %s" backup-dir)
      
      (let ((backup-files (directory-files backup-dir t "db-.*\\.el$")))
        (message "开始扫描 %d 个备份文件..." (length backup-files))
        
        (dolist (backup-file (seq-take (reverse (sort backup-files #'string<)) 10))
          (message "扫描备份文件: %s" (file-name-nondirectory backup-file))
          (setq scanned-files (1+ scanned-files))
          
          (condition-case err
              (with-temp-buffer
                (insert-file-contents backup-file)
                (let ((content (buffer-string)))
                  ;; 查找标签定义模式
                  (let ((pos 0))
                    (while (string-match "ht-set! org-supertag-db--object \"\\([^\"]+\\)\" '(.*:type :tag.*:fields \\(([^)]+)\\)" content pos)
                      (let ((tag-id (match-string 1 content))
                            (tag-info (match-string 2 content)))
                        (unless (assoc tag-id found-tags)
                          (push (cons tag-id backup-file) found-tags)
                          (message "  发现标签定义: %s" tag-id)))
                      (setq pos (match-end 0))))))
            (error
             (message "  读取备份文件失败: %S" err))))
        
        (message "扫描完成！共扫描 %d 个文件，发现 %d 个标签定义" 
                scanned-files (length found-tags))
        
        (when found-tags
          (message "发现的标签定义:")
          (dolist (tag found-tags)
            (message "  - %s (来自: %s)" (car tag) (file-name-nondirectory (cdr tag)))))
        
        found-tags))))

(defun org-supertag-restore-tags-from-backup ()
  "Restore tag definitions from the most recent backup."
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
                 (restored-tags 0))
            
            (message "从最新备份恢复标签定义: %s" (file-name-nondirectory latest-backup))
            
            (condition-case err
                (with-temp-buffer
                  (insert-file-contents latest-backup)
                  (goto-point-min)
                  
                  ;; 查找并恢复标签定义
                  (while (re-search-forward 
                         "ht-set! org-supertag-db--object \"\\([^\"]+\\)\" '(\\([^)]*:type :tag[^)]*\\))" 
                         nil t)
                    (let* ((tag-id (match-string 1))
                           (tag-data-str (match-string 2))
                           (tag-data (read (concat "(" tag-data-str ")"))))
                      
                      ;; 只恢复标签，不覆盖节点
                      (when (eq (plist-get tag-data :type) :tag)
                        (let ((existing (org-supertag-db-get tag-id)))
                          (when (or (null existing) 
                                   (not (eq (plist-get existing :type) :tag)))
                            (ht-set! org-supertag-db--object tag-id tag-data)
                            (setq restored-tags (1+ restored-tags))
                            (message "恢复标签定义: %s" tag-id))))))
                  
                  (when (> restored-tags 0)
                    (org-supertag-db--mark-dirty)
                    (org-supertag-db-save)
                    (message "成功恢复 %d 个标签定义！" restored-tags)
                    
                    ;; 重新测试数据库状态
                    (org-supertag-test-db-status))
                  
                  (when (= restored-tags 0)
                    (message "备份文件中没有找到标签定义")))
              
              (error
               (message "恢复失败: %S" err)))))))))

(defun org-supertag-rebuild-tags-from-usage ()
  "Rebuild tag definitions from existing tag usage in nodes."
  (interactive)
  (when (yes-or-no-p "这将根据节点中的标签使用情况重建标签定义，是否继续？")
    (let ((tag-usage (make-hash-table :test 'equal))
          (created-tags 0))
      
      (message "分析节点中的标签使用情况...")
      
      ;; 1. 收集所有使用的标签
      (ht-map (lambda (k v)
               (when (eq (plist-get v :type) :node)
                 (let ((tags (plist-get v :tags)))
                   (when tags
                     (dolist (tag tags)
                       (let ((clean-tag (if (string-prefix-p "#" tag)
                                           (substring tag 1)
                                         tag)))
                         (let ((count (gethash clean-tag tag-usage 0)))
                           (puthash clean-tag (1+ count) tag-usage))))))))
             org-supertag-db--object)
      
      (message "发现 %d 个不同的标签" (hash-table-count tag-usage))
      
      ;; 2. 为每个标签创建基本定义
      (maphash (lambda (tag-name count)
                (let ((tag-id tag-name))
                  (unless (org-supertag-db-get tag-id)
                    ;; 创建基本的标签定义
                    (let ((tag-data (list
                                    :type :tag
                                    :id tag-id
                                    :fields '() ;; 空字段定义，可以后续添加
                                    :created-at (current-time)
                                    :description (format "Auto-generated from usage (used %d times)" count))))
                      (org-supertag-db-add tag-id tag-data)
                      (setq created-tags (1+ created-tags))
                      (message "创建标签定义: %s (使用次数: %d)" tag-name count)))))
              tag-usage)
      
      (if (> created-tags 0)
          (progn
            (message "成功创建 %d 个标签定义！" created-tags)
            (org-supertag-test-db-status))
        (message "没有创建新的标签定义")))))

;; 提供交互式选择
(defun org-supertag-recover-tag-definitions ()
  "Interactive recovery of tag definitions."
  (interactive)
  (message "标签定义恢复选项:")
  (let ((choice (read-char-choice 
                 "选择恢复方式:
1. 扫描备份文件中的标签定义
2. 从最新备份恢复标签定义  
3. 根据现有标签使用情况重建定义
4. 取消
请选择 (1-4): " 
                 '(?1 ?2 ?3 ?4))))
    (cond
     ((eq choice ?1) (org-supertag-scan-backup-for-tags))
     ((eq choice ?2) (org-supertag-restore-tags-from-backup))
     ((eq choice ?3) (org-supertag-rebuild-tags-from-usage))
     ((eq choice ?4) (message "已取消"))
     (t (message "无效选择")))))

;; 运行恢复程序
(org-supertag-recover-tag-definitions) 