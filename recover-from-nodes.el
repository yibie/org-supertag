;;; recover-from-nodes.el --- Recover tag definitions from node usage -*- lexical-binding: t; -*-

(require 'org-supertag-db)

(defun org-supertag-analyze-node-tags ()
  "Analyze tag usage in all nodes."
  (interactive)
  (let ((tag-usage (make-hash-table :test 'equal))
        (node-count 0)
        (nodes-with-tags 0))
    
    (message "=== 分析节点中的标签使用情况 ===")
    
    ;; 遍历所有节点，收集标签使用信息
    (ht-map (lambda (k v)
             (when (eq (plist-get v :type) :node)
               (setq node-count (1+ node-count))
               (let ((tags (plist-get v :tags)))
                 (when tags
                   (setq nodes-with-tags (1+ nodes-with-tags))
                   (dolist (tag tags)
                     (let ((clean-tag (if (string-prefix-p "#" tag)
                                         (substring tag 1)
                                       tag)))
                       (let ((current-info (gethash clean-tag tag-usage)))
                         (if current-info
                             (progn
                               (puthash clean-tag 
                                       (list :count (1+ (plist-get current-info :count))
                                             :nodes (cons k (plist-get current-info :nodes)))
                                       tag-usage))
                           (puthash clean-tag 
                                   (list :count 1 :nodes (list k))
                                   tag-usage)))))))))
           org-supertag-db--object)
    
    (message "总节点数: %d" node-count)
    (message "有标签的节点数: %d" nodes-with-tags)
    (message "发现的不同标签数: %d" (hash-table-count tag-usage))
    (message "")
    
    ;; 显示标签使用统计
    (message "=== 标签使用统计 ===")
    (let ((tag-list '()))
      (maphash (lambda (tag-name info)
                (push (list tag-name 
                           (plist-get info :count)
                           (length (plist-get info :nodes)))
                     tag-list))
              tag-usage)
      
      ;; 按使用次数排序
      (setq tag-list (sort tag-list (lambda (a b) (> (nth 1 a) (nth 1 b)))))
      
      (dolist (tag-info tag-list)
        (message "标签: %-20s 使用次数: %3d  节点数: %3d" 
                (nth 0 tag-info) (nth 1 tag-info) (nth 2 tag-info)))
      
      tag-usage)))

(defun org-supertag-create-tag-from-usage (tag-name usage-info)
  "Create a tag definition from usage information."
  (let ((count (plist-get usage-info :count))
        (nodes (plist-get usage-info :nodes)))
    (list
     :type :tag
     :id tag-name
     :fields '() ;; 基础空字段定义
     :created-at (current-time)
     :description (format "重建自节点使用 (使用%d次，%d个节点)" 
                         count (length nodes))
     :usage-count count
     :source-nodes (seq-take nodes 10)))) ;; 保存前10个使用节点的ID

(defun org-supertag-rebuild-all-tags-from-nodes ()
  "Rebuild all tag definitions from node usage."
  (interactive)
  (message "开始从节点数据重建标签定义...")
  
  ;; 1. 分析标签使用情况
  (let ((tag-usage (org-supertag-analyze-node-tags))
        (created-count 0)
        (updated-count 0))
    
    (when (> (hash-table-count tag-usage) 0)
      (message "")
      (message "=== 开始重建标签定义 ===")
      
      ;; 2. 为每个标签创建定义
      (maphash (lambda (tag-name usage-info)
                (let ((existing-tag (org-supertag-db-get tag-name)))
                  (if (and existing-tag (eq (plist-get existing-tag :type) :tag))
                      (progn
                        ;; 更新现有标签的使用信息
                        (plist-put existing-tag :usage-count (plist-get usage-info :count))
                        (plist-put existing-tag :source-nodes 
                                  (seq-take (plist-get usage-info :nodes) 10))
                        (org-supertag-db-add tag-name existing-tag)
                        (setq updated-count (1+ updated-count))
                        (message "更新标签: %s" tag-name))
                    ;; 创建新标签定义
                    (let ((tag-def (org-supertag-create-tag-from-usage tag-name usage-info)))
                      (org-supertag-db-add tag-name tag-def)
                      (setq created-count (1+ created-count))
                      (message "创建标签: %s (使用%d次)" 
                              tag-name (plist-get usage-info :count))))))
              tag-usage)
      
      ;; 3. 保存数据库
      (when (or (> created-count 0) (> updated-count 0))
        (org-supertag-db-save)
        (message "")
        (message "=== 重建完成 ===")
        (message "新创建标签: %d" created-count)
        (message "更新标签: %d" updated-count)
        
        ;; 4. 验证结果
        (org-supertag-test-db-status))
      
      (when (and (= created-count 0) (= updated-count 0))
        (message "没有需要创建或更新的标签定义")))))

(defun org-supertag-check-older-backups ()
  "Check older backup files for tag definitions."
  (interactive)
  (let ((backup-dir org-supertag-db-backup-directory)
        (found-tags '())
        (checked-files 0))
    
    (message "=== 检查所有备份文件中的标签定义 ===")
    
    (if (not (file-exists-p backup-dir))
        (message "备份目录不存在: %s" backup-dir)
      
      (let ((backup-files (directory-files backup-dir t "db-.*\\.el$")))
        (message "开始检查 %d 个备份文件..." (length backup-files))
        
        ;; 检查所有备份文件，不限制数量
        (dolist (backup-file (reverse (sort backup-files #'string<)))
          (setq checked-files (1+ checked-files))
          (message "检查第 %d/%d 个: %s" 
                  checked-files (length backup-files)
                  (file-name-nondirectory backup-file))
          
          (condition-case err
              (with-temp-buffer
                (insert-file-contents backup-file)
                (goto-point-min)
                (let ((file-tags 0))
                  ;; 查找标签定义
                  (while (re-search-forward 
                         "ht-set! org-supertag-db--object \"\\([^\"]+\\)\" '([^)]*:type :tag[^)]*)" 
                         nil t)
                    (let ((tag-id (match-string 1)))
                      (unless (assoc tag-id found-tags)
                        (push (cons tag-id backup-file) found-tags)
                        (setq file-tags (1+ file-tags)))))
                  (when (> file-tags 0)
                    (message "  -> 发现 %d 个标签定义" file-tags))))
            (error
             (message "  -> 读取失败: %S" err))))
        
        (message "")
        (message "=== 检查结果 ===")
        (message "检查文件数: %d" checked-files)
        (message "找到标签定义数: %d" (length found-tags))
        
        (if found-tags
            (progn
              (message "发现的标签定义:")
              (dolist (tag found-tags)
                (message "  - %s (来自: %s)" 
                        (car tag) (file-name-nondirectory (cdr tag))))
              (message "")
              (when (yes-or-no-p "是否从最旧的包含标签定义的备份恢复？")
                (org-supertag-restore-from-specific-backup (cdar (last found-tags)))))
          (message "所有备份文件中都没有找到标签定义"))
        
        found-tags))))

(defun org-supertag-restore-from-specific-backup (backup-file)
  "Restore tag definitions from a specific backup file."
  (let ((restored-tags 0))
    (message "从备份文件恢复标签定义: %s" (file-name-nondirectory backup-file))
    
    (condition-case err
        (with-temp-buffer
          (insert-file-contents backup-file)
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
                (ht-set! org-supertag-db--object tag-id tag-data)
                (setq restored-tags (1+ restored-tags))
                (message "恢复标签定义: %s" tag-id))))
          
          (when (> restored-tags 0)
            (org-supertag-db--mark-dirty)
            (org-supertag-db-save)
            (message "成功恢复 %d 个标签定义！" restored-tags)
            (org-supertag-test-db-status)))
      
      (error
       (message "恢复失败: %S" err)))))

;; 主恢复函数
(defun org-supertag-smart-tag-recovery ()
  "Smart tag recovery - tries multiple approaches."
  (interactive)
  (message "=== 智能标签恢复 ===")
  (let ((choice (read-char-choice 
                 "选择恢复策略:
1. 检查所有备份文件中的标签定义
2. 从节点使用情况分析标签
3. 直接从节点重建所有标签定义
4. 取消
请选择 (1-4): " 
                 '(?1 ?2 ?3 ?4))))
    (cond
     ((eq choice ?1) (org-supertag-check-older-backups))
     ((eq choice ?2) (org-supertag-analyze-node-tags))
     ((eq choice ?3) (org-supertag-rebuild-all-tags-from-nodes))
     ((eq choice ?4) (message "已取消"))
     (t (message "无效选择")))))

;; 执行智能恢复
(org-supertag-smart-tag-recovery) 