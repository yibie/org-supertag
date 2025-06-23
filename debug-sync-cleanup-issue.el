;;; debug-sync-cleanup-issue.el --- 诊断同步清理问题

(require 'org-supertag-sync)
(require 'org-supertag-db)

(defun debug-sync-cleanup-issue ()
  "诊断同步清理问题，检查为什么所有节点都被删除了。"
  (interactive)
  (message "\n=== 同步清理问题诊断 ===")
  
  ;; 1. 检查同步目录配置
  (message "\n1. 同步目录配置检查：")
  (message "   同步目录: %S" org-supertag-sync-directories)
  (message "   排除目录: %S" org-supertag-sync-exclude-directories)
  (message "   数据目录: %s" org-supertag-data-directory)
  
  ;; 2. 检查同步状态
  (message "\n2. 同步状态检查：")
  (message "   同步状态文件: %s" org-supertag-sync-state-file)
  (message "   状态文件存在: %s" (file-exists-p org-supertag-sync-state-file))
  (message "   同步状态中的文件数: %d" (hash-table-count org-supertag-sync--state))
  
  ;; 显示同步状态中的文件（前10个）
  (let ((count 0))
    (message "   同步状态中的文件（前10个）：")
    (maphash (lambda (file time)
               (when (< count 10)
                 (message "     %s -> %s" file time)
                 (cl-incf count)))
             org-supertag-sync--state))
  
  ;; 3. 检查数据库状态
  (message "\n3. 数据库状态检查：")
  (let ((node-count 0)
        (files-in-db (make-hash-table :test 'equal)))
    (maphash (lambda (id node)
               (when (eq (plist-get node :type) :node)
                 (cl-incf node-count)
                 (let ((file (plist-get node :file-path)))
                   (when file
                     (puthash file t files-in-db)))))
             org-supertag-db--object)
    (message "   数据库中的节点数: %d" node-count)
    (message "   数据库中涉及的文件数: %d" (hash-table-count files-in-db))
    
    ;; 显示数据库中的文件（前10个）
    (let ((count 0))
      (message "   数据库中的文件（前10个）：")
      (maphash (lambda (file _)
                 (when (< count 10)
                   (message "     %s (存在: %s)" file (file-exists-p file))
                   (cl-incf count)))
               files-in-db)))
  
  ;; 4. 检查文件范围匹配
  (message "\n4. 文件范围匹配检查：")
  (let ((in-scope-count 0)
        (out-scope-count 0))
    (maphash (lambda (id node)
               (when (eq (plist-get node :type) :node)
                 (let ((file (plist-get node :file-path)))
                   (if (org-supertag-sync--in-sync-scope-p file)
                       (cl-incf in-scope-count)
                     (cl-incf out-scope-count)))))
             org-supertag-db--object)
    (message "   在同步范围内的节点: %d" in-scope-count)
    (message "   超出同步范围的节点: %d" out-scope-count))
  
  ;; 5. 检查节点检测逻辑
  (message "\n5. 节点检测逻辑测试：")
  (let ((test-count 0)
        (found-count 0)
        (not-found-count 0))
    (maphash (lambda (id node)
               (when (and (eq (plist-get node :type) :node)
                         (< test-count 5)) ; 只测试前5个节点
                 (cl-incf test-count)
                 (let ((file (plist-get node :file-path)))
                   (if (org-supertag-sync--check-node-exists-in-file id file)
                       (progn
                         (cl-incf found-count)
                         (message "   ✓ 节点 %s 在文件 %s 中找到" id file))
                     (progn
                       (cl-incf not-found-count)
                       (message "   ✗ 节点 %s 在文件 %s 中未找到" id file))))))
             org-supertag-db--object)
    (message "   测试节点: %d, 找到: %d, 未找到: %d" test-count found-count not-found-count))
  
  ;; 6. 检查最近的删除操作
  (message "\n6. 可能的问题原因：")
  (cond
   ((= (hash-table-count org-supertag-sync--state) 0)
    (message "   ❌ 同步状态为空！这会导致所有节点被认为'不在同步范围'"))
   ((not org-supertag-sync-directories)
    (message "   ❌ 同步目录未配置！"))
   (t
    (message "   ✓ 同步配置看起来正常")))
  
  ;; 7. 提供修复建议
  (message "\n7. 修复建议：")
  (message "   - 如果同步状态为空，运行: (org-supertag-sync-recover)")
  (message "   - 如果同步目录未配置，运行: (org-supertag-sync-add-directory \"~/org\")")
  (message "   - 如果节点检测有问题，检查org文件中的ID格式")
  (message "   - 如果需要停止自动清理，运行: (org-supertag-sync-stop-auto-sync)"))

(defun debug-check-node-detection ()
  "详细检查节点检测逻辑是否正确。"
  (interactive)
  (message "\n=== 节点检测逻辑详细检查 ===")
  
  ;; 检查当前buffer中的节点
  (when (and (derived-mode-p 'org-mode) (buffer-file-name))
    (let ((current-file (buffer-file-name))
          (found-ids '())
          (db-nodes '()))
      
      ;; 1. 扫描当前文件中实际存在的ID
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;; 搜索所有ID
          (while (re-search-forward "^[ \t]*:ID:[ \t]+\\([^[:space:]]+\\)" nil t)
            (push (match-string 1) found-ids))))
      
      ;; 2. 检查数据库中属于这个文件的节点
      (maphash (lambda (id node)
                 (when (and (eq (plist-get node :type) :node)
                           (string= (plist-get node :file-path) current-file))
                   (push id db-nodes)))
               org-supertag-db--object)
      
      (message "文件: %s" current-file)
      (message "文件中实际的ID数量: %d" (length found-ids))
      (message "数据库中的节点数量: %d" (length db-nodes))
      
      ;; 3. 比较差异
      (let ((missing-in-file '())
            (missing-in-db '()))
        (dolist (id db-nodes)
          (unless (member id found-ids)
            (push id missing-in-file)))
        (dolist (id found-ids)
          (unless (member id db-nodes)
            (push id missing-in-db)))
        
        (when missing-in-file
          (message "❌ 数据库中有但文件中找不到的节点: %S" missing-in-file))
        (when missing-in-db
          (message "⚠️  文件中有但数据库中没有的节点: %S" missing-in-db))
        
        (when (and (null missing-in-file) (null missing-in-db))
          (message "✓ 数据库与文件状态一致"))))))

(defun debug-test-cleanup-logic ()
  "测试清理逻辑而不实际删除节点。"
  (interactive)
  (message "\n=== 清理逻辑测试（干运行）===")
  
  ;; 模拟 org-supertag-sync--cleanup-database 的逻辑但不删除
  (let ((nodes-to-remove nil)
        (files-in-db (make-hash-table :test 'equal))
        (sync-files (make-hash-table :test 'equal)))
    
    ;; 收集同步状态中的文件
    (maphash (lambda (file _)
               (let ((normalized (expand-file-name file)))
                 (puthash normalized t sync-files)))
             org-supertag-sync--state)
    
    ;; 收集数据库中的文件
    (maphash (lambda (id node)
               (when (eq (plist-get node :type) :node)
                 (let ((file (plist-get node :file-path)))
                   (when file
                     (let ((normalized (expand-file-name file)))
                       (puthash normalized t files-in-db))))))
             org-supertag-db--object)
    
    ;; 检查哪些文件的节点会被删除
    (maphash (lambda (file _)
               (unless (gethash file sync-files)
                 (message "❌ 文件 %s 不在同步状态，其节点会被删除" file)
                 (maphash (lambda (id node)
                           (when (and (eq (plist-get node :type) :node)
                                    (string= (expand-file-name (plist-get node :file-path)) file))
                             (push id nodes-to-remove)))
                         org-supertag-db--object)))
             files-in-db)
    
    (message "总结:")
    (message "  同步状态中的文件: %d" (hash-table-count sync-files))
    (message "  数据库中的文件: %d" (hash-table-count files-in-db))
    (message "  将被删除的节点: %d" (length nodes-to-remove))
    
    (when nodes-to-remove
      (message "前10个将被删除的节点:")
      (let ((count 0))
        (dolist (id nodes-to-remove)
          (when (< count 10)
            (let ((node (org-supertag-db-get id)))
              (message "    %s (文件: %s)" id (plist-get node :file-path)))
            (cl-incf count)))))))

(provide 'debug-sync-cleanup-issue) 