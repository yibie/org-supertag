;;; recover-fields-from-links.el --- Recover field definitions from link relationships -*- lexical-binding: t; -*-

(require 'org-supertag-db)

(defun org-supertag-analyze-field-links ()
  "Analyze node-field links to understand field usage."
  (interactive)
  (message "=== 分析字段链接关系 ===")
  
  (let ((field-usage (make-hash-table :test 'equal)) ; tag-id -> field-info
        (total-links 0)
        (field-links 0))
    
    ;; 遍历所有链接
    (ht-map (lambda (link-id link-data)
             (setq total-links (1+ total-links))
             ;; 改为通过链接ID模式识别字段链接，而不是依赖:type属性
             (when (and (stringp link-id) 
                       (string-prefix-p ":node-field:" link-id))
               (setq field-links (1+ field-links))
               (let* ((field-name (plist-get link-data :to))
                      (field-value (plist-get link-data :value))
                      (tag-id (plist-get link-data :tag-id))
                      (node-id (plist-get link-data :from)))
                 
                 (when (and field-name tag-id)
                   ;; 获取或创建该标签的字段信息
                   (let ((tag-fields (gethash tag-id field-usage (make-hash-table :test 'equal))))
                     (let ((field-info (gethash field-name tag-fields)))
                       (if field-info
                           ;; 更新现有字段信息
                           (progn
                             (puthash :count (1+ (gethash :count field-info)) field-info)
                             (let ((values (gethash :values field-info)))
                               (unless (member field-value values)
                                 (puthash :values (cons field-value values) field-info))))
                         ;; 创建新字段信息
                         (let ((new-field-info (make-hash-table :test 'equal)))
                           (puthash :name field-name new-field-info)
                           (puthash :count 1 new-field-info)
                           (puthash :values (list field-value) new-field-info)
                           (puthash field-name new-field-info tag-fields))))
                     (puthash tag-id tag-fields field-usage))))))
           org-supertag-db--link)
    
    (message "总链接数: %d" total-links)
    (message "字段链接数: %d" field-links)
    (message "发现字段使用的标签数: %d" (hash-table-count field-usage))
    (message "")
    
    ;; 显示分析结果
    (message "=== 字段使用分析 ===")
    (maphash (lambda (tag-id tag-fields)
              (message "标签: %s" tag-id)
              (maphash (lambda (field-name field-info)
                        (let ((count (gethash :count field-info))
                              (values (gethash :values field-info)))
                          (message "  字段: %-15s 使用次数: %3d 示例值: %S" 
                                  field-name count (seq-take values 3))))
                      tag-fields)
              (message ""))
            field-usage)
    
    field-usage))

(defun org-supertag-infer-field-type (values)
  "Infer field type from a list of values."
  (if (null values)
      'string
    (let ((sample-values (seq-take values 10))) ; 检查前10个值
      (cond
       ;; 检查是否都是数字
       ((cl-every (lambda (v) 
                   (and (stringp v) 
                        (string-match-p "^[0-9]+\\.?[0-9]*$" v))) 
                 sample-values)
        'number)
       
       ;; 检查是否是日期格式
       ((cl-every (lambda (v)
                   (and (stringp v)
                        (or (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" v)
                            (string-match-p "^<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" v))))
                 sample-values)
        'date)
       
       ;; 检查是否是时间格式
       ((cl-every (lambda (v)
                   (and (stringp v)
                        (string-match-p "[0-9]\\{2\\}:[0-9]\\{2\\}" v)))
                 sample-values)
        'timestamp)
       
       ;; 检查是否像是选项列表（重复值多）
       ((let ((unique-count (length (delete-dups (copy-sequence sample-values))))
              (total-count (length sample-values)))
          (and (> total-count 3)
               (< unique-count (/ total-count 2))))
        'options)
       
       ;; 默认为字符串
       (t 'string)))))

(defun org-supertag-generate-field-definitions (field-usage)
  "Generate field definitions from usage analysis."
  (let ((generated-definitions (make-hash-table :test 'equal)))
    
    (maphash (lambda (tag-id tag-fields)
              (let ((field-definitions '()))
                (maphash (lambda (field-name field-info)
                          (let* ((values (gethash :values field-info))
                                 (count (gethash :count field-info))
                                 (inferred-type (org-supertag-infer-field-type values))
                                 (unique-values (delete-dups (copy-sequence values))))
                            
                            (push (list :name field-name
                                       :type inferred-type
                                       :usage-count count
                                       :unique-values (length unique-values)
                                       :sample-values (seq-take unique-values 5)
                                       :description (format "从使用情况推断 (使用%d次)" count))
                                 field-definitions)))
                        tag-fields)
                (puthash tag-id field-definitions generated-definitions)))
            field-usage)
    
    generated-definitions))

(defun org-supertag-apply-recovered-field-definitions (field-definitions)
  "Apply recovered field definitions to tags."
  (interactive)
  (let ((updated-tags 0)
        (total-fields 0))
    
    (maphash (lambda (tag-id field-defs)
              (let ((tag-data (org-supertag-db-get tag-id)))
                (when tag-data
                  ;; 更新标签的字段定义
                  (plist-put tag-data :fields field-defs)
                  (plist-put tag-data :fields-recovered-at (current-time))
                  (org-supertag-db-add tag-id tag-data)
                  (setq updated-tags (1+ updated-tags))
                  (setq total-fields (+ total-fields (length field-defs)))
                  (message "更新标签 %s: 添加了 %d 个字段定义" tag-id (length field-defs)))))
            field-definitions)
    
    (when (> updated-tags 0)
      (org-supertag-db-save)
      (message "")
      (message "=== 字段定义恢复完成 ===")
      (message "更新了 %d 个标签" updated-tags)
      (message "总共恢复了 %d 个字段定义" total-fields))
    
    (list :updated-tags updated-tags :total-fields total-fields)))

(defun org-supertag-recover-fields-interactive ()
  "Interactive field recovery from link relationships."
  (interactive)
  (message "=== 从链接关系恢复字段定义 ===")
  
  ;; 1. 分析字段使用情况
  (let ((field-usage (org-supertag-analyze-field-links)))
    
    (if (= (hash-table-count field-usage) 0)
        (message "没有发现字段使用记录，无法恢复字段定义")
      
      ;; 2. 生成字段定义
      (message "=== 生成字段定义 ===")
      (let ((field-definitions (org-supertag-generate-field-definitions field-usage)))
        
        ;; 3. 显示预览
        (message "=== 字段定义预览 ===")
        (maphash (lambda (tag-id field-defs)
                  (message "标签: %s" tag-id)
                  (dolist (field-def field-defs)
                    (message "  - %s (%s): %s" 
                            (plist-get field-def :name)
                            (plist-get field-def :type)
                            (plist-get field-def :description))))
                field-definitions)
        
        ;; 4. 询问是否应用
        (message "")
        (when (yes-or-no-p "是否应用这些字段定义到标签？")
          (org-supertag-apply-recovered-field-definitions field-definitions))))))

(defun org-supertag-show-field-recovery-preview ()
  "Show preview of what can be recovered without applying changes."
  (interactive)
  (message "=== 字段恢复预览（不做更改） ===")
  
  (let ((field-usage (org-supertag-analyze-field-links)))
    (if (= (hash-table-count field-usage) 0)
        (message "没有发现可以恢复的字段信息")
      
      (let ((field-definitions (org-supertag-generate-field-definitions field-usage))
            (total-tags 0)
            (total-fields 0))
        
        (message "")
        (message "=== 可恢复的字段定义 ===")
        (maphash (lambda (tag-id field-defs)
                  (setq total-tags (1+ total-tags))
                  (setq total-fields (+ total-fields (length field-defs)))
                  (message "标签: %s (%d个字段)" tag-id (length field-defs))
                  (dolist (field-def field-defs)
                    (message "  - %-15s 类型: %-8s 使用: %d次 唯一值: %d个" 
                            (plist-get field-def :name)
                            (plist-get field-def :type)
                            (plist-get field-def :usage-count)
                            (plist-get field-def :unique-values))))
                field-definitions)
        
        (message "")
        (message "=== 恢复统计 ===")
        (message "可恢复标签数: %d" total-tags)
        (message "可恢复字段数: %d" total-fields)))))

;; 提供选择菜单
(defun org-supertag-field-recovery-menu ()
  "Field recovery menu."
  (interactive)
  (let ((choice (read-char-choice 
                 "字段定义恢复选项:
1. 预览可恢复的字段定义
2. 执行字段定义恢复
3. 取消
请选择 (1-3): " 
                 '(?1 ?2 ?3))))
    (cond
     ((eq choice ?1) (org-supertag-show-field-recovery-preview))
     ((eq choice ?2) (org-supertag-recover-fields-interactive))
     ((eq choice ?3) (message "已取消"))
     (t (message "无效选择")))))

;; 运行字段恢复菜单
(org-supertag-field-recovery-menu) 