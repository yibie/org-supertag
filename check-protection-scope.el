;;; check-protection-scope.el --- 检查保护机制覆盖范围

;;; Commentary:
;; 检查修复的保护机制是否覆盖字段定义和标签关系

;;; Code:

(defun check-protection-analyze-all-types ()
  "分析数据库中所有实体类型."
  (interactive)
  (message "=== 数据库实体类型分析 ===")
  (message "")
  
  (let ((entity-types (make-hash-table :test 'equal))
        (total-entities 0)
        (sample-entities (make-hash-table :test 'equal)))
    
    ;; 分析 org-supertag-db--object 中的实体
    (message "📊 分析 org-supertag-db--object:")
    (maphash (lambda (id entity)
               (let ((type (plist-get entity :type)))
                 (setq total-entities (1+ total-entities))
                 (puthash type (1+ (gethash type entity-types 0)) entity-types)
                 
                 ;; 收集每种类型的示例
                 (unless (gethash type sample-entities)
                   (puthash type (list :id id :entity entity) sample-entities))))
            org-supertag-db--object)
    
    (message "  总实体数: %d" total-entities)
    (message "  实体类型分布:")
    (maphash (lambda (type count)
               (message "    %s: %d" (or type "nil/Unknown") count))
            entity-types)
    
    entity-types))

(defun check-protection-analyze-links ()
  "分析链接表中的数据."
  (interactive)
  (message "")
  (message "📊 分析 org-supertag-db--link:")
  
  (if (not (boundp 'org-supertag-db--link))
      (message "  ❌ org-supertag-db--link 未定义")
    (let ((total-links (hash-table-count org-supertag-db--link))
          (link-types (make-hash-table :test 'equal)))
      
      (message "  总链接数: %d" total-links)
      
      (when (> total-links 0)
        ;; 分析链接类型
        (maphash (lambda (link-id link-data)
                   (let ((link-type (plist-get link-data :type)))
                     (puthash link-type (1+ (gethash link-type link-types 0)) link-types))
                   
                   ;; 检查是否有字段相关的链接
                   (when (and (stringp link-id)
                             (string-prefix-p ":node-field:" link-id))
                     (message "    发现字段链接: %s" link-id)
                     (message "      字段名: %s" (plist-get link-data :to))
                     (message "      标签ID: %s" (plist-get link-data :tag-id))
                     (message "      值: %s" (plist-get link-data :value))))
                org-supertag-db--link)
        
        (message "  链接类型分布:")
        (maphash (lambda (type count)
                   (message "    %s: %d" (or type "nil") count))
                link-types))
      
      link-types)))

(defun check-protection-analyze-field-storage ()
  "分析字段定义的存储方式."
  (interactive)
  (message "")
  (message "🔍 分析字段定义存储:")
  
  (let ((tags-with-fields 0)
        (total-field-definitions 0)
        (field-examples '()))
    
    ;; 检查标签实体中的字段定义
    (maphash (lambda (id entity)
               (when (eq (plist-get entity :type) :tag)
                 (let ((fields (plist-get entity :fields)))
                   (when fields
                     (setq tags-with-fields (1+ tags-with-fields))
                     (setq total-field-definitions (+ total-field-definitions (length fields)))
                     
                     ;; 收集字段示例
                     (when (< (length field-examples) 3)
                       (push (list :tag-id id :fields fields) field-examples))))))
            org-supertag-db--object)
    
    (message "  有字段定义的标签: %d" tags-with-fields)
    (message "  总字段定义数: %d" total-field-definitions)
    
    (when field-examples
      (message "  字段定义示例:")
      (dolist (example field-examples)
        (let ((tag-id (plist-get example :tag-id))
              (fields (plist-get example :fields)))
          (message "    标签 '%s' 的字段:" tag-id)
          (dolist (field (seq-take fields 2))
            (message "      - %s (%s)" 
                    (plist-get field :name)
                    (plist-get field :type))))))
    
    (list :tags-with-fields tags-with-fields 
          :total-fields total-field-definitions)))

(defun check-protection-analyze-relations ()
  "分析标签关系的存储方式."
  (interactive)
  (message "")
  (message "🔍 分析标签关系存储:")
  
  ;; 1. 检查是否有关系类型的实体
  (let ((relation-entities 0)
        (cooccurrence-links 0)
        (metadata-relations 0))
    
    ;; 检查 object 表中的关系实体
    (maphash (lambda (id entity)
               (let ((type (plist-get entity :type)))
                 (when (or (eq type :relation)
                          (eq type :cooccurrence)
                          (and (stringp (format "%s" type))
                               (string-match-p "relation\\|cooccur" (format "%s" type))))
                   (setq relation-entities (1+ relation-entities))
                   (message "  发现关系实体: %s (类型: %s)" id type))))
            org-supertag-db--object)
    
    ;; 检查 link 表中的共现关系
    (maphash (lambda (link-id link-data)
               (let ((link-type (plist-get link-data :type)))
                 (when (eq link-type 'cooccurrence)
                   (setq cooccurrence-links (1+ cooccurrence-links)))))
            org-supertag-db--link)
    
    ;; 检查元数据中的关系信息
    (let ((metadata (org-supertag-db-get "metadata")))
      (when metadata
        (let ((metadata-hash (plist-get metadata :data)))
          (when (hash-table-p metadata-hash)
            (maphash (lambda (key value)
                       (when (and (symbolp key)
                                 (string-prefix-p "tag-cooccur:" (symbol-name key)))
                         (setq metadata-relations (1+ metadata-relations))))
                    metadata-hash)))))
    
    (message "  关系实体数: %d" relation-entities)
    (message "  共现链接数: %d" cooccurrence-links)
    (message "  元数据关系数: %d" metadata-relations)
    
    (list :relation-entities relation-entities
          :cooccurrence-links cooccurrence-links
          :metadata-relations metadata-relations)))

(defun check-protection-simulate-coverage ()
  "模拟保护机制的覆盖范围."
  (interactive)
  (message "")
  (message "🛡️  保护机制覆盖范围分析:")
  
  (let ((protected-entities (make-hash-table :test 'equal))
        (protected-links (make-hash-table :test 'equal))
        (by-type (make-hash-table :test 'equal)))
    
    ;; 模拟当前的保护逻辑 - 只保护 object 表中的非节点实体
    (maphash (lambda (id entity)
               (let ((entity-type (plist-get entity :type)))
                 (when (and entity-type (not (eq entity-type :node)))
                   (puthash id entity protected-entities)
                   (puthash entity-type (1+ (gethash entity-type by-type 0)) by-type))))
            org-supertag-db--object)
    
    (message "  ✅ 当前保护的实体类型:")
    (maphash (lambda (type count)
               (message "    %s: %d" type count))
            by-type)
    
    ;; 检查 link 表是否需要保护（当前修复没有保护）
    (let ((total-links (hash-table-count org-supertag-db--link)))
      (message "  ⚠️  未保护的数据:")
      (message "    链接表数据: %d 条 (包含字段值、关系等)" total-links))
    
    ;; 总结
    (message "")
    (message "📋 保护覆盖总结:")
    (message "  ✅ 已保护: 标签定义、元数据")
    (message "  ❓ 字段定义: 存储在标签的 :fields 属性中 → ✅ 已保护")
    (message "  ❓ 字段值: 存储在链接表中 → ⚠️  未保护")
    (message "  ❓ 标签关系: 存储在链接表和元数据中 → 部分保护")))

(defun check-protection-main ()
  "主检查函数."
  (interactive)
  (message "=== org-supertag 保护机制覆盖范围检查 ===")
  (message "")
  
  ;; 依次执行各项检查
  (check-protection-analyze-all-types)
  (check-protection-analyze-links) 
  (check-protection-analyze-field-storage)
  (check-protection-analyze-relations)
  (check-protection-simulate-coverage)
  
  (message "")
  (message "=== 检查完成 ==="))

;; 运行检查
(check-protection-main)

(provide 'check-protection-scope)

;;; check-protection-scope.el ends here 