;;; debug-object-count.el --- Debug object count differences

(message "=== OBJECT COUNT ANALYSIS ===")

;; 1. 统计当前数据库状态
(message "\n1. Current database state:")
(let ((total-objects (hash-table-count org-supertag-db--object))
      (total-links (hash-table-count org-supertag-db--link))
      (nodes-count 0)
      (tags-count 0)
      (other-count 0)
      (active-nodes 0))
  
  ;; 统计各类型对象
  (maphash (lambda (_id props)
             (let ((type (plist-get props :type)))
               (cond
                ((eq type :node) 
                 (cl-incf nodes-count)
                 ;; 检查节点是否"活跃"（有内容或标题）
                 (when (or (plist-get props :content)
                          (plist-get props :title)
                          (plist-get props :raw-value))
                   (cl-incf active-nodes)))
                ((eq type :tag) (cl-incf tags-count))
                (t (cl-incf other-count)))))
           org-supertag-db--object)
  
  (message "   Total objects in hash table: %d" total-objects)
  (message "   - Nodes: %d (active: %d)" nodes-count active-nodes)
  (message "   - Tags: %d" tags-count)
  (message "   - Others: %d" other-count)
  (message "   Total links: %d" total-links)
  (message "   Grand total: %d" (+ total-objects total-links)))

;; 2. 模拟 get-changed-objects 的计算逻辑
(message "\n2. Simulating get-changed-objects logic:")
(let ((counted-objects 0)
      (counted-links 0)
      (nodes-to-upsert 0)
      (tags-to-upsert 0)
      (links-to-upsert 0)
      (ids-to-delete 0))
  
  ;; 模拟对象表的遍历
  (maphash (lambda (id props)
             (cl-incf counted-objects)
             (let ((type (plist-get props :type)))
               (cond
                ((eq type :node) (cl-incf nodes-to-upsert))
                ((eq type :tag) (cl-incf tags-to-upsert)))))
           org-supertag-db--object)
  
  ;; 模拟链接表的遍历
  (maphash (lambda (_id _props)
             (cl-incf counted-links)
             (cl-incf links-to-upsert))
           org-supertag-db--link)
  
  ;; 模拟删除检查（哈希表为空，所以没有删除）
  (setq ids-to-delete (hash-table-count org-supertag-background-sync--last-sync-hashes))
  
  (message "   Objects processed: %d" counted-objects)
  (message "   Links processed: %d" counted-links)
  (message "   Total checked: %d" (+ counted-objects counted-links))
  (message "   Changes breakdown:")
  (message "     - Nodes to upsert: %d" nodes-to-upsert)
  (message "     - Tags to upsert: %d" tags-to-upsert)
  (message "     - Links to upsert: %d" links-to-upsert)
  (message "     - IDs to delete: %d" ids-to-delete)
  (message "   Total changes: %d" (+ nodes-to-upsert tags-to-upsert links-to-upsert ids-to-delete)))

;; 3. 检查具体的ID提取逻辑
(message "\n3. ID extraction analysis:")
(let ((successful-extractions 0)
      (failed-extractions 0)
      (sample-failures '()))
  
  (maphash (lambda (id props)
             (let* ((extracted-id (or (plist-get props :id)
                                     (plist-get props :node-id)
                                     (plist-get props :tag-id)
                                     (plist-get props :link-id)
                                     (when (eq (plist-get props :type) :link)
                                       (format "%s->%s" 
                                               (or (plist-get props :source) "")
                                               (or (plist-get props :target) ""))))))
               (if extracted-id
                   (cl-incf successful-extractions)
                 (cl-incf failed-extractions)
                 (when (< (length sample-failures) 5)
                   (push (list id (plist-get props :type)) sample-failures)))))
           org-supertag-db--object)
  
  (maphash (lambda (id props)
             (let* ((extracted-id (or (plist-get props :id)
                                     (plist-get props :node-id)
                                     (plist-get props :tag-id)
                                     (plist-get props :link-id)
                                     (when (eq (plist-get props :type) :link)
                                       (format "%s->%s" 
                                               (or (plist-get props :source) "")
                                               (or (plist-get props :target) ""))))))
               (if extracted-id
                   (cl-incf successful-extractions)
                 (cl-incf failed-extractions)
                 (when (< (length sample-failures) 5)
                   (push (list id "link") sample-failures)))))
           org-supertag-db--link)
  
  (message "   Successful ID extractions: %d" successful-extractions)
  (message "   Failed ID extractions: %d" failed-extractions)
  (when sample-failures
    (message "   Sample failures:")
    (dolist (failure sample-failures)
      (message "     - Key: %s, Type: %s" (car failure) (cadr failure)))))

;; 4. 检查文件范围
(message "\n4. File scope analysis:")
(let ((files-with-nodes (make-hash-table :test 'equal))
      (unique-files 0))
  
  (maphash (lambda (_id props)
             (when (eq (plist-get props :type) :node)
               (let ((file-path (plist-get props :file-path)))
                 (when file-path
                   (puthash file-path t files-with-nodes)))))
           org-supertag-db--object)
  
  (setq unique-files (hash-table-count files-with-nodes))
  (message "   Unique files with nodes: %d" unique-files)
  
  (when (> unique-files 0)
    (message "   Sample files:")
    (let ((count 0))
      (maphash (lambda (file _)
                 (when (< count 5)
                   (message "     - %s" file)
                   (cl-incf count)))
               files-with-nodes))))

(message "\n=== ANALYSIS COMPLETED ===") 