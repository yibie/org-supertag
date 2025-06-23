;;; fix-data-sync.el --- Force complete data synchronization

(message "=== FORCE COMPLETE SYNC ===")

(defun org-supertag-force-complete-sync ()
  "Force a complete synchronization of all data to Python backend."
  (interactive)
  (message "\n1. Checking current database state...")
  
  ;; 统计当前数据
  (let ((total-objects (hash-table-count org-supertag-db--object))
        (total-links (hash-table-count org-supertag-db--link))
        (nodes-count 0)
        (tags-count 0)
        (other-count 0))
    
    ;; 统计各类型对象
    (maphash (lambda (_id props)
               (let ((type (plist-get props :type)))
                 (cond
                  ((eq type :node) (cl-incf nodes-count))
                  ((eq type :tag) (cl-incf tags-count))
                  (t (cl-incf other-count)))))
             org-supertag-db--object)
    
    (message "   Total objects: %d" total-objects)
    (message "   - Nodes: %d" nodes-count)
    (message "   - Tags: %d" tags-count)
    (message "   - Others: %d" other-count)
    (message "   - Links: %d" total-links)
    
    ;; 检查Python后端是否就绪
    (message "\n2. Checking Python backend status...")
    (unless (and (featurep 'org-supertag-bridge)
                 (boundp 'org-supertag-bridge--ready-p)
                 org-supertag-bridge--ready-p)
      (message "   Python backend not ready. Starting...")
      (org-supertag-init-vectorization)
      (sleep-for 3))
    
    (if (and (boundp 'org-supertag-bridge--ready-p)
             org-supertag-bridge--ready-p)
        (message "   ✓ Python backend is ready")
      (error "❌ Python backend failed to start"))
    
    ;; 准备完整快照数据
    (message "\n3. Preparing complete snapshot...")
    (let ((all-nodes '())
          (all-tags '())
          (all-links '()))
      
      ;; 收集所有节点和标签
      (maphash (lambda (id props)
                 (let ((type (plist-get props :type)))
                   (cond
                    ((eq type :node)
                     (push props all-nodes))
                    ((eq type :tag)
                     (push props all-tags)))))
               org-supertag-db--object)
      
      ;; 收集所有链接
      (maphash (lambda (_id link-props)
                 (push link-props all-links))
               org-supertag-db--link)
      
      (message "   Collected: %d nodes, %d tags, %d links"
               (length all-nodes) (length all-tags) (length all-links))
      
      ;; 构建完整快照
      (let ((snapshot-data `(("nodes" . ,all-nodes)
                            ("tags" . ,all-tags)
                            ("links" . ,all-links)
                            ("ids_to_delete" . [])  ; 空数组，不删除任何东西
                            ("sync_timestamp" . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))))
        
        (message "\n4. Sending complete snapshot to Python backend...")
        (message "   Snapshot size: %d total items" 
                 (+ (length all-nodes) (length all-tags) (length all-links)))
        
        ;; 发送数据
        (condition-case err
            (let ((result (org-supertag-api-bulk-process-snapshot
                          snapshot-data
                          (lambda (sync-result)
                            (if (and sync-result (equal (plist-get sync-result :status) "success"))
                                (progn
                                  (message "   ✓ Complete sync successful!")
                                  (message "   Result: %S" (plist-get sync-result :result))
                                  
                                  ;; 重置哈希表以触发重新计算
                                  (message "\n5. Updating sync hashes...")
                                  (clrhash org-supertag-background-sync--last-sync-hashes)
                                  
                                  ;; 为所有对象计算并保存新哈希
                                  (maphash (lambda (id props)
                                             (let ((hash (org-supertag-background-sync--calculate-object-hash props)))
                                               (when hash
                                                 (puthash id hash org-supertag-background-sync--last-sync-hashes))))
                                           org-supertag-db--object)
                                  
                                  (maphash (lambda (id props)
                                             (let ((hash (org-supertag-background-sync--calculate-object-hash props)))
                                               (when hash
                                                 (puthash id hash org-supertag-background-sync--last-sync-hashes))))
                                           org-supertag-db--link)
                                  
                                  (org-supertag-background-sync--save-hashes)
                                  (message "   ✓ Sync hashes updated: %d records"
                                           (hash-table-count org-supertag-background-sync--last-sync-hashes))
                                  
                                  (message "\n=== COMPLETE SYNC FINISHED SUCCESSFULLY ==="))
                              (error "Complete sync failed: %s" 
                                     (or (plist-get sync-result :message) "Unknown error")))))))
              
              (setq org-supertag-background-sync--last-sync-time (current-time))
              result)
          (error
           (message "❌ Error during complete sync: %s" (error-message-string err))
           (error "Complete sync failed: %s" (error-message-string err))))))))

;; 运行完整同步
(message "Starting force complete sync...")
(org-supertag-force-complete-sync) 