;;; test-intelligent-graphrag.el --- Test script for Intelligent GraphRAG system -*- lexical-binding: t; -*-

;;; Commentary:
;; 测试智能GraphRAG系统的各项功能
;; 使用方法：M-x load-file RET test-intelligent-graphrag.el RET
;; 然后运行：M-x test-intelligent-graphrag-all

;;; Code:

(require 'org-supertag-intelligent-graphrag)
(require 'org-supertag-intelligent-graphrag-ui)


(defvar test-igr-results nil
  "测试结果存储")

(defun test-igr-log (test-name status message)
  "记录测试结果"
  (let ((result (list :test test-name :status status :message message :time (current-time))))
    (push result test-igr-results)
    (message "[%s] %s: %s" 
             (if (eq status 'pass) "✓" "✗") 
             test-name message)))

(defun test-igr-check-epc-connection ()
  "测试EPC连接"
  (condition-case err
      (if (and org-supertag-sim-epc-manager
               (epc:live-p org-supertag-sim-epc-manager))
          (test-igr-log "EPC连接" 'pass "EPC服务器连接正常")
        (test-igr-log "EPC连接" 'fail "EPC服务器未连接"))
    (error 
     (test-igr-log "EPC连接" 'fail (format "EPC连接错误: %s" err)))))

(defun test-igr-check-system-status ()
  "测试系统状态"
  (condition-case err
      (let ((enabled org-supertag-igr-enabled)
            (vector-count (hash-table-count org-supertag-igr-node-vectors))
            (cache-count (hash-table-count org-supertag-igr-similarity-cache))
            (resource-count (hash-table-count org-supertag-igr-resource-index)))
        
        (if enabled
            (test-igr-log "系统状态" 'pass 
                         (format "系统已启用 | 向量:%d 缓存:%d 资源:%d" 
                                vector-count cache-count resource-count))
          (test-igr-log "系统状态" 'fail "系统未启用")))
    (error 
     (test-igr-log "系统状态" 'fail (format "状态检查错误: %s" err)))))

(defun test-igr-check-embedding-function ()
  "测试向量化功能"
  (condition-case err
      (let ((test-text "这是一个测试文本，用于验证向量化功能是否正常工作。"))
        (message "正在测试向量化功能...")
2025-05-29 16:19:15 [ERROR] simtag.epc_server: Task batch_node_vectorization_1_1748506755: Processing failed: 'list' object has no attribute 'get'
2025-05-29 16:19:15 [ERROR] simtag.epc_server: Traceback (most recent call last):
  File "/Users/chenyibin/Documents/emacs/package/org-supertag/simtag/epc_server.py", line 1024, in _process_nodes_vectorization
    node["node_id"],
    ~~~~^^^^^^^^^^^
TypeError: list indices must be integers or slices, not str

During handling of the above exception, another exception occurred:

Traceback (most recent call last):
  File "/Users/chenyibin/Documents/emacs/package/org-supertag/simtag/epc_server.py", line 1036, in _process_nodes_vectorization
    self.logger.error(f"Error processing node {node.get('node_id')}: {node_error}")
                                               ^^^^^^^^
AttributeError: 'list' object has no attribute 'get'
2025-05-29 16:19:15 [ERROR] simtag.epc_server: Task batch_node_vectorization_1_1748506755: Processing failed: 'list' object has no attribute 'get'
2025-05-29 16:19:15 [ERROR] simtag.epc_server: Traceback (most recent call last):
  File "/Users/chenyibin/Documents/emacs/package/org-supertag/simtag/epc_server.py", line 1024, in _process_nodes_vectorization
    node["node_id"],
    ~~~~^^^^^^^^^^^
TypeError: list indices must be integers or slices, not str

During handling of the above exception, another exception occurred:

Traceback (most recent call last):
  File "/Users/chenyibin/Documents/emacs/package/org-supertag/simtag/epc_server.py", line 1036, in _process_nodes_vectorization
    self.logger.error(f"Error processing node {node.get('node_id')}: {node_error}")
                                               ^^^^^^^^
AttributeError: 'list' object has no attribute 'get'
        (let ((result (epc:call-sync org-supertag-sim-epc-manager 
                                    'vectorize_node 
                                    (list "test_node" test-text "测试标题" nil))))
          (if (and result (plist-get result :result))
              (test-igr-log "向量化功能" 'pass "向量化功能正常工作")
            (test-igr-log "向量化功能" 'fail "向量化失败或返回空结果"))))
    (error 
     (test-igr-log "向量化功能" 'fail (format "向量化错误: %s" err)))))

(defun test-igr-check-similarity-calculation ()
  "测试相似度计算"
  (condition-case err
      (let ((test-text1 "Python是一种简单易学的编程语言")
            (test-text2 "Java是一种面向对象的编程语言"))
        
        (let ((result (epc:call-sync org-supertag-sim-epc-manager 
                                    'find_similar_nodes
                                    (list test-text1 5 t))))
          
          (if (and result (plist-get result :result))
              (test-igr-log "相似度计算" 'pass "相似度计算功能正常")
            (test-igr-log "相似度计算" 'fail "相似度计算失败"))))
    (error 
     (test-igr-log "相似度计算" 'fail (format "相似度计算错误: %s" err)))))

(defun test-igr-check-hybrid-search ()
  "测试混合搜索功能"
  (condition-case err
      (let ((test-query "编程语言"))
        (let ((result (epc:call-sync org-supertag-sim-epc-manager 
                                    'hybrid_search
                                    (list test-query 5))))
          
          (if (and result (plist-get result :result))
              (test-igr-log "混合搜索" 'pass "混合搜索功能正常")
            (test-igr-log "混合搜索" 'fail "混合搜索失败"))))
    (error 
     (test-igr-log "混合搜索" 'fail (format "混合搜索错误: %s" err)))))

(defun test-igr-check-batch-vectorization ()
  "测试批量向量化功能"
  (condition-case err
      (let ((test-nodes 
             (list 
              `(("node_id" . "test1")
                ("content" . "测试内容1")
                ("title" . "测试标题1")
                ("tags" . nil))
              `(("node_id" . "test2")
                ("content" . "测试内容2")
                ("title" . "测试标题2")
                ("tags" . nil)))))
        
        (let ((result (epc:call-sync org-supertag-sim-epc-manager 
                                    'batch_vectorize_nodes
                                    (list test-nodes 2))))
          
          (if (and result (plist-get result :result))
              (test-igr-log "批量向量化" 'pass "批量向量化功能正常")
            (test-igr-log "批量向量化" 'fail "批量向量化失败"))))
    (error 
     (test-igr-log "批量向量化" 'fail (format "批量向量化错误: %s" err)))))

(defun test-igr-check-resource-classification ()
  "测试资源分类功能"
  (condition-case err
      (let ((test-nodes 
             '((:title "机器学习项目" :content "这是一个关于深度学习的项目" :properties (:deadline "2024-12-31"))
               (:title "学习笔记" :content "今天学习了Python编程的基础知识")
               (:title "联系人信息" :content "张三是我们团队的技术负责人")
               (:title "工具推荐" :content "推荐使用VSCode作为开发工具"))))
        
        (let ((classifications 
               (mapcar #'org-supertag-igr-classify-node-resource-type test-nodes)))
          
          (if (and (eq (nth 0 classifications) :project)
                   (eq (nth 1 classifications) :learning)
                   (eq (nth 2 classifications) :person)
                   (eq (nth 3 classifications) :resource))
              (test-igr-log "资源分类" 'pass "资源分类功能正常")
            (test-igr-log "资源分类" 'fail 
                         (format "资源分类异常: %s" classifications)))))
    (error 
     (test-igr-log "资源分类" 'fail (format "资源分类错误: %s" err)))))

(defun test-igr-check-async-support ()
  "测试异步功能支持"
  (condition-case err
      (if (and (fboundp 'epc:call-deferred)
               (boundp 'org-supertag-igr-async-task-id)
               (boundp 'org-supertag-igr-progress-timer))
          (test-igr-log "异步支持" 'pass "异步功能支持正常")
        (test-igr-log "异步支持" 'fail "异步功能支持不完整"))
    (error 
     (test-igr-log "异步支持" 'fail (format "异步支持检查错误: %s" err)))))

(defun test-igr-check-graph-relation-weights ()
  "测试图关系权重配置"
  (condition-case err
      (if (and (boundp 'org-supertag-rag-graph-relation-weights)
               (listp org-supertag-rag-graph-relation-weights)
               (> (length org-supertag-rag-graph-relation-weights) 5))
          (test-igr-log "图关系权重" 'pass 
                       (format "图关系权重配置正常，包含%d种关系类型" 
                              (length org-supertag-rag-graph-relation-weights)))
        (test-igr-log "图关系权重" 'fail "图关系权重配置异常"))
    (error 
     (test-igr-log "图关系权重" 'fail (format "图关系权重检查错误: %s" err)))))

(defun test-igr-display-results ()
  "显示测试结果"
  (let ((buffer (get-buffer-create "*智能GraphRAG测试结果*"))
        (pass-count 0)
        (fail-count 0))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "🧪 智能GraphRAG系统测试结果\n\n" 
                           'face '(:height 1.3 :weight bold)))
        
        (dolist (result (reverse test-igr-results))
          (let* ((test-name (plist-get result :test))
                 (status (plist-get result :status))
                 (message (plist-get result :message))
                 (time (plist-get result :time))
                 (status-icon (if (eq status 'pass) "✅" "❌"))
                 (status-color (if (eq status 'pass) "dark-green" "red")))
            
            (if (eq status 'pass)
                (setq pass-count (1+ pass-count))
              (setq fail-count (1+ fail-count)))
            
            (insert (format "%s " status-icon))
            (insert (propertize (format "%-20s" test-name) 'face `(:weight bold :foreground ,status-color)))
            (insert (format " %s\n" message))))
        
        (insert "\n")
        (insert (propertize (format "📊 测试总结: %d个通过, %d个失败\n" pass-count fail-count)
                           'face '(:height 1.1 :weight bold)))
        
        (when (> fail-count 0)
          (insert "\n")
          (insert (propertize "🔧 故障排除建议:\n" 'face '(:weight bold :foreground "orange")))
          (insert "1. 确保EPC服务器正在运行: M-x org-supertag-sim-start\n")
          (insert "2. 启用智能GraphRAG系统: M-x org-supertag-igr-enable\n")
          (insert "3. 检查系统状态: M-x org-supertag-igr-system-status\n")
          (insert "4. 重建系统: M-x org-supertag-igr-rebuild-system\n"))
        
        (when (= fail-count 0)
          (insert "\n")
          (insert (propertize "🎉 所有测试通过！智能GraphRAG系统运行正常。\n" 
                             'face '(:weight bold :foreground "dark-green")))
          (insert "\n开始使用:\n")
          (insert "• M-x org-supertag-igr-show-node-analysis - 分析当前节点\n")
          (insert "• M-x org-supertag-igr-find-resources-for-tag - 查找标签资源\n")
          (insert "• 移动光标到org标题 - 自动显示智能建议\n"))
        
        (goto-char (point-min)))
      
      (pop-to-buffer buffer))))

;;;###autoload
(defun test-intelligent-graphrag-all ()
  "运行所有智能GraphRAG测试"
  (interactive)
  (setq test-igr-results nil)
  
  (message "开始智能GraphRAG系统测试...")
  
  ;; 运行所有测试
  (test-igr-check-epc-connection)
  (test-igr-check-system-status)
  (test-igr-check-embedding-function)
  (test-igr-check-similarity-calculation)
  (test-igr-check-hybrid-search)
  (test-igr-check-batch-vectorization)
  (test-igr-check-resource-classification)
  (test-igr-check-graph-relation-weights)
  
  ;; 显示结果
  (test-igr-display-results)
  
  (message "智能GraphRAG系统测试完成"))

;;;###autoload
(defun test-intelligent-graphrag-quick ()
  "快速测试智能GraphRAG基本功能"
  (interactive)
  (setq test-igr-results nil)
  
  (message "开始快速测试...")
  
  ;; 只运行关键测试
  (test-igr-check-epc-connection)
  (test-igr-check-system-status)
  (test-igr-check-embedding-function)
  
  ;; 显示结果
  (test-igr-display-results)
  
  (message "快速测试完成"))

(provide 'test-intelligent-graphrag)

;;; test-intelligent-graphrag.el ends here 