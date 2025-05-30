;;; org-supertag-intelligent-graphrag.el --- Intelligent GraphRAG for automatic discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; 智能GraphRAG系统 - 基于内容向量化和图关系的自动发现
;; 
;; 核心目标：
;; 1. 真正将 node、tag、relation、behavior 等关系联系在一起
;; 2. 提供自动化功能，发现相似 node 和 tag，无需用户手动维护
;; 3. 智能资源检索：为用户标签自动匹配相关笔记、想法、资源、工具、人员
;;
;; 主要特性：
;; - 内容自动向量化（Node级别）
;; - 智能相似性发现
;; - 多维度关系融合（显性标签关系 + 隐性内容极速）
;; - 实时智能建议
;; - 自动资源匹配

;;; Code:

(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-tag)
(require 'org-supertag-relation)
(require 'org-supertag-sim-epc)
(require 'cl-lib)
(require 'json)
(require 'epc)

;;; =================================================================
;;; EPC 服务初始化
;;; =================================================================

(defvar org-supertag-igr--epc-initialized nil
  "EPC服务是否已初始化")

(defun org-supertag-igr-initialize-epc ()
  "初始化EPC服务"
  (interactive)
  (unless org-supertag-igr--epc-initialized
    (condition-case err
        (progn
          ;; 确保org-supertag-sim-epc模块已加载
          (require 'org-supertag-sim-epc)
          
          ;; 如果EPC服务器未运行，尝试启动
          (unless (org-supertag-sim-epc-server-running-p)
            (org-supertag-sim-epc-start-server)
            ;; 等待服务器启动
            (let ((max-attempts 5)
                  (attempt 0)
                  success)
              (while (and (< attempt max-attempts)
                         (not success))
                (sit-for 1)
                (setq attempt (1+ attempt))
                (when (org-supertag-sim-epc-server-running-p)
                  (setq success t)))
              (unless success
                (error "EPC服务器启动超时"))))
          
          ;; 确保EPC管理器存在并正常运行
          (unless (and (boundp 'org-supertag-sim-epc-manager)
                      org-supertag-sim-epc-manager
                      (org-supertag-sim-epc-server-running-p))
            (error "EPC服务器未正常运行"))
          
          ;; 测试EPC连接
          (condition-case test-err
              (progn
                (org-supertag-sim-epc-echo-test)
                ;; 设置初始化标志
                (setq org-supertag-igr--epc-initialized t)
                (message "EPC服务已连接"))
            (error
             (error "EPC连接测试失败: %s" (error-message-string test-err)))))
      (error
       (message "EPC服务初始化失败: %s" (error-message-string err))
       (setq org-supertag-igr--epc-initialized nil)
       ;; 尝试紧急重启
       (when (yes-or-no-p "是否尝试紧急重启EPC服务？")
         (org-supertag-sim-epc-emergency-restart)
         (org-supertag-igr-initialize-epc))))))

(defun org-supertag-igr-ensure-epc-initialized ()
  "确保EPC服务已初始化"
  (unless org-supertag-igr--epc-initialized
    (org-supertag-igr-initialize-epc)))

;;; =================================================================
;;; 配置变量
;;; =================================================================

(defgroup org-supertag-intelligent-graphrag nil
  "Intelligent GraphRAG for automatic discovery."
  :group 'org-supertag
  :prefix "org-supertag-igr-")

(defcustom org-supertag-igr-enabled nil
  "是否启用智能GraphRAG功能"
  :type 'boolean
  :group 'org-supertag-intelligent-graphrag)

(defcustom org-supertag-igr-auto-vectorize t
  "是否自动对新内容进行向量化"
  :type 'boolean
  :group 'org-supertag-intelligent-graphrag)

(defcustom org-supertag-igr-similarity-threshold 0.7
  "内容相似度阈值（0.0-1.0）"
  :type 'float
  :group 'org-supertag-intelligent-graphrag)

(defcustom org-supertag-igr-max-suggestions 10
  "最大建议数量"
  :type 'integer
  :group 'org-supertag-intelligent-graphrag)

(defcustom org-supertag-igr-auto-suggest-delay 2.0
  "自动建议触发延迟（秒）"
  :type 'float
  :group 'org-supertag-intelligent-graphrag)

;;; 融合权重配置：显性关系 vs 隐性相似性
(defvar org-supertag-igr-fusion-weights
  '(:explicit-relations 0.4    ; 用户定义的标签关系
    :content-similarity 0.3    ; 内容向量相似度
    :tag-cooccurrence 0.2      ; 标签共现
    :temporal-proximity 0.1)   ; 时间邻近性
  "多维度关系融合权重配置")

;;; =================================================================
;;; 数据结构和缓存
;;; =================================================================

(defvar org-supertag-igr-node-vectors (make-hash-table :test 'equal)
  "节点内容向量缓存 node-id -> vector")

(defvar org-supertag-igr-similarity-cache (make-hash-table :test 'equal)
  "相似度计算缓存 (node-id1 . node-id2) -> similarity")

(defvar org-supertag-igr-suggestion-cache (make-hash-table :test 'equal)
  "智能建议缓存 node-id -> suggestions")

(defvar org-supertag-igr-auto-suggest-timer nil
  "自动建议定时器")

(defun org-supertag-igr-find-similar-nodes (node-id &optional max-results)
  "基于多维度相似性发现相关节点
NODE-ID: 目标节点
MAX-RESULTS: 最大结果数量
返回: ((node-id . score) ...)"
  (let* ((max-results (or max-results org-supertag-igr-max-suggestions))
         (cache-key (format "similar:%s:%d" node-id max-results))
         (cached-result (gethash cache-key org-supertag-igr-suggestion-cache)))
    
    (if cached-result
        cached-result
      (when-let* ((node (org-supertag-db-get node-id))
                  (content (org-supertag-igr-extract-node-content node)))
        (condition-case err
            (let ((result (epc:call-sync org-supertag-sim-epc-manager 
                                        'find_similar_nodes
                                        (list content max-results t))))
              (when (and result (plist-get result :result))
                (let ((similar-nodes 
                       (cl-remove-if-not
                        (lambda (node-data)
                          (let ((similar-node-id (cdr (assoc "node_id" node-data)))
                            ;; 确保节点存在于数据库中
                            (org-supertag-db-get similar-node-id)))
                        (plist-get result :result))))
                  ;; 构造返回值
                  (let ((nodes-with-scores
                         (mapcar (lambda (node-data)
                                 (cons (cdr (assoc "node_id" node-data))
                                      (cdr (assoc "similarity" node-data))))
                               similar-nodes)))
                    ;; 缓存结果
                    (puthash cache-key nodes-with-scores org-supertag-igr-suggestion-cache)
                    nodes-with-scores))))
          (error
           (message "查找相似节点失败: %s" err)
           nil)))))))

(defun org-supertag-igr-display-smart-suggestions-panel (node-id similar-nodes)
  "显示智能建议面板"
  (let ((buffer (get-buffer-create "*智能GraphRAG建议*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        
        ;; 标题
        (insert (propertize "🧠 智能GraphRAG建议\n\n" 
                           'face '(:height 1.3 :weight bold)))
        
        ;; 当前节点信息
(let ((node (org-supertag-db-get node-id)))
  (insert (format "📍 当前节点: %s\n\n" 
                 (or (plist-get node :title) "无标题节点"))))
        
        ;; 相似节点
        (when similar-nodes
          (insert (propertize "🔗 相似节点\n" 'face '(:height 1.1 :weight bold)))
          (dolist (item similar-nodes)
            (when-let* ((similar-node-id (car item))
                       (score (cdr item))
                       (similar-node (org-supertag-db-get similar-node-id))
                       (title (or (plist-get similar-node :title) "无标题")))
              (insert (format "  • %s (相似度: %.2f)" title score))
              (when-let* ((file-path (plist-get similar-node :file-path)))
                (insert (format " - %s" (file-name-nondirectory file-path))))
              (insert "\n")))
          (insert "\n"))
        
        (goto-char (point-min)))
      
      (display-buffer buffer '(display-buffer-in-side-window 
                              (side . right) 
                              (window-width . 0.3))))))

(defun org-supertag-igr-extract-node-content (node)
  "提取节点的完整内容用于向量化"
  (let* ((title (plist-get node :title))
         (content (plist-get node :content))
         (tags (mapcar #'org-supertag-tag-get-name-by-id 
                      (org-supertag-node-get-tags (plist-get node :id))))
         (properties (plist-get node :properties)))
    
    ;; 组合标题、内容、标签和重要属性
    (string-join
     (delq nil
           (list title
                 content
                 (when tags (format "标签: %s" (string-join tags " ")))
                 (when (plist-get properties :deadline)
                   (format "截止时间: %s" (plist-get properties :deadline)))
                 (when (plist-get properties :scheduled)
                    (format "计划时间: %s" (plist-get properties :scheduled)))))
     " ")))

(defun org-supertag-igr-cursor-moved ()
  "光标移动检测，触发智能建议"
  (when (and org-supertag-igr-enabled
             (derived-mode-p 'org-mode)
             (org-at-heading-p))
    
    ;; 取消之前的定时器
    (when org-supertag-igr-auto-suggest-timer
      (cancel-timer org-supertag-igr-auto-suggest-timer))
    
    ;; 设置新的定时器
    (setq org-supertag-igr-auto-suggest-timer
          (run-with-timer org-supertag-igr-auto-suggest-delay nil
                         #'org-supertag-igr-show-smart-suggestions))))

(defun org-supertag-igr-show-smart-suggestions ()
  "显示智能建议面板"
  (when (and org-supertag-igr-enabled
             (derived-mode-p 'org-mode)
             (org-at-heading-p))
    
    (let* ((node-id (org-id-get-create))
           (similar-nodes (org-supertag-igr-find-similar-nodes node-id))
           (node-tags (org-supertag-node-get-tags node-id)))
      
      (when similar-nodes
        (org-supertag-igr-display-smart-suggestions-panel 
         node-id similar-nodes)))))

(defun org-supertag-igr-clear-caches ()
  "清空所有缓存"
  (clrhash org-supertag-igr-node-vectors)
  (clrhash org-supertag-igr-similarity-cache)
  (clrhash org-supertag-igr-suggestion-cache))

;;; =================================================================
;;; 核心系统管理
;;; =================================================================

;;;###autoload
(defun org-supertag-igr-enable ()
  "启用智能GraphRAG系统"
  (interactive)
  (setq org-supertag-igr-enabled t)
  (org-supertag-igr-setup-hooks)
  (org-supertag-igr-initialize-system)
  (message "智能GraphRAG系统已启用"))

;;;###autoload
(defun org-supertag-igr-disable ()
  "禁用智能GraphRAG系统"
  (interactive)
  (setq org-supertag-igr-enabled nil)
  (org-supertag-igr-cleanup-hooks)
  (org-supertag-igr-cleanup-system)
  (message "智能GraphRAG系统已禁用"))

(defun org-supertag-igr-setup-hooks ()
  "设置必要的钩子函数"
  (add-hook 'org-supertag-node-created-hook #'org-supertag-igr-on-node-created)
  (add-hook 'org-supertag-node-updated-hook #'org-supertag-igr-on-node-updated)
  (add-hook 'org-supertag-tag-added-hook #'org-supertag-igr-on-tag-added)
  (add-hook 'post-command-hook #'org-supertag-igr-cursor-moved))

(defun org-supertag-igr-cleanup-hooks ()
  "清理钩子函数"
  (remove-hook 'org-supertag-node-created-hook #'org-supertag-igr-on-node-created)
  (remove-hook 'org-supertag-node-updated-hook #'org-supertag-igr-on-node-updated)
  (remove-hook 'org-supertag-tag-added-hook #'org-supertag-igr-on-tag-added)
  (remove-hook 'post-command-hook #'org-supertag-igr-cursor-moved))

(defun org-supertag-igr-initialize-system ()
  "初始化智能GraphRAG系统"
  ;; 初始化EPC服务
  (org-supertag-igr-initialize-epc)
  (unless org-supertag-igr--epc-initialized
    (error "EPC服务初始化失败，系统无法正常工作"))
  
  ;; 清空缓存
  (org-supertag-igr-clear-caches)
  
  ;; 自动向量化
  (when org-supertag-igr-auto-vectorize
    (org-supertag-igr-batch-vectorize-existing-nodes))
  
  (message "智能GraphRAG系统初始化完成"))

(defun org-supertag-igr-cleanup-system ()
  "清理系统资源"
  (org-supertag-igr-clear-caches)
  (when org-supertag-igr-auto-suggest-timer
    (cancel-timer org-supertag-igr-auto-suggest-timer)
    (setq org-supertag-igr-auto-suggest-timer nil)))

;;; =================================================================
;;; 内容向量化系统
;;; =================================================================

(defun org-supertag-igr-vectorize-node (node-id)
  "对节点内容进行向量化
NODE-ID: 节点ID
返回向量或nil"
  ;; 确保EPC服务已初始化
  (org-supertag-igr-ensure-epc-initialized)
  (unless org-supertag-igr--epc-initialized
    (error "EPC服务未初始化，无法进行向量化"))
  
  (when-let* ((node (org-supertag-db-get node-id))
              (content (org-supertag-igr-extract-node-content node))
              (tags (org-supertag-node-get-tags node-id)))
    (condition-case err
        (let ((result (epc:call-sync org-supertag-sim-epc-manager 
                                    'vectorize_node 
                                    (list node-id content 
                                          (plist-get node :title)
                                          (mapcar #'org-supertag-tag-get-name-by-id tags)))))
          (when (and result (plist-get result :result))
            (message "节点 %s 向量化成功" node-id)
            t))
      (error
       (message "向量化节点失败 %s: %s" node-id err)
       nil))))

(defun org-supertag-igr-get-content-embedding (content)
  "获取内容的向量表示"
  (when (and content (not (string-empty-p content)))
    (condition-case err
        (let ((result (epc:call-sync org-supertag-sim-epc-manager 'get_embedding 
                                   (list (substring content 0 (min 8000 (length content))))))
          (when (and result (plist-get result :result))
            (let ((embedding-data (plist-get result :result)))
              (plist-get embedding-data :embedding))))
      (error
       (message "获取向量表示失败: %s" err)
       nil)))))

(defvar org-supertag-igr-async-task-id nil
  "当前异步向量化任务ID（已弃用）")

(defvar org-supertag-igr-progress-timer nil
  "进度检查定时器（已弃用）")

(defun org-supertag-igr-batch-vectorize-existing-nodes ()
  "批量向量化现有节点"
  (interactive)
  
  ;; 确保EPC服务已初始化
  (org-supertag-igr-ensure-epc-initialized)
  (unless org-supertag-igr--epc-initialized
    (error "EPC服务未初始化，无法进行向量化"))
  
  (let* ((all-nodes (org-supertag-db-find-by-type :node))
         ;; 过滤掉无效的节点，但条件更宽松
         (valid-nodes (cl-remove-if-not
                      (lambda (node-id)
                        (when-let* ((node (org-supertag-db-get node-id)))
                          (and node
                               (eq (plist-get node :type) :node)
                               ;; 只要有标题、内容、属性或标签中的任意一个即可
                               (or (plist-get node :title)
                                   (plist-get node :content)
                                   (plist-get node :properties)
                                   (plist-get node :tags)))))
                      all-nodes))
         (total-nodes (length valid-nodes))
         (progress-reporter (make-progress-reporter 
                           (format "正在向量化节点 (共%d个)..." total-nodes)
                           0 total-nodes)))
    
    (if (< total-nodes 1)
        (message "没有找到需要向量化的节点")
      
      ;; 准备节点数据
      (let ((nodes-data 
             (mapcar 
              (lambda (node-id)
                (let* ((node (org-supertag-db-get node-id))
                       (node-id-str (if (symbolp node-id) 
                                      (symbol-name node-id) 
                                    (format "%s" node-id)))
                       (content (or (org-supertag-igr-extract-node-content node) ""))
                       (title (or (plist-get node :title) ""))
                       (tags (mapcar (lambda (tag-id) 
                                     (or (org-supertag-tag-get-name-by-id tag-id) ""))
                                   (org-supertag-node-get-tags node-id))))
                  (list (cons "node_id" node-id-str)
                        (cons "content" content)
                        (cons "title" title)
                        (cons "tags" tags))))
              valid-nodes)))
        
        ;; 调用后端进行批量向量化
        (condition-case err
            (let ((result (epc:call-sync org-supertag-sim-epc-manager 
                                        'batch_vectorize_nodes_backend
                                        (list nodes-data))))
              (when (and result (plist-get result :result))
                (let* ((success-count (plist-get (plist-get result :result) :success_count))
                       (failed-nodes (plist-get (plist-get result :result) :failed_nodes)))
                  ;; 更新进度
                  (progress-reporter-done progress-reporter)
                  ;; 显示失败节点信息
                  (when failed-nodes
                    (with-current-buffer (get-buffer-create "*向量化失败节点*")
                      (erase-buffer)
                      (insert (format "向量化失败节点 (%d个):\n\n" (length failed-nodes)))
                      (dolist (node-info failed-nodes)
                        (let* ((node-id (cdr (assoc "node_id" node-info)))
                               (error-msg (cdr (assoc "error" node-info)))
                               (node (org-supertag-db-get node-id)))
                          (insert (format "- %s: %s\n  错误: %s\n" 
                                        node-id 
                                        (or (plist-get node :title) "无标题")
                                        error-msg))))
                      (display-buffer (current-buffer))))
                  (message "向量化完成: 成功%d 失败%d" 
                          success-count (length failed-nodes)))))
          (error
           (message "批量向量化失败: %s" (error-message-string err))))))))

(defun org-supertag-igr-start-progress-monitoring (task-id node-ids)
  "开始监控异步向量化进度（已弃用）"
  (message "异步向量化功能已弃用，请使用同步版本"))

(defun org-supertag-igr-check-vectorization-progress (task-id node-ids)
  "检查向量化进度（已弃用）"
  (message "异步向量化功能已弃用，请使用同步版本"))

(defun org-supertag-igr-finish-vectorization (task-id node-ids status-data)
  "完成向量化处理（已弃用）"
  (message "异步向量化功能已弃用，请使用同步版本"))

(defun org-supertag-igr-handle-vectorization-error (task-id status-data)
  "处理向量化错误（已弃用）"
  (message "异步向量化功能已弃用，请使用同步版本"))

(defun org-supertag-igr-handle-vectorization-cancelled (task-id)
  "处理向量化取消（已弃用）"
  (message "异步向量化功能已弃用，请使用同步版本"))

(defun org-supertag-igr-cancel-vectorization ()
  "取消当前的向量化任务（已极速）"
  (interactive)
  (message "异步向量化功能已弃用，同步版本无需取消"))

;;; =================================================================
;;; 智能相似性发现
;;; =================================================================

(defun org-supertag-igr-hybrid-search (query &optional max-results)
  "混合搜索标签和节点
QUERY: 搜索查询
MAX-RESULTS: 最大结果数量
返回: 包含标签和节点的混合结果列表"
  (let ((max-results (or max-results org-supertag-igr-max-suggestions)))
    (condition-case err
        (let ((result (epc:call-sync org-supertag-sim-epc-manager 
                                    'hybrid_search
                                    (list query max-results))))
          (when (and result (plist-get result :result))
            (plist-get result :result)))
      (error
       (message "混合搜索失败: %s" err)
       nil))))

(defun org-supertag-igr-update-node-vector (node-id)
  "更新节点向量
NODE-ID: 节点ID"
  (when-let* ((node (org-supertag-db-get node-id))
              (content (org-supertag-igr-extract-node-content node))
              (tags (org-supertag-node-get-tags node-id)))
    (condition-case err
        (let ((result (epc:call-sync org-supertag-sim-epc-manager 
                                    'update_node_vector
                                    (list node-id content 
                                          (plist-get node :title)
                                          (mapcar #'org-supertag-tag-get-name-by-id tags)))))
          (when (and result (plist-get result :result))
            (message "节点 %s 向量已更新" node-id)
            t))
      (error
       (message "更新节点向量失败 %s: %s" node-id err)
       nil))))

(defun org-supertag-igr-remove-node-vector (node-id)
  "删除节点向量
NODE-ID: 节点ID"
  (condition-case err
      (let ((result (epc:call-sync org-supertag-sim-epc-manager 
                                  'remove_node_vector
                                  (list node-id))))
        (when (and result (plist-get result :result))
          (message "节点 %s 向量已删除" node-id)
          t))
    (error
     (message "删除节点向量失败 %s: %s" node-id err)
     nil)))

(defun org-supertag-igr-find-similar-nodes-by-tag (tag-name)
  "基于标签查找相似节点"
  (let* ((tag-id (org-supertag-tag-get-id-by-name tag-name))
         (tag-nodes (org-supertag-tag-get-nodes tag-id))
         (all-similar nil))
    
    (dolist (node-id tag-nodes)
      (let ((similar (org-supertag-igr-find-similar-nodes node-id 5)))
        (setq all-similar (append all-similar similar))))
    
    ;; 去重并按分数排序
    (let ((unique-similar (make-hash-table :test 'equal)))
      (dolist (item all-similar)
        (let ((node-id (car item))
              (score (cdr item)))
          (when (> score (gethash node-id unique-similar 0.0))
            (puthash node-id score unique-similar))))
      
      (let ((result nil))
        (maphash (lambda (node-id score)
                   (push (cons node-id score) result))
                 unique-similar)
        (sort result (lambda (a b) (> (cdr a) (cdr b))))))))

;;; =================================================================
;;; 事件处理系统
;;; =================================================================

(defun org-supertag-igr-on-node-created (node-id)
  "节点创建时的处理"
  (when org-supertag-igr-auto-vectorize
    (run-with-timer 1.0 nil 
                    (lambda () (org-supertag-igr-vectorize-node node-id)))))

(defun org-supertag-igr-on-node-updated (node-id)
  "节点更新时的处理"
  ;; 清除相关缓存
  (org-supertag-igr-invalidate-node-cache node-id)
  ;; 重新向量化
  (when org-supertag-igr-auto-vectorize
    (run-with-timer 1.0 nil 
                    (lambda () (org-supertag-igr-vectorize-node node-id)))))

(defun org-supertag-igr-on-tag-added (node-id tag-id)
  "标签添加时的处理 - 核心场景实现"
  (let* ((tag-name (org-supertag-tag-get-name-by-id tag-id))
         (node (org-supertag-db-get node-id)))
    
    ;; 场景：用户添加 #project 标签时自动检索相关资源
    ;; 场景：用户添加 #project 标签时自动检索相关资源
    (when (string-match-p "project\\|项目" tag-name)
      ;; (run-with-timer 0.5 nil
      ;;                 (lambda ()
      ;;                   (org-supertag-igr-auto-suggest-project-resources node-id tag-name)))
      (message "项目资源建议功能已移除")
      )
    
    ;; 更新资源索引 (移除)
    ;; (org-supertag-igr-update-resource-index-for-node node-id)
    ))

;;; =================================================================
;;; 工具函数
;;; =================================================================

(defun org-supertag-igr-invalidate-node-cache (node-id)
  "失效指定节点的相关缓存"
  (remhash node-id org-supertag-igr-node-vectors)
  (remhash node-id org-supertag-igr-suggestion-cache)
  
  ;; 清除相似度缓存中涉及该节点的条目
  (let ((keys-to-remove nil))
    (maphash (lambda (key _)
               (when (or (string-match-p node-id (car key))
                        (string-match-p node-id (cdr key)))
                 (push key keys-to-remove)))
             org-supertag-igr-similarity-cache)
    
    (dolist (key keys-to-remove)
      (remhash key org-supertag-igr-similarity-cache))))

;;; =================================================================
;;; 图关系权重配置（与org-supertag-relation.el集成）
;;; =================================================================

(defvar org-supertag-rag-graph-relation-weights
  '((contain . 0.9)      ; 包含关系 - 最高权重
    (belong . 0.9)       ; 归属关系 - 最高权重
    (cause . 0.8)        ; 因果关系 - 高权重
    (effect . 0.8)       ; 效果关系 - 高权重
    (dependency . 0.7)   ; 依赖关系 - 中高权重
    (prerequisite . 0.7) ; 前置关系 - 中高权重
    (influence . 0.6)    ; 影响关系 - 中等权重
    (relate . 0.5)       ; 一般关系 - 中等权重
    (parallel . 0.4)     ; 并行关系 - 较低权重
    (contrast . 0.3)     ; 对比关系 - 低权重
    (cooccurrence . 0.2)) ; 共现关系 - 最低权重
  "图检索中不同关系类型的权重配置")

;;; =================================================================
;;; 交互式命令
;;; =================================================================

;;;###autoload
(defun org-supertag-igr-show-node-analysis ()
  "显示当前节点的智能分析"
  (interactive)
  (if (not org-supertag-igr-enabled)
      (message "请先启用智能GraphRAG系统: M-x org-supertag-igr-enable")
    (if (not (org-at-heading-p))
        (message "请将光标移动到org标题上")
      (let* ((node-id (org-id-get-create))
             (similar-nodes (org-supertag-igr-find-similar-nodes node-id)))
        
        (if similar-nodes
            (org-supertag-igr-display-smart-suggestions-panel 
             node-id similar-nodes)
          (message "未找到相关的智能建议"))))))

;;;###autoload
(defun org-supertag-igr-rebuild-system ()
  "重建智能GraphRAG系统"
  (interactive)
  (when (y-or-n-p "确定要重建智能GraphRAG系统吗？这将清除所有缓存并重新向量化。")
    (message "正在重建智能GraphRAG系统...")
    (org-supertag-igr-cleanup-system)
    (org-supertag-igr-initialize-system)
    (message "智能GraphRAG系统重建完成")))

;;;###autoload
(defun org-supertag-igr-toggle ()
  "切换智能GraphRAG系统状态"
  (interactive)
  (if org-supertag-igr-enabled
      (org-supertag-igr-disable)
    (org-supertag-igr-enable)))

;;; =================================================================
;;; 系统状态和调试
;;; =================================================================

(defun org-supertag-igr-system-status ()
  "显示系统状态信息"
  (interactive)
  (let ((vector-count (hash-table-count org-supertag-igr-node-vectors))
        (cache-count (hash-table-count org-supertag-igr-similarity-cache)))
    
    (message "智能GraphRAG状态: %s | 向量: %d | 缓存: %d"
             (if org-supertag-igr-enabled "启用" "禁用")
             vector-count cache-count)))

(provide 'org-supertag-intelligent-graphrag)

;;; org-supertag-intelligent-graphrag.el ends here
