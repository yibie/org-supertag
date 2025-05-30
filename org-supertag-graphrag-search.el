;;; org-supertag-graphrag-search.el --- Graph-based search engine for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 基于org-supertag关系系统的图搜索引擎
;; 核心功能：
;; 1. 多跳图关系遍历
;; 2. 关系权重计算
;; 3. 结果排序和缓存
;; 4. 与现有backlink系统集成

;;; Code:

(require 'cl-lib)
(require 'org-supertag-db)
(require 'org-supertag-relation)
(require 'org-supertag-node)

;;; =================================================================
;;; 配置变量
;;; =================================================================

(defgroup org-supertag-graphrag nil
  "GraphRAG functionality for org-supertag."
  :group 'org-supertag)

(defcustom org-supertag-graphrag-max-hops 2
  "图遍历的最大跳数"
  :type 'integer
  :group 'org-supertag-graphrag)

(defcustom org-supertag-graphrag-max-results 20
  "返回结果的最大数量"
  :type 'integer
  :group 'org-supertag-graphrag)

(defcustom org-supertag-graphrag-relation-weights
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
  "不同关系类型的权重配置"
  :type '(alist :key-type symbol :value-type float)
  :group 'org-supertag-graphrag)

(defcustom org-supertag-graphrag-enable-cache t
  "是否启用图搜索缓存"
  :type 'boolean
  :group 'org-supertag-graphrag)

(defcustom org-supertag-graphrag-cache-ttl 300
  "缓存生存时间（秒）"
  :type 'integer
  :group 'org-supertag-graphrag)

;;; =================================================================
;;; 缓存系统
;;; =================================================================

(defvar org-supertag-graphrag--cache (make-hash-table :test 'equal)
  "图搜索结果缓存")

(defvar org-supertag-graphrag--cache-timestamps (make-hash-table :test 'equal)
  "缓存时间戳")

(defun org-supertag-graphrag--cache-key (node-id max-hops)
  "生成缓存键"
  (format "search:%s:%d" node-id max-hops))

(defun org-supertag-graphrag--get-cached-result (cache-key)
  "获取缓存结果"
  (when org-supertag-graphrag-enable-cache
    (let ((timestamp (gethash cache-key org-supertag-graphrag--cache-timestamps))
          (current-time (time-to-seconds (current-time))))
      (when (and timestamp 
                 (< (- current-time timestamp) org-supertag-graphrag-cache-ttl))
        (gethash cache-key org-supertag-graphrag--cache)))))

(defun org-supertag-graphrag--set-cached-result (cache-key result)
  "设置缓存结果"
  (when org-supertag-graphrag-enable-cache
    (puthash cache-key result org-supertag-graphrag--cache)
    (puthash cache-key (time-to-seconds (current-time)) 
             org-supertag-graphrag--cache-timestamps)))

(defun org-supertag-graphrag--clear-cache ()
  "清空缓存"
  (interactive)
  (clrhash org-supertag-graphrag--cache)
  (clrhash org-supertag-graphrag--cache-timestamps)
  (message "GraphRAG cache cleared"))

;;; =================================================================
;;; 核心搜索引擎
;;; =================================================================

(defun org-supertag-graphrag-search (node-id &optional max-hops)
  "基于图关系搜索相关节点
NODE-ID: 起始节点ID
MAX-HOPS: 最大跳数，默认为 `org-supertag-graphrag-max-hops`

返回格式：
((node-id . score) (node-id . score) ...)
按分数降序排列"
  (let* ((max-hops (or max-hops org-supertag-graphrag-max-hops))
         (cache-key (org-supertag-graphrag--cache-key node-id max-hops)))
    
    ;; 尝试从缓存获取
    (or (org-supertag-graphrag--get-cached-result cache-key)
        ;; 缓存未命中，执行搜索
        (let* ((node-tags (org-supertag-node-get-tags node-id))
               (related-nodes (make-hash-table :test 'equal))
               (visited-tags (make-hash-table :test 'equal)))
          
          ;; 多跳图遍历
          (dolist (tag node-tags)
            (org-supertag-graphrag--traverse-relations 
             tag related-nodes visited-tags 0 max-hops))
          
          ;; 排除起始节点
          (remhash node-id related-nodes)
          
          ;; 按分数排序并限制结果数量
          (let ((sorted-results (org-supertag-graphrag--rank-by-strength related-nodes)))
            ;; 缓存结果
            (org-supertag-graphrag--set-cached-result cache-key sorted-results)
            sorted-results)))))

(defun org-supertag-graphrag--traverse-relations (tag-id related-nodes visited-tags current-hop max-hops)
  "递归遍历图关系
TAG-ID: 当前标签ID
RELATED-NODES: 相关节点哈希表 (node-id -> score)
VISITED-TAGS: 已访问标签哈希表，避免循环
CURRENT-HOP: 当前跳数
MAX-HOPS: 最大跳数"
  (when (and (< current-hop max-hops)
             (not (gethash tag-id visited-tags)))
    
    ;; 标记为已访问
    (puthash tag-id t visited-tags)
    
    (let ((relations (org-supertag-relation-get-all tag-id)))
      (dolist (rel relations)
        (let* ((target-tag (plist-get rel :to))
               (rel-type (plist-get rel :type))
               (rel-strength (or (plist-get rel :strength) 1.0))
               (weight (org-supertag-graphrag--get-relation-weight rel-type))
               ;; 距离衰减：跳数越多，权重越低
               (distance-decay (/ 1.0 (1+ current-hop)))
               (final-score (* rel-strength weight distance-decay)))
          
          ;; 获取目标标签的所有节点
          (let ((target-nodes (org-supertag-tag-get-nodes target-tag)))
            (dolist (node-id target-nodes)
              (let ((current-score (gethash node-id related-nodes 0.0)))
                ;; 使用最大值策略合并分数
                (puthash node-id (max current-score final-score) related-nodes))))
          
          ;; 递归遍历（避免重复访问）
          (unless (gethash target-tag visited-tags)
            (org-supertag-graphrag--traverse-relations 
             target-tag related-nodes visited-tags (1+ current-hop) max-hops)))))))

(defun org-supertag-graphrag--get-relation-weight (rel-type)
  "获取关系类型的权重"
  (or (cdr (assq rel-type org-supertag-graphrag-relation-weights))
      0.1)) ; 默认权重

(defun org-supertag-graphrag--rank-by-strength (related-nodes)
  "按关系强度排序节点"
  (let ((node-list nil))
    (maphash (lambda (node-id score)
               (push (cons node-id score) node-list))
             related-nodes)
    
    ;; 按分数降序排序，限制结果数量
    (cl-subseq (sort node-list (lambda (a b) (> (cdr a) (cdr b))))
               0 (min (length node-list) org-supertag-graphrag-max-results))))

;;; =================================================================
;;; 路径发现和分析
;;; =================================================================

(defun org-supertag-graphrag-find-path (from-node-id to-node-id &optional max-hops)
  "找到两个节点之间的关系路径
FROM-NODE-ID: 起始节点
TO-NODE-ID: 目标节点
MAX-HOPS: 最大跳数

返回路径信息：
((:path (tag1 tag2 tag3) :relations (rel1 rel2) :strength 0.8) ...)"
  (let* ((max-hops (or max-hops org-supertag-graphrag-max-hops))
         (from-tags (org-supertag-node-get-tags from-node-id))
         (to-tags (org-supertag-node-get-tags to-node-id))
         (paths nil))
    
    ;; 对每个起始标签和目标标签组合寻找路径
    (dolist (from-tag from-tags)
      (dolist (to-tag to-tags)
        (let ((found-paths (org-supertag-graphrag--find-tag-paths 
                           from-tag to-tag max-hops)))
          (setq paths (append paths found-paths)))))
    
    ;; 按路径强度排序
    (sort paths (lambda (a b) (> (plist-get a :strength) 
                                (plist-get b :strength))))))

(defun org-supertag-graphrag--find-tag-paths (from-tag to-tag max-hops)
  "在两个标签之间寻找路径（BFS）"
  (let ((queue (list (list :path (list from-tag) :relations nil :strength 1.0)))
        (visited (make-hash-table :test 'equal))
        (found-paths nil))
    
    (while (and queue (< (length found-paths) 5)) ; 最多找5条路径
      (let* ((current (pop queue))
             (current-path (plist-get current :path))
             (current-relations (plist-get current :relations))
             (current-strength (plist-get current :strength))
             (current-tag (car (last current-path))))
        
        (unless (gethash current-tag visited)
          (puthash current-tag t visited)
          
          ;; 检查是否到达目标
          (when (equal current-tag to-tag)
            (push current found-paths))
          
          ;; 继续扩展路径
          (when (< (length current-path) (1+ max-hops))
            (let ((relations (org-supertag-relation-get-all current-tag)))
              (dolist (rel relations)
                (let* ((next-tag (plist-get rel :to))
                       (rel-type (plist-get rel :type))
                       (rel-strength (or (plist-get rel :strength) 1.0))
                       (weight (org-supertag-graphrag--get-relation-weight rel-type))
                       (new-strength (* current-strength rel-strength weight)))
                  
                  (unless (member next-tag current-path) ; 避免循环
                    (push (list :path (append current-path (list next-tag))
                               :relations (append current-relations (list rel-type))
                               :strength new-strength)
                          queue)))))))))
    
    found-paths))

;;; =================================================================
;;; 统计和分析功能
;;; =================================================================

(defun org-supertag-graphrag-analyze-node-centrality (node-id)
  "分析节点在图中的中心性
返回：(:degree-centrality N :betweenness-centrality N :closeness-centrality N)"
  (let* ((node-tags (org-supertag-node-get-tags node-id))
         (all-relations nil)
         (degree-count 0))
    
    ;; 计算度中心性（直接连接数）
    (dolist (tag node-tags)
      (let ((tag-relations (org-supertag-relation-get-all tag)))
        (setq degree-count (+ degree-count (length tag-relations)))
        (setq all-relations (append all-relations tag-relations))))
    
    ;; 简化的中心性计算
    (list :degree-centrality degree-count
          :relation-diversity (length (cl-remove-duplicates 
                                      (mapcar (lambda (r) (plist-get r :type)) 
                                             all-relations)))
          :connection-strength (if all-relations
                                  (/ (cl-reduce #'+ (mapcar (lambda (r) 
                                                             (or (plist-get r :strength) 1.0))
                                                           all-relations))
                                     (length all-relations))
                                0.0))))

(defun org-supertag-graphrag-find-bridge-nodes (&optional threshold)
  "找到图中的桥梁节点（连接不同概念簇的节点）
THRESHOLD: 桥梁强度阈值，默认为2"
  (let* ((threshold (or threshold 2))
         (all-nodes (org-supertag-db-find-by-type :node))
         (bridge-candidates nil))
    
    (dolist (node-id all-nodes)
      (let* ((centrality (org-supertag-graphrag-analyze-node-centrality node-id))
             (diversity (plist-get centrality :relation-diversity)))
        (when (>= diversity threshold)
          (push (cons node-id centrality) bridge-candidates))))
    
    ;; 按关系多样性排序
    (sort bridge-candidates 
          (lambda (a b) (> (plist-get (cdr a) :relation-diversity)
                          (plist-get (cdr b) :relation-diversity))))))

;;; =================================================================
;;; 调试和工具函数
;;; =================================================================

(defun org-supertag-graphrag-debug-search (node-id)
  "调试图搜索过程，显示详细信息"
  (interactive (list (org-id-get-create)))
  (let* ((node-tags (org-supertag-node-get-tags node-id))
         (results (org-supertag-graphrag-search node-id))
         (buffer (get-buffer-create "*GraphRAG Debug*")))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "=== GraphRAG Debug for Node: %s ===\n\n" node-id))
        
        ;; 显示节点标签
        (insert "Node Tags:\n")
        (dolist (tag node-tags)
          (insert (format "  - %s\n" tag)))
        (insert "\n")
        
        ;; 显示搜索结果
        (insert "Search Results:\n")
        (dolist (result results)
          (let* ((result-node-id (car result))
                 (score (cdr result))
                 (node-info (org-supertag-db-get result-node-id))
                 (title (plist-get node-info :title)))
            (insert (format "  %.3f - %s (%s)\n" score title result-node-id))))
        
        ;; 显示缓存状态
        (insert (format "\nCache Status: %s\n" 
                       (if org-supertag-graphrag-enable-cache "Enabled" "Disabled")))
        (insert (format "Cache Size: %d entries\n" 
                       (hash-table-count org-supertag-graphrag--cache))))
      
      (goto-char (point-min))
      (pop-to-buffer buffer))))

(defun org-supertag-graphrag-benchmark ()
  "性能基准测试"
  (interactive)
  (let* ((test-nodes (cl-subseq (org-supertag-db-find-by-type :node) 0 10))
         (start-time (current-time))
         (total-results 0))
    
    (dolist (node-id test-nodes)
      (let ((results (org-supertag-graphrag-search node-id)))
        (setq total-results (+ total-results (length results)))))
    
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "GraphRAG Benchmark: %d nodes, %d results, %.2f seconds (%.2f nodes/sec)"
               (length test-nodes) total-results elapsed 
               (/ (length test-nodes) elapsed)))))

;;; =================================================================
;;; 钩子和集成
;;; =================================================================

(defun org-supertag-graphrag--invalidate-cache-on-relation-change (from-tag to-tag)
  "当关系变化时，失效相关缓存"
  (when org-supertag-graphrag-enable-cache
    (let ((keys-to-remove nil))
      (maphash (lambda (key _)
                 (when (or (string-match-p (regexp-quote from-tag) key)
                          (string-match-p (regexp-quote to-tag) key))
                   (push key keys-to-remove)))
               org-supertag-graphrag--cache)
      
      (dolist (key keys-to-remove)
        (remhash key org-supertag-graphrag--cache)
        (remhash key org-supertag-graphrag--cache-timestamps)))))

;; 注册关系变化钩子
(add-hook 'org-supertag-relation-changed-hook 
          #'org-supertag-graphrag--invalidate-cache-on-relation-change)

(provide 'org-supertag-graphrag-search)

;;; org-supertag-graphrag-search.el ends here 