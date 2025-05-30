;;; org-supertag-intelligent-graphrag-example.el --- Usage examples for Intelligent GraphRAG -*- lexical-binding: t; -*-

;;; Commentary:
;; 智能GraphRAG系统的使用示例和配置
;; 展示如何实现用户的三个核心需求：
;; 1. 联系各种关系
;; 2. 自动发现相似性
;; 3. 智能资源检索

;;; Code:

;;; =================================================================
;;; 基础配置示例
;;; =================================================================

;; 1. 加载必要的模块
(require 'org-supertag-intelligent-graphrag)
(require 'org-supertag-intelligent-graphrag-ui)


;; 2. 基础配置
(setq org-supertag-igr-enabled t                      ; 启用智能GraphRAG
      org-supertag-igr-auto-vectorize t               ; 自动向量化
      org-supertag-igr-similarity-threshold 0.65      ; 相似度阈值
      org-supertag-igr-max-suggestions 8              ; 最大建议数
      org-supertag-igr-auto-suggest-delay 1.5)        ; 建议延迟

;; 3. 多维度融合权重配置（根据您的需求调整）
(setq org-supertag-igr-fusion-weights
      '(:explicit-relations 0.35    ; 显性标签关系权重降低
        :content-similarity 0.4     ; 内容相似性权重提高（重点）
        :tag-cooccurrence 0.15      ; 标签共现
        :temporal-proximity 0.1))   ; 时间邻近性


;;; =================================================================
;;; 核心使用场景演示
;;; =================================================================

;;;###autoload
(defun org-supertag-igr-demo-setup ()
  "演示智能GraphRAG系统的设置"
  (interactive)
  
  ;; 1. 启用系统
  (org-supertag-igr-enable)
  
  ;; 2. 等待向量化完成
  (message "正在初始化智能GraphRAG系统...")
  (run-with-timer 3.0 nil))

;;;###autoload
(defun org-supertag-igr-demo-project-workflow ()
  "演示项目工作流：添加#project标签的自动资源发现"
  (interactive)
  
  (unless org-supertag-igr-enabled
    (org-supertag-igr-enable))
  
  (if (org-at-heading-p)
      (let ((node-id (org-id-get-create)))
        ;; 模拟添加project标签
        (message "模拟为当前节点添加 #project 标签...")
        
        ;; 创建演示标签（如果不存在）
        (let ((project-tag-id (or (org-supertag-tag-get-id-by-name "project")
                                  (org-supertag-tag-create "project"))))
          
          ;; 添加标签到节点
          (org-supertag-node-add-tag node-id project-tag-id)
          
          ;; 触发智能资源建议
          (run-with-timer 1.0 nil
                          (lambda ()
                            (org-supertag-igr-auto-suggest-project-resources 
                             node-id "project")))))
    (message "请先移动到一个org headline上")))

;;;###autoload
(defun org-supertag-igr-demo-similarity-discovery ()
  "演示相似性自动发现功能"
  (interactive)
  
  (unless org-supertag-igr-enabled
    (org-supertag-igr-enable))
  
  (if (org-at-heading-p)
      (let* ((node-id (org-id-get-create))
             (similar-nodes (org-supertag-igr-find-similar-nodes node-id 5)))
        
        (if similar-nodes
            (progn
              (message "为当前节点找到 %d 个相似节点：" (length similar-nodes))
              (dolist (item similar-nodes)
                (let* ((other-node-id (car item))
                       (score (cdr item))
                       (node (org-supertag-db-get other-node-id))
                       (title (plist-get node :title)))
                  (message "- %s (相似度: %.2f)" title score)))
              
              ;; 显示详细的建议面板
              (org-supertag-igr-show-smart-suggestions))
          (message "暂未发现相似节点，可能需要更多数据或降低相似度阈值")))
    (message "请先移动到一个org headline上")))

;;; =================================================================
;;; 高级配置示例
;;; =================================================================

(defun org-supertag-igr-advanced-setup ()
  "高级配置示例"
  (interactive)
  
  ;; 1. 针对不同内容类型的个性化配置
  (setq org-supertag-igr-fusion-weights
        (pcase (completing-read "选择配置模式: " 
                               '("平衡模式" "内容导向" "关系导向" "时间敏感") 
                               nil t)
          ("平衡模式" '(:explicit-relations 0.3 :content-similarity 0.3 
                      :tag-cooccurrence 0.2 :temporal-proximity 0.2))
          ("内容导向" '(:explicit-relations 0.2 :content-similarity 0.5 
                      :tag-cooccurrence 0.2 :temporal-proximity 0.1))
          ("关系导向" '(:explicit-relations 0.5 :content-similarity 0.2 
                      :tag-cooccurrence 0.2 :temporal-proximity 0.1))
          ("时间敏感" '(:explicit-relations 0.2 :content-similarity 0.3 
                      :tag-cooccurrence 0.2 :temporal-proximity 0.3))))
  
  ;; 2. 自定义资源分类规则
  (defun my-custom-resource-classifier (node)
    "自定义资源分类器"
    (let* ((title (plist-get node :title))
           (content (plist-get node :content))
           (text (format "%s %s" (or title "") (or content ""))))
      
      (cond
       ;; 会议相关
       ((string-match-p "\\(meeting\\|会议\\|讨论\\)" text) :meeting)
       ;; 决策相关
       ((string-match-p "\\(decision\\|决策\\|选择\\)" text) :decision)
       ;; 问题解决
       ((string-match-p "\\(problem\\|问题\\|bug\\|issue\\)" text) :problem)
       ;; 使用默认分类器
       (t (org-supertag-igr-classify-node-resource-type node)))))
  
  ;; 替换默认分类器
  (advice-add 'org-supertag-igr-classify-node-resource-type 
              :override #'my-custom-resource-classifier)
  
  (message "高级配置已应用"))

;;; =================================================================
;;; 工作流集成示例
;;; =================================================================

;;;###autoload
(defun org-supertag-igr-integrate-with-capture ()
  "与org-capture集成的示例"
  (interactive)
  
  ;; 在org-capture-after-finalize-hook中自动分析新创建的内容
  (add-hook 'org-capture-after-finalize-hook
            (lambda ()
              (when (and org-supertag-igr-enabled
                        (org-at-heading-p))
                (let ((node-id (org-id-get-create)))
                  ;; 延迟执行，确保内容已保存
                  (run-with-timer 2.0 nil
                                  (lambda ()
                                    (org-supertag-igr-vectorize-node node-id)
                                    (org-supertag-igr-show-smart-suggestions))))))))

;;;###autoload  
(defun org-supertag-igr-integrate-with-agenda ()
  "与org-agenda集成的示例"
  (interactive)
  
  ;; 在agenda模式下，为当前项目显示相关资源
  (defun org-supertag-igr-agenda-show-resources ()
    "在agenda中显示当前条目的相关资源"
    (interactive)
    (when (org-agenda-check-type nil 'agenda 'todo 'tags 'search)
      (org-agenda-switch-to)
      (when (org-at-heading-p)
        (let ((node-id (org-id-get-create)))
          (org-supertag-igr-show-smart-suggestions)))))
  
  ;; 绑定快捷键
  (define-key org-agenda-mode-map (kbd "C-c i") 
    #'org-supertag-igr-agenda-show-resources))

;;; =================================================================
;;; 批量分析和报告
;;; =================================================================

;;;###autoload
(defun org-supertag-igr-knowledge-network-analysis ()
  "知识网络分析报告"
  (interactive)
  
  (let* ((all-nodes (org-supertag-db-find-by-type :node))
         (total-nodes (length all-nodes))
         (vectorized-nodes 0)
         (similarity-stats (make-hash-table :test 'equal))
         (resource-stats (make-hash-table :test 'equal)))
    
    ;; 统计向量化节点
    (maphash (lambda (node-id _)
               (when (gethash node-id org-supertag-igr-node-vectors)
                 (setq vectorized-nodes (1+ vectorized-nodes))))
             org-supertag-db--object)
    
    ;; 统计资源分布
    (maphash (lambda (tag-name resources)
               (dolist (resource-pair resources)
                 (let ((resource-type (car resource-pair))
                       (count (length (cdr resource-pair))))
                   (puthash resource-type 
                           (+ count (gethash resource-type resource-stats 0))
                           resource-stats))))
             org-supertag-igr-resource-index)
    
    ;; 生成报告
    (let ((buffer (get-buffer-create "*智能GraphRAG网络分析*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "🧠 智能GraphRAG知识网络分析报告\n\n" 
                             'face '(:height 1.4 :weight bold)))
          
          ;; 基础统计
          (insert (propertize "📊 基础统计\n" 'face '(:weight bold :height 1.2)))
          (insert (format "总节点数: %d\n" total-nodes))
          (insert (format "已向量化节点: %d (%.1f%%)\n" 
                         vectorized-nodes 
                         (* 100.0 (/ vectorized-nodes (float total-nodes)))))
          (insert (format "缓存条目: %d\n" 
                         (hash-table-count org-supertag-igr-suggestion-cache)))
          (insert "\n")
          
          ;; 资源分布统计
          (insert (propertize "📚 资源分布统计\n" 'face '(:weight bold :height 1.2)))
          (maphash (lambda (resource-type count)
                     (insert (format "%s: %d个\n" resource-type count)))
                   resource-stats)
          (insert "\n")
          
          ;; 系统状态
          (insert (propertize "⚙️ 系统状态\n" 'face '(:weight bold :height 1.2)))
          (insert (format "智能GraphRAG: %s\n" 
                         (if org-supertag-igr-enabled "已启用" "已禁用")))
          (insert (format "自动向量化: %s\n" 
                         (if org-supertag-igr-auto-vectorize "已启用" "已禁用")))
          (insert (format "相似度阈值: %.2f\n" org-supertag-igr-similarity-threshold))
          (insert (format "最大建议数: %d\n" org-supertag-igr-max-suggestions))
          
          (goto-char (point-min)))
        (pop-to-buffer buffer)))))

;;; =================================================================
;;; 故障排除和调试
;;; =================================================================

;;;###autoload
(defun org-supertag-igr-diagnose-system ()
  "系统诊断工具"
  (interactive)
  
  (let ((issues nil)
        (suggestions nil))
    
    ;; 检查EPC服务
    (unless (and (boundp 'org-supertag-sim-epc-manager)
                org-supertag-sim-epc-manager)
      (push "EPC服务未启动，向量化功能将不可用" issues)
      (push "运行 (org-supertag-sim-epc-start-server) 启动EPC服务" suggestions))
    
    ;; 检查数据
    (when (= (length (org-supertag-db-find-by-type :node)) 0)
      (push "没有找到任何节点数据" issues)
      (push "确保org-supertag数据库中有节点数据" suggestions))
    
    ;; 检查向量化进度
    (let ((total-nodes (length (org-supertag-db-find-by-type :node)))
          (vectorized-count (hash-table-count org-supertag-igr-node-vectors)))
      (when (and (> total-nodes 0) (= vectorized-count 0))
        (push "没有节点被向量化" issues)
        (push "运行 (org-supertag-igr-batch-vectorize-existing-nodes) 进行批量向量化" suggestions)))
    
    ;; 显示诊断结果
    (if issues
        (progn
          (message "发现以下问题：")
          (dolist (issue issues)
            (message "- %s" issue))
          (message "\n建议解决方案：")
          (dolist (suggestion suggestions)
            (message "- %s" suggestion)))
      (message "系统运行正常！"))))

;;; =================================================================
;;; 使用指南
;;; =================================================================

;;;###autoload
(defun org-supertag-igr-show-usage-guide ()
  "显示使用指南"
  (interactive)
  
  (let ((buffer (get-buffer-create "*智能GraphRAG使用指南*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# 智能GraphRAG使用指南\n\n")
        
        (insert "## 🚀 快速开始\n\n")
        (insert "1. 启用系统：\n")
        (insert "   M-x org-supertag-igr-enable\n")
        (insert "   或使用配置：(org-supertag-igr-demo-setup)\n\n")
        
        (insert "2. 基本使用：\n")
        (insert "   - 移动到任意headline，等待2秒看智能建议\n")
        (insert "   - 添加标签（如#project）会自动触发资源建议\n")
        (insert "   - 使用 C-c i s 手动搜索标签资源\n\n")
        
        (insert "## 🎯 核心功能\n\n")
        (insert "### 1. 多维度相似性发现\n")
        (insert "结合以下四个维度自动发现相关内容：\n")
        (insert "- 显性标签关系（用户定义的关系）\n")
        (insert "- 内容向量相似度（基于语义理解）\n")
        (insert "- 标签共现模式\n")
        (insert "- 时间邻近性\n\n")
        
        (insert "### 2. 智能资源分类\n")
        (insert "自动将内容分类为：\n")
        (insert "- 📊 项目相关\n")
        (insert "- 💭 想法笔记\n")
        (insert "- 🔧 工具资源\n")
        (insert "- 👥 人员联系\n")
        (insert "- 📚 学习资源\n\n")
        
        (insert "### 3. 自动触发场景\n")
        (insert "系统会在以下情况自动提供建议：\n")
        (insert "- 光标移动到新的headline\n")
        (insert "- 为节点添加新标签\n")
        (insert "- 创建新的org-capture条目\n\n")
        
        (insert "## ⚙️ 自定义配置\n\n")
        (insert "### 融合权重调整\n")
        (insert "```elisp\n")
        (insert "(setq org-supertag-igr-fusion-weights\n")
        (insert "      '(:explicit-relations 0.4    ; 显性关系权重\n")
        (insert "        :content-similarity 0.3    ; 内容相似性权重\n")
        (insert "        :tag-cooccurrence 0.2      ; 标签共现权重\n")
        (insert "        :temporal-proximity 0.1))  ; 时间邻近权重\n")
        (insert "```\n\n")
        
        (insert "### 相似度阈值调整\n")
        (insert "```elisp\n")
        (insert "(setq org-supertag-igr-similarity-threshold 0.7)  ; 提高精确度\n")
        (insert "(setq org-supertag-igr-similarity-threshold 0.5)  ; 提高召回率\n")
        (insert "```\n\n")
        
        (insert "## 🔧 维护操作\n\n")
        (insert "- 清空缓存：M-x org-supertag-igr-clear-caches\n")
        (insert "- 重新向量化：M-x org-supertag-igr-batch-vectorize-existing-nodes\n")
        (insert "- 系统诊断：M-x org-supertag-igr-diagnose-system\n")
        (insert "- 网络分析：M-x org-supertag-igr-knowledge-network-analysis\n\n")
        
        (insert "## 🎯 实际使用技巧\n\n")
        (insert "1. **项目管理场景**：\n")
        (insert "   添加#project标签后，系统会自动找到相关的笔记、工具、人员\n\n")
        
        (insert "2. **学习笔记场景**：\n")
        (insert "   基于内容相似度自动发现相关概念和资料\n\n")
        
        (insert "3. **知识发现场景**：\n")
        (insert "   通过多维度融合发现意想不到的知识连接\n\n")
        
        (org-mode)
        (goto-char (point-min)))
      (pop-to-buffer buffer))))

(provide 'org-supertag-intelligent-graphrag-example)

;;; org-supertag-intelligent-graphrag-example.el ends here 