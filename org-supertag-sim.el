;;; org-supertag-sim.el --- Semantic similarity support for org-supertag -*- lexical-binding: t; -*-

;; Author: Your Name
;; Keywords: outlines, org-mode, tags
;; Package-Requires: ((emacs "28.1") (org-supertag "0.1"))

;;; Commentary:
;; Provides semantic 1similarity support for org-supertag.
;; Uses sim-tag.py as the backend to provide tag similarity calculation and recommendation functionality.

;;; Code:

(require 'json)
(require 'org-supertag-db)
(require 'org-supertag-sim-epc)  ; 引入EPC基础功能

(defgroup org-supertag-sim nil
  "Semantic similarity support for org-supertag."
  :group 'org-supertag)

(defcustom org-supertag-sim-vector-file
  (org-supertag-data-file "tag_vectors.json")
  "Tag vector storage file path."
  :type 'file
  :group 'org-supertag-sim)

(defcustom org-supertag-sim-sync-interval 3600
  "Vector library synchronization interval (seconds)."
  :type 'integer
  :group 'org-supertag-sim)

(defvar org-supertag-sim--initialized nil
  "Flag indicating whether the system has been initialized.")

(defvar org-supertag-sim--sync-timer nil
  "Timer for periodic synchronization.")

(defvar org-supertag-sim-tag-select-mode-map
  (let ((map (make-sparse-keymap)))
    ;; 基本导航
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "SPC") #'org-supertag-sim-toggle-tag-selection)
    (define-key map (kbd "RET") #'org-supertag-sim-toggle-tag-selection)
    (define-key map (kbd "C-c C-c") #'org-supertag-sim-apply-selected-tags)
    (define-key map (kbd "C-c C-k") #'org-supertag-sim-cancel-tag-selection)
    (define-key map (kbd "q") #'org-supertag-sim-cancel-tag-selection)
    (define-key map (kbd "a") #'org-supertag-sim-select-all-tags)
    (define-key map (kbd "A") #'org-supertag-sim-unselect-all-tags)
    map)
  "标签选择模式的键盘映射.")

(define-derived-mode org-supertag-sim-tag-select-mode special-mode "OrgSuperTag-TagSelect"
  "为自动标签建议提供标签选择的主模式."
  (setq-local buffer-read-only t))

(defun org-supertag-sim--get-all-tags ()
  "Get the names and IDs of all tags."
  (let ((tags '()))
    (dolist (tag (org-supertag-db-find-by-type :tag))
      (when-let* ((tag-props (org-supertag-db-get tag))
                  (tag-name (plist-get tag-props :name)))
        (push (cons tag tag-name) tags)))
    tags))

(defun org-supertag-sim--ensure-db-file ()
  "Ensure the database file exists and return its path and tag data."
  (unless (boundp 'org-supertag-db-file)
    (error "Database file path is not defined"))
  (let* ((db-file org-supertag-db-file)
         (tags (org-supertag-db-find-by-type :tag)))
    ;; Ensure the database file exists
    (unless (file-exists-p db-file)
      (org-supertag-db-save))
    
    (let ((tag-data
           (delq nil
                 (mapcar (lambda (tag)
                          (when-let* ((props (org-supertag-db-get tag)))
                            ;; Only keep tag ID and name
                            (list
                             (cons "id" tag)
                             (cons "name" (plist-get props :name)))))
                        tags))))
      (list db-file tag-data))))

(defun org-supertag-sim--ensure-vector-dir ()
  "Ensure the vector file directory exists."
  (let ((vector-dir (file-name-directory org-supertag-sim-vector-file)))
    (unless (file-exists-p vector-dir)
      (make-directory vector-dir t))
    vector-dir))

(defun org-supertag-sim-init ()
  "初始化标签相似度引擎，包括EPC服务器和Ollama服务.
作为统一的入口点，会初始化整个系统，包括底层EPC服务和相似度引擎。"
  (interactive)
  (message "正在初始化标签相似度系统...")
  
  ;; 首先初始化EPC服务和Ollama
  (condition-case err
      (progn
        (unless (org-supertag-sim-epc-server-running-p)
          (org-supertag-sim-epc-start-server)
          (sit-for 1)) ;; 给服务器启动一点时间
        
        (when (org-supertag-sim-epc-server-running-p)
          ;; 调用EPC服务初始化
          (org-supertag-sim-epc-init)
          
          ;; 检查EPC初始化是否成功
          (unless org-supertag-sim-epc-initialized
            (error "EPC服务初始化失败"))
          
          ;; 设置事件监听和同步定时器
          (org-supertag-db-on 'entity:created #'org-supertag-sim--on-tag-created)
          (org-supertag-db-on 'entity:removed #'org-supertag-sim--on-tag-removed)
          (org-supertag-sim--start-sync-timer)
          
          ;; 设置初始化完成标志
          (setq org-supertag-sim--initialized t)
          (message "标签相似度系统初始化完成")))
    (error
     (message "标签相似度系统初始化失败: %s" (error-message-string err))
     nil)))

(defun org-supertag-sim--update-tag (tag)
  "Update the vector of a single tag.
TAG is the tag information."
  (when org-supertag-sim--initialized
    (let ((tag-props (org-supertag-db-get tag)))
      (condition-case err
          (epc:call-deferred org-supertag-sim-epc-manager
                            'update_tag
                            (list (list (cons "id" tag)
                                      (cons "name" (plist-get tag-props :name)))))
        (error
         (message "更新标签向量出错: %s" (error-message-string err)))))))

(defun org-supertag-sim--remove-tag (tag-id)
  "Remove a tag from the vector library.
TAG-ID is the tag ID."
  (when org-supertag-sim--initialized
    (condition-case err
        (epc:call-deferred org-supertag-sim-epc-manager
                          'remove_tag
                          (list tag-id))
      (error
       (message "删除标签向量出错: %s" (error-message-string err))))))

(defun org-supertag-sim--on-tag-created (tag)
  "Handle tag creation events.
TAG is the created tag information."
  (when org-supertag-sim--initialized
    (org-supertag-sim--update-tag tag)))

(defun org-supertag-sim--on-tag-removed (tag-id)
  "Handle tag removal events.
TAG-ID is the removed tag ID."
  (when org-supertag-sim--initialized
    (org-supertag-sim--remove-tag tag-id)))

(defun org-supertag-sim--sync-library ()
  "Synchronize the tag vector library."
  (when org-supertag-sim--initialized
    (let* ((db-info (org-supertag-sim--ensure-db-file))
           (db-file (car db-info))
           (tag-data (cadr db-info)))
      (condition-case err
          (epc:call-deferred org-supertag-sim-epc-manager
                            'sync_library
                            (list db-file tag-data))
        (error
         (message "同步标签库出错: %s" (error-message-string err)))))))

(defun org-supertag-sim--start-sync-timer ()
  "Start the periodic synchronization timer."
  (when org-supertag-sim--sync-timer
    (cancel-timer org-supertag-sim--sync-timer))
  (setq org-supertag-sim--sync-timer
        (run-with-timer 
         org-supertag-sim-sync-interval
         org-supertag-sim-sync-interval
         #'org-supertag-sim--sync-library)))

(defun org-supertag-sim--ensure-initialized ()
  "确保系统已初始化，返回一个 deferred 对象.
如果系统未初始化，会自动尝试初始化。"
  (if (and org-supertag-sim--initialized org-supertag-sim-epc-initialized)
      ;; 如果两个系统都已初始化，直接返回成功
      (deferred:next (lambda () t))
    ;; 否则，尝试初始化
    (deferred:$
      (deferred:next
        (lambda ()
          (message "系统未初始化，正在初始化...")
          (org-supertag-sim-init)))
      (deferred:nextc it
        (lambda (_)
          ;; 再次检查初始化状态
          (if (and org-supertag-sim--initialized org-supertag-sim-epc-initialized)
              t
            (error "系统初始化失败")))))))

(defun org-supertag-sim-find-similar (tag-name &optional top-k callback)
  "Find tags similar to the specified tag.
TAG-NAME is the tag name.
TOP-K is the number of similar tags to return, default is 5.
CALLBACK is an optional callback function that receives the result as a parameter."
  (let ((buffer (current-buffer))
        (limit (or top-k 5)))
    (message "正在查找相似标签...")
    (deferred:$
      (deferred:try
        (deferred:$
          (org-supertag-sim--ensure-initialized)
          (deferred:nextc it
            (lambda (_)
              (epc:call-deferred org-supertag-sim-epc-manager
                                'find_similar
                                (list tag-name limit))))
          (deferred:nextc it
            (lambda (response)
              (let ((status (plist-get response :status))
                    (result (plist-get response :result)))
                (if (string= status "success")
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (let ((formatted-result
                               (mapcar (lambda (item)
                                       (cons (car item)
                                             (float (if (listp (cdr item))
                                                      (car (cdr item))
                                                    (cdr item)))))
                                     result)))
                          (if callback
                              (funcall callback formatted-result)
                            ;; 只在没有回调时显示结果
                            (message "找到 %d 个相似标签" (length formatted-result)))
                          formatted-result)))
                  (error "查找相似标签失败: %s" 
                         (or (plist-get response :message) "未知错误")))))))
        :catch
        (lambda (err)
          (message "查找相似标签出错: %s" (error-message-string err))
          nil)))))

(defun org-supertag-sim-search-tags (query-tags &optional weights callback)
  "Search for tag combinations.
QUERY-TAGS is the query tag list.
WEIGHTS is the weight list.
CALLBACK is an optional callback function that receives the result as a parameter."
  (org-supertag-sim--ensure-initialized)
  (let ((buffer (current-buffer)))
    (message "正在搜索标签组合...")
    (condition-case err
        (epc:call-deferred org-supertag-sim-epc-manager
                          'search_tags
                          (list query-tags weights))
      (deferred:nextc it
        (lambda (result)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (if callback
                  (funcall callback result)
                (message "找到 %d 个相关标签" (length result)))))))
      (error
       (message "搜索标签出错: %s" (error-message-string err))
       nil))))

(defun org-supertag-sim-extract-entities (text &optional callback)
  "Extract named entities from TEXT asynchronously.
Optional CALLBACK will be called with the results."
  (org-supertag-sim--ensure-initialized)
  (let ((buffer (current-buffer)))
    (message "正在分析实体...")
    (condition-case err
        (epc:call-deferred org-supertag-sim-epc-manager
                          'extract_entities
                          (list text))
      (deferred:nextc it
        (lambda (result)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (if callback
                  (funcall callback result)
                ;; 默认处理
                (if result
                    (progn
                      (message "找到 %d 个实体:" (length result))
                      (dolist (entity result)
                        (let ((entity-text (cdr (assoc 'entity entity)))
                              (type (cdr (assoc 'type entity)))
                              (start (cdr (assoc 'start entity)))
                              (end (cdr (assoc 'end entity))))
                          (message "  %s [%s] (%d-%d)" entity-text type start end))))
                  (message "未找到实体")))))))
      (error
       (message "提取实体出错: %s" (error-message-string err))
       nil))))

(defun org-supertag-sim-suggest-tags-from-text (text &optional callback)
  "根据文本内容生成标签建议.
可选的 CALLBACK 将在获取到标签后被调用."
  (let ((buffer (current-buffer)))
    (message "正在分析文本内容...")
    
    ;; 构建JSON格式的请求数据
    (let ((request-data (list :content text)))
      
      ;; 记录请求信息
      (message "发送文本长度: %d" (length text))
      
      (deferred:$
        (deferred:try
          (deferred:$
            ;; 首先确保系统已初始化
            (org-supertag-sim--ensure-initialized)
            (deferred:nextc it
              (lambda (_)
                ;; 确保文本不为空
                (if (string-empty-p text)
                    (error "文本内容为空")
                  ;; 使用JSON格式发送请求
                  (epc:call-deferred org-supertag-sim-epc-manager
                                  'suggest_tags_json
                                  (list (json-encode request-data))))))
            (deferred:nextc it
              (lambda (response)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (let ((status (plist-get response :status))
                          (result (plist-get response :result))
                          (error-msg (plist-get response :message)))
                      (cond
                       ;; 成功情况
                       ((string= status "success")
                        (if callback
                            (funcall callback result)
                          (message "找到 %d 个相关标签" (length result))))
                       ;; 错误情况
                       (t
                        (message "生成标签建议失败: %s" 
                                 (or error-msg "未知错误"))
                        (if callback (funcall callback nil)))))))))
          :catch
          (lambda (err)
            (message "生成标签建议出错: %s" (error-message-string err))
            (when callback (funcall callback nil)))))))))

(defun org-supertag-sim-toggle-tag-selection ()
  "切换当前标签的选择状态."
  (interactive)
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (when (looking-at "^\\([[:space:]]*\\)\\(\\[[ X]\\]\\) \\(.*\\)$")
      (let* ((tag-name (match-string-no-properties 3))
             (current-state (match-string-no-properties 2))
             (new-state (if (string= current-state "[X]") "[ ]" "[X]")))
        (replace-match (concat (match-string 1) new-state " " tag-name))
        (forward-line 1)))))

(defun org-supertag-sim-apply-selected-tags ()
  "应用所有已选择的标签."
  (interactive)
  (let* ((context (buffer-local-value 'org-supertag-sim--select-context (current-buffer)))
         (source-buffer (plist-get context :source-buffer))
         (node-id (plist-get context :node-id))
         (selected-tags '()))

    ;; 收集选中的标签
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:space:]]*\\)\\(\\[X\\]\\) \\(.*\\)$" nil t)
        (push (match-string-no-properties 3) selected-tags)))

    ;; 没有选择任何标签时提示
    (if (null selected-tags)
        (message "未选择任何标签")
      ;; 应用标签
      (message "正在应用 %d 个选中的标签..." (length selected-tags))
      (with-current-buffer source-buffer
        (dolist (tag selected-tags)
          (unless (org-supertag-tag-exists-p tag)
            (org-supertag-tag-create tag))
          (org-supertag-tag-apply tag))
        (message "成功应用 %d 个标签: %s" 
                 (length selected-tags)
                 (mapconcat (lambda (tag)
                              (propertize tag 'face 'font-lock-keyword-face))
                            selected-tags ", "))))
    
    ;; 关闭选择窗口
    (quit-window t)))

(defun org-supertag-sim-cancel-tag-selection ()
  "取消标签选择操作."
  (interactive)
  (message "已取消标签选择")
  (quit-window t))

(defun org-supertag-sim-select-all-tags ()
  "选择所有标签."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:space:]]*\\)\\(\\[[ X]\\]\\) \\(.*\\)$" nil t)
        (replace-match (concat (match-string 1) "[X] " (match-string 3))))
      (message "已选择所有标签"))))

(defun org-supertag-sim-unselect-all-tags ()
  "取消选择所有标签."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:space:]]*\\)\\(\\[[ X]\\]\\) \\(.*\\)$" nil t)
        (replace-match (concat (match-string 1) "[ ] " (match-string 3))))
      (message "已取消选择所有标签"))))

(defun org-supertag-sim-auto-tag-node ()
  "根据当前节点内容自动建议并应用标签.
使用语义分析从节点内容中提取相关标签,并在专用缓冲区中显示,
允许用户通过空格键选择需要应用的标签."
  (interactive)
  (let* ((content (org-get-entry))
         (node-id (org-id-get-create))
         (node-title (org-get-heading t t t t))
         (progress-reporter (make-progress-reporter "正在分析内容..." 0 100)))
    
    ;; 显示初始进度
    (progress-reporter-update progress-reporter 10)
    
    ;; 临时显示提示
    (run-with-timer 0.5 nil (lambda () 
                             (progress-reporter-update progress-reporter 30)
                             (message "正在生成标签建议...")))
    
    ;; 标签生成是异步的
    (org-supertag-sim-suggest-tags-from-text
     content
     (lambda (suggested-tags)
       ;; 更新进度
       (progress-reporter-update progress-reporter 80)
       (progress-reporter-done progress-reporter)
       
       (if suggested-tags
           ;; 在专用缓冲区中显示标签选择界面
           (let* ((select-buffer (get-buffer-create "*Org SuperTag Selection*"))
                  (source-buffer (current-buffer)))
             
             (with-current-buffer select-buffer
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (org-supertag-sim-tag-select-mode)
                 
                 ;; 存储上下文信息
                 (setq-local org-supertag-sim--select-context
                             (list :node-id node-id
                                   :node-title node-title
                                   :source-buffer source-buffer))
                 
                 ;; 添加标题和说明
                 (insert (format "为节点建议了 %d 个标签:\n" (length suggested-tags)))
                 (insert "──────────────────────────────────────────────\n")
                 (insert (format "节点: %s\n\n" node-title))
                 
                 ;; 插入标签列表
                 (dolist (tag suggested-tags)
                   (insert (format "[ ] %s\n" tag)))
                 
                 ;; 添加帮助信息
                 (insert "\n操作说明:\n")
                 (insert "空格/回车: 选择/取消选择标签  n/p: 上下移动\n")
                 (insert "a: 全选  A: 全不选\n")
                 (insert "C-c C-c: 应用选中标签  C-c C-k/q: 取消\n")))
             
             ;; 显示缓冲区
             (select-window 
              (display-buffer select-buffer
                              '((display-buffer-below-selected)
                                (window-height . fit-window-to-buffer)
                                (preserve-size . (nil . t))
                                (select . t))))
             
             ;; 移动到第一个标签
             (with-current-buffer select-buffer
               (goto-char (point-min))
               (re-search-forward "^\\[ \\]" nil t)
               (beginning-of-line)))
         (message "未找到合适的标签建议"))))))

(provide 'org-supertag-sim)
