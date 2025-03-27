;;; org-supertag-sim.el --- Semantic similarity support for org-supertag -*- lexical-binding: t; -*-

;; Author: Your Name
;; Keywords: outlines, org-mode, tags
;; Package-Requires: ((emacs "28.1") (org-supertag "0.1"))

;;; Commentary:
;; Provides semantic similarity support for org-supertag.
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
  "Initialize the tag similarity engine."
  (interactive)
  (org-supertag-db-on 'entity:created #'org-supertag-sim--on-tag-created)
  (org-supertag-db-on 'entity:removed #'org-supertag-sim--on-tag-removed)
  (org-supertag-sim--start-sync-timer)
  (setq org-supertag-sim--initialized t))

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
  "确保系统已初始化，返回一个 deferred 对象."
  (if org-supertag-sim-epc-initialized
      (deferred:next (lambda () t))  ; 如果已初始化，返回成功
    (deferred:$
      (deferred:next
        (lambda ()
          (org-supertag-sim-epc-ensure-server)))
      (deferred:nextc it
        (lambda (_)
          (org-supertag-sim-epc-init))))))

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
    (deferred:$
      (deferred:next
        (lambda ()
          (message "发送文本长度: %d" (length text))
          (message "发送文本预览: %s" (substring text 0 (min 100 (length text))))
          (epc:call-deferred org-supertag-sim-epc-manager
                            'extract_entities
                            (list text))))
      (deferred:nextc it
        (lambda (response)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((status (plist-get response :status))
                    (result (plist-get response :result))
                    (error-msg (plist-get response :message)))
                ;; 记录完整响应
                (message "收到响应: status=%s, result=%s, error=%s"
                         status result error-msg)
                (cond
                 ;; 成功且结果有效
                 ((and (string= status "success")
                       result)
                  (if callback
                      (funcall callback result)
                    ;; 默认处理
                    (message "找到 %d 个实体:" (length result))
                    (dolist (entity result)
                      (let ((text (cdr (assoc 'text entity)))
                            (type (cdr (assoc 'type entity))))
                        (message "  %s [%s]" text type)))))
                 ;; 成功但结果为空
                 ((string= status "success")
                  (message "未找到实体")
                  (if callback (funcall callback nil)))
                 ;; 错误情况
                 (t
                  (message "提取实体失败: %s" 
                           (or error-msg "未知错误"))
                  (if callback (funcall callback nil)))))))))
      (deferred:error it
        (lambda (err)
          (message "提取实体出错: %s" (error-message-string err))
          (when callback (funcall callback nil))
          nil)))))

(defun org-supertag-sim--extract-node-content ()
  "提取当前节点的标题和内容，去除属性抽屉等无关内容."
  (let ((node-title (org-get-heading t t t t))
        (raw-content (org-get-entry)))
    (with-temp-buffer
      (insert raw-content)
      (goto-char (point-min))
      ;; 跳过属性抽屉
      (when (re-search-forward ":END:\n+" nil t)
        (delete-region (point-min) (point)))
      ;; 移除空行和多余空白
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\n" nil t)
        (replace-match ""))
      ;; 组合标题和处理后的内容
      (let ((content (string-trim (buffer-string))))
        (if (string-empty-p content)
            node-title  ; 如果内容为空，只返回标题
          (concat node-title "\n\n" content))))))

(defun org-supertag-sim-suggest-tags-from-text (text &optional callback)
  "根据文本内容生成标签建议.
可选的 CALLBACK 将在获取到标签后被调用."
  (let ((buffer (current-buffer)))
    (message "正在分析文本内容...")
    (deferred:$
      (deferred:next
        (lambda ()
          ;; 确保文本不为空
          (if (string-empty-p text)
              (error "文本内容为空")
            ;; 记录发送的文本内容
            (message "发送文本长度: %d" (length text))
            (message "发送文本预览: %s" (substring text 0 (min 100 (length text))))
            (epc:call-deferred org-supertag-sim-epc-manager
                              'suggest_tags
                              (list text)))))
      (deferred:nextc it
        (lambda (response)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((status (plist-get response :status))
                    (result (plist-get response :result))
                    (error-msg (plist-get response :message)))
                ;; 记录完整响应
                (message "收到响应: status=%s, result=%s, error=%s"
                         status result error-msg)
                (cond
                 ;; 成功且结果有效
                 ((and (string= status "success")
                       (listp result)
                       (> (length result) 0))
                  (if callback
                      (funcall callback result)
                    (message "找到 %d 个相关标签: %s" 
                             (length result)
                             (mapconcat #'identity result ", "))))
                 ;; 成功但结果为空
                 ((string= status "success")
                  (message "未找到合适的标签建议")
                  (if callback (funcall callback nil)))
                 ;; 错误情况
                 (t
                  (message "生成标签建议失败: %s" 
                           (or error-msg "未知错误"))
                  (if callback (funcall callback nil))))))))))))

(defun org-supertag-sim--apply-selected-tags (tags)
  "应用用户选择的标签到当前节点."
  (when tags
    (message "正在应用标签...")
    (dolist (tag tags)
      (unless (org-supertag-tag-exists-p tag)
        (org-supertag-tag-create tag))
      (org-supertag-tag-apply tag))
    (message "成功应用%d个标签" (length tags))))

(defun org-supertag-sim-auto-tag-node ()
  "从当前节点内容中提取标签。"
  (interactive)
  (let* ((content (org-supertag-sim--extract-node-content))
         (progress-reporter (make-progress-reporter "正在处理内容..." 0 100)))
    
    ;; 如果内容为空，提示用户
    (if (string-empty-p (string-trim content))
        (message "节点内容为空，无法提取标签")
      
      ;; 显示初始进度
      (progress-reporter-update progress-reporter 20)
      (message "正在从内容中提取标签...内容长度: %d" (length content))
      
      ;; 使用实体提取方法获取潜在标签
      (org-supertag-sim-extract-entities
       content
       (lambda (entities)
         ;; 更新进度
         (progress-reporter-update progress-reporter 80)
         (progress-reporter-done progress-reporter)
         
         ;; 检查是否有有效的实体
         (if (and entities (> (length entities) 0))
             ;; 从实体中提取标签
             (let* ((tags (mapcar (lambda (entity)
                                   (let ((text (cdr (assoc 'text entity)))
                                         (type (cdr (assoc 'type entity))))
                                     ;; 将实体文本转换为标签格式
                                     (cons (downcase
                                            (replace-regexp-in-string
                                             "\\s-+" "_"
                                             (replace-regexp-in-string
                                              "[^[:alnum:][:space:]]" ""
                                              text)))
                                           (format "[%s]" type))))
                                 entities))
                    ;; 去除重复并保留标签信息
                    (unique-tags-with-info (let ((seen (make-hash-table :test 'equal))
                                               result)
                                           (dolist (tag-info tags (nreverse result))
                                             (let ((tag (car tag-info))
                                                   (info (cdr tag-info)))
                                               (unless (gethash tag seen)
                                                 (puthash tag info seen)
                                                 (push (cons tag info) result))))))
                    (formatted-tags (mapcar (lambda (tag-info)
                                             (format "%s %s" (car tag-info) (cdr tag-info)))
                                           unique-tags-with-info))
                    (tag-names (mapcar #'car unique-tags-with-info))
                    (prompt (format "从节点内容中提取了 %d 个标签，请选择要应用的标签: " 
                                   (length unique-tags-with-info)))
                    (selected-tags
                     (completing-read-multiple prompt formatted-tags nil t))
                    (clean-selected-tags 
                     (mapcar (lambda (selected)
                               (car (split-string selected " \\[")))
                            selected-tags)))
               
               ;; 应用已选标签
               (org-supertag-sim--apply-selected-tags clean-selected-tags))
           
           ;; 对于短文本，尝试直接使用内容作为标签源
           (let* ((words (split-string (string-trim content) nil t))
                  (candidates 
                   (if (< (length words) 10)
                       ;; 对于短文本（少于10个词），将所有非停用词作为候选
                       (seq-filter 
                        (lambda (word) 
                          (> (length word) 3))  ;; 简单过滤策略：长度>3
                        words)
                     ;; 否则使用空列表
                     '()))
                  (unique-candidates (delete-dups 
                                     (mapcar 
                                      (lambda (word)
                                        (downcase
                                         (replace-regexp-in-string
                                          "[^[:alnum:][:space:]]" ""
                                          word)))
                                      candidates))))
             
             (if (> (length unique-candidates) 0)
                 (let* ((prompt (format "未找到结构化实体，但从文本中提取了 %d 个可能的标签: " 
                                      (length unique-candidates)))
                        (selected-tags
                         (completing-read-multiple prompt unique-candidates nil t)))
                   ;; 应用已选标签
                   (org-supertag-sim--apply-selected-tags selected-tags))
               (message "未能从内容中提取到标签，文本可能太短或没有明显实体")))))))))

(provide 'org-supertag-sim)
