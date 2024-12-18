;;; org-supertag-query.el --Query for org-supertag -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-supertag-db)  
(require 'org-supertag-field) 

(defun org-supertag-find-matching-tags (keywords)
  "查找匹配关键字的标签.
KEYWORDS 是关键字列表"
  (let ((all-tags (org-supertag-db-find-by-props '(:type :tag)))
        matching-tags)
    (dolist (tag-id (mapcar #'car all-tags))
      (when (cl-some (lambda (keyword)
                      (string-match-p keyword tag-id))
                    keywords)
        (push tag-id matching-tags)))
    matching-tags))

(defun org-supertag-find-matching-nodes (keywords)
  "查找匹配关键字的节点.
KEYWORDS 是关键字列表"
  (mapcar #'car  ; 只返回节点 ID
          (org-supertag-db-find-by-props 
           '(:type :node)  ; 查找所有节点类型
           (lambda (props)  ; 标题匹配检查
             (let ((title (plist-get props :title)))
               (and title
                    (cl-some 
                     (lambda (keyword)
                       (string-match-p 
                        (regexp-quote keyword) 
                        title))
                     keywords)))))))


(defun org-supertag-find-matching-fields (keywords)
  "查找匹配关键字的字段.
KEYWORDS 是关键字列表
返回格式: ((tag-id field-name node-id value) ...)"
  (let (results)
    (dolist (link (org-supertag-db-find-links :node-field nil nil))
      (let ((field-name (plist-get link :field-name))
            (value (plist-get link :value)))
        (when (and field-name value
                  (cl-some 
                   (lambda (keyword)
                     (or (string-match-p keyword field-name)
                         (string-match-p keyword value)))
                   keywords))
          (push (list (plist-get link :to)      ; tag-id
                     field-name
                     (plist-get link :from)      ; node-id 
                     value)
                results))))
    (nreverse results)))

(defun org-supertag-query--get-node-tags (node-id)
  "获取节点的所有标签，用于查询显示.
NODE-ID 是节点ID

返回值：
- 标签列表
- nil 如果节点不存在或没有标签"
  (when-let* ((props (org-supertag-db-get node-id))
              (file (plist-get props :file-path))
              (pos (plist-get props :pos)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)  ; 启用 org-mode
        (goto-char pos)
        (org-get-tags)))))



(defun org-supertag-node-has-children-p (node-id)
  "检查节点是否有子节点.
NODE-ID 是节点ID"
  (when-let* ((props (org-supertag-db-get node-id))
              (file (plist-get props :file-path))
              (pos (plist-get props :pos)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)  ; 启用 org-mode
        (goto-char pos)
        (let ((level (org-current-level)))
          (forward-line)
          (and (re-search-forward org-heading-regexp nil t)
               (> (org-current-level) level)))))))

(defun org-supertag-query-find-by-tag (tag-name)
  "通过标签查找节点.
TAG-NAME 是标签名称"
  (let ((nodes '()))
    (maphash
     (lambda (id props)
       (when (and (eq (plist-get props :type) :node)
                 (with-temp-buffer
                   (when-let* ((file (plist-get props :file-path))
                              (pos (plist-get props :pos)))
                     (when (file-exists-p file)
                       (insert-file-contents file)
                       (goto-char pos)
                       (let ((tags (org-get-tags)))
                         (and tags (member tag-name tags)))))))
         (push props nodes)))
     org-supertag-db--object)
    nodes))

(defun org-supertag-query-format-node (node)
  "格式化节点显示.
NODE 是节点属性列表"
  (let* ((id (plist-get node :id))
         (title (plist-get node :title))
         (file (plist-get node :file-path))  ; 从 node 中获取，而不是 props
         (has-children (org-supertag-node-has-children-p id))
         (tags (org-supertag-query--get-node-tags id))
         (formatted-tags (when tags
                          (concat " :" (mapconcat #'identity tags ":") ":")))
         (children-indicator (if has-children " [+]" ""))
         (file-info (format " (%s)" (file-name-nondirectory file))))
    (format "- [ ] %s%s%s%s [[id:%s]]"
            title
            children-indicator
            (or formatted-tags "")
            file-info
            id)))

;;---------------------------------------------------------------
;; Query Results Page
;;---------------------------------------------------------------

(defun org-supertag-query-find-nodes (keywords)
  "查找匹配关键字的节点."
  (let (results)
    (maphash
     (lambda (id props)
       (when (eq (plist-get props :type) :node)
         (when-let* ((file (plist-get props :file-path))
                    (pos (plist-get props :pos)))
           (when (file-exists-p file)
             (with-temp-buffer
               (insert-file-contents file)
               (org-mode)  ; 启用 org-mode
               (goto-char pos)
               ;; 获取节点的所有可搜索内容
               (let* ((title (plist-get props :title))
                      (tags (org-get-tags))
                      (fields (org-entry-properties nil 'standard))
                      (field-values (mapcar #'cdr fields))
                      ;; 合并所有可搜索的文本
                      (searchable-text (concat 
                                      title " "
                                      (mapconcat #'identity (or tags '()) " ")
                                      " "
                                      (mapconcat #'identity field-values " "))))
                 ;; 检查是否所有关键字都匹配
                 (when (cl-every 
                        (lambda (keyword)
                          (string-match-p 
                           (regexp-quote keyword) 
                           searchable-text))
                        keywords)
                   (push props results))))))))
     org-supertag-db--object)
    ;; 返回结果
    (nreverse results)))

(defvar org-supertag-query-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x f") #'org-supertag-query-export-results-to-file)
    (define-key map (kbd "C-c C-x h") #'org-supertag-query-export-results-here)
    (define-key map (kbd "C-c C-x n") #'org-supertag-query-export-results-to-new-file)
    (define-key map (kbd "C-c C-x C-r") #'org-supertag-query-toggle-checkbox-region)
    (define-key map (kbd "C-c C-x C-u") #'org-supertag-query-untoggle-checkbox-region)
    (define-key map (kbd "C-c C-c") #'org-supertag-query-toggle-checkbox)  ; 添加切换复选框的快捷键
    map)
  "Keymap for `org-supertag-query-mode'.")

;; User Query Command
(define-minor-mode org-supertag-query-mode
  "Minor mode for org-supertag search results buffer."
  :lighter " OrgST"
  :keymap org-supertag-query-mode-map)

(defun org-supertag-query-show-results (keyword-list)
  "显示搜索结果.
KEYWORD-LIST 是关键字列表"
  (with-current-buffer (get-buffer-create "*Org SuperTag Search*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-mode)
      (org-supertag-query-mode)
      
      ;; 显示搜索信息
      (insert "#+TITLE: SuperTag Search Results\n\n")
      (insert (format "* 搜索条件: %s\n" 
                     (mapconcat #'identity keyword-list " ")))
      (org-show-all)
      
      ;; 显示操作说明
      (insert "* 操作说明\n")
      (insert "- 搜索范围：标题、标签、属性和字段值\n")
      (insert "- 多个关键字之间是 AND 关系（都需要匹配）\n")
      (insert "- [+] 表示该节点包含子节点\n\n")
      (insert "快捷键：\n")
      (insert "- C-c C-c   : 切换当前行的复选框状态\n")
      (insert "- C-c C-e f : 导出到指定文件\n")
      (insert "- C-c C-e h : 导出到当前位置\n")
      (insert "- C-c C-e n : 导出到新文件\n")
      (insert "- C-c C-i   : 插入到原始位置\n")
      (insert "\n")
      
      ;; 显示搜索结果
      (insert "* Search Results\n")
      (let ((nodes (org-supertag-query-find-nodes keyword-list)))
        (if nodes
            (progn
              (insert (format "找到 %d 个匹配的节点：\n\n" (length nodes)))
              (dolist (node nodes)
                (insert (org-supertag-query-format-node node) "\n")))
          (insert "没有找到匹配的结果\n")))
      
      (goto-char (point-min))))
  (switch-to-buffer "*Org SuperTag Search*"))

;;---------------------------------------------------------------------
;; Select Serached Item 
;;---------------------------------------------------------------------

(defun org-supertag-query-toggle-checkbox ()
  "切换当前行的复选框状态."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^- \\(\\[[ X]\\]\\)")
        (let ((current-state (if (string= (match-string 1) "[X]")
                                "[ ]"
                              "[X]")))
          (replace-match (concat "- " current-state)))))))

(defun org-supertag-query-toggle-checkbox-region (start end)
  "切换区域内所有复选框的状态.
START 和 END 定义了区域范围."
  (interactive "r")  ; 自动获取当前选中区域
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward "^- \\[[ X]\\]" end t))
        (replace-match "- [X]")))))

(defun org-supertag-query-untoggle-checkbox-region (start end)
  "取消区域内所有复选框的选中状态."
  (interactive "r")
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward "^- \\[X\\]" end t))
        (replace-match "- [ ]")))))

(defun org-supertag-toggle-all-boxes ()
  "全选或取消全选搜索结果."
  (interactive)
  (let* ((inhibit-read-only t)
         (current-state (save-excursion
                         (goto-char (point-min))
                         (re-search-forward "^- \\[[ X]\\]" nil t)
                         (match-string 0)))
         (new-state (if (string-match-p "X" current-state) " " "X")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^- \\[[ X]\\]" nil t)
        (replace-match (format "- [%s]" new-state))))))



;;----------------------------------------------------------------------
;; Export Seleted Results from Query Buffer
;;----------------------------------------------------------------------

(defun org-supertag-get-selected-nodes ()
  "从搜索结果中获取所有被选中的节点 ID.

返回值：
- 选中节点的 ID 列表
- nil 如果没有选中的节点"
  (let (selected-nodes)
    (save-excursion
      (goto-char (point-min))
      ;; 搜索所有选中的条目
      (while (re-search-forward 
              "^-[ \t]+\\[X\\].*?\\[\\[id:\\([^]]+\\)\\]\\]" 
              nil t)
        (when-let ((node-id (match-string-no-properties 1)))
          ;; 验证节点是否存在
          (when (org-supertag-node-db-exists-p node-id)
            (push node-id selected-nodes)))))
    ;; 返回结果
    (when selected-nodes
      (message "找到 %d 个选中的节点" (length selected-nodes))
      (nreverse selected-nodes))))
      
;; org-supertag-query-insert-node 
(defun org-supertag-get-target-level (level-adjust)
  "计算目标层级.
LEVEL-ADJUST 可以是:
  nil        - 保持原有层级
  :child     - 作为当前标题的子标题
  :same-level - 作为当前标题的同级标题

返回值：
- 计算出的目标层级
- nil 如果不需要调整层级"
  (let ((current-level (when (org-at-heading-p)
                        (org-current-level))))
    (cond
     ((eq level-adjust :child)
      (if current-level
          (1+ current-level)
        1))
     ((eq level-adjust :same-level)
      (or current-level 1))
     (t nil))))

(defun org-supertag-adjust-node-level (content target-level)
  "调整节点内容的层级.
CONTENT 是节点内容
TARGET-LEVEL 是目标层级

返回值：
- 调整后的内容"
  (with-temp-buffer
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (when target-level
      (let* ((source-level (org-current-level))
             (level-diff (- target-level source-level)))
        (unless (zerop level-diff)
          (org-map-entries
           (lambda ()
             (let* ((current-level (org-current-level))
                    (new-level (+ current-level level-diff))
                    (new-stars (make-string (max 1 new-level) ?*)))
               (replace-match new-stars t t nil 1)))
           t nil))))
    (buffer-string)))

(defun org-supertag-delete-node-content (node-id)
  "从源文件中删除节点内容.
NODE-ID 是节点ID

返回值：
- t 删除成功
- nil 如果节点不存在或删除失败"
  (when-let* ((node (org-supertag-db-get node-id))
              (source-file (plist-get node :file-path))
              (loc (org-supertag-find-node-location node-id source-file)))
    (with-current-buffer (find-file-noselect source-file)
      (org-with-wide-buffer
       (goto-char (car loc))
       (when (org-at-heading-p)
         (let* ((element (org-element-at-point))
                (begin (org-element-property :begin element))
                (end (org-element-property :end element)))
           (delete-region begin end)
           (when (looking-at "\n") (delete-char 1))
           (save-buffer)
           t))))))

(defun org-supertag-find-node-location (node-id file)
  "通过 ID 在文件中定位节点.
NODE-ID 是节点ID
FILE 是文件路径

返回值：
- (point level olp) 找到节点时返回位置信息
- nil 未找到节点"
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((found nil))
         (org-map-entries
          (lambda ()
            (when (equal (org-id-get) node-id)
              (setq found (list (point)
                              (org-current-level)
                              (org-get-outline-path)))))
          t nil)
         found)))))

(defun org-supertag-get-node-content (node-id)
  "获取节点的完整内容.
NODE-ID 是节点ID

返回值：
- 节点内容字符串
- nil 如果未找到节点"
  (when-let* ((node (org-supertag-db-get node-id))
              (file (plist-get node :file-path))
              (loc (org-supertag-find-node-location node-id file)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (car loc))
       (let ((element (org-element-at-point)))
         (buffer-substring-no-properties
          (org-element-property :begin element)
          (org-element-property :end element)))))))


(defun org-supertag-update-node-db (node-id file)
  "更新节点在数据库中的信息.
NODE-ID 是节点ID
FILE 是目标文件路径"
  (unless (org-supertag-ensure-org-file file)
    (error "Invalid org file: %s" file))
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      ;; 确保在正确的位置
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      ;; 验证我们找到的是正确的节点
      (let ((current-id (org-id-get)))
        (unless (equal current-id node-id)
          (error "Current heading ID (%s) doesn't match expected ID (%s)"
                 current-id node-id)))
      ;; 收集节点信息
      (let* ((pos (point))
             (level (org-current-level))
             (title (org-get-heading t t t t))
             (olp (org-get-outline-path))
             ;; 获取现有节点信息以保留某些属性
             (existing-node (org-supertag-db-get node-id))
             (created-at (and existing-node 
                             (plist-get existing-node :created-at))))
        ;; 构建新的属性列表
        (let ((new-props
               (list :type :node
                     :title title
                     :file-path file
                     :pos pos
                     :level level
                     :olp olp)))
          ;; 如果有创建时间，保留它
          (when created-at
            (setq new-props (plist-put new-props :created-at created-at)))
          ;; 更新数据库
          (condition-case err
              (progn
                (org-supertag-node-db-update node-id new-props)
                (message "Updated node %s in database" node-id))
            (error
             (message "Failed to update node %s: %s" 
                      node-id 
                      (error-message-string err))
             (signal (car err) (cdr err)))))))))

(defun org-supertag-move-node (node-id target-file &optional target-level)
  "将节点移动到目标文件.
NODE-ID 是节点ID
TARGET-FILE 是目标文件路径
TARGET-LEVEL 是目标层级

返回值：
- t 移动成功
- nil 如果移动失败"
  (when-let* ((content (org-supertag-get-node-content node-id)))
    ;; 1. 删除源文件中的节点
    (when (org-supertag-delete-node-content node-id)
      ;; 2. 插入到目标文件并更新数据库
      (with-current-buffer (find-file-noselect target-file)
        (save-excursion
          (let ((adjusted-content 
                 (org-supertag-adjust-node-level content target-level)))
            (insert adjusted-content "\n")
            (forward-line -1)
            (org-supertag-update-node-db node-id target-file)
            t))))))


(defun org-supertag-insert-nodes (node-ids &optional level-adjust)
  "移动节点到当前位置.
NODE-IDS 是节点 ID 列表
LEVEL-ADJUST 是层级调整选项"
  (let ((target-file (buffer-file-name))
        (target-level (org-supertag-get-target-level level-adjust))
        (success-count 0))
    (unless target-file
      (error "Current buffer is not visiting a file"))
    
    (dolist (node-id node-ids)
      (message "处理节点: %s" node-id)
      (condition-case err
          (when (org-supertag-move-node node-id target-file target-level)
            (cl-incf success-count))
        (error
         (message "处理节点 %s 失败: %s" 
                  node-id 
                  (error-message-string err)))))
    
    (message "完成处理: %d/%d 个节点成功移动" 
             success-count 
             (length node-ids))))

;; Export result to...
(defun org-supertag-ensure-org-file (file)
  "确保文件是有效的org文件.
FILE 是文件路径

返回值：
- t 如果是有效的org文件
- nil 如果不是"
  (and file
       (string-match-p "\\.org$" file)
       (or (file-exists-p file)
           (and (not (file-exists-p file))
                (string-match-p "\\.org$" file)))))

(defun org-supertag-query-export-results-to-new-file ()
  "导出选中的搜索结果到新文件.
将选中的节点导出为一个独立的 org 文件."
  (interactive)
  (let ((selected-nodes (org-supertag-get-selected-nodes)))
    (message "Selected nodes: %S" selected-nodes)
    (if (not selected-nodes)
        (message "No items selected")
      ;; 选择目标文件
      (let* ((default-name "export.org")
             (file (read-file-name 
                   "Export to new file: " 
                   nil nil nil 
                   default-name)))
        ;; 验证文件类型
        (unless (org-supertag-ensure-org-file file)
          (error "Export target must be an org file: %s" file))
        ;; 处理目标文件
        (condition-case err
            (progn
              (when (and (file-exists-p file)
                        (not (y-or-n-p 
                              (format "File %s exists. Overwrite? " file))))
                (error "Export cancelled by user"))
              
              (with-current-buffer (find-file-noselect file)
                (erase-buffer)  ; 清空新文件
                (org-mode)
                
                ;; 插入文件头
                (let ((title (file-name-base file)))
                  (insert (format "#+TITLE: %s\n" title)
                          "#+OPTIONS: ^:nil\n"  ; 禁用上标
                          "#+STARTUP: showeverything\n\n"))  ; 显示所有内容
                
                (message "Inserting nodes...")
                (org-supertag-insert-nodes selected-nodes nil)
                (save-buffer)
                ;; 显示新文件
                (find-file file)
                (other-window 1)
                (balance-windows)
                (message "Export completed successfully to %s" file)))
          ;; 错误处理
          (error
           (message "Export failed: %s" (error-message-string err))
           (signal (car err) (cdr err))))))))

(defun org-supertag-query-export-results-to-file ()
  "导出选中的搜索结果到指定文件的指定位置.

可选的插入位置:
1. File End - 插入到文件末尾
2. Under Heading - 作为选中标题的子标题
3. Same Level - 作为选中标题的同级标题"
  (interactive)
  (let* ((selected-nodes (org-supertag-get-selected-nodes)))
    (if (not selected-nodes)
        (message "No nodes selected")
      ;; 选择目标文件
      (condition-case err
          (let* ((target-file (read-file-name "Export to file: "))
                 (insert-type (completing-read 
                             "Insert at: "
                             '("File End" 
                               "Under Heading"
                               "Same Level"))))
            
            ;; 验证文件类型
            (unless (org-supertag-ensure-org-file target-file)
              (error "Export target must be an org file: %s" target-file))
            
            ;; 处理目标文件
            (with-current-buffer (find-file-noselect target-file)
              (org-mode)
              (let (target-point level-adjust)
                ;; 根据不同选项确定插入位置和层级调整
                (pcase insert-type
                  ("File End"
                   (setq target-point (point-max)
                         level-adjust nil))
                  
                  ("Under Heading"
                   (let* ((headlines (org-map-entries 
                                    (lambda () 
                                      (cons (org-get-heading t t t t)
                                           (point)))))
                          (selected (if headlines
                                      (completing-read 
                                       "Select parent heading: "
                                       headlines)
                                    (error "No headlines found in target file"))))
                     (setq target-point (cdr (assoc selected headlines))
                           level-adjust :child)))
                  
                  ("Same Level"
                   (let* ((headlines (org-map-entries 
                                    (lambda () 
                                      (cons (org-get-heading t t t t)
                                           (point)))))
                          (selected (if headlines
                                      (completing-read 
                                       "Select sibling heading: "
                                       headlines)
                                    (error "No headlines found in target file"))))
                     (setq target-point (cdr (assoc selected headlines))
                           level-adjust :same-level))))
                
                ;; 插入内容
                (goto-char (or target-point (point-max)))
                (org-supertag-insert-nodes selected-nodes level-adjust)
                (save-buffer)
                
                ;; 显示目标文件
                (find-file target-file)
                (message "Export completed successfully"))))
        
        ;; 错误处理
        (error
         (message "Export failed: %s" (error-message-string err))
         (signal (car err) (cdr err)))))))

;;; Save org-supertag-query result at cursor place
(defvar org-supertag-query--original-buffer nil
  "存储发起搜索的原始缓冲区.")

(defvar org-supertag-query--original-point nil
  "存储发起搜索时的光标位置.")

(defun org-supertag-query-export-results-here ()
  "导出选中的搜索结果到当前光标位置.
自动调整内容层级以匹配当前位置的上下文.

执行流程:
1. 记住当前位置
2. 启动搜索
3. 在搜索结果中选择内容
4. 将选中内容插入到原始位置"
(interactive)
  (when (and org-supertag-query--original-buffer
             org-supertag-query--original-point)
    (let ((selected-nodes (org-supertag-get-selected-nodes)))
      (when selected-nodes
        (with-current-buffer org-supertag-query--original-buffer
          (save-excursion
            (goto-char org-supertag-query--original-point)
            (let ((current-level (org-current-level))
                  level-adjust)
              ;; 确定层级调整方式
              (setq level-adjust
                    (cond
                     ;; 在标题内部
                     (current-level
                      (if (org-at-heading-p)
                          :same-level  ; 当前在标题行，作为同级
                        :child))      ; 当前在标题内容中，作为子标题
                     ;; 不在任何标题下
                     (t nil)))        ; 保持原有层级
              
              ;; 插入内容
              (org-supertag-insert-nodes selected-nodes level-adjust))))
        (message "Content inserted at original position")
        ;; 清理变量
        (setq org-supertag-query--original-buffer nil
              org-supertag-query--original-point nil)))))

;;------------------------------------------------------
;; User Interactive Command
;;------------------------------------------------------

(defun org-supertag-query ()
  "交互式搜索."
  (interactive)
  ;; 确保数据库已初始化
  (unless (and (boundp 'org-supertag-db--object)
               org-supertag-db--object))
  
  (let* ((input (read-string "输入搜索关键字 (空格分隔): "))
         (keywords (split-string input " " t)))
    (org-supertag-query-show-results keywords)))

(provide 'org-supertag-query)
