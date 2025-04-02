;;; org-supertag-view-table.el --- Table view for org-supertag -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org-supertag-view-utils)

;;----------------------------------------------------------------------
;; Helpful functions
;;----------------------------------------------------------------------

(defun org-supertag-view-table-get-field-info ()
  "获取当前表格单元格的相关信息。
返回一个属性列表，包含以下信息：
- :col - 当前列号
- :row-data - 当前行数据
- :tag - 当前标签
- :tag-def - 标签定义
- :fields - 字段定义列表
- :field-idx - 字段索引（相对于字段列表）
- :field - 当前字段定义
- :value - 当前单元格的值"
  (when (org-at-table-p)
    (let* ((col (org-table-current-column))
           (row-data (org-supertag-view-table-get-line))
           (tag (org-supertag-view--get-current-tag))
           (tag-def (org-supertag-tag-get tag))
           (fields (plist-get tag-def :fields))
           (field-idx (- col 4))  ;; 调整列索引计算，现在有Node、Type和Date三个固定列
           (field (when (and (>= field-idx 0) (< field-idx (length fields)))
                    (nth field-idx fields)))
           (value (org-table-get nil col)))
      (list :col col
            :row-data row-data
            :tag tag
            :tag-def tag-def
            :fields fields
            :field-idx field-idx
            :field field
            :value value))))

(defun org-supertag-view-table-get-line ()
  "Get all fields in the current table row as a list.
Returns a list where each element is the content of a cell in the current row."
  (save-excursion
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)))
          (fields '()))
      ;; 确保我们在表格行上
      (when (string-match "^[ \t]*|" line)
        ;; 移除开头和结尾的 |
        (setq line (replace-regexp-in-string "^[ \t]*|" "" line))
        (setq line (replace-regexp-in-string "|[ \t]*$" "" line))
        ;; 分割字段并删除前后空白
        (setq fields (mapcar #'string-trim (split-string line "|"))))
      fields)))

(defun org-supertag-view--find-node-by-title (title)
  "通过标题查找节点ID。
TITLE是节点的标题。
返回找到的节点ID，如果未找到则返回nil。
进行精确匹配和模糊匹配，优先返回精确匹配结果。"
  (when (and title (not (string-empty-p title)))
    (let ((cleaned-title (string-trim title))
          (exact-match nil)
          (fuzzy-matches '()))
      
      ;; 规范化标题（去除多余空格）
      (setq cleaned-title (replace-regexp-in-string "\\s-+" " " cleaned-title))
      
      ;; 搜索数据库中的所有节点
      (maphash
       (lambda (id props)
         (when (eq (plist-get props :type) :node)
           (let ((node-title (plist-get props :title))
                 (node-org-id (plist-get props :id)))
             (when node-title
               ;; 规范化节点标题
               (setq node-title (replace-regexp-in-string "\\s-+" " " (string-trim node-title)))
               
               ;; 检查精确匹配
               (if (string= cleaned-title node-title)
                   (setq exact-match id)
                 ;; 检查模糊匹配（包含关系）
                 (when (or (string-match-p (regexp-quote cleaned-title) node-title)
                           (string-match-p (regexp-quote node-title) cleaned-title))
                   (push (cons id (length node-title)) fuzzy-matches)))))))
       org-supertag-db--object)
      
      ;; 优先返回精确匹配
      (or exact-match
          ;; 否则返回最接近的模糊匹配（选择标题长度最接近的）
          (when fuzzy-matches
            (caar (sort fuzzy-matches
                        (lambda (a b)
                          (< (abs (- (cdr a) (length cleaned-title)))
                             (abs (- (cdr b) (length cleaned-title))))))))))))

(defun org-supertag-view--find-node-by-id (file-path org-id)
  "通过ID定位到文件中的节点。
FILE-PATH 是文件路径，ORG-ID 是节点的ID。
返回是否成功找到节点。"
  (when (and file-path (file-exists-p file-path) org-id)
    (find-file file-path)
    (widen)
    ;; 禁用org-element-cache以避免解析错误
    (when (boundp 'org-element-use-cache)
      (setq-local org-element-use-cache nil))
    
    (goto-char (point-min))
    ;; 通过ID属性定位节点
    (when (re-search-forward (format ":ID:\\s-*%s\\s-*$" 
                                    (regexp-quote org-id))
                            nil t)
      (org-back-to-heading t)
      (org-fold-show-entry)
      (org-fold-show-children)
      (recenter 1)
      t)))

(defun org-supertag-view-table-node-at-point ()
  "View the node at point in the SuperTag table view.
策略:
1. 从表格行获取节点标题
2. 通过标题在数据库中查找节点ID和文件位置
3. 通过ID定位到节点"
  (interactive)
  (when (org-at-table-p)
    (let* ((row-data (org-supertag-view-table-get-line))
           (node-text (nth 0 row-data))
           ;; 清理节点标题（移除[v]和[N/A]标记）
           (node-title (when node-text
                        (string-trim
                         (replace-regexp-in-string "\\[\\(v\\|N/A\\)\\]\\s-*" "" 
                                                 node-text))))
           (node-id (org-supertag-view--find-node-by-title node-title)))
      
      (if node-id
          (let* ((node-props (gethash node-id org-supertag-db--object))
                 (file-path (plist-get node-props :file-path))
                 (org-id (plist-get node-props :id)))
            (if (org-supertag-view--find-node-by-id file-path org-id)
                (message "成功定位到节点: %s" node-title)
              (message "无法在文件中找到节点: %s" node-title)))
        (message "找不到节点: %s" node-title)))))

;;----------------------------------------------------------------------
;; Single Tag View Mode (Table View)
;;----------------------------------------------------------------------

(defun org-supertag-view--show-content-table (tag)
  "Show content table for TAG in a dedicated full-screen buffer."
  (let ((buffer (get-buffer-create (format "*Org SuperTag Table View: %s*" tag))))
    (with-current-buffer buffer
      (org-mode)  
      (org-supertag-view-table-mode)  
      (setq-local org-supertag-view-current-tag tag)

      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; 使用普通文本而非org标题
        (insert (format "标签: #%s\n\n" tag))
        (insert "操作说明:\n")
        (insert " [q] - 退出    [g] - 刷新    [v] - 查看节点    [m] - 管理关系\n")
        (insert " [e] - 智能编辑    [C-c '] - 切换编辑/只读模式\n")
        (insert " [Tab] - 下一字段    [S-Tab] - 上一字段    [n/p] - 上下移动\n")
        (insert " 点击节点前的[v]按钮可直接查看节点内容\n")
        (insert " 编辑字段时会自动保存修改\n\n")
        ;; 插入表格内容
        (org-supertag-view--insert-content-table-with-button tag))

      (setq buffer-read-only t)
      (goto-char (point-min)))
    
    ;; 使用全屏显示，而非侧边窗口
    (switch-to-buffer buffer)
    (delete-other-windows)))

(defun org-supertag-view--get-field-value-from-db (node-id field-name)
  "Get field value from database.
NODE-ID is the node identifier
FIELD-NAME is the field name"
  (let* ((link-id (format ":node-field:%s->%s" node-id field-name))
         (link (gethash link-id org-supertag-db--link)))
    (when link
      (plist-get link :value))))

(defun org-supertag-view--insert-content-table-with-button (tag)
  "Insert content related to TAG in current buffer using org table format with buttons.
每行节点前添加[v]按钮，点击可直接查看节点内容。"
  (insert "相关节点:\n\n")
  (let* ((content (org-supertag-view--get-related-nodes tag))
         (tag-def (org-supertag-tag-get tag))
         (fields (plist-get tag-def :fields)))
    
    (if (not content)
        (insert (format "未找到与标签 #%s 相关的内容" tag))
      ;; 插入表头
      (insert "|Node|Type|Date")
      (dolist (field fields)
        (insert (format "|%s" (plist-get field :name))))
      (insert "|\n")
      
      ;; 插入分隔线
      (insert "|------|------|----")
      (dolist (_ fields) (insert "|-----"))
      (insert "|\n")
      
      ;; 插入数据行
      (dolist (item content)
        (let* ((node-id (plist-get item :id))
               (node-props (gethash node-id org-supertag-db--object))
               (node-title (plist-get item :node))
               (org-id (plist-get node-props :id))
               (file-path (plist-get node-props :file-path)))
          
          ;; 插入节点列（带按钮）
          (insert "|")
          (if (and file-path org-id)
              (progn
                (insert-text-button "[v]"
                                  'face 'link
                                  'follow-link t
                                  'file-path file-path
                                  'org-id org-id
                                  'node-title node-title
                                  'action (lambda (button)
                                          (let ((file (button-get button 'file-path))
                                                (org-id (button-get button 'org-id))
                                                (title (button-get button 'node-title)))
                                            (when (org-supertag-view--find-node-by-id file org-id)
                                              (message "成功打开节点: %s" title)))))
                (insert " "))
            (insert "[N/A] "))
          
          ;; 插入基本信息
          (insert (format "%s|%s|%s"
                         node-title
                         (or (plist-get item :type) "")
                         (plist-get item :date)))
          
          ;; 插入字段值
          (dolist (field fields)
            (let* ((field-name (plist-get field :name))
                   (value (org-supertag-view--get-field-value-from-db node-id field-name)))
              (insert (format "|%s" (or value "")))))
          (insert "|\n")))
      
      ;; 对齐表格
      (save-excursion
        (backward-char)
        (org-table-align)))))


(defun org-supertag-view--get-field-index (col)
  "Get field index based on column position COL."
  (let ((pos 0)
        (current 0))
    (while (< current col)
      (setq current (+ current (if (= pos 0) 20 15)))
      (setq pos (1+ pos)))
    pos))

(defun org-supertag-view--get-current-tag ()
  "Get current tag from buffer name or buffer local variable."
  (if (boundp 'org-supertag-view-current-tag)
      ;; 首先尝试从局部变量获取
      org-supertag-view-current-tag
    ;; 其次尝试从缓冲区名称提取
    (when (string-match "\\*Org SuperTag Table View: \\([^*]+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name)))))
        
(defun org-supertag-view--update-field-value (node-id field-name value)
  "Update VALUE of FIELD-NAME for NODE-ID.
Returns t if update was successful, nil otherwise."
  (when (and node-id field-name)
    (if (not value)
        ;; 如果值为空，移除字段
        (org-supertag-node-remove-field node-id field-name)
      ;; 否则更新字段值
      (org-supertag-node-set-field node-id field-name value))))

(defun org-supertag-view--get-field-value (node-id field-name)
  "Get value of FIELD-NAME for NODE-ID."
  (let* ((tag (org-supertag-view--get-current-tag))
         (tag-def (org-supertag-tag-get tag)))
    ;; 使用 org-supertag-tag-get-field-value 获取字段值
    (org-supertag-tag-get-field-value tag-def field-name)))

(defun org-supertag-view-table-get-all-rows ()
  "获取表格中的所有数据行。
返回一个列表，每个元素是一行数据的列表。"
  (let ((rows '()))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "|Node|Type|Date" nil t)
        (forward-line 2) ;; 跳过表头和分隔线
        
        ;; 收集每一行
        (while (org-at-table-p)
          (let ((row-data (org-supertag-view-table-get-line)))
            (when row-data
              (push row-data rows)))
          (forward-line 1))))
    (nreverse rows)))

(defun org-supertag-view-table-find-node-id (node-title)
  "根据节点标题查找节点ID。
NODE-TITLE 是节点的标题。
返回找到的节点ID，如果未找到则返回nil。"
  (when (and node-title (not (string-empty-p node-title)))
    ;; 清理标题（去除按钮文本和空白）
    (let ((clean-title node-title))
      (setq clean-title (replace-regexp-in-string "\\[v\\]\\s-*" "" clean-title))
      (setq clean-title (replace-regexp-in-string "\\[N/A\\]\\s-*" "" clean-title))
      (setq clean-title (string-trim clean-title))
      
      ;; 使用通用的节点查找函数
      (org-supertag-view--find-node-by-title clean-title))))

(defun org-supertag-view-table-update-field (node-id field-name field-value tag-id)
  "Update field value in database and file.
NODE-ID is the node identifier
FIELD-NAME is the field name
FIELD-VALUE is the new value
TAG-ID is the tag identifier
Returns t if update successful, nil if failed."
  (condition-case err
      (progn
        ;; 1. 更新数据库
        (let ((link-id (format ":node-field:%s->%s" node-id field-name)))
          (if (and field-value (not (string-empty-p field-value)))
              ;; 有值，更新数据库
              (puthash link-id 
                       (list :from node-id 
                             :to field-name 
                             :tag-id tag-id
                             :value field-value
                             :created-at (current-time))
                       org-supertag-db--link)
            ;; 值为空，移除字段
            (remhash link-id org-supertag-db--link)))
        
        ;; 2. 更新文件
        (when-let* ((node-props (gethash node-id org-supertag-db--object))
                    (file-path (plist-get node-props :file-path))
                    (pos (plist-get node-props :pos)))
          (with-current-buffer (find-file-noselect file-path)
            (save-excursion
              (goto-char pos)
              (if (and field-value (not (string-empty-p field-value)))
                  (org-set-property field-name field-value)
                (org-delete-property field-name)))))
        
        ;; 3. 标记数据库为脏并安排保存
        (org-supertag-db--mark-dirty)
        (org-supertag-db--schedule-save)
        t)  ;; 返回成功
    (error
     (message "Error updating field %s: %s" field-name (error-message-string err))
     nil)))

(defun org-supertag-view-cancel-edit ()
  "Cancel editing and restore original view."
  (interactive)
  (when (yes-or-no-p "Cancel editing? Changes will be lost.")
    (let ((tag (org-supertag-view--get-current-tag)))
      (quit-window)
      (org-supertag-view--show-content-table tag))))

(defun org-supertag-view--get-field-link (node-id field-name)
  "Get field link for NODE-ID and FIELD-NAME.
Performs case-insensitive search for field name."
  (let ((link-id (format ":node-field:%s->%s" node-id field-name)))
    (gethash link-id org-supertag-db--link)))

(defun org-supertag-view--get-related-nodes (tag)
  "Get nodes related to TAG.
Returns a list of plists with properties :node, :type, :date and field values."
  (let* ((nodes '())
         (tag-def (org-supertag-tag-get tag))
         (fields (plist-get tag-def :fields)))
    (maphash
     (lambda (link-id link-props)
       ;; 首先找到与该标签相关的所有节点
       (when (and (string-match ":node-tag:\\(.+\\)->\\(.+\\)$" link-id)
                  (equal (match-string 2 link-id) tag))
         (when-let* ((node-id (plist-get link-props :from))
                    (node-props (gethash node-id org-supertag-db--object)))
           ;; 对每个节点，获取其所有字段的值
           (let ((field-values
                  (mapcar
                   (lambda (field)
                     (let* ((field-name (plist-get field :name))
                            ;; 查询字段值从link哈希表中
                            (link-id (format ":node-field:%s->%s" node-id field-name))
                            (field-link (gethash link-id org-supertag-db--link))
                            (value (when field-link 
                                    (plist-get field-link :value))))
                       (message "DEBUG: Node %s Field %s Value %s" node-id field-name value)
                       (cons field-name value)))
                   fields)))
             
             ;; 构建节点信息
             (push (list :node (or (plist-get node-props :title)
                                  (format "Node %s" node-id))
                        :type (or (plist-get node-props :todo-state) "Node")
                        :date (format-time-string 
                              "%Y-%m-%d"
                              (or (plist-get node-props :created-at)
                                  (current-time)))
                        :id node-id
                        :fields field-values)
                   nodes)))))
     org-supertag-db--link)
    (nreverse nodes)))

(defun org-supertag-view--edit-field-value (field-info)
  "编辑字段值的核心函数。
FIELD-INFO 是包含当前字段所有信息的属性列表。
返回 (success . new-value) 的cons对，success为t表示编辑成功。"
  (let* ((field (plist-get field-info :field))
         (field-type (when field (plist-get field :type)))
         (field-name (when field (plist-get field :name)))
         (current-value (plist-get field-info :value))
         (col (plist-get field-info :col))
         new-value)
    
    (setq new-value
          (cond
           ;; Date列或date类型
           ((or (= col 3) (eq field-type 'date))
            (let ((date (org-read-date nil t nil "Enter date" 
                                      (when (and current-value 
                                                (not (string-empty-p current-value)))
                                        current-value))))
              (format-time-string "<%Y-%m-%d %a>" date)))
           
           ;; timestamp类型
           ((eq field-type 'timestamp)
            (let ((date-time (org-read-date t t nil "Enter date and time" 
                                          (when (and current-value 
                                                    (not (string-empty-p current-value)))
                                            current-value))))
              (format-time-string "[%Y-%m-%d %a %H:%M]" date-time)))
           
           ;; options类型
           ((eq field-type 'options)
            (let ((options (or (plist-get field :options) '("Option1" "Option2"))))
              (completing-read
               (format "Select option for %s: " field-name)
               options nil t current-value)))
           
           ;; tag-reference类型
           ((eq field-type 'tag-reference)
            (let ((nodes (list)))
              (maphash
               (lambda (id props)
                 (when (eq (plist-get props :type) :node)
                   (let ((title (or (plist-get props :title) 
                                   (format "Node %s" id))))
                     (push (cons title id) nodes))))
               org-supertag-db--object)
              (let* ((choices (mapcar #'car nodes))
                     (selected (completing-read
                              (format "Select node for %s: " field-name)
                              choices nil t nil)))
                (when selected
                  (let ((node-id (cdr (assoc selected nodes))))
                    (format "[[%s][%s]]" node-id selected))))))
           
           ;; list类型
           ((eq field-type 'list)
            (read-string 
             (format "Enter values for %s (comma-separated): " field-name)
             current-value))
           
           ;; range类型
           ((eq field-type 'range)
            (read-string 
             (format "Enter range for %s (N-M or N..M): " field-name)
             current-value))
           
           ;; 默认使用简单的字符串输入
           (t
            (read-string (format "Edit%s: " 
                                (if field-name 
                                    (format " %s" field-name)
                                  (format " column %d" col)))
                        current-value))))
    
    (when new-value
      ;; 验证新值
      (let ((validated-value (org-supertag-view-validate-field 
                            new-value field-type field)))
        (when validated-value
          (cons t validated-value))))))

(defun org-supertag-view-smart-edit ()
  "智能编辑功能 - 根据当前字段类型选择合适的编辑方式。
编辑完成后立即保存字段值到数据库。"
  (interactive)
  (if (not (org-at-table-p))
      ;; 不在表格上，切换整体编辑模式
      (progn 
        (setq buffer-read-only nil)
        (message "表格已解锁，进入编辑模式"))
    
    ;; 在表格上，获取字段信息并进行编辑
    (let* ((field-info (org-supertag-view-table-get-field-info))
           (col (plist-get field-info :col))
           (tag (plist-get field-info :tag))
           (row-data (plist-get field-info :row-data))
           (node-title (when row-data 
                        (string-trim
                         (replace-regexp-in-string "\\[\\(v\\|N/A\\)\\]\\s-*" "" 
                                                 (nth 0 row-data)))))
           (node-id (when node-title 
                     (org-supertag-view-table-find-node-id node-title)))
           (field (plist-get field-info :field))
           (field-name (when field (plist-get field :name))))
      
      ;; 确保编辑前解除只读状态
      (when buffer-read-only
        (setq buffer-read-only nil))
      
      ;; 编辑字段值
      (let* ((edit-result (org-supertag-view--edit-field-value field-info))
             (success (car edit-result))
             (new-value (cdr edit-result)))
        
        (when (and success new-value)
          ;; 更新数据库（如果是自定义字段）
          (when (and node-id field-name (> col 3))
            (org-supertag-view-table-update-field node-id field-name new-value tag))
          
          ;; 更新表格显示
          (org-table-put nil col new-value)
          (org-table-align)
          
          (message "字段已更新: %s" new-value))))))

(defun org-supertag-view-validate-field (value field-type field-props)
  "根据字段类型验证并格式化输入值的有效性。
对输入的VALUE根据FIELD-TYPE和FIELD-PROPS验证并返回格式化后的值。
如果值无效，返回nil并显示错误信息。"
  (cond
   ;; 日期字段验证 - 确保格式为 <YYYY-MM-DD XXX>
   ((eq field-type 'date)
    (if (string-match-p "^<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( [A-Za-z]\\{3\\}\\)?>$" value)
        value
      (message "无效的日期格式。请使用 <YYYY-MM-DD> 格式")
      nil))
   
   ;; 时间戳字段验证 - 确保格式为 [YYYY-MM-DD XXX HH:MM]
   ((eq field-type 'timestamp)
    (if (string-match-p "^\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( [A-Za-z]\\{3\\}\\)?\\( [0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\]$" value)
        value
      (message "无效的时间戳格式。请使用 [YYYY-MM-DD HH:MM] 格式")
      nil))
   
   ;; 数字字段验证
   ((eq field-type 'number)
    (if (string-match-p "^[+-]?[0-9]*\\.?[0-9]+$" value)
        value
      (message "无效的数字格式")
      nil))
   
   ;; 范围字段验证 - 确保格式为 N-M 或 N..M
   ((eq field-type 'range)
    (if (string-match-p "^[0-9]+\\(\\.\\.|[-~]\\)[0-9]+$" value)
        value
      (message "无效的范围格式。请使用 N-M 或 N..M 格式")
      nil))
   
   ;; 列表字段验证 - 确保格式为逗号分隔的项目
   ((eq field-type 'list)
    (if (or (string-empty-p value)
            (string-match-p "^[^,]+\\(,[^,]+\\)*$" value))
        value
      (message "无效的列表格式。请使用逗号分隔项目")
      nil))
   
   ;; 选项字段验证 - 确保值在允许的选项列表中
   ((eq field-type 'options)
    (let ((allowed-options (plist-get field-props :options)))
      (if (or (string-empty-p value)
              (member value allowed-options))
          value
        (message "无效的选项值。允许的选项: %s" 
                 (mapconcat #'identity allowed-options ", "))
        nil)))
   
   ;; 标签引用字段验证 - 确保引用的标签存在
   ((eq field-type 'tag-reference)
    (if (or (string-empty-p value)
            (org-supertag-node-exists-p value))
        value
      (message "引用的节点不存在: %s" value)
      nil))
   
   ;; 默认情况下接受任何值
   (t value)))

;;----------------------------------------------------------------------
;; Mode definitions
;;----------------------------------------------------------------------

(defun org-supertag-view-refresh ()
  "Refresh the current view."
  (interactive)
  (cond
   ;; in multi-column view mode
   ((eq major-mode 'org-supertag-column-mode)
    (org-supertag-view--refresh-column-view))
   ;; in tag discovery mode
   ((eq major-mode 'org-supertag-discover-mode)
    (org-supertag-view--refresh-discover))
   ;; in tag-only view mode
   ((and (eq major-mode 'org-mode) 
         (bound-and-true-p org-supertag-view-mode)
         (string-match-p "\\*Org SuperTag Table View:" (buffer-name)))
    (let ((tag (progn
                 (string-match "\\*Org SuperTag Table View: \\(.*\\)\\*" (buffer-name))
                 (match-string 1 (buffer-name)))))
      (when tag
        (org-supertag-view--show-content-table tag))))
   ;; in traditional view (compatible with old code)
   (t
    (let ((tag (car (split-string (buffer-name) ": #"))))
      (when tag
        (org-supertag-tag-columns tag))))))

;;----------------------------------------------------------------------
;; Mode definitions
;;----------------------------------------------------------------------

(define-derived-mode org-supertag-view-table-mode org-mode "SuperTag-Table"
  "Major mode for displaying tag content in table format.
This mode is based on org-mode to ensure compatibility with org table functions.
\\{org-supertag-view-table-mode-map}"
  :group 'org-supertag
  (setq-local org-element-use-cache nil)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq header-line-format 
        (propertize " Org-Supertag Table View" 'face '(:weight bold)))
  (let ((map (make-sparse-keymap)))
    ;; 先定义最关键的编辑按键，确保它们有最高优先级
    (define-key map (kbd "e") (lambda () 
                               (interactive)
                               (let ((inhibit-read-only t))
                                 (call-interactively 'org-supertag-view-smart-edit))))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-view-refresh)
    (define-key map (kbd "v") 'org-supertag-view-table-node-at-point)
    (define-key map (kbd "V") 'org-supertag-view-table-view-all-nodes)
    (define-key map (kbd "m") 'org-supertag-view-manage-relations)
    (dolist (key '("d" "o" "r" "l" "a" "t"))
      (define-key map (kbd key) 
                 (lambda () 
                   (interactive)
                   (let ((inhibit-read-only t))
                     (call-interactively 
                      (intern (format "org-supertag-view-edit-%s-field"
                                     (pcase key
                                       ("d" "date")
                                       ("o" "options")
                                       ("r" "reference")
                                       ("l" "list")
                                       ("a" "range")
                                       ("t" "timestamp")))))))))
    (define-key map (kbd "C-c C-k") 'org-supertag-view-cancel-edit)
    (define-key map (kbd "C-c '") 'org-supertag-view-edit-table)
    (define-key map (kbd "<tab>") 'org-table-next-field)
    (define-key map (kbd "<backtab>") 'org-table-previous-field)
    (define-key map (kbd "M-p") 'org-table-copy-down)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "2") 'org-supertag-view-switch-to-discover)
    (define-key map (kbd "3") 'org-supertag-view-switch-to-columns)
    (set-keymap-parent map org-mode-map)
    (use-local-map map)))

(defun org-supertag-view-table-setup-keys ()
  "设置表格视图模式的按键绑定。"
  (interactive)
  ;; 设置表格查看和编辑功能
  (define-key org-supertag-view-table-mode-map (kbd "e")
              (lambda () 
                (interactive)
                (let ((inhibit-read-only t))
                  (call-interactively 'org-supertag-view-smart-edit))))
  (define-key org-supertag-view-table-mode-map (kbd "v") 
              'org-supertag-view-table-node-at-point)
  (define-key org-supertag-view-table-mode-map (kbd "g") 
              'org-supertag-view-refresh)
  (define-key org-supertag-view-table-mode-map (kbd "q") 
              'quit-window))


;;;###autoload
(defun org-supertag-view-table (&optional tag)
  "Show content related to a tag in table view format.
If TAG is provided, show content for that tag.
If point is on a tag, show content for that tag.
Otherwise, prompt for a tag using completion.

The table view displays:
- Tag properties and description
- A table of related nodes with their properties
- Operation instructions for navigation and management"
  (interactive)
  (let ((tag-to-use (or tag 
                       (org-supertag-view--get-tag-name)
                       (completing-read "View tag in table format: "
                                      (org-supertag-view--get-all-tags)
                                      nil t))))
    (org-supertag-view--show-content-table tag-to-use)))

(define-obsolete-function-alias 'org-supertag-view-tag-only 'org-supertag-view-table "1.0")



(provide 'org-supertag-view-table)


