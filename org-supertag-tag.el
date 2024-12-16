;;; org-supertag-tag.el --- Tag relation management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供标签关系管理功能
;; 核心原则：通过关系连接实体，使用 type、from、to 来表达关系

;;----------------------------------------------------------------------
;; Tag Database Operation
;;---------------------------------------------------------------------
(defun org-supertag-tag-db-add (tag-id props)
  "添加标签到数据库.
TAG-ID: 标签ID
PROPS: 标签属性"
  (org-supertag-db-add tag-id props))

(defun org-supertag-tag-db-get (tag-id)
  "从数据库获取标签.
TAG-ID: 标签ID"
  (org-supertag-db-get tag-id))

(defun org-supertag-tag-db-update (tag-id props)
  "更新数据库中的标签.
TAG-ID: 标签ID
PROPS: 新的属性"
  (org-supertag-db-add tag-id props))

(defun org-supertag-tag-db-delete (tag-id)  
  "删除数据库中的标签.
TAG-ID: 标签ID"
  (org-supertag-db-delete tag-id))

;;----------------------------------------------------------------------
;; Tag Name Operation
;;----------------------------------------------------------------------

(defun org-supertag-sanitize-tag-name (name)
  "将名称转换为有效的标签名称.
NAME 是要转换的名称
- 移除开头的 #
- 将空格转换为下划线
- 验证非空"
  (if (or (null name) (string-empty-p name))
      (error "Tag name cannot be empty")
    (let* ((trimmed (string-trim name))
           (without-hash (replace-regexp-in-string "^#+" "" trimmed))
           (sanitized (replace-regexp-in-string "\\s-+" "_" without-hash)))
      (if (string-empty-p sanitized)
          (error "Invalid tag name: %s" name)
        sanitized))))

(defun org-supertag-tag-exists-p (tag-name)
  "检查标签是否存在.
TAG-NAME 是标签名称"
  (let ((sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    (org-supertag-tag-db-get sanitized-name)))

;;----------------------------------------------------------------------
;; Tag Base Operation
;;----------------------------------------------------------------------

(defun org-supertag-tag-create (tag-name &rest props)
  "创建新标签.
TAG-NAME: 标签名称
PROPS: 额外属性"
  (let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
         (base-props (list :type :tag
                          :id sanitized-name
                          :name sanitized-name  ; 添加必需的 name 属性
                          :fields (or (plist-get props :fields) '()) ; 确保有 fields
                          :created-at (current-time)))
         (full-props (append base-props props)))
    (org-supertag-db-add sanitized-name full-props)
    sanitized-name))

(defun org-supertag-get-all-tags ()
  "获取所有已定义的标签."
  (let ((all-entities (org-supertag-db-get-all)))
    (cl-loop for (_id . entity) in all-entities
             when (eq (plist-get entity :type) :tag)
             collect (plist-get entity :id))))

(defun org-supertag-tag-get (tag-name)
  "获取标签定义.
TAG-NAME 是标签名称"
  (let ((entity (org-supertag-db-get tag-name)))
    (when (and entity 
               (eq (plist-get entity :type) :tag))
      entity)))

;;----------------------------------------------------------------------
;; Tag-Node Relation Operation
;;----------------------------------------------------------------------

(defun org-supertag-tag-apply (tag-id)
  "将标签应用到当前位置的节点."
  (let* ((tag (org-supertag-tag-get tag-id))
         (node-id (org-id-get-create)))
    ;; 确保标签存在
    (unless tag
      (error "Tag %s not found" tag-id))
    ;; 确保节点存在或创建
    (unless (org-supertag-db-get node-id)
      (org-supertag--create-node node-id))
    ;; 创建标签关联
    (org-supertag-db-link 
     :node-tag 
     node-id 
     tag-id 
     (list :created-at (current-time)))
    ;; 初始化字段
    (when-let ((fields (plist-get tag :fields)))
      (dolist (field fields)
        (let ((field-name (plist-get field :name)))
          (org-entry-put nil field-name ""))))
    ;; 添加标签到 org tags
    (let ((tags (org-get-tags)))
      (org-set-tags (cons (concat "#" tag-id) tags)))
    node-id))

(defun org-supertag-tag--remove (tag-id node-id)
  "从节点移除标签.
TAG-ID: 标签ID
NODE-ID: 节点ID"
  ;; 1. 移除标签-节点关系
  (org-supertag-db-remove-link :node-tag node-id tag-id)
  ;; 2. 移除相关的字段值
  (let ((tag (org-supertag-tag-db-get tag-id)))
    (dolist (field-def (plist-get tag :fields))
      (org-supertag-field-remove-value field-def node-id tag-id))))

;;----------------------------------------------------------------------
;; Tag Field Operation
;;----------------------------------------------------------------------

(defun org-supertag--sanitize-field-name (name)
  "将字段名转换为有效的属性名.
NAME 是原始字段名"
  (let ((sanitized (replace-regexp-in-string
                   "\\s-+" "_"  ; 将空格替换为下划线
                   (upcase (string-trim name)))))  ; 转为大写并去除首尾空格
    sanitized))

(defun org-supertag-tag-get-nodes (tag-id)
  "获取使用了指定标签的所有节点.
TAG-ID: 标签ID"
  (org-supertag-db-find-nodes-by-tag tag-id))

(defun org-supertag-tag-get-nodes (tag-id)
  "获取与指定标签关联的所有节点ID.
TAG-ID: 标签ID（不带'tag:'前缀）"
  (let (result)
    (maphash
     (lambda (link-id props)
       (message "Checking link: %s with props: %s" link-id props)
       (when (and (string-prefix-p ":node-tag:" link-id)
                  (equal (plist-get props :to) tag-id))  ; 直接比较，不处理前缀
         (push (plist-get props :from) result)))
     org-supertag-db--link)
    (delete-dups result)))

(defun org-supertag-tag-get-fields (tag-id)
  "获取标签定义的字段列表."
  (plist-get (org-supertag-tag-db-get tag-id) :fields))

(defun org-supertag-tag-add-field (tag-id field-def)
  "为标签添加字段定义."
  (let* ((tag (org-supertag-tag-db-get tag-id))
         (fields (plist-get tag :fields))
         (new-fields (append fields (list field-def))))
    (org-supertag-tag-db-update tag-id 
                               (plist-put tag :fields new-fields))))

(defun org-supertag-tag--set-field-value (tag-id node-id field-name value)
  "内部函数：为标签的字段设置值.
TAG-ID: 标签ID
NODE-ID: 节点ID
FIELD-NAME: 字段名
VALUE: 字段���"
  (message "DEBUG: Setting field value - Tag: %s, Node: %s, Field: %s, Value: %s"
           tag-id node-id field-name value)
  (let* ((tag (org-supertag-tag-db-get tag-id))
         (fields (plist-get tag :fields))
         (field-def (cl-find field-name fields 
                            :key (lambda (f) (plist-get f :name))
                            :test #'equal)))
    (if (not field-def)
        (error "Field %s not found in tag %s" field-name tag-id)
      (org-supertag-field-set-value field-def value node-id tag-id))))

;;----------------------------------------------------------------------
;; Tag User Command
;;----------------------------------------------------------------------

(defun org-supertag-tag-add-tag (tag-name)
  "为当前 headline 添加标签.
TAG-NAME 是标签名称"
  (interactive
   (list (completing-read "选择或输入标签: "
                         (append 
                          (org-supertag-get-all-tags)  ; 已存在的标签
                          org-supertag-preset-tags))))  ; 预设标签
  
(let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
       ;; 获取或创建标签
       (tag (or (org-supertag-tag-get sanitized-name)
                ;; 如果标签不存在，创建一个基础标签
                (org-supertag-tag-create sanitized-name))))   
    ;; 应用标签
    (org-supertag-tag-apply sanitized-name)))

(defun org-supertag-tag-set-field (tag-name)
  "为标签设置字段.
TAG-NAME 是标签名称"
  (interactive
   (list (completing-read "选择标签: "
                         (org-supertag-get-all-tags))))
  (when-let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
              (tag (org-supertag-tag-get sanitized-name)))
    (let* ((current-fields (plist-get tag :fields))
           (fields-and-values (org-supertag--field-interactive-edit current-fields)))
      ;; 更新标签字段
      (org-supertag-tag-db-update sanitized-name
                                 (plist-put tag :fields (car fields-and-values)))      
      ;; 提供反馈
      (message "已更新标签 '%s' 的字段" sanitized-name))))

(defun org-supertag-tag-set-field-value ()
  "为当前节点的标签字段设置值."
  (interactive)
  (let* ((node-id (org-id-get))
         ;; 获取节点的所有标签
         (tags (org-supertag-node-get-tags node-id))
         ;; 收集所有标签的字段信息
         (tag-fields (cl-loop for tag-id in tags
                             for tag = (org-supertag-tag-get tag-id)
                             when tag
                             collect (cons tag-id 
                                         (plist-get tag :fields))))
         ;; 构建所有字段的列表
         (all-fields (cl-loop for (tag-id . fields) in tag-fields
                             when fields
                             append (mapcar (lambda (field)
                                            (list :tag-id tag-id
                                                  :field field
                                                  :current-value (org-entry-get nil 
                                                                             (plist-get field :name))))
                                          fields)))
         ;; 显示和编辑
         (field-values
          (cl-loop for field-info in all-fields
                   for tag-id = (plist-get field-info :tag-id)
                   for field = (plist-get field-info :field)
                   for field-name = (plist-get field :name)
                   for current-value = (plist-get field-info :current-value)
                   collect
                   (cons
                    (format "[%s] %s (current: %s)" tag-id field-name 
                           (or current-value "未设置"))
                    (cons tag-id field)))))
    
    ;; 让用户选择要编辑的字段
    (while (when-let* ((choice (completing-read "选择要编辑的字段 (完成请按 C-g): "
                                              (mapcar #'car field-values)
                                              nil t))
                       (field-info (cdr (assoc choice field-values)))
                       (tag-id (car field-info))
                       (field-def (cdr field-info)))
             ;; 设置字段值
             (org-supertag-field-set-value field-def
                                          (org-supertag-field-read-value field-def)
                                          node-id
                                          tag-id)
             t))))

(defun org-supertag-tag-set-field-and-value ()
  "为当前节点的标签添加字段并设置值."
  (interactive)
  (let* ((node-id (org-id-get))
         (tags (org-supertag-node-get-tags node-id))
         (tag-id (completing-read "选择标签: " tags nil t))
         (tag (org-supertag-tag-get tag-id))
         (fields (plist-get tag :fields))
         (create-new (or (null fields)
                        (y-or-n-p "创建新字段？")))
         (field-def
          (when create-new
            (let* ((field-name (read-string "字段名称: "))
                  (field-type (completing-read "字段类型: "
                                             (org-supertag-get-field-types)
                                             nil t))
                  (type-symbol (cdr (assoc field-type 
                                          (org-supertag-get-field-types))))
                  (base-def (list :name (org-supertag--sanitize-field-name field-name)
                                 :type type-symbol)))
              base-def)))
         (field-def (or field-def
                       (let ((field-name (completing-read "选择字段: "
                                                        (mapcar (lambda (f)
                                                                (plist-get f :name))
                                                              fields)
                                                        nil t)))
                         (cl-find field-name fields
                                 :key (lambda (f)
                                       (plist-get f :name))
                                 :test #'equal)))))
    
    ;; 如果是新字段，添加到标签定义
    (when create-new
      (org-supertag-tag-add-field tag-id field-def))
    
    ;; 设置字段值
    (org-supertag-field-set-value field-def
                                 (org-supertag-field-read-value field-def)
                                 node-id
                                 tag-id)
    
    (message "已%s字段 '%s' 并设置值"
             (if create-new "创建" "更新")
             (plist-get field-def :name))))

(defun org-supertag-tag-batch-add-tag (tag-name)
  "批量为选中的 headlines 添加标签.
TAG-NAME 是标签名称"
  (interactive
   (list (completing-read "选择或输入标签: "
                         (append 
                          (org-supertag-get-all-tags)
                          org-supertag-preset-tags))))
  
  (let* ((ast (org-element-parse-buffer))
         (headlines '())
         (selected-headlines '())
         (tag (org-supertag-tag-get tag-name)))
    
    ;; 收集所有无 ID 的 headlines
    (org-element-map ast 'headline
      (lambda (headline)
        (unless (org-element-property :ID headline)
          (push headline headlines))))
    
    ;; 构建选择列表
    (let ((choices (append 
                   '(("Finish" . :finish))  ; 添加完成选项
                   (mapcar (lambda (hl)
                            (cons (org-element-property :raw-value hl)
                                 hl))
                          headlines))))
      
      ;; 循环选择，直到选择 Finish
      (while (when-let* ((title (completing-read 
                                (format "选择 headline (已选择 %d 个): "
                                       (length selected-headlines))
                                (mapcar #'car choices)
                                nil t))
                        (choice (cdr (assoc title choices))))
               (unless (eq choice :finish)
                 ;; 添加到选中列表
                 (push choice selected-headlines)
                 ;; 从选项中移除已选项
                 (setq choices (assoc-delete-all title choices))
                 t)))  ; 继续循环，除非选择了 Finish
      
      ;; 应用标签到所有选中的 headlines
      (when selected-headlines
        (dolist (hl selected-headlines)
          (save-excursion
            (goto-char (org-element-property :begin hl))
            (org-supertag-tag-add-tag tag-name)))
        (message "已为 %d 个 headlines 添加标签 %s" 
                 (length selected-headlines)
                 tag-name)))))


(defun org-supertag-tag-remove ()
  "从当前节点移除标签关联及其字段值."
  (interactive)
  (let* ((node-id (org-id-get))
         (tags (org-supertag-node-get-tags node-id))
         (tag-id (completing-read "选择要移除的标签: " tags nil t)))
    
    ;; 使用内部函数移除标签
    (org-supertag-tag--remove tag-id node-id)
    
    (message "已移除节点的标签 '%s' 关联及其字段值" tag-id)))
;;----------------------------------------------------------------------
;; Preset Tag
;;----------------------------------------------------------------------

(defconst org-supertag-preset-tags
  '("project" "task" "person" "meeting" "place" "company" "note")
  "预设标签列表.")

(defconst org-supertag-preset-tag-fields
  '(("project" . ((:name "status" 
                  :type options
                  :options ("planning" "active" "on-hold" "completed" "cancelled")
                  :description "项目状态")
                 (:name "priority"
                  :type options
                  :options ("high" "medium" "low")
                  :description "优先级")
                 (:name "deadline"
                  :type date
                  :description "截止日期")
                 (:name "owner"
                  :type string
                  :description "负责人")))
    
    ("task" . ((:name "status"
                :type options
                :options ("todo" "in-progress" "blocked" "done" "cancelled")
                :description "任务状态")
               (:name "priority"
                :type options  
                :options ("A" "B" "C")
                :description "优先级")
               (:name "due"
                :type date
                :description "到期日")
               (:name "assignee"
                :type string
                :description "执行人")))
    
    ("person" . ((:name "role"
                 :type string
                 :description "角色")
                (:name "email"
                 :type string
                 :description "邮箱")
                (:name "phone"
                 :type string
                 :description "电话")))
    
    ("meeting" . ((:name "date"
                  :type date
                  :description "会议日期")
                 (:name "attendees"
                  :type string
                  :description "参会人")
                 (:name "location"
                  :type string
                  :description "地点")))
    
    ("place" . ((:name "address"
                :type string
                :description "地址")
               (:name "category"
                :type options
                :options ("office" "home" "public" "other")
                :description "场所类型")))
    
    ("company" . ((:name "industry"
                  :type string
                  :description "行业")
                 (:name "website"
                  :type string
                  :description "网站")
                 (:name "contact"
                  :type string
                  :description "联系人")))
    
    ("note" . ((:name "category"
                :type options
                :options ("idea" "reference" "summary" "other")
                :description "笔记类型")
               (:name "source"
                :type string
                :description "来源")))))

(defun org-supertag-get-preset-fields (tag-name)
  "获取预设标签的字段定义.
TAG-NAME 是标签名称"
  (cdr (assoc tag-name org-supertag-preset-tag-fields)))


;;----------------------------------------------------------------------
;; Test
;;----------------------------------------------------------------------  

(ert-deftest test-org-supertag-tag-basic ()
  "测试标签基本操作."
  (org-supertag-db-init)  ;; 初化数据库
  
  ;; 1. 创建标签
  (let* ((tag-id "test-tag")
         (fields '((:name "status" :type string :required t)))
         (tag (org-supertag-tag-create tag-id :fields fields)))
    
    ;; 2. 验证标签创建
    (should (equal tag tag-id))
    (let ((stored-tag (org-supertag-db-get tag-id)))
      (should stored-tag)
      (should (eq (plist-get stored-tag :type) :tag))
      (should (equal (plist-get stored-tag :id) tag-id))
      (should (equal (plist-get stored-tag :fields) fields)))
    
    ;; 3. 创建节点并应用标签
    (let ((node-id "test-node"))
      (org-supertag-db-add node-id
                          (list :type :node
                                :id node-id
                                :title "Test Node"
                                :file-path "/test/path"
                                :pos 1
                                :olp nil
                                :level 1))
      
      ;; 4. 应用标签到节点
      (org-supertag-tag-apply tag-id node-id
                             '(("status" . "active")))
      
      ;; 5. 验证节点-标签关系
      (should (member node-id (org-supertag-tag-get-nodes tag-id)))
      
      ;; 6. 验证字段值
      (message "Database links: %S" (ht-items org-supertag-db--link))
      (let ((field-value (org-supertag-field-db-get-value node-id "status" tag-id)))
        (message "Field value: %S" field-value)
        (should (equal field-value "active"))))))

(provide 'org-supertag-tag)
