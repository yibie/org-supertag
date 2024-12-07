;;; org-supertag-api.el --- API layer for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供高层 API 接口

;;; Code:
(require 'cl-lib)
(require 'org-supertag-db)
(require 'org-supertag-types)

(cl-defstruct org-supertag-template
  id
  tag-name
  display-name
  description
  fields
  icon
  color)

;;; API 函数 - 用于程序化操作

(defun org-supertag-create-node (id props)
  "创建一个 org 节点实体.
ID 是节点唯一标识
PROPS 是节点属性"
  (org-supertag-db-put id (plist-put props :type :node)))

(defun org-supertag-update-node (id props)
  "更新节点.
ID 是节点唯一标识
PROPS 是要更新的属性"
  (org-supertag-node-update-metadata id props))

(defun org-supertag-create-tag (id &optional props)
  "创建一个标签实体."
  (org-supertag-db-put id (plist-put props :type :tag)))

(defun org-supertag-create-field (id &optional props)
  "创建一个字段实体."
  (org-supertag-db-put id (plist-put props :type :field)))

;; 关系操作
(defun org-supertag-add-tag (node-id tag-id)
  "为节点添加标签."
  (org-supertag-db-link :node-tag node-id tag-id))

(defun org-supertag-remove-tag (node-id tag-id)
  "移除节点的标签."
  (org-supertag-db-unlink :node-tag node-id tag-id))

(defun org-supertag-set-field (node-id field-id value)
  "设置节点的字段值.
NODE-ID: 节点ID
FIELD-ID: 字段ID
VALUE: 字段值
返回 t 如果设置成功，nil 如果失败"
  (when (and (org-supertag-db-exists-p node-id)
             (org-supertag-db-exists-p field-id))
    (org-supertag-db-link :node-field node-id field-id)
    (org-supertag-db-set-field-value field-id node-id value)
    t))

;; 查询操作
(defun org-supertag-get-node-tags (node-id)
  "获取节点的所有标签."
  (mapcar #'car (org-supertag-db-get-links :node-tag node-id)))

(defun org-supertag-get-node-fields (node-id)
  "获取节点的所有字段."
  (mapcar #'car (org-supertag-db-get-links :node-field node-id)))

(defun org-supertag-get-field (field-id)
  "获取字段定义.
FIELD-ID: 字段ID
返回字段的完整定义，如果字段不存在返回 nil"
  (when (org-supertag-db-exists-p field-id)
    (let ((field-def (org-supertag-db-get field-id)))
      (when (eq (plist-get field-def :type) :field)
        field-def))))

;; 批量操作
(defun org-supertag-batch-add-tags (node-id tag-ids)
  "批量为节点添加多个标签.
NODE-ID: 节点ID
TAG-IDS: 标签ID列表
返回 t 如果所有标签都添加成功，nil 如果有任何失败"
  (message "Debug - Adding tags %S to node %S" tag-ids node-id)
  (when (org-supertag-db-exists-p node-id)
    (let ((success t))
      (dolist (tag-id tag-ids success)
        (message "Debug - Adding tag %S" tag-id)
        (let ((result (org-supertag-add-tag node-id tag-id)))
          (message "Debug - Add tag result: %S" result)
          (unless result
            (setq success nil)))))))

(defun org-supertag-batch-set-fields (node-id field-values)
  "批量设置多个字段值.
NODE-ID: 节点ID
FIELD-VALUES: ((field-id . value) ...) 形式的列表
返回 t 如果所有字段都设置成功，nil 如果有任何失败"
  (message "Debug - Setting fields %S for node %S" field-values node-id)
  (when (org-supertag-db-exists-p node-id)
    (let ((success t))
      (dolist (field-value field-values success)
        (message "Debug - Setting field %S = %S" (car field-value) (cdr field-value))
        (let ((result (org-supertag-set-field node-id 
                                             (car field-value)
                                             (cdr field-value))))
          (message "Debug - Set field result: %S" result)
          (unless result
            (setq success nil)))))))

;; 继承支持
(defun org-supertag-get-inherited-tags (node-id)
  "获取节点继承的所有标签(包括父节点的标签)."
  (let ((tags (org-supertag-get-node-tags node-id))
        (parent-id (org-supertag-get-parent-id node-id)))
    (while parent-id
      (setq tags (append tags (org-supertag-get-node-tags parent-id)))
      (setq parent-id (org-supertag-get-parent-id parent-id)))
    (delete-duplicates tags :test #'equal)))

(defun org-supertag-create-entity (id props)
  "创建一个实体.
ID 是实体的唯一标识
PROPS 是实体的属性"
  (let ((type (plist-get props :type)))
    (unless type
      (error "Entity must have a :type property"))
    
    ;; 根据实体类型调用相应的创建函数
    (pcase type
      (:node (org-supertag-node-create id props))
      (:tag (org-supertag-tag-create id props))
      (:field (org-supertag-field-create id props))
      (:group (org-supertag-group-create id props))
      (_ (error "Unknown entity type: %s" type)))))

(defun org-supertag-get-entity (id)
  "获取实体.
ID 是实体的唯一标识"
  (org-supertag-db-get id))

(defun org-supertag-update-entity (id props)
  "更新实体.
ID 是实体ID
PROPS 是实体属性"
  (message "Debug - Updating entity %s with props: %S" id props)
  (when (and id props)
    (org-supertag-db-put id props)))

(defun org-supertag-remove-entity (id)
  "删除实体.
ID 是实体的唯一标识"
  (when-let ((entity (org-supertag-get-entity id)))
    (let ((type (plist-get entity :type)))
      (pcase type
        (:node (org-supertag-node-remove id))
        (:tag (org-supertag-tag-remove id))
        (:field (org-supertag-field-remove id))
        (:group (org-supertag-group-remove id))
        (_ (error "Unknown entity type: %s" type))))))

(defun org-supertag-get-entities ()
  "获取所有实体的哈希表."
  (org-supertag-db-get-entities))

(defun org-supertag-find-entities (type)
  "查找指定类型的所有实体.
TYPE 是实体类型"
  (org-supertag-db-find-entities type))

;; 关系操作 API

(defun org-supertag-get-relations (type from)
  "获取实体的关系列表.
TYPE 是关系类型
FROM 是源实体
返回 ((to props) ...) 形式的列表"
  (org-supertag-db-get-links type from))

(defun org-supertag-get-relations-reversed (type to)
  "获取指向实体的关系列表.
TYPE 是关系类型
TO 是目标实体
返回 ((from props) ...) 形式的列表"
  (let (results)
    (dolist (rel (org-supertag-db-get-all-relations))
      (when (and (equal type (car rel))
                (equal to (nth 2 rel)))
        (push (list (nth 1 rel) (nth 3 rel)) results)))
    results))

(defun org-supertag-add-relation (type from to &optional props)
  "添加关系.
TYPE 是关系类型
FROM 是源实体
TO 是目标实体
PROPS 是关系属性"
  (org-supertag-db-link type from to props))

(defun org-supertag-remove-relation (type from to)
  "移除关系.
TYPE 是关系类型
FROM 是源实体
TO 是目标实体"
  (org-supertag-db-unlink type from to))

(defun org-supertag-get-relation (type from to)
  "获取特定关系.
TYPE 是关系类型
FROM 是源实体
TO 是目标实体
返回关系属性或 nil"
  (cadr (car (org-supertag-get-relations type from))))

(defun org-supertag-get-relations-v2 (type from)
  "获取实体的关系列表（新版本）.
TYPE 是关系类型
FROM 是源实体
返回 ((to props) ...) 形式的列表"
  (org-supertag-db-get-links type from))

(defun org-supertag-get-relations-reversed-v2 (type to)
  "获取指向实体的关系列表（新版本）.
TYPE 是关系类型
TO 是目标实体
返回 ((from props) ...) 形式的列表"
  (let (results)
    (dolist (rel (org-supertag-db-get-all-relations))
      (when (and (equal type (car rel))
                (equal to (nth 2 rel)))
        (push (list (nth 1 rel) (nth 3 rel)) results)))
    results))


;;; 模板系统初始化
(defun org-supertag-tag-templates-init ()
  "初始化模板系统."
  (unless org-supertag-tag-templates
    (setq org-supertag-tag-templates (make-hash-table :test 'equal)))
  (when (file-exists-p org-supertag-templates-file)
    (load org-supertag-templates-file nil 'nomessage)))

;;; 内部辅助函数
(defun org-supertag--store-template (plist)
  "存储模板数据.
PLIST 是模板的属性列表."
  (let ((id (plist-get plist :id)))
    (if (string-empty-p id)
        (error "Template ID cannot be empty")
      (puthash id plist org-supertag--tag-templates)
      ;; 添加调试信息
      (message "Stored template: %S" plist)
      (message "Current hash table: %S" (org-supertag--hash-to-alist org-supertag--tag-templates))
      t)))

(defun org-supertag--find-template-by-id (id)
  "通过 ID 查找模板."
  (org-supertag-db-get id))

(defun org-supertag--find-templates-by-type ()
  "查找所有模板."
  (org-supertag-db-find :type :template))

(defun org-supertag--link-template-tag (template-id tag-name)
  "建立模板和标签的关联."
  (org-supertag-db-link :template-tag template-id tag-name)
  (org-supertag-db-link :tag-template tag-name template-id))

(defun org-supertag--unlink-template-tag (template-id tag-name)
  "解除模板和标签的关联."
  (org-supertag-db-unlink :template-tag template-id tag-name)
  (org-supertag-db-unlink :tag-template tag-name template-id))

;;; 公共 API
(defun org-supertag-get-template (id)
  "获取模板数据.
ID 是模板的唯一标识符."
  (when-let ((plist (org-supertag--find-template-by-id id)))
    (org-supertag-template-from-plist plist)))

(defun org-supertag-find-templates ()
  "获取所有模板."
  (let ((template-plists (org-supertag--find-templates-by-type)))
    (mapcar #'org-supertag-template-from-plist template-plists)))

(defun org-supertag-get-template-tag (template-id)
  "获取模板关联的标签.
TEMPLATE-ID 是模板ID."
  (org-supertag-db-get-linked template-id :template-tag))

(defun org-supertag--generate-id ()
  "生成唯一的模板ID.
使用时间戳和随机数生成 SHA-1 哈希的前16位作为ID."
  (let* ((time-str (format-time-string "%Y%m%d%H%M%S%N"))
         (random-str (format "%04x" (random #xffff)))
         (base-str (concat time-str random-str)))
    (substring (secure-hash 'sha1 base-str) 0 16)))

(defun org-supertag-create-template (template)
  "创建或更新标签模板.
TEMPLATE 是 org-supertag-template struct."
  (unless (org-supertag-template-validate template)
    (error "Invalid template data"))
  
  ;; 如果 ID 为空，自动生成一个
  (when (string-empty-p (org-supertag-template-id template))
    (setf (org-supertag-template-id template) (org-supertag--generate-id)))
  
  (let* ((plist (org-supertag-template-to-plist template))
         (id (org-supertag-template-id template))
         (tag-name (org-supertag-template-tag-name template)))
    
    ;; 存储到内存
    (org-supertag--store-template plist)
    (org-supertag-create-tag tag-name)
    (org-supertag--link-template-tag id tag-name)
    
    ;; 保存到文件
    (org-supertag-template-save-all)
    t))

(defun org-supertag-update-template (template)
  "更新标签模板.
TEMPLATE 是 org-supertag-template struct."
  (unless (org-supertag-template-validate template)
    (error "Invalid template data"))
  (let* ((plist (org-supertag-template-to-plist template))
         (id (org-supertag-template-id template))
         (tag-name (org-supertag-template-tag-name template))
         (old-template (org-supertag-get-template id)))
    (when old-template
      (let ((old-tag-name (org-supertag-template-tag-name old-template)))
        (unless (equal tag-name old-tag-name)
          (org-supertag--unlink-template-tag id old-tag-name)
          (org-supertag-create-tag tag-name)
          (org-supertag--link-template-tag id tag-name))))
    (org-supertag--store-template plist)))

(defun org-supertag-delete-template (id)
  "删除标签模板.
ID 是模板的唯一标识符."
  (when-let ((template (org-supertag-get-template id)))
    (let ((tag-name (org-supertag-template-tag-name template)))
      (org-supertag--unlink-template-tag id tag-name)
      (org-supertag-db-remove id))))

;; Struct 和 plist 转换函数
(defun org-supertag-template-to-plist (template)
  "将 org-supertag-template struct 转换为 plist."
  (list :type :template
        :id (org-supertag-template-id template)
        :tag-name (org-supertag-template-tag-name template)
        :display-name (org-supertag-template-display-name template)
        :description (org-supertag-template-description template)
        :fields (org-supertag-template-fields template)
        :icon (org-supertag-template-icon template)
        :color (org-supertag-template-color template)))

(defun org-supertag-template-from-plist (plist)
  "将 plist 转换为 org-supertag-template struct."
  (make-org-supertag-template
   :id (plist-get plist :id)
   :tag-name (plist-get plist :tag-name)
   :display-name (plist-get plist :display-name)
   :description (plist-get plist :description)
   :fields (plist-get plist :fields)
   :icon (plist-get plist :icon)
   :color (plist-get plist :color)))

(defun org-supertag-template-validate (template)
  "验证模板数据的有效性.
TEMPLATE 是要验证的模板数据，可以是 struct 或 plist."
  (message "Validating template: %S" template)
  (if (org-supertag-template-p template)
      ;; 验证 struct 格式
      (and (stringp (org-supertag-template-id template))
           (stringp (org-supertag-template-tag-name template))
           (stringp (org-supertag-template-display-name template))
           (or (null (org-supertag-template-fields template))
               (vectorp (org-supertag-template-fields template))))
    ;; 验证 plist 格式
    (and (eq (plist-get template :type) :template)
         (stringp (plist-get template :id))
         (stringp (plist-get template :tag-name))
         (stringp (plist-get template :display-name))
         (or (null (plist-get template :fields))
             (vectorp (plist-get template :fields))))))

(defun org-supertag--hash-to-alist (hash-table)
  "将哈希表转换为关联列表."
  (let (alist)
    (maphash (lambda (k v)
               (push (cons k v) alist))
             hash-table)
    alist))

(defun org-supertag--alist-to-hash (alist)
  "将关联列表转换为哈希表."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (puthash (car pair) (cdr pair) hash))
    hash))

(provide 'org-supertag-api)
;;; org-supertag-api.el ends here