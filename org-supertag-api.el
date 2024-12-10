;;; org-supertag-api.el --- API layer for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供高层 API 接口
;; 不直接修改数据库，而是通过 API 访问和操作数据库
;; API 提供数据类型转换
;; 本文件不支持【破坏性更新】，如固有函数不足以支持功能，则实现新函数

;;; Code:
(require 'cl-lib)
(require 'org-supertag-db)
(require 'org-supertag-types)


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

    

;;------------------------------------------------------------------------------
;; 字段值 API
;;------------------------------------------------------------------------------  

(defun org-supertag-db-find-field-values-by-node (node-id)
  "获取节点的所有字段值."
  (org-supertag-db-find (lambda (k _v)
                          (string-match-p (concat ":" (regexp-quote node-id) "$")
                                        k))))

(defun org-supertag-db-find-field-values-by-field (field-id)
  "获取指定字段的所有值."
  (org-supertag-db-find (lambda (k _v)
                          (string-match-p (concat "^" (regexp-quote field-id) ":")
                                        k))))
;;------------------------------------------------------------------------------
;; 字段编辑 API
;;------------------------------------------------------------------------------

(defun org-supertag-update-field (tag-name field-name new-props)
  "更新标签中的字段定义.
TAG-NAME 是标签名称
FIELD-NAME 是字段名称
NEW-PROPS 是新的字段属性"
  (when-let* ((tag-entity (org-supertag-get-entity tag-name))
              (current-fields (plist-get tag-entity :fields)))
    ;; 更新字段列表
    (let ((updated-fields
           (mapcar (lambda (f)
                    (if (string= (plist-get f :name) field-name)
                        new-props
                      f))
                  current-fields)))
      ;; 更新标签实体
      (org-supertag-update-entity 
       tag-name 
       (list :type :tag
             :fields updated-fields))
      t)))

(defun org-supertag-get-field-definition (tag-name field-name)
  "获取标签中字段的定义.
TAG-NAME 是标签名称
FIELD-NAME 是字段名称
返回字段的完整定义，如果不存在返回 nil"
  (when-let* ((tag-entity (org-supertag-get-entity tag-name))
              (fields (plist-get tag-entity :fields)))
    (cl-find field-name fields
             :key (lambda (f) (plist-get f :name))
             :test #'string=)))

(defun org-supertag-get-tag-fields (tag-name)
  "获取标签的所有字段定义.
TAG-NAME 是标签名称
返回字段定义列表"
  (when-let ((tag-entity (org-supertag-get-entity tag-name)))
    (plist-get tag-entity :fields)))

;;------------------------------------------------------------------------------
;; 标签系统 API
;;------------------------------------------------------------------------------
;; 保持原有函数不变，增加新的实现
(defun org-supertag-find-templates-by-type ()
  "获取所有模板（使用类型查询）."
  (let ((template-plists (mapcar #'cdr 
                                (org-supertag-db-find-by-type :template))))
    (mapcar #'org-supertag-template-from-plist template-plists)))

;; 更新获取字段函数，使用新的查询函数
(defun org-supertag-get-all-fields ()
  "获取所有模板中定义的字段列表."
  (let (all-fields)
    ;; 使用新的查询函数
    (dolist (template (org-supertag-find-templates-by-type))
      (when-let ((fields (org-supertag-template-fields template)))
        (dolist (field fields)
          (let* ((tag-name (org-supertag-template-tag-name template))
                 (field-name (plist-get field :name))
                 (full-name (concat field-name)))
            (push full-name all-fields)))))
    ;; 返回去重后的字段列表
    (delete-dups (nreverse all-fields))))
    
(defun org-supertag-get-linked (from type)
  "获取与 FROM 通过 TYPE 关系关联的实体.
FROM 是源实体的标识符
TYPE 是关系类型"
  (when-let ((links (org-supertag-db-get-links type from)))
    (car (car links))))
    
(defun org-supertag-get-all-tags ()
  "获取所有已定义的标签列表."
  (message "Debug - Starting tag search...")
  (let* ((entities (org-supertag-find-entities :tag))
         (_ (message "Debug - Found tag entities: %S" entities))
         (tags (if (listp (car entities))
                  (mapcar #'car entities)
                (if (stringp entities)
                    (list entities)
                  entities))))
    (message "Debug - Extracted tag names: %S" tags)
    tags))

;;------------------------------------------------------------------------------
;; 节点 API
;;------------------------------------------------------------------------------

(defun org-supertag-get-all-node-ids ()
  "获取所有节点的 ID 列表."
  (message "Debug - Starting node ID search...")
  (let* ((entities (org-supertag-find-entities :node))
         (_ (message "Debug - Found node entities: %S" entities))
         (node-ids (if (listp (car entities))
                      (mapcar #'car entities)
                    (if (stringp entities)
                        (list entities)
                      entities))))
    (message "Debug - Extracted node IDs: %S" node-ids)
    node-ids))


(provide 'org-supertag-api)
;;; org-supertag-api.el ends here