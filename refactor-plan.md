



# org-supertag 重构工作计划

## 1. 数据结构重构
**涉及文件**: org-supertag-db.el

### 1.1 简化核心数据存储
- [新增] 定义新的核心变量:
```elisp
(defvar org-supertag-fields (ht-create))
(defvar org-supertag-tags (ht-create))
(defvar org-supertag-node-tags (ht-create))
(defvar org-supertag-tag-fields (ht-create))
```

- [删除] 移除旧变量:

```15:25:org-supertag-db.el
(defcustom org-supertag-db-file
  (org-supertag-data-file "db.el")
  "数据库文件路径."
  :type 'file
  :group 'org-supertag)

(defcustom org-supertag-db-backup-directory
  (org-supertag-data-file "backups")
  "数据库备份目录."
  :type 'directory
  :group 'org-supertag)
```


### 1.2 重构基础操作函数
- [修改] `org-supertag-db-put` -> `org-supertag-db-create-entity`
  - 简化参数
  - 移除类型检查
  
- [修改] `org-supertag-db-get` -> `org-supertag-db-get-entity`
  - 增加实体类型参数
  - 增加错误处理

- [删除] 移除复杂的关系处理函数:

```150:200:org-supertag-db.el
...

(defun org-supertag-db-remove (id)
  "删除实体及其相关的所有关系."
  (when (org-supertag-db-exists-p id)
    ;; 删除实体
    (ht-remove! org-supertag-db--entities id)
    ;; 删除相关的关系
    (let (to-remove)
      (ht-map (lambda (k v)
                (when (or (equal id (nth 1 v))
                         (equal id (nth 2 v)))
                  (push k to-remove)))
              org-supertag-db--relations)
      (dolist (k to-remove)
        (ht-remove! org-supertag-db--relations k)))
    ;; 删除相关的字段值
    (let ((prefix (concat id ":")))
      (ht-map (lambda (k _)
                (when (string-prefix-p prefix k)
                  (ht-remove! org-supertag-db--field-values k)))
              org-supertag-db--field-values))
    t))

(defun org-supertag-db-unlink (type from to)
  "删除特定关系."
  (let ((rel-id (format "%s:%s->%s" type from to)))
    (when (ht-contains-p org-supertag-db--relations rel-id)
      (ht-remove! org-supertag-db--relations rel-id)
      t)))

;;; 字段值操作

(defun org-supertag-db-set-field-value (field-id node-id value)
  "设置字段值."
  (let ((key (format "%s:%s" field-id node-id)))
    (ht-set! org-supertag-db--field-values key value)))

(defun org-supertag-db-get-field-value (field-id node-id)
  "获取字段值."
  (let ((key (format "%s:%s" field-id node-id)))
    (ht-get org-supertag-db--field-values key)))

(defun org-supertag-db-remove-field-value (field-id node-id)
```


## 2. 字段系统重构

### 2.1 字段定义简化
- [新增] `org-supertag-define-field`
  - 字段名称作为唯一标识
  - 添加字段类型验证
  - 添加字段属性验证

- [删除] 移除字段实例相关代码:

```250:300:org-supertag-db.el
            org-supertag-db--relations)
    (nreverse results)))

;;; 新增函数：按属性查询
(defun org-supertag-db-find-by-props (props)
  "查找满足属性条件的实体.
PROPS 是一个 plist，包含要匹配的属性."
  (let ((pred (lambda (_k v)
                (cl-loop for (key value) on props by #'cddr
                         always (equal (plist-get v key) value)))))
    (org-supertag-db-find pred)))

;;; 新增函数：按类型查询（常用场景的便捷函数）
(defun org-supertag-db-find-by-type (type)
  "查找指定类型的实体.
TYPE 是实体类型，如 :template, :tag 等."
  (org-supertag-db-find-by-props `(:type ,type)))

;;; 持久化支持

(defun org-supertag-db-get-entities ()
  "获取所有实体的哈希表."
  org-supertag-db--entities)

(defun org-supertag-db-get-entities-alist ()
  "获取所有实体的关联列表 ((id . props) ...)."
  (ht-items org-supertag-db--entities))

(defun org-supertag-db-find-entities (type)
  "查找指定类型的所有实体.
TYPE 是实体类型"
  (let (results)
    (ht-map (lambda (id props)
              (when (eq (plist-get props :type) type)
                (push id results)))
            org-supertag-db--entities)
    results))

;; 获取所有关系
(defun org-supertag-db-get-all-relations ()
  "获取所有关系.
返回 ((type from to props) ...) 形式的列表"
  (let (results)
    (ht-map (lambda (k v)
              (push v results))
            org-supertag-db--relations)
    results))

(provide 'org-supertag-db)
;;; org-supertag-db.el ends here

```


### 2.2 字段值处理
- [新增] `org-supertag-set-field-value`
  - 直接使用 org properties
  - 添加值验证
  - 同步更新数据库

## 3. 标签系统重构

### 3.1 标签定义
- [新增] `org-supertag-create-tag`
  - 生成唯一标签 ID
  - 设置标签属性
  - 关联字段定义

- [修改] 标签-字段关联:

```320:350:org-supertag-db.el

```


### 3.2 标签应用
- [新增] `org-supertag-apply-tag`
  - 添加标签到节点
  - 处理字段初始化
  - 更新节点显示

## 4. 节点系统重构

### 4.1 节点标识
- [修改] 使用 org-id 作为唯一标识
- [删除] 移除自定义节点 ID 生成

### 4.2 节点操作
- [新增] `org-supertag-ensure-node`
  - 确保节点有 ID
  - 初始化节点数据
  - 建立基础关联

## 5. 查询系统重构

### 5.1 基础查询
- [新增] `org-supertag-find-nodes-by-tag`
- [新增] `org-supertag-find-nodes-by-field`
- [新增] `org-supertag-find-tags-by-field`

### 5.2 高级查询
- [新增] `org-supertag-query-nodes`
  - 支持复杂条件
  - 支持排序和分组
  - 支持字段过滤

## 6. 持久化系统重构

### 6.1 数据存储
- [修改] `org-supertag-db-save`
  - 简化数据格式
  - 优化存储效率
  - 增加错误处理

### 6.2 数据加载
- [修改] `org-supertag-db-load`
  - 增加版本检查
  - 优化加载性能
  - 增加数据验证

## 7. API 清理

### 7.1 移除废弃 API
- [删除] 所有模板相关 API
- [删除] 复杂的关系处理 API
- [删除] 冗余的工具函数

### 7.2 统一 API 风格
- [修改] 统一命名规范
- [修改] 统一参数顺序
- [修改] 统一返回值格式

这个计划可以保存为 `refactor-plan.md` 用于追踪进度。每完成一项就可以在清单中打勾。需要我详细展开某个部分吗？


# org-supertag 重构计划 v2

## 1. 核心数据结构重构

### 1.1 实体定义 [org-supertag-db.el]
- [新增] 核心变量定义：
```elisp
;; 实体存储
(defvar org-supertag-db--fields (ht-create)
  "字段定义存储 field-name -> plist.")

(defvar org-supertag-db--field-types (ht-create)
  "字段类型存储 type-name -> plist.")

(defvar org-supertag-db--tags (ht-create)
  "标签存储 tag-id -> plist.")

(defvar org-supertag-db--nodes (ht-create)
  "节点存储 node-id -> plist.")

;; 关系存储
(defvar org-supertag-db--relations (ht-create)
  "关系存储 rel-id -> (type from to props).")
```

- [删除] 移除旧的实体存储：
  - `org-supertag-db--entities`
  - `org-supertag-db--field-values`

### 1.2 基础操作函数 [org-supertag-db.el]
- [新增] 实体操作函数：
```elisp
(defun org-supertag-db-put-field (name props))
(defun org-supertag-db-put-field-type (name props))
(defun org-supertag-db-put-tag (id props))
(defun org-supertag-db-put-node (id props))
```

- [新增] 关系操作函数：
```elisp
(defun org-supertag-db-link (type from to &optional props))
(defun org-supertag-db-unlink (type from to))
(defun org-supertag-db-get-links (type from))
```

## 2. 字段系统实现

### 2.1 字段定义 [org-supertag-field.el]
- [新增] 字段定义函数：
```elisp
(defun org-supertag-define-field (name &rest props)
  "定义新字段."
  (when (org-supertag-db-get-field name)
    (user-error "Field %s already exists" name))
  (org-supertag-db-put-field name props))

(defun org-supertag-define-field-type (name &rest props)
  "定义字段类型."
  (when (org-supertag-db-get-field-type name)
    (user-error "Field type %s already exists" name))
  (org-supertag-db-put-field-type name props))
```

### 2.2 字段值处理 [org-supertag-field.el]
- [新增] 字段值操作函数：
```elisp
(defun org-supertag-get-field-value (node-id field-name))
(defun org-supertag-set-field-value (node-id field-name value))
(defun org-supertag--validate-field-value (field-name value))
```

## 3. 标签系统实现

### 3.1 标签定义 [org-supertag-tag.el]
- [新增] 标签操作函数：
```elisp
(defun org-supertag-create-tag (name &rest props))
(defun org-supertag-delete-tag (tag-id))
(defun org-supertag-update-tag (tag-id &rest props))
```

### 3.2 标签-字段关联 [org-supertag-tag.el]
- [新增] 关联操作函数：
```elisp
(defun org-supertag-add-field-to-tag (tag-id field-name))
(defun org-supertag-remove-field-from-tag (tag-id field-name))
(defun org-supertag-get-tag-fields (tag-id))
```

## 4. 节点系统实现

### 4.1 节点管理 [org-supertag-node.el]
- [新增] 节点操作函数：
```elisp
(defun org-supertag-ensure-node (node-id))
(defun org-supertag-delete-node (node-id))
(defun org-supertag-update-node (node-id &rest props))
```

### 4.2 节点-标签关联 [org-supertag-node.el]
- [新增] 关联操作函数：
```elisp
(defun org-supertag-add-tag-to-node (node-id tag-id))
(defun org-supertag-remove-tag-from-node (node-id tag-id))
(defun org-supertag-get-node-tags (node-id))
```

## 5. 用户交互实现

### 5.1 标签操作 [org-supertag.el]
- [新增] 交互函数：
```elisp
(defun org-supertag-add-tag-at-point ())
(defun org-supertag-remove-tag-at-point ())
(defun org-supertag-edit-tag-at-point ())
```

### 5.2 字段操作 [org-supertag.el]
- [新增] 交互函数：
```elisp
(defun org-supertag-edit-field-at-point ())
(defun org-supertag-set-field-at-point ())
```

## 6. 持久化实现

### 6.1 数据存储 [org-supertag-db.el]
- [修改] 存储函数：
```elisp
(defun org-supertag-db-save ())
(defun org-supertag-db-load ())
```

### 6.2 数据同步 [org-supertag-db.el]
- [新增] 同步函数：
```elisp
(defun org-supertag-db-sync ())
(defun org-supertag-db-merge ())
```

这个计划：
1. 完全基于新的数据结构设计
2. 清晰划分了各个模块的职责
3. 保持了简单而强大的 API
4. 提供了完整的功能支持
5. 便于分步实现和测试

你觉得这个计划如何？需要调整或补充吗？



## 实体和关系设计









对，让我们重新梳理实体和关系的设计：

1. **核心实体定义**：
```elisp
;; Field 直接用名称作为标识
(defvar org-supertag-db--fields (ht-create)
  "字段定义存储 field-name -> plist.")

;; Field Type 直接用名称作为标识
(defvar org-supertag-db--field-types (ht-create)
  "字段类型存储 type-name -> plist.")

;; Tag 需要 ID
(defvar org-supertag-db--tags (ht-create)
  "标签存储 tag-id -> plist.")

;; Node 使用 org-id
(defvar org-supertag-db--nodes (ht-create)
  "节点存储 node-id -> plist.")
```

2. **基础操作函数**：
```elisp
;; Field 操作
(defun org-supertag-define-field (name &rest props)
  "定义字段.
NAME 是字段名称（唯一标识）"
  (when (ht-get org-supertag-db--fields name)
    (user-error "Field %s already exists" name))
  (ht-set! org-supertag-db--fields name props))

;; Field Type 操作
(defun org-supertag-define-field-type (name &rest props)
  "定义字段类型.
NAME 是类型名称（唯一标识）"
  (when (ht-get org-supertag-db--field-types name)
    (user-error "Field type %s already exists" name))
  (ht-set! org-supertag-db--field-types name props))

;; Tag 操作
(defun org-supertag-create-tag (name &rest props)
  "创建标签.
返回标签 ID"
  (let ((id (org-id-uuid)))
    (ht-set! org-supertag-db--tags id
             (append (list :name name) props))
    id))

;; Node 操作
(defun org-supertag-ensure-node (node-id)
  "确保节点存在于数据库中."
  (unless (ht-get org-supertag-db--nodes node-id)
    (ht-set! org-supertag-db--nodes node-id
             (list :type :node))))
```

3. **关系定义**：
```elisp
;; 关系类型
(defconst org-supertag-relation-types
  '(:tag-field     ; 标签使用的字段 (tag-id -> field-name)
    :node-tag))    ; 节点应用的标签 (node-id -> tag-id)

;; 关系存储
(defvar org-supertag-db--relations (ht-create)
  "关系存储 rel-id -> (type from to props).")
```

4. **主要交互函数**：
```elisp
(defun org-supertag-add-tag-at-point ()
  "为当前节点添加标签."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "必须在标题处"))
  
  (let* ((node-id (org-id-get-create))
         ;; 确保节点存在
         (_ (org-supertag-ensure-node node-id))
         ;; 选择标签
         (tag-choices (ht-map (lambda (id props)
                               (cons (plist-get props :name) id))
                             org-supertag-db--tags))
         (tag-name (completing-read "选择标签: " 
                                  (mapcar #'car tag-choices)))
         (tag-id (cdr (assoc tag-name tag-choices))))
    
    ;; 添加标签到标题
    (let* ((element (org-element-at-point))
           (title (org-element-property :raw-value element))
           (new-title (concat title " #" tag-name)))
      (org-edit-headline new-title))
    
    ;; 创建节点-标签关系
    (org-supertag-db-link :node-tag node-id tag-id)
    
    ;; 处理字段
    (let ((fields (org-supertag-db-get-links :tag-field tag-id)))
      (dolist (field-name fields)
        (let* ((field-props (ht-get org-supertag-db--fields field-name))
               (field-type (plist-get field-props :type))
               (type-props (ht-get org-supertag-db--field-types field-type))
               (current-value (org-entry-get nil field-name))
               (value (org-supertag--read-field-value 
                      field-name field-props type-props current-value)))
          ;; 设置属性
          (org-entry-put nil field-name value))))))
```

这样的设计：
1. Field 和 Field Type 使用名称作为唯一标识
2. Tag 使用 UUID 作为标识
3. Node 使用 org-id
4. 简化了数据结构
5. 保持了实体和关系的清晰性

你觉得这个设计如何？











1. **字段定义和值的唯一性**：
```elisp
;; 全局字段定义
(defvar org-supertag-field-definitions
  (make-hash-table :test 'equal)
  "存储全局字段定义.
key 是字段名称，value 是字段定义")

(defun org-supertag-define-field (name &rest props)
  "定义一个全局字段.
NAME 是字段名称
PROPS 是字段属性"
  (puthash name props org-supertag-field-definitions))

;; 预定义字段
(org-supertag-define-field 
 "Type" 
 :type 'select
 :options '("Book" "Article" "Movie" "TV Series" "Podcast"))

(org-supertag-define-field
 "Status"
 :type 'select
 :options '("Backlog" "In progress" "Finished" "Next up" "Snoozed"))
```
```


2. **标签使用字段**：
```elisp
(defun org-supertag--create-tag-with-fields (tag-name field-names)
  "创建标签并关联字段.
FIELD-NAMES 是字段名称列表"
  (dolist (name field-names)
    (unless (gethash name org-supertag-field-definitions)
      (user-error "未定义的字段: %s" name)))
  ;; 创建标签并关联字段
  ...)

;; 当用户选择字段时，使用已定义的字段
(defun org-supertag--select-fields ()
  "选择要使用的字段."
  (let* ((available-fields 
          (hash-table-keys org-supertag-field-definitions))
         (selected-fields
          (completing-read-multiple 
           "选择字段: " available-fields)))
    selected-fields))
```




2. **字段值的统一处理**：
```elisp
(defun org-supertag--read-field-value (field-name)
  "读取字段值，根据字段定义提供适当的输入方式."
  (let* ((field-def (gethash field-name org-supertag-field-definitions))
         (type (plist-get field-def :type))
         (current-value (org-entry-get nil field-name)))
    (pcase type
      ('select
       (let ((options (plist-get field-def :options)))
         (completing-read (format "%s [%s]: " field-name current-value)
                         options nil t nil nil current-value)))
      (_ (read-string (format "%s [%s]: " field-name current-value)
                    current-value)))))

(defun org-supertag--apply-tag-fields (tag-name)
  "应用标签的字段到当前节点."
  (let* ((template-id (org-supertag-get-linked tag-name :tag-template))
         (template (when template-id 
                    (org-supertag-get-template template-id)))
         (fields (when template 
                  (org-supertag-template-fields template)))
         (node-id (org-id-get-create)))
    
    (when (and template fields node-id)
      (dolist (field fields)
        (let* ((field-name (plist-get field :name))
               (value (org-supertag--read-field-value field-name)))
          ;; 设置全局唯一的字段值
          (org-entry-put nil field-name value))))))
```

3. **结果展示**：
```org
* 一个节点 #bookmark1 #bookmark2
:PROPERTIES:
:ID: node-123
:Type: Book        ; 全局唯一字段
:Status: Backlog   ; 全局唯一字段
:END:
```

这样的设计：
- 字段定义和字段值都是全局唯一的
- 不同标签共享相同的字段值
- 字段值的修改会同步更新所有使用该字段的标签

这样更符合 Tana 的设计理念，你觉得如何？
