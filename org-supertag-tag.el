;;; org-supertag-tag.el --- Tag relation management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供标签关系管理功能
;; 核心原则：通过关系连接实体，使用 type、from、to 来表达关系
;; 
;; 关系类型：
;; - :tag-extend   ; 标签继承关系
;; - :tag-contain  ; 标签组合关系
;; - :tag-ref      ; 标签引用关系
;; - :tag-excl     ; 标签互斥关系

;;; Code:

(require 'org-supertag-api)
(require 'org-supertag-types)
(require 'org-supertag-cache)
(require 'org-supertag-group)
(require 'org-supertag-parser)
(require 'org-supertag-field)
(require 'cl-lib)

;;; 标签字段定义

(defun org-supertag-tag-set-fields (tag-name fields)
  "设置标签的字段定义.
TAG-NAME 是标签名称
FIELDS 是字段定义列表"
  (org-supertag-update-entity 
   tag-name 
   (list :type :tag
         :fields fields)))

(defun org-supertag-tag-get-fields (tag-name &optional include-inherited)
  "获取标签的字段定义.
TAG-NAME 是标签名称
当 INCLUDE-INHERITED 非空时，包含继承的字段"
  (let ((direct-fields (plist-get (org-supertag-get-entity tag-name) :fields)))
    (if (not include-inherited)
        direct-fields
      (let* ((parent (org-supertag-tag-get-parent tag-name))
             (parent-fields (when parent 
                            (org-supertag-tag-get-fields parent t)))
             (inherit-spec (org-supertag-get-relation :tag-extend tag-name parent)))
        (org-supertag-tag--merge-fields direct-fields parent-fields inherit-spec)))))

(defun org-supertag-tag--merge-fields (direct-fields parent-fields inherit-spec)
  "合并直接字段和继承字段.
DIRECT-FIELDS 是直接定义的字段
PARENT-FIELDS 是父标签的字段
INHERIT-SPEC 是继承规范"
  (let* ((inherit-type (plist-get inherit-spec :inherit))
         (inherit-list (plist-get inherit-spec :inherit-fields))
         (exclude-list (plist-get inherit-spec :exclude-fields)))
    (append
     direct-fields
     (cl-remove-if
      (lambda (field)
        (let ((field-name (plist-get field :name)))
          (or
           ;; 已经在直接字段中定义
           (cl-find field-name direct-fields
                    :key (lambda (f) (plist-get f :name))
                    :test #'string=)
           ;; 根据继承类型过滤
           (pcase inherit-type
             (:inherit-none t)  ; 不继承任何字段
             (:inherit-all nil) ; 继承所有字段
             (:inherit-list    ; 继承指定字段
              (not (member field-name inherit-list)))
             (:inherit-except  ; 继承除了指定字段外的所有字段
              (member field-name exclude-list))))))
      parent-fields))))

;;; 字段验证

(defun org-supertag-tag-validate-field (field-def value)
  "验证字段值是否有效.
FIELD-DEF 是字段定义
VALUE 是要验证的值
返回 (valid . message) cons cell"
  (let ((type (plist-get field-def :type))
        (validate (plist-get field-def :validate))
        (required (plist-get field-def :required))
        (values (plist-get field-def :values)))
    (cond
     ;; 必需字段检查
     ((and required (not value))
      (cons nil (format "字段 %s 是必需的" (plist-get field-def :name))))
     ;; 值列表检查
     ((and values value (not (member value values)))
      (cons nil (format "值 %s 不在允许的值列表中: %s" value values)))
     ;; 自定义验证
     (validate
      (condition-case err
          (if (functionp validate)
              (funcall validate value)
            (eval `(let ((val ,value)) ,validate)))
        (error
         (cons nil (error-message-string err)))))
     ;; 默认有效
     (t (cons t nil)))))

(defun org-supertag-tag-validate-fields (tag-name values)
  "验证标签的字段值.
TAG-NAME 是标签名称
VALUES 是字段值的 alist"
  (let ((fields (org-supertag-tag-get-fields tag-name t))
        (results nil))
    (dolist (field fields)
      (let* ((field-name (plist-get field :name))
             (value (alist-get field-name values nil nil #'string=))
             (validation (org-supertag-tag-validate-field field value)))
        (unless (car validation)
          (push (cons field-name (cdr validation)) results))))
    (nreverse results)))

;;; 关系规范

(defconst org-supertag-tag-relation-specs
  '(:tag-extend   ; 继承关系
    (:props (:fields           ; 可继承的字段列表
            :inherit          ; 继承类型
            :inherit-fields   ; 要继承的字段列表
            :exclude-fields)  ; 要排除的字段列表
     :constraints (:no-circle)) ; 约束：不允许循环继承
    
    :tag-contain  ; 组合关系
    (:props (:access-fields)     ; 可访问的字段列表
     :constraints (:no-self))    ; 约束：不能自包含
    
    :tag-ref      ; 引用关系
    (:props (:ref-field         ; 引用字段
            :target-fields)     ; 目标可访问字段
     :constraints (:no-self))   ; 约束：不能自引用
    
    :tag-excl     ; 互斥关系
    (:props (:group)            ; 互斥组
     :constraints (:symmetric   ; 约束：对称性
                  :no-self)))   ; 约束：不能自互斥
  "标签关系规范.")

;;; 验证函数

(defun org-supertag-tag--validate-relation (type from to props)
  "验证标签关系是否满足规范.
TYPE 是关系类型
FROM 是源标签
TO 是目标标签
PROPS 是关系属性"
  (when-let* ((spec (plist-get org-supertag-tag-relation-specs type))
              (constraints (plist-get spec :constraints)))
    ;; 检查约束
    (and (if (memq :no-circle constraints)
             (not (org-supertag-tag--check-circular type from to))
           t)
         (if (memq :no-self constraints)
             (not (equal from to))
           t)
         (if (memq :symmetric constraints)
             (equal (org-supertag-get-relation type to from)
                   (org-supertag-get-relation type from to))
           t))))

(defun org-supertag-tag--validate-props (type props)
  "验证关系属性是否满足规范.
TYPE 是关系类型
PROPS 是关系属性"
  (when-let* ((spec (plist-get org-supertag-tag-relation-specs type))
              (allowed-props (plist-get spec :props)))
    (cl-every (lambda (prop)
                (memq (car prop) allowed-props))
              props)))

(defun org-supertag-tag--check-circular (type from to)
  "检查是否存在循环关系.
TYPE 是关系类型
FROM 是源标签
TO 是��标标签"
  (let ((visited (make-hash-table :test 'equal)))
    (cl-labels ((visit (current)
                  (if (equal current from)
                      t  ; 发现循环
                    (unless (gethash current visited)
                      (puthash current t visited)
                      (let ((relations (org-supertag-get-relations type current)))
                        (cl-some (lambda (rel)
                                  (visit (car rel)))
                                relations))))))
      (visit to))))

;;; 标签操作 API

(defun org-supertag-tag-create (id props)
  "创建标签.
ID 是标签的唯一标识
PROPS 是标签的属性"
  (let ((tag-props (append (copy-sequence props)
                          `(:id ,id :type :tag))))
    (org-supertag-db-put id tag-props)
    tag-props))

(defun org-supertag-tag-get (name)
  "获取标签信息.
NAME 是标签名称"
  (org-supertag-get-entity name))

(defun org-supertag-tag-update (name props)
  "更新标签属性.
NAME 是标签名称
PROPS 是新的属性"
  (org-supertag-update-entity name (plist-put props :type :tag)))

(defun org-supertag-tag-delete (name)
  "删除标签.
NAME 是标签名称"
  (org-supertag-remove-entity name))

;;; 关系操作 API

(defun org-supertag-tag-add-relation (type from to &optional props)
  "添加标签关系.
TYPE 是关系类型
FROM 是源标签
TO 是目标标签
PROPS 是关系属性"
  (when (and (org-supertag-tag--validate-relation type from to props)
             (org-supertag-tag--validate-props type props))
    (org-supertag-add-relation type from to props)))

(defun org-supertag-tag-remove-relation (type from to)
  "移除标签关系.
TYPE 是关系类型
FROM 是源标签
TO 是目标标签"
  (org-supertag-remove-relation type from to))

(defun org-supertag-tag-get-relations (tag &optional type)
  "获取标签的关系.
TAG 是标签名称
TYPE 是可选的关系类型"
  (if type
      (org-supertag-get-relations type tag)
    (let (results)
      (dolist (rel-type (mapcar #'car org-supertag-tag-relation-specs))
        (setq results (append results
                            (org-supertag-get-relations rel-type tag))))
      results)))

;;; 继承关系 API

(defun org-supertag-tag-extend (child parent)
  "设置标签继承关系.
CHILD 是子标签
PARENT 是父标签"
  (message "Debug - Setting inheritance: %s extends %s" child parent)
  (unless (org-supertag-tag-detect-inheritance-cycle child parent)
    (message "Debug - No inheritance cycle detected")
    (let ((result (org-supertag-db-link :tag-extend child parent)))
      (message "Debug - Link result: %S" result)
      t)))

(defun org-supertag-tag-detect-inheritance-cycle (child parent &optional visited)
  "检测标签继承关系中是否存在循环.
CHILD 是子标签
PARENT 是父标签
VISITED 是已访问的标签列表"
  (let ((visited (or visited (list child))))
    (cond
     ;; 如果父标签已经在访问列表中，说明有循环
     ((member parent visited) t)
     ;; 检查父标签的父标签
     (t (let ((parent-parents (org-supertag-tag-get-parents parent)))
          (cl-some (lambda (p)
                    (org-supertag-tag-detect-inheritance-cycle child p
                                                             (cons parent visited)))
                  parent-parents))))))

(defun org-supertag-tag-get-parents (tag)
  "获取标签的直接父标列表."
  (message "Debug - Getting parents for tag: %s" tag)
  (let ((links (org-supertag-db-get-links :tag-extend tag)))
    (message "Debug - Found links: %S" links)
    (let ((parents (mapcar #'car links)))
      (message "Debug - Parents: %S" parents)
      parents)))

(defun org-supertag-tag--remove-duplicates (list)
  "移除列表中的重复元素，保持顺序."
  (let ((result nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (item list)
      (unless (gethash item seen)
        (push item result)
        (puthash item t seen)))
    (nreverse result)))

(defun org-supertag-tag-get-ancestors (tag &optional visited)
  "获取标签的所有祖先标签列表.
TAG 是要查询的标签
VISITED 是已访问的标签列表，用于防止循环"
  (let ((visited (or visited (list tag)))
        ancestors)
    (dolist (parent (org-supertag-tag-get-parents tag))
      (unless (member parent visited)
        (push parent ancestors)
        (setq ancestors (append ancestors
                              (org-supertag-tag-get-ancestors 
                               parent (cons parent visited))))))
    (cl-delete-duplicates ancestors :test #'equal)))

(defun org-supertag-tag-inherits-p (child parent)
  "判断 CHILD 是否继承自 PARENT."
  (message "Debug - Checking if %s inherits from %s" child parent)
  (let ((direct-parents (org-supertag-tag-get-parents child)))
    (message "Debug - Direct parents of %s: %S" child direct-parents)
    (or (member parent direct-parents)
        (cl-some (lambda (p)
                   (when p  ; 确保父标签不是 nil
                     (message "Debug - Checking parent: %s" p)
                     (let ((result (org-supertag-tag-inherits-p p parent)))
                       (message "Debug - Result for %s inherits %s: %s" p parent result)
                       result)))
                 direct-parents))))

(defun org-supertag-tag-get-parent (tag)
  "获取标签的父标签.
TAG 是标签名称"
  (caar (org-supertag-get-relations :tag-extend tag)))

(defun org-supertag-tag-get-children (tag)
  "获取标签的子标签列表.
TAG 是标签名称"
  (mapcar #'car (org-supertag-get-relations-reversed :tag-extend tag)))

;;; 组合关系 API

(defun org-supertag-tag-contain (container component &optional props)
  "建立标签组合关系.
CONTAINER 包含 COMPONENT"
  (message "Debug - Setting container: %s contains %s" container component)
  (let ((result (org-supertag-db-link :tag-contain container component props)))
    (message "Debug - Container link result: %S" result)
    result))

(defun org-supertag-tag-get-container (tag)
  "获取包含该标签的容器标签.
TAG 是标签名称"
  (message "Debug - Getting container for tag: %s" tag)
  (let ((relations (org-supertag-get-relations-reversed-v2 :tag-contain tag)))
    (message "Debug - Container relations: %S" relations)
    (caar relations)))  ; 获取第一个容器标签

(defun org-supertag-tag-get-components (tag)
  "获取标签包含的组件标签列表.
TAG 是标签名称"
  (message "Debug - Getting components for tag: %s" tag)
  (let ((relations (org-supertag-get-relations :tag-contain tag)))
    (message "Debug - Component relations: %S" relations)
    (mapcar #'car relations)))

;;; 引用关系 API

(defun org-supertag-tag-reference (source target &optional props)
  "建标签引用关系.
SOURCE 引用 TARGET"
  (org-supertag-tag-add-relation :tag-ref source target props))

(defun org-supertag-tag-get-references (tag)
  "获取标签引用的其他标签列表.
TAG 是标签名称"
  (mapcar #'car (org-supertag-get-relations :tag-ref tag)))

(defun org-supertag-tag-get-referrers (tag)
  "获取引用该标签的其他标签列表.
TAG 是标签名称"
  (mapcar #'car (org-supertag-get-relations-reversed-v2 :tag-ref tag)))

;;; 互斥关系 API

(defun org-supertag-tag-exclusive (tag1 tag2)
  "建立标签互斥关系.
TAG1 和 TAG2 是互斥的标签"
  (org-supertag-tag-add-relation :tag-excl tag1 tag2))

(defun org-supertag-tag-get-exclusives (tag)
  "获取与标签互斥的标签列表.
TAG 是标签名称"
  (append
   (mapcar #'car (org-supertag-get-relations :tag-excl tag))
   (mapcar #'car (org-supertag-get-relations-reversed :tag-excl tag))))

;;; 查询功能

(defun org-supertag-tag-find-related (tag &optional relation-type)
  "查找标签相关的标签.
TAG 是标签名称
RELATION-TYPE 是可选的关系类型"
  (let ((relations (org-supertag-tag-get-relations tag relation-type)))
    (mapcar #'car relations)))

(defun org-supertag-tag-find-path (from to)
  "查找从一个标签到另一个标签的路径.
FROM 是起始标签
TO 是目标标签
回标签列表，表示从 FROM 到 TO 的路径"
  (let ((visited (make-hash-table :test 'equal))
        (path nil))
    (cl-labels ((find-path (current target path)
                  (if (equal current target)
                      (cons current path)
                    (unless (gethash current visited)
                      (puthash current t visited)
                      (let* ((related (org-supertag-tag-find-related current))
                             (result (cl-some
                                     (lambda (next)
                                       (find-path next target (cons current path)))
                                     related)))
                        (when result
                          (cons current path)))))))
      (nreverse (find-path from to nil)))))

;;; 独立标签查询

(defun org-supertag-tag-independent-p (tag)
  "判断标签是否是独立的（没有任何关系）.
TAG 是标签名称"
  (null (org-supertag-tag-get-relations tag)))

(defun org-supertag-tag-find-independent ()
  "查找所有独立标签."
  (cl-remove-if-not #'org-supertag-tag-independent-p
                    (org-supertag-find-entities :tag)))

;;; 继承关系检查系统

(defun org-supertag-tag-get-descendants (tag)
  "获取标签的所有后代标签（包括直接和间接子标签）."
  (let ((result nil)
        (visited (make-hash-table :test 'equal)))
    (cl-labels ((collect-descendants (current)
                  (unless (gethash current visited)
                    (puthash current t visited)
                    (maphash
                     (lambda (child-name _)
                       (when (equal (org-supertag-tag-get-parent child-name) current)
                         (push child-name result)
                         (collect-descendants child-name)))
                     (org-supertag-get-entities)))))
      (collect-descendants tag))
    (nreverse result)))

;; 为当前 headline 添加指定标签的字段
(defun org-supertag--maybe-create-template (tag-name)
  "如果标签没有模板，提示用户是否创建.
返回创建的模板 ID 或 nil."
  (unless (org-supertag-db-get-linked tag-name :tag-template)
    (when (y-or-n-p (format "标签 '%s' 没有模板，是否创建？" tag-name))
      ;; 快速创建模板
      (let* ((fields-input (read-string "输入字段 (用逗号分隔): "))
             (fields (unless (string-empty-p fields-input)
                      (mapcar (lambda (name)
                              (list :type :property
                                    :name (string-trim name)
                                    :required nil
                                    :default ""))
                              (split-string fields-input "," t "[ \t\n]+"))))
             (template (make-org-supertag-template
                       :id (org-supertag--generate-id)
                       :tag-name tag-name
                       :display-name tag-name
                       :fields (vconcat fields))))
        ;; 保存模板
        (org-supertag-create-template template)
        (org-supertag-template-id template)))))

(defun org-supertag--apply-tag-fields (tag-name)
  "为当前 headline 添加指定标签的字段."
  (when-let* ((template-id (org-supertag-db-get-linked tag-name :tag-template))
              (template (org-supertag-get-template template-id))
              (fields (org-supertag-template-fields template))
              (node-id (org-id-get)))
    (dolist (field fields)
      (let* ((field-name (plist-get field :name))
             (prop-name (concat field-name))
             (field-id (format "%s_%s" tag-name field-name))
             (default-value (plist-get field :default))
             ;; 获取现有值或使用默认值
             (value (or (org-entry-get nil prop-name)
                       default-value
                       "")))
        ;; 同时更新属性和数据库
        (org-entry-put nil prop-name value)
        (org-supertag-db-set-field-value field-id node-id value)))))

(defun org-supertag--create-template-from-tag (tag-name fields)
  "从标签和字段创建模板.
TAG-NAME 是标签名称
FIELDS 是字段列表，每个字段是 (field-name . default-value) 对"
  (unless (stringp tag-name)
    (error "Tag name must be a string"))
  (unless (listp fields)
    (error "Fields must be a list"))
  
  (let* ((template-id (org-supertag--generate-id))
         (template-fields 
          (mapcar (lambda (field)
                    (unless (consp field)
                      (error "Each field must be a cons pair"))
                    (list :type :property
                          :name (car field)
                          :required nil
                          :default (or (cdr field) "")))
                  fields))
         (template (make-org-supertag-template
                   :id template-id
                   :tag-name tag-name
                   :display-name tag-name
                   :fields (vconcat template-fields))))
    ;; 保存模板
    (org-supertag-create-template template)
    template-id))

(defun org-supertag--infer-field-type (name)
  "根据字段名智能推断字段类型."
  (cond
   ((string-match-p "\\(?:date\\|time\\|created\\|updated\\|deadline\\|due\\|start\\|end\\|scheduled\\|timestamp\\)$" name) 
    :date)
   ((string-match-p "\\(?:tags?\\|categories\\|labels\\|members\\|links?\\|refs?\\|related\\|topics?\\|skills?\\|languages\\)$" name) 
    :list)
   ((string-match-p "\\(?:status\\|state\\|priority\\|type\\|level\\|grade\\|rating\\|category\\|group\\|phase\\|stage\\)$" name) 
    :choice)
   ((string-match-p "\\(?:count\\|number\\|amount\\|price\\|age\\|score\\|quantity\\|weight\\|height\\|size\\|duration\\|id\\)$" name) 
    :number)
   ((string-match-p "\\(?:url\\|link\\|website\\|homepage\\|repo\\|repository\\)$" name)
    :url)
   ((string-match-p "\\(?:email\\|mail\\)$" name)
    :email)
   ((string-match-p "\\(?:phone\\|tel\\|mobile\\|fax\\)$" name)
    :tel)
   (t :property)))

(defun org-supertag--parse-field-spec (spec)
  "解析字段规格字符串.
语法: name[*][:type][=value]
例如: title* desc tags:list author=John"
  (when (string-match "^\\([^:*=]+\\)\\(*\\)?\\(?::\\([^=*]+\\)\\)?\\(*\\)?\\(?:=\\(.+\\)\\)?$" spec)
    (let* ((name (match-string 1 spec))
           (required1 (match-string 2 spec))
           (type (or (match-string 3 spec) 
                    (symbol-name (org-supertag--infer-field-type name))))
           (required2 (match-string 4 spec))
           (default (match-string 5 spec)))
      (list :name (string-trim name)
            :type (intern type)
            :required (and (or required1 required2) t)
            :default (or default "")))))

(defun org-supertag--find-similar-tags (tag-name)
  "查找与给定标签名相似的已存在标签."
  (let* ((all-tags (mapcar #'car (org-supertag-find-entities :tag)))
         (similar-tags '()))
    (dolist (tag all-tags)
      (when (or (string-match-p tag-name tag)
                (string-match-p tag tag-name))
        (push tag similar-tags)))
    similar-tags))

(defun org-supertag--get-tag-fields (tag-name)
  "获取标签的字段定义."
  (when-let* ((template-id (org-supertag-db-get-linked tag-name :tag-template))
              (template (org-supertag-find-entity :template template-id)))
    (mapcar (lambda (field)
              (format "%s%s:%s%s"
                      (plist-get field :name)
                      (if (plist-get field :required) "*" "")
                      (plist-get field :type)
                      (if (string-empty-p (plist-get field :default))
                          ""
                        (concat "=" (plist-get field :default)))))
            (org-supertag-template-fields template))))

(defun org-supertag--infer-fields-from-tag (tag-name)
  "从标签名推断可能的字段."
  (cond
   ((string-match-p "\\(?:task\\|todo\\|issue\\|ticket\\)$" tag-name)
    '("title*" "status:choice=TODO" "priority:choice" "due:date" "assigned:list" "tags:list"))
   
   ((string-match-p "\\(?:note\\|doc\\|article\\|post\\|wiki\\)$" tag-name)
    '("title*" "tags:list" "created:date" "category:choice" "status:choice=Draft"))
   
   ((string-match-p "\\(?:project\\|prog\\|program\\)$" tag-name)
    '("title*" "status:choice=Planning" "start:date" "deadline:date" "members:list" "priority:choice" "budget:number"))
   
   ((string-match-p "\\(?:contact\\|person\\|employee\\|user\\|member\\)$" tag-name)
    '("name*" "email" "phone" "company" "position" "department"))
   
   ((string-match-p "\\(?:event\\|meeting\\|appointment\\|session\\)$" tag-name)
    '("title*" "start:date*" "end:date" "location" "participants:list" "agenda:list"))
   
   ((string-match-p "\\(?:book\\|reading\\|literature\\)$" tag-name)
    '("title*" "author*" "status:choice=ToRead" "rating:number" "tags:list" "notes"))
   
   ((string-match-p "\\(?:link\\|resource\\|bookmark\\|ref\\)$" tag-name)
    '("title*" "url*" "tags:list" "description" "added:date"))
   
   ((string-match-p "\\(?:idea\\|concept\\|brainstorm\\)$" tag-name)
    '("title*" "status:choice=Draft" "category:choice" "related:list" "impact:choice"))
   
   ((string-match-p "\\(?:goal\\|objective\\|target\\)$" tag-name)
    '("title*" "deadline:date" "progress:number" "status:choice=InProgress" "priority:choice"))
   
   (t (list "title*" "desc" "tags:list" "created:date"))))

(defun org-supertag--create-template-interactive (tag-name)
  "交互式创建模板."
  (let* ((similar-tags (org-supertag--find-similar-tags tag-name))
         (has-similar (not (null similar-tags)))
         (use-preset (and org-supertag-template-presets
                         (y-or-n-p "使用预设模板?")))
         (fields
          (cond
           ;; 使用预设
           (use-preset
            (let* ((preset-name (completing-read "选择预设: " 
                                              org-supertag-template-presets nil t))
                   (preset (alist-get preset-name org-supertag-template-presets)))
              (mapcar #'org-supertag--parse-field-spec
                      (split-string preset))))
           ;; 有相似标签时提供选择
           ((and has-similar
                 (y-or-n-p (format "发现相似标签 %s，是否参考它们的字段?" 
                                  similar-tags)))
            (let* ((similar-fields 
                    (delete-dups
                     (mapcan #'org-supertag--get-tag-fields similar-tags)))
                   (selected-fields
                    (completing-read-multiple
                     "选择要使用的字段: " similar-fields nil t)))
              (mapcar #'org-supertag--parse-field-spec selected-fields)))
           ;; 手动输入或智能推断
           (t
            (let ((specs (split-string
                         (read-string 
                          "输入字段 (空格分隔,回车智能推断): "))))
              (mapcar #'org-supertag--parse-field-spec
                      (if (null specs)
                          (org-supertag--infer-fields-from-tag tag-name)
                        specs)))))))
    (when fields
      (org-supertag--create-template-from-tag tag-name fields))))

(provide 'org-supertag-tag)

;;; org-supertag-tag.el ends here
