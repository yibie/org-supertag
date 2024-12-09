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
TO 是目标标签"
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

(defun org-supertag--normalize-field-def (field)
  "规范化字段定义.
FIELD 是字段定义"
  (message "Debug - Normalizing field: %S" field)
  (let ((field-plist (if (and (vectorp field) 
                             (= (length field) 1))
                        ;; 如果是单元素向量，取其内容
                        (elt field 0)
                      ;; 否则尝试转换为列表
                      (if (vectorp field)
                          (append field nil)
                        field))))
    (message "Debug - Field as plist: %S" field-plist)
    ;; 将关键字类型转换为普通符号
    (when-let ((type (plist-get field-plist :type)))
      (message "Debug - Original type: %S" type)
      (setq field-plist 
            (plist-put field-plist :type 
                      (if (keywordp type)
                          (intern (substring (symbol-name type) 1))
                        type)))
      (message "Debug - Normalized type: %S" (plist-get field-plist :type)))
    field-plist))

(defun org-supertag--parse-field-def (field)
  "解析字段定义，确保返回标准格式。
FIELD 是字段定义，可以是 plist 或向量"
  (message "Debug - Parsing field: %S" field)
  (let ((field-def nil))
    ;; 如果是向量，转换为列表
    (when (vectorp field)
      (setq field (append field nil)))
    
    ;; 处理字段定义
    (cond
     ;; 如果已经是标准格式的 plist
     ((and (listp field)
           (plist-member field :name)
           (plist-member field :type))
      (setq field-def field))
     
     ;; 如果是预设标签的字段格式（一个包含属性的列表）
     ((and (listp field)
           (listp (car field))
           (keywordp (caar field)))
      (let ((name (plist-get field :name))
            (type (plist-get field :type))
            (required (plist-get field :required))
            (options (or (plist-get field :options)  ; 新格式
                        (plist-get field :values)))) ; 旧格式
        (setq field-def
              (list :name name
                    :type (if (keywordp type)
                             (intern (substring (symbol-name type) 1))
                           type)
                    :required required
                    :options options)))))
    
    (message "Debug - Parsed field: %S" field-def)
    field-def))
    
(defun org-supertag--validate-field-def (field)
  "验证字段定义是否有效.
FIELD 是字段定义"
  (message "Debug - Validating field: %S" field)
  (let ((valid (and (listp field)
                    (plist-get field :name)
                    (plist-get field :type))))
    (message "Debug - Field validation result: %s" valid)
    valid))

(defun org-supertag-tag-create (name &rest props)
  "创建标签.
NAME 是标签名称
PROPS 是标签属性"
  (let* ((fields (plist-get props :fields))
         (fields-list (if (vectorp fields)
                         (append fields nil)
                       fields)))
    (message "Debug - Processing fields for tag %s" name)
    (message "Debug - Raw fields: %S" fields)
    
    ;; 处理字段定义
    (let* ((normalized-fields
            (mapcar (lambda (field)
                     (let ((parsed (org-supertag--parse-field-def field)))
                       (message "Debug - Field %S parsed as: %S" field parsed)
                       parsed))
                   fields-list))
           (valid-fields
            (cl-remove-if-not
             (lambda (field)
               (when field
                 (message "Debug - Validating field: %S" field)
                 (org-supertag--validate-field-def field)))
             normalized-fields))
           (tag-props (append 
                      (list :type :tag
                            :name name
                            :fields valid-fields)
                      (org-plist-delete props :fields))))
      (message "Debug - Normalized fields: %S" normalized-fields)
      (message "Debug - Valid fields: %S" valid-fields)
      (org-supertag-db-put name tag-props))))

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
  "判断标签是立的（没有任何关系）.
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

;;; 字段管理
(defun org-supertag--apply-tag-fields (tag-name)
  "应用标签的字段到当前节点。
TAG-NAME 是标签名称。"
  (when-let* ((tag (org-supertag-db-get tag-name))
              (fields (plist-get tag :fields))
              (node-id (org-id-get-create)))
    (message "Debug - 应用字段: %S" fields)
    ;; 首先确保标签被添加到节点
    ;;(org-toggle-tag tag-name 'on)
    
    ;; 然后处理字段
    (dolist (field fields)
      (when (org-supertag--validate-field-def field)
        (let* ((field-name (plist-get field :name))
               (field-type (plist-get field :type)))
          (message "Debug - 处理字段: %s (%s)" field-name field-type)
          ;; 使用 field 系统的读取函数获取值
          (condition-case err
              (when-let* ((value (org-supertag-field-read-value field))
                         (prop-name field-name)
                         (prop-value (if (stringp value)
                                       value
                                     (format "%s" value))))
                (message "Debug - Setting property %s to %S (type: %s)" 
                        prop-name prop-value (type-of prop-value))
                (org-entry-put nil prop-name prop-value)
                (message "字段 %s 设置为: %s" field-name prop-value)
                (org-supertag-db-set-field-value field-name node-id prop-value))
            (error
             (message "Error - 处理字段 %s 时出错: %s" 
                     field-name (error-message-string err)))))))
    
    ;; 确保标签被正确添加
    (org-set-tags (org-get-tags))
    (message "标签 %s 的字段应用完成" tag-name)
    (message "Debug - 字段应用完成")))
    
(defun org-supertag-tag-define-fields (tag-name)
  "定义标签的字段.
TAG-NAME 是标签名称"
  (interactive
   (list (completing-read "Tag: " (org-supertag-find-entities :tag))))
  
  (let* ((tag (org-supertag-tag-get tag-name))
         (fields nil))
    ;; 交互式定义字段
    (while (y-or-n-p "添加字段?")
      (let* ((field-name (read-string "字段名: "))
             (field-type (intern
                         (completing-read "类型: "
                                        (mapcar #'symbol-name org-supertag-field-types)
                                        nil t)))
             (required (y-or-n-p "是否必填? "))
             (field-def {:name field-name
                        :type field-type
                        :required required}))
        
        ;; 处理特殊类型的额外属性
        (pcase field-type
          ('choice
           (let ((options (split-string
                          (read-string "选项 (逗号分隔): ")
                          "," t "[ \t\n]+")))
             (setq field-def (plist-put field-def :values options))))
          
          ('list
           (let ((separator (read-string "分隔符 (默认逗号): " ",")))
             (setq field-def (plist-put field-def :separator separator)))))
        
        (push field-def fields)))
    
    ;; 更新标签
    (org-supertag-tag-update tag-name
                            (list :fields fields))))

(defun org-supertag--read-field-value (field-def)
  "读字段值.
FIELD-DEF 是字段定义"
  (let* ((name (plist-get field-def :name))
         (type (plist-get field-def :type))
         (required (plist-get field-def :required))
         (current (org-entry-get nil (concat ":" name))))
    
    (pcase type
      ('choice
       (let ((options (plist-get field-def :values)))
         (completing-read (format "%s: " name)
                         options nil t current)))
      
      ('list
       (let ((separator (or (plist-get field-def :separator) ",")))
         (mapconcat #'identity
                   (completing-read-multiple
                    (format "%s: " name)
                    nil nil nil current)
                   separator)))
      
      (_
       (read-string (format "%s%s: "
                           name
                           (if required "*" ""))
                   (or current ""))))))

(defun org-supertag-tag-apply-preset (preset-name)
  "应用预设标签.
PRESET-NAME 是预设名称"
  (interactive
   (list (completing-read "预设: " 
                         (mapcar #'car org-supertag-tag-presets))))
  
  (when-let* ((preset (alist-get preset-name org-supertag-tag-presets
                                nil nil #'string=)))
    (message "Debug - 应用预设标签: %s" preset-name)
    (message "Debug - 预设字段: %S" preset)
    
    ;; 创建或更新标签
    (org-supertag-tag-create preset-name :fields preset)
    
    ;; 应用标签（包括字段）
    (org-supertag--apply-tag preset-name)))

(defun org-supertag--apply-tag (tag-name)
  "应用标签到当前节点.
TAG-NAME 是标签名称"
  (message "Debug - 开始应用标签: %s" tag-name)
  (let ((node-id (org-id-get-create)))
    (message "Debug - 节点 ID: %s" node-id)
    
    ;; 添加标签关系
    (org-supertag-tag-add-to-node node-id tag-name)
    (message "Debug - 已添加标签关系")
    
    ;; 更新标题
    (let* ((element (org-element-at-point))))))


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
  (when-let* ((template-id (org-supertag-get-linked tag-name :tag-template))
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



(defun org-supertag-tag-define-fields (tag-name)
  "定义标签的字段.
TAG-NAME 是标签名称"
  (interactive
   (list (completing-read "Tag: " (org-supertag-find-entities :tag))))
  
  (let* ((tag (org-supertag-tag-get tag-name))
         (current-fields (plist-get tag :fields))
         (new-fields (org-supertag--field-interactive-edit current-fields)))
    
    ;; 更新标签
    (when new-fields
      (org-supertag-tag-update tag-name
                              (list :fields (append current-fields 
                                                  new-fields))))))

(provide 'org-supertag-tag)

;;; org-supertag-tag.el ends here
