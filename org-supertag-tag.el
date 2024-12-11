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


;;; 标签实例操作 API

(defun org-supertag-sanitize-tag-name (name)
  "将名称转换为有效的标签名称.
NAME 是要转换的名称
只处理空格问题，将空格转换为下划线"
  (if (or (null name) (string-empty-p name))
      (error "Tag name cannot be empty")
    (let* ((trimmed (string-trim name))
           ;; 移除开头的 # （如果有）
           (without-hash (replace-regexp-in-string "^#+" "" trimmed))
           ;; 将空格转换为下划线
           (sanitized (replace-regexp-in-string "\\s-+" "_" without-hash)))
      (if (string-empty-p sanitized)
          (error "Invalid tag name: %s" name)
        sanitized))))

(defun org-supertag-format-tag (tag-name)
  "格式化标签用于显示.
TAG-NAME 是标签名称"
  (concat "#" tag-name))

(defun org-supertag-tag-create (name &rest props)
  "创建标签.
NAME 是标签名称
PROPS 是标签属性"
  (let* ((sanitized-name (org-supertag-sanitize-tag-name name))
         (fields (plist-get props :fields))
         (fields-list (if (vectorp fields)
                         (append fields nil)
                       fields)))
    (message "Debug - 规范化后的标签名: %s" sanitized-name)
    (message "Debug - 处理标签字段: %S" fields)
    
    ;; 处理字段定义
    (let* ((normalized-fields
            (mapcar (lambda (field)
                     (let ((parsed (org-supertag--parse-field-def field)))
                       (message "Debug - 字段 %S 解析为: %S" field parsed)
                       parsed))
                   fields-list))
           (valid-fields
            (cl-remove-if-not
             (lambda (field)
               (when field
                 (message "Debug - 验证字段: %S" field)
                 (org-supertag--validate-field-def field)))
             normalized-fields))
           (tag-props (append 
                      (list :type :tag
                            :name sanitized-name
                            :fields valid-fields)
                      (org-plist-delete props :fields))))
      
      (message "Debug - 创建标签实体: %s" sanitized-name)
      (org-supertag-db-put sanitized-name tag-props))))

(defun org-supertag-tag-get (tag-name)
  "获取标签定义.
TAG-NAME 是标签名称"
  (let ((sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    (org-supertag-db-get sanitized-name)))

(defun org-supertag-tag-exists-p (tag-name)
  "检查标签是否存在.
TAG-NAME 是标签名称"
  (let ((sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    (org-supertag-db-exists-p sanitized-name)))

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
    (message "标签 %s 的字段应用完成" tag-name)))
    
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
  "应用标签到当前节点。
TAG-NAME 是标签名称"
  (let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
         (node-id (org-id-get-create)))
    (message "Debug - 开始应用标签: %s -> %s" tag-name sanitized-name)
    (message "Debug - 节点 ID: %s" node-id)
    
    ;; 添加标签关系
    (org-supertag-tag-add-to-node node-id sanitized-name)
    (message "Debug - 已添加标签关系")
    
    ;; 更新标题
    (let* ((element (org-element-at-point))
           (title (org-element-property :raw-value element))
           (new-title (concat title " #" sanitized-name)))
      (message "Debug - 当前标题: %s" title)
      (message "Debug - 新标题: %s" new-title)
      (org-edit-headline new-title))
    
    ;; 应用字段
    (message "Debug - 开始应用字段")
    (org-supertag--apply-tag-fields sanitized-name)
    (message "Debug - 字段应用完成")))

(defun org-supertag-tag-add-to-node (node-id tag-name)
  "将标签添加到节点.
NODE-ID 是节点 ID
TAG-NAME 是标签名称"
  (let ((sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    (org-supertag-db-link :node-tag node-id sanitized-name)))

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
