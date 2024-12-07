;;; org-supertag-field.el --- Field system for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供字段系统的核心功能：
;; - 字段类型定义
;; - 字段值存取
;; - 与 Org 的集成
;; - 字段同步机制

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-supertag-api)
(require 'org-supertag-types)

;;; 字段类型系统

(defvar org-supertag-field-type-registry
  (ht-create)
  "字段类型注册表。
每个类型定义包含：
- :validator  验证函数
- :formatter  格式化函数
- :description  类型描述")

(defconst org-supertag-field-types
  '(:property    ; Org Properties
    :drawer     ; Org Drawers
    :planning   ; Org Planning (DEADLINE, SCHEDULED)
    :todo       ; TODO 状态
    :priority   ; 优先级
    :tags       ; 标签
    :number     ; 数值
    :string     ; 字符串
    :date       ; 日期
    :enum)      ; 枚举
  "支持的字段类型列表.")

;; :TODO 需要完善系统字段定义
(defconst org-supertag-system-fields
  '((:name "deadline"
     :type :planning
     :org-name "DEADLINE"
     :description "截止日期")
    
    (:name "scheduled"
     :type :planning
     :org-name "SCHEDULED"
     :description "计划日期")
    
    (:name "priority"
     :type :priority
     :org-name "PRIORITY"
     :description "优先级"
     :values ("A" "B" "C"))
    
    (:name "todo"
     :type :todo
     :description "待办状态"
     :values org-todo-keywords)
    
    (:name "tags"
     :type :tags
     :org-name "TAGS"
     :description "标签列表"))
  "预定义的系统字段.")

(defun org-supertag-field--get-planning (field-def)
  "获取 Planning 类型字段的值.
FIELD-DEF 是字段定义"
  (let* ((planning-type (org-supertag-field-get-org-name field-def))
         (planning-element (org-element-map (org-element-parse-buffer) 'planning
                           (lambda (p) p)
                           nil t))
         (timestamp (when planning-element
                     (org-element-property 
                      (intern (concat ":" (downcase planning-type)))
                      planning-element))))
    (message "Debug - Getting planning type: %S" planning-type)
    (message "Debug - Planning element: %S" planning-element)
    (message "Debug - Timestamp: %S" timestamp)
    ;; 将 timestamp 对象转换为字符串
    (when timestamp
      (org-element-property :raw-value timestamp))))

(defun org-supertag-field--set-planning (field-def value)
  "设置 Planning 类型字段的值.
FIELD-DEF 是字段定义
VALUE 是要设置的值"
  (let ((planning-type (org-supertag-field-get-org-name field-def)))
    (org-schedule nil value)))

(defun org-supertag-field--get-todo ()
  "获取 TODO 状态."
  (org-get-todo-state))

(defun org-supertag-field--set-todo (value)
  "设置 TODO 状态.
VALUE 是要设置的状态"
  (org-todo value))

(defun org-supertag-field--get-priority ()
  "获取优先级."
  (org-entry-get nil "PRIORITY"))

(defun org-supertag-field--set-priority (value)
  "设置优先级.
VALUE 是要设置的优先级"
  (org-priority (string-to-char value)))

(defun org-supertag-field--get-tags ()
  "获取标签列表."
  (org-get-tags))

(defun org-supertag-field--set-tags (value)
  "设置标签列表.
VALUE 是要设置的标签列表"
  (org-set-tags value))

;;; 字段类型定义操作


(defun org-supertag-register-field-type (type props)
  "注册字段类型.
TYPE 是类型标识符（必须在 `org-supertag-field-types' 中定义）
PROPS 是类型定义"
  (unless (memq type org-supertag-field-types)
    (error "未定义的字段类型: %s" type))
  (ht-set! org-supertag-field-type-registry type props))

  ;; 注册基本类型
(org-supertag-register-field-type
 :number
 '(:validator (lambda (val)
                (and (numberp val)
                     (<= 0 val 100)))
   :formatter number-to-string
   :description "数值类型（0-5）"))

(org-supertag-register-field-type
 :string
 '(:validator stringp
   :formatter identity
   :description "字符串类型"))

(org-supertag-register-field-type
 :priority
 '(:validator (lambda (val)
                (and (stringp val)
                     (string-match-p "^[A-C]$" val)))
   :formatter identity
   :description "优先级，A-C"))

(org-supertag-register-field-type
 :todo
 '(:validator (lambda (val)
                (and (stringp val)
                     (member val org-todo-keywords)))
   :formatter identity
   :description "TODO 状态"))

;;; 字段定义操作

(defun org-supertag-field-create (name props)
  "创建字段定义.
NAME 是字段名称
PROPS 是字段属性"
  ;; 确保必要的属性存在
  (unless (plist-get props :type)
    (error "Field must have a :type property"))
  
  ;; 确保 :name 属性存在
  (unless (plist-get props :name)
    (setq props (plist-put props :name name)))
  
  ;; 添加实体类型标记
  (setq props (plist-put props :entity-type :field))
  
  ;; 存储字段定义
  (let ((result (org-supertag-db-put name props)))
    (message "Debug - Field creation stored: %S" result)
    result))

(defun org-supertag-field-get (name)
  "获取字段定义.
NAME 是字段名称"
  (let ((field (org-supertag-db-get name)))
    (message "Debug - Raw field data: %S" field)
    (when (and field (eq (plist-get field :entity-type) :field))
      field)))

(defun org-supertag-field-get-type (field-def)
  "获取字段类型.
FIELD-DEF 是字段定义"
  (plist-get field-def :type))

(defun org-supertag-field-get-org-name (field-def)
  "获取字段对应的 Org 属性名.
FIELD-DEF 是字段定义"
  (or (plist-get field-def :org-name)
      (plist-get field-def :name)))

;;; 字段值操作

(defun org-supertag-field--get-property (field-def)
  "获取 Property 类型字段的值.
FIELD-DEF 是字段定义"
  (let ((prop-name (org-supertag-field-get-org-name field-def)))
    (message "Debug - Getting property: %S" prop-name)
    (message "Debug - At point: %S" (point))
    (message "Debug - Buffer content:\n%s" (buffer-string))
    (org-entry-get nil prop-name)))

(defun org-supertag-field--set-property (field-def value)
  "设置 Property 类型字段的值.
FIELD-DEF 是字段定义
VALUE 是要设置的值"
  (org-entry-put nil (org-supertag-field-get-org-name field-def) value))

(defun org-supertag-field--get-drawer (field-def)
  "���取 Drawer 类型字段的值.
FIELD-DEF 是字段定义"
  (let ((drawer-name (org-supertag-field-get-org-name field-def)))
    (org-element-map (org-element-at-point) 'drawer
      (lambda (drawer)
        (when (string= (org-element-property :drawer-name drawer) drawer-name)
          (buffer-substring-no-properties
           (org-element-property :contents-begin drawer)
           (org-element-property :contents-end drawer))))
      nil t)))

(defun org-supertag-field--set-drawer (field-def value)
  "设置 Drawer 类型字段的值.
FIELD-DEF 是字段定义
VALUE 是要设置的值"
  (let ((drawer-name (org-supertag-field-get-org-name field-def)))
    (save-excursion
      (org-back-to-heading t)
      ;; 删除现有抽屉
      (org-element-map (org-element-at-point) 'drawer
        (lambda (drawer)
          (when (string= (org-element-property :drawer-name drawer) drawer-name)
            (delete-region
             (org-element-property :begin drawer)
             (org-element-property :end drawer))))
        nil t)
      ;; 创建新抽屉
      (end-of-line)
      (insert "\n:" drawer-name ":\n" value ":END:\n"))))



;;; 统一接口

(defun org-supertag-field-get-value (field-def)
  "获取字段值.
FIELD-DEF 是字段定义"
  (let ((type (plist-get field-def :type)))
    (message "Debug - Getting value for field: %S" field-def)
    (message "Debug - Field type: %S" type)
    (pcase type
      (:property (org-supertag-field--get-property field-def))
      (:drawer (org-supertag-field--get-drawer field-def))
      (:planning (org-supertag-field--get-planning field-def))
      (:todo (org-supertag-field--get-todo))
      (:priority (org-supertag-field--get-priority))
      (:tags (org-supertag-field--get-tags))
      (_ (error "Unsupported field type: %s" type)))))

(defun org-supertag-field-set-value (field-def value)
  "设置字段值.
FIELD-DEF 是字段定义
VALUE 是要设置的值"
  (let ((type (plist-get field-def :type)))
    (message "Debug - Setting value for field: %S" field-def)
    (message "Debug - Field type: %S" type)
    (message "Debug - Value to set: %S" value)
    (pcase type
      (:property (org-supertag-field--set-property field-def value))
      (:drawer (org-supertag-field--set-drawer field-def value))
      (:planning (org-supertag-field--set-planning field-def value))
      (:todo (org-supertag-field--set-todo value))
      (:priority (org-supertag-field--set-priority value))
      (:tags (org-supertag-field--set-tags value))
      (_ (error "Unsupported field type: %s" type)))))

;;; 同步机制

(defun org-supertag-field-sync-node (node-id)
  "同步节点的所有字段.
NODE-ID 是节点ID"
  (org-with-point-at (org-id-find node-id t)
    ;; 获取节点的所有标签
    (let* ((tags (org-supertag-node-get-tags node-id))
           (all-fields (mapcan #'org-supertag-tag-get-fields tags)))
      ;; 同步每个字段
      (dolist (field all-fields)
        (when-let ((value (org-supertag-field-get-value field)))
          (org-supertag-set-field node-id 
                                 (plist-get field :name)
                                 value))))))

;;; 字段值验证
(defun org-supertag-field-validate (field-def value)
  "验证字段值是否符合字段定义.
FIELD-DEF 是字段定义
VALUE 是要验证的值
返回 (t . nil) 如果验证通过，(nil . 错误信息) 如果验证失败"
  ;; 如果字段定义为空，返回验证失败
  (if (null field-def)
      (cons nil "字段定义不存在")
    (let* ((type (plist-get field-def :type))
           (type-def (ht-get org-supertag-field-type-registry type))
           (type-validator (when type-def (plist-get type-def :validator)))
           (custom-validator (plist-get field-def :validate)))
      (message "Debug - Field definition: %S" field-def)
      (message "Debug - Type: %S" type)
      (message "Debug - Type definition: %S" type-def)
      (message "Debug - Type validator: %S" type-validator)
      (message "Debug - Custom validator: %S" custom-validator)
      (message "Debug - Value to validate: %S" value)
      
      ;; 对枚举类型进行特殊处理
      (if (eq type :enum)
          (let ((valid-values (plist-get field-def :values)))
            (if (memq value valid-values)
                (cons t nil)
              (cons nil (format "值必须是以下之一: %S" valid-values))))
        ;; 其他类型的验证
        (cond
         ;; 类型验证
         ((and type-validator
               (not (funcall type-validator value)))
          (cons nil (format "值不符合类型 %s 的要求" type)))
         ;; 自定义验证
         ((and custom-validator
               (not (funcall custom-validator value)))
          (cons nil "自定义验证失败"))
         ;; 验证通过
         (t (cons t nil)))))))

;; 注册枚举类型
(org-supertag-register-field-type
 :enum
 '(:validator (lambda (val)
                (keywordp val))  ; 确保值是关键字类型
   :formatter symbol-name
   :description "枚举类型"))

(provide 'org-supertag-field)
;;; org-supertag-field.el ends here