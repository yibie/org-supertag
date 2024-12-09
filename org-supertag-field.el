;;; org-supertag-field.el --- Field system for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供字段系统的核心功能：
;; - 字段类型定义
;; - 字段值存取
;; - 与 Org 的集成
;; - 字段同步机制

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-supertag-api)
(require 'org-supertag-types)

;;; 字段类型系统

(defvar org-supertag-field-types
  (make-hash-table :test 'equal)
  "字段类型注册表。
每个类型是一个 plist，包含：
- :validator  验证函数
- :formatter  格式化函数
- :description  类型描述")

(defun org-supertag-register-field-type (type spec)
  "注册字段类型。
TYPE 是类型名称（symbol），
SPEC 是包含 :validator 和 :formatter 的 plist。"
  (puthash type spec org-supertag-field-types))

(defun org-supertag-get-field-types ()
  "获取所有支持的字段类型。"
  '(("字符串" . string)
    ("选项" . options)
    ("数字" . number)
    ("日期" . date)
    ("引用" . reference)
    ("标签" . tags)
    ("Planning" . planning)))

(defun org-supertag-get-field-type (type)
  "获取字段类型定义。
TYPE 是字段类型的符号。"
  (pcase type
    ('string
     '(:validator org-supertag-validate-string
       :formatter org-supertag-format-string
       :reader org-supertag-read-string-field
       :description "基础字符串类型"))
    
    ('options
     '(:validator org-supertag-validate-options
       :formatter org-supertag-format-options
       :reader org-supertag-read-options-field
       :description "从预定义选项中选择"))
    
    ('number
     '(:validator org-supertag-validate-number
       :formatter org-supertag-format-number
       :reader org-supertag-read-number-field
       :description "数字类型"))
    
    ('date
     '(:validator org-supertag-validate-date
       :formatter org-supertag-format-date
       :reader org-supertag-read-date-field
       :description "日期类型"))
    
    ('reference
     '(:validator org-supertag-validate-reference
       :formatter org-supertag-format-reference
       :reader org-supertag-read-reference-field
       :description "引用其他条目"))
    
    ('tags
     '(:validator org-supertag-validate-tags
       :formatter org-supertag-format-tags
       :reader org-supertag-read-tags-field
       :description "标签列表"))
    
    ('planning
     '(:validator org-supertag-validate-planning
       :formatter org-supertag-format-planning
       :reader org-supertag-read-planning-field
       :description "Planning 类型"))
    ('list
     '(:validator org-supertag-validate-list
       :formatter org-supertag-format-list
       :reader org-supertag-read-list-field
       :description "列表类型"))))  


;; 初始化基本字段类型
(defun org-supertag-init-field-types ()
  "初始化字段类型。"
  (clrhash org-supertag-field-types)
  
  ;; 字符串类型
  (org-supertag-register-field-type
   'string
   '(:validator org-supertag-validate-string
     :formatter org-supertag-format-string
     :reader org-supertag-read-string-field
     :description "基础字符串类型"))
  
  ;; 日期类型
  (org-supertag-register-field-type
   'date
   '(:validator org-supertag-validate-date
     :formatter org-supertag-format-date
     :reader org-supertag-read-date-field
     :description "日期类型 (YYYY-MM-DD)"))
  
  ;; 邮箱类型
  (org-supertag-register-field-type
   'email
   '(:validator org-supertag-validate-email
     :formatter org-supertag-format-email
     :reader org-supertag-read-email-field
     :description "电子邮箱地址"))
  
  ;; URL类型
  (org-supertag-register-field-type
   'url
   '(:validator org-supertag-validate-url
     :formatter org-supertag-format-url
     :reader org-supertag-read-url-field
     :description "URL地址"))
  
  ;; 引用类型
  (org-supertag-register-field-type
   'reference
   '(:validator org-supertag-validate-reference
     :formatter org-supertag-format-reference
     :reader org-supertag-read-reference-field
     :description "引用其他带有特定标签的条目"
     :ref-tag nil))  ; 存储被引用的标签名
  
  ;; 选项类型
  (org-supertag-register-field-type
   'options
   '(:validator org-supertag-validate-options
     :formatter org-supertag-format-options
     :reader org-supertag-read-options-field
     :description "从预定义选项中选择"))
  
  ;; 数值类型
  (org-supertag-register-field-type
   'number
   '(:validator org-supertag-validate-number
     :formatter org-supertag-format-number
     :reader org-supertag-read-number-field
     :description "数值类型"))
  
  ;; 列表类型
  (org-supertag-register-field-type
   'list
   '(:validator org-supertag-validate-list
     :formatter org-supertag-format-list
     :reader org-supertag-read-list-field
     :description "列表类型")))

;; 初始化字段类型
(org-supertag-init-field-types)

;;----------------------------------------------------------------------
;; 字段类型的验证器和格式化器
;;----------------------------------------------------------------------

(defun org-supertag-validate-string (value)
  "验证字符串 VALUE。"
  (message "Debug - Validating string value: %S (type: %S)" value (type-of value))
  (let* ((is-string (stringp value))
        (is-non-empty (and value (not (string-empty-p (string-trim value)))))
        (is-valid (and is-string is-non-empty)))
    (message "Debug - String validation: is-string=%S, is-non-empty=%S, is-valid=%S"
             is-string is-non-empty is-valid)
    is-valid))

(defun org-supertag-format-string (value field)
  "格式化字符串值。
VALUE 是要格式化的值
FIELD 是字段定义。"
  (message "Debug - Formatting string value: %S" value)
  (when value
    (string-trim value)))

(defun org-supertag-validate-date (value)
  "验证日期 VALUE。"
  (condition-case nil
      (progn 
        (org-parse-time-string value)
        t)
    (error nil)))

(defun org-supertag-format-date (value field)
  "格式化日期值。
VALUE 是要格式化的值
FIELD 是字段定义。"
  (message "Debug - Formatting date value: %S" value)
  (condition-case nil
      (let ((time (org-parse-time-string value)))
        (format-time-string "%Y-%m-%d" (apply #'encode-time time)))
    (error value)))

(defun org-supertag-validate-email (value)
  "验证邮箱 VALUE。"
  (and (stringp value)
       (string-match-p "^[^@]+@[^@]+\\.[^@]+$" value)))

(defun org-supertag-format-email (value)
  "格式化邮箱 VALUE。"
  (string-trim value))

(defun org-supertag-validate-url (value)
  "验证 URL VALUE。"
  (and (stringp value)
       (string-match-p "^https?://" value)))

(defun org-supertag-format-url (value)
  "格式化 URL VALUE。"
  (string-trim value))

(defun org-supertag-validate-reference (value)
  "验证引用值是否有效。
VALUE 应该是一个 org-id。"
  (and (stringp value)
       (org-id-find value 'marker)))

(defun org-supertag-format-reference (value field)
  "格式化引用值。
VALUE 是要格式化的值
FIELD 是字段定义。"
  (message "Debug - Formatting reference value: %S" value)
  (when-let ((marker (org-id-find value 'marker)))
    (org-with-point-at marker
      (org-get-heading t t t t))))

(defun org-supertag-validate-options (value field)
  "验证选项值。
VALUE 是要验证的值
FIELD 是字段定义，包含 :options。"
  (let ((options (plist-get field :options)))
    (message "Debug - Validating options: value=%S, options=%S" value options)
    (and (stringp value) (member value options))))

(defun org-supertag-format-options (value field)
  "格式化选项值。
VALUE 是要格式化���值
FIELD 是字段定义。"
  (message "Debug - Formatting options value: %S" value)
  (when value
    (string-trim value)))

(defun org-supertag-validate-number (value)
  "验证数值类型的值。"
  (numberp value))

(defun org-supertag-format-number (value field)
  "格式化数字值。
VALUE 是要格式化的值
FIELD 是字段定义。"
  (message "Debug - Formatting number value: %S" value)
  (when value
    (number-to-string value)))

(defun org-supertag-validate-list (value)
  "验证列表值。
VALUE 是要验证的值"
  (or (listp value)
      (and (stringp value)
           (string-match-p "^\\[.*\\]$" value))))

(defun org-supertag-format-list (value)
  "格式化列表值。
VALUE 是要格式化的值"
  (if (listp value)
      (format "%S" value)
    value))

(defun org-supertag-read-list-field (prompt)
  "读取列表类型字段值。
PROMPT 是提示信息"
  (let ((input (read-string (format "%s (用逗号分隔): " prompt))))
    (split-string input "," t "[ \t\n\r]+")))



;;----------------------------------------------------------------------
;; 字段定义操作
;;----------------------------------------------------------------------

(defun org-supertag-field-create (name props)
  "创建字段定义.
NAME 是字段名称
PROPS 是字段属���"
  ;; 确保必要的属性存在
  (unless (plist-get props :type)
    (error "Field must have a :type property"))
  
  ;; 确保 :name 属性存在
  (unless (plist-get props :name)
    (setq props (plist-put props :name name)))
  
  ;; 添加实体类型标记
  (setq props (plist-put props :entity-type :field))
  
  ;; 存字段定义
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

;;----------------------------------------------------------------------
;; 字段值操作
;;----------------------------------------------------------------------

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

(defun org-supertag-field--get-property (field-def)
  "获取 Property 类型字段的值.
FIELD-DEF 是字段定义"
  (let ((prop-name (org-supertag-field-get-org-name field-def)))
    (message "Debug - Getting property: %S" prop-name)
    (org-entry-get nil prop-name)))

(defun org-supertag-field--set-property (field-def value)
  "设置 Property 类型字段的值.
FIELD-DEF 是字段定义
VALUE 是要设置的值"
  (let* ((type (plist-get field-def :type))
         (type-spec (org-supertag-get-field-type type))
         (formatter (plist-get type-spec :formatter))
         (formatted-value (if formatter
                            (funcall formatter value)
                          value)))
    (org-entry-put nil 
                   (org-supertag-field-get-org-name field-def) 
                   formatted-value)))

(defun org-supertag-field--get-drawer (field-def)
  "获取 Drawer 类型字段的值.
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
  (let* ((drawer-name (org-supertag-field-get-org-name field-def))
         (type (plist-get field-def :type))
         (type-spec (org-supertag-get-field-type type))
         (formatter (plist-get type-spec :formatter))
         (formatted-value (if formatter
                            (funcall formatter value)
                          value)))
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
      (insert "\n:" drawer-name ":\n" formatted-value "\n:END:\n"))))

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
  (let* ((planning-type (org-supertag-field-get-org-name field-def))
         (type (plist-get field-def :type))
         (type-spec (org-supertag-get-field-type type))
         (formatter (plist-get type-spec :formatter))
         (formatted-value (if formatter
                            (funcall formatter value)
                          value)))
    (org-schedule nil formatted-value)))

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

;;----------------------------------------------------------------------
;; 字段值验证
;;----------------------------------------------------------------------

;; 改进字段验证
(defun org-supertag--validate-field-def (field)
  "验证字段定义是否有效。
FIELD 是字段定义 plist。"
  (and (plist-get field :name)
       (plist-get field :type)
       (org-supertag-get-field-type (plist-get field :type))))

;; 改进字段值验证
(defun org-supertag-field-validate (field value)
  "验证字段值。
FIELD 是字段定义，VALUE 是要验证的值。
返回 (t . nil) 表示验证通过，(nil . error-message) 表示验证失败。"
  (cl-block org-supertag-field-validate
    (let* ((name (plist-get field :name))
           (type (plist-get field :type))
           (required (plist-get field :required))
           (type-def (org-supertag-get-field-type type))
           (validator (plist-get type-def :validator)))
      
      (message "Debug - Validating field: %S with value: %S (type: %S)" field value (type-of value))
      
      ;; 处理空值
      (when (and required (null value))
        (message "Debug - Required field is nil")
        (cl-return-from org-supertag-field-validate
          (cons nil (format "字段 '%s' 是必填的" name))))
      
      ;; 如果值为空且不是必填，则验证通过
      (when (null value)
        (message "Debug - Optional field is nil")
        (cl-return-from org-supertag-field-validate
          (cons t nil)))
      
      ;; 类型检查
      (when (and (eq type 'string) (not (stringp value)))
        (message "Debug - Type mismatch: expected string, got %S" (type-of value))
        (cl-return-from org-supertag-field-validate
          (cons nil (format "字段 '%s' 需要字符串类型，但得到了 %S" name (type-of value)))))
      
      ;; 使用类型验证器
      (if validator
          (condition-case err
              (let ((valid (if (eq type 'options)
                             ;; 选项类型需要传递字段定义
                             (funcall validator value field)
                           ;; 其他类型只需要值
                           (funcall validator value))))
                (message "Debug - Validator result: %S" valid)
                (if valid
                    (cons t nil)
                  (cons nil (format "值 '%s' 不符合%s类型的要求" value type))))
            (error
             (message "Debug - Validation error: %S" err)
             (cons nil (format "验证字段 '%s' 时出错: %s" name (error-message-string err)))))
        ;; 没有验证器，默认通过
        (cons t nil)))))

;;----------------------------------------------------------------------
;; 字段值读取
;;----------------------------------------------------------------------

;; 修改字段值读取函数
(defun org-supertag-field-read-value (field)
  "读取字段值。
FIELD 是字段定义"
  (let* ((name (plist-get field :name))
         (type (plist-get field :type))
         (type-def (org-supertag-get-field-type type))
         (reader (plist-get type-def :reader))
         (formatter (plist-get type-def :formatter))
         (required (plist-get field :required))
         (options (plist-get field :options))
         result)
    
    (message "Debug - Reading field: %S" field)
    (message "Debug - Field type definition: %S" type-def)
    (message "Debug - Using reader: %S" reader)
    
    (unless reader
      (error "字段类型 %s 没有定义读取函数" type))
    
    (catch 'done
      (while t
        (condition-case err
            (let* ((input-value
                    (if (eq type 'options)
                        (funcall reader name options)
                      (funcall reader name)))
                   (typed-value (cond
                               ((eq type 'string) (if (stringp input-value)
                                                    input-value
                                                  (format "%s" input-value)))
                               (t input-value))))
              (message "Debug - Raw input value: %S (type: %S)" input-value (type-of input-value))
              (message "Debug - Typed value: %S (type: %S)" typed-value (type-of typed-value))
              ;; 验证值
              (let ((validation (org-supertag-field-validate field typed-value)))
                (message "Debug - Validation result: %S" validation)
                (if (car validation)  ; 这里只取验证结果的布尔值部分
                    (progn
                      ;; 格式化值
                      (setq result (if formatter
                                     (condition-case err
                                         (funcall formatter typed-value field)
                                       (error
                                        (message "Error - 格式化字段 %s 时出错: %s" 
                                                name (error-message-string err))
                                        typed-value))
                                   typed-value))
                      (message "Debug - Valid value stored: %S (type: %S)" result (type-of result))
                      (throw 'done result))  ; 返回实际值，而不是验证结果
                  (progn
                    (message "Error - %s" (cdr validation))
                    (unless (or required
                              (y-or-n-p (format "字段 %s 验证失败。重试? " name)))
                      (throw 'done nil))
                    (sit-for 1)))))
          
          (error
           (let ((err-msg (error-message-string err)))
             (message "Error - 处理字段 %s 时出错: %s" name err-msg)
             (unless (or required
                       (y-or-n-p (format "处理字段 %s 时出错。重试? " name)))
               (throw 'done nil))
             (sit-for 1)))))
    
    (message "Debug - Final result for field %s: %S (type: %S)" name result (type-of result))
    result)))

;; 字段类型的输入函数

;;; 添加默认值处理函数
(defun org-supertag-field-get-default-value (type)
  "根据字段类型获取默认值.
TYPE 是字段类型"
  (pcase type
    ;; timestamp 类型自动使用当前时间
    ('timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
    ;; 其他类型默认返回 nil
    (_ nil)))


(defun org-supertag-read-string-field (prompt)
  "读取字符串类型字段值。
PROMPT 是提示信息"
  (message "Debug - Reading string field with prompt: %s" prompt)
  (let* ((raw-input (read-string (format "%s: " prompt)))
         (trimmed-input (string-trim raw-input))
         (result (if (string-empty-p trimmed-input)
                    nil
                  trimmed-input)))
    (message "Debug - String field: raw-input=%S, trimmed=%S, result=%S"
             raw-input trimmed-input result)
    result))

(defun org-supertag-read-date-field (prompt &optional default)
  "读取日期类型字段值。
PROMPT 是提示信息
DEFAULT 是默认值"
  (let ((input (org-read-date nil t nil prompt nil default)))
    (if (org-supertag-validate-date input)
        input
      (progn
        (message "输入的不是有效的日期格式 (YYYY-MM-DD)，请重新输入")
        (sit-for 1)
        (org-supertag-read-date-field prompt default)))))

(defun org-supertag-read-email-field (prompt &optional default)
  "读取邮箱类型字段值。
PROMPT 是提示信息
DEFAULT 是默认值"
  (let ((input (read-string (format "%s (example@domain.com)%s: "
                                   prompt
                                   (if default
                                       (format " (默认: %s)" default)
                                     ""))
                           nil nil default)))
    (if (org-supertag-validate-email input)
        input
      (progn
        (message "输入的不是有效的邮箱地址，请重新输入")
        (sit-for 1)
        (org-supertag-read-email-field prompt default)))))

(defun org-supertag-read-url-field (prompt &optional default)
  "读取URL类型字段值。
PROMPT 是提示信息
DEFAULT 是默认值"
  (let ((input (read-string (format "%s (https://example.com)%s: "
                                   prompt
                                   (if default
                                       (format " (默认: %s)" default)
                                     ""))
                           nil nil default)))
    (if (org-supertag-validate-url input)
        input
      (progn
        (message "输入的不是有效的URL地址，请重新输入")
        (sit-for 1)
        (org-supertag-read-url-field prompt default)))))

(defun org-supertag-read-reference-field (prompt &optional default)
  "读取引用类型字段值。
PROMPT 是提示信息
DEFAULT 是默认值"
  (let* ((completion-table (org-supertag-get-all-node-ids))
         (input (completing-read (format "%s (输入节点ID)%s: "
                                       prompt
                                       (if default
                                           (format " (默认: %s)" default)
                                         ""))
                               completion-table
                               nil t nil nil default)))
    (if (org-supertag-validate-reference input)
        input
      (progn
        (message "输入的不是有效的节点引用，请重新输入")
        (sit-for 1)
        (org-supertag-read-reference-field prompt default)))))

(defun org-supertag-read-options-field (prompt options)
  "读取选项类型字段值。
PROMPT 是提示信息
OPTIONS 是可选值列表"
  (let ((input (completing-read (format "%s (%s): " 
                                      prompt 
                                      (mapconcat #'identity options "/"))
                              options
                              nil t)))
    (if (member input options)
        input
      (progn
        (message "请从给定���项中选择一个值")
        (sit-for 1)
        (org-supertag-read-options-field prompt options)))))

(defun org-supertag-read-number-field (prompt &optional default min max)
  "读取数值类型字段值。
PROMPT 是提示信息
DEFAULT 是默认值
MIN 是最小值
MAX 是最大值"
  (let* ((range-prompt (cond
                       ((and min max) (format " (%s-%s)" min max))
                       (min (format " (>=%s)" min))
                       (max (format " (<=%s)" max))
                       (t "")))
         (input-str (read-string (format "%s%s%s: "
                                        prompt
                                        range-prompt
                                        (if default
                                            (format " (默认: %s)" default)
                                          ""))
                                nil nil default))
         (input-num (condition-case nil
                       (string-to-number input-str)
                     (error nil))))
    (if (and input-num
             (numberp input-num)
             (or (null min) (>= input-num min))
             (or (null max) (<= input-num max)))
        input-num
      (progn
        (message "请输入有效的数值%s" range-prompt)
        (sit-for 1)
        (org-supertag-read-number-field prompt default min max)))))

;;----------------------------------------------------------------------
;; 同步机制
;;----------------------------------------------------------------------

(defun org-supertag-field-sync-node (node-id)
  "同步节点的所有字段.
NODE-ID 是节点ID"
  (org-with-point-at (org-id-find node-id t)
    ;; 获取节点所有标签
    (let* ((tags (org-supertag-node-get-tags node-id))
           (all-fields (mapcan #'org-supertag-tag-get-fields tags)))
      ;; 同步每个字段
      (dolist (field all-fields)
        (when-let ((value (org-supertag-field-get-value field)))
          (org-supertag-set-field node-id 
                                 (plist-get field :name)
                                 value))))))


;; 改进段交互式编辑
(defun org-supertag--field-interactive-edit (&optional current-fields)
  "交互式编辑字段定义。
CURRENT-FIELDS 是当前的字段列表。"
  (let ((fields (or current-fields nil))
        (field-types (org-supertag-get-field-types)))
    
    ;; 显示当前字段
    (when fields
      (message "当前字段:")
      (dolist (field fields)
        (message "  %s (%s)"
                 (plist-get field :name)
                 (plist-get field :type))))
    
    ;; 添加新字段
    (while (not (equal "完成" 
                      (completing-read "添加字段 (或选择'完成'): "
                                     '("+ 添加字段" "完成")
                                     nil t)))
      (let* ((field-name (read-string "字段名称: "))
             ;; 检查字段名唯一性
             (_ (when (cl-find field-name fields
                              :key (lambda (f) (plist-get f :name))
                              :test #'string=)
                  (user-error "字段名 %s 已存在" field-name)))
             ;; 选择字段类型
             (field-type (completing-read "字段类型: "
                                        (mapcar #'symbol-name field-types)
                                        nil t))
             (type-sym (intern field-type))
             (type-def (org-supertag-get-field-type type-sym))
             ;; 创建基本字段定义
             (field-def (list :name field-name
                            :type type-sym
                            :required (y-or-n-p "是否必填? "))))
        
        ;; 添加类型特定的属性
        (pcase type-sym
          ;; 选项类型需要定义选值
          ('options
           (let ((options (split-string
                          (read-string "输入选项 (用逗号分隔): ")
                          "," t "[ \t\n\r]+")))
             (setq field-def (plist-put field-def :options options))))
          
          ;; 引用类型需要定义引用的标签
          ('reference
           (let ((ref-tag (completing-read "引用哪个标签的条目? "
                                         (org-supertag-get-all-tags))))
             (setq field-def (plist-put field-def :ref-tag ref-tag))))
          
          ;; 数值类型可以设置范围
          ('number
           (when (y-or-n-p "是否设置数值范围? ")
             (let ((min (read-number "最小值: "))
                   (max (read-number "最大值: ")))
               (setq field-def (plist-put field-def :min min))
               (setq field-def (plist-put field-def :max max))))))
        
        ;; 添加验证器和格式化器
        (when-let ((validator (plist-get type-def :validator)))
          (setq field-def (plist-put field-def :validate validator)))
        (when-let ((formatter (plist-get type-def :formatter)))
          (setq field-def (plist-put field-def :formatter formatter)))
        
        ;; 添加到字段列表
        (push field-def fields)))
    
    ;; 返回字段列表
    (nreverse fields)))



(provide 'org-supertag-field)
;;; org-supertag-field.el ends here

