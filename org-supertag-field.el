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
  '((string . (:validator org-supertag-validate-string
               :formatter org-supertag-format-string
               :reader org-supertag-read-string-field
               :description "基础字符串类型"))
    (options . (:validator org-supertag-validate-options
                :formatter org-supertag-format-options
                :reader org-supertag-read-options-field
                :description "从预定义选项中选择"))
    (number . (:validator org-supertag-validate-number
               :formatter org-supertag-format-number
               :reader org-supertag-read-number-field
               :description "数字类型"))
    (date . (:validator org-supertag-validate-date
             :formatter org-supertag-format-date
             :reader org-supertag-read-date-field
             :description "日期类型"))
    (reference . (:validator org-supertag-validate-reference
                  :formatter org-supertag-format-reference
                  :reader org-supertag-read-reference-field
                  :description "引用其他条目"))
    (tags . (:validator org-supertag-validate-tags
             :formatter org-supertag-format-tags
             :reader org-supertag-read-tags-field
             :description "标签列表"))
    (list . (:validator org-supertag-validate-list
             :formatter org-supertag-format-list
             :reader org-supertag-read-list-field
             :description "可自由输入的列表类型")))
  "字段类型定义。
每个类型是一个 cons cell，car 是类型名称（symbol），cdr 是类型定义 plist，包含：
- :validator  验证函数
- :formatter  格式化函数
- :reader     读取函数
- :description  类型描述")

(defun org-supertag-get-field-types ()
  "获取所有支持的字段类型。
返回 ((显示名 . 类型符号) ...) 的列表。"
  (mapcar (lambda (type-def)
            (let* ((type-name (car type-def))
                   (type-spec (cdr type-def))
                   (description (plist-get type-spec :description)))
              (cons (or description (symbol-name type-name))
                    type-name)))
          org-supertag-field-types))

(defun org-supertag-get-field-type (type)
  "获取字段类型定义。
TYPE 是字段类型的符号。
返回类型定义 plist，包含 :validator、:formatter、:reader 和 :description。"
  (message "Debug - Getting field type: %S" type)
  (let ((type-def (alist-get type org-supertag-field-types)))
    (message "Debug - Field type definition: %S" type-def)
    type-def))

;;----------------------------------------------------------------------
;; 字段类型的验证器和格式化器
;;----------------------------------------------------------------------

(defun org-supertag-validate-string (value)
"验证字符串 VALUE。"
  (message "Debug - Validating string value: %S (type: %S)" value (type-of value))
  (let ((is-string (stringp value))
        (is-non-empty (and value (not (string-empty-p (string-trim value))))))
        (message "Debug - String validation: is-string=%S, is-non-empty=%S"
    is-string is-non-empty)
(and is-string is-non-empty)))



(defun org-supertag-format-string (value field)
  "格式化字符串值。
VALUE 是要格式化的值
FIELD 是字段定义。"
  (message "Debug - Formatting string value: %S" value)
  (when value
    (string-trim value)))

(defun org-supertag-validate-date (value)
  "验证日期值是否有效.
VALUE 是要验证的值"
  (message "Debug - Validating date value: %S" value)
  (condition-case err
      (progn
        ;; org-read-date 返回的是 time value
        (when (listp value)
          (setq value (format-time-string "%Y-%m-%d" value)))
        ;; 如果是字符串，尝试解析
        (when (stringp value)
          (org-parse-time-string value))
        t)
    (error
     (message "Date validation error: %S" err)
     nil)))

(defun org-supertag-format-date (value field)
  "格式化日期值。
VALUE 是要格式化的值
FIELD 是字段定义。"
  (message "Debug - Formatting date value: %S" value)
  (condition-case err
      (cond
       ;; 处理 org-read-date 返回的 time value
       ((listp value)
        (format-time-string "%Y-%m-%d" value))
       ;; 处理字符串格式
       ((stringp value)
        (let ((time (org-parse-time-string value)))
          (format-time-string "%Y-%m-%d" (apply #'encode-time time))))
       ;; 其他情况返回原值
       (t value))
    (error
     (message "Date formatting error: %S" err)
     value)))

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
  "验证引用值是否有效.
VALUE 应该是一个 org-id。"
  (message "Debug - Validating reference value: %S" value)
  (when value
    (let ((node-ids (org-supertag-get-all-node-ids)))
      (message "Debug - Available node IDs: %S" node-ids)
      (member value node-ids))))

(defun org-supertag-format-reference (value field)
  "格式化引用值。
VALUE 是要格式化的值
FIELD 是字段定义。"
  (message "Debug - Formatting reference value: %S" value)
  (when value
    (let ((node-ids (org-supertag-get-all-node-ids)))
      (if (member value node-ids)
          (org-with-point-at (org-id-find value t)
            (org-get-heading t t t t))
        value))))

(defun org-supertag-validate-options (value field)
  "验证选项值。
VALUE 是要验证的值
FIELD 是字段定义，包含 :options。"
  (let ((options (plist-get field :options)))
    (message "Debug - Validating options: value=%S, options=%S" value options)
    (and (stringp value) (member value options))))

(defun org-supertag-format-options (value field)
  "格式化选项值。
VALUE 是要格式化的值
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
VALUE 是要验证的值。"
  (message "Debug - Validating list value: %S" value)
  (or (listp value)
      (and (stringp value)
           (string-match-p "^\\[.*\\]$" value))))

(defun org-supertag-format-list (value field)
  "格式化列表值。
VALUE 是要格式化的值
FIELD 是字段定义。"
  (message "Debug - Formatting list value: %S" value)
  (cond
   ((listp value)
    (format "%S" value))
   ((stringp value)
    (if (string-match-p "^\\[.*\\]$" value)
        value
      (format "[%s]" value)))
   (t (format "[%s]" value))))

(defun org-supertag-read-list-field (prompt)
  "读取列表类型字段值。
PROMPT 是提示信息。"
  (message "Debug - Reading list field with prompt: %s" prompt)
  (let* ((input (read-string (format "%s (用逗号分隔): " prompt)))
         (values (split-string input "," t "[ \t\n\r]+")))
    (message "Debug - List field input: %S -> %S" input values)
    values))



;;----------------------------------------------------------------------
;; 字段定义操作
;;----------------------------------------------------------------------

(defun org-supertag-sanitize-name (name)
  "将名称转换为有效的 Org property 名称.
NAME 是要转换的名称"
  (if (or (null name) (string-empty-p name))
      (error "Field name cannot be empty")
    (let* ((trimmed (string-trim name))
           (underscored (replace-regexp-in-string "\\s-+" "_" trimmed))
           (sanitized (replace-regexp-in-string "[^A-Za-z0-9_]" "" underscored)))
      (if (string-empty-p sanitized)
          (error "Invalid field name after sanitization: %s" name)
        (upcase sanitized)))))

(defun org-supertag-validate-name (name)
  "验证名称是否为有效的 Org property 名称.
NAME 是要验证的名称"
  (and (stringp name)
       (not (string-empty-p name))
       (let ((valid-name-regex "^[A-Za-z][A-Za-z0-9_]*$"))
         (string-match-p valid-name-regex name))))

(defun org-supertag-field-create (name props)
  "创建字段定义.
NAME 是字段名称
PROPS 是字段属性"
  (when (or (null name) (string-empty-p name))
    (error "Field name cannot be empty"))
  
  ;; 验证并规范化名称
  (let ((sanitized-name (org-supertag-sanitize-name name)))
    (unless (org-supertag-validate-name sanitized-name)
      (error "Invalid field name after sanitization: %s" name))
    
    ;; 确保必要的属性存在
    (unless (plist-get props :type)
      (error "Field must have a :type property"))
    
    ;; 确保 :name 属性存在且合法
    (setq props (plist-put props :name sanitized-name))
    
    ;; 添加实体类型标记
    (setq props (plist-put props :entity-type :field))
    
    ;; 存储字段定义
    (let ((result (org-supertag-db-put sanitized-name props)))
      (message "Debug - Field creation stored: %S" result)
      result)))

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
      (:todo (org-supertag-field--get-todo))
      (:priority (org-supertag-field--get-priority))
      (:tags (org-supertag-field--get-tags))
      (_ (error "Unsupported field type: %s" type)))))

(defun org-supertag-field-set-value (field-def value)
  "设置字段值.
FIELD-DEF 是字段定义
VALUE 是要设置的值"
  (let* ((type (plist-get field-def :type))
         (type-spec (org-supertag-get-field-type type))
         (storage (plist-get type-spec :storage)))
    (message "Debug - Setting value for field: %S" field-def)
    (message "Debug - Field type: %S, storage: %S" type storage)
    (message "Debug - Value to set: %S" value)))

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
      ;; 除现有抽屉
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



(defun org-supertag-field--get-todo ()
  "获取 TODO 状态."
  (org-get-todo-state))

(defun org-supertag-field--set-todo (value)
  "设置 TODO 状态.
VALUE 是要设置的状态"
  (org-todo value))

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
;; (defun org-supertag--validate-field-def (field)
;;   "验证字段定义是否有效。
;; FIELD 是字段定义 plist。"
;;   (and (plist-get field :name)
;;        (plist-get field :type)
;;        (org-supertag-get-field-type (plist-get field :type))))

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
FIELD 是字段定义，可以是 plist、向量或 cons cell"
  (message "Debug - Parsing field: %S" field)
  (let ((field-def nil))
    ;; 如果是向量，转换为列表
    (when (vectorp field)
      (setq field (append field nil)))
    
    ;; 处理字段定义
    (cond
     ;; 处理 cons cell 格式 ("name" . type)
     ((and (consp field) 
           (not (listp (cdr field))))  ; 确保是真正的 cons cell
      (message "Debug - Processing cons cell format: %S" field)
      (let ((name (car field))
            (type (cdr field)))
        (setq field-def
              (list :name (if (stringp name) name (symbol-name name))
                    :type (if (keywordp type)
                             (intern (substring (symbol-name type) 1))
                           type)))))
     
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
  "验证字段定义是否有效。
FIELD 是字段定义"
  (let ((name (plist-get field :name))
        (type (plist-get field :type)))
    (message "Debug - Validating field: %S" field)
    (message "Debug - Field type: %S" type)
    (when (and name type)
      (let ((type-def (org-supertag-get-field-type type)))
        (message "Debug - Field type definition: %S" type-def)
        (when type-def
          (message "Debug - Field validation result: %s" type)
          type)))))

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
      (error "字段类型 %s 有定义读取函数" type))
    
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
  (let* ((input (org-read-date nil t nil prompt nil default))
         (formatted-date (format-time-string "%Y-%m-%d" input)))
    (message "Debug - Date input from org-read-date: %S -> %S" input formatted-date)
    formatted-date))

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

(defun org-supertag-read-reference-field (prompt)
  "读取引用类型字段值。
PROMPT 是提示信息"
  (message "Debug - Reading reference field...")
  (let ((node-ids (org-supertag-get-all-node-ids)))
    (if node-ids
        (let* ((nodes-with-titles 
                (mapcar (lambda (id)
                         (cons (org-with-point-at (org-id-find id t)
                                (org-get-heading t t t t))
                               id))
                       node-ids))
               (choice (completing-read prompt (mapcar #'car nodes-with-titles) nil t)))
          (cdr (assoc choice nodes-with-titles)))
      (user-error "没有可用的节点可供引用"))))

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
        (message "请从给定选项中选择一个值")
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
        input-num
      (progn
        (message "请输入有效的数值%s" range-prompt)
        (sit-for 1)
        (org-supertag-read-number-field prompt default min max))))))



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



;;----------------------------------------------------------------------
;; 预设字段定义
;;----------------------------------------------------------------------

(defcustom org-supertag-preset-fields
  '(("Priority" 
     :type options
     :values ("P1" "P2" "P3" "P4")
     :description "Task Priority")
    ("Status" 
     :type options
     :values ("To Do" "In Progress" "Done" "On Hold")
     :description "Task Status")
    ("Deadline" 
     :type date
     :description "Task Deadline")
    ("Progress" 
     :type number
     :min 0
     :max 100
     :description "Task Completion Percentage")
    ("Who" 
     :type string
     :description "Who is this")
    ("Tags" 
     :type list
     :separator ","
     :description "Keywords or Tags")
    ("URL" 
     :type url
     :description "Related Links")
    ("Rating" 
     :type number
     :min 1 
     :max 5
     :description "Five-star Rating"))
  "预设字段定义列表.
每个预设字段是一个 (name . props) 对，其中：
- name 是字段名称
- props 是字段属性 plist"
  :type '(repeat (cons string plist))
  :group 'org-supertag)

(defun org-supertag--parse-preset-field (field-def)
  "解析预设字段定义。
FIELD-DEF 是预设字段定义"
  (let* ((name (car field-def))
         (props (cdr field-def)))
    (message "Debug - 解析预设字段: %s" name)
    (list :name (org-supertag--sanitize-field-name name)
          :display-name name
          :type (plist-get props :type)
          :options (plist-get props :values)  ;; 注意这里使用 :values
          :description (plist-get props :description))))

(defun org-supertag-get-preset-field (name)
  "获取预设字段定义。
NAME 是字段名称"
  (when-let ((field-def (assoc name org-supertag-preset-fields)))
    (org-supertag--parse-preset-field field-def)))

(defun org-supertag-format-preset-field (preset)
  "格式化预设字段选项。
PRESET 是预设字段定义"
  (let ((name (car preset))
        (desc (plist-get (cdr preset) :description)))
    (format "- %s%s"
            name
            (if desc (format " (%s)" desc) ""))))
(provide 'org-supertag-field)
;;; org-supertag-field.el ends here

