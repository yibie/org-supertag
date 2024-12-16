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
(require 'org-supertag-db)

;;------------------------------------------------------------------------------
;; 字段值数据库操作
;;------------------------------------------------------------------------------

(defun org-supertag-field-db-set-value (node-id field-name value tag-id)
  "Set field value to database.
NODE-ID: 节点ID
FIELD-NAME: 字段名
VALUE: 字段值
TAG-ID: 所属标签ID"
  (let ((link-id (format ":node-field:%s->%s" node-id field-name))
        (props (list :from node-id
                    :to field-name
                    :tag-id tag-id
                    :value value
                    :created-at (current-time))))
    (puthash link-id props org-supertag-db--link)))

(defun org-supertag-field-db-get-value (node-id field-name tag-id)
  "Get field value from database.
NODE-ID: 节点ID
FIELD-NAME: 字段名
TAG-ID: 所属标签ID"
  (let* ((link-id (format ":node-field:%s->%s" node-id field-name))
         (link (gethash link-id org-supertag-db--link)))
    (when (and link 
               (equal (plist-get link :tag-id) tag-id))
      (plist-get link :value))))

(defun org-supertag-field-db-delete-value (node-id field-name tag-id)
  "删除字段值.
NODE-ID: 节点ID
FIELD-NAME: 字段名
TAG-ID: 所属标签ID"
  (let ((link-id (format ":node-field:%s->%s" node-id field-name)))
    (remhash link-id org-supertag-db--link)))

;;----------------------------------------------------------------------
;; 字段值实体操作
;;----------------------------------------------------------------------

(defun org-supertag-field-set-value (field-def value node-id tag-id)
  "设置字段值.
FIELD-DEF: 字段定义
VALUE: 字段值
NODE-ID: 节点ID
TAG-ID: 所属标签ID"
  (condition-case err
      (let* ((field-name (plist-get field-def :name))
             (field-type (plist-get field-def :type))
             (type-def (org-supertag-get-field-type field-type))
             ;; 直接验证和格式化值
             (processed-value (if type-def
                                (progn
                                  (org-supertag-field-validate field-def value)
                                  (if-let ((formatter (plist-get type-def :formatter)))
                                      (funcall formatter value field-def)
                                    value))
                              value)))
        ;; 1. 存储到数据库
        (org-supertag-field-db-set-value node-id field-name processed-value tag-id)
        
        ;; 2. 尝试同步到 org 文件
        (when-let ((pos (condition-case nil
                           (org-id-find node-id t)
                         (error nil))))  ; 如果找不到节点，返回nil
          (condition-case sync-err
              (org-with-point-at pos
                (org-entry-put nil field-name processed-value))
            (error
             ;; 记录同步错误但不影响数据库操作
             (message "Failed to sync field value to org buffer: %s"
                     (error-message-string sync-err))))))
    (error
     ;; 处理主要操作错误
     (message "Error in field set-value operation: %s"
              (error-message-string err))
     (signal (car err) (cdr err)))))

(defun org-supertag-field-validate (field value)
  "Validate field value.
FIELD 是字段定义，VALUE 是要验证的值。
验证通过返回 t，否则抛出错误。"
  (let* ((name (plist-get field :name))
         (type (plist-get field :type))
         (required (plist-get field :required))
         (type-def (org-supertag-get-field-type type))
         (validator (plist-get type-def :validator)))
    ;; 处理空值
    (when (and required (null value))
      (error "字段 '%s' 是必填的" name))
    ;; 如果值为空且不是必填，则验证通过
    (when (null value)
      (cl-return-from org-supertag-field-validate t))
    ;; 类型检查
    (when (and (eq type 'string) (not (stringp value)))
      (error "字段 '%s' 需要字符串类型，但得到了 %S" name (type-of value)))
    ;; 使用类型验证器
    (when validator
      (condition-case err
          (unless (if (eq type 'options)
                     (funcall validator value field)
                   (funcall validator value))
            (error "值 '%s' 不符合%s类型的要求" value type))
        (error
         (error "验证字段 '%s' 时出错: %s" name (error-message-string err)))))
    t))

(defun org-supertag-field-get-value (field-def node-id tag-id)
  "Get field value.
FIELD-DEF: field definition
NODE-ID: node ID
TAG-ID: tag ID"
  (let* ((field-name (plist-get field-def :name))
         (type (plist-get field-def :type))
         (type-spec (org-supertag-get-field-type type))
         ;; 从数据库获取值
         (value (org-supertag-field-db-get-value node-id field-name tag-id)))
    ;; 如果有值且有格式化器，进行格式化
    (when (and value
               (plist-get type-spec :formatter))
      (setq value (funcall (plist-get type-spec :formatter) value field-def)))
    value))

(defun org-supertag-field-remove-value (field-def node-id tag-id)
  "Remove field value.
FIELD-DEF: field definition
NODE-ID: node ID
TAG-ID: tag ID"
  (let ((field-name (plist-get field-def :name)))
    ;; 1. 从数据库删除
    (org-supertag-db-remove-link :node-field node-id field-name
                                `(:tag-id ,tag-id))
    
    ;; 2. 从 org 文件删除
    (org-with-point-at (org-id-find node-id t)
      (org-entry-delete nil field-name))))

(defun org-supertag-field-batch-set-value (field-def-list value-list node-id tag-id)
  "Batch set field values.
FIELD-DEF-LIST: field definition list
VALUE-LIST: value list
NODE-ID: node ID
TAG-ID: tag ID"
  (cl-loop for field-def in field-def-list
           for value in value-list
           do (org-supertag-field-set-value field-def value node-id tag-id)))

(defun org-supertag-field-find-nodes (field-name value &optional tag-id)
  "Find nodes with specific field value.
FIELD-NAME: field name
VALUE: field value
TAG-ID: optional tag ID, for limiting query range"
  (let ((links (org-supertag-db-get-links :node-field)))
    (cl-remove-if-not
     (lambda (link)
       (and (equal (car link) field-name)  ; 字段名匹配
            (equal (plist-get (cadr link) :value) value)  ; 值匹配
            (or (null tag-id)  ; 如果没指定标签ID
                (equal (plist-get (cadr link) :tag-id) tag-id))))  ; 标签ID匹配
     links)))

;;----------------------------------------------------------------------
;; Field Type System
;;----------------------------------------------------------------------

(defvar org-supertag-field-types
  '((string . (:validator org-supertag-validate-string
               :formatter org-supertag-format-string
               :reader org-supertag-read-string-field
               :description "Plain text"))
    (options . (:validator org-supertag-validate-options
                :formatter org-supertag-format-options
                :reader org-supertag-read-options-field
                :description "Options"))
    (number . (:validator org-supertag-validate-number
               :formatter org-supertag-format-number
               :reader org-supertag-read-number-field
               :description "Number"))
    (date . (:validator org-supertag-validate-date
             :formatter org-supertag-format-date
             :reader org-supertag-read-date-field
             :description "Date"))
    (timestamp . (:validator org-supertag-validate-timestamp
             :formatter org-supertag-format-timestamp
             :reader org-supertag-read-timestamp-field
             :description "Timestamp"))
    (reference . (:validator org-supertag-validate-reference
                  :formatter org-supertag-format-reference
                  :reader org-supertag-read-reference-field
                  :description "Reference"))
    (tags . (:validator org-supertag-validate-tags
             :formatter org-supertag-format-tags
             :reader org-supertag-read-tags-field
             :description "Tags"))
    (list . (:validator org-supertag-validate-list
             :formatter org-supertag-format-list
             :reader org-supertag-read-list-field
             :description "List"))
    (range . (:validator org-supertag-validate-range
              :formatter org-supertag-format-range
              :reader org-supertag-read-range-field
              :description "Number Range")))
  "Field type definition.
Each type is a cons cell, car is the type name (symbol), cdr is the type definition plist, containing:
- :validator  验证函数
- :formatter  格式化函数
- :reader     读取函数
- :description  类型描述")

(defun org-supertag-get-field-types ()
  "Get all supported field types.
Return a list of ((display-name . type-symbol) ...)."
  (mapcar (lambda (type-def)
            (let* ((type-name (car type-def))
                   (type-spec (cdr type-def))
                   (description (plist-get type-spec :description)))
              (cons (or description (symbol-name type-name))
                    type-name)))
          org-supertag-field-types))

(defun org-supertag-get-field-type (type)
  "Get field type definition.
TYPE is the type symbol.
Return the type definition plist, containing :validator, :formatter, :reader and :description."
  (message "Debug - Getting field type: %S" type)
  (let ((type-def (alist-get type org-supertag-field-types)))
    (message "Debug - Field type definition: %S" type-def)
    type-def))

;;----------------------------------------------------------------------
;; Field Value Validators and Formatters
;;----------------------------------------------------------------------

(defun org-supertag-validate-string (value)
  "验证字符串值.
VALUE: 要验证的值"
  (let ((is-string (stringp value))
        (is-non-empty (and value 
                          (not (string-empty-p (string-trim value))))))
    (and is-string is-non-empty)))

(defun org-supertag-format-string (value field)
  "Format string VALUE.
VALUE is the value to format.
FIELD is the field definition."
  (message "Debug - Formatting string value: %S" value)
  (when value
    (string-trim value)))

(defun org-supertag-validate-date (value)
  "Validate date VALUE.
VALUE is the value to validate."
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

(defun org-supertag-validate-timestamp (value field-def)
  "验证时间戳值.
VALUE: 时间戳值
FIELD-DEF: 字段定义"
  (condition-case nil
      (when value
        (org-parse-time-string value)  ; 尝试解析时间字符串
        t)
    (error nil)))

(defun org-supertag-format-timestamp (value field-def)
  "格式化时间戳值.
VALUE: 时间戳值
FIELD-DEF: 字段定义"
  (when value
    (let ((time (org-parse-time-string value)))
      (format-time-string "%Y-%m-%d %H:%M" 
                         (apply #'encode-time time)))))

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
  "验证数值类型的值。
VALUE 是要验证的值，可以是数字或数字字符串"
  (or (numberp value)
      (and (stringp value)
           (string-match-p "^[0-9.]+$" value))))

(defun org-supertag-format-number (value field)
  "格式化数字值。
VALUE 是要格式化的值，可以是数字或数字字符串
FIELD 是字段定义。"
  (message "Debug - Formatting number value: %S" value)
  (when value
    (if (numberp value)
        (number-to-string value)
      (if (string-match-p "^[0-9.]+$" value)
          value
        (number-to-string (string-to-number value))))))

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

(defun org-supertag-validate-range (value)
  "验证数值范围.
VALUE 应该是 (MIN . MAX) 格式的 cons cell"
  (and (consp value)
       (numberp (car value))
       (numberp (cdr value))
       (<= (car value) (cdr value))))

(defun org-supertag-format-range (value _field)
  "格式化数值范围.
VALUE 是 (MIN . MAX) 格式的 cons cell"
  (format "%s-%s" (car value) (cdr value)))

(defun org-supertag-read-range-field (prompt &optional default)
  "读取数值范围.
PROMPT 是提示信息
DEFAULT 是默认值，格式为 (MIN . MAX)"
  (let* ((default-min (and default (car default)))
         (default-max (and default (cdr default)))
         (min-str (read-string 
                  (format "%s 最小值%s: "
                          prompt
                          (if default-min
                              (format " (默认: %s)" default-min)
                            ""))
                  nil nil 
                  (and default-min (number-to-string default-min))))
         (max-str (read-string 
                  (format "%s 最大值%s: "
                          prompt
                          (if default-max
                              (format " (默认: %s)" default-max)
                            ""))
                  nil nil
                  (and default-max (number-to-string default-max)))))
    ;; 验证输入
    (if (and (string-match-p "^[0-9.]+$" min-str)
             (string-match-p "^[0-9.]+$" max-str))
        (let ((min (string-to-number min-str))
              (max (string-to-number max-str)))
          (if (<= min max)
              (cons min max)
            (progn
              (message "最小值必须小于或等于最大值")
              (sit-for 1)
              (org-supertag-read-range-field prompt default))))
      (progn
        (message "请输入有效的数字")
        (sit-for 1)
        (org-supertag-read-range-field prompt default)))))

;;----------------------------------------------------------------------
;; Read Field Value
;;----------------------------------------------------------------------

(defun org-supertag-field-read-value (field)
  "读取字段值。
FIELD 是字段定义"
  (let* ((name (plist-get field :name))
         (type (plist-get field :type))
         (type-def (org-supertag-get-field-type type))
         (reader (plist-get type-def :reader))
         (formatter (plist-get type-def :formatter))
         (required (plist-get field :required))
         (options (plist-get field :options)))
    ;; 1. 确保有读取器
    (unless reader
      (error "字段类型 %s 未定义读取函数" type))
    ;; 2. 读取值
    (catch 'done
      (while t
        (condition-case err
            (let* ((input-value
                    (if (eq type 'options)
                        (funcall reader name options)
                      (funcall reader name)))
                   (typed-value (org-supertag-field--convert-value type input-value)))
              ;; 3. 验证和格式化
              (if (org-supertag-field-validate field typed-value)
                  (throw 'done 
                         (if formatter
                             (funcall formatter typed-value field)
                           typed-value))
                (when (or required
                         (y-or-n-p (format "字段 %s 验证失败。重试? " name)))
                  (sit-for 1))))
          (error
           (let ((err-msg (error-message-string err)))
             (message "Error - 处理字段 %s 时出错: %s" name err-msg)
             (when (or required
                      (y-or-n-p (format "处理字段 %s 时出错。重试? " name)))
               (sit-for 1)))))))))

(defun org-supertag-field--convert-value (type value)
  "将值转换为指定类型.
TYPE: 目标类型
VALUE: 要转换的值"
  (let ((type-spec (org-supertag-get-field-type type)))
    (when-let ((formatter (plist-get type-spec :formatter)))
      (funcall formatter value nil))))

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

(defun org-supertag-read-timestamp-field (prompt)
  "读取时间戳类型字段值.
PROMPT 是提示信息"
  (let* ((time (org-read-date t t))  ; 使用 org-mode 的时间读取，带时间
         (ts (org-timestamp-from-time time t)))  ; 转换为 org 时间戳
    (org-timestamp-format ts "%Y-%m-%d %H:%M")))  ; 格式化为标准格式

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

(defun org-supertag-read-number-field (prompt)
  "读取数值类型字段值。
PROMPT 是提示信息"
  (let ((input-str (read-string (format "%s: " prompt))))
    ;; 验证输入是否只包含数字和小数点
    (if (string-match-p "^[0-9.]+$" input-str)
        input-str  ; 返回字符串形式的数字
      (progn
        (message "请输入有效的数字")
        (sit-for 1)
        (org-supertag-read-number-field prompt)))))

;;----------------------------------------------------------------------
;; Preset Field
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
     :description "Progress Bar")
    ("Who" 
     :type string
     :description "Who")
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
  "Preset field definition list.
Each preset field is a (name . props) pair, where:
- name is the field name
- props is the field properties plist"
  :type '(repeat (cons string plist))
  :group 'org-supertag)

(defun org-supertag--parse-preset-field (field-def)
  "Parse preset field definition.
FIELD-DEF is the preset field definition"
  (let* ((name (car field-def))
         (props (cdr field-def)))
    (append 
     (list :name name
           :type (plist-get props :type))
     ;; 可选属性
     (when-let ((desc (plist-get props :description)))
       (list :description desc))
     (when-let ((values (plist-get props :values)))
       (list :options values))
     (when-let ((min (plist-get props :min)))
       (list :min min))
     (when-let ((max (plist-get props :max)))
       (list :max max))
     (when-let ((sep (plist-get props :separator)))
       (list :separator sep)))))

(defun org-supertag-get-preset-field (name)
  "Get preset field definition.
NAME is the field name"
  (when-let ((field-def (assoc name org-supertag-preset-fields)))
    (org-supertag--parse-preset-field field-def)))

(defun org-supertag-format-preset-field (preset)
  "Format preset field options.
PRESET is the preset field definition"
  (let ((name (car preset))
        (desc (plist-get (cdr preset) :description)))
    (format "- %s%s"
            name
            (if desc (format " (%s)" desc) ""))))

;;----------------------------------------------------------------------
;;   Test
;;----------------------------------------------------------------------

(ert-deftest test-org-supertag-field-basic ()
  "测试基本的字段值操作."
  ;; 1. 先创建一个测试节点
  (org-supertag-db-add "test-node-1" 
                       (list :type :node
                            :id "test-node-1"
                            :title "Test Node"
                            :file-path "/test/path"
                            :pos 1                  ; 节点位置
                            :olp '("Root" "Parent") ; 祖先标题列表
                            :level 2                ; 层级
                            :created-at (current-time)))
  
  ;; 2. 创建一个测试标签
  (org-supertag-db-add "test-tag" 
                       (list :type :tag
                            :id "test-tag"
                            :name "Test Tag"
                            :fields nil
                            :created-at (current-time)))
  
  (let* ((node-id "test-node-1")
         (field-name "test-field")
         (tag-id "test-tag")
         (value "test-value"))
    
    ;; 3. 测试设置字段值
    (org-supertag-field-db-set-value node-id field-name value tag-id)
    (message "Stored links: %S" (org-supertag-db-get-link :node-field node-id))
    (message "Database content: %S" (ht-items org-supertag-db--link))
    (message "Get link result: %S" (org-supertag-db-get-link :node-field node-id))
    ;; 4. 测试获取字段值
    (should (equal (org-supertag-field-db-get-value node-id field-name tag-id)
                  value))))

(ert-deftest test-org-supertag-field-multiple ()
  "测试多个字段值操作."
  (let* ((node-id "test-node-1")
         (tag-id "test-tag"))
    (org-supertag-db-add "test-node-1" 
                       (list :type :node
                            :id "test-node-1"
                            :title "Test Node"
                            :file-path "/test/path"
                            :pos 1                  ; 节点位置
                            :olp '("Root" "Parent") ; 祖先标题列表
                            :level 2                ; 层级
                            :created-at (current-time)))
  
  ;; 2. 创建一个测试标签
  (org-supertag-db-add "test-tag" 
                       (list :type :tag
                            :id "test-tag"
                            :name "Test Tag"
                            :fields nil
                            :created-at (current-time)))
  
  (let* ((node-id "test-node-1")
         (field-name "test-field")
         (tag-id "test-tag")
         (value "test-value"))

    ;; 1. 设置多个字段值
    (org-supertag-field-db-set-value node-id "field1" "value1" tag-id)
    (org-supertag-field-db-set-value node-id "field2" "value2" tag-id)
    
    ;; 2. 测试获取
    (should (equal (org-supertag-field-db-get-value node-id "field1" tag-id)
                  "value1"))
    (should (equal (org-supertag-field-db-get-value node-id "field2" tag-id)
                  "value2")))))

(ert-deftest test-org-supertag-field-update-delete ()
  "测试字段值的更新和删除操作."
  (org-supertag-db-init)
  
  (let* ((node-id "test-node")
         (field-name "test-field")
         (tag-id "test-tag")
         (value1 "value1")
         (value2 "value2"))
    
    ;; 1. 设置初始值
    (org-supertag-field-db-set-value node-id field-name value1 tag-id)
    (should (equal (org-supertag-field-db-get-value node-id field-name tag-id)
                  value1))
    
    ;; 2. 更新值
    (org-supertag-field-db-update-value node-id field-name value2 tag-id)
    (should (equal (org-supertag-field-db-get-value node-id field-name tag-id)
                  value2))
    
    ;; 3. 删除值
    (org-supertag-field-db-delete-value node-id field-name tag-id)
    (should-not (org-supertag-field-db-get-value node-id field-name tag-id))))

(ert-deftest test-org-supertag-field-entity-ops ()
  "测试字段值的实体操作."
  (org-supertag-db-init)
  
  ;; 1. 准备测试数据
  (let* ((node-id "test-node")
         (tag-id "test-tag")
         (field-def (list :name "test-field"
                         :type 'string
                         :required t))
         (value1 "test-value-1")
         (value2 "test-value-2"))
    
    ;; 2. 测试设置值
    (org-supertag-field-set-value field-def value1 node-id tag-id)
    (should (equal (org-supertag-field-get-value field-def node-id tag-id)
                  value1))
    
    ;; 3. 测试更新值
    (org-supertag-field-set-value field-def value2 node-id tag-id)
    (should (equal (org-supertag-field-get-value field-def node-id tag-id)
                  value2))
    
    ;; 4. 测试删除值
    (org-supertag-field-remove-value field-def node-id tag-id)
    (should-not (org-supertag-field-get-value field-def node-id tag-id))))

(provide 'org-supertag-field)
;;; org-supertag-field.el ends here
