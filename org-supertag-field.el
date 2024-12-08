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

(defun org-supertag-get-field-type (type)
  "获取字段类型定义。"
  (gethash type org-supertag-field-types))

;; 初始化基本字段类型
(defun org-supertag-init-field-types ()
  "初始化字段类型。"
  (clrhash org-supertag-field-types)
  
  ;; 字符串类型
  (org-supertag-register-field-type
   'string
   '(:validator org-supertag-validate-string
     :formatter org-supertag-format-string
     :description "基础字符串类型"))
  
  ;; 日期类型
  (org-supertag-register-field-type
   'date
   '(:validator org-supertag-validate-date
     :formatter org-supertag-format-date
     :description "日期类型 (YYYY-MM-DD)"))
  
  ;; 邮箱类型
  (org-supertag-register-field-type
   'email
   '(:validator org-supertag-validate-email
     :formatter org-supertag-format-email
     :description "电子邮箱地址"))
  
  ;; URL类型
  (org-supertag-register-field-type
   'url
   '(:validator org-supertag-validate-url
     :formatter org-supertag-format-url
     :description "URL地址"))
  
  ;; 引用类型
  (org-supertag-register-field-type
   'reference
   '(:validator org-supertag-validate-reference
     :formatter org-supertag-format-reference
     :description "引用其他带有特定标签的条目"
     :ref-tag nil))  ; 存储被引用的标签名
  
  ;; 选项类型
  (org-supertag-register-field-type
   'options
   '(:validator org-supertag-validate-options
     :formatter org-supertag-format-options
     :description "从预定义选项中选择"))
  
  ;; 数值类型
  (org-supertag-register-field-type
   'number
   '(:validator org-supertag-validate-number
     :formatter org-supertag-format-number
     :description "数值类型"))
  
  ;; 整数类型
  (org-supertag-register-field-type
   'integer
   '(:validator (lambda (x) (and (integerp x) (>= x 0)))
     :formatter number-to-string
     :description "非负整数类型"))
  
  ;; 时间类型
  (org-supertag-register-field-type
   'time
   '(:validator org-supertag-validate-time
     :formatter org-supertag-format-time
     :description "时间类型 (HH:MM)"))
  
  ;; 日期时间类型
  (org-supertag-register-field-type
   'datetime
   '(:validator org-supertag-validate-datetime
     :formatter org-supertag-format-datetime
     :description "日期时间类型 (ISO format)"))
  
  ;; 持续时间类型
  (org-supertag-register-field-type
   'duration
   '(:validator org-supertag-valid-duration-p
     :formatter org-supertag-format-duration
     :description "持续时间类型"))
  
  ;; 列表类型
  (org-supertag-register-field-type
   'list
   '(:validator org-supertag-valid-list-p
     :formatter org-supertag-format-list
     :description "列表类型"))
  
  ;; 选择类型
  (org-supertag-register-field-type
   'choice
   '(:validator org-supertag-validate-choice
     :formatter org-supertag-format-choice
     :description "选择类型"))
  
  ;; 复选框类型
  (org-supertag-register-field-type
   'checkbox
   '(:validator org-supertag-validate-checkbox
     :formatter org-supertag-format-checkbox
     :description "复选框类型"))
  
  ;; 评分类型
  (org-supertag-register-field-type
   'rating
   '(:validator org-supertag-validate-rating
     :formatter org-supertag-format-rating
     :description "评分类型 (1-5)"))
  
  ;; 进度类型
  (org-supertag-register-field-type
   'progress
   '(:validator org-supertag-validate-progress
     :formatter org-supertag-format-progress
     :description "进度类型 (0-100%)"))
  
  ;; 文件类型
  (org-supertag-register-field-type
   'file
   '(:validator org-supertag-validate-file
     :formatter org-supertag-format-file
     :description "文件类型"))
  
  ;; 目录类型
  (org-supertag-register-field-type
   'directory
   '(:validator org-supertag-validate-directory
     :formatter org-supertag-format-directory
     :description "目录类型"))
  
  ;; 颜色类型
  (org-supertag-register-field-type
   'color
   '(:validator org-supertag-validate-color
     :formatter org-supertag-format-color
     :description "颜色类型 (#RRGGBB)"))
  
  ;; 人员类型
  (org-supertag-register-field-type
   'person
   '(:validator org-supertag-validate-person
     :formatter org-supertag-format-person
     :description "人名类型"))
  
  ;; 电话号码类型
  (org-supertag-register-field-type
   'telephone
   '(:validator org-supertag-validate-telephone
     :formatter org-supertag-format-telephone
     :description "电话号码类型"))
  
  ;; Org链接类型
  (org-supertag-register-field-type
   'org-link
   '(:validator org-supertag-valid-org-link-p
     :formatter identity
     :description "Org链接类型"))
  
  ;; 密码类型
  (org-supertag-register-field-type
   'password
   '(:validator org-supertag-valid-password-p
     :formatter identity
     :description "密码类型")))

;; 初始化字段类型
(org-supertag-init-field-types)

;;----------------------------------------------------------------------
;; 字段类型的验证器和格式化器
;;----------------------------------------------------------------------

(defun org-supertag-validate-string (value)
  "验证字符串 VALUE。"
  (stringp value))

(defun org-supertag-format-string (value)
  "格式化字符串 VALUE。"
  (string-trim value))

(defun org-supertag-validate-date (value)
  "验证日期 VALUE。"
  (condition-case nil
      (progn 
        (org-parse-time-string value)
        t)
    (error nil)))

(defun org-supertag-format-date (value)
  "格式化日期 VALUE。"
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

(defun org-supertag-format-reference (value)
  "格式化引用值。
将 org-id 格式化为可读的标题。"
  (when-let ((marker (org-id-find value 'marker)))
    (org-with-point-at marker
      (org-get-heading t t t t))))

(defun org-supertag-validate-options (value spec)
  "验证选项值。
VALUE 是要验证的值，
SPEC 是字段规格说明，包含 :options。"
  (let ((options (plist-get spec :options)))
    (and (stringp value)
         (member value options))))

(defun org-supertag-format-options (value _spec)
  "格式化选项值。"
  (string-trim value))

(defun org-supertag-validate-number (value)
  "验证数值类型的值。"
  (numberp value))

(defun org-supertag-format-number (value)
  "格式化数值类型的值。"
  (number-to-string value))

(defun org-supertag-valid-time-p (val)
  "验证是否为有效的时间格式."
  (string-match-p "^\\([0-9]\\|0[0-9]\\|1[0-9]\\|2[0-3]\\):[0-5][0-9]\\(:[0-5][0-9]\\)?$" val))

(defun org-supertag-format-time (time-str)
  "格式化时间字符串."
  time-str)

(defun org-supertag-valid-datetime-p (val)
  "验证是否为有效的日期时间格式."
  (condition-case nil
      (progn 
        (org-parse-time-string val)
        t)
    (error nil)))

(defun org-supertag-format-datetime (datetime-str)
  "格式化日期时间字符串."
  (condition-case nil
      (let ((time (org-parse-time-string datetime-str)))
        (format-time-string "%Y-%m-%d %H:%M:%S" (apply #'encode-time time)))
    (error datetime-str)))

(defun org-supertag-valid-duration-p (val)
  "验证是否为有效的持续时间格式."
  (string-match-p "^\\([0-9]+\\)\\(\\([0-9]+\\)\\)?\\(\\([0-9]+\\)\\)?\\(\\([0-9]+\\)\\)?" val))

(defun org-supertag-format-duration (duration-str)
  "格式化持续时间字符串."
  duration-str)

(defun org-supertag-valid-list-p (val)
  "验证是否为有效的列表格式."
  (and (stringp val)
       (string-match-p "^\\[.*\\]$\\|^(.*)\$" val)))

(defun org-supertag-format-list (list-str)
  "格式化列表字符串."
  (if (string-match-p "^\\[.*\\]$" list-str)
      list-str
    (format "[%s]" list-str)))

(defun org-supertag-valid-choice-p (val choices)
  "验证值是否在选项列表中."
  (member val choices))

(defun org-supertag-valid-checkbox-p (val)
  "验证复选框值."
  (or (equal val "[ ]") (equal val "[X]")))

(defun org-supertag-valid-tel-p (val)
  "验证是否为有效的电话号码格式."
  (string-match-p "^\\+?[0-9() -]+$" val))

(defun org-supertag-valid-file-p (val)
  "验证是否为有效的文件路径."
  (file-exists-p val))

(defun org-supertag-valid-directory-p (val)
  "验证是否为有效的目录路径."
  (file-directory-p val))

(defun org-supertag-valid-tags-p (val)
  "验证是否为有效的标签列表."
  (and (listp val)
       (every #'stringp val)))

(defun org-supertag-valid-rating-p (val)
  "验证是否为有效的评分值."
  (and (numberp val)
       (>= val 0)
       (<= val 5)))

(defun org-supertag-valid-progress-p (val)
  "验证是否为有效的进度值."
  (and (numberp val)
       (>= val 0)
       (<= val 100)))

(defun org-supertag-valid-currency-p (val)
  "验证是否为有效的货币值."
  (and (numberp val)
       (>= val 0)))

(defun org-supertag-valid-color-p (val)
  "验证是否为有效的颜色值."
  (and (stringp val)
       (string-match-p "^#\\([0-9a-fA-F]\\{6\\}\\)$" val)))

(defun org-supertag-valid-location-p (val)
  "验证是否为有效的位置值."
  (and (stringp val)
       (string-match-p "^\\([0-9.-]+\\),\\([0-9.-]+\\)$" val)))

(defun org-supertag-valid-person-p (val)
  "验证是否为有效的人员值."
  (and (stringp val)
       (string-match-p "^\\([^ ]+\\) \\([^ ]+\\)$" val)))

(defun org-supertag-valid-org-link-p (val)
  "验证是否为有效的 Org 链接值."
  (and (stringp val)
       (string-match-p "^\\[\\[.*\\]\\]" val)))

(defun org-supertag-valid-password-p (val)
  "验证是否为有效的密码值."
  (and (stringp val)
       (>= (length val) 8)))

(defun org-supertag-validate-time (val)
  "Validate if VAL is a valid time string in HH:MM format."
  (and (stringp val)
       (string-match-p "^\\([0-1][0-9]\\|2[0-3]\\):[0-5][0-9]$" val)))

(defun org-supertag-format-time (val)
  "Format time value VAL for display."
  (if (org-supertag-validate-time val)
      val
    "Invalid time"))

(defun org-supertag-validate-datetime (val)
  "Validate if VAL is a valid datetime string in ISO format."
  (and (stringp val)
       (string-match-p "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\(Z\\|[+-]\\d{2}:?\\d{2}\\)?$" val)))

(defun org-supertag-format-datetime (val)
  "Format datetime value VAL for display."
  (if (org-supertag-validate-datetime val)
      (format-time-string "%Y-%m-%d %H:%M:%S" (date-to-time val))
    "Invalid datetime"))

(defun org-supertag-validate-choice (val)
  "Validate if VAL is one of the allowed choices."
  (let ((choices (plist-get (org-supertag-field-get-type-spec) :choices)))
    (and choices (member val choices))))

(defun org-supertag-format-choice (val)
  "Format choice value VAL for display."
  val)

(defun org-supertag-validate-checkbox (val)
  "Validate if VAL is a valid checkbox value (t or nil)."
  (memq val '(t nil)))

(defun org-supertag-format-checkbox (val)
  "Format checkbox value VAL for display."
  (if val "[X]" "[ ]"))

(defun org-supertag-validate-rating (val)
  "Validate if VAL is a valid rating (1-5)."
  (and (integerp val)
       (<= 1 val 5)))

(defun org-supertag-format-rating (val)
  "Format rating value VAL for display."
  (if (org-supertag-validate-rating val)
      (make-string val ?★)
    "Invalid rating"))

(defun org-supertag-validate-progress (val)
  "Validate if VAL is a valid progress percentage (0-100)."
  (and (integerp val)
       (<= 0 val 100)))

(defun org-supertag-format-progress (val)
  "Format progress value VAL for display."
  (if (org-supertag-validate-progress val)
      (format "%d%%" val)
    "Invalid progress"))

(defun org-supertag-validate-file (val)
  "Validate if VAL is a valid file path."
  (and (stringp val)
       (file-exists-p val)))

(defun org-supertag-format-file (val)
  "Format file value VAL for display."
  (if (org-supertag-validate-file val)
      (file-name-nondirectory val)
    "Invalid file"))

(defun org-supertag-validate-directory (val)
  "Validate if VAL is a valid directory path."
  (and (stringp val)
       (file-directory-p val)))

(defun org-supertag-format-directory (val)
  "Format directory value VAL for display."
  (if (org-supertag-validate-directory val)
      (abbreviate-file-name val)
    "Invalid directory"))

(defun org-supertag-validate-color (val)
  "Validate if VAL is a valid color (hex format)."
  (and (stringp val)
       (string-match-p "^#[0-9a-fA-F]\\{6\\}$" val)))

(defun org-supertag-format-color (val)
  "Format color value VAL for display."
  (if (org-supertag-validate-color val)
      val
    "Invalid color"))

(defun org-supertag-validate-person (val)
  "Validate if VAL is a valid person name."
  (and (stringp val)
       (not (string-empty-p val))))

(defun org-supertag-format-person (val)
  "Format person value VAL for display."
  val)

(defun org-supertag-validate-telephone (val)
  "Validate if VAL is a valid telephone number."
  (and (stringp val)
       (string-match-p "^\\+?[0-9() -]+$" val)))

(defun org-supertag-format-telephone (val)
  "Format telephone value VAL for display."
  (if (org-supertag-validate-telephone val)
      val
    "Invalid telephone"))

;;----------------------------------------------------------------------
;; 字段定义操作
;;----------------------------------------------------------------------

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

(defun org-supertag-field-validate (field-def value)
  "验证字段值是否符合字段定义.
FIELD-DEF 是字段定义
VALUE 是要验证的值
返回 (t . nil) 如果验证通过，(nil . 错误信息) 如果验证失败"
  ;; 如果字段定义为空，返回验证失败
  (if (null field-def)
      (cons nil "字段定义不存在")
    (let* ((type (plist-get field-def :type))
           (type-def (org-supertag-get-field-type type))
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

;;----------------------------------------------------------------------
;; 同步机制
;;----------------------------------------------------------------------

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

(provide 'org-supertag-field)
;;; org-supertag-field.el ends here
