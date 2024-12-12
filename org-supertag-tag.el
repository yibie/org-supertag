;;; org-supertag-tag.el --- Tag relation management for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供标签关系管理功能
;; 核心原则：通过关系连接实体，使用 type、from、to 来表达关系


;;; Code:

(require 'org-supertag-api)
(require 'org-supertag-types)
(require 'org-supertag-cache)
(require 'org-supertag-group)
(require 'org-supertag-parser)
(require 'org-supertag-field)
(require 'cl-lib)

;;; 标签字段定义



(defun org-supertag-tag-get-fields (tag-name)
  "获取标签的字段定义.
TAG-NAME 是标签名称"
  (plist-get (org-supertag-get-entity tag-name) :fields))

;;; 字段验证

;;; 标签实例操作 API

(defun org-supertag-format-tag (tag-name)
  "格式化标签用于显示.
TAG-NAME 是标签名称"
  (concat "#" tag-name))

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

(defun org-supertag-tag-create (name &rest props)
  "创建标签.
NAME 是标签名称
PROPS 是标签属性"
  (let* ((sanitized-name (org-supertag-sanitize-tag-name name))
         (fields (plist-get props :fields)))
    (message "Debug - 规范化后的标签名: %s" sanitized-name)
    (message "Debug - 处理标签字段: %S" fields)
    
    ;; 处理字段定义
    (let* ((normalized-fields
            (if (listp fields)
                (mapcar (lambda (field)
                         (message "Debug - 处理字段: %S" field)
                         field)  ; 已经是标准格式，不需要再解析
                       fields)
              (list fields)))
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


;;; 字段管理
(defun org-supertag--apply-tag (tag-name &optional field-values)
  "应用标签到当前节点。
TAG-NAME 是标签名称
FIELD-VALUES 是可选的预设字段值"
  (let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
         (node-id (org-id-get-create)))
    (message "Debug - 开始应用标签: %s -> %s" tag-name sanitized-name)
    ;; 处理内联标签
    (if (string-match-p "^#" tag-name)
        (org-supertag--handle-inline-tag tag-name node-id)
      ;; 原有的标签处理逻辑
      (progn
        ;; 1. 添加标签关系
        (org-supertag-tag-add-to-node node-id sanitized-name)
        ;; 2. 更新标题
        (let* ((element (org-element-at-point))
               (title (org-element-property :raw-value element))
               (new-title (concat title " #" sanitized-name)))
          (org-edit-headline new-title))
        ;; 3. 应用字段，不管是否预设
        (org-supertag--apply-tag-fields sanitized-name field-values)))
    (message "Debug - 标签应用完成")))

(defun org-supertag-tag-add-to-node (node-id tag-name)
  "将标签添加到节点.
NODE-ID 是节点 ID
TAG-NAME 是标签名称"
  (let ((sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    (org-supertag-db-link :node-tag node-id sanitized-name)))


;;----------------------------------------------------------------------
;; 为 org-supertag-tag 提供字段管理功能
;;----------------------------------------------------------------------

(defun org-supertag--sanitize-field-name (name)
  "将字段名转换为有效的属性名.
NAME 是原始字段名"
  (let ((sanitized (replace-regexp-in-string
                   "\\s-+" "_"  ; 将空格替换为下划线
                   (upcase (string-trim name)))))  ; 转为大写并去除首尾空格
    sanitized))

(defun org-supertag--create-custom-field ()
  "创建自定义字段，返回字段定义."
  (let* ((type-choice (completing-read "字段类型: "
                                     '("字符串" "选项" "数字" "日��" "引用" "标签")
                                     nil t))
         (type-sym (cdr (assoc type-choice
                              '(("字符串" . string)
                                ("选项" . options)
                                ("数字" . number)
                                ("日期" . date)
                                ("引用" . reference)
                                ("标签" . tags)))))
         (type-def (org-supertag-get-field-type type-sym))
         ;; 处理字段名
         (raw-name (read-string "字段名称: "))
         (field-name (org-supertag--sanitize-field-name raw-name))
         (field-def (list :name field-name
                         :type type-sym
                         :display-name raw-name)))  ; 保存原始名称用于显示
    
    ;; 添加类型特定的属性
    (pcase type-sym
      ('options
       (let ((options (split-string
                      (read-string "输入选项 (用逗号分隔): ")
                      "," t "[ \t\n\r]+")))
         (when options  ; 确保有输入选项
           (setq field-def (plist-put field-def :options options)))))
      
      ('reference
       (let ((ref-tag (completing-read "引用哪个标签的条目? "
                                     (org-supertag-get-all-tags))))
         (when ref-tag  ; 确保选择了标签
           (setq field-def (plist-put field-def :ref-tag ref-tag)))))
      
      ('number
       (let ((min (read-number "最小值 (直接回车跳过): "))
             (max (read-number "最大值 (直接回车跳过): ")))
         (when (and min max)  ; 确保输入了范围
           (setq field-def (plist-put field-def :min min))
           (setq field-def (plist-put field-def :max max))))))
    
    ;; 添加验证器和格式化器
    (when-let ((validator (plist-get type-def :validator)))
      (setq field-def (plist-put field-def :validate validator)))
    (when-let ((formatter (plist-get type-def :formatter)))
      (setq field-def (plist-put field-def :formatter formatter)))
    
    field-def))

(defun org-supertag--add-single-field (fields)
  "添加单个字段并设置其值。
FIELDS 是当前字段列表。
返回 (是否继续 . (更新后的字段列表 . 字段值列表))"
  (let* ((choices (append
                  '("Finish - 完成")
                  '("+ 新建字段")
                  (mapcar #'org-supertag-format-preset-field
                         org-supertag-preset-fields)))
         (choice (completing-read 
                 (if fields
                     "选择字段 (或选择 Finish 完成): "
                   "选择字段: ")
                 choices nil t)))
    
    (cond
     ;; 选择完成
     ((string= choice "Finish - 完成")
      (cons nil (cons fields nil)))
     
     ;; 新建自定义字段
     ((string= choice "+ 新建字段")
      (if-let ((new-field (org-supertag--create-custom-field)))
          (if (cl-find (plist-get new-field :name) fields
                      :key (lambda (f) (plist-get f :name))
                      :test #'string=)
              (progn
                (message "字段名 %s 已存在" (plist-get new-field :name))
                (cons t (cons fields nil)))
            ;; 立即读取字段值
            (when-let ((value (org-supertag-field-read-value new-field)))
              (cons t (cons 
                      (cons new-field fields)
                      (list (cons (plist-get new-field :name) value))))))
        (cons t (cons fields nil))))
     
     ;; 选择预设字段
     ((string-prefix-p "- " choice)
      (let* ((field-name (substring choice 2))
             (field-name (car (split-string field-name " (")))
             (preset (org-supertag-get-preset-field field-name)))
        (if preset
            (when-let ((value (org-supertag-field-read-value preset)))
              (cons t (cons 
                      (cons preset fields)
                      (list (cons (plist-get preset :name) value)))))
          (cons t (cons fields nil)))))
          (cons t fields))))


(defun org-supertag--field-interactive-edit (&optional current-fields)
  "交互式编辑字段定义。
CURRENT-FIELDS 是当前的字段列表。
返回 (fields . values) cons cell"
  (let ((fields (or current-fields nil))
        (values nil))
    (message "Debug - 开始编辑字段，当前字段: %S" fields)
    ;; 如果有预设字段，直接读取它们的值
    (if current-fields
        (progn
          (message "Debug - 处理预设字段")
          (dolist (field current-fields)
            (let* ((field-name (plist-get field :name))
                   (parsed-field (if (stringp field-name)
                                   (org-supertag-get-preset-field field-name)
                                 field)))
              (when-let ((value (org-supertag-field-read-value parsed-field)))
                (push (cons field-name value) values))))
          (cons current-fields (nreverse values)))
      ;; 否则进入交互式编辑模式
      (catch 'done
        (while t
          (let* ((result (org-supertag--add-single-field fields))
                 (continue (car result))
                 (fields-and-values (cdr result))
                 (updated-fields (car fields-and-values))
                 (new-values (cdr fields-and-values)))
            (setq fields updated-fields
                  values (append new-values values))
            (unless continue
              (throw 'done (cons fields values)))))))))

(defun org-supertag--apply-tag-fields (tag-name field-values)
  "应用标签的字段到当前节点。
TAG-NAME 是标签名称。
FIELD-VALUES 是字段值的 alist"
  (when-let* ((tag (org-supertag-tag-get tag-name))
              (fields (plist-get tag :fields))
              (node-id (org-id-get)))
    (message "Debug - 应用字段: %S" fields)
    
    ;; 处理每个字段
    (dolist (field fields)
      (let* ((field-name (plist-get field :name))
             (value (or (alist-get field-name field-values nil nil #'string=)
                       (org-supertag-field-read-value field))))
        (when value
          (message "Debug - 设置字段 %s = %s" field-name value)
          (org-entry-put nil field-name value)
          (org-supertag-db-set-field-value field-name node-id value))))))



(provide 'org-supertag-tag)

;;; org-supertag-tag.el ends here
