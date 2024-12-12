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






(provide 'org-supertag-tag)

;;; org-supertag-tag.el ends here
