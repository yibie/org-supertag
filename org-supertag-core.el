;;; org-supertag-core.el --- Core functionality for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供 org-supertag 的核心功能
;; 包括标签名称处理、格式化等基础功能

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-id)


(defgroup org-supertag nil
  "Org-supertag 配置选项."
  :group 'org
  :prefix "org-supertag-")

;; 标签名称处理

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

;; 节点 ID 管理

(defun org-supertag-ensure-node-id ()
  "确保当前 headline 有 ID."
  (unless (org-id-get)
    (org-id-get-create)))

;; 数据目录管理


;; 节点属性提取

(defun org-supertag--extract-node-props (element)
  "从 org-element 中提取节点属性.
ELEMENT 是 org-element 结构"
  (let* ((level (org-element-property :level element))
         (_ (message "Debug - Extracting props for level %d node: %s"
                    level (org-element-property :raw-value element)))
         (olp (when (> level 1)
                (let ((current element)
                      path)
                  (message "Debug - Starting outline path collection...")
                  (while (and current 
                             (eq (org-element-type current) 'headline))
                    (let ((title (org-element-property :raw-value current)))
                      (message "Debug - Found headline: %s" title)
                      (push title path))
                    (setq current (org-element-property :parent current))
                    (when current
                      (message "Debug - Parent type: %s" (org-element-type current))))
                  (message "Debug - Final path before processing: %S" path)
                  (let ((final-result (butlast path)))
                    (message "Debug - Final result: %S" final-result)
                    final-result))))
         (_ (message "Debug - Node properties before return: %S"
                    (list :type :node
                          :title (org-element-property :raw-value element)
                          :level level
                          :todo (org-element-property :todo-keyword element)
                          :priority (org-element-property :priority element)
                          :tags (org-get-tags)
                          :properties (org-entry-properties nil 'standard)
                          :file (buffer-file-name)
                          :position (org-element-property :begin element)
                          :olp olp
                          :mtime (float-time))))
    (list :type :node
          :title (org-element-property :raw-value element)
          :level level
          :todo (org-element-property :todo-keyword element)
          :priority (org-element-property :priority element)
          :tags (org-get-tags)
          :properties (org-entry-properties nil 'standard)
          :file (buffer-file-name)
          :position (org-element-property :begin element)
          :olp olp
          :mtime (float-time)))))

;; 标签应用

(defun org-supertag--apply-tag (tag-name &optional field-values)
  "应用标签到当前节点.
TAG-NAME 是标签名称
FIELD-VALUES 是可选的预设字段值"
  (let* ((sanitized-name (org-supertag-sanitize-tag-name tag-name))
         (node-id (org-id-get-create)))
    (message "Debug org-supertag--apply-tag 开始应用标签: %s -> %s (field-values=%S)" 
             tag-name sanitized-name field-values)
    
    ;; 1. 添加标签关系
    (org-supertag-db-link :node-tag node-id sanitized-name)
    
    ;; 2. 更新标题
    (let* ((element (org-element-at-point))
           (title (org-element-property :raw-value element))
           (new-title (concat title " #" sanitized-name)))
      (org-edit-headline new-title))
    
    ;; 3. 应用字段
    (when-let ((tag (org-supertag-db-tag-get sanitized-name)))
      (message "Debug org-supertag--apply-tag 获取到标签: %S" tag)
      (let ((fields (org-supertag-db-get-tag-fields sanitized-name)))
        (message "Debug org-supertag--apply-tag 标签字段: %S" fields)
        (when fields
          (org-supertag--apply-tag-fields sanitized-name field-values))))))

(defun org-supertag--apply-tag-fields (tag-name field-values)
  "应用标签的字段到当前节点.
TAG-NAME 是标签名称
FIELD-VALUES 是字段值的 alist"
  (when-let* ((tag (org-supertag-tag-get tag-name))
              (fields (org-supertag-tag-type-fields tag))
              (node-id (org-id-get)))
    (message "Debug org-supertag--apply-field 应用字段开始，tag=%S" tag)
    (message "Debug org-supertag--apply-field 标签字段定义：%S" fields)
    (message "Debug org-supertag--apply-field 字段值：%S" field-values)
    
    ;; 确保在当前节点
    (save-excursion
      (when-let ((pos (org-id-find node-id t)))
        (message "Debug org-supertag--apply-field 找到节点位置：%S" pos)
        (org-with-point-at pos
          ;; 处理每个字段
          (dolist (field fields)
            (let* ((field-name (plist-get field :name))
                   (field-def (org-supertag-plist-to-field field))
                   ;; 获取或读取字段值
                   (value (or (and field-values  ; 确保 field-values 非空
                                 (alist-get field-name field-values nil nil #'equal))
                            (org-supertag--read-field-value field-def))))
              (when value
                (message "Debug org-supertag--apply-field 设置字段 %s = %s" field-name value)
                (org-set-property field-name value)))))))))

(provide 'org-supertag-core)
;;; org-supertag-core.el ends here
