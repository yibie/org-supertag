;;; org-supertag-base.el --- Base functionality for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供基础功能和共享函数

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-id)

(defgroup org-supertag nil
  "Org-supertag 配置选项."
  :group 'org
  :prefix "org-supertag-")

(defcustom org-supertag-data-directory
  (expand-file-name "org-supertag" user-emacs-directory)
  "Org-supertag 数据存储目录.
用于存储数据库文件、缓存等数据."
  :type 'directory
  :group 'org-supertag) 

(defun org-supertag-ensure-data-directory ()
  "确保数据目录存在."
  (unless (file-exists-p org-supertag-data-directory)
    (make-directory org-supertag-data-directory t)))

(defun org-supertag-data-file (filename)
  "获取数据文件的完整路径.
FILENAME 是相对于数据目录的文件名."
  (expand-file-name filename org-supertag-data-directory))

;;; 共享的核心功能

(defun org-supertag-tag-add-to-node (node-id tag-name)
  "为节点添加标签.
NODE-ID 是节点ID
TAG-NAME 是标签名称"
  (org-supertag-add-relation :node-tag node-id tag-name))

(defun org-supertag-tag-add-to-node (node-id tag-name)
  "将标签添加到节点.
NODE-ID 是节点 ID
TAG-NAME 是标签名称"
  (let ((sanitized-name (org-supertag-sanitize-tag-name tag-name)))
    (org-supertag-db-link :node-tag node-id sanitized-name)))

(defun org-supertag-tag-remove-from-node (node-id tag-name)
  "从节点移除标签.
NODE-ID 是节点ID
TAG-NAME 是标签名称"
  (org-supertag-remove-relation :node-tag node-id tag-name))

(defun org-supertag-ensure-node-id ()
  "确保当前 headline 有 ID."
  (unless (org-id-get)
    (org-id-get-create)))


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
                          :olp olp))))
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

(provide 'org-supertag-base)
;;; org-supertag-base.el ends here 