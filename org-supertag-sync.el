;;; org-supertag-sync.el --- Synchronization for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供基础的同步机制，包括：
;; - 实体同步
;; - 标签同步
;; - 字段同步
;; - 变更监控

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-id)
(require 'org-supertag-db)
(require 'org-supertag-parser)

;;; 基础同步函数

(defun org-supertag-sync-node-at-point ()
  "同步当前位置的节点."
  (interactive)
  (when (org-at-heading-p)
    (let* ((node-id (org-id-get-create))
           (parsed (org-supertag-parse-node)))
      
      (message "\n=== 开始同步节点 ===")
      (message "节点 ID: %s" node-id)
      
      ;; 1. 确保节点实体存在
      (org-supertag-db-put node-id 
                          `(:type :node 
                            :title ,(org-get-heading t t t t)
                            :file ,(buffer-file-name)))
      
      ;; 2. 处理标签引用
      (dolist (tag-spec (plist-get parsed :tags))
        (let ((tag (plist-get tag-spec :tag))
              (field (plist-get tag-spec :field))
              (value (plist-get tag-spec :value)))
          (message "处理标签引用: %s.%s=%s" tag field value)
          ;; 确保标签实体存在
          (unless (org-supertag-db-exists-p tag)
            (org-supertag-db-put tag '(:type :tag)))
          ;; 关联标签到节点
          (org-supertag-db-link :node-tag node-id tag)
          ;; 如果有字段值，更新数据库
          (when value
            (org-supertag-db-set-field-value field tag value))))
      
      ;; 3. 处理节点引用
      (dolist (ref-spec (plist-get parsed :refs))
        (let ((node (plist-get ref-spec :node))
              (field (plist-get ref-spec :field))
              (value (plist-get ref-spec :value)))
          (message "处理节点引用: @%s.%s=%s" node field value)
          ;; 确保节点实体存在
          (unless (org-supertag-db-exists-p node)
            (org-supertag-db-put node '(:type :node)))
          ;; 关联引用到节点
          (org-supertag-db-link :node-ref node-id node)
          ;; 如果有字段值，更新数据库
          (when value
            (org-supertag-db-set-field-value field node value))))
      
      ;; 4. 保存数据库
      (org-supertag-db-save)
      (message "数据库已保存")
      
      ;; 5. 返回节点 ID
      node-id)))

;;; 变更监控

(defvar org-supertag-sync--before-change-functions nil
  "变更前要执行的函数列表.")

(defvar org-supertag-sync--after-change-functions nil
  "变更后要执行的函数列表.")

(defun org-supertag-sync--before-change-hook (beg end)
  "变更前的处理钩子.
BEG 和 END 是变更的范围"
  (dolist (func org-supertag-sync--before-change-functions)
    (funcall func beg end)))

(defun org-supertag-sync--after-change-hook ()
  "变更后的处理钩子."
  (dolist (func org-supertag-sync--after-change-functions)
    (funcall func)))

;;; 钩子管理

(defun org-supertag-sync-setup-hooks ()
  "设置同步钩子."
  (add-hook 'before-change-functions #'org-supertag-sync--before-change-hook nil t)
  (add-hook 'after-change-functions #'org-supertag-sync--after-change-hook nil t)
  (add-hook 'org-after-todo-state-change-hook #'org-supertag-sync--after-change-hook)
  (add-hook 'org-after-tags-change-hook #'org-supertag-sync--after-change-hook)
  (add-hook 'org-after-promote-entry-hook #'org-supertag-sync--after-change-hook)
  (add-hook 'org-after-demote-entry-hook #'org-supertag-sync--after-change-hook))

(defun org-supertag-sync-remove-hooks ()
  "移除同步钩子."
  (remove-hook 'before-change-functions #'org-supertag-sync--before-change-hook t)
  (remove-hook 'after-change-functions #'org-supertag-sync--after-change-hook t)
  (remove-hook 'org-after-todo-state-change-hook #'org-supertag-sync--after-change-hook)
  (remove-hook 'org-after-tags-change-hook #'org-supertag-sync--after-change-hook)
  (remove-hook 'org-after-promote-entry-hook #'org-supertag-sync--after-change-hook)
  (remove-hook 'org-after-demote-entry-hook #'org-supertag-sync--after-change-hook))

(provide 'org-supertag-sync)
;;; org-supertag-sync.el ends here
