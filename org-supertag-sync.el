;;; org-supertag-sync.el --- Synchronization for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供基础的同步机制，包括：
;; - 实体同步
;; - 关系同步
;; - 值同步
;; - 变更监控

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-id)
(require 'org-supertag-db)

;;; 基础同步函数

(defun org-supertag-sync-entity (id props)
  "同步实体.
ID 是实体ID，PROPS 是属性列表
返回 t 如果同步成功，nil 如果失败"
  (when (and id props)
    (org-supertag-db-put id props)))

(defun org-supertag-sync-relation (type from to)
  "同步关系.
TYPE 是关系类型，FROM 和 TO 是实体ID
返回 t 如果同步成功，nil 如果失败"
  (when (and type from to)
    (org-supertag-db-link type from to)))

(defun org-supertag-sync-value (type entity-id value)
  "同步值.
TYPE 是值类型，ENTITY-ID 是实体ID，VALUE 是值
返回 t 如果同步成功，nil 如果失败"
  (when (and type entity-id)
    (org-supertag-db-set-field-value type entity-id value)))

(defun org-supertag-sync-node-at-point ()
  "同步当前位置的节点.
返回同步后的节点 ID，如果同步失败返回 nil"
  (when (org-at-heading-p)
    (let* ((node-id (org-id-get-create))
           (props (org-supertag-parse-node)))
      (when (org-supertag-sync-entity node-id props)
        node-id))))

(defun org-supertag-parse-node ()
  "解析当前节点的完整信息，包括 ID、标签和大纲路径"
  (let* ((element (org-element-at-point))
         (tags-info (org-supertag-parse-node-get-tags))
         (props (org-supertag-parse-node-props element)))
    (append props tags-info)))

;;; 变更监控

(defvar org-supertag-sync--before-change-functions nil
  "变更前要执行的函数列表.")

(defvar org-supertag-sync--after-change-functions nil
  "变更后要执行的函数列表.")

(defun org-supertag-sync-add-before-change-function (func)
  "添加变更前要执行的函数.
FUNC 是一个接受 BEG 和 END 参数的函数"
  (add-to-list 'org-supertag-sync--before-change-functions func))

(defun org-supertag-sync-add-after-change-function (func)
  "添加变更后要执行的函数.
FUNC 是一个无参数函数"
  (add-to-list 'org-supertag-sync--after-change-functions func))

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