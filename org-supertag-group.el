;;; org-supertag-group.el --- Tag group management -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供标签组管理功能

;;; Code:

(require 'org-supertag-api)
(require 'org-supertag-db)

(defun org-supertag-create-group (id name &optional props)
  "创建标签组."
  (let ((group-props (copy-sequence (or props (list)))))
    (setq group-props (plist-put group-props :type :group))
    (setq group-props (plist-put group-props :name name))
    (message "Debug - Creating group with props: %S" group-props)
    (org-supertag-db-put id group-props)))

(defun org-supertag-add-tag-to-group (tag-id group-id)
  "将标签添加到组."
  (when (and (org-supertag-db-exists-p tag-id)
             (org-supertag-db-exists-p group-id))
    (let ((tag-type (plist-get (org-supertag-db-get tag-id) :type))
          (group-type (plist-get (org-supertag-db-get group-id) :type)))
      (message "Debug - Tag type: %S, Group type: %S" tag-type group-type)
      (when (and (eq tag-type :tag)
                 (eq group-type :group))
        ;; 从组到标签建立关系
        (org-supertag-db-link :tag-group group-id tag-id)
        t))))


(defun org-supertag-get-group-tags (group-id)
  "获取组内的所有标签."
  (when (org-supertag-db-exists-p group-id)
    (let ((group-type (plist-get (org-supertag-db-get group-id) :type)))
      (message "Debug - Getting tags for group: %S (type: %S)" group-id group-type)
      (when (eq group-type :group)
        (let ((links (org-supertag-db-get-links :tag-group group-id)))
          (message "Debug - Found links: %S" links)
          (mapcar #'car links))))))

(defun org-supertag-remove-tag-from-group (tag-id group-id)
  "从组中移除标签."
  (when (and (org-supertag-db-exists-p tag-id)
             (org-supertag-db-exists-p group-id))
    ;; 修改这里：从 group-id 到 tag-id 的关系
    (org-supertag-db-unlink :tag-group group-id tag-id)))

(provide 'org-supertag-group)
;;; org-supertag-group.el ends here 