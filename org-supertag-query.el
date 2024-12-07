;;; org-supertag-query.el --- Query functions for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; 提供高级查询功能

;;; Code:

(require 'org-supertag-db)
(require 'org-supertag-types)

(defun org-supertag-find-entities (type &optional pred)
  "查找指定类型的实体.
TYPE 为实体类型 (:node :tag :field :group)
PRED 为可选的过滤函数"
  (org-supertag-db-find
   (lambda (id props)
     (and (eq (plist-get props :type) type)
          (or (null pred)
              (funcall pred id props))))))

(defun org-supertag-find-nodes-by-tag (tag-id)
  "查找包含指定标签的所有节点."
  (let (nodes)
    (org-supertag-db-find-relations
     (lambda (_rel-id rel)
       (when (and (eq (car rel) :node-tag)
                 (equal (nth 2 rel) tag-id))
         (push (nth 1 rel) nodes))))
    nodes))

(defun org-supertag-find-nodes-by-field (field-id value)
  "查找字段值匹配的所有节点."
  (message "Debug - Finding nodes with field %S = %S" field-id value)
  (message "Debug - All relations: %S" (ht-items org-supertag-db--relations))
  (message "Debug - All field values: %S" (ht-items org-supertag-db--field-values))
  (let (nodes)
    (org-supertag-db-find-relations
     (lambda (_rel-id rel)
       (message "Debug - Checking relation: %S" rel)
       (when (and (eq (car rel) :node-field)
                 (equal (nth 2 rel) field-id))
         (let ((node-id (nth 1 rel))
               (field-value (org-supertag-db-get-field-value field-id (nth 1 rel))))
           (message "Debug - Node %S field value: %S" node-id field-value)
           (when (equal field-value value)
             (push node-id nodes))))))
    (message "Debug - Found nodes: %S" nodes)
    (nreverse nodes)))
(provide 'org-supertag-query)
;;; org-supertag-query.el ends here 