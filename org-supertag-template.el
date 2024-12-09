(require 'org-supertag-api)

(defun org-supertag-template-get (id)
  "从API获取模板.
ID 是模板的唯一标识符."
  (org-supertag-get-template id))

(defun org-supertag-template-list ()
  "列出所有模板."
  (org-supertag-find-templates))

(defun org-supertag-template-get-tag (template-id)
  "获取模板关联的标签.
TEMPLATE-ID 是模板ID."
  (org-supertag-get-template-tag template-id))

(defun org-supertag-template-link-tag (template-id tag-name)
  "建立模板和标签的关联.
TEMPLATE-ID 是模板ID
TAG-NAME 是标签名称."
  (org-supertag-link-template-tag template-id tag-name))

(provide 'org-supertag-template )

;;; org-supertag-template.el ends here