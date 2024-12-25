;;; org-supertag-face.el --- Face management for org-supertag --*- lexical-binding: t -*-

(defgroup org-supertag-faces nil
  "Faces for org-supertag."
  :group 'org-faces)

(defcustom org-supertag-tag-faces nil
  "Alist of tag faces.
Each element is (TAG-ID . FACE-PLIST) where:
TAG-ID is the tag identifier (e.g. \"@project\")
FACE-PLIST is a property list of face attributes."
  :type '(alist :key-type string
                :value-type (plist :key-type symbol
                                 :value-type sexp))
  :group 'org-supertag-faces)

(defun org-supertag-face-set (tag-id face-plist)
  "Set face for TAG-ID to FACE-PLIST."
  (customize-save-variable
   'org-supertag-tag-faces
   (cons (cons tag-id face-plist)
         (assoc-delete-all tag-id org-supertag-tag-faces))))

(defun org-supertag-face-get (tag-id)
  "Get face for TAG-ID."
  (cdr (assoc tag-id org-supertag-tag-faces)))

(defun org-supertag-face-apply (node-id tag-id)
  "Apply face for TAG-ID to NODE-ID."
  (when-let* ((pos (org-supertag-db-get-pos node-id))
              (face-plist (org-supertag-face-get tag-id)))
    (org-with-point-at pos
      (org-back-to-heading t)
      (let* ((beg (line-beginning-position))
             (end (line-end-position))
             ;; 创建叠加层
             (ov (make-overlay beg end)))
        ;; 设置叠加层属性
        (overlay-put ov 'face face-plist)
        ;; 存储叠加层以便后续管理
        (overlay-put ov 'org-supertag-face t)
        (overlay-put ov 'node-id node-id)))))

(defun org-supertag-face-remove (node-id)
  "Remove custom face for NODE-ID."
  (when-let ((pos (org-supertag-db-get-pos node-id)))
    (org-with-point-at pos
      (org-back-to-heading t)
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        ;; 移除所有相关的叠加层
        (dolist (ov (overlays-in beg end))
          (when (and (overlay-get ov 'org-supertag-face)
                     (equal (overlay-get ov 'node-id) node-id))
            (delete-overlay ov)))))))

;; 确保在 buffer 变化时保持叠加层
(defun org-supertag-face-refresh ()
  "Refresh all faces in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (when-let* ((node-id (org-id-get))
                  (tag-id (org-supertag-node-get-tag node-id)))
        (org-supertag-face-apply node-id tag-id)))))

;; 添加到相关钩子
(add-hook 'org-mode-hook #'org-supertag-face-refresh)
(add-hook 'after-save-hook #'org-supertag-face-refresh)

(provide 'org-supertag-face)