;;; org-supertag-view.el --- View system for org-supertag -*- lexical-binding: t -*-

(require 'org-supertag-db)

;;----------------------------------------------------------------------
;; View content related to a tag
;;---------------------------------------------------------------------- 

;;;###autoload
(defun org-supertag-view-tag ()
  "Show content related to a tag.
If point is on a tag, show content for that tag.
Otherwise, prompt for a tag using completion."
  (interactive)
  (let ((tag-at-point (org-supertag-view--get-tag-name)))
    (if tag-at-point
        ;; If point is on a tag, show content for that tag
        (org-supertag-view--show-content tag-at-point)
      ;; If point is not on a tag, prompt for a tag using completion
      (let ((tag (completing-read "View tag: "
                                (org-supertag-view--get-all-tags)
                                nil t)))
        (org-supertag-view--show-content tag)))))

(defun org-supertag-view--get-tag-name ()
  "Get tag name at point.
Returns nil if no tag is found at point."
  (save-excursion
    (when (thing-at-point-looking-at "#\\([[:alnum:]_-]+\\)")
      (let ((tag-name (match-string-no-properties 1)))
        (when (gethash tag-name org-supertag-db--object)
          tag-name)))))

(defun org-supertag-view--get-all-tags ()
  "Get all available tags from the database."
  (let ((tags '()))
    (maphash
     (lambda (id props)
       (when (eq (plist-get props :type) :tag)
         (push id tags)))
     org-supertag-db--object)
    tags))

;;----------------------------------------------------------------------
;; Get related nodes for a tag
;;----------------------------------------------------------------------    

(defun org-supertag-view--get-related-nodes (tag)
  "Get nodes related to TAG.
Returns a list of plists with properties :node, :type, and :date."
  (let ((nodes '()))
    (maphash
     (lambda (link-id link-props)
       (when (and (string-match ":node-tag:.*->\\(.+\\)$" link-id)
                  (equal (match-string 1 link-id) tag))
         (when-let* ((node-id (plist-get link-props :from))
                    (node-props (gethash node-id org-supertag-db--object)))
           (push (list :node (or (plist-get node-props :title)
                                (format "Node %s" node-id))
                      :type (or (plist-get node-props :todo-state) "Node")
                      :date (format-time-string 
                            "%Y-%m-%d"
                            (or (plist-get node-props :created-at)
                                (current-time))))
                 nodes))))
     org-supertag-db--link)
    (nreverse nodes)))

(defun org-supertag-view--show-content (tag)
  "Show content related to TAG in a new buffer."
  (let ((buf (get-buffer-create (format "*Org SuperTag View: %s*" tag))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (org-supertag-view-mode)
        (org-supertag-view--insert-header tag)
        (org-supertag-view--insert-content tag)))
    (switch-to-buffer buf)))

(defun org-supertag-view--insert-header (tag)
  "Insert header for TAG view."
  (insert (format "Content tagged with #%s:\n\n" tag)))

(defun org-supertag-view--insert-content (tag)
  "Insert content related to TAG in current buffer."
  (let ((content (org-supertag-view--get-related-nodes tag)))
    (if content
        (progn
          (insert "| Node | Type | Date |\n")
          (insert "|------+------+------|\n")
          (dolist (item content)
            (insert (format "| %s | %s | %s |\n"
                          (plist-get item :node)
                          (plist-get item :type)
                          (plist-get item :date))))
          (org-table-align))
      (insert (format "No content found for tag #%s" tag)))))

(define-derived-mode org-supertag-view-mode org-mode "SuperTag-View"
  "Major mode for viewing org-supertag tag-related content."
  :group 'org-supertag
  (setq buffer-read-only t)
  (buffer-disable-undo))



(provide 'org-supertag-view)

;;; org-supertag-view.el ends here 