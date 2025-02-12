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

;;----------------------------------------------------------------------
;; Get Value at Point
;;----------------------------------------------------------------------  

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

(defun org-supertag-view--get-field-link (node-id field-name)
  "Get field link for NODE-ID and FIELD-NAME.
Performs case-insensitive search for field name."
  (let ((field-name-upcase (upcase field-name))
        result)
    (maphash
     (lambda (link-id link-props)
       (when (and (string-match ":node-field:\\(.+\\)->\\(.+\\)$" link-id)
                  (equal (match-string 1 link-id) node-id)
                  (equal (upcase (match-string 2 link-id)) field-name-upcase))
         (setq result link-props)))
     org-supertag-db--link)
    result))

(defun org-supertag-view--get-related-nodes (tag)
  "Get nodes related to TAG.
Returns a list of plists with properties :node, :type, :date and field values."
  (let* ((nodes '())
         (tag-def (org-supertag-tag-get tag))
         (fields (plist-get tag-def :fields)))
    (message "Debug - Tag definition: %S" tag-def)
    (maphash
     (lambda (link-id link-props)
       (when (and (string-match ":node-tag:.*->\\(.+\\)$" link-id)
                  (equal (match-string 1 link-id) tag))
         (when-let* ((node-id (plist-get link-props :from))
                    (node-props (gethash node-id org-supertag-db--object)))
           ;; 获取所有字段值
           (let ((field-values
                  (mapcar (lambda (field)
                          (let* ((field-name (plist-get field :name))
                                (field-link (org-supertag-view--get-field-link 
                                           node-id field-name))
                                (value (when field-link 
                                       (plist-get field-link :value))))
                            (message "Debug - Field value lookup: node=%s field=%s link=%S value=%S" 
                                    node-id field-name field-link value)
                            (cons field-name value)))
                        fields)))
             (message "Debug - Node field values: %S" field-values)
             (push (append
                    (list :node (or (plist-get node-props :title)
                                   (format "Node %s" node-id))
                          :type (or (plist-get node-props :todo-state) "Node")
                          :date (format-time-string 
                                "%Y-%m-%d"
                                (or (plist-get node-props :created-at)
                                    (current-time)))
                          :id node-id
                          :fields field-values)
                    node-props)
                   nodes)))))
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

(defun org-supertag-view--get-field-value (node-id field-name tag-id)
  "Get field value for NODE-ID, FIELD-NAME and TAG-ID.
Returns the field value or nil if not found."
  (let* ((link-id (format ":node-field:%s->%s" node-id field-name))
         (link (gethash link-id org-supertag-db--link)))
    (message "Debug - Getting field value: node=%s field=%s tag=%s link=%S"
             node-id field-name tag-id link)
    (when (and link 
               (equal (plist-get link :tag-id) tag-id))
      (plist-get link :value))))

(defun org-supertag-view--insert-content (tag)
  "Insert content related to TAG in current buffer."
  (insert "* Related Nodes\n\n")
  (let* ((content (org-supertag-view--get-related-nodes tag))
         (tag-def (org-supertag-tag-get tag))
         (fields (plist-get tag-def :fields)))
    (message "Debug - Content: %S" content)
    (if content
        (progn
          ;; 构建表头
          (insert "|Node|Type|Date|")
          (dolist (field fields)
            (insert (format "%s|" (plist-get field :name))))
          (insert "\n|-\n")
          
          ;; 插入每个节点的数据
          (dolist (item content)
            (let ((field-values (plist-get item :fields)))
              (insert "|")
              (insert (format "%s|%s|%s|"
                            (plist-get item :node)
                            (plist-get item :type)
                            (plist-get item :date)))
              ;; 插入字段值
              (dolist (field fields)
                (let* ((field-name (plist-get field :name))
                       (value (cdr (assoc field-name field-values))))
                  (insert (format "%s|" (or value "")))))
              (insert "\n")))
          (insert "\n")
          (save-excursion
            (forward-line -1)
            (org-table-align)))
      (insert (format "No content found for tag #%s" tag)))))

(define-derived-mode org-supertag-view-mode org-mode "SuperTag-View"
  "Major mode for viewing org-supertag tag-related content."
  :group 'org-supertag
  (setq buffer-read-only t)
  (buffer-disable-undo))



(provide 'org-supertag-view)

;;; org-supertag-view.el ends here 