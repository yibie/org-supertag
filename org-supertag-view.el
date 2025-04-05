;;; org-supertag-view.el --- View system for org-supertag -*- lexical-binding: t -*-

(require 'org-supertag-db)
(require 'org-supertag-relation)
(require 'cl-lib)
(require 'org-supertag-view-utils)
(require 'org-supertag-view-table)
(require 'org-supertag-view-discover)
(require 'org-supertag-view-column)


;;----------------------------------------------------------------------
;; Main functions
;;---------------------------------------------------------------------- 

;;;###autoload
(defun org-supertag-view-tag ()
  "Show content related to a tag with user's choice of view mode.
Available views:
- Tag-only view: Simple table view showing tag properties and related nodes
- Discover view: Interactive tag exploration and filtering
- Columns view: Multi-column view for comparing nodes"
  (interactive)
  (let* ((tag-at-point (org-supertag-view--get-tag-name))
         (view-options '(("Table view" . tag-only)
                        ("Discover view" . discover)
                        ("Columns view" . columns)))
         (view-choice (completing-read "Select view mode: "
                                     view-options nil t))
         (view-type (cdr (assoc view-choice view-options)))
         (tag (or tag-at-point
                 (completing-read "Select tag: "
                                (org-supertag-view--get-all-tags)


(define-derived-mode org-supertag-discover-mode special-mode "Org-ST-Discover"
  "Major mode for progressive tag discovery in org-supertag."
  :group 'org-supertag
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq header-line-format 
        (propertize " Org-Supertag Tag Discovery" 'face '(:weight bold)))
  (setq-local org-element-use-cache nil)
  (setq-local org-mode-hook nil)
  ;; 确保不影响 minibuffer
  (setq-local minibuffer-auto-raise t)
  (setq-local enable-recursive-minibuffers t))

(define-derived-mode org-supertag-column-mode special-mode "SuperTag-Columns"
  "Major mode for multi-column tag comparison in org-supertag."
  :group 'org-supertag
  (setq buffer-read-only t)
  (setq truncate-lines t)  
  (buffer-disable-undo)
  (setq header-line-format 
        (propertize " Org-Supertag Multi-Column Tag View" 'face '(:weight bold)))
  (setq-local org-element-use-cache nil)
  (setq-local org-mode-hook nil)
  (use-local-map org-supertag-view-column-mode-map))

(define-minor-mode org-supertag-view-mode
  "Minor mode for viewing org-supertag tag-related content."
  :lighter " SuperTag-View"
  :group 'org-supertag

  (if org-supertag-view-mode
      ;; When enabling the mode
      (when (string-match-p "\\*Org SuperTag" (buffer-name))
        ;; Only set read-only for SuperTag view buffers, not regular org files
        (setq-local org-supertag-view--prev-read-only buffer-read-only)
        (setq buffer-read-only t)
        (buffer-disable-undo))
    ;; When disabling the mode
    (when (boundp 'org-supertag-view--prev-read-only)
      (setq buffer-read-only org-supertag-view--prev-read-only))))     

(defun org-supertag-view--view-node-from-table ()
  "Select and view a node from the traditional table view.
This function is specific to the traditional table view mode."
  (interactive)
  (let* ((buffer-name (buffer-name))
         (tag nil)
         (nodes nil)
         (node-titles nil))
    
    ;; 从缓冲区名称中提取标签名
    (when (string-match "\\*Org SuperTag Table View: \\(.*\\)\\*" buffer-name)
      (setq tag (match-string 1 buffer-name)))
    
    (if tag
        (progn
          ;; 获取与标签相关的节点
          (setq nodes (org-supertag-view--get-nodes-with-tags (list tag)))
          (setq node-titles
                (mapcar (lambda (node-id)
                        (let* ((props (gethash node-id org-supertag-db--object))
                               (title (or (plist-get props :title) "无标题"))
                               (type (or (plist-get props :todo-state) "")))
                         (cons (format "%s %s" title 
                                      (if (string-empty-p type) 
                                          "" 
                                        (format "[%s]" type)))
                               node-id)))
                       nodes))
          
          ;; 让用户选择节点并查看
          (if nodes
              (let* ((selected (completing-read "View node: " 
                                             (mapcar #'car node-titles) nil t)))
                (when selected
                  (let ((node-id (cdr (assoc selected node-titles))))
                        (org-supertag-view--goto-node node-id))))
                    (org-supertag-view--goto-node node-id))))
            (message "No nodes found for tag: %s" tag)))


(provide 'org-supertag-view)

;;; org-supertag-view.el ends here

