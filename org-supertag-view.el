;;; org-supertag-view.el --- View system for org-supertag -*- lexical-binding: t -*-

(require 'org-supertag-db)
(require 'org-supertag-relation)

;; Database status check function
(defun org-supertag-db-initialized-p ()
  "Check if the org-supertag database is initialized.
Returns non-nil if the database has been loaded and contains data."
  (and (boundp 'org-supertag-db--object)
       org-supertag-db--object
       (> (hash-table-count org-supertag-db--object) 0)))

  ;; Customization options
(defgroup org-supertag-view nil
  "Customization for org-supertag view system."
  :group 'org-supertag)

(defcustom org-supertag-view-max-cooccurring-tags 15
  "Maximum number of co-occurring tags to display in tag discovery view."
  :type 'integer
  :group 'org-supertag-view)

(defvar org-supertag-view--current-filters nil
  "Current filter tags for tag-discover mode.")

;; 添加自定义设置选项
(defcustom org-supertag-view-display-side 'right
  "Side to display org-supertag view panels.
Options are 'right, 'left, 'bottom, 'top, or nil (use default display behavior)."
  :type '(choice (const :tag "Right side" right)
                (const :tag "Left side" left)
                (const :tag "Bottom" bottom)
                (const :tag "Top" top)
                (const :tag "Default behavior" nil))
  :group 'org-supertag-view)

(defcustom org-supertag-view-window-width 0.4
  "Width of the side window used for org-supertag view panels (as fraction of frame width).
Only used when `org-supertag-view-display-side' is 'right or 'left."
  :type 'float
  :group 'org-supertag-view)

(defcustom org-supertag-view-window-height 0.4
  "Height of the side window used for org-supertag view panels (as fraction of frame height).
Only used when `org-supertag-view-display-side' is 'bottom or 'top."
  :type 'float
  :group 'org-supertag-view)

;;----------------------------------------------------------------------
;; Panel display functions
;;----------------------------------------------------------------------

(defun org-supertag-view--display-buffer-right (buffer)
  "Display BUFFER in side window according to `org-supertag-view-display-side'."
  (display-buffer
   buffer
   `(display-buffer-in-side-window
     (side . ,(or org-supertag-view-display-side 'right))
     (window-width . ,(when (memq org-supertag-view-display-side '(right left))
                       org-supertag-view-window-width))
     (window-height . ,(when (memq org-supertag-view-display-side '(top bottom))
                        org-supertag-view-window-height))
     (preserve-size . t))))


;;----------------------------------------------------------------------
;; View content related to a tag
;;---------------------------------------------------------------------- 

;;;###autoload
(defun org-supertag-view-tag ()
  "Show content related to a tag or explore tags.
If point is on a node title or tag, show multi-column view panel.
Otherwise, show tag-discover panel for exploration."
  (interactive)
  (let ((tag-at-point (org-supertag-view--get-tag-name)))
    (if tag-at-point
        ;; If point is on a tag, show content for that tag with multi-column view
        (org-supertag-tag-columns tag-at-point)
      ;; If point is not on a tag, check if we're on a headline
      (if (org-supertag-view--on-node-title-p)
          ;; If on a headline, show multi-column view mode
          (let ((tag (completing-read "View tag: "
                                    (org-supertag-view--get-all-tags)
                                    nil t)))
            (org-supertag-tag-columns tag))
        ;; Otherwise, ask for a starting tag and show tag-discover mode
        (let ((starting-tag (completing-read "Start tag discovery with: "
                                          (org-supertag-view--get-all-tags)
                                          nil t)))
          (when starting-tag
            (org-supertag-view--show-tag-discover (list starting-tag))))))))

;;----------------------------------------------------------------------
;; Single Tag View Mode (Traditional Table View)
;;----------------------------------------------------------------------

;;;###autoload
(defun org-supertag-view-tag-only ()
  "Show content related to a tag in traditional table view.
If point is on a tag, show content for that tag.
Otherwise, prompt for a tag using completion."
  (interactive)
  (let ((tag-at-point (org-supertag-view--get-tag-name)))
    (if tag-at-point
        ;; If point is on a tag, show content for that tag
        (org-supertag-view--show-content-table tag-at-point)
      ;; If point is not on a tag, prompt for a tag using completion
      (let ((tag (completing-read "View tag: "
                                (org-supertag-view--get-all-tags)
                                nil t)))
        (org-supertag-view--show-content-table tag)))))

(defun org-supertag-view--show-content-table (tag)
  "Show content related to TAG in a new buffer with table format."
  (let ((buf (get-buffer-create (format "*Org SuperTag Table View: %s*" tag))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (org-supertag-view-mode)
        (org-supertag-view--insert-header tag)
        (org-supertag-view--insert-content-table tag)))
    (org-supertag-view--display-buffer-right buf)))

(defun org-supertag-view--insert-content-table (tag)
  "Insert content related to TAG in current buffer using table format."
  (insert "* Related Nodes\n\n")
  (let* ((content (org-supertag-view--get-related-nodes tag))
         (tag-def (org-supertag-tag-get tag))
         (fields (plist-get tag-def :fields)))
    (if content
        (progn
          ;; Build table header
          (insert "|Node|Type|Date|")
          (dolist (field fields)
            (insert (format "%s|" (plist-get field :name))))
          (insert "\n|-\n")
          
          ;; Insert data for each node
          (dolist (item content)
            (let ((field-values (plist-get item :fields)))
              (insert "|")
              (insert (format "%s|%s|%s|"
                            (plist-get item :node)
                            (plist-get item :type)
                            (plist-get item :date)))
              ;; Insert field values
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

;;----------------------------------------------------------------------
;; Get Value at Point
;;----------------------------------------------------------------------  

(defun org-supertag-view--on-node-title-p ()
  "Return non-nil if point is on an org headline.
Only works in Org-mode buffers; returns nil in non-Org buffers."
  (and (derived-mode-p 'org-mode)
       (save-excursion
         (beginning-of-line)
         (looking-at org-heading-regexp))))

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
    (maphash
     (lambda (link-id link-props)
       (when (and (string-match ":node-tag:.*->\\(.+\\)$" link-id)
                  (equal (match-string 1 link-id) tag))
         (when-let* ((node-id (plist-get link-props :from))
                    (node-props (gethash node-id org-supertag-db--object)))
           ;; get all field values
           (let ((field-values
                  (mapcar (lambda (field)
                          (let* ((field-name (plist-get field :name))
                                (field-link (org-supertag-view--get-field-link 
                                           node-id field-name))
                                (value (when field-link 
                                       (plist-get field-link :value))))
                            (cons field-name value)))
                        fields)))
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

;;----------------------------------------------------------------------
;; Refresh View
;;----------------------------------------------------------------------

(defun org-supertag-view-refresh ()
  "Refresh the current view."
  (interactive)
  (cond
   ;; in multi-column view mode
   ((eq major-mode 'org-supertag-column-mode)
    (org-supertag-view--update-column-view))
   ;; in tag discovery mode
   ((eq major-mode 'org-supertag-discover-mode)
    (org-supertag-view--refresh-discover))
   ;; in traditional view (compatible with old code)
   (t
    (let ((tag (car (split-string (buffer-name) ": #"))))
      (when tag
        (org-supertag-tag-columns tag))))))

;;----------------------------------------------------------------------
;; Mode definitions
;;----------------------------------------------------------------------

(defvar org-supertag-discover-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-view--refresh-discover)
    (define-key map (kbd "r") 'org-supertag-view--reset-filters)
    (define-key map (kbd "a") 'org-supertag-view--add-filter)
    (define-key map (kbd "d") 'org-supertag-view--remove-filter)
    (define-key map (kbd "v") 'org-supertag-view--view-node)
    (define-key map (kbd "m") 'org-supertag-view-manage-relations)
    map)
  "Keymap for `org-supertag-discover-mode'.")      

(defvar org-supertag-view-column-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-view--update-column-view)
    (define-key map (kbd "a") 'org-supertag-view-add-column)
    (define-key map (kbd "A") 'org-supertag-view-add-related-column)
    (define-key map (kbd "t") 'org-supertag-view-add-tag-to-column)
    (define-key map (kbd "T") 'org-supertag-view-add-related-tag-to-column)
    (define-key map (kbd "d") 'org-supertag-view-remove-column)
    (define-key map (kbd "r") 'org-supertag-view-reset-columns)
    (define-key map (kbd "v") 'org-supertag-view-view-node-at-point)
    (define-key map (kbd "m") 'org-supertag-view-manage-relations)
    map)
  "Keymap for multi-column tag view mode.")

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
      
;;----------------------------------------------------------------------
;; Tag Discover Mode - Progressive tag filtering
;;----------------------------------------------------------------------

(defun org-supertag-view--show-tag-discover (&optional filter-tags)
  "Show tag-discover panel with optional FILTER-TAGS.
If FILTER-TAGS is nil and no current filters exist, will prompt for a tag."
  (message "Preparing tag discovery view...")
  (let* ((all-tags (org-supertag-view--get-all-tags))
         (old-completion-extra-properties completion-extra-properties)
         (completion-extra-properties '(:annotation-function
                                      (lambda (tag)
                                        (let ((count (length (org-supertag-view--get-nodes-with-tags (list tag)))))
                                          (format " (%d nodes)" count))))))
    (unwind-protect
        (progn
          ;; 先设置过滤器，再显示缓冲区
          (cond
           ;; 如果提供了过滤器标签，直接使用
           (filter-tags
            (setq org-supertag-view--current-filters filter-tags))
           
           ;; 如果没有当前过滤器，需要选择一个起始标签
           ((null org-supertag-view--current-filters)
            (let ((starting-tag (completing-read "Start tag discovery with: "
                                               (completion-table-dynamic
                                                (lambda (_) all-tags))
                                               nil t)))
              (when starting-tag
                (setq org-supertag-view--current-filters (list starting-tag)))))
          
          ;; 保持现有过滤器不变
          (t nil))
        
        ;; 如果有过滤器，显示缓冲区
        (when org-supertag-view--current-filters
          (org-supertag-view--show-tag-discover-buffer))))
      ;; 清理
      (setq completion-extra-properties old-completion-extra-properties)))

(defun org-supertag-view--show-tag-discover-buffer ()
  "Show the tag discover buffer with current filters."
  (let ((buffer (get-buffer-create "*Org SuperTag Discover*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-supertag-discover-mode)
        
        ;; Header with title
        (insert (propertize "Tag Discovery\n\n" 
                           'face '(:height 1.5 :weight bold)))
        
        ;; Operation instructions
        (insert (propertize "Operations:\n" 'face '(:weight bold)))
        (insert " [a] - Add Filter    [d] - Remove Filter    [r] - Reset Filters    [q] - Quit\n")
        (insert " [g] - Refresh    [v] - View Node    [m] - Manage Relations\n\n")
        
        ;; Current filters section
        (insert (propertize "Current Filters:\n" 'face '(:weight bold)))
        (if org-supertag-view--current-filters
            (dolist (tag org-supertag-view--current-filters)
              (let ((remove-button-text (propertize "[-]" 'face '(:foreground "red"))))
                (insert " ")
                (insert-text-button remove-button-text
                                   'action 'org-supertag-view--remove-filter-button-action
                                   'tag tag
                                   'follow-link t
                                   'help-echo "Remove this filter")
                (insert (format " %s\n" tag))))
          (insert "  (no filters applied)\n"))
        (insert "\n")
        
        ;; Co-occurring tags section with debug info
        (insert (propertize "Co-occurring Tags:\n" 'face '(:weight bold)))
        (if org-supertag-view--current-filters
            (let ((nodes (org-supertag-view--get-nodes-with-tags org-supertag-view--current-filters)))
              (message "Found %d nodes for current filters" (length nodes))
              (let ((cooccur-tags (org-supertag-view--get-cooccurring-tags org-supertag-view--current-filters))
                    (count 0))
                (message "Found %d co-occurring tags" (length cooccur-tags))
                (if cooccur-tags
                    (dolist (item cooccur-tags)
                      (let* ((tag (car item))
                             (strength (cdr item))
                             (strength-display (if (> strength 0.1)
                                                 (format " (%.1f)" strength)
                                               "")))
                        (when (and (not (member tag org-supertag-view--current-filters)))
                          (let ((add-button-text (propertize "[+]" 'face '(:foreground "green"))))
                            (insert " ")
                            (insert-text-button add-button-text
                                               'action 'org-supertag-view--add-filter-button-action
                                               'tag tag
                                               'follow-link t
                                               'help-echo "Add this tag to filters")
                            (insert (format " %s%s\n" 
                                           tag 
                                           (propertize strength-display 'face '(:foreground "gray50"))))
                            (setq count (1+ count))))))
                  (insert "  (no co-occurring tags found)\n"))))
          (insert "  (select a filter first)\n"))
        (insert "\n")
        
        ;; Matching nodes section with debug info
        (let ((nodes (org-supertag-view--get-nodes-with-tags org-supertag-view--current-filters)))
          (message "Displaying %d matching nodes" (length nodes))
          (insert (propertize (format "Matching Nodes (%d):\n" (length nodes))
                             'face '(:weight bold)))
          (if nodes
              (let ((node-index 0))
                (dolist (node-id nodes)
                  (let* ((props (gethash node-id org-supertag-db--object))
                         (title (or (plist-get props :title) "无标题"))
                         (type (or (plist-get props :todo-state) ""))
                         (date (format-time-string "%Y-%m-%d" 
                                                  (or (plist-get props :created-at) 
                                                     (current-time))))
                         (view-button-text (propertize "[view]" 'face 'link)))
                    (insert (format " %d. " (setq node-index (1+ node-index))))
                    (insert-text-button view-button-text
                                       'action 'org-supertag-view--view-node-button-action
                                       'node-id node-id
                                       'follow-link t
                                       'help-echo "View this node")
                    (if (string-empty-p type)
                        (insert (format " %s (%s)\n" title date))
                      (insert (format " %s [%s] (%s)\n" title type date))))))
            (insert "  No nodes found with these tags.\n\n")))
        
        (insert "\n")
        (insert (propertize "Note: " 'face '(:weight bold)))
        (insert "Click [+] to add tags to filter, click [-] to remove filters, click [view] to open node\n"))
      
      ;; 确保缓冲区显示
      (message "Displaying tag discovery buffer...")
      (condition-case err
          (progn
            ;; 使用明确的参数显示缓冲区
            (org-supertag-view--display-buffer-right buffer)
            (message "Tag discovery buffer displayed successfully"))
        (error
         (message "Error displaying tag discovery buffer: %s" (error-message-string err)))))))

(defun org-supertag-view--remove-filter-button-action (button)
  "Action to remove a filter tag when clicking its button."
  (let ((tag (button-get button 'tag)))
    (org-supertag-view--remove-filter-tag tag)))

(defun org-supertag-view--add-filter-button-action (button)
  "Action to add a filter tag when clicking its button."
  (let ((tag (button-get button 'tag)))
    (org-supertag-view--add-filter-tag tag)))

(defun org-supertag-view--view-node-button-action (button)
  "Action to view a node when clicking its button."
  (let ((node-id (button-get button 'node-id)))
    (org-supertag-view--goto-node node-id)))

(defun org-supertag-view--goto-node (node-id)
  "Open and jump to the node with NODE-ID.
Uses multiple fallback strategies if direct location fails."
  (when node-id
    (require 'org-supertag-query)
    (cond
     ;; First use org-supertag-query--find-node to find the node, 
     ;; and ensure cursor is at the heading
     ((progn
        (org-supertag-query--find-node node-id)
        ;; 确保当前缓冲区是 Org 模式缓冲区，然后再执行 Org 特定操作
        (when (derived-mode-p 'org-mode)
          (when (org-before-first-heading-p)
            (outline-next-heading))
          (unless (org-at-heading-p)
            (org-back-to-heading t))
          (org-fold-show-entry)
          (recenter))
        t))     
     ;; If failed, try manual search method
     ((let ((node (org-supertag-db-get node-id))
            (file-path nil))
        (when (and (setq file-path (plist-get node :file-path))
                   (file-exists-p file-path))
          (with-current-buffer (find-file-noselect file-path)
            (org-with-wide-buffer
             (goto-char (point-min))
             (when (re-search-forward (format ":ID: %s" node-id) nil t)
               (beginning-of-line)
               (re-search-backward org-heading-regexp nil t)
               ;; 确保在 Org 模式的缓冲区中执行 Org 特定操作
               (when (derived-mode-p 'org-mode)
                 (org-show-context)
                 (org-reveal))
               (switch-to-buffer (current-buffer))
               (recenter)
               t))))))
     ;; if all else fails, try to open the node
     (t 
      (message "Cannot locate node position for ID: %s" node-id)
      nil))))

(defun org-supertag-view--remove-filter-tag (tag)
  "Remove TAG from the current filter tags."
  (setq org-supertag-view--current-filters 
        (delete tag org-supertag-view--current-filters))
  (org-supertag-view--show-tag-discover-buffer))

(defun org-supertag-view--add-filter ()
  "Interactively add a tag to the current filters."
  (interactive)
  (let* ((available-tags (org-supertag-get-all-tags))
         (filtered-tags (seq-difference available-tags org-supertag-view--current-filters))
         (tag (completing-read "Add tag to filter: " filtered-tags nil t)))
    (when (and tag (not (string-empty-p tag)))
      (org-supertag-view--add-filter-tag tag))))

(defun org-supertag-view--remove-filter ()
  "Interactively remove a tag from the current filters."
  (interactive)
  (if (null org-supertag-view--current-filters)
      (message "No filters to remove")
    (let ((tag (completing-read "Remove tag from filter: " 
                              org-supertag-view--current-filters nil t)))
      (when (and tag (not (string-empty-p tag)))
        (org-supertag-view--remove-filter-tag tag)))))

(defun org-supertag-view--view-node ()
  "Interactively select and view a matching node.
This function works in both multi-column view and tag discovery view.
It intelligently determines the current mode and shows only nodes
that are relevant to the current view context."
  (interactive)
  (let* ((nodes nil)
         (node-titles nil))
    
    ;; 根据当前模式获取相应的节点
    (cond
     ;; 多列视图模式
     ((eq major-mode 'org-supertag-column-mode)
      (let ((all-nodes (apply #'append 
                             (mapcar (lambda (tags)
                                      (org-supertag-view--get-nodes-with-tags tags))
                                    org-supertag-view--current-columns))))
        (setq nodes (delete-dups all-nodes))
        (setq node-titles
              (mapcar (lambda (node-id)
                       (let* ((props (gethash node-id org-supertag-db--object))
                              (title (or (plist-get props :title) "无标题"))
                              (tags-text (mapconcat
                                        (lambda (tag-list)
                                          (concat "[" (string-join tag-list ", ") "]"))
                                        (seq-filter
                                         (lambda (tag-list)
                                           (member node-id (org-supertag-view--get-nodes-with-tags tag-list)))
                                         org-supertag-view--current-columns)
                                        " ")))
                         (cons (format "%s %s" title tags-text) node-id)))
                     nodes))))
     
     ;; 标签发现模式
     ((eq major-mode 'org-supertag-discover-mode)
      (setq nodes (org-supertag-view--get-nodes-with-tags org-supertag-view--current-filters))
      (setq node-titles
            (mapcar (lambda (node-id)
                     (let* ((props (gethash node-id org-supertag-db--object))
                            (title (or (plist-get props :title) "无标题"))
                            (tags-text (format "[%s]" (string-join org-supertag-view--current-filters ", "))))
                       (cons (format "%s %s" title tags-text) node-id)))
                   nodes)))
     
     ;; 其他模式（默认行为）
     (t
      (message "Not in a SuperTag view mode. Cannot select nodes.")
      (setq nodes nil)))
    
    ;; 如果找到节点，让用户选择并查看
    (when nodes
      (let* ((selected (completing-read "View node: " 
                                      (mapcar #'car node-titles) nil t)))
        (when selected
          (let ((node-id (cdr (assoc selected node-titles))))
            (org-supertag-view--goto-node node-id)))))))

;; 保留这个函数作为向后兼容，但直接调用通用函数
(defun org-supertag-view--view-node-from-columns ()
  "Select and view a node from the current column view display.
This is now just a wrapper around `org-supertag-view--view-node`
for backward compatibility."
  (interactive)
  (org-supertag-view--view-node))

(defun org-supertag-view--manage-relations ()
  "Open the relation management interface for a tag."
  (interactive)
  (if (and org-supertag-view--current-filters 
           (= (length org-supertag-view--current-filters) 1))
      (let* ((tag-name (car org-supertag-view--current-filters))
             (tag-id (org-supertag-tag-get-id-by-name tag-name)))
        (if tag-id
            (org-supertag-relation--show-management-interface tag-id)
          (message "Cannot find ID for tag: %s" tag-name)))
    (call-interactively #'org-supertag-relation-manage)))

(defun org-supertag-view-manage-relations ()
  "Open the relation management for the current tag."
  (interactive)
  (org-supertag-view--manage-relations))

(defun org-supertag-view--get-nodes-with-tags (tags)
  "Get all nodes that contain all TAGS.
Returns a list of node IDs."
  (unless (and tags (listp tags))
    (error "Invalid tags argument: %S" tags))
  (let ((result nil))
    (maphash
     (lambda (id props)
       (when (eq (plist-get props :type) :node)
         (let ((node-tags (org-supertag-node-get-tags id)))
           (when (cl-every (lambda (tag) 
                            (member tag node-tags))
                          tags)
             (push id result)))))
     org-supertag-db--object)
    (nreverse result)))

(defun org-supertag-view--get-cooccurring-tags (filter-tags)
  "Get tags co-occurring with FILTER-TAGS.
Returns list of (tag-name . count) pairs."
  (let ((result (make-hash-table :test 'equal))
        (nodes (org-supertag-view--get-nodes-with-tags filter-tags)))
    ;; Find co-occurring tags for filtered nodes
    (dolist (node-id nodes)
      (dolist (tag (org-supertag-node-get-tags node-id))
        (unless (member tag filter-tags)
          (puthash tag (1+ (gethash tag result 0)) result))))
    
    ;; Convert to alist and sort by frequency
    (let ((alist nil))
      (maphash (lambda (k v) (push (cons k v) alist)) result)
      (sort alist (lambda (a b) (> (cdr a) (cdr b)))))))

(defun org-supertag-view--add-filter-tag (tag)
  "Add TAG to the current filter tags."
  (push tag org-supertag-view--current-filters)
  (setq org-supertag-view--current-filters (delete-dups org-supertag-view--current-filters))
  (org-supertag-view--show-tag-discover-buffer))

(defun org-supertag-view--reset-filters ()
  "Reset all filters."
  (interactive)
  (setq org-supertag-view--current-filters nil)
  (org-supertag-view--show-tag-discover nil))

(defun org-supertag-view--refresh-discover ()
  "Refresh the tag-discover buffer."
  (interactive)
  (when (eq major-mode 'org-supertag-discover-mode)
    (org-supertag-view--show-tag-discover-buffer)))

;;----------------------------------------------------------------------
;; Public API
;;----------------------------------------------------------------------

;;;###autoload
(defun org-supertag-tag-discover (&optional filter-tags)
  "Show tag discovery interface for progressive filtering.
Optional FILTER-TAGS can be provided to initialize the filter."
  (interactive)
  (message "Starting tag discovery interface...")
  ;; 简化数据库检查逻辑
  (if (not (and (boundp 'org-supertag-db--object)
                org-supertag-db--object
                (> (hash-table-count org-supertag-db--object) 0)))
      ;; 数据库检查失败
      (if (yes-or-no-p "SuperTag database is empty or not initialized. Update it now? ")
          (progn
            (call-interactively 'org-supertag-db-update)
            ;; 更新后再次确认数据库状态
            (if (and (boundp 'org-supertag-db--object)
                     (> (hash-table-count org-supertag-db--object) 0))
                (org-supertag-view--show-tag-discover filter-tags)
              (message "Database still empty after update. Cannot proceed.")))
        (message "SuperTag database not initialized. Please run org-supertag-db-update first."))
    
    ;; 数据库已初始化，显示标签发现界面
    (let ((window-config (current-window-configuration)))
      (unwind-protect
          (org-supertag-view--show-tag-discover filter-tags)
        (unless (get-buffer "*Org SuperTag Discover*")
          (message "Failed to create discovery buffer, restoring window configuration")
          (set-window-configuration window-config))))))

;;----------------------------------------------------------------------
;; Multi-column Tag Comparison View
;;----------------------------------------------------------------------

(defvar org-supertag-view--current-columns nil
  "List of tag combinations currently displayed in columns.")

(defun org-supertag-view--show-tag-columns (&optional initial-tag)
  "Show multi-column tag comparison view, optionally starting with INITIAL-TAG."
  (interactive)
  (let ((starting-tag (or initial-tag
                         (completing-read "Start multi-column view with tag: "
                                          (org-supertag-view--get-all-tags)
                                          nil t))))
    (when starting-tag
      (setq org-supertag-view--current-columns (list (list starting-tag)))
      (org-supertag-view--update-column-view))))

(defun org-supertag-tag-columns (&optional initial-tag)
  "Show multi-column tag comparison interface.
Optional INITIAL-TAG can be provided to initialize the first column."
  (interactive)
  (message "Starting multi-column tag view...")
  ;; 数据库检查逻辑
  (if (not (and (boundp 'org-supertag-db--object)
                org-supertag-db--object
                (> (hash-table-count org-supertag-db--object) 0)))
      ;; 数据库检查失败
      (if (yes-or-no-p "SuperTag database is empty or not initialized. Update it now? ")
          (progn
            (call-interactively 'org-supertag-db-update)
            ;; 更新后再次确认数据库状态
            (if (and (boundp 'org-supertag-db--object)
                     (> (hash-table-count org-supertag-db--object) 0))
                (org-supertag-view--show-tag-columns initial-tag)
              (message "Database still empty after update. Cannot proceed.")))
        (message "SuperTag database not initialized. Please run org-supertag-db-update first."))
    
    ;; 数据库已初始化，显示多列视图
    (org-supertag-view--show-tag-columns initial-tag)))

;; 为兼容旧代码而保留的函数，但内部使用多列视图
(defun org-supertag-view--show-content (tag)
  "Show content related to TAG in multi-column view.
This function is kept for backward compatibility."
  (org-supertag-tag-columns tag))

(defun org-supertag-view-add-column ()
  "Add a new tag column to the multi-column view."
  (interactive)
  (when (and (boundp 'org-supertag-view--current-columns)
             org-supertag-view--current-columns)
    (let* ((available-tags (org-supertag-view--get-all-tags))
           (tag (completing-read "Add tag for new column: " available-tags nil t)))
      (when (and tag (not (string-empty-p tag)))
        ;; 添加新标签作为独立的一列
        (push (list tag) org-supertag-view--current-columns)
        (org-supertag-view--update-column-view)))))

(defun org-supertag-view-add-tag-to-column ()
  "Add a tag to an existing column in multi-column view."
  (interactive)
  (when (and (boundp 'org-supertag-view--current-columns)
             org-supertag-view--current-columns)
    (let* ((column-names (mapcar (lambda (tags)
                                  (format "%s" (string-join tags ", ")))
                                     org-supertag-view--current-columns))
           (selected-col (completing-read "Select column to add tag: " column-names nil t))
           (col-idx (cl-position selected-col column-names :test 'string=)))
      (when col-idx
        (let* ((available-tags (org-supertag-view--get-all-tags))
               (current-tags (nth col-idx org-supertag-view--current-columns))
               (filtered-tags (seq-difference available-tags current-tags))
               (tag (completing-read "Add tag to column: " filtered-tags nil t)))
          (when (and tag (not (string-empty-p tag)))
            ;; 添加新标签到选定的列
            (push tag (nth col-idx org-supertag-view--current-columns))
            (org-supertag-view--update-column-view)))))))

(defun org-supertag-view-remove-column ()
  "Remove a column from the multi-column view."
  (interactive)
  (when (and (boundp 'org-supertag-view--current-columns)
             org-supertag-view--current-columns
             (> (length org-supertag-view--current-columns) 1)) ;; 至少保留一列
    (let* ((column-names (mapcar (lambda (tags)
                                  (format "%s" (string-join tags ", ")))
                                     org-supertag-view--current-columns))
           (selected-col (completing-read "Remove column: " column-names nil t))
           (col-idx (cl-position selected-col column-names :test 'string=)))
      (when col-idx
        (setq org-supertag-view--current-columns
              (append (cl-subseq org-supertag-view--current-columns 0 col-idx)
                      (cl-subseq org-supertag-view--current-columns (1+ col-idx))))
        (org-supertag-view--update-column-view)))))

(defun org-supertag-view-reset-columns ()
  "Reset the multi-column view."
  (interactive)
  (when (and (boundp 'org-supertag-view--current-columns)
             org-supertag-view--current-columns)
    (setq org-supertag-view--current-columns nil)
    (let ((tag (completing-read "Start fresh with tag: " (org-supertag-view--get-all-tags) nil t)))
      (when (and tag (not (string-empty-p tag)))
        (setq org-supertag-view--current-columns (list (list tag)))
        (org-supertag-view--update-column-view)))))

(defun org-supertag-view--update-column-view ()
  "Update the multi-column tag comparison view with strict column alignment.
Each row shows corresponding entries from different tags, with proper vertical alignment."
  (interactive)
  (let ((buffer (get-buffer-create "*Org SuperTag Columns*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (column-width 45)  ;; 列宽略微增加以适应更多内容
            (padding 1)        ;; 减少列间距，因为我们会使用垂直分隔线
            (separator "│"))   ;; 垂直分隔线字符
        (erase-buffer)
        (org-supertag-column-mode)
        
        ;; 标题
        (insert (propertize "Multi-Column Tag View\n\n" 
                           'face '(:height 1.5 :weight bold)))
        
        ;; 操作说明 - 更新，添加新的快捷键说明
        (insert (propertize "Operations:\n" 'face '(:weight bold)))
        (insert " [a] - Add Column    [A] - Add Related Column    [d] - Remove Column    [r] - Reset    [q] - Quit\n")
        (insert " [t] - Add Tag to Column    [T] - Add Related Tag    [v] - View Node    [m] - Manage Relations\n\n")
        
        ;; 准备每列的数据
        (let* ((all-nodes-lists (mapcar (lambda (tags)
                                         (org-supertag-view--get-nodes-with-tags tags))
                                       org-supertag-view--current-columns))
               (col-count (length all-nodes-lists))
               (max-nodes 0))
          
          ;; 找出最多节点数
          (dolist (nodes all-nodes-lists)
            (setq max-nodes (max max-nodes (length nodes))))
          
          ;; 创建一个用于布局控制的函数
          (cl-flet ((insert-in-column 
                     (col-idx content &optional face fill-char)
                     ;; 在指定列位置插入内容，并填充到列宽度
                     (let* ((current-pos (current-column))
                            (target-pos (* col-idx (1+ column-width)))
                            (padding-needed (- target-pos current-pos)))
                       ;; 如果需要，先填充空格以对齐到目标列位置
                       (when (> padding-needed 0)
                         (insert (make-string padding-needed ?\s)))
                       
                       ;; 插入分隔符（除第一列外）
                       (when (> col-idx 0)
                         (insert (propertize separator 'face '(:foreground "gray50")))
                         (insert " "))
                       
                       ;; 插入内容（带可选的face属性）
                       (let ((text (if face
                                      (propertize content 'face face)
                                    content)))
                         (insert text))
                       
                       ;; 计算剩余空间并填充
                       (let* ((content-width (string-width content))
                              (separator-space (if (> col-idx 0) 2 0)) 
                              (avail-width (- column-width separator-space))
                              (fill-needed (- avail-width content-width)))
                         (when (and (> fill-needed 0) fill-char)
                           (insert (make-string fill-needed fill-char)))))))
            
            ;; 列标题行
            (dotimes (col-idx col-count)
              (let* ((tags (nth col-idx org-supertag-view--current-columns))
                     (tag-text (string-join tags ", "))
                     (display-text (if (> (string-width tag-text) (- column-width 8))
                                      (concat (substring tag-text 0 (- column-width 11)) "...")
                                    tag-text))
                     (header-text (format "Column %d: %s" (1+ col-idx) display-text)))
                (insert-in-column col-idx header-text '(:weight bold) ?\s)))
            
            (insert "\n")
            
            ;; 分隔线
            (dotimes (col-idx col-count)
              (insert-in-column col-idx "" nil ?─))
            (insert "\n\n")
            
            ;; 如果没有节点
            (if (= max-nodes 0)
                (insert "  No nodes found for any combination of tags.\n\n")
              
              ;; 以行为单位处理，每行显示每列的同一位置节点
              (dotimes (row-idx max-nodes)
                ;; 第一行：序号、按钮和标题
                (dotimes (col-idx col-count)
                  (let* ((nodes (nth col-idx all-nodes-lists))
                         (node-id (and (< row-idx (length nodes)) (nth row-idx nodes))))
                    (if node-id
                        (let* ((props (gethash node-id org-supertag-db--object))
                               (title (or (plist-get props :title) "无标题"))
                               (view-button-text (propertize "[view]" 'face 'link))
                               (node-number (format "%d. " (1+ row-idx)))
                               ;; 计算标题最大长度（考虑序号和按钮的空间）
                               (max-title-len (- column-width 12)) ;; 增加预留空间
                               (title-width (string-width title))
                               (title-text (if (> title-width max-title-len)
                                             (truncate-string-to-width title max-title-len nil nil "...")
                                           title)))
                          
                          ;; 插入到列中，序号和按钮
                          (insert-in-column col-idx node-number nil nil)
                          
                          ;; 插入view按钮
                          (insert-text-button view-button-text
                                             'action 'org-supertag-view--view-node-button-action
                                             'node-id node-id
                                             'follow-link t
                                             'help-echo "Click to view this node")
                          
                          ;; 插入标题（带填充）
                          (insert " " title-text))
                      
                      ;; 如果该列在当前行没有节点，插入空白列
                      (insert-in-column col-idx "" nil ?\s))))
                (insert "\n")
                
                ;; 第二行：TODO状态（如果有）
                (let ((has-status nil))
                  (dotimes (col-idx col-count)
                    (let* ((nodes (nth col-idx all-nodes-lists))
                           (node-id (and (< row-idx (length nodes)) (nth row-idx nodes))))
                      (if (and node-id 
                               (let ((props (gethash node-id org-supertag-db--object)))
                                 (not (string-empty-p (or (plist-get props :todo-state) "")))))
                          (let* ((props (gethash node-id org-supertag-db--object))
                                 (type (or (plist-get props :todo-state) ""))
                                 (status-text (format "   [%s]" type)))
                            (setq has-status t)
                            (insert-in-column col-idx status-text '(:foreground "purple") ?\s))
                        (insert-in-column col-idx "" nil ?\s))))
                  
                  ;; 只有当至少有一个状态时才添加换行
                  (when has-status
                    (insert "\n")))
                
                ;; 每个节点组后添加一个空行
                (insert "\n")))
            
            ;; 底部说明，更新快捷键说明
            (goto-char (point-max))
            (insert (propertize "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n" 
                               'face '(:foreground "white")))
            (insert "Press 'a'/'A' to add a column (A: by relation), 'd' to remove, 'v' to view a node\n")
            (insert "Press 't'/'T' to add tag to column (T: by relation), 'm' to manage tag relations\n")
            (insert "Each column displays nodes matching all tags in that column's filter.\n"))))
        (pop-to-buffer buffer))))

(defun org-supertag-view-find-in-all-nodes ()
  "Find the current tag in all nodes."
  (interactive)
  (let ((tag (car (split-string (buffer-name) ": #"))))
    (when tag
      (org-supertag-query-nodes-by-tags (list tag)))))

(defun org-supertag-view-view-node-at-point ()
  "View the node at point in either single or multi-column view."
  (interactive)
  ;; 确保数据库已初始化
  (if (not (and (boundp 'org-supertag-db--object)
                org-supertag-db--object
                (> (hash-table-count org-supertag-db--object) 0)))
      ;; 数据库未初始化
      (if (yes-or-no-p "SuperTag database is empty or not initialized. Update it now? ")
          (progn
            (call-interactively 'org-supertag-db-update)
            ;; 更新后再次确认数据库状态
            (if (and (boundp 'org-supertag-db--object)
                     (> (hash-table-count org-supertag-db--object) 0))
                ;; 数据库已更新，继续执行
                (org-supertag-view--view-node-internal)
              (message "Database still empty after update. Cannot view nodes.")))
        (message "SuperTag database not initialized. Please run org-supertag-db-update first."))
    
    ;; 数据库已初始化，继续执行
    (org-supertag-view--view-node-internal)))

(defun org-supertag-view-ensure-database ()
  "Ensure the org-supertag database is initialized and has data.
If database is empty, offers to update it."
  (interactive)
  (if (org-supertag-db-initialized-p)
      (message "Org-supertag database is initialized with %d entries"
              (hash-table-count org-supertag-db--object))
    (when (yes-or-no-p "Org-supertag database is empty. Update it now? ")
      (call-interactively 'org-supertag-db-update))))

(defun org-supertag-view--view-node-internal ()
  "Internal implementation of node viewing logic."
  (cond
   ;; 当在多列Tag Column模式中
   ((eq major-mode 'org-supertag-column-mode)
    (let ((pos (point))
          (line-start (line-beginning-position))
          (found nil))
      
      ;; 先尝试找到当前行中的view按钮
      (save-excursion
        (goto-char line-start)
        (while (and (< (point) (line-end-position))
          (when (and (looking-at "\\([0-9]+\\)\\. \\[view\\]")
                     ;; 当光标在按钮附近
                     (<= (match-beginning 0) pos)
                     (<= pos (+ (match-beginning 0) 10)))
            (let ((button (button-at (+ (match-beginning 0) 5))))
              (when button
                (setq found t)
                (button-activate button))))
          (forward-char 1)))
      
      ;; 如果没找到按钮，则使用通用节点选择函数
      (unless found
        (org-supertag-view--view-node-from-columns))))
   
   ;; 当在Tag Discover模式中
   ((eq major-mode 'org-supertag-discover-mode)
    (org-supertag-view--view-node))
   
   ;; 当在其他模式中
   (t
    (message "Please move cursor to a node to view it.")))))

(defun org-supertag-view-add-related-column ()
  "Add a new column with a tag related to an existing tag.
This function selects a source tag from the current columns,
then shows all related tags for selection based on tag relationships."
  (interactive)
  ;; 确保我们可以在只读缓冲区中执行
  (message "Adding column based on related tag...")
  (condition-case err
      (let ((inhibit-read-only t))
        (when (and (boundp 'org-supertag-view--current-columns)
                  org-supertag-view--current-columns)
          (let* ((column-names (mapcar (lambda (tags)
                                       (format "%s" (string-join tags ", ")))
                                     org-supertag-view--current-columns))
                (selected-col (completing-read "Select source column for related tags: " 
                                             column-names nil t))
                (col-idx (cl-position selected-col column-names :test 'string=)))
            (when col-idx
              (let* ((source-tags (nth col-idx org-supertag-view--current-columns))
                    (source-tag (if (= (length source-tags) 1)
                                   (car source-tags)
                                 (completing-read "Select source tag: " source-tags nil t)))
                    (source-tag-id (org-supertag-tag-get-id-by-name source-tag)))
                
                (when source-tag-id
                  ;; 获取与源标签相关的所有标签
                  (let* ((related-from (org-supertag-relation-get-all-from source-tag-id))
                        (related-to (org-supertag-relation-get-all-to source-tag-id))
                        (related-all (append related-from related-to))
                        (related-tag-ids (mapcar #'car related-all))
                        (related-tag-names (delq nil (mapcar #'org-supertag-tag-get-name-by-id 
                                                           related-tag-ids)))
                        (relation-choices (mapcar (lambda (tag-name)
                                                  (let* ((tag-id (org-supertag-tag-get-id-by-name tag-name))
                                                         (rel-from (org-supertag-relation-get source-tag-id tag-id))
                                                         (rel-to (org-supertag-relation-get tag-id source-tag-id))
                                                                 (rel-type (or (and rel-from (plist-get rel-from :type))
                                                                     (and rel-to (plist-get rel-to :type))
                                                                     "unknown")))
                                                    (format "%s (%s)" tag-name rel-type)))
                                                related-tag-names)))
                   
                   (if relation-choices
                       (let* ((selected (completing-read "Select related tag: " relation-choices nil t))
                             (tag-name (car (split-string selected " ("))))
                             (tag-id (org-supertag-tag-get-id-by-name tag-name)))
                         (when (and tag-name (not (string-empty-p tag-name)))
                           ;; 添加新标签作为独立的一列
                           (push (list tag-name) org-supertag-view--current-columns)
                           (org-supertag-view--update-column-view)
                           (message "Added new column with related tag: %s" tag-name)))
                         (message "No related tags found for %s" source-tag)))))))))
    (error (message "Error adding related column: %s" (error-message-string err))))
  

(defun org-supertag-view-add-related-tag-to-column ()
  "Add a tag related to existing tags in a column.
This function selects a source tag from the selected column,
then shows all related tags for selection based on tag relationships."
  (interactive)
  ;; 确保我们可以在只读缓冲区中执行
  (message "Adding related tag to column...")
  (condition-case err
      (let ((inhibit-read-only t))
        (when (and (boundp 'org-supertag-view--current-columns)
                  org-supertag-view--current-columns)
          (let* ((column-names (mapcar (lambda (tags)
                                       (format "%s" (string-join tags ", ")))
                                     org-supertag-view--current-columns))
                (selected-col (completing-read "Select column to add related tag: " 
                                             column-names nil t))
                (col-idx (cl-position selected-col column-names :test 'string=)))
            (when col-idx
              (let* ((current-tags (nth col-idx org-supertag-view--current-columns))
                    (source-tag (if (= (length current-tags) 1)
                                   (car current-tags)
                                 (completing-read "Select source tag: " current-tags nil t)))
                    (source-tag-id (org-supertag-tag-get-id-by-name source-tag)))
                
                (when source-tag-id
                  ;; 获取与源标签相关的所有标签
                  (let* ((related-from (org-supertag-relation-get-all-from source-tag-id))
                        (related-to (org-supertag-relation-get-all-to source-tag-id))
                        (related-all (append related-from related-to))
                        (related-tag-ids (mapcar #'car related-all))
                        (related-tag-names (delq nil (mapcar #'org-supertag-tag-get-name-by-id 
                                                           related-tag-ids)))
                        ;; 过滤掉已经在当前列中的标签
                        (filtered-tags (seq-difference related-tag-names current-tags))
                        (relation-choices (mapcar (lambda (tag-name)
                                                  (let* ((tag-id (org-supertag-tag-get-id-by-name tag-name))
                                                         (rel-from (org-supertag-relation-get source-tag-id tag-id))
                                                         (rel-to (org-supertag-relation-get tag-id source-tag-id))
                                                         (rel-type (or (and rel-from (plist-get rel-from :type))
                                                                     (and rel-to (plist-get rel-to :type))
                                                                     "unknown")))
                                                    (format "%s (%s)" tag-name rel-type)))
                                                filtered-tags)))
                    
                    (if relation-choices
                        (let* ((selected (completing-read "Select related tag to add: " relation-choices nil t))
                              (tag-name (car (split-string selected " ("))))
                              (tag-id (org-supertag-tag-get-id-by-name tag-name)))
                            (when (and tag-name (not (string-empty-p tag-name)))
                              ;; 添加相关标签到选定的列
                              (push tag-name (nth col-idx org-supertag-view--current-columns))
                                (org-supertag-view--update-column-view)
                                (message "Added related tag '%s' to column" tag-name)))
                            (message "No related tags found for %s or all related tags already in column" source-tag)))))))))
    (error (message "Error adding related tag to column: %s" (error-message-string err))))

(provide 'org-supertag-view)

;;; org-supertag-view.el ends here
