;;; org-supertag-view.el --- View system for org-supertag -*- lexical-binding: t -*-

(require 'org-supertag-db)
(require 'org-supertag-relation)

;; 添加可选的语义相似性支持
(declare-function org-supertag-sim-find-similar "org-supertag-sim")
(declare-function org-supertag-sim-init "org-supertag-sim")
(defvar org-supertag-sim--initialized)

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

(defun org-supertag-view--insert-header (tag)
  "Insert header information for TAG in the current buffer."
  (let ((tag-def (org-supertag-tag-get tag)))
    (insert (propertize (format "Content for tag #%s\n\n" tag)
                        'face '(:height 1.5 :weight bold)))
    ;; Add tag description if exists
    (when-let* ((desc (plist-get tag-def :description)))
      (unless (string-empty-p desc)
        (insert (format "Description: %s\n\n" desc))))
    (insert (propertize "Operations:\n" 'face '(:weight bold)))
    (insert " [q] - Quit    [g] - Refresh    [v] - View Node\n\n")))

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

(defvar org-supertag-view-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-view-refresh)
    (define-key map (kbd "v") 'org-supertag-view--view-node-from-table)
    map)
  "Keymap for traditional table view mode.")

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
    (define-key map (kbd "d") 'org-supertag-view-remove-column)
    (define-key map (kbd "R") 'org-supertag-view-reset-columns)
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
  (let* ((all-tags (org-supertag-view--get-all-tags)))
    (unwind-protect
    (progn

      (cond

       (filter-tags
        (setq org-supertag-view--current-filters filter-tags))

       ((null org-supertag-view--current-filters)
        (let ((starting-tag (completing-read "Start tag discovery with: "
                                           (org-supertag-view--get-all-tags)
                                           nil t)))
          (when starting-tag
            (setq org-supertag-view--current-filters (list starting-tag)))))
      
      (t nil))
    (when org-supertag-view--current-filters
          (org-supertag-view--show-tag-discover-buffer))))

      ))

(defun org-supertag-view--get-similar-tags (tag-id)
  "获取与 TAG-ID 语义相似的标签.
返回 (tag . similarity) 对的列表，或在出错时返回 nil."
  (when (and (featurep 'org-supertag-sim)
             (bound-and-true-p org-supertag-sim--initialized))
    (condition-case err
        (org-supertag-sim-find-similar tag-id 5)
      (error
       (message "获取相似标签失败: %s" (error-message-string err))
       nil))))

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
        
        ;; Co-occurring tags section
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
        
        ;; 添加语义相似标签部分
        (when (and org-supertag-view--current-filters
                   (= (length org-supertag-view--current-filters) 1))
          (insert (propertize "Semantically Similar Tags:\n" 'face '(:weight bold)))
          (let* ((current-tag (car org-supertag-view--current-filters))
                 (similar-tags (org-supertag-view--get-similar-tags current-tag)))
            (if similar-tags
                (dolist (item similar-tags)
                  (let* ((similar-tag (car item))
                         (similarity (cdr item))
                         (add-button-text (propertize "[+]" 'face '(:foreground "green"))))
                    (insert " ")
                    (insert-text-button add-button-text
                                     'action 'org-supertag-view--add-filter-button-action
                                     'tag similar-tag
                                     'follow-link t
                                     'help-echo "Add this tag to filters")
                    (insert (format " %s (similarity: %.2f)\n" 
                                   similar-tag similarity))))
              (insert "  (no similar tags found)\n")))
          (insert "\n"))
        
        ;; Matching nodes section 
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
     
     ;; 表格视图模式
     ((and (eq major-mode 'org-mode) 
           (bound-and-true-p org-supertag-view-mode)
           (string-match-p "\\*Org SuperTag Table View:" (buffer-name)))
      (let* ((buffer-name (buffer-name))
             (tag (progn
                    (string-match "\\*Org SuperTag Table View: \\(.*\\)\\*" buffer-name)
                    (match-string 1 buffer-name))))
        (when tag
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
                       nodes)))))
     
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
  ;; Check if database is initialized
  (if (not (and (boundp 'org-supertag-db--object)
                org-supertag-db--object
                (> (hash-table-count org-supertag-db--object) 0)))
      (if (yes-or-no-p "SuperTag database is empty or not initialized. Update it now? ")
          (progn
            (call-interactively 'org-supertag-db-update)
            ;; After update db, check if database is initialized
            (if (and (boundp 'org-supertag-db--object)
                     (> (hash-table-count org-supertag-db--object) 0))
                (org-supertag-view--show-tag-columns initial-tag)
              (message "Database still empty after update. Cannot proceed.")))
        (message "SuperTag database not initialized. Please run org-supertag-db-update first."))    
    ;; Db is initialized
    (org-supertag-view--show-tag-columns initial-tag)))

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

(defun org-supertag-view-add-cooccurring-column ()
  "Add a new column with a tag that co-occurs with the selected column's tags."
  (interactive)
  (when (and (boundp 'org-supertag-view--current-columns)
             org-supertag-view--current-columns)
    (let* ((column-names (mapcar (lambda (tags)
                                  (format "%s" (string-join tags ", ")))
                                org-supertag-view--current-columns))
           (selected-col (completing-read "Select column to find co-occurring tags: " 
                                        column-names nil t))
           (col-idx (cl-position selected-col column-names :test 'string=)))
      (when col-idx
        (let* ((current-tags (nth col-idx org-supertag-view--current-columns))
               ;; 获取所有与当前标签共同出现的标签
               (nodes (org-supertag-view--get-nodes-with-tags current-tags))
               (cooccurring-tags (org-supertag-view--get-cooccurring-tags current-tags))
               ;; 过滤掉已经在当前列中的标签
               (available-tags 
                (seq-filter (lambda (tag-pair)
                             (not (member (car tag-pair) current-tags)))
                           cooccurring-tags)))
          (if available-tags
              (let* ((choices 
                      (mapcar (lambda (tag-pair)
                               (format "%s (co-occurs %d times)" 
                                      (car tag-pair) (cdr tag-pair)))
                             available-tags))
                     (selected (completing-read "Choose co-occurring tag for new column: " 
                                             choices nil t))
                     (selected-tag (car (nth (cl-position selected choices :test 'equal)
                                           available-tags))))
                (when (and selected-tag (not (string-empty-p selected-tag)))
                  ;; 添加新的列
                  (push (list selected-tag) org-supertag-view--current-columns)
                  (org-supertag-view--update-column-view)))
            (message "No co-occurring tags found for the selected column")))))))

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

(defun org-supertag-view-add-related-column ()
  "Add a new column with a related tag (either by defined relations or co-occurrence)."
  (interactive)
  (when (and (boundp 'org-supertag-view--current-columns)
             org-supertag-view--current-columns)
    (let* ((column-names (mapcar (lambda (tags)
                                  (format "%s" (string-join tags ", ")))
                                org-supertag-view--current-columns))
           (selected-col (completing-read "Select column to find related tags: " 
                                        column-names nil t))
           (col-idx (cl-position selected-col column-names :test 'string=)))
      (when col-idx
        (let* ((current-tags (nth col-idx org-supertag-view--current-columns))
               (tag-id (org-supertag-tag-get-id-by-name (car current-tags)))
               ;; Get manually defined relations
               (related-by-similar (org-supertag-relation-find-tags-by-group tag-id 'similar))
               (related-by-parent (org-supertag-relation-find-tags-by-group tag-id 'parent))
               (related-by-child (org-supertag-relation-find-tags-by-group tag-id 'child))
               (manual-related-names 
                (mapcar (lambda (id) 
                         (format "%s (defined relation)" 
                                (org-supertag-tag-get-name-by-id id)))
                       (append related-by-similar related-by-parent related-by-child)))
               ;; Get co-occurring tags
               (cooccurring-tags (org-supertag-view--get-cooccurring-tags current-tags))
               (cooccur-names
                (mapcar (lambda (tag-pair)
                         (format "%s (co-occurs %d times)" 
                                (car tag-pair) (cdr tag-pair)))
                       cooccurring-tags))
               ;; Combine both types of relations
               (all-related (append manual-related-names cooccur-names)))
          (if all-related
              (let* ((selected (completing-read "Choose related tag for new column: " 
                                             all-related nil t))
                     ;; Extract actual tag name from the formatted string
                     (selected-tag (replace-regexp-in-string " (.*)" "" selected)))
                (when (and selected-tag (not (string-empty-p selected-tag)))
                  ;; Add new column
                  (push (list selected-tag) org-supertag-view--current-columns)
                  (org-supertag-view--update-column-view)))
            (message "No related tags found for the selected column")))))))

(defun org-supertag-view-remove-column ()
  "Remove a column from the multi-column view."
  (interactive)
  (when (and (boundp 'org-supertag-view--current-columns)
             org-supertag-view--current-columns)
    (let* ((column-names (mapcar (lambda (tags)
                                  (format "%s" (string-join tags ", ")))
                                     org-supertag-view--current-columns))
           (selected-col (completing-read "Remove column: " column-names nil t))
           (col-idx (cl-position selected-col column-names :test 'string=)))
      (when col-idx
        (setq org-supertag-view--current-columns
              (append (cl-subseq org-supertag-view--current-columns 0 col-idx)
                      (cl-subseq org-supertag-view--current-columns (1+ col-idx))))
        ;; If all columns are removed, reset the view
        (if (null org-supertag-view--current-columns)
            (let ((tag (completing-read "Start fresh with tag: " 
                                      (org-supertag-view--get-all-tags)
                                      nil t)))
              (when (and tag (not (string-empty-p tag)))
                (setq org-supertag-view--current-columns (list (list tag)))
                (org-supertag-view--update-column-view)))
          (org-supertag-view--update-column-view))))))

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

(defun org-supertag-view--wrap-text (text available-width)
  "Wrap TEXT to fit within AVAILABLE-WIDTH.
Returns a list of wrapped lines."
  (if (<= (string-width text) available-width)
      (list text)
    (let ((words (split-string text " "))
          (lines '())
          (current-line ""))
      (dolist (word words)
        (let ((new-line (if (string-empty-p current-line)
                           word
                         (concat current-line " " word))))
          (if (<= (string-width new-line) available-width)
              (setq current-line new-line)
            (push current-line lines)
            (setq current-line word))))
      (when (not (string-empty-p current-line))
        (push current-line lines))
      (nreverse lines))))

(defun org-supertag-view--update-column-view ()
  "Update the multi-column tag comparison view with strict column alignment.
Each row shows corresponding entries from different tags, with proper vertical alignment."
  (interactive)
  (let ((buffer (get-buffer-create "*Org SuperTag Columns*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (column-width 45)  
            (padding 1)        
            (separator "│"))   
        (erase-buffer)
        (org-supertag-column-mode)
        (insert (propertize "Multi-Column Tag View\n\n" 
                           'face '(:height 1.5 :weight bold)))
        (insert (propertize "Operations:\n" 'face '(:weight bold)))
        (insert " [a] - Add Column    [A] - Add Related Column    [d] - Remove Column    [R] - Reset\n")
        (insert " [t] - Add Tag to Column    [v] - View Node      [m] - Manage Relations\n\n")
        (let* ((all-nodes-lists (mapcar (lambda (tags)
                                         (org-supertag-view--get-nodes-with-tags tags))
                                       org-supertag-view--current-columns))
               (col-count (length all-nodes-lists))
               (max-nodes 0))
          (dolist (nodes all-nodes-lists)
            (setq max-nodes (max max-nodes (length nodes))))
          
          (cl-flet ((insert-in-column 
                     (col-idx content &optional face fill-char first-line-prefix other-lines-prefix)
                     (let* ((current-pos (current-column))
                            (target-pos (* col-idx (1+ column-width)))
                            (padding-needed (- target-pos current-pos))
                            (separator-space (if (> col-idx 0) 2 0))
                            (avail-width (- column-width separator-space))
                            (prefix-width (if first-line-prefix 
                                            (string-width first-line-prefix) 0))
                            (wrapped-lines (org-supertag-view--wrap-text content 
                                                                        (- avail-width prefix-width)))
                            (first-line t))
                       
                       (when (> padding-needed 0)
                         (insert (make-string padding-needed ?\s)))
                       
                       (when (> col-idx 0)
                         (insert (propertize separator 'face '(:foreground "gray50")))
                         (insert " "))
                       
                       (dolist (line wrapped-lines)
                         (when (not first-line)
                           (insert "\n")
                           (insert (make-string target-pos ?\s))
                           (when (> col-idx 0)
                             (insert (propertize separator 'face '(:foreground "gray50")))
                             (insert " ")))
                         
                         (when (and first-line first-line-prefix)
                           (insert first-line-prefix))
                         (when (and (not first-line) other-lines-prefix)
                           (insert other-lines-prefix))
                         
                         (let ((text (if face
                                        (propertize line 'face face)
                                      line)))
                           (insert text))
                         
                         (when (and first-line (= (length wrapped-lines) 1))
                           (let* ((content-width (+ (string-width line) 
                                                  (if first-line-prefix 
                                                      (string-width first-line-prefix) 
                                                    0)))
                                  (fill-needed (- avail-width content-width)))
                             (when (and (> fill-needed 0) fill-char)
                               (insert (make-string fill-needed fill-char)))))
                         
                         (setq first-line nil)))))
            
            ;; Insert column headers
            (dotimes (col-idx col-count)
              (let* ((tags (nth col-idx org-supertag-view--current-columns))
                     (tag-text (string-join tags ", "))
                     (header-text (format "Column %d: %s" (1+ col-idx) tag-text)))
                (insert-in-column col-idx header-text '(:weight bold) ?\s)))
            
            (insert "\n")
            
            ;; Insert separator line
            (dotimes (col-idx col-count)
              (insert-in-column col-idx "" nil ?─))
            (insert "\n\n")
            
            (if (= max-nodes 0)
                (insert "  No nodes found for any combination of tags.\n\n")
              
              ;; Insert node content
              (dotimes (row-idx max-nodes)
                (dotimes (col-idx col-count)
                  (let* ((nodes (nth col-idx all-nodes-lists))
                         (node-id (and (< row-idx (length nodes)) 
                                     (nth row-idx nodes)))
                         (view-button-text (propertize "[view]" 'face 'link)))
                    (when node-id
                      (let* ((props (gethash node-id org-supertag-db--object))
                             (title (or (plist-get props :title) "No title"))
                             (node-number (format "%d. " (1+ row-idx))))
                        (insert-in-column col-idx title nil nil 
                                        (concat node-number view-button-text " ")
                                        "    ")))))
                (insert "\n")
                
                ;; Insert status if exists
                (let ((has-status nil))
                  (dotimes (col-idx col-count)
                    (let* ((nodes (nth col-idx all-nodes-lists))
                           (node-id (and (< row-idx (length nodes)) 
                                       (nth row-idx nodes))))
                      (when (and node-id 
                               (let ((props (gethash node-id org-supertag-db--object)))
                                 (not (string-empty-p (or (plist-get props :todo-state) 
                                                        "")))))
                        (let* ((props (gethash node-id org-supertag-db--object))
                               (type (or (plist-get props :todo-state) ""))
                               (status-text (format "   [%s]" type)))
                          (setq has-status t)
                          (insert-in-column col-idx status-text 
                                          '(:foreground "purple") ?\s)))))
                  
                  (when has-status
                    (insert "\n")))
                
                (insert "\n")))
            
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
  (if (not (and (boundp 'org-supertag-db--object)
                org-supertag-db--object
                (> (hash-table-count org-supertag-db--object) 0)))
      (if (yes-or-no-p "SuperTag database is empty or not initialized. Update it now? ")
          (progn
            (call-interactively 'org-supertag-db-update)
            (if (and (boundp 'org-supertag-db--object)
                     (> (hash-table-count org-supertag-db--object) 0))
                (org-supertag-view--view-node-internal)
              (message "Database still empty after update. Cannot view nodes.")))
        (message "SuperTag database not initialized. Please run org-supertag-db-update first."))
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
   ((eq major-mode 'org-supertag-column-mode)
    (let ((pos (point))
          (line-start (line-beginning-position))
          (found nil))
      (save-excursion
        (goto-char line-start)
        (while (and (< (point) (line-end-position))
                   (when (and (looking-at "\\([0-9]+\\)\\. \\[view\\]")
                            (<= (match-beginning 0) pos))
                     (let ((button (button-at (+ (match-beginning 0) 5))))
                       (when button
                         (setq found t)
                         (button-activate button)))))
          (forward-char 1)))
      (unless found
        (org-supertag-view--view-node-from-columns))))
   ((eq major-mode 'org-supertag-discover-mode)
    (org-supertag-view--view-node))
   (t
    (message "Please move cursor to a node to view it."))))



(provide 'org-supertag-view)

;;; org-supertag-view.el ends here
