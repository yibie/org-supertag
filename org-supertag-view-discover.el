;;; org-supertag-view-discover.el --- Tag discovery view for org-supertag -*- lexical-binding: t -*-

(require 'org-supertag-view-utils)
(require 'org-supertag-db)
(require 'org-supertag-relation)

;;----------------------------------------------------------------------
;; Tag Discover Mode - Progressive tag filtering
;;----------------------------------------------------------------------

(defun org-supertag-view--show-tag-discover (&optional filter-tags)
  "Show tag-discover panel with optional FILTER-TAGS.
If FILTER-TAGS is nil and no current filters exist, will prompt for a tag."
  (message "Preparing tag discovery view...")
  (let* ((all-tags (org-supertag-view--get-all-tags)))
    (cond
     (filter-tags
      (setq org-supertag-view--current-filters
            (delete-dups (append org-supertag-view--current-filters filter-tags))))
     
     ((null org-supertag-view--current-filters)
      (let ((starting-tag (completing-read "Start tag discovery with: "
                                         (org-supertag-view--get-all-tags)
                                         nil t)))
        (when starting-tag
          (setq org-supertag-view--current-filters (list starting-tag))))))
    
    (when org-supertag-view--current-filters
      (org-supertag-view--show-tag-discover-buffer))))

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
        
        ;; Matching nodes section 
        (if org-supertag-view--current-filters
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
          (progn
            (insert (propertize "Matching Nodes:\n" 'face '(:weight bold)))
            (insert "  (select a filter first)\n\n")))
        
        (insert "\n")
        (insert (propertize "Note: " 'face '(:weight bold)))
        (insert "Click [+] to add tags to filter, click [-] to remove filters, click [view] to open node\n"))
      

      (message "Displaying tag discovery buffer...")
      (condition-case err
          (progn
            (org-supertag-view--display-buffer-right buffer)
            (select-window (get-buffer-window buffer))
            (goto-char (point-min))
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
  (if org-supertag-view--current-filters
      (org-supertag-view--show-tag-discover-buffer)
    (org-supertag-view--show-tag-discover nil)))

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
    (cond
     ;; Multi-column view mode
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

     ;; Tag discovery mode
     ((eq major-mode 'org-supertag-discover-mode)
      (setq nodes (org-supertag-view--get-nodes-with-tags org-supertag-view--current-filters))
      (setq node-titles
            (mapcar (lambda (node-id)
                     (let* ((props (gethash node-id org-supertag-db--object))
                            (title (or (plist-get props :title) "No Title"))
                            (tags-text (format "[%s]" (string-join org-supertag-view--current-filters ", "))))
                       (cons (format "%s %s" title tags-text) node-id)))
                   nodes)))
     
     ;; Table view mode
     ((and (eq major-mode 'org-mode) 
           (bound-and-true-p org-supertag-view-mode)
           (string-match-p "\\*Org SuperTag Table View: \\(.*\\)\\*" (buffer-name)))
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
     
     ;; Other modes (default behavior)
     (t
      (message "Not in a SuperTag view mode. Cannot select nodes.")
      (setq nodes nil)))
    
    ;; If nodes are found, let the user select and view
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
If TAGS is nil, returns all nodes.
Returns a list of node IDs."
  (let ((result nil))
    (maphash
     (lambda (id props)
       (when (eq (plist-get props :type) :node)
         (if tags
             (let ((node-tags (org-supertag-node-get-tags id)))
               (when (cl-every (lambda (tag) 
                                (member tag node-tags))
                             tags)
                 (push id result)))
           (push id result))))
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
  (unless (member tag org-supertag-view--current-filters)
    (push tag org-supertag-view--current-filters)
    (setq org-supertag-view--current-filters (delete-dups org-supertag-view--current-filters)))
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
;; Main functions
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
    (define-key map (kbd "1") 'org-supertag-view-switch-to-tag-only)
    (define-key map (kbd "3") 'org-supertag-view-switch-to-columns)
    (define-key map (kbd "n") 'next-line)  
    (define-key map (kbd "p") 'previous-line)  
    map)
  "Keymap for `org-supertag-discover-mode'.")   

;;;###autoload
(defun org-supertag-view-discover (&optional filter-tags)
  "Show tag discovery interface for progressive filtering.
Optional FILTER-TAGS can be provided to initialize the filter."
  (interactive)
  (message "Starting tag discovery interface...")
  (if (not (and (boundp 'org-supertag-db--object)
                org-supertag-db--object
                (> (hash-table-count org-supertag-db--object) 0)))
      (if (yes-or-no-p "SuperTag database is empty or not initialized. Update it now? ")
          (progn
            (call-interactively 'org-supertag-db-update)

            (if (and (boundp 'org-supertag-db--object)
                     (> (hash-table-count org-supertag-db--object) 0))
                (progn
                  (unless filter-tags
                    (setq org-supertag-view--current-filters nil))
                  (org-supertag-view--show-tag-discover filter-tags))
              (message "Database still empty after update. Cannot proceed.")))
        (message "SuperTag database not initialized. Please run org-supertag-db-update first."))

    (let ((window-config (current-window-configuration)))
      (unwind-protect
          (progn
            (unless filter-tags
              (setq org-supertag-view--current-filters nil))
            (org-supertag-view--show-tag-discover filter-tags))
        (unless (get-buffer "*Org SuperTag Discover*")
          (message "Failed to create discovery buffer, restoring window configuration")
          (set-window-configuration window-config))))))


(provide 'org-supertag-view-discover)