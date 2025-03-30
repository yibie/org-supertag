;;; org-supertag-view-column.el --- Multi-column tag comparison view for org-supertag -*- lexical-binding: t -*-

(require 'org-supertag-view-utils)

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
      (org-supertag-view--refresh-column-view))))

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
        (org-supertag-view--refresh-column-view)))))

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

               (nodes (org-supertag-view--get-nodes-with-tags current-tags))
               (cooccurring-tags (org-supertag-view--get-cooccurring-tags current-tags))

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
                  (org-supertag-view--refresh-column-view)))
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
            ;; Add new tag to the selected column
            (push tag (nth col-idx org-supertag-view--current-columns))
            (org-supertag-view--refresh-column-view)))))))

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
                  (org-supertag-view--refresh-column-view)))
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
                (org-supertag-view--refresh-column-view)))
          (org-supertag-view--refresh-column-view))))))

(defun org-supertag-view-reset-columns ()
  "Reset the multi-column view."
  (interactive)
  (when (and (boundp 'org-supertag-view--current-columns)
             org-supertag-view--current-columns)
    (setq org-supertag-view--current-columns nil)
    (let ((tag (completing-read "Start fresh with tag: " (org-supertag-view--get-all-tags) nil t)))
      (when (and tag (not (string-empty-p tag)))
        (setq org-supertag-view--current-columns (list (list tag)))
        (org-supertag-view--refresh-column-view)))))

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


(defun org-supertag-view-column-node ()
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


(defun org-supertag-view--refresh-column-view ()
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
                         (button-activate button)))
                   (forward-char 1)))
          (unless found
            (org-supertag-view--view-node-from-columns))))
      (unless found
        (org-supertag-view--view-node-from-columns))))
   ((eq major-mode 'org-supertag-discover-mode)
    (org-supertag-view--view-node))
   (t
    (message "Please move cursor to a node to view it."))))

;;----------------------------------------------------------------------
;; Save and load column view configurations
;;----------------------------------------------------------------------
(defun org-supertag-view-save-current-columns ()
  "Save current column view configuration for future use."
  (interactive)
  (when (eq major-mode 'org-supertag-column-mode)
    (let* ((name (read-string "Enter a name for this view configuration: "))
           (current-layout (copy-tree org-supertag-view--current-columns))
           (existing-entry (assoc name org-supertag-view-saved-columns)))
      (if existing-entry
          (when (y-or-n-p (format "View name '%s' already exists. Overwrite? " name))
            (setcdr existing-entry current-layout)
            (org-supertag-view--save-columns-to-file)
            (message "View configuration updated: %s" name))
        (push (cons name current-layout) org-supertag-view-saved-columns)
        (org-supertag-view--save-columns-to-file)
        (message "View configuration saved: %s" name)))))

(defun org-supertag-view-load-saved-columns ()
  "Load a saved column view configuration."
  (interactive)
  (org-supertag-view--load-saved-columns)
  (if (null org-supertag-view-saved-columns)
      (message "No saved view configurations")
    (let* ((names (mapcar #'car org-supertag-view-saved-columns))
           (selected (completing-read "Select a view configuration to load: " names nil t)))
      (when selected
        (let ((columns (cdr (assoc selected org-supertag-view-saved-columns))))
          (setq org-supertag-view--current-columns (copy-tree columns))
          (org-supertag-view--refresh-column-view)
          (message "View configuration loaded: %s" selected))))))

(defun org-supertag-view--load-saved-columns ()
  "Load saved column view configurations from file."
  (when (file-exists-p org-supertag-view-columns-file)
    (condition-case err
        (setq org-supertag-view-saved-columns
              (with-temp-buffer
                (insert-file-contents org-supertag-view-columns-file)
                (read (current-buffer))))
      (error
       (message "Error loading column configurations: %s" (error-message-string err))
       nil))))

(defun org-supertag-view--save-columns-to-file ()
  "Save current column view configurations to file."
  (with-temp-file org-supertag-view-columns-file
    (insert ";; org-supertag column view configurations\n\n")
    (pp org-supertag-view-saved-columns (current-buffer))))

(org-supertag-view--ensure-columns-file)
(org-supertag-view--load-saved-columns)

;;----------------------------------------------------------------------
;; View switching functions
;;----------------------------------------------------------------------

(defun org-supertag-view-switch-to-tag-only ()
  "Switch to single tag table view from current view."
  (interactive)
  (let ((current-tag nil))
    (cond
     ;; Switch from multi-column view
     ((eq major-mode 'org-supertag-column-mode)
      (when org-supertag-view--current-columns
        (when (= (length (car org-supertag-view--current-columns)) 1)
          (setq current-tag (caar org-supertag-view--current-columns)))))
     
     ;; Switch from tag discovery view
     ((eq major-mode 'org-supertag-discover-mode)
      (when (and org-supertag-view--current-filters
                 (= (length org-supertag-view--current-filters) 1))
        (setq current-tag (car org-supertag-view--current-filters)))))
    
    (if current-tag
        (progn
          (quit-window)
          (org-supertag-view--show-content-table current-tag))
      (call-interactively 'org-supertag-view-table))))

(defun org-supertag-view-switch-to-discover ()
  "Switch to tag discovery view from current view."
  (interactive)
  (let ((current-tag nil))
    (cond
     ;; Switch from table view
     ((and (eq major-mode 'org-mode) 
           (bound-and-true-p org-supertag-view-mode)
           (string-match-p "\\*Org SuperTag Table View: \\(.*\\)\\*" (buffer-name)))
      (string-match "\\*Org SuperTag Table View: \\(.*\\)\\*" (buffer-name))
      (setq current-tag (match-string 1 (buffer-name))))
      
     ((eq major-mode 'org-supertag-column-mode)
      (when org-supertag-view--current-columns
        (when (= (length (car org-supertag-view--current-columns)) 1)
          (setq current-tag (caar org-supertag-view--current-columns))))))
    
    (quit-window)
    (if current-tag
        (org-supertag-tag-discover (list current-tag))
      (call-interactively 'org-supertag-tag-discover))))

(defun org-supertag-view-switch-to-columns ()
  "Switch to multi-column view from current view."
  (interactive)
  (let ((current-tag nil))
    (cond
     ;; Switch from table view
     ((and (eq major-mode 'org-mode) 
           (bound-and-true-p org-supertag-view-mode)
           (string-match-p "\\*Org SuperTag Table View: \\(.*\\)\\*" (buffer-name)))
      (string-match "\\*Org SuperTag Table View: \\(.*\\)\\*" (buffer-name))
      (setq current-tag (match-string 1 (buffer-name))))
     
     ;; Switch from tag discovery view
     ((eq major-mode 'org-supertag-discover-mode)
      (when (and org-supertag-view--current-filters
                 (= (length org-supertag-view--current-filters) 1))
        (setq current-tag (car org-supertag-view--current-filters)))))
    
    (quit-window)
    (if current-tag
        (org-supertag-tag-columns current-tag)
      (call-interactively 'org-supertag-tag-columns))))

;;----------------------------------------------------------------------
;; Multi-column view functions
;;----------------------------------------------------------------------

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
    (define-key map (kbd "S") 'org-supertag-view-save-current-columns)
    (define-key map (kbd "L") 'org-supertag-view-load-saved-columns)
    (define-key map (kbd "1") 'org-supertag-view-switch-to-tag-only)
    (define-key map (kbd "2") 'org-supertag-view-switch-to-discover)
    (define-key map (kbd "n") 'next-line)  
    (define-key map (kbd "p") 'previous-line)  
    map)
  "Keymap for multi-column tag view mode.")
  
;;;###autoload
(defun org-supertag-view-column (&optional initial-tag)
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

(provide 'org-supertag-view-column)