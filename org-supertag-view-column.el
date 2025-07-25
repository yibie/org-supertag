;;; org-supertag-view-column.el --- Multi-column tag comparison view for org-supertag -*- lexical-binding: t -*-

(require 'org-supertag-view-utils)

;;----------------------------------------------------------------------
;; Multi-column Tag Comparison View
;;----------------------------------------------------------------------

(defvar org-supertag-view--current-columns nil
  "List of tag combinations currently displayed in columns.")

(defun org-supertag-view--show-tag-columns (&optional initial-tag)
  "Show multi-column tag comparison view, optionally starting with INITIAL-TAG."
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
               (cooccurring-tags (org-supertag-view--get-cooccurring-tags current-tags)))

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
            (message "No co-occurring tags found for the selected column"))))))

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
               ;; 只依赖共现关系
               (cooccurring-tags (org-supertag-view--get-cooccurring-tags current-tags))
               (all-related
                (mapcar (lambda (tag-pair)
                         (format "%s (co-occurs %d times)"
                                (car tag-pair) (cdr tag-pair)))
                       cooccurring-tags)))
          (if all-related
              (let* ((selected (completing-read "Choose related tag for new column: " 
                                             all-related nil t))
                     ;; Extract actual tag name from the formatted string
                     (selected-tag (replace-regexp-in-string " \(.*\)" "" selected)))
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


(defun org-supertag-view-node-at-point ()
  "Jump to the source of the node under the cursor."
  (interactive)
  (when-let ((node-id (get-text-property (point) 'node-id)))
    (org-supertag-view--goto-node node-id)))

(defun org-supertag-view-column--move-horizontally (direction)
  "Move cursor horizontally by one column in the column view.
DIRECTION should be 1 for right, -1 for left."
  (org-supertag-view-util-move-horizontally-in-columns
   direction
   40 ; column-width
   2  ; separator-width
   (length org-supertag-view--current-columns)))

(defun org-supertag-view-column-next-column ()
  "Move to the next column to the right."
  (interactive)
  (org-supertag-view-column--move-horizontally 1))

(defun org-supertag-view-column-previous-column ()
  "Move to the previous column to the left."
  (interactive)
  (org-supertag-view-column--move-horizontally -1))

(defun org-supertag-view--refresh-column-view ()
  "Update the multi-column tag comparison view with a clean, aligned layout."
  (interactive)
  (let ((buffer (get-buffer-create "*Org SuperTag Columns*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (column-width 40)
            (separator "  "))
        (erase-buffer)
        (org-supertag-column-mode)
        ;; Simplified header
        (insert (propertize "Multi-Column Tag View\n\n" 'face '(:height 1.5 :weight bold)))
        (insert " [a/A] Add Column (A: related)  [d] Remove  [t] Add Tag to Column\n")
        (insert " [n/p] Cursor Up/Down [TAB/S-TAB] Column Jump [v] View Node    [g] Refresh     [q] Quit\n\n")

        ;; Data Fetching
        (let* ((all-nodes-lists (mapcar (lambda (tags)
                                         (org-supertag-view--get-nodes-with-tags tags))
                                       org-supertag-view--current-columns))
               (col-count (length all-nodes-lists)))

          ;; 1. Insert column headers
          (dotimes (i col-count)
            (let* ((tags (nth i org-supertag-view--current-columns))
                   (tag-text (string-join tags ", "))
                   (node-count (length (nth i all-nodes-lists)))
                   (header-text (format "%s (%d)" tag-text node-count))
                   (padded-header (org-supertag-view-util-pad-string header-text column-width 'center)))
              (insert (propertize padded-header 'face '(:weight bold)))
              (when (< i (1- col-count)) (insert separator))))
          (insert "\n")
          (dotimes (i col-count)
            (insert (make-string column-width ?─))
            (when (< i (1- col-count)) (insert separator)))
          (insert "\n")

          ;; 2. Pre-render each column into a list of its lines.
          (let* ((rendered-columns
                  (mapcar
                   (lambda (nodes-list)
                     (let ((lines '()))
                       (dolist (node-id nodes-list)
                         (let* ((title (or (plist-get (gethash node-id org-supertag-db--object) :title) "No Title"))
                                (wrapped-title (org-supertag-view-util-wrap-text title column-width)))
                           (dolist (line wrapped-title)
                             (push (propertize line 'node-id node-id) lines)))
                         (push "" lines))) ; Blank line separator
                       (if (not lines) '() (butlast (nreverse lines)))))
                   all-nodes-lists))
                 (max-height (apply #'max 0 (mapcar #'length rendered-columns))))

            (if (zerop max-height)
                (insert "\n  No nodes found for any combination of tags.\n")
              ;; 3. Print the board row by row, ensuring alignment.
              (dotimes (line-idx max-height)
                (dotimes (col-idx col-count)
                  (let* ((col-lines (nth col-idx rendered-columns))
                         (line-to-insert (or (nth line-idx col-lines) "")))
                    (insert (org-supertag-view-util-pad-string line-to-insert column-width :left)))
                  (when (< col-idx (1- col-count)) (insert separator)))
                (insert "\n"))))))
        (goto-char (point-min))
        (pop-to-buffer buffer)))

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
                   (forward-char 1))))
          (unless found
            (org-supertag-view--view-node-from-columns))))
      (unless found
        (org-supertag-view--view-node-from-columns))))
   ((eq major-mode 'org-supertag-discover-mode)
    (org-supertag-view--view-node))
   (t
    (message "Please move cursor to a node to view it.")))

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
           (string-match-p "\\le *Org SuperTag TabView: \\(.*\\)\\*" (buffer-name)))
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
    (define-key map (kbd "g") 'org-supertag-view--refresh-column-view)
    (define-key map (kbd "a") 'org-supertag-view-add-column)
    (define-key map (kbd "A") 'org-supertag-view-add-related-column)
    (define-key map (kbd "t") 'org-supertag-view-add-tag-to-column)
    (define-key map (kbd "d") 'org-supertag-view-remove-column)
    (define-key map (kbd "R") 'org-supertag-view-reset-columns)
    (define-key map (kbd "v") 'org-supertag-view-node-at-point)
    (define-key map (kbd "S") 'org-supertag-view-save-current-columns)
    (define-key map (kbd "L") 'org-supertag-view-load-saved-columns)
    (define-key map (kbd "1") 'org-supertag-view-switch-to-tag-only)
    (define-key map (kbd "2") 'org-supertag-view-switch-to-discover)
    (define-key map (kbd "TAB") 'org-supertag-view-column-next-column)
    (define-key map (kbd "<backtab>") 'org-supertag-view-column-previous-column)
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
  (org-supertag-view--show-tag-columns initial-tag))

(provide 'org-supertag-view-column)