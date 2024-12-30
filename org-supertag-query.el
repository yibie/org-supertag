;;; org-supertag-query.el --Query for org-supertag -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-supertag-db)  
(require 'org-supertag-field) 

(defun org-supertag-find-matching-tags (keywords)
  "Find tags matching the given keywords.
KEYWORDS is a list of keywords to match against tag names."
  (let ((all-tags (org-supertag-db-find-by-props '(:type :tag)))
        matching-tags)
    (dolist (tag-id (mapcar #'car all-tags))
      (when (cl-some (lambda (keyword)
                      (string-match-p (concat "(?i)" keyword) tag-id))
                    keywords)
        (push tag-id matching-tags)))
    matching-tags))

(defun org-supertag-find-matching-nodes (keywords)
  "Find nodes matching the given keywords.
KEYWORDS is a list of keywords to match against node titles.
Returns a list of matching node IDs."
  (mapcar #'car  ; Return only node IDs
          (org-supertag-db-find-by-props 
           '(:type :node)  ; Find all nodes
           (lambda (props)  ; Check title matches
             (let ((title (plist-get props :title)))
               (and title
                    (cl-some 
                     (lambda (keyword)
                       (string-match-p 
                        (concat "(?i)" (regexp-quote keyword))
                        title))
                     keywords)))))))

(defun org-supertag-find-matching-fields (keywords)
  "Find fields matching the given keywords.
KEYWORDS is a list of keywords to match against field names and values.
Returns a list of (tag-id field-name node-id value) tuples."
  (let (results)
    (dolist (link (org-supertag-db-find-links :node-field nil nil))
      (let ((field-name (plist-get link :field-name))
            (value (plist-get link :value)))
        (when (and field-name value
                  (cl-some 
                   (lambda (keyword)
                     (or (string-match-p (concat "(?i)" keyword) field-name)
                         (string-match-p (concat "(?i)" keyword) value)))
                   keywords))
          (push (list (plist-get link :to)      ; tag-id
                     field-name
                     (plist-get link :from)      ; node-id 
                     value)
                results))))
    (nreverse results)))

(defun org-supertag-query--get-node-tags (node-id)
  "Get all tags for a node for query display.
NODE-ID is the node identifier.

Returns:
- List of tags
- nil if node doesn't exist or has no tags"
  (when-let* ((props (org-supertag-db-get node-id))
              (file (plist-get props :file-path))
              (pos (plist-get props :pos)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)  ; Enable org-mode
        (goto-char pos)
        (org-get-tags)))))

(defun org-supertag-node-has-children-p (node-id)
  "Check if node has child nodes.
NODE-ID is the node identifier."
  (when-let* ((props (org-supertag-db-get node-id))
              (file (plist-get props :file-path))
              (pos (plist-get props :pos)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)  ; Enable org-mode
        (goto-char pos)
        (let ((level (org-current-level)))
          (forward-line)
          (and (re-search-forward org-heading-regexp nil t)
               (> (org-current-level) level)))))))

(defun org-supertag-query-find-by-tag (tag-name)
  "Find nodes by tag name.
TAG-NAME is the tag to search for."
  (let ((nodes '()))
    (maphash
     (lambda (id props)
       (when (and (eq (plist-get props :type) :node)
                 (with-temp-buffer
                   (when-let* ((file (plist-get props :file-path))
                              (pos (plist-get props :pos)))
                     (when (file-exists-p file)
                       (insert-file-contents file)
                       (goto-char pos)
                       (let ((tags (org-get-tags)))
                         (and tags (member tag-name tags)))))))
         (push props nodes)))
     org-supertag-db--object)
    nodes))

(defun org-supertag-query-format-node (node)
  "Format node for display.
NODE is the node property list."
  (let* ((id (plist-get node :id))
         (title (plist-get node :title))
         (file (plist-get node :file-path))  ; Get from node, not props
         (has-children (org-supertag-node-has-children-p id))
         (tags (org-supertag-query--get-node-tags id))
         (formatted-tags (when tags
                          (concat " :" (mapconcat #'identity tags ":") ":")))
         (children-indicator (if has-children " [+]" ""))
         (file-info (format " (%s)" (file-name-nondirectory file))))
    (format "- [ ] %s%s%s%s [[id:%s]]"
            title
            children-indicator
            (or formatted-tags "")
            file-info
            id)))

;;---------------------------------------------------------------
;; Query Results Page
;;---------------------------------------------------------------

(defun org-supertag-query-find-nodes (keywords)
  "Find nodes matching keywords."
  (let (results)
    (maphash
     (lambda (id props)
       (when (eq (plist-get props :type) :node)
         (when-let* ((file (plist-get props :file-path))
                    (pos (plist-get props :pos)))
           (when (file-exists-p file)
             (with-temp-buffer
               (insert-file-contents file)
               (org-mode)  ; Enable org-mode
               (goto-char pos)
               ;; Get all searchable content
               (let* ((title (plist-get props :title))
                      (tags (org-get-tags))
                      (fields (org-entry-properties nil 'standard))
                      (field-values (mapcar #'cdr fields))
                      ;; Combine all searchable text
                      (searchable-text (concat 
                                      title " "
                                      (mapconcat #'identity (or tags '()) " ")
                                      " "
                                      (mapconcat #'identity field-values " "))))
                 ;; Check if all keywords match
                 (when (cl-every 
                        (lambda (keyword)
                          (string-match-p 
                           (regexp-quote keyword) 
                           searchable-text))
                        keywords)
                   (push props results))))))))
     org-supertag-db--object)
    ;; Return results
    (nreverse results)))

(defvar org-supertag-query-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x f") #'org-supertag-query-export-results-to-file)
    (define-key map (kbd "C-c C-x h") #'org-supertag-query-export-results-here)
    (define-key map (kbd "C-c C-x n") #'org-supertag-query-export-results-to-new-file)
    (define-key map (kbd "C-c C-x C-r") #'org-supertag-query-toggle-checkbox-region)
    (define-key map (kbd "C-c C-x C-u") #'org-supertag-query-untoggle-checkbox-region)
    (define-key map (kbd "C-c C-c") #'org-supertag-query-toggle-checkbox)  ; Add checkbox toggle shortcut
    map)
  "Keymap for `org-supertag-query-mode'.")

;; User Query Command
(define-minor-mode org-supertag-query-mode
  "Minor mode for org-supertag search results buffer."
  :lighter " OrgST"
  :keymap org-supertag-query-mode-map)

(defun org-supertag-query-show-results (keyword-list nodes)
  "Display search results.
KEYWORD-LIST is the list of keywords to search for.
NODES is the list of matched nodes to display."
  (with-current-buffer (get-buffer-create "*Org SuperTag Search*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-mode)
      (org-supertag-query-mode)
      
      ;; Display search info
      (insert "#+TITLE: SuperTag Search Results\n\n")
      (insert (format "* Search Terms: %s\n" 
                     (mapconcat #'identity keyword-list " ")))
      (org-show-all)
      
      ;; Display instructions
      (insert "* Instructions\n")
      (insert "- Search scope: titles, tags, properties and field values\n")
      (insert "- Multiple keywords use AND logic (all must match)\n")
      (insert "- [+] indicates node has children\n\n")
      (insert "Shortcuts:\n")
      (insert "- C-c C-c     : Toggle checkbox state\n")
      (insert "- C-c C-x f   : Export to file\n") 
      (insert "- C-c C-x n   : Export to new file\n")
      (insert "- C-c C-x C-r : Toggle checkbox region\n")
      (insert "- C-c C-x C-u : Untoggle checkbox region\n")
      (insert "\n")
      ;; Display search results
      (insert "* Search Results\n")
      (if nodes
          (progn
            (insert (format "Found %d matching nodes:\n\n" (length nodes)))
            (dolist (node nodes)
              (insert (org-supertag-query-format-node node) "\n")))
        (insert "No matching results found\n"))
      (goto-char (point-min))))
  (switch-to-buffer "*Org SuperTag Search*"))

;;---------------------------------------------------------------------
;; Select Serached Item 
;;---------------------------------------------------------------------

(defun org-supertag-query-toggle-checkbox ()
  "Toggle checkbox state of current line."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^- \\(\\[[ X]\\]\\)")
        (let ((current-state (if (string= (match-string 1) "[X]")
                                "[ ]"
                              "[X]")))
          (replace-match (concat "- " current-state)))))))

(defun org-supertag-query-toggle-checkbox-region (start end)
  "Toggle checkbox states in region.
START and END define the region boundaries."
  (interactive "r")  ; Automatically get selected region
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward "^- \\[[ X]\\]" end t))
        (replace-match "- [X]")))))

(defun org-supertag-query-untoggle-checkbox-region (start end)
  "Uncheck all checkboxes in region.
START and END define the region boundaries."
  (interactive "r")
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward "^- \\[X\\]" end t))
        (replace-match "- [ ]")))))

(defun org-supertag-toggle-all-boxes ()
  "Toggle all checkboxes in search results."
  (interactive)
  (let* ((inhibit-read-only t)
         (current-state (save-excursion
                         (goto-char (point-min))
                         (re-search-forward "^- \\[[ X]\\]" nil t)
                         (match-string 0)))
         (new-state (if (string-match-p "X" current-state) " " "X")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^- \\[[ X]\\]" nil t)
        (replace-match (format "- [%s]" new-state))))))

;;----------------------------------------------------------------------
;; Export Selected Results from Query Buffer
;;----------------------------------------------------------------------

(defun org-supertag-get-selected-nodes ()
  "Get IDs of all selected nodes from search results.

Returns:
- List of selected node IDs
- nil if no nodes are selected"
  (let (selected-nodes)
    (save-excursion
      (goto-char (point-min))
      ;; Search for all checked items
      (while (re-search-forward 
              "^-[ \t]+\\[X\\].*?\\[\\[id:\\([^]]+\\)\\]\\]" 
              nil t)
        (when-let ((node-id (match-string-no-properties 1)))
          ;; Verify node exists
          (when (org-supertag-node-db-exists-p node-id)
            (push node-id selected-nodes)))))
    ;; Return results
    (when selected-nodes
      (message "Found %d selected nodes" (length selected-nodes))
      (nreverse selected-nodes))))
      
(defun org-supertag-get-target-level (level-adjust)
  "Calculate target heading level.
LEVEL-ADJUST can be:
  nil        - Keep original level
  :child     - Make child of current heading
  :same-level - Make same level as current heading

Returns:
- Calculated target level
- nil if no adjustment needed"
  (let ((current-level (when (org-at-heading-p)
                        (org-current-level))))
    (cond
     ((eq level-adjust :child)
      (if current-level
          (1+ current-level)
        1))
     ((eq level-adjust :same-level)
      (or current-level 1))
     (t nil))))

(defun org-supertag-adjust-node-level (content target-level)
  "Adjust heading level of node content.
CONTENT is the node content
TARGET-LEVEL is the target heading level

Returns:
- Adjusted content string"
  (with-temp-buffer
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (when (and target-level
               (org-at-heading-p))
      (let* ((source-level (org-current-level))
             (level-diff (- target-level source-level)))
        (unless (zerop level-diff)
          (org-map-entries
           (lambda ()
             (let* ((current-level (org-current-level))
                    (new-level (max 1 (+ current-level level-diff)))
                    (new-stars (make-string new-level ?*)))
               (when (looking-at org-complex-heading-regexp)
                 (replace-match new-stars t t nil 1))))
           t nil))))
    (buffer-string)))

(defun org-supertag-delete-node-content (node-id)
  "Delete node content from source file.
NODE-ID is the node identifier

Returns:
- t if deletion successful
- nil if node not found or deletion failed"
  (when-let* ((node (org-supertag-db-get node-id))
              (source-file (plist-get node :file-path))
              (loc (org-supertag-find-node-location node-id source-file)))
    (with-current-buffer (find-file-noselect source-file)
      (org-with-wide-buffer
       (goto-char (car loc))
       (when (org-at-heading-p)
         (let* ((element (org-element-at-point))
                (begin (org-element-property :begin element))
                (end (org-element-property :end element)))
           (delete-region begin end)
           (when (looking-at "\n") (delete-char 1))
           (save-buffer)
           t))))))

(defun org-supertag-find-node-location (node-id file)
  "Locate node position in file by ID.
NODE-ID is the node identifier
FILE is the file path

Returns:
- (point level olp) if node found
- nil if not found"
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((found nil))
         (org-map-entries
          (lambda ()
            (when (equal (org-id-get) node-id)
              (setq found (list (point)
                              (org-current-level)
                              (org-get-outline-path)))))
          t nil)
         found)))))

(defun org-supertag-get-node-content (node-id)
  "Get complete content of a node.
NODE-ID is the node identifier

Returns:
- Node content string
- nil if node not found"
  (when-let* ((node (org-supertag-db-get node-id))
              (file (plist-get node :file-path))
              (loc (org-supertag-find-node-location node-id file)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (car loc))
       (let ((element (org-element-at-point)))
         (buffer-substring-no-properties
          (org-element-property :begin element)
          (org-element-property :end element)))))))

(defun org-supertag-update-node-db (node-id file)
  "Update node information in database.
NODE-ID is the node identifier
FILE is the target file path"
  (unless (org-supertag-ensure-org-file file)
    (error "Invalid org file: %s" file))
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      ;; Ensure at correct position
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      ;; Verify correct node found
      (let ((current-id (org-id-get)))
        (unless (equal current-id node-id)
          (error "Current heading ID (%s) doesn't match expected ID (%s)"
                 current-id node-id)))
      ;; Collect node info
      (let* ((pos (point))
             (level (org-current-level))
             (title (org-get-heading t t t t))
             (olp (org-get-outline-path))
             ;; Get existing node info to preserve properties
             (existing-node (org-supertag-db-get node-id))
             (created-at (and existing-node 
                             (plist-get existing-node :created-at))))
        ;; Build new property list
        (let ((new-props
               (list :type :node
                     :title title
                     :file-path file
                     :pos pos
                     :level level
                     :olp olp)))
          ;; Preserve creation time if exists
          (when created-at
            (setq new-props (plist-put new-props :created-at created-at)))
          ;; Update database
          (condition-case err
              (progn
                (org-supertag-node-db-update node-id new-props)
                (message "Updated node %s in database" node-id))
            (error
             (message "Failed to update node %s: %s" 
                      node-id 
                      (error-message-string err))
             (signal (car err) (cdr err)))))))))

(defun org-supertag-move-node (node-id target-file &optional target-level)
  "Move node to target file.
NODE-ID is the node identifier
TARGET-FILE is the target file path
TARGET-LEVEL is the target heading level

Returns:
- t if move successful
- nil if move failed"
  (org-supertag-node-move node-id target-file target-level))

(defun org-supertag-insert-nodes (node-ids &optional level-adjust)
  "Insert nodes at current position.
NODE-IDS is list of node identifiers
LEVEL-ADJUST is level adjustment option"
  (let ((target-file (buffer-file-name))
        (target-level (org-supertag-get-target-level level-adjust))
        (success-count 0))
    (unless target-file
      (error "Current buffer is not visiting a file"))
    
    ;; 添加目标位置信息
    (message "Target file: %s, level: %s, point: %s" 
             target-file target-level (point))
    
    (dolist (node-id node-ids)
      (message "\n=== Processing node: %s ===" node-id)
      ;; 获取源文件信息
      (when-let ((source-pos (org-supertag-db-get-pos node-id)))
        (message "Source position: %s" source-pos)
        (condition-case err
            (progn
              ;; 验证节点
              (unless (org-supertag-behavior--validate-node node-id)
                (error "Invalid node: %s" node-id))
              ;; 尝试移动
              (when (org-supertag-move-node node-id target-file target-level)
                (cl-incf success-count)
                (message "Successfully moved node %s" node-id)))
          (error
           (message "Failed processing node %s: %s" 
                    node-id 
                    (error-message-string err))))))
    
    (message "Processing complete: %d/%d nodes moved successfully" 
             success-count 
             (length node-ids))))

;; Export result to...
(defun org-supertag-ensure-org-file (file)
  "Ensure file is valid org file.
FILE is the file path

Returns:
- t if valid org file
- nil if not"
  (and file
       (string-match-p "\\.org$" file)
       (or (file-exists-p file)
           (and (not (file-exists-p file))
                (string-match-p "\\.org$" file)))))

(defun org-supertag-query-export-results-to-new-file ()
  "Export selected search results to new file.
Export selected nodes to a standalone org file."
  (interactive)
  (let ((selected-nodes (org-supertag-get-selected-nodes)))
    (message "Selected nodes: %S" selected-nodes)
    (if (not selected-nodes)
        (message "No items selected")
      ;; Select target file
      (let* ((default-name "export.org")
             (file (read-file-name 
                   "Export to new file: " 
                   nil nil nil 
                   default-name)))
        ;; Validate file type
        (unless (org-supertag-ensure-org-file file)
          (error "Export target must be an org file: %s" file))
        ;; Process target file
        (condition-case err
            (progn
              (when (and (file-exists-p file)
                        (not (y-or-n-p 
                              (format "File %s exists. Overwrite? " file))))
                (error "Export cancelled by user"))
              
              (with-current-buffer (find-file-noselect file)
                (erase-buffer)  ; Clear new file
                (org-mode)
                ;; Insert file header
                (let ((title (file-name-base file)))
                  (insert (format "#+TITLE: %s\n" title)
                          "#+OPTIONS: ^:nil\n"  ; Disable superscript
                          "#+STARTUP: showeverything\n\n"))  ; Show all content
                (message "Inserting nodes...")
                (org-supertag-insert-nodes selected-nodes nil)
                (save-buffer)
                ;; Display new file
                (find-file file)
                (other-window 1)
                (balance-windows)
                (message "Export completed successfully to %s" file)))
          ;; Error handling
          (error
           (message "Export failed: %s" (error-message-string err))
           (signal (car err) (cdr err))))))))

(defun org-supertag-query-export-results-to-file ()
  "Export selected search results to specified file location.

Available insertion positions:
1. File End - Insert at end of file
2. Under Heading - Insert as child of selected heading 
3. Same Level - Insert as sibling of selected heading"
  (interactive)
  (let* ((selected-nodes (org-supertag-get-selected-nodes)))
    (if (not selected-nodes)
        (message "No nodes selected")
      ;; Select target file
      (condition-case err
          (let* ((target-file (read-file-name "Export to file: "))
                 (insert-type (completing-read 
                             "Insert at: "
                             '("File End" 
                               "Under Heading"
                               "Same Level"))))
            
            ;; Validate file type
            (unless (org-supertag-ensure-org-file target-file)
              (error "Export target must be an org file: %s" target-file))
            
            ;; Process target file
            (with-current-buffer (find-file-noselect target-file)
              (org-mode)
              (let (target-point level-adjust)
                ;; Determine insertion point and level adjustment based on option
                (pcase insert-type
                  ("File End"
                   (setq target-point (point-max)
                         level-adjust nil))
                  ("Under Heading"
                   (let* ((headlines (org-map-entries 
                                    (lambda () 
                                      (cons (org-get-heading t t t t)
                                           (point)))))
                          (selected (if headlines
                                      (completing-read 
                                       "Select parent heading: "
                                       headlines)
                                    (error "No headlines found in target file"))))
                     (setq target-point (cdr (assoc selected headlines))
                           level-adjust :child)))
                  
                  ("Same Level"
                   (let* ((headlines (org-map-entries 
                                    (lambda () 
                                      (cons (org-get-heading t t t t)
                                           (point)))))
                          (selected (if headlines
                                      (completing-read 
                                       "Select sibling heading: "
                                       headlines)
                                    (error "No headlines found in target file"))))
                     (setq target-point (cdr (assoc selected headlines))
                           level-adjust :same-level))))
                
                ;; Insert content
                (goto-char (or target-point (point-max)))
                (org-supertag-insert-nodes selected-nodes level-adjust)
                (save-buffer)
                
                ;; Display target file
                (find-file target-file)
                (message "Export completed successfully"))))
        ;; Error handling
        (error
         (message "Export failed: %s" (error-message-string err))
         (signal (car err) (cdr err)))))))

;;; Save org-supertag-query result at cursor place
(defvar org-supertag-query--original-buffer nil
  "Store the original buffer where search was initiated.")

(defvar org-supertag-query--original-point nil
  "Store cursor position when search was initiated.")

(defun org-supertag-query-export-results-here ()
  "Insert search results at cursor position."
  (interactive)
  (let* ((input (read-string "Enter search keywords (space separated): "))
         (keywords (split-string input " " t))
         (block-name (string-join keywords "_"))  ; Use keywords as block name
         (matched-nodes nil))
    ;; 1. Find matching nodes
    (setq matched-nodes (org-supertag-query-find-nodes keywords))
    ;; 2. If there are matching nodes, continue processing
    (if matched-nodes
        (let* ((node-choices 
                (mapcar (lambda (node)
                         (let* ((title (plist-get node :title))
                                (id (plist-get node :id))
                                (file-path (plist-get node :file-path))
                                ;; Clean title text
                                (clean-title 
                                 (substring-no-properties 
                                  (if (stringp title)
                                      title
                                    (prin1-to-string title))))
                                ;; Get file name (without path)
                                (file-name 
                                 (file-name-nondirectory file-path))
                                ;; Format display options
                                (display-text 
                                 (format "%-30s (%s)"  ; Left align, fixed width
                                        (truncate-string-to-width 
                                         clean-title 30 nil nil "...")
                                        file-name)))
                           (cons display-text id)))
                       matched-nodes))
               (selected-nodes
                (completing-read-multiple 
                 "Select nodes (TAB:complete, RET:confirm, ,:multi): "
                 (mapcar #'car node-choices)
                 nil t)))
          ;; 3. Generate link content
          (let ((content
                 (with-temp-buffer
                   (insert (format "#+begin_%s\n" block-name))
                   (dolist (selection selected-nodes)
                     (let* ((node-id (cdr (assoc selection node-choices)))
                            (node (org-supertag-db-get node-id))
                            (title (plist-get node :title))
                            (clean-title 
                             (substring-no-properties 
                              (if (stringp title)
                                  title
                                (prin1-to-string title)))))
                       (insert (format "- [[id:%s][%s]]\n"
                                     node-id
                                     clean-title))))
                   (insert (format "#+end_%s\n" block-name))
                   (buffer-string))))
            (save-excursion
              (insert content)
              (message "Inserted %d node links" 
                       (length selected-nodes)))))
      (message "No matching nodes found for keywords: %s" 
               (string-join keywords " ")))))

;;------------------------------------------------------
;; User Interactive Command
;;------------------------------------------------------

(defun org-supertag-query ()
  "Interactive search across all indexed files."
  (interactive)
  ;; Save current position
  (setq org-supertag-query--original-buffer (current-buffer)
        org-supertag-query--original-point (point))
  ;; Ensure database is initialized
  (unless (and (boundp 'org-supertag-db--object)
               org-supertag-db--object)
    (error "Database not initialized"))
  
  (let* ((input (read-string "Enter search keywords (space separated): "))
         (keywords (split-string input " " t))
         (nodes (org-supertag-query-find-nodes keywords)))
    (org-supertag-query-show-results keywords nodes)))

(defun org-supertag-query--find-nodes-in-buffer (keywords)
  "Find nodes matching KEYWORDS in current buffer.
KEYWORDS is a list of keywords to match against node content."
  (let (results)
    (save-excursion
      (goto-char (point-min))
      (while (outline-next-heading)
        (when-let* ((node-id (org-id-get))
                    (props (org-supertag-db-get node-id)))
          ;; Get all searchable content
          (let* ((title (org-get-heading t t t t))
                 (tags (org-get-tags))
                 (fields (org-entry-properties nil 'standard))
                 (field-values (mapcar #'cdr fields))
                 ;; Combine all searchable text
                 (searchable-text (concat 
                                 title " "
                                 (mapconcat #'identity (or tags '()) " ")
                                 " "
                                 (mapconcat #'identity field-values " "))))
            ;; Check if all keywords match
            (when (cl-every 
                   (lambda (keyword)
                     (string-match-p 
                      (regexp-quote keyword) 
                      searchable-text))
                   keywords)
              (push props results))))))
    ;; Return results
    (nreverse results)))

(defun org-supertag-query-buffer ()
  "Search for nodes in current buffer.
Shows results in a dedicated buffer with selection and export options."
  (interactive)
  ;; Save current position
  (setq org-supertag-query--original-buffer (current-buffer)
        org-supertag-query--original-point (point))
  
  ;; Ensure database is initialized
  (unless (and (boundp 'org-supertag-db--object)
               org-supertag-db--object)
    (error "Database not initialized"))
  
  (let* ((input (read-string "Enter search keywords (space separated): "))
         (keywords (split-string input " " t))
         (nodes (org-supertag-query--find-nodes-in-buffer keywords)))
    (org-supertag-query-show-results keywords nodes)))

;;----------------------------------------------------------------------
;;  Query nodes in selected files 
;;----------------------------------------------------------------------

(defun org-supertag-query--get-org-files ()
  "Get list of org files in database.
Returns a list of absolute file paths."
  (let* ((node-ids (org-supertag-db-find-by-type :node))
         (nodes (mapcar #'org-supertag-db-get node-ids)))  ; 获取完整的节点数据
    (message "Debug: Found nodes: %S" nodes)  ; 调试信息
    (let ((files (delete-dups
                  (mapcar (lambda (props)
                           (plist-get props :file-path))
                         nodes))))
      (message "Debug: Extracted files: %S" files)  ; 调试信息
      files)))

(defun org-supertag-query--select-files ()
  "Select files for search scope.
Returns:
- List of file paths
- nil if no files selected"
  (let* ((choices '(("Single file" . :single)
                   ("Multiple files" . :multiple)
                   ("Agenda files" . :agenda)))
         (scope (cdr (assoc 
                     (completing-read "Select scope: " choices nil t)
                     choices)))
         (files (org-supertag-query--get-org-files)))
    
    (unless files
      (user-error "No files found in database"))
    (pcase scope
      (:single 
       (when-let ((file (completing-read 
                        "Select file: " 
                        (lambda (string pred action)
                          (if (eq action 'metadata)
                              '(metadata (display-sort-function . identity)
                                       (cycle-sort-function . identity))
                            (complete-with-action 
                             action files string pred)))
                        nil t)))
         (list file)))
      (:multiple 
       (completing-read-multiple 
        "Select files (TAB:complete, RET:confirm, ,:multi): "
        (lambda (string pred action)
          (if (eq action 'metadata)
              '(metadata (display-sort-function . identity)
                       (cycle-sort-function . identity))
            (complete-with-action 
             action files string pred)))
        nil t))
      (:agenda 
       (unless org-agenda-files
         (user-error "No agenda files defined"))
       org-agenda-files))))

(defun org-supertag-query--find-nodes-in-files (keywords files)
  "Find nodes matching KEYWORDS in FILES.
KEYWORDS is a list of keywords to match
FILES is a list of file paths to search

Returns:
- List of matched nodes
- nil if no matches found"
  (let (results)
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (let ((nodes (org-supertag-query--find-nodes-in-buffer keywords)))
            (when nodes
              (push nodes results))))))
    (apply #'append (nreverse results))))

(defun org-supertag-query-in-files ()
  "Search for nodes in selected files.
Shows results in a dedicated buffer with selection and export options."
  (interactive)
  ;; Save current position
  (setq org-supertag-query--original-buffer (current-buffer)
        org-supertag-query--original-point (point))
  
  ;; Ensure database is initialized
  (unless (and (boundp 'org-supertag-db--object)
               org-supertag-db--object)
    (error "Database not initialized"))
  
  ;; Select files
  (when-let* ((files (org-supertag-query--select-files)))
    ;; Get search keywords
    (let* ((input (read-string "Enter search keywords (space separated): "))
           (keywords (split-string input " " t))
           (nodes (org-supertag-query--find-nodes-in-files keywords files)))
      
      ;; Display results
      (org-supertag-query-show-results keywords nodes))))

(provide 'org-supertag-query)
