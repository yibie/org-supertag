;;; org-supertag-view-table.el --- Table view for org-supertag -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org-supertag-view-utils)

;;----------------------------------------------------------------------
;; Helpful functions
;;----------------------------------------------------------------------

(defun org-supertag-view-table-get-field-info ()
  "Get field information of the current table cell.
Returns a property list containing the following information:
- :col - Current column number
- :row-data - Current row data
- :tag - Current tag
- :tag-def - Tag definition
- :fields - List of field definitions
- :field-idx - Field index (relative to the field list)
- :field - Current field definition
- :value - Value of the current cell"
  (when (org-at-table-p)
    (let* ((col (org-table-current-column))
           (row-data (org-supertag-view-table-get-line))
           (tag (org-supertag-view--get-current-tag))
           (tag-def (org-supertag-tag-get tag))
           (fields (plist-get tag-def :fields))
           (field-idx (- col 4))  ;; Adjust column index calculation, now there are three fixed columns: Node, Type, and Date
           (field (when (and (>= field-idx 0) (< field-idx (length fields)))
                    (nth field-idx fields)))
           (value (org-table-get nil col)))
      (list :col col
            :row-data row-data
            :tag tag
            :tag-def tag-def
            :fields fields
            :field-idx field-idx
            :field field
            :value value))))

(defun org-supertag-view-table-get-line ()
  "Get all fields in the current table row as a list.
Returns a list where each element is the content of a cell in the current row."
  (save-excursion
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)))
          (fields '()))
      ;; Ensure we are on a table row
      (when (string-match "^[ \t]*|" line)
        ;; Remove leading and trailing |
        (setq line (replace-regexp-in-string "^[ \t]*|" "" line))
        (setq line (replace-regexp-in-string "|[ \t]*$" "" line))
        ;; Split fields and remove leading/trailing whitespace
        (setq fields (mapcar #'string-trim (split-string line "|"))))
      fields)))

(defun org-supertag-view--find-node-by-title (title)
  "Find node ID by title.
TITLE is the title of the node.
Returns the found node ID, or nil if not found.
Performs exact and fuzzy matching, prioritizing exact matches."
  (when (and title (not (string-empty-p title)))
    (let ((cleaned-title (string-trim title))
          (exact-match nil)
          (fuzzy-matches '()))
      
      ;; Normalize title (remove extra spaces)
      (setq cleaned-title (replace-regexp-in-string "\\s-+" " " cleaned-title))
      
      ;; Search all nodes in the database
      (maphash
       (lambda (id props)
         (when (eq (plist-get props :type) :node)
           (let ((node-title (plist-get props :title))
                 (node-org-id (plist-get props :id)))
             (when node-title
               ;; Normalize node title
               (setq node-title (replace-regexp-in-string "\\s-+" " " (string-trim node-title)))
               
               ;; Check for exact match
               (if (string= cleaned-title node-title)
                   (setq exact-match id)
                 ;; Check for fuzzy match (containment relationship)
                 (when (or (string-match-p (regexp-quote cleaned-title) node-title)
                           (string-match-p (regexp-quote node-title) cleaned-title))
                   (push (cons id (length node-title)) fuzzy-matches)))))))
       org-supertag-db--object)
      
      ;; Prioritize exact match
      (or exact-match
          ;; Otherwise, return the closest fuzzy match (choose the one with the closest title length)
          (when fuzzy-matches
            (caar (sort fuzzy-matches
                        (lambda (a b)
                          (< (abs (- (cdr a) (length cleaned-title)))
                             (abs (- (cdr b) (length cleaned-title))))))))))))

(defun org-supertag-view--find-node-by-id (file-path org-id)
  "Locate the node in the file by ID.
FILE-PATH is the file path, ORG-ID is the ID of the node.
Returns t if the node is successfully found."
  (when (and file-path (file-exists-p file-path) org-id)
    (find-file file-path)
    (widen)
    ;; Disable org-element-cache to avoid parsing errors
    (when (boundp 'org-element-use-cache)
      (setq-local org-element-use-cache nil))
    
    (goto-char (point-min))
    ;; Locate node by ID property
    (when (re-search-forward (format ":ID:\\s-*%s\\s-*$"
                                    (regexp-quote org-id))
                            nil t)
      (org-back-to-heading t)
      (org-fold-show-entry)
      (org-fold-show-children)
      (recenter 1)
      t)))

(defun org-supertag-view-table-node-at-point ()
  "View the node at point in the SuperTag table view.
Strategy:
1. Get node title from the table row
2. Find node ID and file location in the database by title
3. Locate the node by ID"
  (interactive)
  (when (org-at-table-p)
    (let* ((row-data (org-supertag-view-table-get-line))
           (node-text (nth 0 row-data))
           ;; Clean up node title (remove [v] and [N/A] markers)
           (node-title (when node-text
                        (string-trim
                         (replace-regexp-in-string "\\[\\(v\\|N/A\\)\\]\\s-*" ""
                                                 node-text))))
           (node-id (org-supertag-view--find-node-by-title node-title)))
      
      (if node-id
          (let* ((node-props (gethash node-id org-supertag-db--object))
                 (file-path (plist-get node-props :file-path))
                 (org-id (plist-get node-props :id)))
            (if (org-supertag-view--find-node-by-id file-path org-id)
                (message "Successfully located node: %s" node-title)
              (message "Could not find node in file: %s" node-title)))
        (message "Node not found: %s" node-title)))))

;;----------------------------------------------------------------------
;; Single Tag View Mode (Table View)
;;----------------------------------------------------------------------

(defun org-supertag-view--show-content-table (tag)
  "Show content table for TAG in a dedicated full-screen buffer."
  (let ((buffer (get-buffer-create (format "*Org SuperTag Table View: %s*" tag))))
    (with-current-buffer buffer
      (org-mode)
      (org-supertag-view-table-mode)
      (org-supertag-view-mode 1) ; Enable the minor mode for correct dispatching
      (setq-local org-supertag-view-current-tag tag)

      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Use plain text instead of org heading
        (insert (format "Tag: #%s\n\n" tag))
        (insert "Instructions:\n")
        (insert " [q] - Quit    [g] - Refresh    [v] - View Node    [m] - Manage Relations\n")
        (insert " [e] - Smart Edit    [C-c '] - Toggle Edit/Read-only Mode\n")
        (insert " [Tab] - Next Field    [S-Tab] - Previous Field    [n/p] - Move Up/Down\n")
        (insert " Click the [v] button before a node to directly view its content\n")
        (insert " Editing fields will automatically save changes\n\n")
        ;; Insert table content
        (org-supertag-view--insert-content-table tag))

      (setq buffer-read-only t)
      (goto-char (point-min)))

    ;; Use full-screen display instead of sidebar window
    (switch-to-buffer buffer)
    (delete-other-windows)))

(defun org-supertag-view--insert-content-table (tag)
  "Insert content related to TAG in current buffer using org table format with buttons.
Add a [v] button before each node in each row, clicking it will directly view the node content."
  (insert "Related Nodes:\n\n")
  (let* ((content (org-supertag-view--get-related-nodes tag))
         (tag-def (org-supertag-tag-get tag))
         (fields (plist-get tag-def :fields)))
    
    (if (not content)
        (insert (format "No content found related to tag #%s" tag))
      ;; Insert table header
      (insert "|Node|Type|Date")
      (dolist (field fields)
        (insert (format "|%s" (plist-get field :name))))
      (insert "|\n")
      
      ;; Insert separator line
      (insert "|------|------|----")
      (dolist (_ fields) (insert "|-----"))
      (insert "|\n")
      
      ;; Insert data rows
      (dolist (item content)
        (let* ((node-id (plist-get item :id))
               (node-props (gethash node-id org-supertag-db--object))
               (node-title (plist-get item :node))
               (org-id (plist-get node-props :id))
               (file-path (plist-get node-props :file-path)))
          
          ;; Insert node column (with button)
          (insert "|")
          (if (and file-path org-id)
              (progn
                (insert-text-button "[v]"
                                  'face 'link
                                  'follow-link t
                                  'file-path file-path
                                  'org-id org-id
                                  'node-title node-title
                                  'action (lambda (button)
                                          (let ((file (button-get button 'file-path))
                                                (org-id (button-get button 'org-id))
                                                (title (button-get button 'node-title)))
                                            (when (org-supertag-view--find-node-by-id file org-id)
                                              (message "Successfully opened node: %s" title)))))
                (insert " "))
            (insert "[N/A] "))
          
          ;; Insert basic information
          (insert (format "%s|%s|%s"
                         node-title
                         (or (plist-get item :type) "")
                         (plist-get item :date)))
          
          ;; Insert field values
          (dolist (field fields)
            (let* ((field-name (plist-get field :name))
                   (value (org-supertag-field-get-value node-id field-name tag)))
              (insert (format "|%s" (or value "")))))
          (insert "|\n")))
      
      ;; Align table
      (save-excursion
        (backward-char)
        (org-table-align)))))

(defun org-supertag-view--get-field-index (col)
  "Get field index based on column position COL."
  (let ((pos 0)
        (current 0))
    (while (< current col)
      (setq current (+ current (if (= pos 0) 20 15)))
      (setq pos (1+ pos)))
    pos))

(defun org-supertag-view--get-current-tag ()
  "Get current tag from buffer name or buffer local variable."
  (if (boundp 'org-supertag-view-current-tag)
      ;; First try to get from local variable
      org-supertag-view-current-tag
    ;; Then try to extract from buffer name
    (when (string-match "\\*Org SuperTag Table View: \\([^*]+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name)))))

(defun org-supertag-view-table-get-all-rows ()
  "Get all data rows in the table.
Returns a list, each element is a list of row data."
  (let ((rows '()))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "|Node|Type|Date" nil t)
        (forward-line 2) ;; Skip header and separator line
        
        ;; Collect each row
        (while (org-at-table-p)
          (let ((row-data (org-supertag-view-table-get-line)))
            (when row-data
              (push row-data rows)))
          (forward-line 1))))
    (nreverse rows)))

(defun org-supertag-view-table-find-node-id (node-title)
  "Find node ID based on node title.
NODE-TITLE is the title of the node.
Returns the found node ID, or nil if not found."
  (when (and node-title (not (string-empty-p node-title)))
    ;; Clean up title (remove button text and whitespace)
    (let ((clean-title node-title))
      (setq clean-title (replace-regexp-in-string "\\[v\\]\\s-*" "" clean-title))
      (setq clean-title (replace-regexp-in-string "\\[N/A\\]\\s-*" "" clean-title))
      (setq clean-title (string-trim clean-title))
      
      ;; Use the common node finding function
      (org-supertag-view--find-node-by-title clean-title))))

(defun org-supertag-view-table-update-field (node-id field-name field-value tag-id)
  "Update field value in database.
NODE-ID is the node identifier
FIELD-NAME is the field name
FIELD-VALUE is the new value
TAG-ID is the tag identifier
Returns t if update successful, nil if failed."
  (condition-case err
      (progn
        ;; 1. Update database by calling the centralized field function
        (org-supertag-field-set-value node-id field-name field-value tag-id)

        ;; The file update part is removed to decouple from org-properties.
        ;; The database is now the single source of truth.

        ;; 2. Mark database as dirty and schedule save
        ;; This is now handled by org-supertag-field-set-value,
        ;; so it can be removed from here.
        t)  ;; Return success
    (error
     (message "Error updating field %s: %s" field-name (error-message-string err))
     nil)))

(defun org-supertag-view-cancel-edit ()
  "Cancel editing and restore original view."
  (interactive)
  (when (yes-or-no-p "Cancel editing? Changes will be lost.")
    (let ((tag (org-supertag-view--get-current-tag)))
      (quit-window)
      (org-supertag-view--show-content-table tag))))

(defun org-supertag-view--get-related-nodes (tag)
  "Get nodes related to TAG.
Returns a list of plists with properties :node, :type, :date and field values."
  (let* ((nodes '())
         (tag-def (org-supertag-tag-get tag))
         (fields (plist-get tag-def :fields)))
    (maphash
     (lambda (link-id link-props)
       ;; First find all nodes related to this tag
       (when (and (string-match ":node-tag:\\(.+\\)->\\(.+\\)$" link-id)
                  (equal (match-string 2 link-id) tag))
         (when-let* ((node-id (plist-get link-props :from))
                    (node-props (gethash node-id org-supertag-db--object)))
           ;; For each node, get the values of all its fields
           (let ((field-values
                  (mapcar
                   (lambda (field)
                     (let* ((field-name (plist-get field :name))
                            ;; Use the centralized function to get the value
                            (value (org-supertag-field-get-value node-id field-name tag)))
                       (cons field-name value)))
                   fields)))
             
             ;; Build node information
             (push (list :node (or (plist-get node-props :title)
                                  (format "Node %s" node-id))
                        :type (or (plist-get node-props :todo-state) "Node")
                        :date (format-time-string
                              "%Y-%m-%d"
                              (or (plist-get node-props :created-at)
                                  (current-time)))
                        :id node-id
                        :fields field-values)
                   nodes)))))
     org-supertag-db--link)
    (nreverse nodes)))

(defun org-supertag-view-smart-edit ()
  "Smart edit function - calls the centralized field editor.
Immediately saves the field value to the database after editing."
  (interactive)
  (let* ((field-info (org-supertag-view-table-get-field-info))
         (field-def (plist-get field-info :field))
         (current-value (plist-get field-info :value))
         (tag (plist-get field-info :tag))
         (node-title (nth 0 (plist-get field-info :row-data)))
         (node-id (org-supertag-view--find-node-by-title node-title))
         (field-name (plist-get field-def :name)))

    (unless node-id
      (error "Could not find node with title: %s" node-title))

    (unless field-def
      (error "Cannot edit this column. Not a user-defined field."))

    (let ((new-value (org-supertag-field-read-and-validate-value field-def current-value)))
      (when new-value
        ;; Save the new value to the database
        (org-supertag-field-set-value node-id field-name new-value tag)

        ;; Update the value in the table view
        (org-table-edit-field (format "%s" new-value))
        (org-table-align)

        (message "Field updated: %s" new-value)
        (org-supertag-view-table-refresh)))))

;;----------------------------------------------------------------------
;; Mode definitions
;;----------------------------------------------------------------------

(defun org-supertag-view-table-refresh ()
  "Refresh the table view by re-generating its content."
  (interactive)
  (let ((tag (org-supertag-view--get-current-tag)))
    (when tag
      (org-supertag-view--show-content-table tag))))

(define-derived-mode org-supertag-view-table-mode org-mode "SuperTag-Table"
  "Major mode for displaying tag content in table format.
This mode is based on org-mode to ensure compatibility with org table functions.
\\{org-supertag-view-table-mode-map}"
  :group 'org-supertag
  (setq-local org-element-use-cache nil)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq header-line-format
        (propertize " Org-Supertag Table View" 'face '(:weight bold)))
  (let ((map (make-sparse-keymap)))
    ;; Define the most critical editing keys first to ensure they have the highest priority
    (define-key map (kbd "e") (lambda ()
                               (interactive)
                               (let ((inhibit-read-only t))
                                 (call-interactively 'org-supertag-view-smart-edit))))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-view-table-refresh)
    (define-key map (kbd "v") 'org-supertag-view-table-node-at-point)
    (define-key map (kbd "V") 'org-supertag-view-table-view-all-nodes)
    (define-key map (kbd "m") 'org-supertag-view-manage-relations)
    (dolist (key '("d" "o" "r" "l" "a" "t"))
      (define-key map (kbd key)
                 (lambda ()
                   (interactive)
                   (let ((inhibit-read-only t))
                     (call-interactively
                      (intern (format "org-supertag-view-edit-%s-field"
                                     (pcase key
                                       ("d" "date")
                                       ("o" "options")
                                       ("r" "reference")
                                       ("l" "list")
                                       ("a" "range")
                                       ("t" "timestamp")))))))))
    (define-key map (kbd "C-c C-k") 'org-supertag-view-cancel-edit)
    (define-key map (kbd "C-c '") 'org-supertag-view-edit-table)
    (define-key map (kbd "<tab>") 'org-table-next-field)
    (define-key map (kbd "<backtab>") 'org-table-previous-field)
    (define-key map (kbd "M-p") 'org-table-copy-down)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "2") 'org-supertag-view-switch-to-discover)
    (define-key map (kbd "3") 'org-supertag-view-switch-to-columns)
    (set-keymap-parent map org-mode-map)
    (use-local-map map)))

(defun org-supertag-view-table-setup-keys ()
  "Set up key bindings for table view mode."
  (interactive)
  ;; Set up table view and edit functions
  (define-key org-supertag-view-table-mode-map (kbd "e")
              (lambda ()
                (interactive)
                (let ((inhibit-read-only t))
                  (call-interactively 'org-supertag-view-smart-edit))))
  (define-key org-supertag-view-table-mode-map (kbd "v")
              'org-supertag-view-table-node-at-point)
  (define-key org-supertag-view-table-mode-map (kbd "g")
              'org-supertag-view-table-refresh)
  (define-key org-supertag-view-table-mode-map (kbd "q")
              'quit-window))


;;;###autoload
(defun org-supertag-view-table (&optional tag)
  "Show content related to a tag in table view format.
If TAG is provided, show content for that tag.
If point is on a tag, show content for that tag.
Otherwise, prompt for a tag using completion.

The table view displays:
- Tag properties and description
- A table of related nodes with their properties
- Operation instructions for navigation and management"
  (interactive)
  (let ((tag-to-use (or tag
                       (org-supertag-view--get-tag-name)
                       (completing-read "View tag in table format: "
                                      (org-supertag-view--get-all-tags)
                                      nil t))))
    (org-supertag-view--show-content-table tag-to-use)))

(define-obsolete-function-alias 'org-supertag-view-tag-only 'org-supertag-view-table "1.0")



(provide 'org-supertag-view-table)

