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
        (org-supertag-view--insert-content-table-with-button tag))

      (setq buffer-read-only t)
      (goto-char (point-min)))

    ;; Use full-screen display instead of sidebar window
    (switch-to-buffer buffer)
    (delete-other-windows)))

(defun org-supertag-view--get-field-value-from-db (node-id field-name)
  "Get field value from database.
NODE-ID is the node identifier
FIELD-NAME is the field name"
  (let* ((link-id (format ":node-field:%s->%s" node-id field-name))
         (link (gethash link-id org-supertag-db--link)))
    (when link
      (plist-get link :value))))

(defun org-supertag-view--insert-content-table-with-button (tag)
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
                   (value (org-supertag-view--get-field-value-from-db node-id field-name)))
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

(defun org-supertag-view--update-field-value (node-id field-name value)
  "Update VALUE of FIELD-NAME for NODE-ID.
Returns t if update was successful, nil otherwise."
  (when (and node-id field-name)
    (if (not value)
        ;; If value is empty, remove field
        (org-supertag-node-remove-field node-id field-name)
      ;; Otherwise, update field value
      (org-supertag-node-set-field node-id field-name value))))

(defun org-supertag-view--get-field-value (node-id field-name)
  "Get value of FIELD-NAME for NODE-ID."
  (let* ((tag (org-supertag-view--get-current-tag))
         (tag-def (org-supertag-tag-get tag)))
    ;; Use org-supertag-tag-get-field-value to get field value
    (org-supertag-tag-get-field-value tag-def field-name)))

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
  "Update field value in database and file.
NODE-ID is the node identifier
FIELD-NAME is the field name
FIELD-VALUE is the new value
TAG-ID is the tag identifier
Returns t if update successful, nil if failed."
  (condition-case err
      (progn
        ;; 1. Update database
        (let ((link-id (format ":node-field:%s->%s" node-id field-name)))
          (if (and field-value (not (string-empty-p field-value)))
              ;; If there is a value, update the database
              (puthash link-id
                       (list :from node-id
                             :to field-name
                             :tag-id tag-id
                             :value field-value
                             :created-at (current-time))
                       org-supertag-db--link)
            ;; If value is empty, remove field
            (remhash link-id org-supertag-db--link)))
        
        ;; 2. Update file
        (when-let* ((node-props (gethash node-id org-supertag-db--object))
                    (file-path (plist-get node-props :file-path))
                    (pos (plist-get node-props :pos)))
          (with-current-buffer (find-file-noselect file-path)
            (save-excursion
              (goto-char pos)
              (if (and field-value (not (string-empty-p field-value)))
                  (org-set-property field-name field-value)
                (org-delete-property field-name)))))
        
        ;; 3. Mark database as dirty and schedule save
        (org-supertag-db--mark-dirty)
        (org-supertag-db--schedule-save)
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

(defun org-supertag-view--get-field-link (node-id field-name)
  "Get field link for NODE-ID and FIELD-NAME.
Performs case-insensitive search for field name."
  (let ((link-id (format ":node-field:%s->%s" node-id field-name)))
    (gethash link-id org-supertag-db--link)))

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
                            ;; Query field value from link hash table
                            (link-id (format ":node-field:%s->%s" node-id field-name))
                            (field-link (gethash link-id org-supertag-db--link))
                            (value (when field-link
                                    (plist-get field-link :value))))
                       (message "DEBUG: Node %s Field %s Value %s" node-id field-name value)
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

(defun org-supertag-view--edit-field-value (field-info)
  "Core function to edit field values.
FIELD-INFO is a property list containing all information about the current field.
Returns a cons pair of (success . new-value), where success is t if editing is successful."
  (let* ((field (plist-get field-info :field))
         (field-type (when field (plist-get field :type)))
         (field-name (when field (plist-get field :name)))
         (current-value (plist-get field-info :value))
         (col (plist-get field-info :col))
         new-value)
    
    (setq new-value
          (cond
           ;; Date column or date type
           ((or (= col 3) (eq field-type 'date))
            (let ((date (org-read-date nil t nil "Enter date"
                                      (when (and current-value
                                                (not (string-empty-p current-value)))
                                        current-value))))
              (format-time-string "<%Y-%m-%d %a>" date)))
           
           ;; timestamp type
           ((eq field-type 'timestamp)
            (let ((date-time (org-read-date t t nil "Enter date and time"
                                          (when (and current-value
                                                    (not (string-empty-p current-value)))
                                            current-value))))
              (format-time-string "[%Y-%m-%d %a %H:%M]" date-time)))
           
           ;; options type
           ((eq field-type 'options)
            (let ((options (or (plist-get field :options) '("Option1" "Option2"))))
              (completing-read
               (format "Select option for %s: " field-name)
               options nil t current-value)))
           
           ;; tag-reference type
           ((eq field-type 'tag-reference)
            (let ((nodes (list)))
              (maphash
               (lambda (id props)
                 (when (eq (plist-get props :type) :node)
                   (let ((title (or (plist-get props :title)
                                   (format "Node %s" id))))
                     (push (cons title id) nodes))))
               org-supertag-db--object)
              (let* ((choices (mapcar #'car nodes))
                     (selected (completing-read
                              (format "Select node for %s: " field-name)
                              choices nil t nil)))
                (when selected
                  (let ((node-id (cdr (assoc selected nodes))))
                    (format "[[%s][%s]]" node-id selected))))))
           
           ;; list type
           ((eq field-type 'list)
            (read-string
             (format "Enter values for %s (comma-separated): " field-name)
             current-value))
           
           ;; range type
           ((eq field-type 'range)
            (read-string
             (format "Enter range for %s (N-M or N..M): " field-name)
             current-value))
           
           ;; Default to simple string input
           (t
            (read-string (format "Edit%s: "
                                (if field-name
                                    (format " %s" field-name)
                                  (format " column %d" col)))
                        current-value))))
    
    (when new-value
      ;; Validate new value
      (let ((validated-value (org-supertag-view-validate-field
                            new-value field-type field)))
        (when validated-value
          (cons t validated-value))))))

(defun org-supertag-view-smart-edit ()
  "Smart edit function - selects the appropriate editing method based on the current field type.
Immediately saves the field value to the database after editing."
  (interactive)
  (if (not (org-at-table-p))
      ;; Not on a table, switch to overall edit mode
      (progn
        (setq buffer-read-only nil)
        (message "Table unlocked, entering edit mode"))
    
    ;; On a table, get field information and edit
    (let* ((field-info (org-supertag-view-table-get-field-info))
           (col (plist-get field-info :col))
           (tag (plist-get field-info :tag))
           (row-data (plist-get field-info :row-data))
           (node-title (when row-data
                        (string-trim
                         (replace-regexp-in-string "\\[\\(v\\|N/A\\)\\]\\s-*" ""
                                                 (nth 0 row-data)))))
           (node-id (when node-title
                     (org-supertag-view-table-find-node-id node-title)))
           (field (plist-get field-info :field))
           (field-name (when field (plist-get field :name))))
      
      ;; Ensure read-only status is removed before editing
      (when buffer-read-only
        (setq buffer-read-only nil))
      
      ;; Edit field value
      (let* ((edit-result (org-supertag-view--edit-field-value field-info))
             (success (car edit-result))
             (new-value (cdr edit-result)))
        
        (when (and success new-value)
          ;; Update database (if it is a custom field)
          (when (and node-id field-name (> col 3))
            (org-supertag-view-table-update-field node-id field-name new-value tag))
          
          ;; Update table display
          (org-table-put nil col new-value)
          (org-table-align)
          
          (message "Field updated: %s" new-value))))))

(defun org-supertag-view-validate-field (value field-type field-props)
  "Validate and format the validity of input values based on field type.
Validates the input VALUE based on FIELD-TYPE and FIELD-PROPS and returns the formatted value.
If the value is invalid, returns nil and displays an error message."
  (cond
   ;; Date field validation - ensure format is <YYYY-MM-DD XXX>
   ((eq field-type 'date)
    (if (string-match-p "^<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( [A-Za-z]\\{3\\}\\)?>$" value)
        value
      (message "Invalid date format. Please use <YYYY-MM-DD> format")
      nil))
   
   ;; Timestamp field validation - ensure format is [YYYY-MM-DD XXX HH:MM]
   ((eq field-type 'timestamp)
    (if (string-match-p "^\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( [A-Za-z]\\{3\\}\\)?\\( [0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\]$" value)
        value
      (message "Invalid timestamp format. Please use [YYYY-MM-DD HH:MM] format")
      nil))
   
   ;; Number field validation
   ((eq field-type 'number)
    (if (string-match-p "^[+-]?[0-9]*\\.?[0-9]+$" value)
        value
      (message "Invalid number format")
      nil))
   
   ;; Range field validation - ensure format is N-M or N..M
   ((eq field-type 'range)
    (if (string-match-p "^[0-9]+\\(\\.\\.|[-~]\\)[0-9]+$" value)
        value
      (message "Invalid range format. Please use N-M or N..M format")
      nil))
   
   ;; List field validation - ensure format is comma-separated items
   ((eq field-type 'list)
    (if (or (string-empty-p value)
            (string-match-p "^[^,]+\\(,[^,]+\\)*$" value))
        value
      (message "Invalid list format. Please use comma-separated items")
      nil))
   
   ;; Options field validation - ensure value is in the allowed options list
   ((eq field-type 'options)
    (let ((allowed-options (plist-get field-props :options)))
      (if (or (string-empty-p value)
              (member value allowed-options))
          value
        (message "Invalid option value. Allowed options: %s"
                 (mapconcat #'identity allowed-options ", "))
        nil)))
   
   ;; Tag reference field validation - ensure referenced tag exists
   ((eq field-type 'tag-reference)
    (if (or (string-empty-p value)
            (org-supertag-node-exists-p value))
        value
      (message "Referenced node does not exist: %s" value)
      nil))
   
   ;; Accept any value by default
   (t value)))

;;----------------------------------------------------------------------
;; Mode definitions
;;----------------------------------------------------------------------

(defun org-supertag-view-refresh ()
  "Refresh the current view."
  (interactive)
  (cond
   ;; in multi-column view mode
   ((eq major-mode 'org-supertag-column-mode)
    (org-supertag-view--refresh-column-view))
   ;; in tag discovery mode
   ((eq major-mode 'org-supertag-discover-mode)
    (org-supertag-view--refresh-discover))
   ;; in tag-only view mode
   ((and (eq major-mode 'org-mode)
         (bound-and-true-p org-supertag-view-mode)
         (string-match-p "\\*Org SuperTag Table View:" (buffer-name)))
    (let ((tag (progn
                 (string-match "\\*Org SuperTag Table View: \\(.*\\)\\*" (buffer-name))
                 (match-string 1 (buffer-name)))))
      (when tag
        (org-supertag-view--show-content-table tag))))
   ;; in traditional view (compatible with old code)
   (t
    (let ((tag (car (split-string (buffer-name) ": #"))))
      (when tag
        (org-supertag-tag-columns tag))))))

;;----------------------------------------------------------------------
;; Mode definitions
;;----------------------------------------------------------------------

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
    (define-key map (kbd "g") 'org-supertag-view-refresh)
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
              'org-supertag-view-refresh)
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

