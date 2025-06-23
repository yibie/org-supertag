;;; org-supertag-query.el --Query for org-supertag -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-supertag-db)  
(require 'org-supertag-field) 
(require 'cl-lib)

;; Data structure for query items
(cl-defstruct (org-supertag-query-item
               (:constructor org-supertag-query-item-create)
               (:copier nil))
  "Structure for holding query result items."
  id          ; Node ID
  title       ; Title
  tags        ; Tags list
  fields      ; Field values
  file        ; File name
  marked)     ; Marked status

(defcustom org-supertag-query-history-max-items 100
  "Maximum number of keywords to keep in history.
When the limit is reached, least frequently used items will be removed."
  :type 'integer
  :group 'org-supertag)

(defcustom org-supertag-query-history-keywords nil
  "History of search keywords with their frequencies.
Each element is a cons cell (keyword . frequency)."
  :type '(alist :key-type string :value-type integer)
  :group 'org-supertag)

(defvar org-supertag-query--last-keyword nil
  "Store the last used search keyword.")

(defcustom org-supertag-query-history-file
  (expand-file-name "query-history.el" org-supertag-data-directory)
  "File to store query history."
  :type 'file
  :group 'org-supertag)

(defvar org-supertag-query--history nil
  "List of query history items.
Each item is a plist with :query, :count, and :last-used keys.")

(defun org-supertag-query--load-history ()
  "Load query history from file."
  (when (file-exists-p org-supertag-query-history-file)
    (with-temp-buffer
      (insert-file-contents org-supertag-query-history-file)
      (setq org-supertag-query--history (read (current-buffer))))))

(defun org-supertag-query--save-history ()
  "Save query history to file."
  (with-temp-file org-supertag-query-history-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 org-supertag-query--history (current-buffer)))))

(defun org-supertag-query--update-keyword-frequency (input)
  "Update frequency of search INPUT in query history.
INPUT is a string containing the complete search query.
If the query already exists, update its count and timestamp.
If not, add it as a new entry."
  (let* ((now (format-time-string "%Y-%m-%d %H:%M:%S"))
         (query-str (if (stringp input) input (prin1-to-string input)))
         ;; Remove existing entry if found
         (existing-entry (cl-find query-str org-supertag-query--history
                                :key (lambda (x) (plist-get x :query))
                                :test #'equal)))
    
    ;; If entry exists, remove it from history first
    (when existing-entry
      (setq org-supertag-query--history
            (cl-remove query-str org-supertag-query--history
                      :key (lambda (x) (plist-get x :query))
                      :test #'equal)))
    
    ;; Create or update entry
    (let ((entry (or existing-entry
                    `(:query ,query-str :count 0 :last-used ,now))))
      ;; Update count and timestamp
      (plist-put entry :count (1+ (plist-get entry :count)))
      (plist-put entry :last-used now)
      ;; Add to front of history
      (push entry org-supertag-query--history)))
  
  ;; Sort by frequency and last used time
  (setq org-supertag-query--history
        (sort org-supertag-query--history
              (lambda (a b)
                (or (> (plist-get a :count) (plist-get b :count))
                    (and (= (plist-get a :count) (plist-get b :count))
                         (string> (plist-get a :last-used)
                                (plist-get b :last-used)))))))
  
  ;; Truncate if exceeds max items
  (when (> (length org-supertag-query--history) org-supertag-query-history-max-items)
    (setq org-supertag-query--history
          (seq-take org-supertag-query--history org-supertag-query-history-max-items)))
  
  ;; Save changes
  (org-supertag-query--save-history))

(defun org-supertag-query-get-history (&optional limit)
  "Get query history, optionally limited to LIMIT entries."
  (if limit
      (seq-take org-supertag-query--history limit)
    org-supertag-query--history))

;; Hook to load history on startup
(add-hook 'after-init-hook #'org-supertag-query--load-history)

;;---------------------------------------------------------------
;; Org-supertag-query ineral function
;;---------------------------------------------------------------

(defun org-supertag-query--get-keywords ()
  "Get keywords from user input with history support.
Return a list of keywords split from user input."
  (let* ((history (delete-dups
                  (mapcar (lambda (x) (plist-get x :query))
                         (org-supertag-query-get-history))))
         (input (completing-read "Search: " history nil nil nil 'history history)))
    ;; Save complete search query
    (org-supertag-query--update-keyword-frequency input)
    ;; Return list of individual keywords
    (split-string input " " t)))

(defun org-supertag-find-matching-tags (keywords)
  "Find tags matching the given keywords.
KEYWORDS is a list of keywords to match against tag names."
  (let ((all-tags (org-supertag-db-find-by-props '(:type :tag)))
    (dolist (tag-id (mapcar #'car all-tags))
      (when (cl-some (lambda (keyword)
                      (string-match-p (concat "(?i)" keyword) tag-id))
        (push tag-id matching-tags)))
    matching-tags))))

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
  (when-let* ((props (org-supertag-db-get node-id)))
    (plist-get props :tags)))

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
        (unless (org-at-heading-p)
          (org-back-to-heading t))
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

;;---------------------------------------------------------------
;; Query Results Page
;;---------------------------------------------------------------

(defgroup org-supertag-query nil
  "Customization for org-supertag query."
  :group 'org-supertag)

(defcustom org-supertag-query-preview-length 300
  "Preview content maximum length."
  :type 'integer
  :group 'org-supertag-query)

(defface org-supertag-query-current
  '((t :inherit region))
  "Face for current selected item."
  :group 'org-supertag-query)

(defface org-supertag-query-title
  '((t :weight bold))
  "Face for titles in query results."
  :group 'org-supertag-query)

(defface org-supertag-query-tag
  '((t :box t))
  "Face for tags in query results."
  :group 'org-supertag-query)

(defface org-supertag-query-file
  '((t :inherit fixed-pitch))
  "Face for file names in query results."
  :group 'org-supertag-query)

;; Buffer-local 
(defvar-local org-supertag-query-ewoc nil
  "Ewoc object for displaying query results.")


;; Formatting display function
(defun org-supertag-query-pp-item (item)
  "Format display for a single query item."
  (let* ((title (org-supertag-query-item-title item))
         (tags (org-supertag-query-item-tags item))
         (file (org-supertag-query-item-file item))
         (marked (org-supertag-query-item-marked item))
         (id (org-supertag-query-item-id item)))   
    ;; Title line with checkbox and tags
    (insert (propertize (if marked "☑ " "☐ ")
                       'face (if marked 'org-checkbox-statistics-done
                              'org-checkbox))
            (propertize title 'face 'bold)
            " "
            (format "[[id:%s][🔗]]" id))  ; Add ID link  
    (when tags
      (insert " ")
      (dolist (tag tags)
        (insert (propertize (concat "[" tag "]")
                           'face 'org-supertag-query-tag)
                " ")))
    (when file
      (insert " " (propertize (file-name-nondirectory file)
                             'face 'org-supertag-query-file)))
    (insert "\n")))

(defun org-supertag-query-highlight-current ()
  "Highlight current item."
  (let* ((node (ewoc-locate org-supertag-query-ewoc))
         (inhibit-read-only t)
         (beg (ewoc-location node))
         (next (ewoc-next org-supertag-query-ewoc node))
         (end (if next
                  (ewoc-location next)
                (point-max))))
    (remove-overlays (point-min) (point-max) 'org-supertag-query t)
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'face 'org-supertag-query-current)
      (overlay-put ov 'org-supertag-query t))))

(defun org-supertag-query-next ()
  "Move to next result."
  (interactive)
  (when-let* ((current (ewoc-locate org-supertag-query-ewoc))
             (next (ewoc-next org-supertag-query-ewoc current)))
    (goto-char (ewoc-location next))
    (beginning-of-line)
    (org-supertag-query-highlight-current)))

(defun org-supertag-query-prev ()
  "Move to previous result."
  (interactive)
  (when-let* ((current (ewoc-locate org-supertag-query-ewoc))
             (prev (ewoc-prev org-supertag-query-ewoc current)))
    (goto-char (ewoc-location prev))
    (beginning-of-line)
    (org-supertag-query-highlight-current)))

(defun org-supertag-query-toggle-mark ()
  "Toggle mark for current item."
  (interactive)
  (when-let* ((node (ewoc-locate org-supertag-query-ewoc))
              (data (ewoc-data node)))
    (setf (org-supertag-query-item-marked data)
          (not (org-supertag-query-item-marked data)))
    (ewoc-invalidate org-supertag-query-ewoc node)
    (org-supertag-query-next)))
    
(defun org-supertag-query-find-nodes (keywords)
  "Find nodes matching KEYWORDS."
  (let (results)
    (maphash
     (lambda (id props)
       (when (eq (plist-get props :type) :node)
         (let* ((title (plist-get props :title))
                (content (plist-get props :content))
                ;; Get tags and tag fields
                (tag-fields
                 (let ((fields-map (make-hash-table :test 'equal)))
                   (maphash 
                    (lambda (link-id link-props)
                      (when (and (string-prefix-p ":node-tag:" link-id)
                               (equal (plist-get link-props :from) id))
                        (when-let* ((tag-id (plist-get link-props :to))
                                  (tag (org-supertag-db-get tag-id))
                                  (fields (plist-get tag :fields)))
                          (dolist (field fields)
                            (puthash (plist-get field :name) field fields-map)))))
                    org-supertag-db--link)
                   fields-map))
                ;; Get tags
                (tags (let (node-tags)
                       (maphash 
                        (lambda (link-id link-props)
                          (when (and (string-prefix-p ":node-tag:" link-id)
                                   (equal (plist-get link-props :from) id))
                            (push (plist-get link-props :to) node-tags)))
                        org-supertag-db--link)
                       node-tags))
                ;;  Get field values
                (fields (let (node-fields)
                         (maphash
                          (lambda (link-id link-props)
                            (when (and (string-prefix-p ":node-field:" link-id)
                                     (equal (plist-get link-props :from) id))
                              (let* ((field-name (plist-get link-props :field-name))
                                    (field-def (gethash field-name tag-fields))
                                    (value (plist-get link-props :value)))
                                (push (list :name field-name
                                          :type (plist-get field-def :type)
                                          :value value)
                                      node-fields))))
                          org-supertag-db--link)
                         node-fields))
                ;; Build searchable text
                (searchable-text (concat 
                                ;; Title
                                (or title "")
                                " "
                                ;; Tags
                                (mapconcat #'identity tags " ")
                                " "
                                ;; Field values
                                (mapconcat
                                 (lambda (field)
                                   (format "%s:%s"
                                          (plist-get field :name)
                                          (plist-get field :value)))
                                 fields " ")
                                " "
                                ;; Content
                                (or content ""))))
           
           ;; Check if all keywords match
           (when (cl-every 
                  (lambda (keyword)
                    (string-match-p 
                     (regexp-quote keyword) 
                     searchable-text))
                  keywords)
             (push props results)))))
     org-supertag-db--object)
    
    (nreverse results)))


(defvar-local org-supertag-query-mode-map nil
  "Keymap for `org-supertag-query-mode'.")

(defun org-supertag-query-mode-init-map ()
  "Initialize the keymap for org-supertag-query-mode."
  (let ((map (make-sparse-keymap)))
    ;; Navigation and Marking
    (define-key map (kbd "n") #'org-supertag-query-next)
    (define-key map (kbd "p") #'org-supertag-query-prev)
    (define-key map (kbd "m") #'org-supertag-query-toggle-mark)
    (define-key map (kbd "RET") #'org-supertag-query-visit-node)
    (define-key map (kbd "q") #'org-supertag-query-quit)
    
    ;; Export Results
    (define-key map (kbd "e f") #'org-supertag-query-export-results-to-file)
    (define-key map (kbd "e n") #'org-supertag-query-export-results-to-new-file)
    ;; Add new shortcut for inserting at point
    (define-key map (kbd "i") #'org-supertag-query-insert-at-point)
    map))

;; User Query Command
(define-minor-mode org-supertag-query-mode
  "Minor mode for org-supertag query results buffer."
  :lighter " OrgST"
  (when org-supertag-query-mode
    ;; Initialize buffer-local keymap
    (unless org-supertag-query-mode-map
      (setq org-supertag-query-mode-map (org-supertag-query-mode-init-map)))
    ;; Setup when mode is enabled
    (setq buffer-read-only t)
    ;; Use buffer-local keymap
    (use-local-map org-supertag-query-mode-map)))

(defun org-supertag-query-insert-header (keyword-list nodes)
  "Insert query results header information."
  (insert "#+TITLE: SuperTag Query Results\n")
    ;; Search scope
  (insert "* Query Rules:\n")
  (insert "- Search scope: titles, tags, properties and field values\n")
  (insert "- Multiple keywords use AND logic (all must match)\n")
  ;; Search information
  (insert "* Search Terms: "
          (propertize (string-join keyword-list " ")
                     'face 'bold)
                     "\n")
  (insert (format "Found %d matching nodes\n" (length nodes)))

  ;; Shortcuts explanation
  (insert "* Shortcuts:\n")
  (insert "- n/p         : Navigate results\n")
  (insert "- m           : Toggle mark\n")
  (insert "- RET         : Visit node\n")
  (insert "- e f         : Export to file\n")
  (insert "- e n         : Export to new file\n")
  (insert "- i           : Insert selected nodes at current cursor positon\n")
  (insert "- q           : Quit query results\n")
  (insert (make-string 18 ?━))
  (insert "\n")
  )

(defun org-supertag-query-create-item (node)
  "Create display item from node properties."
  (let* ((id (plist-get node :id))
         ;; Get tags
         (tags (let (node-tags)
                (maphash 
                 (lambda (link-id link-props)
                   (when (and (string-prefix-p ":node-tag:" link-id)
                            (equal (plist-get link-props :from) id))
                     (push (plist-get link-props :to) node-tags)))
                org-supertag-db--link)
                node-tags))
         ;; Get field values
         (fields (let (node-fields)
                  (maphash
                   (lambda (link-id link-props)
                     (when (and (string-prefix-p ":node-field:" link-id)
                              (equal (plist-get link-props :from) id))
                       (push (cons (plist-get link-props :field-name)
                                 (plist-get link-props :value))
                             node-fields)))
                   org-supertag-db--link)
                  node-fields)))
    (org-supertag-query-item-create
     :id id
     :title (plist-get node :title)
     :tags tags
     :fields fields
     :file (plist-get node :file-path)
     :marked nil)))

(defun org-supertag-query-show-results (keyword-list nodes)
  "Display search results.
KEYWORD-LIST is the list of keywords to search for.
NODES is the list of matched nodes to display."
  (let ((buf (get-buffer-create "*Org SuperTag Query*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        ;; Ensure our minor mode is enabled
        (org-supertag-query-mode 1)
        ;; Insert header information
        (org-supertag-query-insert-header keyword-list nodes)
        ;; Create ewoc and add results
        (setq-local org-supertag-query-ewoc 
                    (ewoc-create #'org-supertag-query-pp-item))
        ;; Add all results
        (dolist (node nodes)
          (ewoc-enter-last org-supertag-query-ewoc
                          (org-supertag-query-create-item node)))
        ;; Highlight first item if there are results
        (when nodes
          (goto-char (point-min))
          (re-search-forward "^\\[" nil t)
          (beginning-of-line)
          (org-supertag-query-highlight-current))))
    
    (switch-to-buffer buf)))

(defun org-supertag-query--find-node (node-id)
  "Visit node with specified ID.
NODE-ID is the identifier of the node to find.

Returns t if node was found and visited successfully, nil otherwise."
  (when-let* ((props (org-supertag-db-get node-id))
              (file-path (plist-get props :file-path))
              ((file-exists-p file-path))
              (buffer (find-file-noselect file-path)))
    (message "Finding node %s in file %s" node-id file-path)
    (with-current-buffer buffer
      ;; make sure in org-mode buffer, avoid conflict with org-supertag-view.el
      (when (derived-mode-p 'org-mode)
        (message "Buffer is in org-mode")
        (widen)
        (let ((marker (org-id-find-id-in-file node-id file-path)))
          (if marker
              (progn
                (message "Found node at position %s" (cdr marker))
                (goto-char (cdr marker))
                (org-show-entry)
                (org-show-children)
                ;; Switch to buffer before recentering
                (switch-to-buffer buffer)
                (recenter)
                t)
            (message "org-id-find-id-in-file returned nil for node %s in file %s" 
                     node-id file-path)
            ;; Try alternative method using org-map-entries
            (message "Trying alternative method...")
            (save-excursion
              (goto-char (point-min))
              (let ((found nil))
                (org-map-entries
                 (lambda ()
                   (when (equal (org-entry-get nil "ID") node-id)
                     (setq found (point))))
                 t nil)
                (if found
                    (progn
                      (message "Found node using alternative method at position %s" found)
                      (goto-char found)
                      (org-show-entry)
                      (org-show-children)
                      (switch-to-buffer buffer)
                      (recenter)
                      t)
                  (message "Node not found using alternative method either")
                  nil)))))))))

(defun org-supertag-query-visit-node ()
  "Visit current selected node."
  (interactive)
  (when-let* ((node (ewoc-locate org-supertag-query-ewoc))
              (data (ewoc-data node))
              (id (org-supertag-query-item-id data)))
    (org-supertag-query--find-node id)))
    

(defun org-supertag-query-quit ()
  "Quit the query results buffer and return to previous buffer."
  (interactive)
  (let ((results-buffer (get-buffer "*Org SuperTag Query*")))
    (when (and org-supertag-query--original-buffer
               (buffer-live-p org-supertag-query--original-buffer))
      (message "Switching back to original buffer: %s" 
               (buffer-name org-supertag-query--original-buffer))
      (switch-to-buffer org-supertag-query--original-buffer)
      (when org-supertag-query--original-point
        (message "Restoring cursor position to: %d" org-supertag-query--original-point)
        (goto-char org-supertag-query--original-point)))
    ;; Kill the results buffer
    (when results-buffer
      (kill-buffer results-buffer))))


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
  "Toggle all checkboxes in query results."
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
  "Get IDs of all selected nodes from query results."
  (let (selected-nodes)
    (save-excursion
      (goto-char (point-min))
      ;; Skip header section
      (when (re-search-forward "^\\* Shortcuts:" nil t)
        (forward-line))
      ;; Search for all checked items
      (while (re-search-forward 
              "^☑.*\\[\\[id:\\([^]]+\\)\\]"
              nil t)
        (when-let* ((node-id (match-string-no-properties 1)))
          ;; Verify node exists
          (when (org-supertag-node-db-exists-p node-id)
            (push node-id selected-nodes)))))
    ;; Return results
    (when selected-nodes
      (message "Found %d selected nodes" (length selected-nodes))
      (nreverse selected-nodes))))
  

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

(defun org-supertag-insert-nodes (node-ids target-pos-level)
  "Insert nodes at specified position.
NODE-IDS is list of node identifiers
TARGET-POS-LEVEL is cons of (target-point . target-level)"
  (let ((target-file (buffer-file-name))
        (target-point (car target-pos-level))
        (target-level (cdr target-pos-level))
        (success-count 0))
    
    (unless target-file
      (error "Current buffer is not visiting a file"))
    
    (dolist (node-id node-ids)
      (message "Processing node: %s" node-id)
      (condition-case err
          (when (org-supertag-node-move node-id target-file target-level target-point)
            (cl-incf success-count))
        (error
         (message "Failed processing node %s: %s" 
                  node-id 
                  (error-message-string err)))))
    
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
  (let* ((selected-nodes (org-supertag-get-selected-nodes))
         (target-file (read-file-name "Export to file: "))
         (target-pos-level (org-supertag-query--get-insert-position target-file)))
    
    (if (not selected-nodes)
        (message "No nodes selected")
      ;; Select target file
      (condition-case err
          (let* ((target-file (read-file-name "Export to file: "))
                 (target-pos-level (org-supertag-query--get-insert-position target-file)))
            
            ;; Validate file type
            (unless (org-supertag-ensure-org-file target-file)
              (error "Export target must be an org file: %s" target-file))
            
            ;; Process target file
            (with-current-buffer (find-file-noselect target-file)
              (org-mode)
              ;; Insert content at specified position with level
              (org-supertag-insert-nodes selected-nodes target-pos-level)
              (save-buffer)
              
              ;; Display target file
              (find-file target-file)
              (message "Export completed successfully")))
        ;; Error handling
        (error
         (message "Export failed: %s" (error-message-string err))
         (signal (car err) (cdr err)))))))

(defun org-supertag-query--get-insert-position (target-file)
  "Get insertion position and level adjustment for target file.
Shows outline structure in format: filename / outline-path / title
Returns cons cell (point . level-adjust)."
  (let* ((headlines (with-current-buffer (find-file-noselect target-file)
                     (org-with-wide-buffer
                      (org-map-entries
                       (lambda ()
                         (let* ((title (org-get-heading t t t t))
                                (olp (org-get-outline-path))
                                (display (org-supertag-node--format-path
                                        (file-name-nondirectory target-file)
                                        olp
                                        title)))
                           (cons display (point))))))))
         (insert-type (completing-read 
                      "Insert at: "
                      (append
                       '("File Start" "File End")
                       (when headlines
                         '("Under Heading" "Same Level"))))))
    
    (with-current-buffer (find-file-noselect target-file)
      (org-with-wide-buffer
       (pcase insert-type
         ("File Start"
          (cons (org-supertag-find-file-content-start) nil))
         
         ("File End"
          (cons (point-max) nil))
         
         ("Under Heading"
          (let* ((selected (completing-read 
                           "Select parent heading: "
                           (mapcar #'car headlines)
                           nil t))
                 (pos (cdr (assoc selected headlines))))
            ;; Move to selected heading
            (goto-char pos)
            ;; Get current level
            (let ((parent-level (org-current-level)))
              ;; Move to end of subtree to insert
              (org-end-of-subtree)
              (forward-line)
              ;; Return position and target level
              (cons (point) (1+ parent-level)))))
         
         ("Same Level"
          (let* ((selected (completing-read 
                           "Select sibling heading: "
                           (mapcar #'car headlines)
                           nil t))
                 (pos (cdr (assoc selected headlines))))
            ;; Move to selected heading
            (goto-char pos)
            ;; Get current level
            (let ((sibling-level (org-current-level)))
              ;; Move to end of subtree to insert after
              (org-end-of-subtree)
              (forward-line)
              ;; Return position and target level
              (cons (point) sibling-level)))))))))

;;; Save org-supertag-query result at cursor place
(defvar org-supertag-query--original-buffer nil
  "Store the original buffer where search was initiated.")

(defvar org-supertag-query--original-point nil
  "Store cursor position when search was initiated.")

(defun org-supertag-query-export-results-here ()
  "Search and insert results at current cursor position.
Opens query interface and adds 'i' shortcut to insert selected nodes."
  (interactive)
  ;; Save current position
  (setq org-supertag-query--original-buffer (current-buffer)
        org-supertag-query--original-point (point))
  ;; Start normal query interface
  (call-interactively #'org-supertag-query))

(defun org-supertag-query-insert-at-point ()
  "Insert selected nodes at saved cursor position."
  (interactive)
  (when-let* ((selected-nodes (org-supertag-get-selected-nodes))
              (orig-buf org-supertag-query--original-buffer)
              (orig-point org-supertag-query--original-point))
    (if (not selected-nodes)
        (message "No nodes selected")
      ;; Switch to original buffer and position
      (with-current-buffer orig-buf
        (save-excursion
          (goto-char orig-point)
          ;; Insert links
          (let ((content
                 (with-temp-buffer
                   (dolist (node-id selected-nodes)
                     (when-let* ((node (org-supertag-db-get node-id))
                                (title (plist-get node :title))
                                (clean-title 
                                 (substring-no-properties 
                                  (if (stringp title)
                                      title
                                    (prin1-to-string title)))))
                       (insert (format "- [[id:%s][%s]]\n"
                                     node-id
                                     clean-title))))
                   (buffer-string))))
            (insert content)
            (message "Inserted %d node links at original position" 
                     (length selected-nodes)))))
      ;; Kill query buffer
      (kill-buffer))))

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
  
  (let* ((keywords (org-supertag-query--get-keywords))
         (nodes (org-supertag-query-find-nodes keywords)))
    (org-supertag-query-show-results keywords nodes)))

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
  
  (let* ((keywords (org-supertag-query--get-keywords))
         (nodes (org-supertag-query--find-nodes-in-buffer keywords)))
    (org-supertag-query-show-results keywords nodes)))




(provide 'org-supertag-query)

