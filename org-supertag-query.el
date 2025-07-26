;;; org-supertag-query.el --Query for org-supertag -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-supertag-view-utils)
(require 'org-supertag-db)  
(require 'org-supertag-field) 
(require 'cl-lib)

;; Data structure for query items
;; 删除 org-supertag-query-item 结构体定义

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
;; 删除 org-supertag-query-ewoc 相关定义


;; Formatting display function
;; 删除 org-supertag-query-pp-item 函数定义

(defun org-supertag-query-highlight-current ()
  "Highlight the current result card."
  (remove-overlays (point-min) (point-max) 'org-supertag-query t)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "┌")
      (let ((beg (point))
            (end (save-excursion
                   (re-search-forward "^└" nil t)
                   (line-end-position))))
        (when end
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'face 'org-supertag-query-current)
            (overlay-put ov 'org-supertag-query t)))))))

(defun org-supertag-query-next ()
  "Jump to the next result card."
  (interactive)
  (let ((p (point)))
    ;; First, find the end of the current card to start our search from there.
    (let ((search-start-point
           (save-excursion
             (beginning-of-line)
             ;; Find the start of the current card.
             (unless (looking-at "┌")
               (re-search-backward "^┌" nil t))
             ;; Now, find the end of this card.
             (when (re-search-forward "^└" nil t)
               (point))))) ;; The search for the next card will start *after* this point.
      (if (and search-start-point
               (save-excursion (goto-char search-start-point)
                               (re-search-forward "^┌" nil t)))
          ;; If a next card is found...
          (progn
            (goto-char (match-beginning 0))
            (org-supertag-query-highlight-current))
        ;; Otherwise, stay put.
        (goto-char p)))))

(defun org-supertag-query-prev ()
  "Jump to the previous result card."
  (interactive)
  (let ((p (point)))
    ;; Find the start of the current card. This will be our search boundary.
    (let ((current-card-start
           (save-excursion
             (beginning-of-line)
             (if (looking-at "┌")
                 (point)
               (re-search-backward "^┌" nil t)))))
      (message "DEBUG: Current point: %d, Card start: %s, Point-min: %d" p current-card-start (point-min))
      ;; If we found the start of the current card, and it's not the first thing...
      (if (and current-card-start (> current-card-start (point-min)))
          ;; ...then try to search backward from just before it.
          (if (save-excursion
                (goto-char (1- current-card-start))
                (re-search-backward "^┌" nil t))
              ;; If successful, move to the matched position
              (progn
                (message "DEBUG: Found previous card at: %d" (match-beginning 0))
                (goto-char (match-beginning 0))
                (org-supertag-query-highlight-current))
            ;; If search fails, go back to original point.
            (message "DEBUG: No previous card found")
            (goto-char p))
        ;; If we couldn't find the current card start, go back to original point.
        (message "DEBUG: Could not find current card start or at beginning")
        (goto-char p)))))

(defun org-supertag-query-toggle-mark ()
  "Toggle the marked state of the current card and redraw it."
  (interactive)
  (when-let* ((node-id (get-text-property (point) 'node-id))
              (result-pair (get-text-property (point) 'result-pair)))
    (if (member node-id org-supertag-query--marked-nodes)
        (setq org-supertag-query--marked-nodes (remove node-id org-supertag-query--marked-nodes))
      (push node-id org-supertag-query--marked-nodes))
    ;; Redraw the card to show the new checkbox state
    (let ((inhibit-read-only t)
          (p (point))
          (card-width 80)) ; Ensure this matches the width in show-results
      (save-excursion
        (let ((beg (save-excursion
                     (beginning-of-line)
                     (if (looking-at "┌")
                         (point)
                       (re-search-backward "^┌" nil t))))
              (end nil))
          (when beg
            (goto-char beg)
            ;; Find the end of the card's region to delete it entirely.
            (setq end (save-excursion
                        (when (re-search-forward "^└" nil t)
                          (end-of-line)
                          (forward-char 1) ; Include the final newline
                          (point))))
            (when end
              (delete-region beg end)
              (goto-char beg)
              (let* ((node (car result-pair))
                     (context (cdr result-pair))
                     (card-lines (org-supertag-query--format-card
                                  node context card-width
                                  (member node-id org-supertag-query--marked-nodes))))
                (dolist (line card-lines)
                  (insert line "\n"))
                ;; Restore the text property for the whole card
                (add-text-properties beg (- (point) 1) `(result-pair ,result-pair)))))))
      (goto-char p)
      (org-supertag-query-highlight-current))))

(defun org-supertag-query-find-nodes (keywords)
  "Find nodes matching KEYWORDS.
Returns a list of cons cells (NODE-PROPS . CONTEXT-SNIPPET),
where CONTEXT-SNIPPET is a string of content around the first
matching keyword, or nil if the match was not in the content."
  (let (results)
    (maphash
     (lambda (id props)
       (when (eq (plist-get props :type) :node)
         (let* ((title (plist-get props :title))
                (content (plist-get props :content))
                (tag-fields
                 (let ((fields-map (make-hash-table :test 'equal)))
                   (maphash
                    (lambda (link-id link-props)
                      (when (and (string-prefix-p ":node-tag:" link-id) (equal (plist-get link-props :from) id))
                        (when-let* ((tag-id (plist-get link-props :to)) (tag (org-supertag-db-get tag-id)) (fields (plist-get tag :fields)))
                          (dolist (field fields) (puthash (plist-get field :name) field fields-map)))))
                    org-supertag-db--link)
                   fields-map))
                (tags (org-supertag-query--get-node-tags id)))
           ;; Check if all keywords match
           (let ((match-context nil)
                 (all-match t))
             (dolist (keyword keywords)
               (let* ((keyword-re (regexp-quote keyword))
                      (title-match (and title (string-match-p keyword-re title)))
                      (tag-match (cl-some (lambda (t) (string-match-p keyword-re t)) tags))
                      (content-match (and content (string-match keyword-re content))))
                 (unless (or title-match tag-match content-match)
                   (setq all-match nil))
                 ;; Capture context from the first keyword that matches in content
                 (when (and content-match (not match-context))
                   (let* ((match-start (match-beginning 0))
                          (context-start (max 0 (- match-start 40)))
                          (context-end (min (length content) (+ (match-end 0) 40)))
                          (prefix (if (> context-start 0) "..." ""))
                          (suffix (if (< context-end (length content)) "..." "")))
                     (setq match-context (concat prefix (substring content context-start context-end) suffix))))))
             (when all-match
               (push (cons props match-context) results))))))
     org-supertag-db--object)
    (nreverse results)))


(defvar-local org-supertag-query-mode-map nil
  "Keymap for `org-supertag-query-mode'.")

(defvar-local org-supertag-query--marked-nodes nil
  "List of marked node IDs in the query buffer.")

(defun org-supertag-query-mode-init-map ()
  "Initialize the keymap for org-supertag-query-mode."
  (let ((map (make-sparse-keymap)))
    ;; Navigation and Marking
    (define-key map (kbd "n") #'org-supertag-query-next)
    (define-key map (kbd "p") #'org-supertag-query-prev)
    (define-key map (kbd "SPC") #'org-supertag-query-toggle-mark)
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
  (insert (propertize (format "SuperTag Query Results: ‘%s’" (string-join keyword-list " "))
                      'face '(:height 1.5 :weight bold)))
  (insert (format "\nFound %d matching nodes.\n\n" (length nodes)))
  (insert (propertize "Operations:\n" 'face '(:weight bold)))
  (insert " [n/p] Navigate [SPC] Toggle Mark [RET] Visit Node [i] Insert Links\n")
  (insert " [e f] Export to File [e n] Export to New File [q] Quit\n\n"))

(defun org-supertag-query--strict-pad-line (line target-width)
  "Strictly pad LINE to exactly TARGET-WIDTH characters.
This ensures consistent line lengths for card borders."
  (let* ((current-width (string-width line))
         (padding-needed (- target-width current-width)))
    (if (<= padding-needed 0)
        (truncate-string-to-width line target-width)
      (concat line (make-string padding-needed ?\s)))))

(defun org-supertag-query--format-card (node-props context-snippet width marked-p)
  "Format a node into a bordered card of fixed WIDTH.
Returns the card as a list of strings, each correctly padded."
  (let* ((title (or (plist-get node-props :title) "No Title"))
         (node-id (plist-get node-props :id))
         (file-path (plist-get node-props :file-path))
         (tags (org-supertag-query--get-node-tags node-id))
         (inner-width (- width 4)) ; Width for content inside borders
         (checkbox (if marked-p "[X]" "[ ]"))
         (title-with-checkbox (format "%s %s" checkbox title))
         (card-lines '()))
    ;; Top border
    (push (format "┌%s┐" (make-string (- width 2) ?─)) card-lines)
    ;; Title
    (dolist (line (org-supertag-view-util-wrap-text title-with-checkbox inner-width))
      (push (format "│ %s │" (org-supertag-query--strict-pad-line line inner-width)) card-lines))
    ;; Separator
    (push (format "├%s┤" (make-string (- width 2) ?─)) card-lines)
    ;; File Path
    (when file-path
      (let ((file-str (format "File: %s" (file-name-nondirectory file-path))))
        (dolist (line (org-supertag-view-util-wrap-text file-str inner-width))
          (push (format "│ %s │" (org-supertag-query--strict-pad-line line inner-width)) card-lines))))
    ;; Tags
    (when tags
      (let ((tag-str (format "Tags: %s" (string-join tags " "))))
        (dolist (line (org-supertag-view-util-wrap-text tag-str inner-width))
          (push (format "│ %s │" (org-supertag-query--strict-pad-line line inner-width)) card-lines))))
    ;; Context Snippet - display as separate section
    (when context-snippet
      ;; Add a subtle separator for context
      (push (format "├%s┤" (make-string (- width 2) ?─)) card-lines)
      ;; Context header
      (push (format "│ %s │" (org-supertag-query--strict-pad-line "Context:" inner-width)) card-lines)
      ;; Context content - preserve original formatting
      (let ((context-lines (split-string context-snippet "\n" t)))
        (dolist (line context-lines)
          (let ((clean-line (string-trim line)))
            (when (not (string-empty-p clean-line))
              (dolist (wrapped-line (org-supertag-view-util-wrap-text clean-line inner-width))
                (push (format "│ %s │" (org-supertag-query--strict-pad-line wrapped-line inner-width)) card-lines)))))))
    ;; Bottom border
    (push (format "└%s┘" (make-string (- width 2) ?─)) card-lines)
    ;; Propertize and return
    (mapcar (lambda (line) (propertize line 'node-id node-id))
            (nreverse card-lines))))

(defun org-supertag-query-show-results (keyword-list nodes)
  "Display search results in a card-based layout."
  (let ((buf (get-buffer-create "*Org SuperTag Query*"))
        (card-width 80))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-supertag-query-mode 1)
        ;; Initialize marked nodes list for this view
        (setq-local org-supertag-query--marked-nodes nil)
        (org-supertag-query-insert-header keyword-list nodes)
        (if (not nodes)
            (insert "  No matching nodes found.\n")
          (dolist (result-pair nodes)
            (let* ((node (car result-pair))
                   (context (cdr result-pair))
                   (node-id (plist-get node :id))
                   ;; Pass marked status to card formatter
                   (card-lines (org-supertag-query--format-card
                                node context card-width
                                (member node-id org-supertag-query--marked-nodes))))
              (let ((start (point)))
                (dolist (line card-lines)
                  (insert line "\n"))
                ;; Add result-pair property for the toggle function
                (add-text-properties start (point) `(result-pair ,result-pair)))
              (insert "\n")))))
      ;; Highlight first result
      (when nodes
        (goto-char (point-min))
        (re-search-forward "^┌" nil t)
        (beginning-of-line)
        (org-supertag-query-highlight-current)))
    (switch-to-buffer buf)))

(defun org-supertag-query--find-node (node-id)
  "Visit node with specified ID.
NODE-ID is the identifier of the node to find.

Returns t if node was found and visited successfully, nil otherwise.
This function now uses the robust `org-supertag-goto-node` which
does not depend on potentially stale caches."
  (org-supertag-goto-node node-id))

(defun org-supertag-query-visit-node ()
  "访问当前选中节点。"
  (interactive)
  (when-let ((id (get-text-property (point) 'node-id)))
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
  "Get all marked node IDs from the query buffer."
  (let ((selected-ids (copy-list org-supertag-query--marked-nodes)))
    (when selected-ids
      (message "Found %d selected nodes" (length selected-ids)))
    (nreverse selected-ids)))
  

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

