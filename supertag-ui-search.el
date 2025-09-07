;;; supertag-ui-search.el --- Interactive search window for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides the interactive search window functionality for org-supertag.
;; It includes:
;; - Card-based results display with navigation, marking, and export capabilities
;; - Query history management
;; - Keyword-based search across nodes, tags, and content
;; - Functions renamed from org-supertag-query-* to supertag-search-*
;; - Main entry point: supertag-search (alias: org-supertag-query for backward compatibility)

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'supertag-core-store) ; For data access

;;; --- Search History Management ---

(defcustom supertag-search-history-max-items 100
  "Maximum number of keywords to keep in history."
  :type 'integer
  :group 'org-supertag)

(defcustom supertag-search-history-file
  (expand-file-name "search-history.el" 
                    (or (bound-and-true-p org-supertag-data-directory) 
                        user-emacs-directory))
  "File to store search history."
  :type 'file
  :group 'org-supertag)

(defvar supertag-search--history nil
  "List of search history items.")

(defvar supertag-search--original-buffer nil
  "Store the original buffer where search was initiated.")

(defvar supertag-search--original-point nil
  "Store cursor position when search was initiated.")

(defvar-local supertag-search--marked-nodes nil
  "List of marked node IDs in the search buffer.")

(defun supertag-search--load-history ()
  "Load search history from file."
  (when (file-exists-p supertag-search-history-file)
    (with-temp-buffer
      (insert-file-contents supertag-search-history-file)
      (setq supertag-search--history (read (current-buffer))))))

(defun supertag-search--save-history ()
  "Save search history to file."
  (with-temp-file supertag-search-history-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 supertag-search--history (current-buffer)))))

(defun supertag-search--update-keyword-frequency (input)
  "Update frequency of search INPUT in history."
  (let* ((now (current-time))
         (query-str (if (stringp input) input (prin1-to-string input)))
         (existing-entry (cl-find query-str supertag-search--history
                                :key (lambda (x) (plist-get x :query))
                                :test #'equal)))
    
    (when existing-entry
      (setq supertag-search--history
            (cl-remove query-str supertag-search--history
                      :key (lambda (x) (plist-get x :query))
                      :test #'equal)))
    
    (let ((entry (or existing-entry
                    `(:query ,query-str :count 0 :last-used ,now))))
      (plist-put entry :count (1+ (plist-get entry :count)))
      (plist-put entry :last-used now)
      (push entry supertag-search--history)))
  
  (setq supertag-search--history
        (sort supertag-search--history
              (lambda (a b)
                (or (> (plist-get a :count) (plist-get b :count))
                    (and (= (plist-get a :count) (plist-get b :count))
                         (supertag-search--time-less-p (plist-get b :last-used)
                                (plist-get a :last-used)))))))
  
  (when (> (length supertag-search--history) supertag-search-history-max-items)
    (setq supertag-search--history
          (seq-take supertag-search--history supertag-search-history-max-items)))
  
  (supertag-search--save-history))

(defun supertag-search--time-less-p (time-a time-b)
  "Compare two time values that may be in different formats.
Handles both time stamps (list) and date strings."
  (let ((time-converter (lambda (time-val)
                          (if (stringp time-val)
                              ;; If it's a string, parse it, replacing nils with 0 for encode-time.
                              (apply #'encode-time
                                     (mapcar (lambda (x) (or x 0))
                                             (parse-time-string time-val)))
                            ;; Otherwise, assume it's a valid time list.
                            time-val))))
    (time-less-p (funcall time-converter time-b) (funcall time-converter time-a))))

(defun supertag-search--get-keywords ()
  "Get keywords from user input with history support."
  (let* ((history (delete-dups
                  (mapcar (lambda (x) (plist-get x :query))
                         supertag-search--history)))
         (input (read-string "Search: " nil 'history history)))
    (supertag-search--update-keyword-frequency input)
    (split-string input " " t)))

;;; --- Search Window UI ---

(defgroup org-supertag-search nil
  "Customization for org-supertag search."
  :group 'org-supertag)

(defcustom org-supertag-search-preview-length 300
  "Preview content maximum length."
  :type 'integer
  :group 'org-supertag-search)

(defface org-supertag-search-current
  '((t :inherit region))
  "Face for current selected item."
  :group 'org-supertag-search)

(defface org-supertag-search-title
  '((t :weight bold))
  "Face for titles in search results."
  :group 'org-supertag-search)

(defface org-supertag-search-tag
  '((t :box t))
  "Face for tags in search results."
  :group 'org-supertag-search)

(defface org-supertag-search-file
  '((t :inherit fixed-pitch))
  "Face for file names in search results."
  :group 'org-supertag-search)

(defvar-local org-supertag-search-mode-map nil
  "Keymap for `org-supertag-search-mode'.")

(defun supertag-search-mode-init-map ()
  "Initialize the keymap for supertag-search-mode."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'supertag-search-next)
    (define-key map (kbd "p") #'supertag-search-prev)
    (define-key map (kbd "SPC") #'supertag-search-toggle-mark)
    (define-key map (kbd "RET") #'supertag-search-visit-node)
    (define-key map (kbd "q") #'supertag-search-quit)
    (define-key map (kbd "e f") #'supertag-search-export-results-to-file)
    (define-key map (kbd "e n") #'supertag-search-export-results-to-new-file)
    (define-key map (kbd "i") #'supertag-search-insert-at-point)
    map))

(define-minor-mode org-supertag-search-mode
  "Minor mode for org-supertag search results buffer."
  :lighter " SupertagSearch"
  (when org-supertag-search-mode
    (unless org-supertag-search-mode-map
      (setq org-supertag-search-mode-map (supertag-search-mode-init-map)))
    (setq buffer-read-only t)
    (use-local-map org-supertag-search-mode-map)))

;;; --- Search Functions ---

(defun supertag-search-find-nodes (keywords)
  "Find nodes matching KEYWORDS."
  (let (results)
    (let ((nodes-collection (supertag-get '(:nodes))))
      (when (hash-table-p nodes-collection)
        (maphash
         (lambda (id node-data)
           (let* ((title (plist-get node-data :title))
                  (content (plist-get node-data :content))
                  (tags (plist-get node-data :tags))
                  (properties (plist-get node-data :properties))
                  (match-context nil)
                  (all-match t))
             (dolist (keyword keywords)
               (let* ((keyword-re (regexp-quote keyword))
                      (title-match (and title (string-match-p keyword-re title)))
                      (tag-match (and tags (cl-some (lambda (tag) (string-match-p keyword-re tag)) tags)))
                      (content-match (and content (string-match keyword-re content)))
                      (field-match (and properties
                                           (cl-some (lambda (prop)
                                                      (and (stringp prop)
                                                           (string-match-p keyword-re prop)))
                                                    (let (prop-values)
                                                      (let ((props properties))
                                                        (while props
                                                          (push (cadr props) prop-values)
                                                          (setq props (cddr props))))
                                                      prop-values)))))
                 (unless (or title-match tag-match content-match field-match)
                   (setq all-match nil))
                 ;; Generate context for content matches
                 (when (and content-match (not match-context))
                   (let* ((match-start (match-beginning 0))
                          (context-start (max 0 (- match-start 40)))
                          (context-end (min (length content) (+ (match-end 0) 40)))
                          (prefix (if (> context-start 0) "..." ""))
                          (suffix (if (< context-end (length content)) "..." "")))
                     (setq match-context (concat prefix (substring content context-start context-end) suffix))))))
             (when all-match
               (push (cons node-data match-context) results))))
         nodes-collection))
    (nreverse results))))

;;; --- Card Formatting ---

(defun supertag-search--strict-pad-line (line target-width)
  "Strictly pad LINE to exactly TARGET-WIDTH characters."
  (let* ((current-width (string-width line))
         (padding-needed (- target-width current-width)))
    (if (<= padding-needed 0)
        (truncate-string-to-width line target-width)
      (concat line (make-string padding-needed ?\ )))))

(defun supertag-search--wrap-text (text width)
  "Wrap TEXT to WIDTH characters, breaking at word boundaries."
  (if (<= (length text) width)
      (list text)
    (let ((words (split-string text " " t))
          (lines '())
          (current-line ""))
      (dolist (word words)
        (let ((test-line (if (string-empty-p current-line)
                             word
                           (concat current-line " " word))))
          (if (<= (length test-line) width)
              (setq current-line test-line)
            (when (not (string-empty-p current-line))
              (push current-line lines))
            (setq current-line word))))
      (when (not (string-empty-p current-line))
        (push current-line lines))
      (nreverse lines))))

(defun supertag-search--get-node-tags (node-id)
  "Get all tags for a node for search display."
  (when-let* ((node-data (supertag-get (list :nodes node-id))))
    (plist-get node-data :tags)))

(defun supertag-search--format-card (node-props context-snippet width marked-p)
  "Format a node into a bordered card of fixed WIDTH."
  (let* ((title (or (plist-get node-props :title) "No Title"))
         (node-id (plist-get node-props :id))
         (file-path (plist-get node-props :file))
         (tags (supertag-search--get-node-tags node-id))
         (inner-width (- width 4))
         (checkbox (if marked-p "[X]" "[ ]"))
         (title-with-checkbox (format "%s %s" checkbox title))
         (card-lines '()))
    ;; Top border
    (push (format "┌%s┐" (make-string (- width 2) ?─)) card-lines)
    ;; Title with checkbox
    (dolist (line (supertag-search--wrap-text title-with-checkbox inner-width))
      (push (format "│ %s │" (supertag-search--strict-pad-line line inner-width)) card-lines))
    ;; Separator
    (push (format "├%s┤" (make-string (- width 2) ?─)) card-lines)
    ;; File Path
    (when file-path
      (let ((file-str (format "File: %s" (file-name-nondirectory file-path))))
        (dolist (line (supertag-search--wrap-text file-str inner-width))
          (push (format "│ %s │" (supertag-search--strict-pad-line line inner-width)) card-lines))))
    ;; Tags
    (when tags
      (let ((tag-str (format "Tags: %s" (string-join tags " "))))
        (dolist (line (supertag-search--wrap-text tag-str inner-width))
          (push (format "│ %s │" (supertag-search--strict-pad-line line inner-width)) card-lines))))
    ;; Context Snippet
    (when context-snippet
      (push (format "├%s┤" (make-string (- width 2) ?─)) card-lines)
      (push (format "│ %s │" (supertag-search--strict-pad-line "Context:" inner-width)) card-lines)
      (let ((context-lines (split-string context-snippet "\n" t)))
        (dolist (line context-lines)
          (let ((clean-line (string-trim line)))
            (when (not (string-empty-p clean-line))
              (dolist (wrapped-line (supertag-search--wrap-text clean-line inner-width))
                (push (format "│ %s │" (supertag-search--strict-pad-line wrapped-line inner-width)) card-lines)))))))
    ;; Bottom border
    (push (format "└%s┘" (make-string (- width 2) ?─)) card-lines)
    ;; Propertize and return
    (mapcar (lambda (line) (propertize line 'node-id node-id))
            (nreverse card-lines))))

(defun supertag-search-insert-header (keyword-list nodes)
  "Insert search results header information."
  (insert (propertize (format "SuperTag Search Results: '%s'" (string-join keyword-list " "))
                      'face '(:height 1.5 :weight bold)))
  (insert (format "\nFound %d matching nodes.\n\n" (length nodes)))
  (insert (propertize "Operations:\n" 'face '(:weight bold)))
  (insert " [n/p] Navigate [SPC] Toggle Mark [RET] Visit Node [i] Insert Links\n")
  (insert " [e f] Export to File [e n] Export to New File [q] Quit\n\n"))

(defun supertag-search-show-results (keyword-list nodes)
  "Display search results in a card-based layout."
  (let ((buf (get-buffer-create "*Org SuperTag Search*"))
        (card-width 80))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-supertag-search-mode 1)
        (setq-local supertag-search--marked-nodes nil)
        (supertag-search-insert-header keyword-list nodes)
        (if (not nodes)
            (insert "  No matching nodes found.\n")
          (dolist (result-pair nodes)
            (let* ((node (car result-pair))
                   (context (cdr result-pair))
                   (node-id (plist-get node :id))
                   (card-lines (supertag-search--format-card
                                node context card-width
                                (member node-id supertag-search--marked-nodes))))
              (let ((start (point)))
                (dolist (line card-lines)
                  (insert line "\n"))
                (add-text-properties start (point) `(result-pair ,result-pair
                                                               node-id ,node-id)))
              (insert "\n")))))
      ;; Highlight first result
      (when nodes
        (goto-char (point-min))
        (re-search-forward "^┌" nil t)
        (beginning-of-line)
        (supertag-search-highlight-current)))
    (switch-to-buffer buf)))

;;; --- Navigation Functions ---

(defun supertag-search-highlight-current ()
  "Highlight the current result card."
  (remove-overlays (point-min) (point-max) 'org-supertag-search t)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "┌")
      (let ((beg (point))
            (end (save-excursion
                   (re-search-forward "^└" nil t)
                   (line-end-position))))
        (when end
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'face 'org-supertag-search-current)
            (overlay-put ov 'org-supertag-search t)))))))

(defun supertag-search-next ()
  "Jump to the next result card."
  (interactive)
  (let ((p (point)))
    (let ((search-start-point
           (save-excursion
             (beginning-of-line)
             (unless (looking-at "┌")
               (re-search-backward "^┌" nil t))
             (when (re-search-forward "^└" nil t)
               (point)))))
      (if (and search-start-point
               (save-excursion (goto-char search-start-point)
                               (re-search-forward "^┌" nil t)))
          (progn
            (goto-char (match-beginning 0))
            (supertag-search-highlight-current))
        (goto-char p)))))

(defun supertag-search-prev ()
  "Jump to the previous result card."
  (interactive)
  (let ((p (point)))
    (let ((current-card-start
           (save-excursion
             (beginning-of-line)
             (if (looking-at "┌")
                 (point)
               (re-search-backward "^┌" nil t)))))
      (if (and current-card-start (> current-card-start (point-min)))
          (if (save-excursion
                (goto-char (1- current-card-start))
                (re-search-backward "^┌" nil t))
              (progn
                (goto-char (match-beginning 0))
                (supertag-search-highlight-current))
            (goto-char p))
        (goto-char p)))))

(defun supertag-search-toggle-mark ()
  "Toggle the marked state of the current card and redraw it."
  (interactive)
  (when-let* ((node-id (get-text-property (point) 'node-id))
              (result-pair (get-text-property (point) 'result-pair)))
    (if (member node-id supertag-search--marked-nodes)
        (setq supertag-search--marked-nodes (remove node-id supertag-search--marked-nodes))
      (push node-id supertag-search--marked-nodes))
    (let ((inhibit-read-only t) (p (point)) (card-width 80))
      (save-excursion
        (let* ((beg (save-excursion (beginning-of-line) (if (looking-at "┌")
                                                             (point) (re-search-backward "^┌" nil t))))
               (end (when beg (save-excursion (goto-char beg) (when
                                                                 (re-search-forward "^└" nil t) (end-of-line) (forward-char 1) (point))))))
          (when (and beg end)
            (delete-region beg end)
            (goto-char beg)
            (let* ((node (car result-pair)) (context (cdr result-pair))
                   (card-lines (supertag-search--format-card node context
                                                                card-width (member node-id supertag-search--marked-nodes))))
              (dolist (line card-lines) (insert line "\n"))
              (add-text-properties beg (- (point) 1) `(result-pair ,result-pair
                                                                    node-id ,node-id))))))
      (goto-char p)
      (supertag-search-highlight-current))))

(defun supertag-search--find-node (node-id)
  "Visit node with specified ID."
  (let ((node-data (supertag-get (list :nodes node-id))))
    (if node-data
        (let ((file-path (plist-get node-data :file)))
          (if (and file-path (file-exists-p file-path))
              (progn
                (find-file file-path)
                (goto-char (point-min))
                (if (re-search-forward (format ":ID: *%s" (regexp-quote node-id)) nil t)
                    (progn
                      (org-back-to-heading t)
                      (org-show-context)
                      t)
                  (message "Node %s found in file but ID not located" node-id)
                  t))
            (message "File not found: %s" file-path)
            nil))
      (message "Node %s not found in database" node-id)
      nil)))

(defun supertag-search-visit-node ()
  "Visit the current selected node."
  (interactive)
  (when-let* ((id (get-text-property (point) 'node-id)))
    (supertag-search--find-node id)))

(defun supertag-search-quit ()
  "Quit the search results buffer and return to previous buffer."
  (interactive)
  (let ((results-buffer (get-buffer "*Org SuperTag Search*")))
    (when (and supertag-search--original-buffer
               (buffer-live-p supertag-search--original-buffer))
      (switch-to-buffer supertag-search--original-buffer)
      (when supertag-search--original-point
        (goto-char supertag-search--original-point)))
    (when results-buffer
      (kill-buffer results-buffer))))

;;; --- Export Functions ---

(defun supertag-search-get-selected-nodes ()
  "Get all marked node IDs from the search buffer."
  (with-current-buffer (get-buffer "*Org SuperTag Search*")
    (let ((selected-ids (cl-copy-list supertag-search--marked-nodes)))
      (when selected-ids
        (message "Found %d selected nodes" (length selected-ids)))
      (nreverse selected-ids))))

(defun supertag-search-insert-at-point ()
  "Insert selected nodes at saved cursor position."
  (interactive)
  (when-let* ((selected-nodes (supertag-search-get-selected-nodes))
              (orig-buf supertag-search--original-buffer)
              (orig-point supertag-search--original-point))
    (if (not selected-nodes)
        (message "No nodes selected")
      (with-current-buffer orig-buf
        (save-excursion
          (goto-char orig-point)
          (let ((content
                 (with-temp-buffer
                   (dolist (node-id selected-nodes)
                     (when-let* ((node-data (supertag-get (list :nodes node-id)))
                                (title (plist-get node-data :title))
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
      (kill-buffer))))

(defun supertag-search-export-results-to-new-file ()
  "Export selected search results as links to new file."
  (interactive)
  (let ((selected-nodes (supertag-search-get-selected-nodes)))
    (if (not selected-nodes)
        (message "No items selected")
      (let* ((default-name "export.org")
             (file (read-file-name 
                   "Export to new file: " 
                   nil nil nil 
                   default-name)))
        (unless (string-match-p "\\.org$" file)
          (error "Export target must be an org file: %s" file))
        (when (and (file-exists-p file)
                  (not (y-or-n-p 
                        (format "File %s exists. Overwrite? " file))))
          (error "Export cancelled by user"))
        
        (with-current-buffer (find-file-noselect file)
          (erase-buffer)
          (org-mode)
          (let ((title (file-name-base file)))
            (insert (format "#+TITLE: %s\n" title)
                    "#+OPTIONS: ^:nil\n"
            "#+STARTUP: showeverything\n\n"
            "* Search Results\n\n"))
          (dolist (node-id selected-nodes)
            (when-let* ((node-data (supertag-get (list :nodes node-id)))
                       (title (plist-get node-data :title))
                       (clean-title (if (stringp title)
                                       (substring-no-properties title)
                                     (prin1-to-string title))))
              (insert (format "- [[id:%s][%s]]\n" node-id clean-title))))
          (save-buffer)
          (find-file file)
          (message "Export of %d links completed successfully to %s" 
                  (length selected-nodes) file))))))

(defun supertag-search-export-results-to-file ()
  "Export selected search results to specified file location."
  (interactive)
  (message "Export to file functionality not yet implemented in new version"))

(defun supertag-search-get-history (&optional limit)
  "Get search history, optionally limited to LIMIT entries."
  (if limit
      (seq-take supertag-search--history limit)
    supertag-search--history))

;;; --- Initialization ---

;; Hook to load history on startup
(add-hook 'after-init-hook #'supertag-search--load-history)

;;; --- Backward Compatibility ---

;; Create alias for backward compatibility
(defalias 'org-supertag-query 'supertag-search
  "Backward compatibility alias for the interactive search command.")

;; Ensure the alias is marked as interactive
(put 'org-supertag-query 'interactive-form
     (interactive-form 'supertag-search))

;;; --- Main Search Function ---

(defun supertag-search ()
  "Interactive search across all indexed nodes using the new search system.
This is the main entry point for the renamed search functionality."
  (interactive)
  ;; Save current position
  (setq supertag-search--original-buffer (current-buffer)
        supertag-search--original-point (point))
  
  (unless supertag--store
    (message "Supertag store not initialized. Please run supertag sync first.")
    (user-error "Supertag store not initialized"))
  
  ;; Load history
  (supertag-search--load-history)
  
  (let* ((keywords (supertag-search--get-keywords))
         (nodes (supertag-search-find-nodes keywords)))
    (supertag-search-show-results keywords nodes)))

(provide 'supertag-ui-search)

;;; supertag-ui-search.el ends here
