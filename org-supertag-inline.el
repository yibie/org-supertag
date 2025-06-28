;;; org-supertag-inline.el --- Support for inline tags in org-mode content -*- lexical-binding: t; -*-

;;; Code:

(require 'org)
(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-tag)
(require 'seq)  ; For seq-remove function

;;;----------------------------------------------------------------------
;;; Compatibility Functions
;;;----------------------------------------------------------------------

;; Compatibility function for org-at-commented-p
(unless (fboundp 'org-at-commented-p)
  (defun org-at-commented-p ()
    "Check if point is at a commented line."
    (save-excursion
      (beginning-of-line)
      (looking-at-p "^[ \t]*#\\+"))))

;;;----------------------------------------------------------------------
;;; Compatibility Functions
;;;----------------------------------------------------------------------

;; Ensure string-trim is available (for Emacs < 24.4 compatibility)
(unless (fboundp 'string-trim)
  (defun string-trim (string)
    "Remove leading and trailing whitespace from STRING."
    (replace-regexp-in-string "\\`[ \t\n\r]+" ""
                              (replace-regexp-in-string "[ \t\n\r]+\\'" "" string))))

(defconst org-supertag-inline--valid-tag-chars "a-zA-Z0-9_@#%\\u4e00-\\u9fff-"
  "The set of characters considered valid for an inline supertag.
This is used to construct regular expressions and includes ASCII
alphanumeric characters and a basic CJK range for unicode support.")

(defgroup org-supertag-inline-style nil
  "Customization options for org-supertag inline tag styling."
  :group 'org-supertag)

(defcustom org-supertag-inline-manual-insert-add-newline nil
  "When non-nil, add a newline after manually inserting an inline tag.
This is useful for workflows where tags are expected to be on their own line,
such as with the auto-tagging system."
  :type 'boolean
  :group 'org-supertag-inline-style)

;; Define the face for inline tags
(defface org-supertag-inline-face
  '((t :weight bold :foreground "deeppink"))
  "Face for org-supertag inline tags."
  :group 'org-supertag-inline-style)

;; Compose font-lock keywords for highlighting inline tags
(defvar org-supertag-inline-font-lock-keywords
  `((,(concat "#[" org-supertag-inline--valid-tag-chars "]+")
     (0 (if (and (not (org-in-src-block-p))
                 (not (org-at-table-p))
                 (not (org-at-commented-p))
                 (not (eq (get-text-property (match-beginning 0) 'face) 'org-verbatim)))
            'org-supertag-inline-face) t)))
  "Font-lock keywords for highlighting inline tags.")

;;;###autoload
(define-minor-mode org-supertag-inline-style-mode
  "Minor mode for styling org-supertag inline tags."
  :lighter " Tag-Style"
  (if org-supertag-inline-style-mode
      (progn
        (font-lock-add-keywords nil org-supertag-inline-font-lock-keywords t)
        (if (fboundp 'font-lock-flush)
            (font-lock-flush)
          (font-lock-fontify-buffer)))
    (font-lock-remove-keywords nil org-supertag-inline-font-lock-keywords)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (font-lock-fontify-buffer))))

;; Enable the minor mode in org buffers
(add-hook 'org-mode-hook 'org-supertag-inline-style-mode)

;; Override the tag insertion function to apply styling immediately
(advice-add 'org-supertag-inline-insert-tag :after
            (lambda (&rest _)
              (font-lock-flush)))

;;----------------------------------------------------------------------
;; Helper functions for inline tag insertion
;;----------------------------------------------------------------------
(defun org-supertag-inline--read-tag-name ()
  "Read tag name from user input with completion.
Returns the selected or entered tag name."
  (let* ((all-tags (org-supertag-get-all-tags))
         (preset-names (mapcar #'car org-supertag-preset-tags))
         ;; Use seq-remove instead of remove-if for compatibility
         (user-tags (seq-remove (lambda (tag) (member tag preset-names)) all-tags))
         (candidates (delete-dups
                     (append 
                      ;; Use [P] prefix to mark preset tags
                      (mapcar (lambda (name) (format "[P] %s" name)) preset-names)
                      ;; Regular tags are kept as is
                      user-tags)))
         ;; Get region content if actually active and non-empty
         (region-text (when (and (use-region-p) 
                                (region-active-p)
                                (/= (region-beginning) (region-end)))
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))))
         (input (completing-read "Inline tag: " candidates nil nil region-text)))
    ;; Process input, remove [P] prefix
    (if (string-prefix-p "[P] " input)
        (substring input 4)
      input)))

(defun org-supertag-inline--analyze-context ()
  "Analyze the current context for tag insertion.
Returns a plist with context information:
- :context - the org element context
- :type - the context type
- :in-drawer - whether we're in a drawer
- :node-id - the current node ID (if any)"
  (let* ((context (org-element-context))
         (context-type (org-element-type context))
         (in-drawer (or (eq context-type 'drawer)
                       (eq context-type 'property-drawer)
                       (eq context-type 'node-property)
                       (and (eq context-type 'keyword)
                            (string= (org-element-property :key context) "END"))))
         ;; ID lookup logic
         (force-id (and (boundp 'org-supertag-force-node-id)
                       org-supertag-force-node-id))
         (existing-id (or force-id
                         (org-entry-get nil "ID")
                         (save-excursion
                           (ignore-errors
                             (org-back-to-heading t)
                             (org-entry-get nil "ID")))))
         (node-id (or existing-id
                     (save-excursion
                       (when (ignore-errors (org-back-to-heading t))
                         (org-supertag-node-create))))))
    (list :context context
          :type context-type
          :in-drawer in-drawer
          :node-id node-id)))

(defun org-supertag-inline--ensure-tag (tag-name)
  "Ensure a tag exists, creating it if necessary.
TAG-NAME is the desired tag name.
Returns a plist with :tag-id and :tag-name."
  ;; Validate input first
  (when (or (null tag-name) (string-empty-p (string-trim tag-name)))
    (user-error "Tag name cannot be empty"))
  
  (let* ((direct-create (string-suffix-p "#" tag-name))
         (tag-name-clean (if direct-create
                            (substring tag-name 0 -1)
                          tag-name))
         (sanitized-name (org-supertag-sanitize-tag-name tag-name-clean)))
    
    ;; Validate sanitized name is not empty
    (when (or (null sanitized-name) (string-empty-p sanitized-name))
      (user-error "Invalid tag name after sanitization: '%s'" tag-name))
    
    (let ((tag-id (cond
                   ;; If tag exists, get its ID
                   ((org-supertag-tag-exists-p sanitized-name)
                    (org-supertag-tag-get-id-by-name sanitized-name))
                   ;; Otherwise create a new tag
                   (t
                    (if (or direct-create
                            (y-or-n-p (format "Create new tag '%s'? " sanitized-name)))
                        (org-supertag-tag--create sanitized-name)
                      (user-error "Tag creation cancelled"))))))
      ;; Return both tag-id and tag-name
      (list :tag-id tag-id :tag-name sanitized-name))))

(defun org-supertag-inline--adjust-position (context-info)
  "Adjust cursor position based on context with intelligent positioning.
CONTEXT-INFO is the context analysis from `org-supertag-inline--analyze-context'.
Handles region deletion and smart drawer/content positioning."
  ;; Delete region ONLY if there's an actual visible region
  (when (and (use-region-p) 
             (region-active-p)
             (/= (region-beginning) (region-end)))
    (let ((region-start (region-beginning))
          (region-end (region-end)))
      (message "Deleting region from %d to %d" region-start region-end)
      (delete-region region-start region-end)))
  
  ;; For manual insertion, don't adjust position - stay at cursor
  ;; This ensures manual tags are inserted exactly where the user wants them
  )

(defun org-supertag-inline--smart-position-for-insertion ()
  "Smartly position the cursor for inline tag insertion.
Rules:
- If no drawer, create a new line below the heading or find an existing tag line
- If there's a drawer, position below the drawer
- If there's an existing tag line, move to the end of the tag line
- If already in content, create a new line above the content"
  (org-back-to-heading t)
  (let ((drawer-end (org-supertag-inline--find-drawer-end))
        (existing-tag-line (org-supertag-inline--find-existing-tag-line)))
    (cond
     (existing-tag-line
      (goto-char existing-tag-line)
      (end-of-line))
     (drawer-end
      (goto-char drawer-end)
      (while (and (not (eobp))
                 (not (org-at-heading-p))
                 (looking-at-p "^[ \t]*$"))
        (forward-line 1))
      (if (and (not (eobp))
              (not (org-at-heading-p))
              (not (looking-at-p "^[ \t]*$")))
          (progn
            (beginning-of-line)
            (open-line 1))
        (unless (bolp) (insert "\n"))))
     (t
      (end-of-line)
      (insert "\n")))))

(defun org-supertag-inline--find-drawer-end ()
  "Find the end position of the current node's drawer."
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    (let ((end-pos nil)
          (section-end (save-excursion
                        (org-end-of-subtree t t)
                        (point))))
      ;; Find the end position of the last drawer
      (while (and (< (point) section-end)
                 (not (org-at-heading-p)))
        (let ((element (org-element-at-point)))
          (cond
           ;; Property drawer
           ((eq (org-element-type element) 'property-drawer)
            (setq end-pos (org-element-property :end element))
            (goto-char end-pos))
           ;; Other drawers
           ((eq (org-element-type element) 'drawer)
            (setq end-pos (org-element-property :end element))
            (goto-char end-pos))
           ;; Other elements, continue forward
           (t
            (forward-line 1)))))
      end-pos)))

(defun org-supertag-inline--find-existing-tag-line ()
  "Find the position of an existing tag line in the current node.
Returns the tag line position, or nil if not found."
  (save-excursion
    (org-back-to-heading t)
    (let ((drawer-end (org-supertag-inline--find-drawer-end))
          (section-end (save-excursion
                        (org-end-of-subtree t t)
                        (point)))
          (tag-line-pos nil))
      ;; Start searching from the drawer end position, or from the heading if no drawer
      (goto-char (or drawer-end (progn (end-of-line) (point))))
      (forward-line 1)
      
      ;; Search for lines containing #tags within the node range
      (while (and (< (point) section-end)
                 (not (org-at-heading-p))
                 (not tag-line-pos))
        (beginning-of-line)
        ;; Check if the current line contains #tags (but not comment lines)
        (when (and (looking-at-p (concat "^[ \t]*#[" org-supertag-inline--valid-tag-chars "]"))
                  (not (looking-at-p "^[ \t]*#\\+")))  ; Exclude org keywords
          (setq tag-line-pos (point)))
        (forward-line 1))
      
      tag-line-pos)))

(defun org-supertag-inline--insert-tag-text (tag-name)
  "Insert the tag text with intelligent spacing.
TAG-NAME is the tag name to display (not the UUID).

Smart spacing rules:
- Add space before tag if previous char exists and is not whitespace
- Add space after tag for future tags (unless at line end with content after)
- Special handling for tag lines to ensure proper spacing"
  (let* ((prev-char (char-before))
         (next-char (char-after))
         (at-tag-line (org-supertag-inline--at-tag-line-p))
         ;; Need space before if:
         ;; 1. Previous character exists AND
         ;; 2. Previous char is not whitespace AND
         ;; 3. We're at a tag line OR previous char is alphanumeric/tag-related
         (need-space-before 
          (and prev-char
               (not (eq prev-char ?\s))
               (not (eq prev-char ?\t))
               (not (eq prev-char ?\n))
               (or at-tag-line
                   (and (>= prev-char ?a) (<= prev-char ?z))
                   (and (>= prev-char ?A) (<= prev-char ?Z))
                   (and (>= prev-char ?0) (<= prev-char ?9))
                   (and (>= prev-char ?\u4e00) (<= prev-char ?\u9fff)))))
         ;; Need space after ONLY if:
         ;; 1. next character exists AND is alphanumeric
         ;; 2. AND we're not at line end (next char is not newline)
         (need-space-after
          (and next-char
               (not (eq next-char ?\n))  ; Never add space before newline
               (or (and (>= next-char ?a) (<= next-char ?z))
                   (and (>= next-char ?A) (<= next-char ?Z))
                   (and (>= next-char ?0) (<= next-char ?9))))))
    
    ;; Insert space before tag if needed
    (when need-space-before
      (insert " "))
    
    ;; Insert the tag with tag-name (visible to user)
    (insert (concat "#" tag-name))
    
    ;; Insert space after tag if needed (but not newline)
    (when need-space-after
      (insert " "))))

(defun org-supertag-inline--at-tag-line-p ()
  "Check if the current line is a tag line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p (concat "^[ \t]*#[" org-supertag-inline--valid-tag-chars "]+"))))

(defun org-supertag-inline--establish-relationship (tag-result node-id)
  "Establish relationship between tag and node.
TAG-RESULT is a plist with :tag-id and :tag-name from org-supertag-inline--ensure-tag.
NODE-ID is the node identifier.
Only creates the relationship if NODE-ID is not nil."
  (when node-id
    (let ((tag-id (plist-get tag-result :tag-id))
          (org-supertag-tag-apply-skip-headline t)
          (org-supertag-force-node-id node-id))
      (org-supertag-tag-apply tag-id))))

;;;###autoload
(defun org-supertag-inline-insert-tag (tag-name &optional pos)
  "Insert an inline tag at POS, or at the current point.
TAG-NAME: The name of the tag to insert (without '#').
POS: The position to insert the tag. If nil, insert at point.
The smart positioning logic has been removed to simplify the flow.
The caller, `org-supertag-tag-apply`, is now responsible for positioning."
  (let ((text-to-insert (concat " #" tag-name " ")))
    (if pos (goto-char pos))
    (insert text-to-insert)))

(defun org-supertag-inline-insert-tag-for-autotag (node-id tag-name)
  "Insert a tag for the auto-tag system, positioning it smartly.
This function is for programmatic use by the auto-tag engine and
will typically place the tag on a new line below the headline properties."
  (when (and node-id tag-name)
    (when-let* ((node-props (org-supertag-db-get node-id))
                (file-path (plist-get node-props :file))
                (pos (plist-get node-props :pos)))
      (with-current-buffer (find-file-noselect file-path)
        (goto-char pos)
        ;; Use the smart positioning logic for auto-tag insertion
        (org-supertag-inline--smart-position-for-insertion)
        ;; Create tag and insert
        (let* ((display-name (org-supertag-sanitize-tag-name tag-name))
               (tag-entity (or (org-supertag-tag-get display-name)
                               (progn
                                 (org-supertag-tag-create display-name)
                                 (org-supertag-tag-get display-name)))))
          (org-supertag-inline--insert-tag-text display-name)
          (when tag-entity
            (org-supertag-node-db-add-tag node-id (plist-get tag-entity :id)))
          (message "Auto-inserted tag #%s for node %s" display-name node-id))))))

(defun org-supertag-inline-insert-tags-batch (tag-names)
  "Insert multiple inline tags, respecting the user's choice for newlines."
  (interactive (list (org-supertag-inline--read-multiple-tags)))
  (when tag-names
    (let* ((node-id (org-id-get-create))
           (processed-tags (mapcar #'org-supertag-tag-get-or-create tag-names)))

      ;; Do not reposition the cursor for manual batch insertion.
      ;; The insertion will happen at the user's cursor position.
      ;; (org-supertag-inline--position-for-batch-insert)

      ;; Insert tags in batch
      (org-supertag-inline--insert-tags-batch processed-tags)

      ;; Establish relationships
      (dolist (tag-result processed-tags)
        (org-supertag-node-db-add-tag node-id (plist-get tag-result :id)))

      ;; Add newline only if explicitly configured
      (when org-supertag-inline-manual-insert-add-newline
        (insert "\n"))

      (message "Inserted %d tags: %s"
               (length processed-tags)
               (mapconcat (lambda (r) (plist-get r :tag-name))
                          processed-tags ", ")))))

(defun org-supertag-inline--insert-tags-batch (tag-results)
  "Insert multiple tags in a single line.
TAG-RESULTS is a list of plists, each from `org-supertag-tag-get-or-create'."
  (let ((first t))
    (dolist (tag-result tag-results)
      (unless first
        (insert " ")) ; Space between tags
      (org-supertag-inline--insert-tag-text (plist-get tag-result :tag-name))
      (setq first nil))))

;; The function org-supertag-inline-insert-tag-for-node is now obsolete
;; and replaced by org-supertag-inline-insert-tag-for-autotag
;; (defun org-supertag-inline-insert-tag-for-node (node-id tag-name) ... )

;; The function org-supertag-inline-insert-tag-no-newline is now obsolete
;; as its logic will be merged into the main org-supertag-inline-insert-tag.
;; (defun org-supertag-inline-insert-tag-no-newline (tag-name) ... )

(defun org-supertag-inline-remove-tag-at-point (tag-id)
  "Remove an inline tag at or before point.
TAG-ID is the tag name to remove."
  (let ((tag-pattern (concat "#" (regexp-quote tag-id))))
    (save-excursion
      (when (re-search-backward tag-pattern nil t)
        (replace-match "")))))

;;----------------------------------------------------------------------
;; Interactive Commands
;;----------------------------------------------------------------------

(defun org-supertag-inline-add ()
  "Interactively add a supertag to the current node.
This command handles both database relations and text insertion."
  (interactive)
  (message "DEBUG: org-supertag-inline-add called")
  (let* ((context (org-supertag-inline--analyze-context))
         (node-id (plist-get context :node-id))
         (tag-name-raw (org-supertag-inline--read-tag-name))
         (tag-info (org-supertag-inline--ensure-tag tag-name-raw))
         (tag-id (plist-get tag-info :tag-id)))
    (message "DEBUG: node-id=%s, tag-id=%s" node-id tag-id)
    (when (and node-id tag-id)
      ;; The single call to the unified apply function.
      ;; Text insertion is handled within `org-supertag-tag-apply`.
      (let ((org-supertag-force-node-id node-id))
        (org-supertag-tag-apply tag-id))
      (message "Tag '%s' applied to node %s." tag-id node-id))))
(defalias 'org-supertag-tag-add-tag 'org-supertag-inline-add)

(defun org-supertag-inline-remove ()
  "Interactively remove a supertag from the current node."
  (interactive)
  (let* ((node-id (org-id-get))
         (tags (when node-id (org-supertag-node-get-tags node-id)))
         (tag-to-remove (completing-read "Remove tag: " tags nil t)))
    (when (and node-id (not (string-empty-p tag-to-remove)))
      ;; 1. Update database
      (org-supertag-db-remove-link :node-tag node-id tag-to-remove)
      ;; 2. Update buffer
      (org-supertag-inline-remove-tag-at-point tag-to-remove)
      (message "Tag '%s' removed from node %s." tag-to-remove node-id))))
(defalias 'org-supertag-tag-remove 'org-supertag-inline-remove)

(defun org-supertag-inline-delete-all ()
  "Interactively delete a tag definition and all its instances.
This removes the tag from the database and from all org files."
  (interactive)
  (let* ((all-tags (org-supertag-get-all-tags))
         (tag-id (completing-read "Delete tag permanently: " all-tags nil t)))
    (when (and (not (string-empty-p tag-id))
               (yes-or-no-p (format "DELETE tag '%s' and all its uses? This is irreversible. " tag-id)))
      ;; First, find all nodes before deleting from DB
      (let ((nodes (org-supertag-db-get-nodes-by-tag tag-id)))
        ;; 1. Delete from database (this is the function from org-supertag-tag.el)
        (org-supertag-tag--delete-at-all tag-id)
        
        ;; 2. Remove from all org buffers
        (message "Removing inline tags from buffers...")
        (let ((files (delete-dups (mapcar #'org-supertag-db-get-node-file nodes))))
          (dolist (file files)
            (when (file-exists-p file)
              (with-current-buffer (find-file-noselect file)
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward (concat "#" (regexp-quote tag-id)) nil t)
                    (replace-match ""))))))
          (message "Completed buffer cleanup for tag '%s'." tag-id))))))

(defun org-supertag-inline-rename ()
  "Interactively rename a tag across all files."
  (interactive)
  (let* ((all-tags (org-supertag-get-all-tags))
         (old-name (completing-read "Tag to rename: " all-tags nil t))
         (new-name (read-string (format "New name for '%s': " old-name))))
    (when (and (not (string-empty-p old-name))
               (not (string-empty-p new-name)))
      ;; 1. Update database first
      (org-supertag-tag--rename old-name new-name)
      
      ;; 2. Update all buffers
      (message "Renaming tag in buffers...")
      (let* ((nodes (org-supertag-db-get-nodes-by-tag new-name)) ; Get nodes by new name
             (files (delete-dups (mapcar #'org-supertag-db-get-node-file nodes))))
        (dolist (file files)
          (when (file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward (concat "#" (regexp-quote old-name)) nil t)
                  (replace-match (concat "#" new-name))))
              (save-buffer))))
        (message "Finished renaming tag '%s' to '%s' in %d files."
                 old-name new-name (length files))))))
(defalias 'org-supertag-tag-rename 'org-supertag-inline-rename)

(defun org-supertag-inline-change-tag ()
  "Interactively change a tag on the current node."
  (interactive)
  (let* ((node-id (org-id-get))
         (current-tags (org-supertag-node-get-tags node-id))
         (source-tag (completing-read "Select tag to change: " current-tags nil t))
         (target-tag-raw (read-string "Change to new tag name: ")))
    (when (and node-id
               (not (string-empty-p source-tag))
               (not (string-empty-p target-tag-raw)))
      (let* ((tag-info (org-supertag-inline--ensure-tag target-tag-raw))
             (target-tag-id (plist-get tag-info :tag-id)))
        ;; 1. Update database
        (org-supertag-db-remove-link :node-tag node-id source-tag)
        (org-supertag-node-db-add-tag node-id target-tag-id)
        ;; 2. Update buffer
        (org-supertag-inline-remove-tag-at-point source-tag)
        (org-supertag-inline-insert-tag target-tag-id)
        (message "Changed tag '%s' to '%s' on node %s."
                 source-tag target-tag-id node-id)))))
(defalias 'org-supertag-tag-change-tag 'org-supertag-inline-change-tag)

(defun org-supertag-debug-force-refontify ()
  "DEBUG: Force removal of old face properties and re-fontify.
This is a temporary command to fix stale highlighting."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (concat "#[" org-supertag-inline--valid-tag-chars "]+") nil t)
      (remove-text-properties (match-beginning 0) (match-end 0) '(face t))))
  (font-lock-fontify-buffer)
  (message "Forced re-fontification complete."))

(provide 'org-supertag-inline)
;;; org-supertag-inline.el ends here 
