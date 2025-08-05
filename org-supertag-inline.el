;;; org-supertag-inline.el --- Support for inline tags in org-mode content -*- lexical-binding: t; -*-

;;; Code:

(require 'org)
(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-tag)
(require 'org-supertag-relation nil t)
(require 'org-supertag-util)
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

(defconst org-supertag-inline--valid-tag-chars "a-zA-Z0-9_@%一-龥-"
  "The set of characters considered valid for an inline supertag.
This is used to construct regular expressions and includes ASCII
alphanumeric characters and a direct UTF-8 CJK range for max compatibility.")

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
     (0 (if (and (not (org-supertag-safe-org-in-src-block-p))
                 (not (org-supertag-safe-org-at-table-p))
                 (not (org-supertag-safe-org-at-commented-p))
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
  (let* ((candidates (org-supertag-get-all-tags))
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
- Position cursor at the beginning of a new line below the headline
- If there's a property drawer, position below the drawer
- If there's an existing tag line, move to the end of that line
- Always ensure we're on a separate line, not on the headline itself
- Ensure there's a blank line between tags and content"
  (org-back-to-heading t)
  (let ((drawer-end (org-supertag-inline--find-drawer-end))
        (existing-tag-line (org-supertag-inline--find-existing-tag-line)))
    (cond
     ;; If there's already a tag line, position at the end of it
     (existing-tag-line
      (goto-char existing-tag-line)
      (end-of-line))
     
     ;; If there's a drawer, position after it
     (drawer-end
      (goto-char drawer-end)
      ;; Ensure we are at the beginning of a line after drawer
      (unless (bolp) (forward-line 1))
      ;; Skip any empty lines after drawer
      (while (and (not (eobp))
                  (not (org-at-heading-p))
                  (looking-at-p "^[ \t]*$"))
        (forward-line 1))
      ;; Always position at the beginning of line for tag insertion
      (beginning-of-line))
     
     ;; No drawer, position directly after headline
     (t
      (end-of-line)
      (insert "\n")
      (beginning-of-line)))))

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

(defun org-supertag-inline--insert-tag-for-autotag (tag-name)
  "Insert tag for auto-tag system with improved formatting.
TAG-NAME is the tag name to insert.
This function ensures proper spacing and formatting for tags."
  ;; Check if we're at the beginning of a line
  (unless (bolp)
    (insert "\n"))
  
  ;; Insert the tag
  (insert "#" tag-name)
  
  ;; Ensure we end with a newline for proper separation from content
  (unless (eolp)
    (insert "\n"))
  
  ;; Add an extra newline if the next line is not empty and not a tag line
  (forward-line 1)
  (unless (or (eobp)
              (looking-at-p "^[ \t]*$")
              (looking-at-p (concat "^[ \t]*#[" org-supertag-inline--valid-tag-chars "]+"))
              (org-at-heading-p))
    (insert "\n"))
  (forward-line -1))

(defun org-supertag-inline--insert-multiple-tags-for-autotag (tag-names)
  "Insert multiple tags for auto-tag system with proper formatting.
TAG-NAMES is a list of tag names to insert.
This function ensures all tags are properly spaced and formatted."
  (when tag-names
    ;; Check if we're at the beginning of a line
    (unless (bolp)
      (insert "\n"))
    
    ;; Insert all tags with proper spacing
    (let ((first t))
      (dolist (tag-name tag-names)
        (unless first
          (insert " ")) ; Space between tags
        (insert "#" tag-name)
        (setq first nil)))
    
    ;; Ensure we end with a newline for proper separation from content
    (unless (eolp)
      (insert "\n"))
    
    ;; Add an extra newline if the next line is not empty and not a tag line
    (forward-line 1)
    (unless (or (eobp)
                (looking-at-p "^[ \t]*$")
                (looking-at-p (concat "^[ \t]*#[" org-supertag-inline--valid-tag-chars "]+"))
                (org-at-heading-p))
      (insert "\n"))
    (forward-line -1)))

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
This function uses our custom node location finder to avoid dependency on org-id-find."
  (when (and node-id tag-name)
    (let ((found-location (org-supertag-find-node-location node-id)))
      (if found-location
          (let ((pos (car found-location))
                (file-path (cdr found-location)))
            (with-current-buffer (find-file-noselect file-path)
              (save-excursion
                (goto-char pos)
                (org-back-to-heading t)
                ;; Use the smart positioning logic for tag insertion
                (org-supertag-inline--smart-position-for-insertion)
                ;; Create tag and insert using the new batch function
                (let* ((display-name (org-supertag-sanitize-tag-name tag-name))
                       (tag-entity (or (org-supertag-tag-get display-name)
                                       (progn
                                         (org-supertag-tag--create display-name)
                                         (org-supertag-tag-get display-name)))))
                  ;; Insert tag with improved formatting
                  (org-supertag-inline--insert-multiple-tags-for-autotag (list display-name))
                  (when tag-entity
                    (org-supertag-node-db-add-tag node-id (plist-get tag-entity :id)))
                  ;; Save the buffer to ensure tag is written to file
                  (save-buffer)
                  (message "Auto-inserted tag #%s for node %s at %s" display-name node-id file-path)))))
        (error "Could not find node with ID: %s" node-id)))))


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

(defun org-supertag-inline--remove-tag-in-current-node (tag)
  "Remove all occurrences of #TAG inside the current node (headline + content)."
  (save-excursion
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t) (point)))
          (regex (concat "#" (regexp-quote tag)))
          (case-fold-search nil))
      ;; Search headline line first (including todo / tags part)
      (beginning-of-line)
      (while (re-search-forward regex (line-end-position) t)
        (replace-match ""))
      ;; Now scan the subtree
      (forward-line 1)
      (while (re-search-forward regex subtree-end t)
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
  "Interactively remove a supertag from the current node and clean buffer text."
  (interactive)
  (let* ((node-id (org-id-get))
         (tags (when node-id (org-supertag-node-get-tags node-id)))
         (tag-to-remove (completing-read "Remove tag: " tags nil t)))
    (when (and node-id (not (string-empty-p tag-to-remove)))
      ;; 1. Update database first
      (org-supertag-db-remove-link :node-tag node-id tag-to-remove)
      ;; 2. Update co-occurrence relationships
      (when (featurep 'org-supertag-relation)
        (org-supertag-relation-unrecord-cooccurrence node-id tag-to-remove))
      ;; 3. Update buffer text: remove every occurrence within the node
      (org-supertag-inline--remove-tag-in-current-node tag-to-remove)
      (message "Tag '%s' removed from node %s." tag-to-remove node-id))))
(defalias 'org-supertag-tag-remove 'org-supertag-inline-remove)

(defun org-supertag-inline-delete-all ()
  "Interactively delete a tag definition and all its instances.
This removes the tag from the database and from all org files.
WARNING: This function may cause org-mode element structure issues.
Use org-supertag-safe-delete-all for safer deletion."
  (interactive)
  (let* ((all-tags (org-supertag-get-all-tags))
         (raw-input (completing-read "Delete tag permanently: " all-tags nil t))
         (clean-tag (org-supertag-sanitize-tag-name (substring-no-properties raw-input))))
    (when (and (not (string-empty-p clean-tag))
               (yes-or-no-p (format "DELETE tag '%s' and all its uses? This is irreversible. " clean-tag)))
      ;; First, find all nodes before deleting from DB
      (let ((nodes (org-supertag-db-get-tag-nodes clean-tag)))
        ;; 1. Delete from database (this is the function from org-supertag-tag.el)
        (org-supertag-tag--delete-at-all clean-tag)
        
        ;; 2. Safely remove from all org buffers using the safe method
        (message "Safely removing inline tags from buffers...")
        (let ((files (delete-dups (mapcar #'org-supertag-db-get-node-file nodes)))
              (total-deleted 0))
          (dolist (file files)
            (when (file-exists-p file)
              (with-current-buffer (find-file-noselect file)
                (save-excursion
                  (goto-char (point-min))
                  (let ((deleted-count 0))
                    ;; Use safer deletion method
                    (while (re-search-forward (concat "#" (regexp-quote clean-tag)) nil t)
                      (let ((start (match-beginning 0))
                            (end (match-end 0)))
                        ;; Only delete tags that are not in comments or code blocks
                        (unless (or (org-supertag-safe-org-in-src-block-p start)
                                   (save-excursion
                                     (goto-char start)
                                     (looking-at-p "^[ \t]*#\\+")))
                          ;; Analyze context and delete more intelligently
                          (let* ((prev-char (char-before start))
                                 (next-char (char-after end))
                                 (need-space-before (and prev-char
                                                       (not (eq prev-char ?\s))
                                                       (not (eq prev-char ?\t))
                                                       (not (eq prev-char ?\n))))
                                 (need-space-after (and next-char
                                                      (not (eq next-char ?\s))
                                                      (not (eq next-char ?\t))
                                                      (not (eq next-char ?\n))
                                                      (not (eq next-char ?\n))))
                                 (delete-start start)
                                 (delete-end end))
                            ;; Extend deletion range to include surrounding spaces if needed
                            (when need-space-before
                              (setq delete-start (1- delete-start)))
                            (when need-space-after
                              (setq delete-end (1+ delete-end)))
                            ;; Perform the deletion
                            (delete-region delete-start delete-end)
                            (setq deleted-count (1+ deleted-count))))))
                    (when (> deleted-count 0)
                      (save-buffer)
                      (setq total-deleted (+ total-deleted deleted-count))
                      (message "Deleted %d instances in %s" deleted-count file))))))
          (message "Completed safe buffer cleanup for tag '%s'. Total deleted: %d" 
                   clean-tag total-deleted)))))))

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
      (let* ((nodes (org-supertag-db-get-tag-nodes new-name)) ; Get nodes by new name
             (files (delete-dups (mapcar #'org-supertag-db-get-node-file nodes))))
        (dolist (file files)
          (when (file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (goto-char (point-min))
                (let ((renamed-count 0))
                  (while (re-search-forward (concat "#" (regexp-quote old-name)) nil t)
                    (let ((start (match-beginning 0))
                          (end (match-end 0)))
                      ;; Only rename tags that are not in comments or code blocks
                      (unless (or (org-supertag-safe-org-in-src-block-p start)
                                 (save-excursion
                                   (goto-char start)
                                   (looking-at-p "^[ \t]*#\\+")))
                        (replace-match (concat "#" new-name))
                        (setq renamed-count (1+ renamed-count)))))
                  (when (> renamed-count 0)
                    (save-buffer)
                    (message "Renamed %d instances in %s" renamed-count file))))))
        (message "Finished renaming tag '%s' to '%s' in %d files."
                 old-name new-name (length files)))))))

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

(defun org-supertag-inline-get-tag-at-point ()
  "Get the inline supertag name at point using the official regex.
The tag format is '#tagname'. This is a more robust version."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let* ((tag-re (and (boundp 'org-supertag-inline-font-lock-keywords)
                         (car (car org-supertag-inline-font-lock-keywords))))
             (start (point)))
        (when (and tag-re (re-search-backward tag-re nil t))
          ;; Check if the point is within the found match
          (when (and (<= (match-beginning 0) start)
                     (> (match-end 0) start))
            ;; Extract the tag name, removing the leading '#' 
            (substring (match-string 0) 1)))))))

;;;###autoload
(defun org-supertag-inline-relation-add-contextual ()
  "Intelligently add a relation based on context.
If the cursor is on a tag, use it as the source.
Otherwise, find the current node and its tags. If there is one tag,
use it. If there are multiple, prompt the user to select one."
  (interactive)
  (let* ((source-tag-name
          (or (org-supertag-inline-get-tag-at-point)
              (let* ((node-id (org-id-get))
                     (node-tags (and node-id (org-supertag-node-get-tags node-id))))
                (cond
                 ((= (length node-tags) 1) (car node-tags))
                   ((> (length node-tags) 1)
                    (completing-read "Select source tag for this node: " node-tags nil t))
                   (t nil))))))

    (if (not source-tag-name)
        (user-error "No source tag found at point or in the current node.")
        (let* ((from-tag-id (org-supertag-tag-get-id-by-name source-tag-name))
              (all-tags (org-supertag-get-all-tags))
               (target-candidates (remove source-tag-name all-tags))
               (other-tag-name (completing-read (format "Relate '%s' to: " source-tag-name) target-candidates nil t))
               (other-tag-id (and (not (string-empty-p other-tag-name))
                                  (org-supertag-tag-get-id-by-name other-tag-name))))
          (if other-tag-id
              (let* ((rel-choices (org-supertag-relation--get-relation-type-choices))
                     (choice (completing-read "Select relation type: " rel-choices nil t))
                     (rel-type (org-supertag-relation--get-type-from-choice choice)))
                (when (and from-tag-id other-tag-id rel-type)
                  (if (org-supertag-relation-has-complement-p rel-type)
                      (org-supertag-relation-add-with-complement from-tag-id other-tag-id rel-type)
                    (org-supertag-relation-add-relation from-tag-id other-tag-id rel-type)))
                  ;; Inform the user once the relation has been added
                  (message "Added relation: %s -[%s]-> %s" source-tag-name rel-type other-tag-name))
            (user-error "Target tag not found or is empty: %s" other-tag-name))))))

;;----------------------------------------------------------------------
;; Test Functions
;;----------------------------------------------------------------------
(defun org-supertag-inline-test-formatting ()
  "Test function to verify improved tag formatting.
This function creates a test node with multiple tags to demonstrate the improved formatting."
  (interactive)
  (let ((test-content "**** Test Node for Tag Formatting
:PROPERTIES:
:ID:       TEST-1234-5678-9ABC-DEF012345678
:END:

This is a test node to demonstrate improved tag formatting.

The tags should be properly spaced and separated from the content."))
    (with-current-buffer (get-buffer-create "*Tag Formatting Test*")
      (org-mode)
      (erase-buffer)
      (insert test-content)
      (goto-char (point-min))
      (org-back-to-heading t)
      (org-supertag-inline--smart-position-for-insertion)
      (org-supertag-inline--insert-multiple-tags-for-autotag '("test_tag" "formatting" "improved"))
      (message "Test completed. Check the buffer for improved tag formatting."))))

(provide 'org-supertag-inline)
;;; org-supertag-inline.el ends here 
