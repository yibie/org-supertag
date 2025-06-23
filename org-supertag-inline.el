;;; org-supertag-inline.el --- Support for inline tags in org-mode content -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024

;; Author: User <user@example.com>
;; Keywords: org-mode, tags, inline

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module adds support for inline tags within org-mode content.
;; Inline tags are prefixed with a '#' symbol (like #hashtags) and are
;; distinct from headline tags.
;;
;; Example:
;; "This is a paragraph with an #inline-tag that can be tracked and queried."

;;; Code:

(require 'org)
(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-tag)
(require 'seq)  ; For seq-remove function

;; Compatibility function for org-at-commented-p
(unless (fboundp 'org-at-commented-p)
  (defun org-at-commented-p ()
    "Check if point is at a commented line."
    (save-excursion
      (beginning-of-line)
      (looking-at-p "^[ \t]*#\\+"))))

;; Ensure string-trim is available (for Emacs < 24.4 compatibility)
(unless (fboundp 'string-trim)
  (defun string-trim (string)
    "Remove leading and trailing whitespace from STRING."
    (replace-regexp-in-string "\\`[ \t\n\r]+" ""
                              (replace-regexp-in-string "[ \t\n\r]+\\'" "" string))))

(defgroup org-supertag-inline-style nil
  "Customization options for org-supertag inline tag styling."
  :group 'org-supertag)

(defcustom org-supertag-inline-style-hide-prefix t
  "Whether to hide the '#' prefix of inline tags."
  :type 'boolean
  :group 'org-supertag-inline-style)

(defcustom org-supertag-inline-background 'unspecified
  "Background color for inline tags.
Can be:
- A color string (e.g. \"#e8f0ff\")
- 'unspecified for transparent
- nil for transparent"
  :type '(choice
          (const :tag "Transparent" unspecified)
          (const :tag "None" nil)
          (color :tag "Color"))
  :group 'org-supertag-inline-style)

(defcustom org-supertag-inline-foreground 'unspecified
  "Foreground color for inline tags.
Can be:
- A color string (e.g. \"#0066cc\")
- 'unspecified for transparent
- nil for transparent"
  :type '(choice
          (const :tag "Transparent" unspecified)
          (const :tag "None" nil)
          (color :tag "Color"))
  :group 'org-supertag-inline-style)

(defcustom org-supertag-inline-box '(:line-width 1 :color "#b0b0b0" :style nil)
  "Box properties for inline tags.
Can be t for a simple box, nil for no box, or a property list with
:line-width, :color and :style attributes.

The :line-width can be a positive or negative number:
- Positive: draws the box around the text (increases text height)
- Negative: draws the box within the text area (preserves text height)

The :line-width can also be a cons cell (VWIDTH . HWIDTH) to specify
different widths for vertical and horizontal lines.

The :style can be:
- released-button (3D button that is not pressed)
- pressed-button (3D button that is being pressed)
- nil (a simple 2D box, the default)"
  :type '(choice
          (const :tag "No box" nil)
          (const :tag "Simple box" t)
          (list :tag "Custom box"
                :value (:line-width 1 :color "#b0b0b0")
                (choice :tag "Line width"
                       (cons :tag "Width as cons"
                             :format "%t: %v"
                             :value (:line-width (1 . 1))
                             (const :format "" :line-width)
                             (cons (number :tag "Vertical") (number :tag "Horizontal")))
                       (cons :tag "Width as number"
                             :format "%t: %v"
                             :value (:line-width 1)
                             (const :format "" :line-width)
                             (number :format "%v")))
                (cons :tag "Color" :format "%t: %v"
                      :value (:color "#b0b0b0")
                      (const :format "" :color)
                      (color :format "%v"))
                (cons :tag "Style" :format "%t: %v"
                      :value (:style nil)
                      (const :format "" :style)
                      (choice :format "%v"
                              (const :tag "None (regular 2D box)" nil)
                              (const :tag "Released button" released-button)
                              (const :tag "Pressed button" pressed-button)))))
  :group 'org-supertag-inline-style)

(defcustom org-supertag-inline-weight 'semi-bold
  "Font weight for inline tags."
  :type '(choice
          (const :tag "Normal" normal)
          (const :tag "Bold" bold)
          (const :tag "Semi-bold" semi-bold))
  :group 'org-supertag-inline-style)

;; Define the face for inline tags
(defface org-supertag-inline-face
  `((t (:background ,org-supertag-inline-background
        :foreground ,org-supertag-inline-foreground
        :box ,org-supertag-inline-box
        :weight ,org-supertag-inline-weight)))
  "Face for org-supertag inline tags."
  :group 'org-supertag-inline-style)

;; Function to create the display property for hiding the # prefix
(defun org-supertag-inline-display-property (tag-name)
  "Create a display property to hide the # prefix for TAG-NAME."
  (when org-supertag-inline-style-hide-prefix
    (let ((len (length tag-name)))
      (propertize (substring tag-name 1) 
                 'face 'org-supertag-inline-face
                 'org-supertag-inline t))))

;; Compose font-lock keywords for highlighting inline tags
(defvar org-supertag-inline-font-lock-keywords
  `((,(rx "#" (+ (any alnum "-_")))
     (0 (let* ((tag-name (match-string-no-properties 0))
               (tag-text (substring tag-name 1)))
          ;; Only apply if not in special contexts
          (when (and (not (org-in-src-block-p))
                     (not (org-at-table-p))
                     (not (org-at-commented-p))
                     (not (eq (get-text-property (match-beginning 0) 'face) 'org-verbatim)))
            ;; Apply different properties to the prefix and the tag text
            (when org-supertag-inline-style-hide-prefix
              (add-text-properties
               (match-beginning 0) (+ (match-beginning 0) 1)
               '(invisible org-supertag-prefix display "")))
            ;; Apply face to the entire tag including prefix if not hidden
            (add-text-properties
             (if org-supertag-inline-style-hide-prefix
                 (+ (match-beginning 0) 1)
               (match-beginning 0))
             (match-end 0)
             `(face org-supertag-inline-face
                   help-echo ,(format "Tag: %s" tag-text)
                   org-supertag-inline t)))
          ;; Return nil to allow other fontification
          nil))))
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

;; Refresh styling when customizations change
(defun org-supertag-inline-style-update ()
  "Update the org-supertag-inline face based on current customizations."
  (custom-set-faces
   `(org-supertag-inline-face
     ((t (:background ,org-supertag-inline-background
          :foreground ,org-supertag-inline-foreground
          :box ,org-supertag-inline-box
          :weight ,org-supertag-inline-weight))))))

(advice-add 'customize-save-variable :after
            (lambda (&rest _)
              (when (boundp 'org-supertag-inline-style-mode)
                (org-supertag-inline-style-update))))

;; Override the tag insertion function to apply styling immediately
(advice-add 'org-supertag-inline-insert-tag :after
            (lambda (&rest _)
              (font-lock-flush)))

;;; Helper functions for inline tag insertion

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
                        (org-supertag-tag-create sanitized-name)
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
     ;; 如果找到现有标签行，移动到行尾
     (existing-tag-line
      (goto-char existing-tag-line)
      (end-of-line))
     ;; 如果有drawer，在drawer后智能定位
     (drawer-end
      (goto-char drawer-end)
      ;; 跳过空行到内容开始
      (while (and (not (eobp))
                 (not (org-at-heading-p))
                 (looking-at-p "^[ \t]*$"))
        (forward-line 1))
      ;; 如果已经在内容行，在内容行上方创建新行插入
      (if (and (not (eobp))
              (not (org-at-heading-p))
              (not (looking-at-p "^[ \t]*$")))
          (progn
            (beginning-of-line)
            (open-line 1))
        ;; 否则在当前位置创建新行
        (unless (bolp) (insert "\n"))))
     ;; 如果没有drawer，在标题下方新建一行
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
        (when (and (looking-at-p "^[ \t]*#[a-zA-Z0-9_-]+")
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
                   ;; 中文字符
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
    (looking-at-p "^[ \t]*#[a-zA-Z0-9_-]+")))

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
(defun org-supertag-inline-insert-tag (tag-name)
  "Insert an inline tag at point and establish proper relationships.
When called with an active region, use the region text as the default tag name.
TAG-NAME is the name of the tag to insert.

This function follows a structured approach:
1. Analyze the current context
2. Ensure the tag exists (create if necessary)
3. Adjust cursor position appropriately (for manual insertion, stay at cursor)
4. Insert the tag with proper spacing
5. Establish tag-node relationships"
  (interactive (list (org-supertag-inline--read-tag-name)))
  
  (let* ((original-pos (point-marker))
         (context-info (org-supertag-inline--analyze-context))
         (node-id (plist-get context-info :node-id))
         (tag-result (org-supertag-inline--ensure-tag tag-name))
         (tag-id (plist-get tag-result :tag-id))
         (display-name (plist-get tag-result :tag-name)))
    
    ;; Debug information
    (message "inline-insert-tag: ID=%s drawer=%s ctx=%s pos=%d"
             node-id
             (plist-get context-info :in-drawer)
             (plist-get context-info :type)
             (marker-position original-pos))
    
    ;; Process the insertion - for manual insertion, don't change position
    (org-supertag-inline--adjust-position context-info)
    (org-supertag-inline--insert-tag-text display-name)
    ;; Ensure tag line ends with newline if not at end of buffer
    (unless (or (eobp) (looking-at-p "\n"))
      (insert "\n"))
    (org-supertag-inline--establish-relationship tag-result node-id)
    
    ;; Cleanup
    (set-marker original-pos nil)
    
    ;; Return message
    (message "Inserted inline tag #%s%s"
             display-name
             (if node-id
                 (format " and linked to node %s" node-id)
               ""))))

;;;###autoload
(defun org-supertag-inline-insert-tags-batch (tag-names)
  "Insert multiple inline tags in a single line and establish relationships.
TAG-NAMES is a list of tag names."
  (when tag-names
    (let* ((context-info (org-supertag-inline--analyze-context))
           (node-id (plist-get context-info :node-id))
           (processed-tags '()))
      
      ;; Process and validate all tags
      (dolist (tag-name tag-names)
        (condition-case err
            (let ((tag-result (org-supertag-inline--ensure-tag tag-name)))
              (push tag-result processed-tags))
          (error
           (message "Failed to process tag: %s - %s" tag-name (error-message-string err)))))
      
      ;; Insert tags in batch
      (when processed-tags
        (setq processed-tags (nreverse processed-tags))
        (org-supertag-inline--insert-tags-batch processed-tags)
        
        ;; Establish relationships
        (dolist (tag-result processed-tags)
          (org-supertag-inline--establish-relationship tag-result node-id))
        
        (message "Inserted %d tags: %s" 
                 (length processed-tags)
                 (mapconcat (lambda (tr) (plist-get tr :tag-name)) processed-tags " "))))))

(defun org-supertag-inline--insert-tags-batch (tag-results)
  "Insert multiple tags in a single line.
TAG-RESULTS is a list of tag result plists."
  (when tag-results
    (let* ((drawer-end (org-supertag-inline--find-drawer-end))
           (existing-tag-line (org-supertag-inline--find-existing-tag-line)))
      (cond
       ;; If there's an existing tag line, add at the end
       (existing-tag-line
        (goto-char existing-tag-line)
        (end-of-line)
        (dolist (tag-result tag-results)
          (insert (format " #%s" (plist-get tag-result :tag-name))))
        ;; Ensure tag line ends with newline
        (unless (looking-at-p "\n")
          (insert "\n")))
       ;; If there's a drawer, create a tag line after the drawer
       (drawer-end
        (goto-char drawer-end)
        ;; Skip empty lines
        (while (and (not (eobp))
                   (not (org-at-heading-p))
                   (looking-at-p "^[ \t]*$"))
          (forward-line 1))
        ;; If in content line, insert above
        (if (and (not (eobp))
                (not (org-at-heading-p))
                (not (looking-at-p "^[ \t]*$")))
            (progn
              (beginning-of-line)
              (open-line 1))
          (unless (bolp) (insert "\n")))
        ;; Insert all tags 
        (insert (mapconcat (lambda (tag-result) 
                            (format "#%s" (plist-get tag-result :tag-name)))
                          tag-results " "))
        (insert "\n"))
       ;; If no drawer, create a tag line below the heading
       (t
        (end-of-line)
        (insert "\n")
        (insert (mapconcat (lambda (tag-result) 
                            (format "#%s" (plist-get tag-result :tag-name)))
                          tag-results " "))
        (insert "\n"))))))

(defun org-supertag-inline-insert-tag-for-node (node-id tag-name)
  "Insert an inline tag for a specific node ID (used by auto-tag system).
This finds the node's file and position, then inserts the tag below the drawer.
NODE-ID: The ID of the node.
TAG-NAME: The name of the tag to insert."
  (when-let* ((node-data (org-supertag-db-get node-id))
              (file-path (plist-get node-data :file-path))
              (pos (plist-get node-data :pos)))
    (if (and file-path (file-exists-p file-path))
        (with-current-buffer (find-file-noselect file-path)
          (goto-char pos)
          (when (org-at-heading-p)
            ;; Use the smart positioning logic for auto-tag insertion
            (org-supertag-inline--smart-position-for-insertion)
            ;; Create tag and insert
            (let* ((tag-result (org-supertag-inline--ensure-tag tag-name))
                   (display-name (plist-get tag-result :tag-name))
                   (tag-id (plist-get tag-result :tag-id)))
              ;; Insert the tag name (not UUID)
              (insert "#" display-name " ")
              ;; Establish relationship using tag-id
              (let ((org-supertag-tag-apply-skip-headline t)
                    (org-supertag-force-node-id node-id))
                (org-supertag-tag-apply tag-id)))
            (save-buffer)))
      (message "Node file not found for ID: %s" node-id))))

(provide 'org-supertag-inline)
;;; org-supertag-inline.el ends here 
