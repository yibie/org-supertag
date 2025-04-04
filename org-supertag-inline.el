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

(defcustom org-supertag-inline-border-radius 3
  "Border radius for inline tags (only works in GUI Emacs with CSS support)."
  :type 'integer
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
  `((,(rx (group "#" (+ (any alnum "-_"))))
      (0 (let* ((tag-name (match-string-no-properties 1))
                (prefix "#")
                (tag-text (substring tag-name 1)))
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
                  org-supertag-inline t))
           ;; Return nil to allow other fontification
           nil))
      ;; Don't apply in org src blocks, code blocks, or verbatim sections
      (0 'org-supertag-inline-face nil
         (and (not (org-in-src-block-p))
              (not (org-at-table-p))
              (not (org-at-commented-p))
              (not (eq (get-text-property (match-beginning 0) 'face) 'org-verbatim))))))
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

;;;###autoload
(defun org-supertag-inline-insert-tag (tag-name)
  "Insert an inline tag at point and establish proper relationships.
When called with an active region, use the region text as the default tag name.
TAG-NAME is the name of the tag to insert."
  (interactive
   (list (let* ((all-tags (org-supertag-get-all-tags))
                (preset-names (mapcar #'car org-supertag-preset-tags))
                ;; Remove existing preset tags to avoid duplicates
                (user-tags (cl-remove-if (lambda (tag) (member tag preset-names)) all-tags))
                (candidates (delete-dups
                            (append 
                             ;; Use [P] prefix to mark preset tags
                             (mapcar (lambda (name) 
                                     (format "[P] %s" name))
                                   preset-names)
                             ;; Regular tags are kept as is
                             user-tags)))
                ;; Get region content if active
                (region-text (when (use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning)
                              (region-end))))
                (input (completing-read
                       "Inline tag: "
                       candidates nil nil
                       ;; Use region text as initial input if available
                       region-text)))
           ;; Process input, remove [P] prefix
           (if (string-prefix-p "[P] " input)
               (substring input 4)
             input))))
  
  ;; 保存原始位置，便于后续分析上下文
  (let* ((original-pos (point-marker))
         ;; 基本参数处理
         (direct-create (string-suffix-p "#" tag-name))
         (tag-name-clean (if direct-create
                           (substring tag-name 0 -1)
                         tag-name))
         (sanitized-name (org-supertag-sanitize-tag-name tag-name-clean))
         ;; 对上下文的分析，检查是否在drawer内等
         (context (org-element-context))
         (context-type (org-element-type context))
         (in-drawer (or (eq context-type 'drawer)
                        (eq context-type 'property-drawer)
                        (eq context-type 'node-property)
                        (and (eq context-type 'keyword)
                             (string= (org-element-property :key context) "END"))))
         ;; ID 查找逻辑 - 优先使用强制 ID，其次查找现有 ID
         (force-id (and (boundp 'org-supertag-force-node-id)
                      org-supertag-force-node-id))
         ;; 可靠地获取节点 ID - 先尝试直接获取，如果失败则尝试定位到标题
         (existing-id (or force-id
                         (org-entry-get nil "ID")
                         (save-excursion
                           (ignore-errors 
                             (org-back-to-heading t)
                             (org-entry-get nil "ID")))))
         ;; 如果没有现有 ID 且在标题上，则创建新 ID
         (node-id (or existing-id
                     (save-excursion
                       (when (ignore-errors (org-back-to-heading t))
                         (org-supertag-node-create))))))
      
    ;; 调试信息
    (message "inline-insert-tag: ID=%s force=%s drawer=%s ctx=%s pos=%d" 
             node-id force-id in-drawer context-type (marker-position original-pos))
    
    ;; 处理标签创建
    (let ((tag-id
           (cond
            ;; 如果标签存在，直接使用
            ((org-supertag-tag-exists-p sanitized-name)
             sanitized-name)
            ;; 否则创建新标签
            (t
             (if (or direct-create
                     (y-or-n-p (format "Create new tag '%s'? " sanitized-name)))
                 (org-supertag-tag-create sanitized-name)
               (user-error "Tag creation cancelled"))))))
      
      ;; 删除选区（如果有）
      (when (use-region-p)
        (delete-region (region-beginning) (region-end)))
      
      ;; 处理光标位置 - 如果在 drawer 中，移到 drawer 后面
      (when in-drawer
        (let ((end-pos (org-element-property :end context)))
          (when end-pos
            (goto-char end-pos))))
      
      ;; 插入标签文本
      (unless (or (bobp) (eq (char-before) ? ))
        (insert " "))
      (insert (concat "#" tag-id))
      (unless (or (eobp) (eq (char-after) ? ))
        (insert " "))
      
      ;; 应用标签关系
      (when node-id
        (let ((org-supertag-tag-apply-skip-headline t)
              (org-supertag-force-node-id node-id))
          (org-supertag-tag-apply tag-id)))
      
      ;; 清理
      (set-marker original-pos nil)
      
      ;; 返回消息
      (message "Inserted inline tag #%s%s" 
              tag-id
              (if node-id
                  (format " and linked to node %s" node-id)
                "")))))

(provide 'org-supertag-inline)
;;; org-supertag-inline.el ends here 
