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

(defgroup org-supertag-inline nil
  "Customization options for org-supertag inline tags."
  :group 'org-supertag)

(defcustom org-supertag-inline-enable-fontification t
  "Whether to enable highlighting of inline tags.
When non-nil, inline tags will be highlighted with a special face."
  :type 'boolean
  :group 'org-supertag-inline)

(defcustom org-supertag-inline-light-theme-colors
  '(:box (:line-width -1 :color "#d0d0d0")
    :background "#f8f8f8"
    :foreground "#2a2a2a")
  "Color settings for inline tags in light theme.
You can customize :box, :background and :foreground colors."
  :type '(plist :key-type symbol :value-type sexp)
  :group 'org-supertag-inline)

(defcustom org-supertag-inline-dark-theme-colors
  '(:box (:line-width 2 :color "#C9C9C7")
    :background "#383838"
    :foreground "#e8e8e8")
  "Color settings for inline tags in dark theme.
You can customize :box, :background and :foreground colors."
  :type '(plist :key-type symbol :value-type sexp)
  :group 'org-supertag-inline)

(defcustom org-supertag-inline-text-properties
  '(:height 0.95
    :weight normal
    :spacing 0.2)
  "Text properties for inline tags.
You can customize :height (relative to default font size),
:weight (bold, normal, etc.), and :spacing between characters."
  :type '(plist :key-type symbol :value-type sexp)
  :group 'org-supertag-inline)

(defface org-supertag-inline-tag-face
  `((((class color) (background light))
     :inherit org-tag
     ,@org-supertag-inline-light-theme-colors
     ,@org-supertag-inline-text-properties)
    (((class color) (background dark))
     :inherit org-tag
     ,@org-supertag-inline-dark-theme-colors
     ,@org-supertag-inline-text-properties))
  "Face used for inline tags in org-supertag-mode.
You can customize this face through:
`org-supertag-inline-light-theme-colors' - Colors for light theme
`org-supertag-inline-dark-theme-colors' - Colors for dark theme
`org-supertag-inline-text-properties' - Text properties like height and spacing"
  :group 'org-supertag-inline)

(defcustom org-supertag-inline-excluded-contexts
  '(src-block comment example-block export-block verse-block
    quote-block comment-block center-block special-block
    headline property-drawer keyword)
  "List of org element types where inline tags should not be highlighted.
This prevents highlighting tags in places where they should be treated as literal text."
  :type '(repeat symbol)
  :group 'org-supertag-inline)

(defcustom org-supertag-inline-tag-regexp "#\\([[:alnum:]_-]+\\)\\(?:[^:]\\|$\\)"
  "Regular expression pattern to match inline tags.
The first capture group should match the tag name without the '#' prefix.
Supports alphanumeric characters, underscores and hyphens.
The pattern ensures we don't match org-mode's native tag format like :tag:.

Examples of valid tags:
- #tag
- #tag_with_underscore
- #tag-with-hyphen
- #tag123

Invalid matches (will be ignored):
- :tag:
- #tag:
- :#tag:"
  :type 'regexp
  :group 'org-supertag-inline)

(defcustom org-supertag-inline-hide-hash t
  "Whether to hide the # symbol in inline tags.
When non-nil, the # prefix will be hidden for cleaner display."
  :type 'boolean
  :group 'org-supertag-inline)

(defvar org-supertag-inline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t i") 'org-supertag-inline-insert-tag)
    map)
  "Keymap for `org-supertag-inline-mode'.")

(defun org-supertag-inline--fontify-tag (limit)
  "Font-lock function to highlight inline tags up to LIMIT."
  (when (re-search-forward org-supertag-inline-tag-regexp limit t)
    (let* ((begin (match-beginning 0))  ; start of #
           (tag-begin (match-beginning 1))  ; start of tag name
           (end (match-end 1))  ; end of tag name only
           (element-type (org-element-type (org-element-context))))
      (when (and (not (memq element-type org-supertag-inline-excluded-contexts))
                 ;; Additional check to avoid org-mode native tags
                 (not (save-excursion
                        (goto-char begin)
                        (looking-at-p ":[^:]*#\\|#[^:]*:"))))
        ;; Apply face to the tag text only (not the #)
        (put-text-property tag-begin end 'face 'org-supertag-inline-tag-face)
        
        ;; Create a distinctive visual effect for the # symbol
        (when org-supertag-inline-hide-hash
          (compose-region begin tag-begin " "))        
        ;; Apply display properties to entire tag
        (put-text-property begin end 'display 
                          (list 'raise 0.1))  ;; 轻微升高标签
        ;; Add space after tag if needed
        (when (and (< end (point-max))
                   (not (eq (char-after end) ? )))
          (put-text-property end (1+ end) 'display " "))
        t))))

(defvar org-supertag-inline--keywords
  '((org-supertag-inline--fontify-tag))
  "Font-lock keywords for `org-supertag-inline-mode'.")

(defun org-supertag-inline--setup-font-lock ()
  "Setup font-lock for inline tags."
  (if org-supertag-inline-enable-fontification
      (font-lock-add-keywords nil org-supertag-inline--keywords t)
    (font-lock-remove-keywords nil org-supertag-inline--keywords))
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (with-no-warnings (font-lock-fontify-buffer))))

(defun org-supertag-inline-refresh ()
  "Refresh inline tag highlighting in the current buffer."
  (interactive)
  (when org-supertag-inline-mode
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))
    (message "Refreshed inline tag highlighting using font-lock")))

;;;###autoload
(define-minor-mode org-supertag-inline-mode
  "Toggle org-supertag inline tag mode.
When enabled, inline tags (prefixed with #) can be inserted and highlighted."
  :init-value nil
  :lighter " OrgST-Inline"
  :keymap org-supertag-inline-mode-map
  (if org-supertag-inline-mode
      (progn
        ;; Setup font-lock for highlighting
        (org-supertag-inline--setup-font-lock)
        ;; Keep hook for compatibility, but our implementation uses font-lock
        (add-hook 'after-change-functions #'org-supertag-inline--after-change nil t))
    ;; Disable
    (font-lock-remove-keywords nil org-supertag-inline--keywords)
    (remove-hook 'after-change-functions #'org-supertag-inline--after-change t)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

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
  (when tag-name
    (let* (;; Check if input ends with #, if so, create new tag directly
           (direct-create (string-suffix-p "#" tag-name))
           (tag-name-clean (if direct-create
                             (substring tag-name 0 -1)
                           tag-name))
           (sanitized-name (org-supertag-sanitize-tag-name tag-name-clean))
           ;; Ensure current node exists
           (node-id (org-id-get-create)))      
      ;; Get or create tag
      (let ((tag-id
             (cond
              ;; If tag exists, use it directly
              ((org-supertag-tag-exists-p sanitized-name)
               sanitized-name)
              ;; Otherwise, create new tag
              (t
               (if (or direct-create
                       (y-or-n-p (format "Create new tag '%s'? " sanitized-name)))
                   (org-supertag-tag-create sanitized-name)
                 (user-error "Tag creation cancelled"))))))
        ;; Delete region if active
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        ;; Add space before tag if needed
        (unless (or (bobp) (eq (char-before) ? ))
          (insert " "))
        ;; Insert inline tag
        (insert (concat "#" tag-id))
        ;; Add space after tag if needed
        (unless (or (eobp) (eq (char-after) ? ))
          (insert " "))
        ;; Use org-supertag-tag-apply to apply the tag
        (save-excursion
          (org-back-to-heading t)
          (let ((org-supertag-tag-apply-skip-headline t))
            (org-supertag-tag-apply tag-id)))
        
        (message "Inserted inline tag #%s" tag-id)))))

(defun org-supertag-inline--after-change (_beg _end _len)
  "Compatibility function for after-change hooks.
Updates inline tag highlighting between BEG and END.
Now delegates to font-lock."
  (when org-supertag-inline-enable-fontification
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

;;;###autoload
(defun org-supertag-inline-setup ()
  "Set up the inline tag system."
  (interactive)
  (add-hook 'org-supertag-mode-hook
            (lambda ()
              (org-supertag-inline-mode (if org-supertag-mode 1 -1))))
  (message "Inline tag support enabled. Use C-c t i to insert tags."))

(defun org-supertag-inline--highlight-buffer ()
  "Highlight all inline tags in the current buffer using font-lock.
Compatibility function for existing code."
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (with-no-warnings (font-lock-fontify-buffer))))

(defun org-supertag-inline--clear-overlays ()
  "Compatibility function for old overlay-based code.
This is now a no-op as we use font-lock."
  (when font-lock-mode
    (font-lock-remove-keywords nil org-supertag-inline--keywords)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

(provide 'org-supertag-inline)
;;; org-supertag-inline.el ends here 
