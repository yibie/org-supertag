;;; org-supertag/supertag-view-helper.el --- Reusable view helpers for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides common, reusable functions for building view buffers,
;; such as rendering headers, formatting values, and managing windows.

;;; Code:

(require 'cl-lib)
(require 'org)

;; Forward declare functions to avoid circular dependencies
(declare-function supertag-field-get "supertag-ops-field")

;;;----------------------------------------------------------------------
;;; Constants and Configuration
;;;----------------------------------------------------------------------

(defconst supertag-view-helper--valid-tag-chars "a-zA-Z0-9_@%一-龥-"
  "The set of characters considered valid for an inline supertag.
This is used to construct regular expressions and includes ASCII
alphanumeric characters and a direct UTF-8 CJK range for max compatibility.")

;; Legacy alias for backward compatibility
(defconst supertag-view-style--valid-tag-chars supertag-view-helper--valid-tag-chars
  "Legacy alias for valid tag characters.")

(defgroup supertag-view-style nil
  "Customization options for supertag inline tag styling."
  :group 'org-supertag)

(defcustom supertag-view-style-tag-face-properties
  '(:foreground "snow3")
  "Face properties for inline supertags.
This should be a plist of face attributes."
  :type '(plist :key-type symbol :value-type sexp)
  :group 'supertag-view-style)

(defcustom supertag-view-style-auto-enable t
  "Whether to automatically enable supertag-view-style-mode in org buffers."
  :type 'boolean
  :group 'supertag-view-style)

;;;----------------------------------------------------------------------
;;; Context Detection Functions
;;;----------------------------------------------------------------------

(defun supertag-view-helper--in-src-block-p (&optional pos)
  "Check if point (or POS) is inside a source block.
This is a safe wrapper around org-in-src-block-p."
  (let ((check-pos (or pos (point))))
    (save-excursion
      (goto-char check-pos)
      (cond
       ;; Use org-in-src-block-p if available
       ((fboundp 'org-in-src-block-p)
        (org-in-src-block-p))
       ;; Fallback implementation
       (t
        (let ((element (org-element-at-point)))
          (memq (org-element-type element) '(src-block example-block))))))))

(defun supertag-view-helper--at-table-p (&optional pos)
  "Check if point (or POS) is at a table.
This is a safe wrapper around org-at-table-p."
  (let ((check-pos (or pos (point))))
    (save-excursion
      (goto-char check-pos)
      (cond
       ;; Use org-at-table-p if available
       ((fboundp 'org-at-table-p)
        (org-at-table-p))
       ;; Fallback implementation
       (t
        (let ((element (org-element-at-point)))
          (eq (org-element-type element) 'table)))))))

(defun supertag-view-helper--at-commented-p (&optional pos)
  "Check if point (or POS) is at a commented line.
This is a safe wrapper around org-at-commented-p."
  (let ((check-pos (or pos (point))))
    (save-excursion
      (goto-char check-pos)
      (cond
       ;; Use org-at-commented-p if available
       ((fboundp 'org-at-commented-p)
        (org-at-commented-p))
       ;; Fallback implementation
       (t
        (beginning-of-line)
        (looking-at-p "^[ \t]*#\\+"))))))

;;;----------------------------------------------------------------------
;;; Minor Mode Definition and Public API
;;;----------------------------------------------------------------------

;;;###autoload
(define-minor-mode supertag-view-style-mode
  "Minor mode for styling supertag inline tags."
  :lighter " Tag-Style"
  :group 'supertag-view-style
  (if supertag-view-style-mode
      (progn
        (font-lock-add-keywords nil supertag-view-helper--font-lock-keywords t)
        (supertag-view-helper--refresh-fontification))
    (font-lock-remove-keywords nil supertag-view-helper--font-lock-keywords)
    (supertag-view-helper--refresh-fontification)))

(defun supertag-view-helper--refresh-fontification ()
  "Refresh font-lock fontification in the current buffer."
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (font-lock-fontify-buffer)))

(defun supertag-view-helper--auto-enable ()
  "Auto-enable supertag-view-style-mode in org buffers if configured."
  (when supertag-view-style-auto-enable
    (supertag-view-style-mode 1)))

;; Hook into org-mode
(add-hook 'org-mode-hook #'supertag-view-helper--auto-enable)

;;;###autoload
(defun supertag-view-style-refresh ()
  "Manually refresh tag highlighting in the current buffer."
  (interactive)
  (when supertag-view-style-mode
    (supertag-view-helper--refresh-fontification)
    (message "Supertag highlighting refreshed")))

;;;###autoload
(defun supertag-view-style-toggle ()
  "Toggle supertag-view-style-mode in the current buffer."
  (interactive)
  (supertag-view-style-mode 'toggle)
  (message "Supertag styling %s" 
           (if supertag-view-style-mode "enabled" "disabled")))

;; For backward compatibility
(defalias 'org-supertag-inline-style-mode 'supertag-view-style-mode)
(defalias 'org-supertag-debug-force-refontify 'supertag-view-style-refresh)

(defconst supertag-view-helper--section-separator "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  "Separator line for sections.")

(defconst supertag-view-helper--subsection-separator "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"
  "Separator line for subsections.")

;;;----------------------------------------------------------------------
;;; Face Definitions and Font-lock Integration
;;;----------------------------------------------------------------------

(defface supertag-inline-face
  `((t ,supertag-view-style-tag-face-properties))
  "Face for supertag inline tags."
  :group 'supertag-view-style)

(defvar supertag-view-helper--font-lock-keywords
  `((,(concat "#[" supertag-view-helper--valid-tag-chars "]+")
     (0 (if (and (not (supertag-view-helper--in-src-block-p))
                 (not (supertag-view-helper--at-table-p))
                 (not (supertag-view-helper--at-commented-p))
                 (not (eq (get-text-property (match-beginning 0) 'face) 'org-verbatim)))
            'supertag-inline-face) t)))
  "Font-lock keywords for highlighting inline tags.")

(defun supertag-view-helper-insert-section-header (title icon)
  "Insert a section header with ICON and TITLE."
  (insert (propertize (format "%s %s\n" icon title) 'face '(:weight bold :height 1.2)))
  (insert (propertize (format "%s\n" supertag-view-helper--section-separator) 'face '(:foreground "gray50")))
  (insert "\n"))

(defun supertag-view-helper-insert-subsection-header (title)
  "Insert a subsection header with TITLE."
  (insert (propertize (format "  %s\n" title) 'face '(:weight bold)))
  (insert (propertize (format "  %s\n" supertag-view-helper--subsection-separator) 'face '(:foreground "gray70")))
  (insert "\n"))

;;; --- Theme Adaptive Colors ---

(defun supertag-view-helper-get-theme-adaptive-color (light-color dark-color)
  "Get color that adapts to current theme."
  (if (eq (frame-parameter nil 'background-mode) 'dark)
      dark-color
    light-color))

(defun supertag-view-helper-get-accent-color ()
  "Get accent color that works well in both light and dark themes."
  (supertag-view-helper-get-theme-adaptive-color "#0066CC" "#66B3FF"))

(defun supertag-view-helper-get-emphasis-color ()
  "Get emphasis color that works well in both light and dark themes."
  (supertag-view-helper-get-theme-adaptive-color "#003D82" "#99D6FF"))

(defun supertag-view-helper-get-muted-color ()
  "Get muted color that works well in both light and dark themes."
  (supertag-view-helper-get-theme-adaptive-color "#666666" "#AAAAAA"))

(defun supertag-view-helper-get-success-color ()
  "Get success color that works well in both light and dark themes."
  (supertag-view-helper-get-theme-adaptive-color "#22C55E" "#4ADE80"))

(defun supertag-view-helper-get-warning-color ()
  "Get warning color that works well in both light and dark themes."
  (supertag-view-helper-get-theme-adaptive-color "#F59E0B" "#FCD34D"))

(defun supertag-view-helper-get-error-color ()
  "Get error color that works well in both light and dark themes."
  (supertag-view-helper-get-theme-adaptive-color "#EF4444" "#F87171"))

(defun supertag-view-helper-get-background-color ()
  "Get background color that works well in both light and dark themes."
  (supertag-view-helper-get-theme-adaptive-color "#F8FAFC" "#1E293B"))

(defun supertag-view-helper-get-border-color ()
  "Get border color that works well in both light and dark themes."
  (supertag-view-helper-get-theme-adaptive-color "#E2E8F0" "#334155"))

;;; --- Value Formatting ---

(defun supertag-view-helper-format-value (value)
  "Format VALUE for display. Handles lists and nil."
  (let ((formatted-value (if (listp value)
                              (mapconcat #'identity value " / ")
                            (format "%s" (or value "")))))
    (if (string-empty-p formatted-value)
        (propertize "[Empty]" 'face `(:foreground ,(supertag-view-helper-get-muted-color) :slant italic))
      (propertize formatted-value 'face `(:foreground ,(supertag-view-helper-get-accent-color))))))

(defun supertag-view-helper-format-tag-value (value)
  "Format tag VALUE with special styling for tags."
  (if (or (null value) (string-empty-p (format "%s" value)))
      (propertize "[No tags]" 'face `(:foreground ,(supertag-view-helper-get-muted-color) :slant italic))
    (let ((tags (if (stringp value)
                    (split-string value "," t "[ \t\n\r]+")
                  (if (listp value) value (list (format "%s" value))))))
      (mapconcat (lambda (tag)
                   (propertize (concat "#" (string-trim tag))
                               'face `(:foreground ,(supertag-view-helper-get-emphasis-color)
                                       :weight bold
                                       :box (:line-width 1 :color ,(supertag-view-helper-get-border-color)))))
                 tags " "))))

(defun supertag-view-helper-format-boolean-value (value)
  "Format boolean VALUE with visual indicators."
  (let ((bool-val (cond
                   ((or (eq value t) (string= value "true") (string= value "yes") (string= value "1")) t)
                   ((or (eq value nil) (string= value "false") (string= value "no") (string= value "0")) nil)
                   (t nil))))
    (if bool-val
        (propertize "✓ True" 'face `(:foreground ,(supertag-view-helper-get-success-color) :weight bold))
      (propertize "✗ False" 'face `(:foreground ,(supertag-view-helper-get-muted-color))))))

(defun supertag-view-helper-format-number-value (value)
  "Format numeric VALUE with proper styling."
  (if (or (null value) (string-empty-p (format "%s" value)))
      (propertize "[No value]" 'face `(:foreground ,(supertag-view-helper-get-muted-color) :slant italic))
    (propertize (format "%s" value) 'face `(:foreground ,(supertag-view-helper-get-accent-color) :weight bold))))

(defun supertag-view-helper-format-date-value (value)
  "Format date VALUE with calendar icon."
  (if (or (null value) (string-empty-p (format "%s" value)))
      (propertize "[No date]" 'face `(:foreground ,(supertag-view-helper-get-muted-color) :slant italic))
    (propertize (concat "📅 " (format "%s" value))
                'face `(:foreground ,(supertag-view-helper-get-accent-color)))))

(defun supertag-view-helper-format-url-value (value)
  "Format URL VALUE as clickable link."
  (if (or (null value) (string-empty-p (format "%s" value)))
      (propertize "[No URL]" 'face `(:foreground ,(supertag-view-helper-get-muted-color) :slant italic))
    (propertize (concat "🔗 " (format "%s" value))
                'face `(:foreground ,(supertag-view-helper-get-accent-color) :underline t)
                'mouse-face 'highlight
                'help-echo "Click to open URL")))

(defun supertag-view-helper-format-field-value (field-def value)
  "Format field VALUE according to its FIELD-DEF type."
  (let ((field-type (plist-get field-def :type)))
    (pcase field-type
      (:tag (supertag-view-helper-format-tag-value value))
      (:boolean (supertag-view-helper-format-boolean-value value))
      ((or :number :integer) (supertag-view-helper-format-number-value value))
      ((or :date :timestamp) (supertag-view-helper-format-date-value value))
      (:url (supertag-view-helper-format-url-value value))
      (_ (supertag-view-helper-format-value value)))))

;;; --- Simple and Clean Components ---

(defun supertag-view-helper-insert-simple-header (title &optional stats)
  "Insert a simple header with TITLE and optional STATS."
  (insert (propertize title 'face `(:weight bold :height 1.3 :foreground ,(supertag-view-helper-get-emphasis-color))))
  (when stats
    (insert (propertize (format "    %s" stats) 
                        'face `(:foreground ,(supertag-view-helper-get-muted-color)))))
  (insert "\n")
  (insert (propertize (make-string 60 ?─) 'face `(:foreground ,(supertag-view-helper-get-border-color))))
  (insert "\n\n"))

(defun supertag-view-helper-insert-section-title (title &optional icon)
  "Insert a simple section title with optional ICON."
  (insert (propertize (if icon (format "%s %s" icon title) title)
                      'face `(:weight bold :foreground ,(supertag-view-helper-get-emphasis-color))))
  (insert "\n"))

(defun supertag-view-helper-insert-field-line (field-name value field-def &optional interactive-props)
  "Insert a simple field line with FIELD-NAME, VALUE, FIELD-DEF and optional INTERACTIVE-PROPS."
  (let* ((formatted-value (supertag-view-helper-format-field-value field-def value))
         (field-type (plist-get field-def :type))
         (type-icon (pcase field-type
                      (:string "📝")
                      (:number "🔢")
                      (:integer "🔢")
                      (:boolean "☑")
                      (:date "📅")
                      (:timestamp "⏰")
                      (:url "🔗")
                      (:email "📧")
                      (:tag "🏷")
                      (_ "📄")))
         (start-pos (point)))
    (insert "  ")
    (insert (propertize (format "%s %s" type-icon field-name)
                        'face `(:weight bold :foreground ,(supertag-view-helper-get-emphasis-color))))
    (let* ((dots-length (max 3 (- 35 (length field-name)))))
      (insert (propertize (concat " " (make-string dots-length ?.)) 
                          'face `(:foreground ,(supertag-view-helper-get-muted-color)))))
    (insert " ")
    (insert formatted-value)
    (insert "\n")
    ;; Add text properties for interactivity
    (when interactive-props
      (add-text-properties start-pos (point) interactive-props))))

(defun supertag-view-helper-insert-simple-footer (&rest help-lines)
  "Insert a simple footer with essential HELP-LINES."
  (insert "\n")
  (insert (propertize (make-string 60 ?─) 'face `(:foreground ,(supertag-view-helper-get-border-color))))
  (insert "\n")
  (dolist (line help-lines)
    (insert (propertize (format "%s\n" line)
                        'face `(:foreground ,(supertag-view-helper-get-muted-color) :height 0.9)))))

(defun supertag-view-helper-insert-simple-empty-state (message)
  "Insert a simple empty state MESSAGE."
  (insert (propertize (format "  %s\n" message)
                      'face `(:foreground ,(supertag-view-helper-get-muted-color) :slant italic))))

(defun supertag-view-helper-insert-tag-block (tag-id fields node-id)
  "Insert a simple tag block with TAG-ID, FIELDS for NODE-ID."
  (insert (propertize (format "🏷️ %s\n" tag-id)
                      'face `(:weight bold :foreground ,(supertag-view-helper-get-accent-color))
                      'supertag-context t
                      'type :tag
                      'tag-id tag-id
                      'field-name nil
                      'id tag-id))
  
  (if (not fields)
      (supertag-view-helper-insert-simple-empty-state "No fields defined")
    (dolist (field-def fields)
      (let* ((field-name (plist-get field-def :name))
             (value (supertag-field-get node-id tag-id field-name))
             (interactive-props `(field-name ,field-name
                                  tag-id ,tag-id
                                  field-def ,field-def
                                  supertag-context t
                                  type :field-value
                                  mouse-face highlight
                                  help-echo "Click to edit this field")))
        (supertag-view-helper-insert-field-line field-name value field-def interactive-props))))
  (insert "\n"))

;;; --- Card-like Components ---

(defun supertag-view-helper-insert-card-start (title &optional icon color)
  "Start a card-like container with TITLE, optional ICON and COLOR."
  (let ((card-color (or color (supertag-view-helper-get-border-color)))
        (title-with-icon (if icon (format "%s %s" icon title) title)))
    (insert (propertize (format "┌─ %s " title-with-icon)
                        'face `(:weight bold :foreground ,(supertag-view-helper-get-emphasis-color))))
    (insert (propertize (make-string (max 0 (- 60 (length title-with-icon) 3)) ?─)
                        'face `(:foreground ,card-color)))
    (insert (propertize "┐\n" 'face `(:foreground ,card-color)))))

(defun supertag-view-helper-insert-card-content (content &optional padding)
  "Insert CONTENT inside a card with optional PADDING."
  (let ((pad (or padding "│ "))
        (border-color (supertag-view-helper-get-border-color)))
    (dolist (line (split-string content "\n"))
      (insert (propertize pad 'face `(:foreground ,border-color)))
      (insert line "\n"))))

(defun supertag-view-helper-insert-card-end ()
  "End a card-like container."
  (let ((border-color (supertag-view-helper-get-border-color)))
    (insert (propertize "└" 'face `(:foreground ,border-color)))
    (insert (propertize (make-string 58 ?─) 'face `(:foreground ,border-color)))
    (insert (propertize "┘\n" 'face `(:foreground ,border-color)))))

(defun supertag-view-helper-insert-field-row (field-name value field-def &optional interactive-props)
  "Insert a formatted field row with FIELD-NAME, VALUE, FIELD-DEF and optional INTERACTIVE-PROPS."
  (let ((border-color (supertag-view-helper-get-border-color))
        (formatted-value (supertag-view-helper-format-field-value field-def value))
        (field-type (plist-get field-def :type))
        (type-icon (pcase (plist-get field-def :type)
                     (:string "📝")
                     (:number "🔢")
                     (:integer "🔢")
                     (:boolean "☑️")
                     (:date "📅")
                     (:timestamp "⏰")
                     (:url "🔗")
                     (:email "📧")
                     (:tag "🏷️")
                     (_ "📄")))
        (start-pos (point)))
    ;; Left border
    (insert (propertize "│ " 'face `(:foreground ,border-color)))
    ;; Field icon and name
    (insert (propertize (format "%s %s" type-icon field-name)
                        'face `(:weight bold :foreground ,(supertag-view-helper-get-emphasis-color))))
    ;; Separator dots
    (let ((dots-length (max 2 (- 25 (length field-name)))))
      (insert (propertize (concat " " (make-string dots-length ?.)) 
                          'face `(:foreground ,(supertag-view-helper-get-muted-color)))))
    ;; Field value
    (insert " " formatted-value)
    ;; Right border and padding
    (let ((line-length (- (point) start-pos 2))
          (target-length 56))
      (when (< line-length target-length)
        (insert (make-string (- target-length line-length) ?\ ))))
    (insert (propertize " │\n" 'face `(:foreground ,border-color)))
    ;; Add text properties for interactivity
    (when interactive-props
      (add-text-properties start-pos (point) interactive-props))))

(defun supertag-view-helper-insert-separator-line (&optional style)
  "Insert a separator line with optional STYLE (:thin, :thick, :dotted)."
  (let ((char (pcase style
                (:thin "─")
                (:thick "━")
                (:dotted "┄")
                (_ "─")))
        (border-color (supertag-view-helper-get-border-color)))
    (insert (propertize "│" 'face `(:foreground ,border-color)))
    (insert (propertize (make-string 58 (string-to-char char)) 'face `(:foreground ,border-color)))
    (insert (propertize "│\n" 'face `(:foreground ,border-color)))))

;;; --- Interactive Line Highlighting ---

(defun supertag-view-helper-highlight-current-line ()
  "Highlight the current line for better visibility."
  (let ((inhibit-read-only t))
    (remove-overlays (point-min) (point-max) 'category 'current-line)
    (let ((overlay (make-overlay (line-beginning-position) (1+ (line-end-position)))))
      (overlay-put overlay 'category 'current-line)
      (overlay-put overlay 'face `(:background ,(supertag-view-helper-get-theme-adaptive-color "#F1F5F9" "#334155")
                                   :extend t
                                   :box (:line-width 1 :color ,(supertag-view-helper-get-accent-color))))
      ;; Add subtle animation effect
      (overlay-put overlay 'priority 100))))

(defun supertag-view-helper-unhighlight-all-lines ()
  "Remove all line highlighting."
  (remove-overlays (point-min) (point-max) 'category 'current-line))

(defun supertag-view-helper-enable-line-highlighting ()
  "Enable enhanced line highlighting for the current buffer."
  ;; Line highlighting disabled to prevent visual flickering during cursor movement
  ;; (supertag-view-helper-unhighlight-all-lines)
  ;; (add-hook 'post-command-hook #'supertag-view-helper-highlight-current-line nil t)
  )

;;; --- Status and Statistics Display ---

(defun supertag-view-helper-insert-status-badge (status &optional label)
  "Insert a status badge with STATUS and optional LABEL."
  (let* ((badge-text (or label (format "%s" status)))
         (badge-face (pcase status
                       ('active `(:background ,(supertag-view-helper-get-success-color)
                                  :foreground "white" :weight bold :box 2))
                       ('warning `(:background ,(supertag-view-helper-get-warning-color)
                                   :foreground "black" :weight bold :box 2))
                       ('error `(:background ,(supertag-view-helper-get-error-color)
                                 :foreground "white" :weight bold :box 2))
                       ('inactive `(:background ,(supertag-view-helper-get-muted-color)
                                    :foreground "white" :weight bold :box 2))
                       (_ `(:background ,(supertag-view-helper-get-accent-color)
                           :foreground "white" :weight bold :box 2)))))
    (insert " ")
    (insert (propertize (format " %s " badge-text) 'face badge-face))
    (insert " ")))

(defun supertag-view-helper-insert-stats-summary (stats)
  "Insert a statistics summary from STATS plist."
  (let ((total (or (plist-get stats :total) 0))
        (active (or (plist-get stats :active) 0))
        (modified (or (plist-get stats :modified) 0)))
    (insert (propertize "\n📊 Statistics: " 'face `(:weight bold :foreground ,(supertag-view-helper-get-emphasis-color))))
    (insert (propertize (format "Total: %d" total) 'face `(:weight bold :foreground ,(supertag-view-helper-get-accent-color))))
    (insert (propertize " • " 'face `(:foreground ,(supertag-view-helper-get-muted-color))))
    (insert (propertize (format "Active: %d" active) 'face `(:weight bold :foreground ,(supertag-view-helper-get-success-color))))
    (when (> modified 0)
      (insert (propertize " • " 'face `(:foreground ,(supertag-view-helper-get-muted-color))))
      (insert (propertize (format "Modified: %d" modified) 'face `(:weight bold :foreground ,(supertag-view-helper-get-warning-color)))))
    (insert "\n\n")))

;;; --- Help Text and Empty State ---

(defun supertag-view-helper-insert-help-text (text)
  "Insert help TEXT with consistent styling."
  (insert (propertize (format "    %s\n" text)
                      'face `(:foreground ,(supertag-view-helper-get-muted-color) :height 0.9))))

(defun supertag-view-helper-insert-empty-state (message)
  "Insert empty state MESSAGE with consistent styling."
  (insert (propertize (format "  %s\n" message)
                      'face `(:foreground ,(supertag-view-helper-get-muted-color) :slant italic))))

(defun supertag-view-helper-insert-field-help ()
  "Insert standard field help text."
  (supertag-view-helper-insert-help-text "[RET] Edit Value [E] Edit Field Definition [a] Add Field [d] Delete Field [M-↑/↓] Reorder"))

;;; --- Node and File Information Display ---

(defun supertag-view-helper-insert-node-info (title file)
  "Insert node information with TITLE and FILE in consistent format."
  (insert (propertize (format "    📄 %s\n" title) 'face '(:weight bold :foreground "default")))
  (when file
    (insert (propertize (format "    📁 %s\n" (file-name-nondirectory file))
                        'face `(:foreground ,(supertag-view-helper-get-muted-color) :slant italic)))))

;;; --- Footer and Navigation ---

(defun supertag-view-helper-insert-footer-start ()
  "Insert footer separator and navigation title."
  (insert (propertize (format "%s\n" supertag-view-helper--section-separator) 'face `(:foreground ,(supertag-view-helper-get-muted-color))))
  (insert (propertize "Navigation:\n" 'face '(:weight bold :foreground "default"))))

(defun supertag-view-helper-insert-footer-with-content (&rest help-lines)
  "Insert complete footer with custom HELP-LINES."
  (supertag-view-helper-insert-footer-start)
  (dolist (line help-lines)
    (supertag-view-helper-insert-help-text line)))

;;;----------------------------------------------------------------------
;;; Tag Text Operations
;;;----------------------------------------------------------------------

(defun supertag-view-helper-find-node-location (node-id)
  "Find the location of a node by its ID.
Returns a cons (POSITION . FILE-PATH) if found, nil otherwise."
  (when node-id
    ;; First try to get the node data from store
    (require 'supertag-ops-node)
    (when-let* ((node (supertag-node-get node-id)))
      (let ((file-path (plist-get node :file))
            (position (plist-get node :position)))
        (when (and file-path (file-exists-p file-path))
          (cons (or position 1) file-path))))))

(defun supertag-view-helper-at-tag-line-p ()
  "Check if the current line is a tag line (contains #tags)."
  (save-excursion
    (beginning-of-line)
    (looking-at-p (concat "^[ \t]*#[" supertag-view-helper--valid-tag-chars "]+"))))

(defun supertag-view-helper-find-tag-insertion-point ()
  "Find the best position to insert tags in the current node.
Returns the position where tags should be inserted."
  (save-excursion
    (org-back-to-heading t)
    (let ((drawer-end (supertag-view-helper--find-drawer-end))
          (existing-tag-line (supertag-view-helper--find-existing-tag-line)))
      (cond
       ;; If there's already a tag line, position at the end of it
       (existing-tag-line
        (goto-char existing-tag-line)
        (end-of-line)
        (point))
       
       ;; If there's a drawer, position after it
       (drawer-end
        (goto-char drawer-end)
        (unless (bolp) (forward-line 1))
        ;; Skip empty lines after drawer
        (while (and (not (eobp))
                    (not (org-at-heading-p))
                    (looking-at-p "^[ \t]*$"))
          (forward-line 1))
        (beginning-of-line)
        (point))
       
       ;; No drawer, position directly after headline
       (t
        (end-of-line)
        (insert "\n")
        (beginning-of-line)
        (point))))))

(defun supertag-view-helper--find-drawer-end ()
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

(defun supertag-view-helper--find-existing-tag-line ()
  "Find the position of an existing tag line in the current node."
  (save-excursion
    (org-back-to-heading t)
    (let ((drawer-end (supertag-view-helper--find-drawer-end))
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
        (when (and (looking-at-p (concat "^[ \t]*#[" supertag-view-helper--valid-tag-chars "]+"))
                  (not (looking-at-p "^[ \t]*#\\+")))  ; Exclude org keywords
          (setq tag-line-pos (point)))
        (forward-line 1))
      
      tag-line-pos)))

(defun supertag-view-helper-insert-tag-text (tag-name &optional position)
  "Insert tag text with intelligent spacing.
TAG-NAME is the tag name to display.
POSITION is optional insertion position.
This function works in any location within a node - heading or content area."
  (when position (goto-char position))
  (let* ((prev-char (char-before))
         (next-char (char-after))
         (at-tag-line (supertag-view-helper-at-tag-line-p))
         ;; Need space before if previous char exists and is not whitespace
         (need-space-before
          (and prev-char
               (not (memq prev-char '(?\s ?\t ?\n)))
               (or at-tag-line
                   ;; Check for alphanumeric or punctuation that needs separation
                   (and (>= prev-char ?a) (<= prev-char ?z))
                   (and (>= prev-char ?A) (<= prev-char ?Z))
                   (and (>= prev-char ?0) (<= prev-char ?9))
                   ;; Include common punctuation that should be separated
                   (memq prev-char '(?. ?, ?\; ?: ?! ?\? ?\)))
                   ;; Chinese/Japanese/Korean characters
                   (and (>= prev-char ?\u4e00) (<= prev-char ?\u9fff)))))
         ;; Need space after only if next char exists and is alphanumeric or chinese
         (need-space-after
          (and next-char
               (not (eq next-char ?\n))
               (not (memq next-char '(?\s ?\t ?. ?, ?\; ?: ?! ?\?)))
               (or (and (>= next-char ?a) (<= next-char ?z))
                   (and (>= next-char ?A) (<= next-char ?Z))
                   (and (>= next-char ?0) (<= next-char ?9))
                   ;; Chinese/Japanese/Korean characters
                   (and (>= next-char ?\u4e00) (<= next-char ?\u9fff))))))
    
    ;; Insert space before tag if needed
    (when need-space-before
      (insert " "))
    ;; Insert the tag
    (insert (concat "#" tag-name))
    
    ;; Insert space after tag if needed
    (when need-space-after
      (insert " "))))

(defun supertag-view-helper-remove-tag-text (tag-name)
  "Remove all occurrences of #TAG-NAME from the current node.
TAG-NAME is the tag name to remove."
  (save-excursion
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t) (point)))
          (regex (concat "#" (regexp-quote tag-name)))
          (case-fold-search nil)
          (removed-count 0))
      ;; Search and remove from headline line first
      (beginning-of-line)
      (while (re-search-forward regex (line-end-position) t)
        (replace-match "")
        (setq removed-count (1+ removed-count)))
      ;; Now scan the subtree
      (forward-line 1)
      (while (re-search-forward regex subtree-end t)
        (replace-match "")
        (setq removed-count (1+ removed-count)))
      removed-count)))

(defun supertag-view-helper-get-tag-at-point ()
  "Get the inline supertag name at point.
Returns the tag name (without #) if found, nil otherwise."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let* ((tag-re (concat "#[" supertag-view-helper--valid-tag-chars "]+"))
             (start (point)))
        (when (re-search-backward tag-re nil t)
          ;; Check if the point is within the found match
          (when (and (<= (match-beginning 0) start)
                     (> (match-end 0) start))
            ;; Extract the tag name, removing the leading '#' 
            (substring (match-string 0) 1)))))))

;;;----------------------------------------------------------------------
;;; Cross-File Tag Text Operations
;;;----------------------------------------------------------------------

(defun supertag-view-helper-remove-tag-text-from-files (tag-name files)
  "Remove all occurrences of #TAG-NAME from specified FILES.
TAG-NAME is the tag name to remove.
FILES is a list of file paths.
Returns the total number of instances removed."
  (let ((total-removed 0))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (let ((removed-count 0))
              (while (re-search-forward (concat "#" (regexp-quote tag-name)) nil t)
                (let ((start (match-beginning 0))
                      (end (match-end 0)))
                  ;; Only remove tags that are not in comments or code blocks
                  (unless (or (save-excursion
                                (goto-char start)
                                (org-in-src-block-p))
                             (save-excursion
                               (goto-char start)
                               (beginning-of-line)
                               (looking-at-p "^[ \t]*#\+")))
                    ;; Smart deletion with space handling
                    (let* ((prev-char (char-before start))
                           (next-char (char-after end))
                           (delete-start start)
                           (delete-end end))
                      ;; Extend deletion to include surrounding spaces if appropriate
                      (when (and prev-char (eq prev-char ?\s))
                        (setq delete-start (1- delete-start)))
                      (when (and next-char (eq next-char ?\s))
                        (setq delete-end (1+ delete-end)))
                      (delete-region delete-start delete-end)
                      (setq removed-count (1+ removed-count))))))
              (when (> removed-count 0)
                (save-buffer)
                (setq total-removed (+ total-removed removed-count))
                (message "Removed %d instances of #%s from %s" removed-count tag-name (file-name-nondirectory file))))))
    total-removed))))

(defun supertag-view-helper-rename-tag-text-in-files (old-tag-name new-tag-name files)
  "Rename all occurrences of #OLD-TAG-NAME to #NEW-TAG-NAME in specified FILES.
OLD-TAG-NAME is the current tag name.
NEW-TAG-NAME is the new tag name.
FILES is a list of file paths.
Returns the total number of instances renamed."
  (let ((total-renamed 0))
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (let ((renamed-count 0))
              (while (re-search-forward (concat "#" (regexp-quote old-tag-name)) nil t)
                ;; Only rename tags that are not in comments or code blocks
                (unless (or (save-excursion
                              (goto-char (match-beginning 0))
                              (org-in-src-block-p))
                           (save-excursion
                             (goto-char (match-beginning 0))
                             (beginning-of-line)
                             (looking-at-p "^[ \t]*#\+")))
                  (replace-match (concat "#" new-tag-name))
                  (setq renamed-count (1+ renamed-count))))
              (when (> renamed-count 0)
                (save-buffer)
                (setq total-renamed (+ total-renamed renamed-count))
                (message "Renamed %d instances from #%s to #%s in %s" 
                         renamed-count old-tag-name new-tag-name (file-name-nondirectory file))))))
    total-renamed)))) 

;;;----------------------------------------------------------------------
;;; Display Buffer Management
;;;----------------------------------------------------------------------

(defun supertag-view-helper-display-buffer-right (buffer)
  "Display BUFFER in a window to the right."
  (let ((window (display-buffer buffer '(display-buffer-in-side-window
                                          (side . right)
                                          (window-width . 0.4)))))
    (select-window window)
    (goto-char (point-min))))

(provide 'supertag-view-helper)

;;; supertag-view-helper.el ends here
