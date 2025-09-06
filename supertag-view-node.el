;;; org-supertag/supertag-view-node.el --- Node-centric view for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the node-centric view for Org-Supertag. It defines
;; a major mode and commands to display and interact with a single node's
;; metadata in a dedicated buffer.

;;; Code:

(require 'supertag-ops-node)
(require 'supertag-ops-tag)
(require 'supertag-ops-field)
(require 'supertag-ops-relation)
(require 'supertag-view-helper)
(require 'supertag-services-ui)

;;; --- Variables ---

(defvar-local supertag-view-node--current-node-id nil
  "The ID of the node currently displayed in the view buffer.")

;;; --- Visual Style Variables ---

;; These functions are now available in supertag-view-helper.el
;; Keeping these as convenience aliases for backward compatibility
(defun supertag-view-node--get-theme-adaptive-color (light-color dark-color)
  "Get color that adapts to current theme."
  (supertag-view-helper-get-theme-adaptive-color light-color dark-color))

(defun supertag-view-node--get-accent-color ()
  "Get accent color that works well in both light and dark themes."
  (supertag-view-helper-get-accent-color))

(defun supertag-view-node--get-emphasis-color ()
  "Get emphasis color that works well in both light and dark themes."
  (supertag-view-helper-get-emphasis-color))

;;; --- Mode Definition ---

(defvar supertag-view-node-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "SPC") 'scroll-up-command)
    (define-key map (kbd "S-SPC") 'scroll-down-command)
    (define-key map (kbd "M-v") 'scroll-down-command)
    (define-key map (kbd "C-v") 'scroll-up-command)
    (define-key map (kbd "M-<") 'beginning-of-buffer)
    (define-key map (kbd "M->") 'end-of-buffer)
    
    ;; Field editing
    (define-key map (kbd "RET") 'supertag-view-node-edit-at-point)
    (define-key map (kbd "E") 'supertag-view-node-edit-field-definition-at-point)
    (define-key map (kbd "a") 'supertag-view-node-add-field)
    (define-key map (kbd "d") 'supertag-view-node-remove-field-at-point)
    (define-key map (kbd "M-<up>") 'supertag-view-node-move-field-up)
    (define-key map (kbd "M-<down>") 'supertag-view-node-move-field-down)
    
    ;; Utility
    (define-key map (kbd "g") 'supertag-view-node-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "h") 'describe-mode)
    map)
  "Keymap for `supertag-view-node-mode'.")

(define-derived-mode supertag-view-node-mode special-mode "Supertag Node"
  "A modern major mode for viewing and editing an Org-Supertag node.

\{supertag-view-node-mode-map}

Key Bindings:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📝 Field Operations:
  RET     - Edit field value at point
  E       - Edit field definition (name, type) at point
  a       - Add new field to tag
  d       - Delete field at point

🧭 Navigation:
  j/k     - Move up/down by line (also n/p)
  SPC     - Scroll down one page
  S-SPC   - Scroll up one page (also M-v)
  C-v     - Scroll down one page
  M-<     - Jump to beginning of buffer
  M->     - Jump to end of buffer

🔧 Actions:
  g       - Refresh the view
  h       - Show this help (describe-mode)
  q       - Quit and close window

📋 Field Reordering (Future):
  M-↑     - Move field up
  M-↓     - Move field down

💡 Tips:
  - Click on any field value or name to edit it
  - Field types determine input validation and display format
  - Changes are saved automatically
  - Use Tab completion when available

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  :group 'org-supertag
  :keymap supertag-view-node-mode-map
  (setq-local buffer-read-only t)
  (setq-local mode-line-format
        '(" "
          (:propertize mode-name face (:weight bold :foreground "#0066CC"))
          " | 📄 "
          (:eval (let ((node-id (or supertag-view-node--current-node-id "None")))
                  (if (string= node-id "None")
                      (propertize node-id 'face '(:foreground "gray"))
                    (propertize (truncate-string-to-width node-id 20 nil nil "...") 
                               'face '(:weight bold)))))
          " | 🏷️ "
          (:eval (let ((count (supertag-view-node--count-fields supertag-view-node--current-node-id)))
                  (propertize (format "%d" count)
                              'face (if (> count 0) '(:foreground "#22C55E" :weight bold) '(:foreground "gray")))))
          " | 🔗 "
          (:eval (let ((refs (length (supertag-view-node--get-references supertag-view-node--current-node-id)))
                       (refd-by (length (supertag-view-node--get-referenced-by supertag-view-node--current-node-id))))
                  (propertize (format "%d→%d" refs refd-by)
                              'face (if (> (+ refs refd-by) 0) '(:foreground "#0066CC" :weight bold) '(:foreground "gray")))))
          " %[%p%] "))
  ;; Line highlighting disabled to prevent cursor movement flickering
  ;; (supertag-view-helper-enable-line-highlighting)
  )

;;; --- Helper Functions ---

;; Line highlighting functions are now in supertag-view-helper.el
;; Keeping these as convenience aliases for backward compatibility
(defun supertag-view-node--highlight-current-line ()
  "Highlight the current line for better visibility."
  (supertag-view-helper-highlight-current-line))

(defun supertag-view-node--unhighlight-all-lines ()
  "Remove all line highlighting."
  (supertag-view-helper-unhighlight-all-lines))

(defun supertag-view-node--count-fields (node-id)
  "Count the total number of fields for NODE-ID."
  (when node-id
    (let* ((relations (supertag-relation-find-by-from node-id :node-tag))
           (tag-ids (mapcar (lambda (rel) (plist-get rel :to)) relations))
           (field-count 0))
      (dolist (tag-id tag-ids)
        (let ((fields (supertag-tag-get-all-fields tag-id)))
          (setq field-count (+ field-count (length fields)))))
      field-count)))

(defun supertag-view-node--get-references (node-id)
  "Get references from NODE-ID to other nodes."
  (when-let* ((node (supertag-get (list :nodes node-id))))
    (plist-get node :ref-to)))

(defun supertag-view-node--get-referenced-by (node-id)
  "Get nodes that reference NODE-ID."
  (when-let* ((node (supertag-get (list :nodes node-id))))
    (plist-get node :ref-from)))

(defun supertag-view-node--format-display-value (value field-def)
  "Format VALUE for display with enhanced styling based on FIELD-DEF."
  (supertag-view-helper-format-field-value field-def value))

;;; --- Modern Rendering Functions ---

(defun supertag-view-node--insert-simple-header (node-data)
  "Insert a simple, clean header with NODE-DATA."
  (let* ((title (or (plist-get node-data :title) "Untitled Node"))
         (file (plist-get node-data :file))
         (node-id supertag-view-node--current-node-id)
         (field-count (supertag-view-node--count-fields node-id))
         (ref-count (+ (length (supertag-view-node--get-references node-id))
                       (length (supertag-view-node--get-referenced-by node-id))))
         (stats (format "⚡ %d fields | 🔗 %d refs" field-count ref-count)))
    (supertag-view-helper-insert-simple-header 
     (format "📄 %s" title)
     stats)
    (when file
      (insert (propertize (format "📁 %s\n\n" (file-name-nondirectory file))
                          'face `(:foreground ,(supertag-view-helper-get-muted-color) :slant italic))))))

;;; --- Rendering Functions ---

(defun supertag-view-node--insert-simple-metadata-section (node-id)
  "Insert a simple metadata section for NODE-ID."
  (let* ((relations (supertag-relation-find-by-from node-id :node-tag))
         (tag-ids (sort (mapcar (lambda (rel) (plist-get rel :to)) relations) #'string<))
         (deleted-tags '())
         (valid-tags '()))
    
    ;; Separate valid and deleted tags
    (dolist (tag-id tag-ids)
      (let ((tag-data (supertag-tag-get tag-id)))
        (if tag-data
            (push tag-id valid-tags)
          (push tag-id deleted-tags))))
    
    ;; Clean up relations for deleted tags
    (when deleted-tags
      (dolist (deleted-tag-id deleted-tags)
        (let ((stale-relations (supertag-relation-find-by-from node-id :node-tag)))
          (dolist (rel stale-relations)
            (when (equal (plist-get rel :to) deleted-tag-id)
              (supertag-relation-delete (plist-get rel :id))
              (message "Cleaned up stale relation to deleted tag: %s" deleted-tag-id))))))
    
    ;; Display content with simple styling
    (if (and (not valid-tags) (not deleted-tags))
        (progn
          (supertag-view-helper-insert-section-title "Metadata" "🏷️")
          (supertag-view-helper-insert-simple-empty-state "No metadata found."))
      
      ;; Display valid tags with simple blocks
      (supertag-view-helper-insert-section-title "Metadata" "🏷️")
      (dolist (tag-id (sort valid-tags #'string<))
        (let ((fields (supertag-tag-get-all-fields tag-id)))
          (supertag-view-helper-insert-tag-block tag-id fields node-id)))
      
      ;; Show cleaned up deleted tags if any
      (when deleted-tags
        (insert (propertize (format "🗑️ Cleaned up %d deleted tag%s\n\n" 
                                    (length deleted-tags)
                                    (if (= (length deleted-tags) 1) "" "s"))
                            'face `(:foreground ,(supertag-view-helper-get-warning-color))))))))

(defun supertag-view-node--insert-simple-references-section (node-id)
  "Insert a simple references section for NODE-ID."
  (let* ((refs-to (supertag-view-node--get-references node-id))
         (refs-from (supertag-view-node--get-referenced-by node-id))
         (total-refs (+ (length (or refs-to '())) (length (or refs-from '())))))
    
    (when (> total-refs 0)
      (supertag-view-helper-insert-section-title "References" "🔗")
      
      ;; References To
      (when (and refs-to (> (length refs-to) 0))
        (insert (propertize (format "  → References %d node%s\n" 
                                   (length refs-to)
                                   (if (= (length refs-to) 1) "" "s"))
                           'face `(:foreground ,(supertag-view-helper-get-accent-color))))
        (dolist (ref-id refs-to)
          (when-let* ((node (supertag-get (list :nodes ref-id)))
                      (title (plist-get node :title)))
            (insert (propertize (format "    📄 %s\n" (or title "[Untitled]"))
                               'face `(:foreground ,(supertag-view-helper-get-muted-color)))))))
      
      ;; Referenced By
      (when (and refs-from (> (length refs-from) 0))
        (insert (propertize (format "  ← Referenced by %d node%s\n" 
                                   (length refs-from)
                                   (if (= (length refs-from) 1) "" "s"))
                           'face `(:foreground ,(supertag-view-helper-get-accent-color))))
        (dolist (ref-id refs-from)
          (when-let* ((node (supertag-get (list :nodes ref-id)))
                      (title (plist-get node :title)))
            (insert (propertize (format "    📄 %s\n" (or title "[Untitled]"))
                               'face `(:foreground ,(supertag-view-helper-get-muted-color)))))))
      
      (insert "\n"))))

;; Add advanced editing functions
(defun supertag-view-node--get-field-info-at-point ()
  "Return plist of field info at point, or nil."
  (let* ((pos (point))
         (fallback-pos (max (point-min) (1- pos)))
         (tag-id     (or (get-text-property pos 'tag-id)
                         (get-text-property fallback-pos 'tag-id)))
         (field-name (or (get-text-property pos 'field-name)
                         (get-text-property fallback-pos 'field-name)))
         (node-id supertag-view-node--current-node-id))
    (when (and tag-id field-name node-id)
      (let* ((field-def (supertag-tag-get-field tag-id field-name))
             (value (when field-def
                      (supertag-field-get node-id tag-id field-name))))
        (list :node-id   node-id
              :tag-id    tag-id
              :field-name field-name
              :field-def field-def
              :value     value)))))

(defun supertag-view-node-edit-field-definition-at-point ()
  "Edit the field definition at the current point."
  (interactive)
  (when-let* ((field-info (supertag-view-node--get-field-info-at-point))
              (tag-id (plist-get field-info :tag-id))
              (field-name (plist-get field-info :field-name))
              (field-def (plist-get field-info :field-def)))
    (when (and tag-id field-name field-def)
      (let* ((current-type (plist-get field-def :type))
             (action (completing-read "Edit: " '("Name" "Type") nil t)))
        (cond
         ((string= action "Name")
          (let ((new-name (read-string (format "New name for field '%s': " field-name) nil nil field-name)))
            (when (and new-name (not (string-empty-p new-name)) (not (string= new-name field-name)))
              (supertag-tag-rename-field tag-id field-name new-name)
              (supertag-view-node-refresh)
              (message "✓ Field renamed from '%s' to '%s'" field-name new-name))))
         ((string= action "Type")
          (let* ((type-and-options (supertag-field-read-type-with-options current-type))
                 (new-type (car type-and-options))
                 (options (cdr type-and-options))
                 (new-field-def (list :name field-name :type new-type)))
            ;; Add options for :options type
            (when (eq new-type :options)
              (setq new-field-def (plist-put new-field-def :options options)))
            (when (supertag-tag-add-field tag-id new-field-def)  ; This will update the existing field
              (supertag-view-node-refresh)))))))))

(defun supertag-view-node-move-field-up ()
  "Move the field at point up in its tag's field list."
  (interactive)
  (let ((context (supertag-view-node--get-field-info-at-point)))
    (when (and context
               (plist-get context :tag-id)
               (plist-get context :field-name))
      (let ((tag-id (plist-get context :tag-id))
            (field-name (plist-get context :field-name)))
        (supertag-tag-move-field-up tag-id field-name)
        (supertag-view-node-refresh)
        (message "Field '%s' moved up in tag '%s'." field-name tag-id)))))

(defun supertag-view-node-move-field-down ()
  "Move the field at point down in its tag's field list."
  (interactive)
  (let ((context (supertag-view-node--get-field-info-at-point)))
    (when (and context
               (plist-get context :tag-id)
               (plist-get context :field-name))
      (let ((tag-id (plist-get context :tag-id))
            (field-name (plist-get context :field-name)))
        (supertag-tag-move-field-down tag-id field-name)
        (supertag-view-node-refresh)
        (message "Field '%s' moved down in tag '%s'." field-name tag-id)))))

(defun supertag-view-node--render (node-id)
  "Render a simple, clean view for NODE-ID."
  (let ((node-data (supertag-get (list :nodes node-id)))
        (inhibit-read-only t))
    (erase-buffer)
    (setq supertag-view-node--current-node-id node-id)
    (when node-data
      ;; Simple header
      (supertag-view-node--insert-simple-header node-data)
      
      ;; Simple metadata section
      (supertag-view-node--insert-simple-metadata-section node-id)
      
      ;; Simple references section
      (supertag-view-node--insert-simple-references-section node-id)
      
      ;; Complete footer with all available shortcuts
      (supertag-view-helper-insert-simple-footer
       "⌨️ Field: [RET] Edit Value | [E] Edit Definition | [a] Add | [d] Delete"
       "📍 Navigation: [j/k] Move | [SPC] Page Down | [S-SPC] Page Up | [M-</>] Start/End"
       "🔧 Actions: [g] Refresh | [h] Help | [q] Quit"))
    
    (goto-char (point-min))))

;;; --- Interactive Functions ---

(defun supertag-view-node--get-context-at-point ()
  "Return a plist of supertag context at point, or nil."
  (when (get-text-property (point) 'supertag-context)
    (list :type (get-text-property (point) 'type)
          :tag-id (get-text-property (point) 'tag-id)
          :field-name (get-text-property (point) 'field-name)
          :id (get-text-property (point) 'id))))

(defun supertag-view-node-edit-at-point ()
  "Dispatch edit action based on the context at point."
  (interactive)
  (let ((context (supertag-view-node--get-context-at-point)))
    (when context
      (pcase (plist-get context :type)
        (:field-value (supertag-view-node--edit-field-value context))
        ;; TODO: Add other cases for editing tag, field-key etc.
        (_ (message "No edit action defined for this context."))))))

(defun supertag-view-node--edit-field-value (context)
  "Handle the logic to edit a field's value with enhanced UI feedback."
  (let* ((node-id supertag-view-node--current-node-id)
         (tag-id (plist-get context :tag-id))
         (field-name (plist-get context :field-name))
         (field-def (supertag-tag-get-field tag-id field-name))
         (current-value (supertag-field-get node-id tag-id field-name)))
    (when field-def
      (let ((new-value (supertag-ui-read-field-value field-def current-value)))
        ;; We don't check for nil, allowing user to clear a value
        (supertag-field-set node-id tag-id field-name new-value)
        (supertag-view-node-refresh)
        (message "✓ Field '%s' updated successfully!" field-name)))))

(defun supertag-view-node-add-field ()
  "Add a new field definition to a tag on the current node."
  (interactive)
  (let* ((node-id supertag-view-node--current-node-id)
         (tag-id (supertag-ui-select-tag-on-node node-id)))
    (when tag-id
      (let ((field-def (supertag-ui-create-field-definition)))
        (when field-def
          (supertag-tag-add-field tag-id field-def)
          (supertag-view-node-refresh)
          (message "Field '%s' added to tag '%s'." (plist-get field-def :name) tag-id)))))) 


(defun supertag-view-node-remove-field-at-point ()
  "Remove the field definition at the current point from its tag."
  (interactive)
  (let ((context (supertag-view-node--get-context-at-point)))
    (when (and context
               (or (eq (plist-get context :type) :field-key)
                   (eq (plist-get context :type) :field-value))
               (plist-get context :tag-id)
               (plist-get context :field-name))
      (let ((tag-id (plist-get context :tag-id))
            (field-name (plist-get context :field-name)))
        (when (yes-or-no-p (format "Remove field '%s' from tag '%s'?" field-name tag-id))
          (supertag-tag-remove-field tag-id field-name)
          (supertag-view-node-refresh))))))


;;; --- Commands ---

(defun supertag-view-node-refresh ()
  "Refresh the node view buffer."
  (interactive)
  (when supertag-view-node--current-node-id
    (supertag-view-node--render supertag-view-node--current-node-id)))

(defun supertag-view-node ()
  "Display the Org-Supertag node view for the node at point."
  (interactive)
  (let* ((node-id (supertag-ui--get-node-at-point))
         (buffer-name (format "Supertag: %s" node-id))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (supertag-view-node-mode)
      (supertag-view-node--render node-id))
    (supertag-view-helper-display-buffer-right buffer)))

(provide 'supertag-view-node)

;;; supertag-view-node.el ends here
