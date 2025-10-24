;;; org-supertag/supertag-view-node.el --- Node-centric view for Org-Supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the node-centric view for Org-Supertag. It defines
;; a major mode and commands to display and interact with a single node's
;; metadata in a dedicated buffer.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-store)
(require 'supertag-ops-node)
(require 'supertag-ops-tag)
(require 'supertag-ops-field)
(require 'supertag-ops-relation)
(require 'supertag-ui-commands) ; For backlink helpers
(require 'supertag-view-helper)
(require 'supertag-services-ui)

;;; --- Variables ---

(defvar-local supertag-view-node--current-node-id nil
  "The ID of the node currently displayed in the view buffer.")

(defcustom supertag-view-node-strip-todo-keywords t
  "Whether to strip TODO keywords from node titles in view buffers.
If non-nil, TODO keywords will be removed from titles.
If nil, titles will be displayed as-is with TODO keywords."
  :type 'boolean
  :group 'org-supertag)

(defcustom supertag-view-node-todo-keywords
  '("TODO" "DONE" "NEXT" "WAITING" "HOLD" "CANCELLED" "CANCELED" 
    "STARTED" "DELEGATED" "DEFERRED" "SOMEDAY")
  "List of TODO keywords to strip from node titles.
Only used when `supertag-view-node-strip-todo-keywords' is non-nil.
You can customize this list to match your org-mode TODO keywords."
  :type '(repeat string)
  :group 'org-supertag)

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
    
    ;; Field value editing
    (define-key map (kbd "RET") 'supertag-view-node-edit-at-point)
    
    ;; Utility
    (define-key map (kbd "g") 'supertag-view-node-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "h") 'describe-mode)
    ;; Debug
    (define-key map (kbd "?") 'supertag-view-node-debug-field-at-point)
    map)
  "Keymap for `supertag-view-node-mode'.")

(define-derived-mode supertag-view-node-mode special-mode "Supertag Node"
  "A modern major mode for viewing and editing an Org-Supertag node.

\{supertag-view-node-mode-map}

Key Bindings:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📝 Field Operations:
  RET     - Edit field value at point. All schema changes (adding, deleting,
            or reordering fields) should be done in the Schema View (M-x supertag-view-schema).

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
  
  ;; Subscribe to store changes for auto-refresh
  (supertag-view-node--subscribe-to-events))

;;; --- Event Subscription ---

(defun supertag-view-node--subscribe-to-events ()
  "Subscribe to store events for auto-refresh.
This implements the correct separation: data layer emits events, UI subscribes."
  (when (fboundp 'supertag-subscribe)
    ;; Subscribe to store changes
    (supertag-subscribe :store-changed #'supertag-view-node--handle-store-change)))

(defun supertag-view-node--handle-store-change (path old-value new-value)
  "Handle store change events and refresh if relevant.
PATH is the change path, e.g., (:nodes node-id) or (:relations rel-id).
OLD-VALUE is the value before change (used for deletions).
NEW-VALUE is the value after change (nil for deletions)."
  (when (and supertag-view-node--current-node-id
             (listp path)
             (>= (length path) 2))
    (let* ((entity-type (car path))
           (entity-id (cadr path))
           ;; For relations, use new-value if available, otherwise use old-value
           ;; This is critical for detecting deletions!
           (relation-data (when (eq entity-type :relations)
                           (or new-value old-value))))
      ;; Refresh if:
      ;; 1. The changed entity is the current node
      ;; 2. The changed entity is a relation involving the current node (create/update/delete)
      ;; 3. The changed entity is a field of the current node
      (when (or (and (eq entity-type :nodes) 
                     (equal entity-id supertag-view-node--current-node-id))
                (and (eq entity-type :relations)
                     relation-data
                     (or (equal (plist-get relation-data :from) supertag-view-node--current-node-id)
                         (equal (plist-get relation-data :to) supertag-view-node--current-node-id)))
                (and (eq entity-type :fields)
                     (equal entity-id supertag-view-node--current-node-id)))
        ;; Refresh after a short delay to batch multiple rapid changes
        (run-with-idle-timer 0.1 nil #'supertag-view-node-refresh)))))

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
  (when node-id
    (let ((relations (supertag-relation-find-by-from node-id :reference)))
      (mapcar (lambda (rel) (plist-get rel :to)) relations))))

(defun supertag-view-node--get-referenced-by (node-id)
  "Get nodes that reference NODE-ID."
  (when node-id
    (let ((relations (supertag-store-get-collection :relations))
          (referencing-ids '()))
      (maphash
       (lambda (_ rel-data)
         (let ((relation (if (hash-table-p rel-data)
                             (let (plist)
                               (maphash (lambda (k v)
                                          (setq plist (plist-put plist k v)))
                                        rel-data)
                               plist)
                           rel-data)))
           (when (and (eq (plist-get relation :type) :reference)
                      (equal (plist-get relation :to) node-id))
             (push (plist-get relation :from) referencing-ids))))
       relations)
      (nreverse referencing-ids))))

(defun supertag-view-node--format-display-value (value field-def)
  "Format VALUE for display with enhanced styling based on FIELD-DEF."
  (supertag-view-helper-format-field-value field-def value))

;;; --- Modern Rendering Functions ---

(defun supertag-view-node--strip-todo-keyword (title)
  "Remove TODO keywords from TITLE if configured to do so.
Removes org-mode TODO keywords based on `supertag-view-node-todo-keywords'.
Only strips keywords if `supertag-view-node-strip-todo-keywords' is non-nil."
  (if (not supertag-view-node-strip-todo-keywords)
      title
    (let ((keywords-regexp (concat "^\\("
                                   (mapconcat #'regexp-quote 
                                             supertag-view-node-todo-keywords
                                             "\\|")
                                   "\\)\\s-+")))
      (if (string-match keywords-regexp title)
          (string-trim (substring title (match-end 0)))
        title))))

(defun supertag-view-node--insert-simple-header (node-data)
  "Insert a simple, clean header with NODE-DATA."
  (let* ((raw-title (or (plist-get node-data :title) "Untitled Node"))
         (title (supertag-view-node--strip-todo-keyword raw-title))
         (file (plist-get node-data :file))
         (node-id supertag-view-node--current-node-id)
         (field-count (supertag-view-node--count-fields node-id))
         (ref-count (+ (length (supertag-view-node--get-references node-id))
                       (length (supertag-view-node--get-referenced-by node-id))))
         (stats (format "⚡ %d fields | 🔗 %d refs" field-count ref-count)))
    (supertag-view-helper-insert-simple-header 
     (format "📄 %s" (supertag-view-helper-render-org-links title))
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
          ;; Defensively ensure `fields` is a list to prevent rendering errors.
          (supertag-view-helper-insert-tag-block tag-id (or fields '()) node-id)))
      
      ;; Show cleaned up deleted tags if any
      (when deleted-tags
        (insert (propertize (format "🗑️ Cleaned up %d deleted tag%s\n\n" 
                                    (length deleted-tags)
                                    (if (= (length deleted-tags) 1) "" "s"))
                            'face `(:foreground ,(supertag-view-helper-get-warning-color))))))))

(defun supertag-view-node--insert-simple-references-section (node-id)
  "Insert a simple references section for NODE-ID, always showing the section."
  (let* ((refs-to (supertag-view-node--get-references node-id))
         (refs-from (supertag-view-node--get-referenced-by node-id))
         (total-refs (+ (length (or refs-to '())) (length (or refs-from '())))))

    (supertag-view-helper-insert-section-title "References" "🔗")

    (if (> total-refs 0)
        (progn
          ;; References To
          (when (and refs-to (> (length refs-to) 0))
            (insert (propertize (format "  → References %d node%s\n"
                                       (length refs-to)
                                       (if (= (length refs-to) 1) "" "s"))
                               'face `(:foreground ,(supertag-view-helper-get-accent-color))))
            (dolist (ref-id refs-to)
          (when-let* ((node (supertag-node-get ref-id))
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
          (when-let* ((node (supertag-node-get ref-id))
                      (title (plist-get node :title)))
                (insert (propertize (format "    📄 %s\n" (or title "[Untitled]"))
                                   'face `(:foreground ,(supertag-view-helper-get-muted-color)))))))
          (insert "\n"))
      ;; Else, show empty state
      (supertag-view-helper-insert-simple-empty-state "No references found."))))

;; Add advanced editing functions
(defun supertag-view-node-debug-field-at-point ()
  "Debug function to show field information at point."
  (interactive)
  (let* ((pos (point))
         (fallback-pos (max (point-min) (1- pos)))
         (tag-id     (or (get-text-property pos 'tag-id)
                         (get-text-property fallback-pos 'tag-id)))
         (field-name (or (get-text-property pos 'field-name)
                         (get-text-property fallback-pos 'field-name)))
         (node-id supertag-view-node--current-node-id)
         (context (get-text-property pos 'supertag-context))
         (type (get-text-property pos 'type)))
    (message "Debug: pos=%d, tag-id=%s, field-name=%s, node-id=%s, context=%s, type=%s"
             pos tag-id field-name node-id context type)))



(defun supertag-view-node--disallow-definition-edit ()
  "Signal that field definition edits are not available in node view."
  (user-error "Field definitions are read-only here; use `supertag-view-schema' instead."))

(defun supertag-view-node-edit-field-definition-at-point ()
  "Edit the field definition at the current point."
  (interactive)
  (supertag-view-node--disallow-definition-edit))

(defun supertag-view-node-move-field-up ()
  "Move the field at point up in its tag's field list."
  (interactive)
  (supertag-view-node--disallow-definition-edit))

(defun supertag-view-node-move-field-down ()
  "Move the field at point down in its tag's field list."
  (interactive)
  (supertag-view-node--disallow-definition-edit))

(defun supertag-view-node--render (node-id)
  "Render a simple, clean view for NODE-ID."
  (let ((node-data (supertag-node-get node-id))
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
       "⌨️ Field: [RET] Edit Value"
       "📍 Navigation: [j/k] Move | [SPC] Page Down | [S-SPC] Page Up | [M-</>] Start/End"
       "🔧 Actions: [g] Refresh | [h] Help | [q] Quit")

      ;; Activate links in the entire buffer
      (supertag-view-node--activate-links-in-buffer))
    
    (goto-char (point-min))))

;;; --- Link Activation ---

(defun supertag-view-node--activate-links-in-buffer ()
  "Find all [[id:...]] links in the buffer and make them clickable."
  (goto-char (point-min))
  (let ((inhibit-read-only t))
    (while (re-search-forward "\[\[id:\([0-9A-Za-z-]+\)\]\[\(.*?\)\]\]" nil t)
      (let* ((id (match-string 1))
             (desc (match-string 2))
             (action `(lambda () (interactive) (supertag-goto-node ,id)))
             (map (make-sparse-keymap)))
        (define-key map [mouse-1] action)
        (define-key map (kbd "RET") action)

        (add-text-properties (match-beginning 0) (match-end 0)
                             `(display ,desc
                               face org-link
                               keymap ,map
                               help-echo ,(format "Jump to node ID: %s" id)))))))

;;; --- Interactive Functions ---

(defun supertag-view-node--get-context-at-point ()
  "Return a plist of supertag context at point, or nil."
  (let ((pos (point)))
    ;; Check current point first, then fallback to point before it.
    (unless (get-text-property pos 'supertag-context)
      (setq pos (max (point-min) (1- pos))))
    (when (get-text-property pos 'supertag-context)
      (list :type (get-text-property pos 'type)
            :tag-id (get-text-property pos 'tag-id)
            :field-name (get-text-property pos 'field-name)
            :id (get-text-property pos 'id)))))

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
  "Handle the logic to edit a field's value with enhanced UI feedback.
Handles special logic for :node-reference fields."
  (let* ((node-id supertag-view-node--current-node-id)
         (tag-id (plist-get context :tag-id))
         (field-name (plist-get context :field-name))
         (field-def (supertag-tag-get-field tag-id field-name)))
    (when field-def
      (let* ((field-type (plist-get field-def :type))
             (current-value (supertag-field-get-with-default node-id tag-id field-name))
             (new-value (supertag-ui-read-field-value field-def current-value)))

        ;; Handle :node-reference side effects
        (when (eq field-type :node-reference)
          (let* ((current-targets (supertag-field-normalize-node-reference-list current-value))
                 (new-targets (supertag-field-normalize-node-reference-list new-value))
                 (removed (cl-set-difference current-targets new-targets :test #'string=))
                 (added (cl-set-difference new-targets current-targets :test #'string=)))
            ;; Remove stale relations and backlinks
            (dolist (target removed)
              (dolist (rel (supertag-relation-find-between node-id target :field-reference))
                (when (and (equal (plist-get rel :tag-id) tag-id)
                           (equal (plist-get rel :field-name) field-name))
                  (supertag-relation-delete (plist-get rel :id))))
              (supertag-ui--remove-link-under-node target node-id))
            ;; Add new relations and backlinks
            (when added
              (let ((from-node-title (plist-get (supertag-node-get node-id) :title)))
                (dolist (target added)
                  (supertag-relation-create `(:type :field-reference
                                              :from ,node-id
                                              :to ,target
                                              :tag-id ,tag-id
                                              :field-name ,field-name))
                  (supertag-ui--insert-link-under-node target node-id from-node-title)))))))

        ;; Set the field value (for all types)
        (supertag-field-set node-id tag-id field-name new-value)
        (supertag-view-node-refresh)
        (message "✓ Field '%s' updated successfully!" field-name))))

(defun supertag-view-node-add-field ()
  "Add a new field definition to a tag on the current node."
  (interactive)
  (supertag-view-node--disallow-definition-edit))


(defun supertag-view-node-remove-field-at-point ()
  "Remove the field definition at the current point from its tag."
  (interactive)
  (supertag-view-node--disallow-definition-edit))


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
         (buffer-name "*Supertag Node View*")
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (supertag-view-node-mode)
      (supertag-view-node--render node-id))
    (supertag-view-helper-display-buffer-right buffer)))

(provide 'supertag-view-node)

;;; supertag-view-node.el ends here
