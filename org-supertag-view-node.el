;;; org-supertag-view-node.el --- Node-centric view for org-supertag -*- lexical-binding: t; -*-

(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-tag)
(require 'org-supertag-field)
(require 'org-supertag-view-utils)

;;----------------------------------------------------------------------
;; Variables
;;----------------------------------------------------------------------

(defvar org-supertag-view-node--current-node-id nil
  "Current node ID being viewed in the node view buffer.")

;;----------------------------------------------------------------------
;; Visual Style Variables
;;----------------------------------------------------------------------

(defvar org-supertag-view-node--section-separator "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  "Separator line for sections.")

(defvar org-supertag-view-node--subsection-separator "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"
  "Separator line for subsections.")

(defun org-supertag-view-node--get-theme-adaptive-color (light-color dark-color)
  "Get color that adapts to current theme."
  (if (eq (frame-parameter nil 'background-mode) 'dark)
      dark-color
    light-color))

(defun org-supertag-view-node--get-accent-color ()
  "Get accent color that works well in both light and dark themes."
  (org-supertag-view-node--get-theme-adaptive-color "blue" "cyan"))

(defun org-supertag-view-node--get-emphasis-color ()
  "Get emphasis color that works well in both light and dark themes."
  (org-supertag-view-node--get-theme-adaptive-color "darkblue" "lightblue"))

;;----------------------------------------------------------------------
;; Mode Definition
;;----------------------------------------------------------------------

(defvar org-supertag-view-node-mode-map
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
    (define-key map (kbd "RET") 'org-supertag-view-node-action-at-point)
    (define-key map (kbd "E") 'org-supertag-view-node-edit-field-definition-at-point)
    (define-key map (kbd "a") 'org-supertag-view-node-add-field)
    (define-key map (kbd "d") 'org-supertag-view-node-remove-field-at-point)
    (define-key map (kbd "M-<up>") 'org-supertag-view-node-move-field-up)
    (define-key map (kbd "M-<down>") 'org-supertag-view-node-move-field-down)
    
    ;; AI features
    (define-key map (kbd "s") 'org-supertag-view-node-suggest-tags)
    (define-key map (kbd "c") 'org-supertag-view-node-chat-with-node)
    
    ;; Utility
    (define-key map (kbd "g") 'org-supertag-view-node-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "h") 'describe-mode)
    map)
  "Keymap for `org-supertag-view-node-mode'.")

(define-derived-mode org-supertag-view-node-mode special-mode "Org-Supertag-Node-View"
  "Major mode for the unified node view."
  :group 'org-supertag
  (setq mode-line-format
        '(" " mode-name " | "
          (:eval (format "Node: %s" (or org-supertag-view-node--current-node-id "None")))
          " | "
          (:eval (format "Fields: %d" (length (org-supertag-node-get-tags org-supertag-view-node--current-node-id))))
          " | "
          (:eval (format "Refs: %d" (length (org-supertag-view-node--get-references org-supertag-view-node--current-node-id))))
          " | "
          (:eval (format "RefdBy: %d" (length (org-supertag-view-node--get-referenced-by org-supertag-view-node--current-node-id))))
          " | %[%p%] "))
  ;; Enable line highlighting
  (org-supertag-view-node--unhighlight-all-lines)
  (add-hook 'post-command-hook #'org-supertag-view-node--highlight-current-line nil t))

;;----------------------------------------------------------------------
;; Core Functions
;;----------------------------------------------------------------------

(defun org-supertag-view-node--highlight-current-line ()
  "Highlight the current line for better visibility."
  (let ((inhibit-read-only t))
    (remove-overlays (point-min) (point-max) 'category 'current-line)
    (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
      (overlay-put overlay 'category 'current-line)
      (overlay-put overlay 'face '(:background "lightgray" :foreground "black")))))

(defun org-supertag-view-node--unhighlight-all-lines ()
  "Remove all line highlighting."
  (remove-overlays (point-min) (point-max) 'category 'current-line))

(add-hook 'post-command-hook #'org-supertag-view-node--highlight-current-line nil t)

(defun org-supertag-view-node--get-references (node-id)
  (when-let* ((node (org-supertag-db-get node-id)))
    (plist-get node :ref-to)))

(defun org-supertag-view-node--get-referenced-by (node-id)
  (when-let* ((node (org-supertag-db-get node-id)))
    (plist-get node :ref-from)))

;;----------------------------------------------------------------------
;; Enhanced Formatting Functions
;;----------------------------------------------------------------------

(defun org-supertag-view-node--insert-section-header (title icon)
  "Insert a section header with icon and title."
  (insert (propertize (format "%s %s\n" icon title) 'face '(:weight bold :height 1.2 :foreground "default")))
  (insert (propertize (format "%s\n" org-supertag-view-node--section-separator) 'face '(:foreground "gray50")))
  (insert "\n"))

(defun org-supertag-view-node--insert-subsection-header (title)
  "Insert a subsection header."
  (insert (propertize (format "  %s\n" title) 'face '(:weight bold :foreground "default")))
  (insert (propertize (format "  %s\n" org-supertag-view-node--subsection-separator) 'face '(:foreground "gray70")))
  (insert "\n"))

(defun org-supertag-view-node--format-node-content (node-id)
  (when-let* ((node (org-supertag-db-get node-id))
              (title (plist-get node :title))
              (file-path (plist-get node :file-path))
              (content (or (plist-get node :content) "")))
    (let* ((file-name (file-name-nondirectory file-path))
           (styled-title (propertize title 'face '(:weight bold :foreground "default")))
           (styled-file (propertize (format "(%s)" file-name) 'face '(:foreground "gray60" :slant italic))))
      (format "    📄 %s %s\n    %s\n\n" styled-title styled-file 
              (if (string-empty-p content) 
                  (propertize "[No content]" 'face '(:foreground "gray50" :slant italic))
                (let ((truncated (substring content 0 (min 60 (length content)))))
                  (if (> (length content) 60)
                      (concat truncated "...")
                    truncated)))))))

(defun org-supertag-view-node--format-display-value (value)
  "Format VALUE for display with enhanced styling."
  (let ((formatted-value (if (listp value)
                              (mapconcat #'identity value " / ")
                            (format "%s" (or value "")))))
    (if (string-empty-p formatted-value)
        (propertize "[Empty]" 'face '(:foreground "gray50" :slant italic))
      (propertize formatted-value 'face `(:foreground ,(org-supertag-view-node--get-accent-color))))))

(defun org-supertag-view-node--insert-metadata-section (node-id)
  (org-supertag-view-node--insert-section-header "Metadata" "🏷️")
  (let ((tags (org-supertag-node-get-tags node-id)))
    (if (not tags)
        (insert (propertize "  No metadata found.\n" 'face '(:foreground "gray50" :slant italic)))
              (dolist (tag-id (sort tags #'string<))
          (insert (propertize (format "  📌 %s\n" tag-id) 'face '(:weight bold :foreground "default") 'tag-id tag-id 'field-name nil))
        (when-let* ((tag-def (org-supertag-tag-get tag-id))
                    (fields (org-supertag-get-all-fields-for-tag tag-id)))
          (if (not fields)
              (insert (propertize "    No fields defined.\n" 'face '(:foreground "gray50" :slant italic)))
            (dolist (field-def fields)
              (let* ((field-name (plist-get field-def :name))
                     (value (org-supertag-field-get-value node-id field-name tag-id))
                     (display-value (org-supertag-view-node--format-display-value value))
                     (start (point)))
                (insert (propertize (format "    %s: " field-name)
                                   'face '(:weight bold :foreground "default")))
                (insert (format "%s\n" display-value))
                (add-text-properties start (point)
                                   `(field-name ,field-name
                                     tag-id ,tag-id
                                     field-def ,field-def
                                     face (:height 1.0))))))))
      (insert "\n")
      ;; Add tag suggestion button
      (let ((start (point)))
        (insert (propertize "    💡 Get AI Tag Suggestions" 'face '(:foreground "blue" :weight bold)))
        (insert "\n")
        (add-text-properties start (point)
                           `(action suggest-tags
                             mouse-face highlight
                             help-echo "Click to get AI tag suggestions for this node")))
      (insert "\n")
      (insert (propertize "    [RET] Edit Value [E] Edit Field Definition [a] Add Field [d] Delete Field [M-<up>/<down>] Reorder\n" 
                          'face '(:foreground "gray60" :height 0.9))))))

(defun org-supertag-view-node--insert-relations-section (node-id)
  (org-supertag-view-node--insert-section-header "Relations" "🔗")
  (let ((tags (org-supertag-node-get-tags node-id))
        (relations '()))
    (dolist (tag-id tags)
      (let ((tag-relations (org-supertag-relation-get-all tag-id 'cooccurrence)))
        (setq relations (append relations tag-relations))))
    (if relations
        (dolist (rel relations)
          (let* ((source-name (org-supertag-tag-get-name-by-id (plist-get rel :from)))
                 (target-name (org-supertag-tag-get-name-by-id (plist-get rel :to))))
            ;; Only display the relation if both tags actually exist.
            ;; This prevents errors from stale relations pointing to deleted tags.
            (when (and source-name target-name)
              (insert "    ")
              (insert (propertize source-name 'face '(:foreground "default")))
              (insert " ")
              (insert (propertize "⋈" 'face '(:foreground "default" :weight 'bold)))
              (insert " ")
              (insert (propertize target-name 'face '(:foreground "default")))
              (insert "\n"))))
      (insert (propertize "    No relations found.\n" 'face '(:foreground "gray50" :slant 'italic)))))
    (insert "\n"))

;; FIXME: There is a problem with the data contract. simtag.utils.unified_tag_processor - ERROR - Parsed data is not a dict, but <class 'list'>. Returning empty dict
;; (defun org-supertag-view-node--insert-similar-entities-section (node-id)
;;   (insert (propertize "🧑‍🤝‍🧑 Similar Entities\n" 'face 'org-level-2))
;;   ;; Per data contract, send a list containing an alist.
;;   (org-supertag-bridge-call-async "query/get_similar_entities" (list `(("node_id" . ,node-id)))
;;                                   (lambda (result)
;;                                     (with-current-buffer (get-buffer-create "*Org SuperTag Node View*")
;;                                       (let ((inhibit-read-only t))
;;                                         (goto-char (point-max))
;;                                         (if (and result (equal (plist-get result :status) "success"))
;;                                             (let ((entities (plist-get result :result)))
;;                                               (if entities
;;                                                   (dolist (entity entities)
;;                                                     (insert (propertize (format "    %s " (plist-get entity :title)) 'face 'org-tag))
;;                                                     (insert (propertize (format "(Score: %.2f)\n" (plist-get entity :score)) 'face 'success)))
;;                                                 (insert (propertize "    No similar entities found.\n" 'face 'shadow))))
;;                                           (insert (propertize "    Error getting similar entities.\n" 'face 'error)))))))
;;   (insert "\n"))

(defun org-supertag-view-node--insert-backlinks-section (node-id)
  (org-supertag-view-node--insert-section-header "References" "🔗")
  
  (org-supertag-view-node--insert-subsection-header "References To")
  (let ((refs (org-supertag-view-node--get-references node-id)))
    (if (and refs (listp refs) (not (null refs)))
        (let ((any-inserted nil))
          (dolist (ref-id refs)
            (let ((content (org-supertag-view-node--format-node-content ref-id)))
              (when content
                (insert content)
                (setq any-inserted t))))
          (unless any-inserted
            (insert (propertize "    No references found\n" 'face '(:foreground "gray50" :slant italic))))
      (insert (propertize "    No references found\n" 'face '(:foreground "gray50" :slant italic))))
  
  (org-supertag-view-node--insert-subsection-header "Referenced By")
  (let ((refd-by (org-supertag-view-node--get-referenced-by node-id)))
    (if (and refd-by (listp refd-by) (not (null refd-by)))
        (let ((any-inserted nil))
          (dolist (ref-id refd-by)
            (let ((content (org-supertag-view-node--format-node-content ref-id)))
              (when content
                (insert content)
                (setq any-inserted t))))
          (unless any-inserted
            (insert (propertize "    Not referenced by any nodes\n" 'face '(:foreground "gray50" :slant italic))))
      (insert (propertize "    Not referenced by any nodes\n" 'face '(:foreground "gray50" :slant italic))))
    (insert "\n"))))))

(defun org-supertag-view-node--get-field-info-at-point ()
  "Return plist of field info at point, or nil.
Tries current position first"
  (let* ((pos (point))
         (fallback-pos (max (point-min) (1- pos)))
         (tag-id     (or (get-text-property pos 'tag-id)
                         (get-text-property fallback-pos 'tag-id)))
         (field-name (or (get-text-property pos 'field-name)
                         (get-text-property fallback-pos 'field-name)))
         (node-id org-supertag-view-node--current-node-id))
    (when (and tag-id field-name node-id)
      (let* ((tag-def   (org-supertag-tag-get tag-id))
             (all-fields (org-supertag-get-all-fields-for-tag tag-id)) ;; Get all fields, including inherited
             (field-def (cl-find field-name all-fields
                                 :key (lambda (f) (plist-get f :name))
                                 :test #'string=))
             (value (when field-def
                      (org-supertag-field-get-value node-id field-name tag-id))))
        (list :node-id   node-id
              :tag-id    tag-id
              :field-name field-name
              :field-def field-def
              :value     value)))))

(defun org-supertag-view-node-edit-field-at-point ()
  "Edit the field value at the current point. "
  (interactive)
  (when-let* ((field-info (org-supertag-view-node--get-field-info-at-point))
              (field-def (plist-get field-info :field-def)))
    (when field-def
      (let* ((current-value (org-supertag-field-get-value-for-display
                             (plist-get field-info :node-id)
                             field-def))
             (node-id (plist-get field-info :node-id))
             (tag-id (plist-get field-info :tag-id))
             (field-name (plist-get field-info :field-name))
             (new-value (org-supertag-field-read-and-validate-value field-def current-value)))
        (message "DEBUG: view-node-edit new-value=%S (type=%S)" new-value (type-of new-value))
        (when new-value
          (let ((values (if (listp new-value) new-value (list new-value)))
                (all-tags (org-supertag-get-all-tags)))
            (dolist (val values)
              (unless (member val all-tags)
                (message "Creating new tag: %s" val)
                (org-supertag-tag--create val))))
          (org-supertag-field-set-value node-id field-name new-value tag-id)
          (org-supertag-view-node-refresh))))))


(defun org-supertag-view-node-edit-field-definition-at-point ()
  "Edit the field definition (e.g., its name) at the current point."
  (interactive)
  (when-let* ((field-info (org-supertag-view-node--get-field-info-at-point))
              (tag-id (plist-get field-info :tag-id))
              (field-name (plist-get field-info :field-name))
              (field-def (plist-get field-info :field-def)))
    (when (and tag-id field-name field-def)
      (let* ((current-type (plist-get field-def :type))
             (current-options (plist-get field-def :options))
             (action (completing-read "Edit: " '("Name" "Type and Options") nil t)))
        (cond
         ((string= action "Name")
          (let ((new-name (read-string (format "New name for field '%s': " field-name) nil nil field-name)))
            (when (and new-name (not (string-empty-p new-name)) (not (string= new-name field-name)))
              (org-supertag-tag-rename-field tag-id field-name new-name)
              (org-supertag-view-node-refresh))))
         ((string= action "Type and Options")
          (let* ((field-type-choices (org-supertag-get-field-types))
                 (field-type-str (completing-read "Field type: "
                                                  (mapcar #'car field-type-choices)
                                                  nil t nil nil (format "%s" current-type)))
                 (new-type (cdr (assoc field-type-str field-type-choices)))
                 (new-field-def (list :name field-name :type new-type)))
            ;; If it's options type, ask for options
            (when (eq new-type 'options)
              (let* ((current-options-str (if (and (eq current-type 'options) current-options)
                                              (mapconcat #'identity current-options ", ")
                                            ""))
                     (options-input (read-string "Options (comma separated): " current-options-str))
                     (new-options (split-string options-input "," t "[ \t\n\r]+")))
                (setq new-field-def (plist-put new-field-def :options new-options))))
            (when (org-supertag-tag--update-field-definition tag-id field-name new-field-def)
              (org-supertag-view-node-refresh)))))))))

(defun org-supertag-view-node-add-field ()
  "Add a new field to a tag associated with the current node."
  (interactive)
  (let* ((node-id org-supertag-view-node--current-node-id)
         (tags (org-supertag-node-get-tags node-id))
         (tag-id (completing-read "Add field to which tag: " tags nil t)))
    (when tag-id
      (when-let ((field-def (org-supertag-tag--create-field-definition)))
        (org-supertag-tag-add-field tag-id field-def)
        (org-supertag-view-node-refresh)))))

(defun org-supertag-view-node-remove-field-at-point ()
  "Remove the field definition at point from its tag."
  (interactive)
  (when-let* ((field-info (org-supertag-view-node--get-field-info-at-point)))
    (let ((tag-id (plist-get field-info :tag-id))
          (field-name (plist-get field-info :field-name)))
      (when (and field-name (yes-or-no-p (format "Really remove field '%s' from tag '%s'?"
                                                 field-name tag-id)))
        (when (org-supertag-tag--remove-field tag-id field-name)
          (org-supertag-view-node-refresh))))))

(defun org-supertag-view-node-move-field-up ()
  "Move the field at point up in its tag's field list."
  (interactive)
  (when-let* ((field-info (org-supertag-view-node--get-field-info-at-point)))
    (let ((tag-id (plist-get field-info :tag-id))
          (field-name (plist-get field-info :field-name)))
      (when (org-supertag-tag-move-field-up tag-id field-name)
        (org-supertag-view-node-refresh-and-restore-position tag-id field-name)))))

(defun org-supertag-view-node-move-field-down ()
  "Move the field at point down in its tag's field list."
  (interactive)
  (when-let* ((field-info (org-supertag-view-node--get-field-info-at-point)))
    (let ((tag-id (plist-get field-info :tag-id))
          (field-name (plist-get field-info :field-name)))
      (when (org-supertag-tag-move-field-down tag-id field-name)
        (org-supertag-view-node-refresh-and-restore-position tag-id field-name)))))

(defun org-supertag-view-node--find-tag-for-field (node-id field-name)
  (let ((found-tag-id nil))
    (dolist (tag-id (org-supertag-node-get-tags node-id))
      (when-let* ((tag-def (org-supertag-tag-get tag-id))
                  (fields (org-supertag-get-all-fields-for-tag tag-id)))
        (dolist (field-def fields)
          (when (string= (plist-get field-def :name) field-name)
            (setq found-tag-id tag-id)))))
    found-tag-id))

    
(defun org-supertag-view-node-refresh ()
  (interactive)
  (org-supertag-view-node--show-buffer))

(defun org-supertag-view-node-refresh-and-restore-position (tag-id field-name)
  "Refresh the view and restore cursor position to the specified field."
  (org-supertag-view-node--show-buffer)
  ;; Try to find and position cursor at the moved field
  (when (and tag-id field-name)
    (let ((found nil))
      (save-excursion
        (goto-char (point-min))
        ;; First find the tag section
        (while (and (not found) (search-forward tag-id nil t))
          (let ((tag-pos (match-beginning 0)))
            (when (and (get-text-property tag-pos 'tag-id)
                       (string= (get-text-property tag-pos 'tag-id) tag-id))
              ;; Found the tag, now look for the field within this tag's section
              (let ((tag-end (save-excursion
                              (goto-char tag-pos)
                              (forward-line)
                              (while (and (not (eobp))
                                         (or (looking-at "    [^ ]") ; Next field
                                             (looking-at "    $")))   ; Empty line
                                (forward-line))
                              (point))))
                (save-excursion
                  (goto-char tag-pos)
                  (while (and (not found) (< (point) tag-end))
                    (when (and (search-forward field-name tag-end t)
                              (get-text-property (match-beginning 0) 'field-name)
                              (string= (get-text-property (match-beginning 0) 'field-name) field-name)
                              (string= (get-text-property (match-beginning 0) 'tag-id) tag-id))
                      (setq found (match-beginning 0)))
                    (forward-line))))))))
      (when found
        (goto-char found)
        ;; Move to the beginning of the field line for better positioning
        (beginning-of-line)
        (search-forward field-name (line-end-position) t)
        (goto-char (match-beginning 0))))))

(defun org-supertag-view-node--show-buffer ()
  (let ((buffer (get-buffer-create "*Org SuperTag Node View*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-supertag-view-node-mode)
        (when-let* ((node (org-supertag-db-get org-supertag-view-node--current-node-id))
                   (title (plist-get node :title))
                   (file-path (plist-get node :file-path)))
          ;; Header section
          (insert (propertize (format "📄 %s\n" title) 'face '(:weight bold :height 1.4 :foreground "default")))
          (insert (propertize (format "📁 %s\n" (file-name-nondirectory file-path)) 'face '(:foreground "gray60" :slant italic)))
          (insert (propertize (format "%s\n" org-supertag-view-node--section-separator) 'face '(:foreground "gray50")))
          (insert "\n")
          
          ;; Content sections
          (org-supertag-view-node--insert-metadata-section org-supertag-view-node--current-node-id)
          (org-supertag-view-node--insert-backlinks-section org-supertag-view-node--current-node-id)
          (org-supertag-view-node--insert-relations-section org-supertag-view-node--current-node-id)
          ;;(org-supertag-view-node--insert-similar-entities-section org-supertag-view-node--current-node-id)
          
          ;; Footer with enhanced styling
          (insert (propertize (format "%s\n" org-supertag-view-node--section-separator) 'face '(:foreground "gray50")))
          (insert (propertize "Navigation:\n" 'face '(:weight bold :foreground "default")))
          (insert (propertize "  [j/k] Next/Previous Section  [g] Refresh  [q] Quit\n" 'face '(:foreground "gray60" :height 0.9)))
          (insert (propertize "  [RET] Edit Field Value  [E] Edit Field Definition  [a] Add Field  [d] Delete Field\n" 'face '(:foreground "gray60" :height 0.9)))
          (insert (propertize "  [M-↑/↓] Move Field Up/Down\n" 'face '(:foreground "gray60" :height 0.9)))
          ))
    (org-supertag-view--display-buffer-right buffer)
    (select-window (get-buffer-window buffer))
    (goto-char (point-min)))))

;;;###autoload
(defun org-supertag-view-node-show ()
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Must be on a heading"))
  (let ((node-id (org-id-get-create)))
    (setq org-supertag-view-node--current-node-id node-id)
    (org-supertag-view-node--show-buffer)))

;;----------------------------------------------------------------------
;; AI Integration Functions
;;----------------------------------------------------------------------

(defun org-supertag-view-node-action-at-point ()
  "Handle action at point - either edit field or trigger AI suggestion."
  (interactive)
  (let ((action (get-text-property (point) 'action)))
    (cond
     ((eq action 'suggest-tags)
      (org-supertag-view-node-suggest-tags))
     (t
      (org-supertag-view-node-edit-field-at-point)))))

(defun org-supertag-view-node-suggest-tags ()
  "Get AI tag suggestions for current node."
  (interactive)
  (when org-supertag-view-node--current-node-id
    (message "Getting AI tag suggestions for current node...")
    (require 'org-supertag-api)
    (let ((node-data (org-supertag-db-get org-supertag-view-node--current-node-id))
          (current-buffer (current-buffer)))
      (if (and node-data 
               (plist-get node-data :content)
               (> (length (string-trim (plist-get node-data :content))) 20))
          (progn
            ;; Switch to the org file temporarily to use the suggest function
            (let ((file-path (plist-get node-data :file-path))
                  (pos (plist-get node-data :pos)))
              (when (and file-path pos (file-exists-p file-path))
                (with-current-buffer (find-file-noselect file-path)
                  (save-excursion
                    (goto-char pos)
                    (org-supertag-suggest-tags-here)))
                ;; Refresh the view after suggestions are applied
                (with-current-buffer current-buffer
                  (org-supertag-view-node-refresh)))))
        (message "Node content too short for meaningful tag suggestions")))))

(defun org-supertag-view-node-chat-with-node ()
  "Open chat with current node context."
  (interactive)
  (when org-supertag-view-node--current-node-id
    (require 'org-supertag-api)
    (let ((node-data (org-supertag-db-get org-supertag-view-node--current-node-id)))
      (when node-data
        (let ((file-path (plist-get node-data :file-path))
              (pos (plist-get node-data :pos)))
          (when (and file-path pos (file-exists-p file-path))
            (with-current-buffer (find-file-noselect file-path)
              (save-excursion
                (goto-char pos)
                (org-supertag-chat-with-node)))))))))
    
(provide 'org-supertag-view-node)
