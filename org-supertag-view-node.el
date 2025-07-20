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
;; Mode Definition
;;----------------------------------------------------------------------

(defvar org-supertag-view-node-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "RET") 'org-supertag-view-node-edit-field-at-point)
    (define-key map (kbd "E") 'org-supertag-view-node-edit-field-definition-at-point)
    (define-key map (kbd "a") 'org-supertag-view-node-add-field)
    (define-key map (kbd "d") 'org-supertag-view-node-remove-field-at-point)
    (define-key map (kbd "f") 'org-supertag-feedback-on-relation-at-point)

    (define-key map (kbd "g") 'org-supertag-view-node-refresh)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `org-supertag-view-node-mode'.")

(define-derived-mode org-supertag-view-node-mode special-mode "Org-Supertag-Node-View"
  "Major mode for the unified node view."
  :group 'org-supertag)

;;----------------------------------------------------------------------
;; Core Functions
;;----------------------------------------------------------------------

(defun org-supertag-view-node--get-references (node-id)
  (when-let* ((node (org-supertag-db-get node-id)))
    (plist-get node :ref-to)))

(defun org-supertag-view-node--get-referenced-by (node-id)
  (when-let* ((node (org-supertag-db-get node-id)))
    (plist-get node :ref-from)))

(defun org-supertag-view-node--format-node-content (node-id)
  (when-let* ((node (org-supertag-db-get node-id))
              (title (plist-get node :title))
              (file-path (plist-get node :file-path))
              (content (or (plist-get node :content) "")))
    (let* ((file-name (file-name-nondirectory file-path))
           (styled-title (propertize title 'face '(:weight bold))))
      (format "%s (%s)\n%s\n\n" styled-title file-name content))))

(defun org-supertag-view-node--format-display-value (value)
  "Format VALUE for display.
If VALUE is a list, join elements with ' / '.
Otherwise, return VALUE as a string."
  (if (listp value)
      (mapconcat #'identity value " / ")
    (format "%s" (or value ""))))

(defun org-supertag-view-node--insert-metadata-section (node-id)
  (let ((tags (org-supertag-node-get-tags node-id)))
    (insert (propertize "🏷️ Tags\n" 'face 'org-level-2))
    (if (not tags)
        (insert (propertize "  No metadata found.\n" 'face 'shadow))
      (dolist (tag-id tags)
        (insert (propertize (format "    %s\n" tag-id) 'face 'org-tag 'tag-id tag-id 'field-name nil))
        (when-let* ((tag-def (org-supertag-tag-get tag-id))
                    (fields (plist-get tag-def :fields)))
          (if (not fields)
              (insert (propertize "    No fields defined.\n" 'face 'shadow))
            (dolist (field-def fields)
              (let* ((field-name (plist-get field-def :name))
                     (value (org-supertag-field-get-value node-id field-name tag-id))
                     (display-value (org-supertag-view-node--format-display-value value))
                     (start (point)))
                (insert (propertize (format "    %s: " field-name)
                                     'face '(:weight bold :underline t)))
                (insert (propertize (format "%s\n" display-value) 'face 'font-lock-string-face))
                (add-text-properties start (point)
                                     `(field-name ,field-name
                                       tag-id ,tag-id
                                       field-def ,field-def
                                       face (:height 1.0))))))))
      (insert (propertize "    [RET] Edit Value [E] Edit Field Definition [a] Add Field [d] Delete Field\n" 'face 'org-meta-line)))
    (insert "\n")))

(defun org-supertag-view-node--insert-relations-section (node-id)
  (let ((tags (org-supertag-node-get-tags node-id))
        (relations '())
        (valid-relations '()))
    
    (dolist (tag-id tags)
      (let ((tag-relations (org-supertag-relation-get-all tag-id)))
        (setq relations (append relations tag-relations))))
    
    ;; Filter out invalid relations to prevent "emacs --[cooccurrence]--> nil" display
    (dolist (rel relations)
      (let* ((source-id (plist-get rel :from))
             (target-id (plist-get rel :to))
             (type (plist-get rel :type))
             (source-name (org-supertag-tag-get-name-by-id source-id))
             (target-name (org-supertag-tag-get-name-by-id target-id)))
        ;; Only include relations where both source and target names are valid, and exclude cooccurrence relations
        (when (and source-name target-name (not (string-empty-p source-name)) (not (string-empty-p target-name)) (not (string= type "cooccurrence")))
          (push (list :source-id source-id :target-id target-id :type type :source-name source-name :target-name target-name) valid-relations))))
    
    ;; Only show relations section if there are valid relations
    (when valid-relations
      (insert (propertize "🔗 Relations\n" 'face 'org-level-2))
      (dolist (rel valid-relations)
        (let* ((source-name (plist-get rel :source-name))
               (target-name (plist-get rel :target-name))
               (type (plist-get rel :type))
               (target-id (plist-get rel :target-id)))
          ;; Display the relation
          (insert (propertize (format "    %s " source-name) 'face 'org-tag))
          (insert (propertize (format "--[%s]-->" type) 'face 'font-lock-keyword-face))
          (insert (propertize (format " %s\n" target-name) 'face 'org-tag))
          
          ;; Display nodes associated with the target tag
          (let ((target-nodes (org-supertag-db-get-tag-nodes target-id)))
            (if target-nodes
                (progn
                  (insert (propertize "      📄 Related nodes:\n" 'face '(:slant italic)))
                  (dolist (node-id target-nodes)
                    (when-let* ((node (org-supertag-db-get node-id))
                               (title (plist-get node :title))
                               (file-path (plist-get node :file-path)))
                      (let ((file-name (file-name-nondirectory file-path)))
                        (insert "        • ")
                        (insert-text-button title
                                            'action (lambda (_btn) (org-supertag-view--goto-node node-id))
                                            'follow-link t
                                            'help-echo (format "Open node %s" node-id)
                                            'face 'org-link)
                        (insert (format " (%s)\n" file-name)))))
                  ;; Limit display to first 5 nodes to avoid overwhelming
                  (when (> (length target-nodes) 5)
                    (insert (propertize (format "        ... and %d more nodes\n" (- (length target-nodes) 5)) 'face 'shadow)))))))
      (insert "\n")))))

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
  (insert (propertize "👉 References\n" 'face 'org-level-2))
  (let ((refs (org-supertag-view-node--get-references node-id)))
    (if refs
        (dolist (ref-id refs)
          (insert (org-supertag-view-node--format-node-content ref-id)))
      (insert (propertize "    No references found\n" 'face 'shadow))))
  (insert "\n")
  (insert (propertize "👈 Referenced By\n" 'face 'org-level-2))
  (let ((refd-by (org-supertag-view-node--get-referenced-by node-id)))
    (if refd-by
        (dolist (ref-id refd-by)
          (insert (org-supertag-view-node--format-node-content ref-id)))       
      (insert (propertize "    Not referenced by any nodes\n" 'face 'shadow))))
  (insert "\n"))

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
             (field-def (cl-find field-name (plist-get tag-def :fields)
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
  "Edit the field value at the current point."
  (interactive)
  (when-let* ((field-info (org-supertag-view-node--get-field-info-at-point))
              (field-def (plist-get field-info :field-def)))
    (when field-def
      (let* ((current-value (plist-get field-info :value))
             (node-id (plist-get field-info :node-id))
             (tag-id (plist-get field-info :tag-id))
             (field-name (plist-get field-info :field-name))
             (new-value (org-supertag-field-read-and-validate-value field-def current-value)))
        (when new-value
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
                                                  nil t nil nil (symbol-name current-type)))
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
            (org-supertag-view-node--update-field-definition tag-id field-name new-field-def)
            (org-supertag-view-node-refresh))))))))

(defun org-supertag-view-node-add-field ()
  "Add a new field to a tag associated with the current node."
  (interactive)
  (let* ((node-id org-supertag-view-node--current-node-id)
         (tags (org-supertag-node-get-tags node-id))
         (tag-id (completing-read "Add field to which tag: " tags nil t)))
    (when tag-id
      (let* ((field-name (read-string "Field name: "))
             (field-type-choices (org-supertag-get-field-types))
             (field-type-str (completing-read "Field type: "
                                              (mapcar #'car field-type-choices)))
             (field-type (cdr (assoc field-type-str field-type-choices)))
             (field-def (list :name field-name :type field-type)))
        ;; For options type, ask for the options
        (when (eq field-type 'options)
          (let* ((options-input (read-string "Options (comma separated): "))
                 (options-list (split-string options-input "," t "[ \t\n\r]+")))
            (setq field-def (plist-put field-def :options options-list))))
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
        (let* ((tag (org-supertag-db-get tag-id))
               (fields (plist-get tag :fields))
               (new-fields (cl-remove-if (lambda (f)
                                           (string= (plist-get f :name) field-name))
                                         fields))
               (new-tag (plist-put (copy-sequence tag) :fields new-fields)))
          (org-supertag-db-add tag-id new-tag)
          (message "Field '%s' removed from tag '%s'." field-name tag-id)
          (org-supertag-view-node-refresh))))))

(defun org-supertag-view-node--find-tag-for-field (node-id field-name)
  (let ((found-tag-id nil))
    (dolist (tag-id (org-supertag-node-get-tags node-id))
      (when-let* ((tag-def (org-supertag-tag-get tag-id))
                  (fields (plist-get tag-def :fields)))
        (dolist (field-def fields)
          (when (string= (plist-get field-def :name) field-name)
            (setq found-tag-id tag-id)))))
    found-tag-id))

(defun org-supertag-view-node--update-field-definition (tag-id field-name new-field-def)
  "Update the complete definition of a field in a tag."
  (when-let* ((tag (org-supertag-db-get tag-id))
              (fields (plist-get tag :fields)))
    (let* ((new-fields (mapcar (lambda (f)
                                 (if (string= (plist-get f :name) field-name)
                                     new-field-def
                                   f))
                               fields))
           (new-tag (plist-put (copy-sequence tag) :fields new-fields)))
      (org-supertag-db-add tag-id new-tag)
      (message "Field '%s' definition updated in tag '%s'." field-name tag-id))))


    
(defun org-supertag-view-node-refresh ()
  (interactive)
  (org-supertag-view-node--show-buffer))

(defun org-supertag-view-node--show-buffer ()
  (let ((buffer (get-buffer-create "*Org SuperTag Node View*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-supertag-view-node-mode)
        (when-let* ((node (org-supertag-db-get org-supertag-view-node--current-node-id))
                   (title (plist-get node :title)))
          (insert (propertize (format "📄 %s\n" title) 'face 'org-level-1))
          (insert (propertize "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n" 'face 'org-meta-line))
          (org-supertag-view-node--insert-metadata-section org-supertag-view-node--current-node-id)
          (org-supertag-view-node--insert-backlinks-section org-supertag-view-node--current-node-id)
          (org-supertag-view-node--insert-relations-section org-supertag-view-node--current-node-id)
          ;;(org-supertag-view-node--insert-similar-entities-section org-supertag-view-node--current-node-id)
          (insert (propertize "\nj/k: move  RET: edit  E: edit field def  a: add field  d: delete field  f: feedback  g: refresh  q: quit\n" 'face 'org-meta-line))
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
    
(provide 'org-supertag-view-node)
