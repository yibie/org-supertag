;;; org-supertag-ui-ner.el --- UI for NER-based features -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the UI for NER-based features, including
;; suggesting tags and discovering relationships from note content.

;;; Code:

(require 'org-supertag-api)
(require 'org-supertag-db)
(require 'org-supertag-ui-sidebar)
(require 'org-supertag-util)

;; --- Keymap and Dispatcher ---

(defvar org-supertag-ui-ner--suggestion-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'org-supertag-ui-ner--dispatch-action)
    (define-key map (kbd "RET") 'org-supertag-ui-ner--dispatch-action)
    map)
  "Keymap for interactive suggestions in the sidebar.")

(defun org-supertag-ui-ner--dispatch-action ()
  "Dispatch the action stored in text properties at point."
  (interactive)
  (let ((action (get-text-property (point) 'action)))
    (when (functionp action)
      (funcall action))))


;; --- Tag Suggestion UI ---

(defun org-supertag-ui-ner--format-tag-suggestions (suggestions current-note-tags)
  "Format tag SUGGESTIONS for display."
  (insert (propertize "NER Tag Suggestions:\n" 'face '(:weight bold)))
  (if (seq-empty-p suggestions)
      (insert "  (No suggestions found.)\n")
    (dolist (suggestion suggestions)
      (let* ((tag-name (plist-get suggestion :tag))
             (confidence (plist-get suggestion :confidence))
             (reason (plist-get suggestion :reason))
             (already-exists (member tag-name current-note-tags)))

        (insert (format "  %s %s (Confidence: %.2f)\n" (if already-exists "✓" "＋") tag-name confidence))
        (insert (format "    Reason: %s\n" reason))
        (unless already-exists
          (insert "    ")
          (insert (propertize "[Accept]"
                              'face 'link
                              'action (lambda () (org-supertag-ui-ner--accept-tag-suggestion tag-name))
                              'help-echo (format "Click to add tag '%s'" tag-name)))
          (insert "\n")))
      (insert "\n"))))

(defun org-supertag-ui-ner--accept-tag-suggestion (tag-name)
  "Handle accepting a tag suggestion."
  (interactive)
  (let ((node-id (org-id-get-create)))
    (org-supertag-add tag-name (list node-id))
    (message "Tag '%s' added to node %s." tag-name node-id)
    (org-supertag-ui-ner-suggest-tags))) ; Refresh view

(defun org-supertag-ui-ner-suggest-tags ()
  "Fetch and display NER-based tag suggestions for the current note."
  (interactive)
  (let* ((note-content (org-supertag-util-get-node-content-for-llm))
         (current-tags (org-get-tags-at (point)))
         (payload `((note_content . ,note-content) (existing_tags . ,current-tags))))
    (org-supertag-ui-sidebar-show)
    (org-supertag-ui-sidebar-render
     :ner-tags
     (lambda () (insert "  (Fetching NER suggestions...)\n"))
     t)
    (let* ((response (org-supertag-api-get-tag-relationship-suggestions payload))
           (suggestions (and (eq (car response) 'success) (plist-get (cdr response) :result))))
      (org-supertag-ui-sidebar-render
       :ner-tags
       (lambda () (org-supertag-ui-ner--format-tag-suggestions suggestions current-tags))
       t))))


;; --- Relationship Suggestion UI ---

(defun org-supertag-ui-ner--format-relationship-suggestions (relations)
  "Format relationship SUGGESTIONS for display."
  (insert (propertize "Inferred Relationships:\n" 'face '(:weight bold)))
  (if (seq-empty-p relations)
      (insert "  (No relationships inferred.)\n")
    (dolist (relation relations)
      (let ((from (plist-get relation :from_tag))
            (to (plist-get relation :to_tag))
            (type (plist-get relation :relation_type)))
        (insert (format "  %s → %s (%s)\n" from to type))
        (insert "    ")
        (insert (propertize "[Create Relation]"
                            'face 'link
                            'action (lambda () (org-supertag-ui-ner--accept-relationship-suggestion from to type))
                            'help-echo (format "Create '%s -> %s' relationship" from to)))
        (insert "\n\n")))))

(defun org-supertag-ui-ner--accept-relationship-suggestion (from to type)
  "Handle accepting a relationship suggestion."
  (interactive)
  (org-supertag-db-add-relation from to type)
  (message "Relation '%s -> %s (%s)' created." from to type)
  (org-supertag-ui-ner-discover-relationships)) ; Refresh view

(defun org-supertag-ui-ner-discover-relationships ()
  "Fetch and display inferred relationships for the current note."
  (interactive)
  (let ((note-content (org-supertag-util-get-node-content-for-llm)))
    (org-supertag-ui-sidebar-show)
    (org-supertag-ui-sidebar-render
     :ner-rels
     (lambda () (insert "  (Discovering relationships...)\n"))
     t)
    (let* ((response (org-supertag-api-discover-inferred-relationships `((note_content . ,note-content))))
           (relations (and (eq (car response) 'success) (plist-get (cdr response) :result))))
      (org-supertag-ui-sidebar-render
       :ner-rels
       (lambda () (org-supertag-ui-ner--format-relationship-suggestions relations))
       t))))


(provide 'org-supertag-ui-ner)
;;; org-supertag-ui-ner.el ends here 