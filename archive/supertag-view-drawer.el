
;;; org-supertag/supertag-view-drawer.el --- In-buffer drawer view for nodes -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides functions to render node details into an Org mode drawer.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'supertag-drawer-adapter)
(require 'supertag-view-helper)
(require 'supertag-ui-commands)
(require 'supertag-ops-field)

;;; --- Faces (can be consolidated later) ---

(defface supertag-drawer-title-face
  '((t :inherit outline-1))
  "Face for the drawer title line."
  :group 'org-supertag)

(defface supertag-drawer-tag-face
  '((t :inherit outline-2))
  "Face for tag headers inside the drawer."
  :group 'org-supertag)

(defface supertag-drawer-field-label-face
  '((t :inherit shadow))
  "Face for field labels."
  :group 'org-supertag)


;;; --- Interactivity ---

(defun supertag-drawer-edit-field-at-point ()
  "Edit the supertag field at point and refresh the drawer."
  (interactive)
  (let* ((context (get-text-property (point) 'supertag-context)))
    (when context
      (let* ((node-id (plist-get context :node-id))
             (tag-id (plist-get context :tag-id))
             (field-name (plist-get context :name))
             (definition (plist-get context :definition))
             (current (plist-get context :value))
             (new-value (supertag-ui-read-field-value definition current)))
        (unless (equal new-value current)
          (supertag-field-set node-id tag-id field-name new-value)
          (let ((p (point)))
            (supertag-view-node) ; Close
            (supertag-view-node) ; Re-open to refresh
            (goto-char p)))))))

(defvar supertag-drawer-field-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-RET") #'supertag-drawer-edit-field-at-point)
    map)
  "Keymap for interactive fields in the drawer.")


;;; --- Rendering Helpers ---

(defun supertag-drawer--insert-tags (tags node-id)
  "Insert tag sections for TAGS of NODE-ID into the current buffer."
  (if (null tags)
      (insert (propertize "No tags attached to this node.\n"
                          'face 'supertag-drawer-field-label-face))
    (dolist (tag tags)
      (insert (propertize (format "%s\n" (plist-get tag :tag-name))
                          'face 'supertag-drawer-tag-face))
      (let ((fields (plist-get tag :fields)))
        (if (or (null fields) (zerop (length fields)))
            (insert (propertize "  • No fields defined\n"
                                'face 'supertag-drawer-field-label-face))
          (dolist (field fields)
            (let* ((label (or (plist-get field :name) "[unnamed]"))
                   (definition (plist-get field :definition))
                   (value (plist-get field :value))
                   (context (list :node-id node-id
                                  :tag-id (plist-get tag :tag-id)
                                  :name label
                                  :definition definition
                                  :value value))
                   (interactive-props
                    (list 'keymap supertag-drawer-field-keymap
                          'mouse-face 'highlight
                          'help-echo "M-RET to edit"
                          'supertag-context context)))
              (supertag-view-helper-insert-field-line label value definition interactive-props))))))))

(defun supertag-drawer--insert-references (direction refs)
  "Insert reference list for DIRECTION (:outgoing or :incoming) given REFS."
  (let ((label (pcase direction
                 (:outgoing "References →")
                 (:incoming "← Referenced By")
                 (_ (symbol-name direction)))))
    (insert (propertize (format "%s\n" label)
                        'face 'supertag-drawer-field-label-face))
    (if refs
        (dolist (ref refs)
          (insert (format "  • %s\n"
                          (or (plist-get ref :title)
                              (plist-get ref :node-id)))))
      (insert "  • None\n"))))

;;; --- Core Rendering Function ---

(defun supertag-drawer-insert-content (node-id)
  "Insert node details for NODE-ID directly into the current buffer with text properties."
  (let ((details (supertag-drawer-adapter-node-details node-id)))
    (unless details
      (error "Node %s not found in store." node-id))
    (when-let ((title (plist-get details :title)))
      (insert (propertize (format "%s\n" title) 'face 'supertag-drawer-title-face)))
    (when-let ((file (plist-get details :file)))
      (insert (propertize (format "%s\n" (abbreviate-file-name file))
                          'face 'supertag-drawer-field-label-face)))
    (insert "\n")
    (supertag-drawer--insert-tags (plist-get details :tags) node-id)
    (let* ((refs (plist-get details :references))
           (outgoing (plist-get refs :outgoing))
           (incoming (plist-get refs :incoming)))
      (when (or outgoing incoming)
        (insert "\n")
        (supertag-drawer--insert-references :outgoing outgoing)
        (insert "\n")
        (supertag-drawer--insert-references :incoming incoming)))))

(defun supertag-drawer-render-content (node-id)
  "Render node details for NODE-ID and return as a string.

DEPRECATED: Use `supertag-drawer-insert-content' instead.
This function is kept for backward compatibility but will strip text properties."
  (with-temp-buffer
    (supertag-drawer-insert-content node-id)
    (buffer-string)))

(provide 'supertag-view-drawer)

;;; supertag-view-drawer.el ends here
