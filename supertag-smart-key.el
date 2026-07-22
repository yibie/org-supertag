;;; supertag-smart-key.el --- Semantic activation at point -*- lexical-binding: t; -*-

;;; Commentary:
;; `supertag-smart-key' turns existing Org-Supertag text properties and
;; ordinary Emacs interaction primitives into one context-sensitive command.
;; Targets are transient data; recognition never mutates the buffer or store.

;;; Code:

(require 'button)
(require 'org)
(require 'org-id)
(require 'supertag-view-helper)

(declare-function supertag-add-reference "supertag-ui-commands" ())
(declare-function supertag-add-tag "supertag-ui-commands" (&optional beg end))
(declare-function supertag-delete-tag-everywhere "supertag-ui-commands" (&optional tag-id))
(declare-function supertag-goto-node "supertag-services-ui" (node-id &optional other-window))
(declare-function supertag-menu "supertag-menu" ())
(declare-function supertag-remove-tag-from-node "supertag-ui-commands" ())
(declare-function supertag-rename-tag "supertag-ui-commands" (&optional tag-id))
(declare-function supertag-schema--edit-field-definition-at-point "supertag-view-schema" ())
(declare-function supertag-ui-quick-edit-field "supertag-ui-commands" ())
(declare-function supertag-view-schema "supertag-view-schema" ())
(declare-function supertag-view-node--focus-view "supertag-view-node" ())
(declare-function supertag-view-node--show-side "supertag-view-node" (&optional node-id))
(declare-function supertag-view-node-edit-at-point "supertag-view-node" ())
(declare-function supertag-view-table "supertag-view-table"
                  (data-source &optional columns view-config named-views))
(declare-function supertag-view-table-edit-cell "supertag-view-table" ())
(declare-function supertag-view-table-goto-node "supertag-view-table" ())

(defun supertag--smart-key-context-target-at (position)
  "Return the existing `supertag-context' at POSITION as a target."
  (let* ((context (get-text-property position 'supertag-context))
         (kind (if (consp context)
                   (plist-get context :type)
                 (and context (get-text-property position 'type)))))
    (when kind
      (if (consp context)
          (append (list :kind kind :origin :supertag-context) context)
        (list :kind kind
              :origin :supertag-context
              :tag-id (get-text-property position 'tag-id)
              :field-name (get-text-property position 'field-name)
              :id (get-text-property position 'id))))))

(defun supertag--smart-key-node-target-at (position)
  "Return a target from a semantic node property at POSITION."
  (let ((concept-id (get-text-property position 'supertag-concept-node-id))
        (reference-id (get-text-property position 'supertag-ref-id))
        (node-id (get-text-property position 'supertag-node-id)))
    (cond
     (concept-id (list :kind :concept :origin :concept-mention :node-id concept-id))
     (reference-id (list :kind :node-reference :origin :table-reference :node-id reference-id))
     (node-id (list :kind :node-reference :origin :node-link :node-id node-id)))))

(defun supertag--smart-key-table-target-at (position)
  "Return the Table View cell at POSITION as a target."
  (when (derived-mode-p 'supertag-view-table-mode)
    (let ((entity-id (get-text-property position 'entity-id))
          (column (get-text-property position 'col-key)))
      (when (and entity-id column)
        (list :kind :table-cell :origin :table-view
              :node-id entity-id :column column)))))

(defun supertag--smart-key-button-target-at (position)
  "Return an Emacs button at POSITION as a target."
  (let ((button (button-at position)))
    (when button
      (list :kind :button :origin :emacs-button :button button))))

(defun supertag--smart-key-org-link-target ()
  "Return the Org link at point as a target."
  (when (derived-mode-p 'org-mode)
    (let ((element (org-element-context)))
      (when (eq (org-element-type element) 'link)
        (list :kind :org-link :origin :org-link
              :type (org-element-property :type element)
              :path (org-element-property :path element))))))

(defun supertag--smart-key-inline-tag-target ()
  "Return the valid inline tag at point as a target."
  (when-let* ((tag-id (supertag-view-helper-get-tag-at-point)))
    (list :kind :tag :origin :inline-tag :tag-id tag-id)))

(defun supertag--smart-key-heading-target ()
  "Return the Org heading at point as a target without creating an ID."
  (when (and (derived-mode-p 'org-mode) (org-at-heading-p))
    (list :kind :node :origin :org-heading :node-id (org-entry-get nil "ID"))))

(defun supertag--smart-key-local-ret-target-at (position)
  "Return a legacy text-property RET command at POSITION as a target."
  (let* ((map (get-text-property position 'keymap))
         (command (and (keymapp map) (lookup-key map (kbd "RET")))))
    (when (commandp command)
      (list :kind :command :origin :local-ret :command command))))

(defun supertag--smart-key-primary-target-at (position)
  "Return the first semantic property target at POSITION."
  (or (supertag--smart-key-context-target-at position)
      (supertag--smart-key-node-target-at position)
      (supertag--smart-key-table-target-at position)
      (supertag--smart-key-button-target-at position)))

(defun supertag--target-at-point ()
  "Return a transient semantic target for point, or nil.
This recognizer is read-only; it does not create Org IDs or mutate the store."
  (let ((previous (and (> (point) (point-min)) (1- (point)))))
    (or (supertag--smart-key-primary-target-at (point))
        (supertag--smart-key-org-link-target)
        (supertag--smart-key-inline-tag-target)
        (supertag--smart-key-heading-target)
        (supertag--smart-key-local-ret-target-at (point))
        (and previous (supertag--smart-key-primary-target-at previous))
        (and previous (supertag--smart-key-local-ret-target-at previous)))))

(defun supertag--activate-target (target)
  "Execute the default action for TARGET."
  (pcase (plist-get target :kind)
    (:field-value
     (unless (fboundp 'supertag-view-node-edit-at-point)
       (require 'supertag-view-node))
     (call-interactively #'supertag-view-node-edit-at-point))
    (:field
     (unless (fboundp 'supertag-schema--edit-field-definition-at-point)
       (require 'supertag-view-schema))
     (call-interactively #'supertag-schema--edit-field-definition-at-point))
    (:tag
     (unless (fboundp 'supertag-view-table)
       (require 'supertag-view-table))
     (supertag-view-table
      (list :type :tag :value (plist-get target :tag-id))))
    ((or :concept :node-reference)
     (unless (fboundp 'supertag-goto-node)
       (require 'supertag-services-ui))
     (supertag-goto-node (plist-get target :node-id)))
    (:table-cell
     (if (eq (plist-get target :column) :title)
         (progn
           (unless (fboundp 'supertag-view-table-goto-node)
             (require 'supertag-view-table))
           (call-interactively #'supertag-view-table-goto-node))
       (unless (fboundp 'supertag-view-table-edit-cell)
         (require 'supertag-view-table))
       (call-interactively #'supertag-view-table-edit-cell)))
    (:button (button-activate (plist-get target :button)))
    (:org-link (org-open-at-point))
    (:node
     (unless (plist-get target :node-id)
       (user-error "Heading has no ID; Node View does not create IDs"))
     (unless (fboundp 'supertag-view-node--show-side)
       (require 'supertag-view-node))
     (supertag-view-node--show-side (plist-get target :node-id))
     (supertag-view-node--focus-view))
    (:command (call-interactively (plist-get target :command)))
    (_ (user-error "No default action for target: %S" target))))

(defun supertag--assist-call (feature command &rest args)
  "Load FEATURE and call COMMAND, passing ARGS when present."
  (unless (fboundp command)
    (require feature))
  (if args
      (apply command args)
    (call-interactively command)))

(defun supertag--open-main-menu ()
  "Open the complete Org-Supertag command menu."
  (supertag--assist-call 'supertag-menu 'supertag-menu))

(defun supertag--target-label (target)
  "Return a concise user-facing label for TARGET."
  (pcase (plist-get target :kind)
    (:tag (format "#%s" (plist-get target :tag-id)))
    ((or :field :field-value)
     (format "%s.%s"
             (or (plist-get target :tag-id) "field")
             (or (plist-get target :field-name) "value")))
    ((or :concept :node-reference)
     (format "node %s" (plist-get target :node-id)))
    (:node (if-let* ((node-id (plist-get target :node-id)))
               (format "node %s" node-id)
             "current heading"))
    (:table-cell (format "table cell %s" (plist-get target :column)))
    (:org-link (format "%s:%s"
                       (plist-get target :type)
                       (plist-get target :path)))
    (:button "button")
    (:command "command")
    (_ "object")))

(defun supertag--assist-actions (target)
  "Return context-relevant action choices for TARGET."
  (let* ((kind (plist-get target :kind))
         (default-label
          (pcase kind
            (:tag "Open tagged nodes")
            (:field-value "Edit field value")
            (:field "Edit field definition")
            ((or :concept :node-reference) "Open node")
            (:table-cell (if (eq (plist-get target :column) :title)
                             "Open source node"
                           "Edit cell"))
            (:button "Activate button")
            (:org-link "Open Org link")
            (:node "Open node view")
            (:command "Run command")
            (_ "Run default action")))
         (default-action
          (cons (format "%s (default)" default-label)
                (apply-partially #'supertag--activate-target target)))
         (specific-actions
          (pcase kind
            (:tag
             (let ((tag-id (plist-get target :tag-id)))
               (list
                (cons "Open schema"
                      (apply-partially #'supertag--assist-call
                                       'supertag-view-schema 'supertag-view-schema))
                (cons "Rename tag..."
                      (apply-partially #'supertag--assist-call
                                       'supertag-ui-commands 'supertag-rename-tag tag-id))
                (cons "Delete tag everywhere..."
                      (apply-partially #'supertag--assist-call
                                       'supertag-ui-commands
                                       'supertag-delete-tag-everywhere tag-id)))))
            (:field-value
             (list
              (cons "Open schema"
                    (apply-partially #'supertag--assist-call
                                     'supertag-view-schema 'supertag-view-schema))))
            (:node
             (list
              (cons "Add tag..."
                    (apply-partially #'supertag--assist-call
                                     'supertag-ui-commands 'supertag-add-tag))
              (cons "Remove tag..."
                    (apply-partially #'supertag--assist-call
                                     'supertag-ui-commands 'supertag-remove-tag-from-node))
              (cons "Quick edit field..."
                    (apply-partially #'supertag--assist-call
                                     'supertag-ui-commands 'supertag-ui-quick-edit-field))
              (cons "Add reference..."
                    (apply-partially #'supertag--assist-call
                                     'supertag-ui-commands 'supertag-add-reference))))
            ((or :concept :node-reference)
             (let ((node-id (plist-get target :node-id)))
               (list
                (cons "Open node in other window"
                      (apply-partially #'supertag--assist-call
                                       'supertag-services-ui 'supertag-goto-node node-id t)))))
            (:table-cell
             (unless (eq (plist-get target :column) :title)
               (let ((node-id (plist-get target :node-id)))
                 (list
                  (cons "Open source node"
                        (apply-partially #'supertag--assist-call
                                         'supertag-services-ui 'supertag-goto-node node-id)))))))))
    (append (list default-action)
            specific-actions
            (list (cons "All Org-Supertag commands..." #'supertag--open-main-menu)))))

;;;###autoload
(defun supertag-assist ()
  "Offer actions for the semantic object at point.
When point has no semantic target, open the complete `supertag-menu'."
  (interactive)
  (if-let* ((target (supertag--target-at-point)))
      (let* ((actions (supertag--assist-actions target))
             (choice (completing-read
                      (format "Action for %s: " (supertag--target-label target))
                      actions nil t nil nil (caar actions)))
             (action (cdr (assoc choice actions))))
        (funcall action))
    (supertag--open-main-menu)))

;;;###autoload
(defun supertag-smart-key (&optional assist)
  "Activate the semantic object at point.
With prefix argument ASSIST, offer actions relevant to that object."
  (interactive "P")
  (if assist
      (supertag-assist)
    (if-let* ((target (supertag--target-at-point)))
        (supertag--activate-target target)
      (user-error "No Org-Supertag target at point"))))

(provide 'supertag-smart-key)
;;; supertag-smart-key.el ends here
