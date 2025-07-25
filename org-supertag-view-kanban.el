;;; org-supertag-view-kanban.el --- Kanban board view for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a Kanban-style board view for org-supertag.
;; Unlike the column view which compares different tags, the Kanban view
;; is based on a single tag and groups its nodes into columns based on the
;; unique values of a chosen field (e.g., a 'status' field).
;;
;; This view is fully interactive, allowing users to:
;; - Move nodes between columns (changing their field value).
;; - Edit the values of any field directly on the node card.
;; - Manage the field definitions of the underlying tag.

;;; Code:

(require 'org-supertag-view-utils)
(require 'org-supertag-db)
(require 'org-supertag-tag)
(require 'org-supertag-field)


;;----------------------------------------------------------------------
;; State Variables
;;----------------------------------------------------------------------

(defvar org-supertag-kanban--current-base-tag nil
  "The base tag for the current Kanban view. e.g., \"project\".")

(defvar org-supertag-kanban--current-group-field nil
  "The field used to group nodes into columns. e.g., \"status\".")

(defvar org-supertag-kanban--column-headers nil
  "The sorted list of column headers for the current Kanban view.")

;;----------------------------------------------------------------------
;; Mode & Keymap Definition
;;----------------------------------------------------------------------

(defvar org-supertag-kanban-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-kanban-refresh)
    ;; Card movement
    (define-key map (kbd "h") 'org-supertag-kanban-move-card-left)
    (define-key map (kbd "<left>") 'org-supertag-kanban-move-card-left)
    (define-key map (kbd "l") 'org-supertag-kanban-move-card-right)
    (define-key map (kbd "<right>") 'org-supertag-kanban-move-card-right)
    ;; Value and definition editing
    (define-key map (kbd "RET") 'org-supertag-kanban-edit-field-value-at-point)
    (define-key map (kbd "E") 'org-supertag-kanban-edit-field-definition-at-point)
    (define-key map (kbd "a") 'org-supertag-kanban-add-field)
    (define-key map (kbd "d") 'org-supertag-kanban-delete-field-at-point)
    ;; Navigation
    (define-key map (kbd "v") 'org-supertag-kanban-view-node-at-point)
    (define-key map (kbd "TAB") 'org-supertag-kanban-next-column)
    (define-key map (kbd "<backtab>") 'org-supertag-kanban-previous-column)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map)
  "Keymap for `org-supertag-kanban-mode'.")

;;----------------------------------------------------------------------
;; Data Preparation
;;----------------------------------------------------------------------

(defun org-supertag-kanban--group-nodes-by-field (base-tag-name field-name)
  "Group nodes by the value of a specific field.
Returns a hash-table where keys are field values and values are lists of node IDs."
  (let ((nodes-with-tag (org-supertag-view--get-nodes-with-tags (list base-tag-name)))
        (grouped-nodes (make-hash-table :test 'equal)))
    (dolist (node-id nodes-with-tag)
      (let* ((field-value (org-supertag-field-get-value node-id field-name base-tag-name))
             (key (or field-value "Uncategorized")))
        (push node-id (gethash key grouped-nodes '()))))
    grouped-nodes))

(defun org-supertag-kanban--format-node-fields (node-id base-tag group-field)
  "Format the fields of a NODE-ID for display in the Kanban card.
Excludes the field used for grouping."
  (let ((field-strings '()))
    (when-let* ((tag-def (org-supertag-tag-get base-tag))
                (fields (plist-get tag-def :fields)))
      (dolist (field-def fields)
        (let ((field-name (plist-get field-def :name)))
          (unless (string= field-name group-field)
            (when-let ((value (org-supertag-field-get-value node-id field-name base-tag)))
              (push (format "    └ %s: %s" field-name value) field-strings))))))
    (nreverse field-strings)))

(defun org-supertag-kanban--format-card (node-id width)
  "Format a NODE-ID into a bordered card of fixed WIDTH.
Returns the card as a list of strings, each correctly padded."
  (let* ((props (gethash node-id org-supertag-db--object))
         (title (or (plist-get props :title) "No Title"))
         (inner-width (- width 4)) ; For "│ text │"
         (wrapped-lines (org-supertag-view-util-wrap-text title inner-width))
         (card-lines '()))
    ;; Top border
    (push (format "┌%s┐" (make-string (- width 2) ?─)) card-lines)
    ;; Content lines
    (dolist (line wrapped-lines)
      (push (format "│ %s │" (org-supertag-view-util-pad-string line inner-width :left)) card-lines))
    ;; Bottom border
    (push (format "└%s┘" (make-string (- width 2) ?─)) card-lines)
    ;; Propertize all lines and return
    (let ((final-lines (nreverse card-lines)))
      (mapcar (lambda (line)
                (propertize line
                            'node-id node-id
                            'base-tag org-supertag-kanban--current-base-tag
                            'group-field org-supertag-kanban--current-group-field))
              final-lines))))

;;----------------------------------------------------------------------
;; Core Rendering Function
;;----------------------------------------------------------------------

(defun org-supertag-kanban--refresh-display ()
  "Render the Kanban board based on the current state variables."
  (let ((buffer (get-buffer-create "Org SuperTag Kanban")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (column-width 40)
            (separator "  "))
        (erase-buffer)
        (org-supertag-kanban-mode)

        ;; Header
        (insert (propertize (format "Kanban Board: %s / %s\n\n"
                                    org-supertag-kanban--current-base-tag
                                    org-supertag-kanban--current-group-field)
                            'face '(:height 1.5 :weight bold)))
        (insert (propertize "Operations:\n" 'face '(:weight bold)))
        (insert " [h/l] Move Card  [RET] Edit Value  [E] Edit Field Def  [a/d] Add/Del Field\n")
        (insert " [n/p] Cursor Up/Down [TAB/S-TAB] Column Jump [v] View Node    [g] Refresh     [q] Quit\n\n")

        ;; Data Fetching and Preparation
        (let* ((grouped-nodes (org-supertag-kanban--group-nodes-by-field
                               org-supertag-kanban--current-base-tag
                               org-supertag-kanban--current-group-field))
               (tag-def (org-supertag-tag-get org-supertag-kanban--current-base-tag))
               (field-def (cl-find org-supertag-kanban--current-group-field
                                   (plist-get tag-def :fields)
                                   :key (lambda (f) (plist-get f :name)) :test #'string=))
               ;; Set the global headers var and use it as a local var. This is the fix.
               (column-headers (setq org-supertag-kanban--column-headers
                                     (if (eq (plist-get field-def :type) 'options)
                                         (plist-get field-def :options)
                                       (sort (hash-table-keys grouped-nodes) #'string<))))
               (all-nodes-lists (mapcar (lambda (key) (gethash key grouped-nodes)) column-headers))
               (col-count (length column-headers)))

          ;; 1. Insert column headers
          (dotimes (i col-count)
            (let* ((header-text (format "%s (%d)" (nth i column-headers) (length (nth i all-nodes-lists))))
                   (padded-header (org-supertag-view-util-pad-string header-text column-width 'center)))
              (insert padded-header)
              (when (< i (1- col-count)) (insert separator))))
          (insert "\n")
          (dotimes (i col-count)
            (insert (make-string column-width ?─))
            (when (< i (1- col-count)) (insert separator)))
          (insert "\n\n") ; Extra space after header

          ;; 2. Pre-render each column into a list of its lines.
          (let* ((rendered-columns
                  (mapcar
                   (lambda (nodes-list)
                     (let ((lines '()))
                       (dolist (node-id nodes-list)
                         (setq lines (append lines (org-supertag-kanban--format-card node-id column-width))))
                       lines))
                   all-nodes-lists))
                 (max-height (apply #'max 0 (mapcar #'length rendered-columns))))

            (if (zerop max-height)
                (insert "\n  No nodes found for this tag.\n")
              ;; 3. Print the board row by row, ensuring alignment.
              (dotimes (line-idx max-height)
                (dotimes (col-idx col-count)
                  (let* ((col-lines (nth col-idx rendered-columns))
                         (line-to-insert (or (nth line-idx col-lines) (make-string column-width ?\s)))
                         (group-value (nth col-idx column-headers)))
                    (let ((final-line (copy-sequence line-to-insert)))
                      (add-text-properties 0 (length final-line) `(group-value ,group-value) final-line)
                      (insert final-line)))
                  (when (< col-idx (1- col-count)) (insert separator)))
                (insert "\n")))))))
      (switch-to-buffer buffer)
      (delete-other-windows)))

;;---------------------------------------------------------------------
;; Interactive Commands
;;----------------------------------------------------------------------

(defun org-supertag-kanban--get-info-at-point ()
  "Return a plist of Kanban context info at point."
  (list :node-id (get-text-property (point) 'node-id)
        :base-tag (get-text-property (point) 'base-tag)
        :group-field (get-text-property (point) 'group-field)
        :group-value (get-text-property (point) 'group-value)))

(defun org-supertag-kanban--move-horizontally (direction)
  "Move cursor horizontally by one column in the Kanban view.
DIRECTION should be 1 for right, -1 for left."
  (org-supertag-view-util-move-horizontally-in-columns
   direction
   40 ; column-width
   2  ; separator-width
   (length org-supertag-kanban--column-headers)))

(defun org-supertag-kanban-next-column ()
  "Move to the next column to the right."
  (interactive)
  (org-supertag-kanban--move-horizontally 1))

(defun org-supertag-kanban-previous-column ()
  "Move to the previous column to the left."
  (interactive)
  (org-supertag-kanban--move-horizontally -1))

(defun org-supertag-kanban-move-card (direction)
  "Move the card at point in DIRECTION (:left or :right)."
  (when-let* ((info (org-supertag-kanban--get-info-at-point))
              (node-id (plist-get info :node-id))
              (base-tag (or (plist-get info :base-tag) org-supertag-kanban--current-base-tag))
              (group-field (or (plist-get info :group-field) org-supertag-kanban--current-group-field))
              (current-value (plist-get info :group-value)))
    (when (and node-id base-tag group-field current-value)
      (let* ((tag-def (org-supertag-tag-get base-tag))
             (field-def (cl-find group-field
                                 (plist-get tag-def :fields)
                                 :key (lambda (f) (plist-get f :name)) :test #'string=))
             (columns (if (eq (plist-get field-def :type) 'options)
                          (plist-get field-def :options)
                        (let ((grouped-nodes (org-supertag-kanban--group-nodes-by-field base-tag group-field)))
                          (sort (hash-table-keys grouped-nodes) #'string<))))
             (current-idx (cl-position current-value columns :test #'string=))
             (target-idx (+ current-idx (if (eq direction :left) -1 1))))
        (if (and current-idx (>= target-idx 0) (< target-idx (length columns)))
            (let ((new-value (nth target-idx columns)))
              (org-supertag-field-set-value node-id group-field new-value base-tag)
              (org-supertag-kanban-refresh)
              (message "Moved '%s' to '%s'" (or (plist-get (gethash node-id org-supertag-db--object) :title) node-id) new-value))
          (message "Cannot move further in that direction."))))))

(defun org-supertag-kanban-move-card-left ()
  "Move the card at point to the column on the left."
  (interactive)
  (org-supertag-kanban-move-card :left))

(defun org-supertag-kanban-move-card-right ()
  "Move the card at point to the column on the right."
  (interactive)
  (org-supertag-kanban-move-card :right))

(defun org-supertag-kanban-edit-field-value-at-point ()
  "Edit the value of the grouping field for the node at point."
  (interactive)
  ;; Get context info: node-id, base-tag, group-field-name
  (when-let* ((info (org-supertag-kanban--get-info-at-point))
              (node-id (plist-get info :node-id))
              (base-tag org-supertag-kanban--current-base-tag)
              (group-field-name org-supertag-kanban--current-group-field))
    ;; Get group field definition
    (let* ((tag-def (org-supertag-tag-get base-tag))
           (field-def (cl-find group-field-name (plist-get tag-def :fields)
                               :key (lambda (f) (plist-get f :name))
                               :test #'string=)))
      (unless field-def
        (user-error "Could not find definition for grouping field '%s' in tag '%s'"
                    group-field-name base-tag))
      ;; Get current value and call standard editor
      (let* ((current-value (org-supertag-field-get-value node-id group-field-name base-tag))
             (new-value (org-supertag-field-read-and-validate-value field-def current-value)))
        (when new-value
          (org-supertag-field-set-value node-id group-field-name new-value base-tag)
          (message "Updated '%s' for node '%s' to '%s'." group-field-name node-id new-value)
          (org-supertag-kanban-refresh))))))

;; --- Field Definition Management (reusing logic from table-view) ---

(defun org-supertag-kanban-add-field ()
  "Add a new field to the current base tag."
  (interactive)
  (let* ((base-tag org-supertag-kanban--current-base-tag)
         (field-name (read-string "New field name: ")))
    (when (and field-name (not (string-empty-p field-name)))
      ;; Get field type
      (let* ((field-type-choices (org-supertag-get-field-types))
             (field-type-str (completing-read "Field type: " (mapcar #'car field-type-choices)))
             (field-type (cdr (assoc field-type-str field-type-choices)))
             (field-def (list :name field-name :type field-type)))
        ;; If options type, get options
        (when (eq field-type 'options)
          (let* ((options-input (read-string "Options (comma separated): "))
                 (options-list (split-string options-input "," t "[ \t\n\r]+")))
            (setq field-def (plist-put field-def :options options-list))))
        ;; Add field and refresh
        (org-supertag-tag-add-field base-tag field-def)
        (message "Field '%s' added to tag '%s'." (plist-get field-def :name) base-tag)
        (org-supertag-kanban-refresh)))))

(defun org-supertag-kanban-delete-field-at-point ()
  "Delete a field from the Kanban's base tag."
  (interactive)
  (let* ((base-tag org-supertag-kanban--current-base-tag)
         (tag-def (org-supertag-tag-get base-tag))
         (fields (plist-get tag-def :fields))
         (field-names (mapcar (lambda (f) (plist-get f :name)) fields))
         (field-to-delete (completing-read "Delete which field: " field-names nil t)))
    (when (and field-to-delete (yes-or-no-p (format "Really delete field '%s' from tag '%s'?" field-to-delete base-tag)))
      (let* ((new-fields (cl-remove-if (lambda (f) (string= (plist-get f :name) field-to-delete)) fields))
             (new-tag (plist-put (copy-sequence tag-def) :fields new-fields)))
        (org-supertag-db-add base-tag new-tag)
        (message "Field '%s' removed from tag '%s'." field-to-delete base-tag)
        (org-supertag-kanban-refresh)))))

(defun org-supertag-kanban-edit-field-definition-at-point ()
  "Edit a field's definition for the Kanban's base tag."
  (interactive)
  (message "Functionality to edit field definition from Kanban view is not yet implemented."))

(defun org-supertag-kanban-view-node-at-point ()
  "Jump to the source of the node under the cursor."
  (interactive)
  (when-let* ((info (org-supertag-kanban--get-info-at-point))
              (node-id (plist-get info :node-id)))
    (org-supertag-view--goto-node node-id))) ; Reuses the goto helper

(defun org-supertag-kanban-refresh ()
  "Refresh the Kanban board view."
  (interactive)
  (org-supertag-kanban--refresh-display))

;;----------------------------------------------------------------------
;; Entry Point
;;----------------------------------------------------------------------

;;;###autoload
(defun org-supertag-view-kanban ()
  "Create an interactive Kanban board view based on a tag's field."
  (interactive)
  (let* ((tag-name (completing-read "Select a tag to build Kanban from: " (org-supertag-view--get-all-tags) nil t))
         (tag-def (when tag-name (org-supertag-tag-get tag-name))))
    (if (not tag-def)
        (message "No valid tag selected.")
      (let* ((fields (plist-get tag-def :fields))
             (field-names (mapcar (lambda (f) (plist-get f :name)) fields)))
        (if (not field-names)
            (message "Tag '%s' has no fields to group by." tag-name)
          (let ((field-name (completing-read "Group columns by which field: " field-names nil t)))
            (when field-name
              (setq org-supertag-kanban--current-base-tag tag-name)
              (setq org-supertag-kanban--current-group-field field-name)
              (org-supertag-kanban--refresh-display))))))))

(provide 'org-supertag-view-kanban)

;;; org-supertag-view-kanban.el ends here
