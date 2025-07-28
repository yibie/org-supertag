;;; org-supertag-view-grid.el --- Grid/Table view for org-supertag -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a grid-style view for org-supertag, inspired by
;; the "table writing method" for brainstorming and content organization.
;;
;; Unlike Kanban or column views, the grid view displays nodes as rows
;; and selected fields as columns, allowing for direct, multi-line editing
;; in each cell.

;;; Code:

(require 'org-supertag-view-utils)
(require 'org-supertag-db)
(require 'org-supertag-tag)
(require 'org-supertag-field)

;;----------------------------------------------------------------------
;; State Variables
;;----------------------------------------------------------------------

(defvar org-supertag-grid--current-base-tag nil
  "The base tag for the current grid view. e.g., "novel-scene".")

(defvar org-supertag-grid--current-fields nil
  "The list of field names to be displayed as columns.")

(defvar org-supertag-grid--grid-data nil
  "The prepared data for the grid, as a list of rows, where each row is a list of cell values.")

(defvar org-supertag-grid--column-widths nil
  "A list of calculated widths for each column.")

;;----------------------------------------------------------------------
;; Mode & Keymap Definition
;;----------------------------------------------------------------------

(defvar org-supertag-grid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-grid-refresh)
    ;; Navigation
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "h") 'org-supertag-grid-previous-cell)
    (define-key map (kbd "l") 'org-supertag-grid-next-cell)
    (define-key map (kbd "<up>") 'previous-line)
    (define-key map (kbd "<down>") 'next-line)
    (define-key map (kbd "<left>") 'org-supertag-grid-previous-cell)
    (define-key map (kbd "<right>") 'org-supertag-grid-next-cell)
    ;; Editing
    (define-key map (kbd "RET") 'org-supertag-grid-edit-cell-at-point)
    ;; Node Actions
    (define-key map (kbd "v") 'org-supertag-grid-view-node-at-point)
    map)
  "Keymap for `org-supertag-grid-mode'.")

;;----------------------------------------------------------------------
;; Data Preparation
;;----------------------------------------------------------------------

(defun org-supertag-grid--prepare-data (base-tag fields)
  "Prepare the data for the grid view.
Returns a list of rows, where each row corresponds to a node and
is a list of cell values for the specified FIELDS."
  ;; This is a placeholder. The actual implementation will fetch nodes
  ;; and their field values.
  (list
   (list "Node 1, Field 1 Value" "Node 1, Field 2 Value (long content that needs wrapping)")
   (list "Node 2, Field 1 Value" "Node 2, Field 2 Value")))

(defun org-supertag-grid--calculate-column-widths (headers data)
  "Calculate the optimal width for each column."
  ;; Placeholder implementation.
  (make-list (length headers) 40))

;;----------------------------------------------------------------------
;; Core Rendering Function
;;----------------------------------------------------------------------

(defun org-supertag-grid--refresh-display ()
  "Render the grid view based on the current state variables."
  (let ((buffer (get-buffer-create "Org SuperTag Grid")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; (org-supertag-grid-mode) ; Mode will be defined later
        (insert (propertize (format "Grid View: %s\n\n" org-supertag-grid--current-base-tag)
                            'face '(:height 1.5 :weight bold)))
        (insert "Placeholder for the grid view.\n")
        (insert "This will be replaced with the actual grid rendering logic.\n\n")
        (insert (format "Base Tag: %s\n" org-supertag-grid--current-base-tag))
        (insert (format "Fields: %s\n" (mapconcat #'identity org-supertag-grid--current-fields ", ")))
        (setq-local mode-line-format
                    '("Grid: "
                      (:eval org-supertag-grid--current-base-tag)
                      " | "
                      (:eval (mapconcat #'identity org-supertag-grid--current-fields ", "))))))
    (switch-to-buffer buffer)
    (delete-other-windows)))

;;---------------------------------------------------------------------
;; Interactive Commands
;;----------------------------------------------------------------------

(defun org-supertag-grid-refresh ()
  "Refresh the grid view."
  (interactive)
  (org-supertag-grid--refresh-display))

(defun org-supertag-grid-edit-cell-at-point ()
  "Edit the value of the cell at point."
  (interactive)
  (message "Editing cell at point (not yet implemented)."))

(defun org-supertag-grid-view-node-at-point ()
  "Jump to the source of the node in the current row."
  (interactive)
  (message "Jumping to node source (not yet implemented)."))

(defun org-supertag-grid-next-cell ()
  "Move to the next cell in the row."
  (interactive)
  (message "Moving to next cell (not yet implemented)."))

(defun org-supertag-grid-previous-cell ()
  "Move to the previous cell in the row."
  (interactive)
  (message "Moving to previous cell (not yet implemented)."))

;;----------------------------------------------------------------------
;; Entry Point
;;----------------------------------------------------------------------

;;;###autoload
(defun org-supertag-view-grid ()
  "Create an interactive grid view for a tag and its selected fields."
  (interactive)
  (let* ((tag-name (completing-read "Select a tag to build grid from: " (org-supertag-view--get-all-tags) nil t))
         (tag-def (when tag-name (org-supertag-tag-get tag-name))))
    (if (not tag-def)
        (message "No valid tag selected.")
      (let* ((fields (plist-get tag-def :fields))
             (field-names (mapcar (lambda (f) (plist-get f :name)) fields))
             ;; For selecting multiple fields, we need a better UI,
             ;; but for now, we'll use a simple comma-separated input.
             (selected-fields-str (read-string (format "Select fields (comma-separated) from [%s]: "
                                                       (mapconcat #'identity field-names ", "))))
             (selected-fields (split-string selected-fields-str "," t "[ \t\n\r]+")))
        (if (not selected-fields)
            (message "No fields selected.")
          (setq org-supertag-grid--current-base-tag tag-name)
          (setq org-supertag-grid--current-fields selected-fields)
          (org-supertag-grid--refresh-display))))))

(provide 'org-supertag-view-grid)

;;; org-supertag-view-grid.el ends here
