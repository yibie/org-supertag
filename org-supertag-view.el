;;; org-supertag-view.el --- View system for org-supertag -*- lexical-binding: t -*-

(require 'org-supertag-db)
(require 'org-supertag-relation)
(require 'cl-lib)
(require 'org-supertag-view-utils)
(require 'org-supertag-view-table)
(require 'org-supertag-view-discover)
(require 'org-supertag-view-column)
(require 'org-supertag-view-node)
(require 'org-supertag-view-chat)
(require 'org-supertag-view-kanban)

;;----------------------------------------------------------------------
;; Main functions
;;---------------------------------------------------------------------- 

;;;###autoload
(defun org-supertag-view-node ()
  "Show the unified view for the node at point."
  (interactive)
  (org-supertag-view-node-show))

;;;###autoload
(defun org-supertag-view-tag ()
  "Show content related to a tag with user's choice of view mode.
Available views:
- Tag-only view: Simple table view showing tag properties and related nodes
- Discover view: Interactive tag exploration and filtering
- Columns view: Multi-column view for comparing nodes"
  (interactive)
  (let* ((tag-at-point (org-supertag-view--get-tag-name))
         (view-options '(("Table view" . tag-only)
                        ("Discover view" . discover)
                        ("Columns view" . columns)))
         (view-choice (completing-read "Select view mode: "
                                     view-options nil t))
         (view-type (cdr (assoc view-choice view-options)))
         (tag (or tag-at-point
                 (completing-read "Select tag: "
                                (org-supertag-view--get-all-tags)
                                nil t))))
    (when tag
      (pcase view-type
        ('tag-only (org-supertag-view-table tag))
        ('discover (org-supertag-view-discover (list tag)))
        ('columns  (org-supertag-view-column tag))
        (_ (error "Invalid view type"))))))

(define-derived-mode org-supertag-discover-mode special-mode "SuperTag-Discover"
  "Major mode for progressive tag discovery in org-supertag."
  :group 'org-supertag
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq header-line-format 
        (propertize " Org-Supertag Tag Discovery" 'face '(:weight bold)))
  (setq-local org-element-use-cache nil)
  (setq-local org-mode-hook nil)
  (setq-local minibuffer-auto-raise t)
  (setq-local enable-recursive-minibuffers t))

(define-derived-mode org-supertag-column-mode special-mode "SuperTag-Columns"
  "Major mode for multi-column tag comparison in org-supertag."
  :group 'org-supertag
  (setq buffer-read-only t)
  (setq truncate-lines t)  
  (buffer-disable-undo)
  (setq header-line-format 
        (propertize " Org-Supertag Multi-Column Tag View" 'face '(:weight bold)))
  (setq-local org-element-use-cache nil)
  (setq-local org-mode-hook nil)
  (use-local-map org-supertag-view-column-mode-map))

(define-derived-mode org-supertag-kanban-mode special-mode "SuperTag-Kanban"
  "Major mode for the interactive Kanban board view."
  :group 'org-supertag
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (buffer-disable-undo)
  (setq header-line-format
        (propertize " Org-Supertag Kanban View" 'face '(:weight bold)))
  (use-local-map org-supertag-kanban-mode-map))

(defvar org-supertag-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'org-supertag-feedback-submit)
    map)
  "Keymap for org-supertag-view-mode.")

(define-minor-mode org-supertag-view-mode
  "Minor mode for viewing org-supertag tag-related content."
  :lighter " SuperTag-View"
  :group 'org-supertag
  :keymap org-supertag-view-mode-map

  (if org-supertag-view-mode
      ;; When enabling the mode
      (when (string-match-p "\\*Org SuperTag" (buffer-name))
        ;; Only set read-only for SuperTag view buffers, not regular org files
        (setq-local org-supertag-view--prev-read-only buffer-read-only)
        (setq buffer-read-only t)
        (buffer-disable-undo))
    ;; When disabling the mode
    (when (boundp 'org-supertag-view--prev-read-only)
      (setq buffer-read-only org-supertag-view--prev-read-only))))

(provide 'org-supertag-view)
;;; org-supertag-view.el ends here