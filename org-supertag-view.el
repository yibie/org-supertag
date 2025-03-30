;;; org-supertag-view.el --- View system for org-supertag -*- lexical-binding: t -*-

(require 'org-supertag-db)
(require 'org-supertag-relation)
(require 'cl-lib)


(unless (fboundp 'org-supertag-db-get-properties)
  (defun org-supertag-db-get-properties (id)
    "Get the list of all property names for an object ID."
    (let ((props (org-supertag-db-get id)))
      (cl-loop for (key _) on props by #'cddr
               collect key))))

(declare-function org-supertag-sim-find-similar "org-supertag-sim")
(declare-function org-supertag-sim-init "org-supertag-sim")
(defvar org-supertag-sim--initialized)

;; Database status check function
(defun org-supertag-db-initialized-p ()
  "Check if the org-supertag database is initialized.
Returns non-nil if the database has been loaded and contains data."
  (and (boundp 'org-supertag-db--object)
       org-supertag-db--object
       (> (hash-table-count org-supertag-db--object) 0)))

(defgroup org-supertag-view nil
  "Customization for org-supertag view system."
  :group 'org-supertag)

(defcustom org-supertag-view-max-cooccurring-tags 15
  "Maximum number of co-occurring tags to display in tag discovery view."
  :type 'integer
  :group 'org-supertag-view)

(defcustom org-supertag-view-columns-file
  (expand-file-name "supertag-columns.el"
                   (file-name-directory
                    (if (boundp 'org-supertag-db-file)
                        org-supertag-db-file
                      (expand-file-name "supertag-db.el" org-supertag-data-directory))))
  "File path for saved column view layouts."
  :type 'string
  :group 'org-supertag-view)

;; Load saved column configurations
(defun org-supertag-view--ensure-columns-file ()
  "Ensure the columns configuration file exists."
  (unless (file-exists-p org-supertag-view-columns-file)
    (with-temp-file org-supertag-view-columns-file
      (insert ";; org-supertag column view configurations\n\nnil\n"))))



(defvar org-supertag-view--current-filters nil
  "Current filter tags for tag-discover mode.")

(defcustom org-supertag-view-display-side 'right
  "Side to display org-supertag view panels.
Options are 'right, 'left, 'bottom, 'top, or nil (use default display behavior)."
  :type '(choice (const :tag "Right side" right)
                (const :tag "Left side" left)
                (const :tag "Bottom" bottom)
                (const :tag "Top" top)
                (const :tag "Default behavior" nil))
  :group 'org-supertag-view)

(defcustom org-supertag-view-window-width 0.4
  "Width of the side window used for org-supertag view panels (as fraction of frame width).
Only used when `org-supertag-view-display-side' is 'right or 'left."
  :type 'float
  :group 'org-supertag-view)

(defcustom org-supertag-view-window-height 0.4
  "Height of the side window used for org-supertag view panels (as fraction of frame height).
Only used when `org-supertag-view-display-side' is 'bottom or 'top."
  :type 'float
  :group 'org-supertag-view)

;;----------------------------------------------------------------------
;; Helper functions for tag operations
;;----------------------------------------------------------------------

(defun org-supertag-view--get-tag-name ()
  "Get tag name at point.
Returns nil if no tag is found at point."
  (save-excursion
    (when (thing-at-point-looking-at "#\\([[:alnum:]_-]+\\)")
      (let ((tag-name (match-string-no-properties 1)))
        (when (gethash tag-name org-supertag-db--object)
          tag-name)))))

(defun org-supertag-view--get-all-tags ()
  "Get all available tags from the database."
  (let ((tags '()))
    (maphash
     (lambda (id props)
       (when (eq (plist-get props :type) :tag)
         (push id tags)))
     org-supertag-db--object)
    tags))

(defun org-supertag-view-node-at-point ()
  "View the node at point in the SuperTag table view."
  (interactive)
  (when (looking-at "^\\([^ ]+\\)\\s-+\\([^ ]+\\)\\s-+\\([^ ]+\\)\\s-+\\(.*\\)$")
    (let* ((node-title (string-trim (match-string 1)))
           (node-id (org-supertag-view--find-node-by-title node-title)))
      (when node-id
        (org-supertag-view--goto-node node-id)))))

(defun org-supertag-view--find-node-by-title (title)
  "Find node ID matching TITLE.
Searches the database for a node with the given title and returns its ID."
  (let ((result nil))
    (maphash
     (lambda (id props)
       (when (and (not result)
                  (eq (plist-get props :type) :node)
                  (string= (plist-get props :title) title))
         (setq result id)))
     org-supertag-db--object)
    result))

(defun org-supertag-view-node (node-id)
  "View the node with NODE-ID.
This opens the node in an appropriate buffer for viewing."
  (if-let ((node-props (gethash node-id org-supertag-db--object))
           (file (plist-get node-props :file))
           (point (plist-get node-props :point)))
      (progn
        (find-file file)
        (goto-char point)
        (org-show-context))
    (message "Node %s Not Found" node-id)))

;;---------------------------------------------------------------------
;; Panel display functions
;;----------------------------------------------------------------------

(defun org-supertag-view--display-buffer-right (buffer)
  "Display BUFFER in side window according to `org-supertag-view-display-side'."
  (display-buffer
   buffer
   `(display-buffer-in-side-window
     (side . ,(or org-supertag-view-display-side 'right))
     (window-width . ,(when (memq org-supertag-view-display-side '(right left))
                       org-supertag-view-window-width))
     (window-height . ,(when (memq org-supertag-view-display-side '(top bottom))
                        org-supertag-view-window-height))
     (preserve-size . t))))


;;----------------------------------------------------------------------
;; Main functions
;;---------------------------------------------------------------------- 

;;;###autoload
(defun org-supertag-view-tag ()
  "Show content related to a tag with user's choice of view mode.
Available views:
- Tag-only view: Simple table view showing tag properties and related nodes
- Discover view: Interactive tag exploration and filtering
- Columns view: Multi-column view for comparing nodes"
  (interactive)
  (let* ((tag-at-point (org-supertag-view--get-tag-name))
         (view-options '(("Tag-only view" . tag-only)
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
        ('tag-only (org-supertag-view--show-content-table tag))
        ('discover (org-supertag-view--show-tag-discover (list tag)))
        ('columns  (org-supertag-tag-columns tag))
        (_ (error "Invalid view type"))))))



(defvar org-supertag-discover-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-view--refresh-discover)
    (define-key map (kbd "r") 'org-supertag-view--reset-filters)
    (define-key map (kbd "a") 'org-supertag-view--add-filter)
    (define-key map (kbd "d") 'org-supertag-view--remove-filter)
    (define-key map (kbd "v") 'org-supertag-view--view-node)
    (define-key map (kbd "m") 'org-supertag-view-manage-relations)
    (define-key map (kbd "1") 'org-supertag-view-switch-to-tag-only)
    (define-key map (kbd "3") 'org-supertag-view-switch-to-columns)
    (define-key map (kbd "n") 'next-line)  
    (define-key map (kbd "p") 'previous-line)  
    map)
  "Keymap for `org-supertag-discover-mode'.")      

(defvar org-supertag-view-column-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-view--update-column-view)
    (define-key map (kbd "a") 'org-supertag-view-add-column)
    (define-key map (kbd "A") 'org-supertag-view-add-related-column)
    (define-key map (kbd "t") 'org-supertag-view-add-tag-to-column)
    (define-key map (kbd "d") 'org-supertag-view-remove-column)
    (define-key map (kbd "R") 'org-supertag-view-reset-columns)
    (define-key map (kbd "v") 'org-supertag-view-view-node-at-point)
    (define-key map (kbd "m") 'org-supertag-view-manage-relations)
    (define-key map (kbd "S") 'org-supertag-view-save-current-columns)
    (define-key map (kbd "L") 'org-supertag-view-load-saved-columns)
    (define-key map (kbd "1") 'org-supertag-view-switch-to-tag-only)
    (define-key map (kbd "2") 'org-supertag-view-switch-to-discover)
    (define-key map (kbd "n") 'next-line)  
    (define-key map (kbd "p") 'previous-line)  
    map)
  "Keymap for multi-column tag view mode.")

(define-derived-mode org-supertag-discover-mode special-mode "Org-ST-Discover"
  "Major mode for progressive tag discovery in org-supertag."
  :group 'org-supertag
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq header-line-format 
        (propertize " Org-Supertag Tag Discovery" 'face '(:weight bold)))
  (setq-local org-element-use-cache nil)
  (setq-local org-mode-hook nil)
  ;; 确保不影响 minibuffer
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


(define-derived-mode org-supertag-view-table-mode org-mode "SuperTag-Table"
  "Major mode for displaying tag content in table format.
This mode is based on org-mode to ensure compatibility with org table functions.
\\{org-supertag-view-table-mode-map}"
  :group 'org-supertag
  (setq-local org-element-use-cache nil)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq header-line-format 
        (propertize " Org-Supertag Table View" 'face '(:weight bold)))
  (let ((map (make-sparse-keymap)))
    ;; 先定义最关键的编辑按键，确保它们有最高优先级
    (define-key map (kbd "e") (lambda () 
                               (interactive)
                               (let ((inhibit-read-only t))
                                 (call-interactively 'org-supertag-view-smart-edit))))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-supertag-view-refresh)
    (define-key map (kbd "v") 'org-supertag-view-table-node-at-point)
    (define-key map (kbd "V") 'org-supertag-view-table-view-all-nodes)
    (define-key map (kbd "m") 'org-supertag-view-manage-relations)
    (dolist (key '("d" "o" "r" "l" "a" "t"))
      (define-key map (kbd key) 
                 (lambda () 
                   (interactive)
                   (let ((inhibit-read-only t))
                     (call-interactively 
                      (intern (format "org-supertag-view-edit-%s-field"
                                     (pcase key
                                       ("d" "date")
                                       ("o" "options")
                                       ("r" "reference")
                                       ("l" "list")
                                       ("a" "range")
                                       ("t" "timestamp")))))))))
    (define-key map (kbd "C-c C-k") 'org-supertag-view-cancel-edit)
    (define-key map (kbd "C-c '") 'org-supertag-view-edit-table)
    (define-key map (kbd "<tab>") 'org-table-next-field)
    (define-key map (kbd "<backtab>") 'org-table-previous-field)
    (define-key map (kbd "M-p") 'org-table-copy-down)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "2") 'org-supertag-view-switch-to-discover)
    (define-key map (kbd "3") 'org-supertag-view-switch-to-columns)
    (set-keymap-parent map org-mode-map)
    (use-local-map map)))

(define-minor-mode org-supertag-view-mode
  "Minor mode for viewing org-supertag tag-related content."
  :lighter " SuperTag-View"
  :group 'org-supertag

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

