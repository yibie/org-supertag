;; org-supertag-view-utils.el --- Utility functions for org-supertag view system -*- lexical-binding: t -*-

(unless (fboundp 'org-supertag-db-get-properties)
  (defun org-supertag-db-get-properties (id)
    "Get the list of all property names for an object ID."
    (let ((props (org-supertag-db-get id)))
      (cl-loop for (key _) on props by #'cddr
               collect key))))

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
  (let ((dir (file-name-directory org-supertag-view-columns-file)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (unless (file-exists-p org-supertag-view-columns-file)
      (with-temp-file org-supertag-view-columns-file
        (insert ";; org-supertag column view configurations\n\nnil\n")))))



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

(defun org-supertag-view-util-wrap-text (text available-width)
  "Wrap TEXT to fit within AVAILABLE-WIDTH. Returns a list of wrapped lines."
  (if (<= (string-width text) available-width)
      (list text)
    (let ((words (split-string text " "))
          (lines '())
          (current-line ""))
      (dolist (word words)
        (let ((new-line (if (string-empty-p current-line)
                            word
                          (concat current-line " " word))))
          (if (<= (string-width new-line) available-width)
              (setq current-line new-line)
            (push current-line lines)
            (setq current-line word))))
      (when (not (string-empty-p current-line))
        (push current-line lines))
      (nreverse lines))))

(defun org-supertag-view-util-pad-string (str width &optional align)
  "Pad STR with spaces to fit WIDTH.
ALIGN can be :left, :right, or :center. Defaults to :left."
  (let* ((align (or align :left))) ; Set default value
    (let* ((len (string-width str))
           (padding-needed (- width len)))
      (if (<= padding-needed 0)
         (truncate-string-to-width str width)
       (pcase align
         (:left (concat str (make-string padding-needed ?\s)))
         (:right (concat (make-string padding-needed ?\s) str))
         (:center (let* ((left-pad (floor padding-needed 2))
                         (right-pad (- padding-needed left-pad)))
                   (concat (make-string left-pad ?\s)
                           str
                           (make-string right-pad ?\s))))
         ;; Fallback for safety
         (t (concat str (make-string padding-needed ?\s))))))))

(provide 'org-supertag-view-utils)
