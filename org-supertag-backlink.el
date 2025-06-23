;;; org-supertag-backlink.el --- Backlink view for org-supertag -*- lexical-binding: t; -*-

(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-view-utils)

;;----------------------------------------------------------------------
;; Variables
;;----------------------------------------------------------------------

(defvar org-supertag-backlink--current-node-id nil
  "Current node ID being viewed in backlink buffer.")

;;----------------------------------------------------------------------
;; Mode Definition
;;----------------------------------------------------------------------

(defvar org-supertag-backlink-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "RET") 'org-supertag-backlink--view-node)
    (define-key map (kbd "g") 'org-supertag-backlink--refresh)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `org-supertag-backlink-mode'.")

(define-derived-mode org-supertag-backlink-mode special-mode "Org-Supertag-Backlink"
  "Major mode for viewing node backlinks."
  :group 'org-supertag)

;;----------------------------------------------------------------------
;; Core Functions
;;----------------------------------------------------------------------

(defun org-supertag-backlink--get-references (node-id)
  "Get all nodes referenced by NODE-ID.
Returns a list of node IDs."
  (when-let* ((node (org-supertag-db-get node-id)))
    (plist-get node :ref-to)))

(defun org-supertag-backlink--get-referenced-by (node-id)
  "Get all nodes that reference NODE-ID.
Returns a list of node IDs."
  (when-let* ((node (org-supertag-db-get node-id)))
    (plist-get node :ref-from)))

(defun org-supertag-backlink--format-node-content (node-id)
  "Format node content for display.
NODE-ID is the node identifier.
Returns formatted string with node content."
  (when-let* ((node (org-supertag-db-get node-id))
              (title (plist-get node :title))
              (file-path (plist-get node :file-path))
              (content (or (plist-get node :content) "")))
    (let* ((file-name (file-name-nondirectory file-path))
           (styled-title (propertize title 'face '(:weight bold))))
      (format "%s (%s)\n%s\n\n" styled-title file-name content))))

(defun org-supertag-backlink--show-buffer ()
  "Show backlink buffer with current node's references."
  (let ((buffer (get-buffer-create "*Org SuperTag Backlink*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-supertag-backlink-mode)
        
        ;; Header
        (when-let* ((node (org-supertag-db-get org-supertag-backlink--current-node-id))
                   (title (plist-get node :title)))
          (insert (propertize "Current Node: " 'face '(:weight bold)))
          (insert title "\n")
          
          ;; Reference counts
          (let ((refs-count (length (org-supertag-backlink--get-references org-supertag-backlink--current-node-id)))
                (refd-by-count (length (org-supertag-backlink--get-referenced-by org-supertag-backlink--current-node-id))))
            (insert (format "References: %d    Referenced By: %d\n\n" refs-count refd-by-count)))
          
          ;; References section
          (insert (propertize "References\n" 'face '(:weight bold)))
          (insert "────────────────────────────────\n")
          (let ((refs (org-supertag-backlink--get-references org-supertag-backlink--current-node-id))
                (index 1))
            (if refs
                (dolist (ref-id refs)
                  (let ((content (org-supertag-backlink--format-node-content ref-id)))
                    (insert (format "(%d) %s" index content))
                    (setq index (1+ index))))
              (insert "  No references found\n")))
          
          (insert "\n")
          
          ;; Referenced by section
          (insert (propertize "Referenced By\n" 'face '(:weight bold)))
          (insert "────────────────────────────────\n")
          (let ((refd-by (org-supertag-backlink--get-referenced-by org-supertag-backlink--current-node-id))
                (index 1))
            (if refd-by
                (dolist (ref-id refd-by)
                  (let ((content (org-supertag-backlink--format-node-content ref-id)))
                    (insert (format "(%d) %s" index content))
                    (setq index (1+ index))))
              (insert "  Not referenced by any nodes\n")))
          
          ;; Operations
          (insert "\nOperations:\n")
          (insert " [n/p] Navigate    [RET] View    [g] Refresh    [q] Quit\n\n")
          (insert "Note: Press [RET] to view the selected node"))))
    
    ;; Display buffer
    (org-supertag-view--display-buffer-right buffer)
    (select-window (get-buffer-window buffer))
    (goto-char (point-min))))

(defun org-supertag-backlink--view-node ()
  "View the node at current position."
  (interactive)
  (when-let* ((node-id (org-supertag-backlink--get-node-at-point)))
    (org-supertag-view--goto-node node-id)
    (select-window (get-buffer-window "*Org SuperTag Backlink*"))))

(defun org-supertag-backlink--get-node-at-point ()
  "Get node ID at current position in backlink buffer."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([0-9]+\\) \\(.*\\) (.*)")
      (let ((title (match-string 2)))
        (org-supertag-node--find-by-title title)))))

(defun org-supertag-backlink--refresh ()
  "Refresh the backlink buffer."
  (interactive)
  (when (eq major-mode 'org-supertag-backlink-mode)
    (org-supertag-backlink--show-buffer)))

;;;###autoload
(defun org-supertag-backlink-show ()
  "Show backlink window for current node."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Must be on a heading"))
  
  (let ((node-id (or (org-id-get)
                     (progn 
                       (org-supertag-node-create)
                       (org-id-get)))))
    (setq org-supertag-backlink--current-node-id node-id)
    (org-supertag-backlink--show-buffer)))

(provide 'org-supertag-backlink) 