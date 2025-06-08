;;; org-supertag-ui-sidebar.el --- Core management for the UI sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the core, content-agnostic functionality for managing
;; the org-supertag \"Intelligent Sidebar\". It handles window creation,
;; buffer management, and the structure of sections, but does not handle
;; the content of those sections.

;;; Code:

(require 'org-supertag-node)
(require 'org-supertag-db)
(require 'window)

;; --- Variables ---

(defcustom org-supertag-ui-sidebar-buffer-name "*org-supertag-intelligent-sidebar*"
  "Name of the buffer used for the RAG intelligent sidebar."
  :type 'string
  :group 'org-supertag)

(defcustom org-supertag-ui-sidebar-window-width 45
  "Width of the sidebar window in characters."
  :type 'integer
  :group 'org-supertag)

(defvar org-supertag-ui-sidebar-sections
  '((:dialogue . "💬 对话窗口")
    (:nodes . "🔗 关联节点")
    (:ner-tags . "📝 建议标签")
    (:ner-rels . "🔗 建议关系")
    (:archaeology . "🏺 知识考古")
    (:memory . "🧠 候选记忆"))
  "Alist defining the sections of the sidebar. (id . \"Display Title\")")

(defvar-local org-supertag-ui-sidebar--session-id nil
  "Buffer-local variable to hold the unique session ID for the RAG dialogue.")

;; --- Session Management ---

(defun org-supertag-ui-sidebar-get-session-id ()
  "Get or create a unique session ID for the current RAG buffer.
The ID is stored in a buffer-local variable `org-supertag-ui-sidebar--session-id`."
  (unless org-supertag-ui-sidebar--session-id
    (setq-local org-supertag-ui-sidebar--session-id
                (format "emacs-session-%s-%s"
                        (user-login-name)
                        (replace-regexp-in-string
                         "[^a-zA-Z0-9-]" ""
                         (format "%s" (current-time))))))
  org-supertag-ui-sidebar--session-id)


;; --- Sidebar Management ---

(defun org-supertag-ui-sidebar--get-buffer (&optional force-create)
  "Get or create the RAG sidebar buffer.
If FORCE-CREATE is non-nil, create a new buffer even if one exists."
  (let ((buf (get-buffer org-supertag-ui-sidebar-buffer-name)))
    (when (or force-create (not buf) (not (buffer-live-p buf)))
      (setq buf (get-buffer-create org-supertag-ui-sidebar-buffer-name))
      (with-current-buffer buf
        (setq-local truncate-lines t)
        (erase-buffer)
        (org-supertag-ui-sidebar-get-session-id) ; Initialize session ID
        (dolist (section org-supertag-ui-sidebar-sections)
          (let ((title (cdr section)))
            (insert (propertize (format "╭─ %s " title) 'face 'font-lock-comment-face))
            (insert (propertize (make-string (max 0 (- org-supertag-ui-sidebar-window-width 4 (length title))) ?─) 'face 'font-lock-comment-face))
            (insert (propertize "╮\n" 'face 'font-lock-comment-face))
            (insert "│\n") ; Placeholder for content
            (insert (propertize (format "╰%s╯\n\n" (make-string (max 0 (- org-supertag-ui-sidebar-window-width 2)) ?─)) 'face 'font-lock-comment-face)))))))
    buf)

(defun org-supertag-ui-sidebar-show ()
  "Display the intelligent sidebar in a side window."
  (interactive)
  (let ((sidebar-buffer (org-supertag-ui-sidebar--get-buffer)))
    (display-buffer-in-side-window sidebar-buffer
                                   `((side . left)
                                     (window-width . ,org-supertag-ui-sidebar-window-width)
                                     (slot . 0)
                                     (window-parameters . ((no-other-window . t)
                                                           (mode-line-format . none)
                                                           (header-line-format . "🧠 Living Document Intelligence")))))))

(defun org-supertag-ui-sidebar--find-section-start (buffer section-title)
  "Find the starting point (after title) of a SECTION-TITLE in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (search-forward (format "╭─ %s " section-title) nil t)
      (line-end-position))))

(defun org-supertag-ui-sidebar--find-section-end (buffer section-start-pos)
  "Find the end point (before next section or buffer end) of a section in BUFFER."
  (with-current-buffer buffer
    (goto-char section-start-pos)
    (save-excursion
      (if (search-forward "\n╭─ " nil t) ; Start of next section
          (match-beginning 0)
        (point-max))))) ; End of buffer if no next section

(defun org-supertag-ui-sidebar-render (section-id content &optional clear-section-p)
  "Render CONTENT into a specific SECTION-ID in the sidebar.
SECTION-ID is a keyword like :dialogue, :tags, :nodes.
If CLEAR-SECTION-P is non-nil, existing content in that section is cleared first.
CONTENT can be a string, or a function that inserts content."
  (let* ((sidebar-buffer (org-supertag-ui-sidebar--get-buffer))
         (section-info (assoc section-id org-supertag-ui-sidebar-sections))
         (section-title (cdr section-info)))
    (unless section-info
      (error "Unknown sidebar section ID: %S" section-id))
    (with-current-buffer sidebar-buffer
      (let ((inhibit-read-only t)
            (start-point (org-supertag-ui-sidebar--find-section-start sidebar-buffer section-title))
            (end-point))
        (unless start-point
          (error "Could not find section '%s' in sidebar" section-title))

        (goto-char start-point)
        (forward-line 1)
        (setq start-point (point))

        (setq end-point (save-excursion
                          (search-forward-regexp (format "^╰%s╯" (make-string (max 0 (- org-supertag-ui-sidebar-window-width 2)) ?─)) nil t)
                          (match-beginning 0)))

        (when clear-section-p
          (delete-region start-point end-point))

        (goto-char end-point)
        (unless (eq (char-before end-point) ?\n) (insert "\n"))
        
        (if (functionp content)
            (funcall content)
          (insert content))
        
        (unless (eq (char-before) ?\n) (insert "\n"))

        (when (get-buffer-window sidebar-buffer)
          (with-selected-window (get-buffer-window sidebar-buffer)
            (set-window-point (selected-window) (point-max))))))))

(defun org-supertag-ui-sidebar--insert-section (title content)
  "Insert a foldable section with TITLE and CONTENT into the current buffer."
  (let ((inhibit-read-only t))
    (insert (propertize (format "▼ %s\n" title)
                        'face 'font-lock-comment-face))
    (insert (propertize (make-string (max 0 (- org-supertag-ui-sidebar-window-width 4 (length title))) ?─) 'face 'font-lock-comment-face))
    (insert (propertize "╮\n" 'face 'font-lock-comment-face))
    (insert "│\n") ; Placeholder for content
    (insert (propertize (format "╰%s╯\n\n" (make-string (max 0 (- org-supertag-ui-sidebar-window-width 2)) ?─)) 'face 'font-lock-comment-face))
    (insert content)
    (insert "\n")))

(provide 'org-supertag-ui-sidebar)
;;; org-supertag-ui-sidebar.el ends here 