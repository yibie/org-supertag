;;; org-supertag-inline.el --- Support for inline tags in org-mode content -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024

;; Author: User <user@example.com>
;; Keywords: org-mode, tags, inline

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module adds support for inline tags within org-mode content.
;; Inline tags are prefixed with a '#' symbol (like #hashtags) and are
;; distinct from headline tags.
;;
;; Example:
;; "This is a paragraph with an #inline-tag that can be tracked and queried."

;;; Code:

(require 'org)
(require 'org-supertag-db)
(require 'org-supertag-node)
(require 'org-supertag-tag)

(defgroup org-supertag-inline nil
  "Customization options for org-supertag inline tags."
  :group 'org-supertag)

(defcustom org-supertag-inline-tag-regexp "#\\([[:alnum:]-_]+\\)"
  "Regular expression pattern to match inline tags.
The first capture group should match the tag name without the '#' prefix."
  :type 'regexp
  :group 'org-supertag-inline)

(defvar org-supertag-inline--overlays nil
  "List of overlays used for highlighting inline tags in the current buffer.")

(defun org-supertag-inline--clear-overlays ()
  "Remove all inline tag overlays from the current buffer."
  (dolist (ov org-supertag-inline--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq org-supertag-inline--overlays nil))

(defun org-supertag-inline--make-overlay (beg end)
  "Create an overlay for inline tag between BEG and END."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'org-supertag-inline-tag-face)
    (overlay-put ov 'org-supertag-inline t)
    ;; Add spaces before and after, but only if not already present
    (unless (eq (char-before beg) ? )  ; space
      (overlay-put ov 'before-string " "))
    (unless (eq (char-after end) ? )   ; space
      (overlay-put ov 'after-string " "))
    (push ov org-supertag-inline--overlays)
    ov))

(defun org-supertag-inline--highlight-region (beg end)
  "Highlight inline tags in region between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward org-supertag-inline-tag-regexp end t)
      (when (org-supertag-inline--valid-context-p)
        (org-supertag-inline--make-overlay (match-beginning 0) (match-end 0))))))

(defun org-supertag-inline--highlight-buffer ()
  "Highlight all inline tags in the current buffer."
  (org-supertag-inline--clear-overlays)
  (when org-supertag-inline-enable-fontification
    (org-supertag-inline--highlight-region (point-min) (point-max))))

(defun org-supertag-inline--after-change (beg end _len)
  "Function to run after changes in buffer.
Updates inline tag highlighting between BEG and END."
  (when org-supertag-inline-enable-fontification
    ;; Remove overlays in the changed region
    (dolist (ov (overlays-in beg end))
      (when (overlay-get ov 'org-supertag-inline)
        (delete-overlay ov)
        (setq org-supertag-inline--overlays (delq ov org-supertag-inline--overlays))))
    ;; Re-highlight the changed region
    (save-excursion
      (let ((end-line (save-excursion (goto-char end) (line-end-position))))
        (org-supertag-inline--highlight-region beg end-line)))))

(defun org-supertag-inline--valid-context-p ()
  "Check if the current point is in a valid context for inline tags.
Returns nil if the point is in one of the contexts listed in
`org-supertag-inline-excluded-contexts'."
  (let ((context (org-element-context)))
    (not (memq (org-element-type context) org-supertag-inline-excluded-contexts))))

;;;###autoload
(defun org-supertag-inline-insert-tag (tag-name)
  "Insert an inline tag at point and establish proper relationships.
TAG-NAME is the name of the tag to insert."
  (interactive
   (list (let* ((all-tags (org-supertag-get-all-tags))
                (preset-names (mapcar #'car org-supertag-preset-tags))
                ;; Remove existing preset tags to avoid duplicates
                (user-tags (cl-remove-if (lambda (tag) (member tag preset-names)) all-tags))
                (candidates (delete-dups
                            (append 
                             ;; Use [P] prefix to mark preset tags
                             (mapcar (lambda (name) 
                                     (format "[P] %s" name))
                                   preset-names)
                             ;; Regular tags are kept as is
                             user-tags)))
                (input (completing-read
                       "Inline tag: "
                       candidates nil nil)))
           ;; Process input, remove [P] prefix
           (if (string-prefix-p "[P] " input)
               (substring input 4)
             input))))
  (when tag-name
    (let* (;; Check if input ends with #, if so, create new tag directly
           (direct-create (string-suffix-p "#" tag-name))
           (tag-name-clean (if direct-create
                             (substring tag-name 0 -1)
                           tag-name))
           (sanitized-name (org-supertag-sanitize-tag-name tag-name-clean))
           ;; Ensure current node exists
           (node-id (org-id-get-create)))      
      ;; Get or create tag
      (let ((tag-id
             (cond
              ;; If tag exists, use it directly
              ((org-supertag-tag-exists-p sanitized-name)
               sanitized-name)
              ;; Otherwise, create new tag
              (t
               (if (or direct-create
                       (y-or-n-p (format "Create new tag '%s'? " sanitized-name)))
                   (org-supertag-tag-create sanitized-name)
                 (user-error "Tag creation cancelled"))))))
        ;; Add space before tag if needed
        (unless (or (bobp) (eq (char-before) ? ))
          (insert " "))
        ;; Insert inline tag
        (insert (concat "#" tag-id))
        ;; Add space after tag if needed
        (unless (or (eobp) (eq (char-after) ? ))
          (insert " "))
        ;; Use org-supertag-tag-apply to apply the tag
        (save-excursion
          (org-back-to-heading t)
          (let ((org-supertag-tag-apply-skip-headline t))
            (org-supertag-tag-apply tag-id)))
        
        (message "Inserted inline tag #%s" tag-id)))))

;; Fontification

(defun org-supertag-inline-refresh ()
  "Refresh inline tag highlighting in the current buffer."
  (interactive)
  (when (and org-supertag-inline-mode 
             org-supertag-inline-enable-fontification)
    (org-supertag-inline--highlight-buffer)
    (message "Refreshed inline tag highlighting")))

;; Minor mode for inline tags

;;;###autoload
(define-minor-mode org-supertag-inline-mode
  "Toggle org-supertag inline tag mode.
When enabled, inline tags (prefixed with #) can be inserted."
  :lighter " OrgST-Inline"
  (if org-supertag-inline-mode
      (local-set-key (kbd "C-c C-x #") 'org-supertag-inline-insert-tag)
    (local-unset-key (kbd "C-c C-x #"))))

;;;###autoload
(defun org-supertag-inline-setup ()
  "Set up the inline tag system."
  (interactive)
  (add-hook 'org-supertag-mode-hook
            (lambda ()
              (org-supertag-inline-mode (if org-supertag-mode 1 -1))))
  (message "Inline tag support enabled. Use C-c C-x # to insert tags."))

(provide 'org-supertag-inline)
;;; org-supertag-inline.el ends here 
