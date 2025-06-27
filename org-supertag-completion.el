;;; org-supertag-completion.el --- Universal and robust completion for org-supertag -*- lexical-binding: t; -*-

;; This file provides a completion-at-point function (CAPF) for org-supertag.
;; It uses the classic, most compatible CAPF design pattern to ensure it works
;; correctly across all completion UIs, including company-mode and corfu.
;;
;; The core principle is to return a list of PURE, PROPERTIZED STRINGS,
;; and use a SINGLE :exit-function that inspects the properties of the
;; selected string to decide on the action. This is the "lowest common
;; denominator" approach that all completion frameworks understand.

(require 'org-supertag-tag)
(require 'org-supertag-inline)

;;;----------------------------------------------------------------------
;;; Shared Logic
;;;----------------------------------------------------------------------

(defun org-supertag--get-prefix-bounds ()
  "Find the bounds of a tag prefix (like '#tag') before point."
  (save-excursion
    (when (re-search-backward "#\\([^ \t\n\r]+\\)" nil t)
      (cons (match-beginning 1) (match-end 1)))))

(defun org-supertag--get-completion-table (prefix)
  "Return a list of candidates, with '[Create New Tag]' propertized."
  (let* ((safe-prefix (or prefix ""))
         (all-tags (org-supertag-get-all-tags))
         (matching-tags (all-completions safe-prefix all-tags))
         (new-tag-candidate "[Create New Tag]"))
    (if (and (not (string-empty-p safe-prefix))
             (not (member safe-prefix matching-tags)))
        ;; If creating a new tag is possible, add the special candidate
        ;; to the list and attach the `is-new-tag` property to it.
        (cons (propertize new-tag-candidate 'is-new-tag t) matching-tags)
      matching-tags)))

(defun org-supertag--post-completion-action (selected-string original-prefix)
  "The single, unified post-completion action.
It now correctly calls the main `org-supertag-tag-apply` function
to ensure database relations and behaviors are triggered."
  ;; First, clean up what the completion UI inserted.
  (delete-region (- (point) (length selected-string)) (point))
  (when (eq (char-before) ?#) (delete-char -1))

  ;; Now, determine the tag and apply it using the unified function.
  (let* ((is-new (get-text-property 0 'is-new-tag selected-string))
         (tag-name-raw (if is-new original-prefix selected-string))
         ;; `ensure-tag` handles creation for new tags
         (tag-info (org-supertag-inline--ensure-tag tag-name-raw))
         (tag-id (plist-get tag-info :tag-id))
         (node-id (org-id-get-create)))

    (when (and node-id tag-id)
      ;; We do NOT skip text insertion here. We've just deleted the
      ;; completion text, so we rely on `tag-apply` to insert the
      ;; final, properly-formatted tag text.
      (let ((org-supertag-force-node-id node-id))
        (org-supertag-tag-apply tag-id))
      (message "Tag '%s' applied to node %s." tag-id node-id))))


;;;----------------------------------------------------------------------
;;; Main CAPF Entry Point
;;;----------------------------------------------------------------------

(defun org-supertag-completion-at-point ()
  "Main `completion-at-point` function using the classic, compatible API."
  (when-let* ((bounds (org-supertag--get-prefix-bounds))
              (start (car bounds))
              (end (cdr bounds))
              (prefix (buffer-substring-no-properties start end)))

    (list start end
          ;; 1. The completion table. It returns a simple list of
          ;;    propertized strings. This is universally understood.
          (lambda (str pred action)
            (if (eq action 'metadata)
                '((category . org-supertag))
              (org-supertag--get-completion-table prefix)))

          ;; 2. A SINGLE, UNIFIED :exit-function. This is also
          ;;    universally understood by all completion frameworks.
          :exit-function
          (lambda (selected-string status)
            (when (eq status 'finished)
              (org-supertag--post-completion-action selected-string prefix))))))


;;;----------------------------------------------------------------------
;;; Setup
;;;----------------------------------------------------------------------

;;;###autoload
(defun org-supertag-setup-completion ()
  "Setup completion for org-supertag."
  (add-to-list 'completion-at-point-functions
               #'org-supertag-completion-at-point))

(add-hook 'org-mode-hook #'org-supertag-setup-completion)

(provide 'org-supertag-completion)