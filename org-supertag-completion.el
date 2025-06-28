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
  "Find the bounds of a tag prefix at point, if any.
Completion should only trigger when the point is immediately after
a hash and a sequence of valid tag characters."
  (let ((end (point)))
    (save-excursion
      (skip-chars-backward org-supertag-inline--valid-tag-chars)
      (when (and (> (point) (point-min))
               (eq (char-before) ?#))
        ;; Return the bounds of the text part of the tag (after the '#')
        (cons (point) end)))))

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
It handles both existing and new tags correctly by inspecting the
completion candidate and correcting the buffer if necessary."
  (let* ((is-new (get-text-property 0 'is-new-tag selected-string))
         ;; For new tags, the REAL tag name is the prefix the user typed.
         ;; For existing tags, it's the candidate they selected.
         (tag-name (org-supertag-sanitize-tag-name (if is-new original-prefix selected-string))))

    (when (and tag-name (not (string-empty-p tag-name)))

      ;; --- CRITICAL FIX ---
      ;; If this is a new tag, the completion UI has inserted the placeholder
      ;; text "[Create New Tag]". We MUST delete that and insert the actual
      ;; tag name that the user typed (`original-prefix`).
      (when is-new
        (delete-region (- (point) (length selected-string)) (point))
        (insert original-prefix))

      ;; Now the buffer is in the correct state (e.g., "#mynewtag").
      ;; We can proceed with the backend logic, but we must tell `tag-apply`
      ;; NOT to insert text again, as it's already correct in the buffer.
      (let ((org-supertag-skip-text-insertion t))
        (when is-new
          (unless (org-supertag-tag-exists-p tag-name)
            (org-supertag-tag--create tag-name)))
        ;; This handles DB relations and behaviors.
        (org-supertag-tag-apply tag-name))

      ;; Finally, add the trailing space to delimit the tag.
      (insert " ")
      (message "Tag '%s' applied." tag-name))))


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
            ;; The condition now accepts 'finished, 'exact', and 'sole' to be
            ;; compatible with various completion UIs like Corfu.
            (when (memq status '(finished exact sole))
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