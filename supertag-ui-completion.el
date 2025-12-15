;;; supertag-ui-completion.el --- Universal and robust completion for org-supertag -*- lexical-binding: t; -*-

;; This file provides a completion-at-point function (CAPF) for org-supertag.
;; It uses the classic, most compatible CAPF design pattern to ensure it works
;; correctly across all completion UIs, including company-mode and corfu.
;;
;; The core principle is to return a list of PURE, PROPERTIZED STRINGS,
;; and use a SINGLE :exit-function that inspects the properties of the
;; selected string to decide on the action. This is the "lowest common
;; denominator" approach that all completion frameworks understand.

(require 'org)
(require 'org-id)
(require 'cl-lib)

;; Required dependencies for supertag architecture
;; These MUST be loaded for completion to work correctly
(require 'supertag-core-store)
(require 'supertag-core-scan)
(require 'supertag-ops-tag)
(require 'supertag-ops-node)
(require 'supertag-services-query)

;;;----------------------------------------------------------------------
;;; Customization
;;;----------------------------------------------------------------------

(defgroup supertag-completion nil
  "Completion settings for org-supertag."
  :group 'org-supertag
  :prefix "supertag-completion-")

(defcustom supertag-completion-auto-enable t
  "Whether to automatically enable tag completion in org-mode buffers.
When non-nil, `global-supertag-ui-completion-mode' will be enabled by default."
  :type 'boolean
  :group 'supertag-completion)

;;;----------------------------------------------------------------------
;;; Helper Functions
;;;----------------------------------------------------------------------

(defun supertag-completion--get-all-tags ()
  "Get all available tag names from the supertag store."
  (condition-case err
      (let ((tags-ht (and (fboundp 'supertag-store-get-collection)
                          (supertag-store-get-collection :tags)))
            (all-tags '()))
        (if (hash-table-p tags-ht)
            ;; Primary method: get all tag IDs from :tags collection
            (progn
              (maphash (lambda (tag-id _tag-data)
                         (push tag-id all-tags))
                       tags-ht)
              (nreverse all-tags))
          ;; Fallback: scan nodes to collect unique tags
          (let ((nodes-ht (and (fboundp 'supertag-store-get-collection)
                               (supertag-store-get-collection :nodes))))
            (when (hash-table-p nodes-ht)
              (maphash (lambda (_node-id node-data)
                         (when-let ((tags (plist-get node-data :tags)))
                           (setq all-tags (append tags all-tags))))
                       nodes-ht))
            (delete-dups all-tags))))
    (error
     (message "supertag-completion: Failed to get tags: %S" err)
     '())))

(defun supertag-completion--get-node-tags (node-id)
  "Get tags currently applied to NODE-ID."
  (when-let ((node-data (supertag-node-get node-id)))
    (plist-get node-data :tags)))

(defun supertag-completion--valid-tag-char-p (char)
  "Return non-nil if CHAR should be considered part of a tag name.
Anything except whitespace/control characters and # counts as valid.
This keeps completion flexible enough for emoji and other symbols."
  (and char
       (not (memq char '(?\s ?\t ?\n ?\r ?#)))))

(defun supertag-completion--get-prefix-bounds ()
  "Find the bounds of a tag prefix at point, if any.
Returns (START . END) where START is right after the # character."
  (save-excursion
    (let* ((end (point))
           (start nil)
           (result nil))

      ;; Walk backwards over everything that counts as part of the tag.
      ;; We explicitly allow emoji/unicode, hyphen, slash, etc. Anything
      ;; except whitespace/control characters and another # triggers stop.
      (while (and (> (point) (point-min))
                  (supertag-completion--valid-tag-char-p
                   (char-before (point))))
        (backward-char))

      ;; Check if we're right after a # character
      ;; Note: after skip-chars-backward, point is at the start of the tag prefix
      (when (and (> (point) (point-min))
                 (eq (char-before (point)) ?#))
        ;; Found a #, so the tag starts right after it
        (setq start (point))
        (setq result (cons start end)))
      
      result)))

(defun supertag-completion--get-completion-table (prefix)
  "Return a completion table function that handles both existing tags and new tag creation."
  (let* ((safe-prefix (or prefix ""))
         (node-id (org-id-get))
         (current-tags (when node-id (supertag-completion--get-node-tags node-id)))
         (all-tags (supertag-completion--get-all-tags))
         (available-tags (if current-tags
                             (seq-remove (lambda (tag) (member tag current-tags)) all-tags)
                           all-tags))
         (matching-tags (all-completions safe-prefix available-tags))
         (new-tag-candidate (propertize "[Create New Tag]" 'is-new-tag t))
         (should-add-new (and (not (string-empty-p safe-prefix))
                             (not (member safe-prefix matching-tags))
                             (not (member safe-prefix current-tags)))))
    
    ;; Return all matching tags, plus [Create New Tag] if applicable
    (if should-add-new
        (cons new-tag-candidate matching-tags)
      matching-tags)))

(defun supertag-completion--post-completion-action (selected-string original-prefix)
  "The single, unified post-completion action.
It handles both existing and new tags correctly by inspecting the
completion candidate and correcting the buffer if necessary."
  (let* ((is-new (get-text-property 0 'is-new-tag selected-string))
         ;; For new tags, the REAL tag name is the prefix the user typed.
         ;; For existing tags, it's the candidate they selected.
         (clean-tag-string (if is-new original-prefix (substring-no-properties selected-string)))
         (tag-name clean-tag-string)
         (node-id (org-id-get-create)))

    (when (and tag-name (not (string-empty-p tag-name)) node-id)

      ;; --- CRITICAL FIX ---
      ;; If this is a new tag, the completion UI has inserted the placeholder
      ;; text "[Create New Tag]". We MUST delete that and insert the actual
      ;; tag name that the user typed (`original-prefix`).
      (when is-new
        (delete-region (- (point) (length selected-string)) (point))
        (insert original-prefix))

      ;; Ensure the node exists in the database
      (unless (supertag-node-get node-id)
        (when (fboundp 'supertag-node-sync-at-point)
          (supertag-node-sync-at-point)))

      ;; Add the tag to the node (creates tag if needed)
      (when (fboundp 'supertag-ops-add-tag-to-node)
        (let ((result (supertag-ops-add-tag-to-node node-id tag-name :create-if-needed t)))
          (when result
            (if is-new
                (message "New tag '%s' created and added to node %s" tag-name node-id)
              (message "Tag '%s' added to node %s" tag-name node-id)))))

      ;; Finally, add the trailing space to delimit the tag.
      (insert " "))))

;;;----------------------------------------------------------------------
;;; Main CAPF Entry Point
;;;----------------------------------------------------------------------

(defun supertag-completion-at-point ()
  "Main `completion-at-point` function using the classic, compatible API."
  (when-let ((bounds (supertag-completion--get-prefix-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (prefix (buffer-substring-no-properties start end)))
      
      (list start end
            ;; 1. The completion table. Returns a custom completion function
            ;;    that always includes [Create New Tag] in results
            (lambda (str pred action)
              (cond
               ;; Return metadata
               ((eq action 'metadata)
                '(metadata (category . supertag-tag)
                          (annotation-function . (lambda (cand)
                                                   (if (get-text-property 0 'is-new-tag cand)
                                                       " [new]"
                                                     " [tag]")))))
               ;; Return all candidates (for display)
               ((eq action t)
                (supertag-completion--get-completion-table prefix))
               ;; Test for exact match
               ((eq action 'lambda)
                (member str (supertag-completion--get-completion-table prefix)))
               ;; Try completion (return common prefix or t if unique)
               ((null action)
                (try-completion str (supertag-completion--get-completion-table prefix) pred))
               ;; Boundaries and other actions
               (t
                (complete-with-action action
                                     (supertag-completion--get-completion-table prefix)
                                     str pred))))

            ;; 2. A SINGLE, UNIFIED :exit-function. This is also
            ;;    universally understood by all completion frameworks.
            :exit-function
            (lambda (selected-string status)
              ;; The condition now accepts 'finished, 'exact', and 'sole' to be
              ;; compatible with various completion UIs like Corfu.
              (when (memq status '(finished exact sole))
                (supertag-completion--post-completion-action selected-string prefix)))))))

;;;----------------------------------------------------------------------
;;; Setup
;;;----------------------------------------------------------------------

;;;###autoload
(defun supertag-completion-setup ()
  "Setup completion for org-supertag."
  (add-hook 'completion-at-point-functions
            #'supertag-completion-at-point nil t))

;;;###autoload
(define-minor-mode supertag-ui-completion-mode
  "Enhanced tag completion for org-supertag."
  :lighter " ST-C"
  (if supertag-ui-completion-mode
      (supertag-completion-setup)
    (remove-hook 'completion-at-point-functions
                 #'supertag-completion-at-point t)))

;;;###autoload
(defun supertag-ui-completion-enable ()
  "Enable tag completion in org-mode buffers."
  (when (derived-mode-p 'org-mode)
    (supertag-ui-completion-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-supertag-ui-completion-mode
  supertag-ui-completion-mode
  supertag-ui-completion-enable)

(provide 'supertag-ui-completion)

;;; supertag-ui-completion.el ends here
