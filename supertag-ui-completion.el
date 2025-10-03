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
(require 'org-id nil t)
(require 'cl-lib)

;; Optional requires for new supertag architecture
(require 'supertag-ops-tag nil t)
(require 'supertag-ops-node nil t)
(require 'supertag-services-query nil t)

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
      (or (when (fboundp 'supertag-query)
            (let ((result (supertag-query '(:tags))))
              (mapcar #'car result)))
          ;; Fallback: scan nodes directly
          (let ((nodes-ht (supertag-get '(:nodes)))
                (all-tags '()))
            (when (hash-table-p nodes-ht)
              (maphash (lambda (_node-id node-data)
                         (when-let ((tags (plist-get node-data :tags)))
                           (setq all-tags (append tags all-tags))))
                       nodes-ht))
            (delete-dups all-tags)))
    (error
     (message "supertag-completion: Failed to get tags: %S" err)
     '())))

(defun supertag-completion--get-node-tags (node-id)
  "Get tags currently applied to NODE-ID."
  (when-let ((node-data (supertag-node-get node-id)))
    (plist-get node-data :tags)))

(defun supertag-completion--get-prefix-bounds ()
  "Find the bounds of a tag prefix at point, if any.
Returns (START . END) where START is right after the # character."
  (save-excursion
    (let ((end (point))
          (start nil))
      
      ;; Show context around point
      (message "DEBUG bounds: point=%d, char-at-point=%S, char-before-point=%S"
               (point)
               (char-after (point))
               (char-before (point)))
      (message "DEBUG bounds: text around point: '%s'"
               (buffer-substring-no-properties
                (max (point-min) (- (point) 10))
                (min (point-max) (+ (point) 10))))
      
      ;; Skip back over valid tag characters
      (skip-chars-backward "a-zA-Z0-9_-")
      (setq start (point))
      
      (message "DEBUG bounds: end=%d, after-skip=%d, char-before-start=%S"
               end start (char-before start))
      
      ;; Check if we're right after a # character
      (when (and (> start (point-min))
                 (eq (char-before start) ?#))
        (message "DEBUG: Found tag bounds: (%d . %d), prefix='%s'"
                 start end (buffer-substring-no-properties start end))
        (cons start end)))))

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
    (message "DEBUG table: prefix='%s', matching=%d, should-add-new=%s"
             safe-prefix (length matching-tags) should-add-new)
    
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
  (message "DEBUG CAPF: supertag-completion-at-point called at point=%d" (point))
  (message "DEBUG CAPF: About to call get-prefix-bounds")
  (let ((bounds (supertag-completion--get-prefix-bounds)))
    (message "DEBUG CAPF: bounds=%S" bounds)
    (if (not bounds)
        (progn
          (message "DEBUG CAPF: No bounds found, returning nil")
          nil)
      (let* ((start (car bounds))
             (end (cdr bounds))
             (prefix (buffer-substring-no-properties start end)))
        (message "DEBUG CAPF: prefix='%s', start=%d, end=%d" prefix start end)
        
        (list start end
              ;; 1. The completion table. Returns a custom completion function
              ;;    that always includes [Create New Tag] in results
              (lambda (str pred action)
                (message "DEBUG: completion table called with action=%S, str='%s'" action str)
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
                  (let ((table (supertag-completion--get-completion-table prefix)))
                    (message "DEBUG: action=t, returning %d candidates" (length table))
                    table))
                 ;; Test for exact match
                 ((eq action 'lambda)
                  (message "DEBUG: action=lambda, testing str='%s'" str)
                  (member str (supertag-completion--get-completion-table prefix)))
                 ;; Try completion (return common prefix or t if unique)
                 ((null action)
                  (message "DEBUG: action=nil (try-completion)")
                  (try-completion str (supertag-completion--get-completion-table prefix) pred))
                 ;; Boundaries
                 (t
                  (message "DEBUG: action=%S (other)" action)
                  (complete-with-action action
                                       (supertag-completion--get-completion-table prefix)
                                       str pred))))

              ;; 2. A SINGLE, UNIFIED :exit-function. This is also
              ;;    universally understood by all completion frameworks.
              :exit-function
              (lambda (selected-string status)
                (message "DEBUG: exit-function called with status=%S, selected='%s'" status selected-string)
                ;; The condition now accepts 'finished, 'exact', and 'sole' to be
                ;; compatible with various completion UIs like Corfu.
                (when (memq status '(finished exact sole))
                  (supertag-completion--post-completion-action selected-string prefix))))))))

;;;----------------------------------------------------------------------
;;; Setup
;;;----------------------------------------------------------------------

;;;###autoload
(defun supertag-completion-setup ()
  "Setup completion for org-supertag."
  (message "DEBUG: supertag-completion-setup called in buffer %s" (buffer-name))
  (message "DEBUG: completion-at-point-functions before: %S" completion-at-point-functions)
  (add-hook 'completion-at-point-functions
            #'supertag-completion-at-point nil t)
  (message "DEBUG: completion-at-point-functions after: %S" completion-at-point-functions))

;;;###autoload
(define-minor-mode supertag-ui-completion-mode
  "Enhanced tag completion for org-supertag."
  :lighter " ST-C"
  (message "DEBUG: supertag-ui-completion-mode toggled to %s in buffer %s"
           supertag-ui-completion-mode (buffer-name))
  (if supertag-ui-completion-mode
      (supertag-completion-setup)
    (progn
      (message "DEBUG: Removing completion hook")
      (remove-hook 'completion-at-point-functions
                   #'supertag-completion-at-point t))))

;;;###autoload
(defun supertag-ui-completion-enable ()
  "Enable tag completion in org-mode buffers."
  (when (derived-mode-p 'org-mode)
    (supertag-ui-completion-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-supertag-ui-completion-mode
  supertag-ui-completion-mode
  supertag-ui-completion-enable)

;; Auto-enable if customization variable is set
;;;###autoload
(when (and (boundp 'supertag-completion-auto-enable)
           supertag-completion-auto-enable)
  (add-hook 'org-mode-hook #'supertag-ui-completion-mode))

(provide 'supertag-ui-completion)

;;; supertag-ui-completion.el ends here
