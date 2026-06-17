;;; supertag-ui-completion.el --- Universal and robust completion for org-supertag -*- lexical-binding: t; -*-

;; This file provides a completion-at-point function (CAPF) for org-supertag.
;; It uses the classic, most compatible CAPF design pattern to ensure it works
;; correctly across all completion UIs, including company-mode and corfu.
;;
;; The core principle is to return a list of PURE, PROPERTIZED STRINGS,
;; and use a SINGLE :exit-function that inspects the properties of the
;; selected string to decide on the action. This is the "lowest common
;; denominator" approach that all completion frameworks understand.
;;
;; ── Corfu Setup ──
;; For corfu, you need corfu-auto enabled. To trigger completion after #, use:
;;
;;   (setq corfu-auto t
;;         corfu-auto-delay 0.3
;;         corfu-auto-prefix 1)      ; trigger after 1 char
;;
;; Or manually trigger with M-TAB / C-M-i after typing #:
;;   (define-key org-mode-map (kbd "TAB") #'completion-at-point)
;;
;; ── Company Setup ──
;; Company should work out of the box if company-capf is in company-backends.
;; To ensure #tag completion takes priority over other backends:
;;
;;   (setq company-backends '((company-capf :with company-dabbrev-code)))
;;
;; Or add company-capf to your existing backends:
;;   (add-to-list 'company-backends 'company-capf)

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
Returns (START . END) where START is right after the # character.
Handles edge cases: cursor right after # (empty prefix), mid-word, etc."
  (save-excursion
    (let* ((end (point))
           (start nil))

      ;; Walk backwards over valid tag characters
      (while (and (> (point) (point-min))
                  (supertag-completion--valid-tag-char-p
                   (char-before (point))))
        (backward-char))

      ;; Check if we're right after a # character
      (when (and (> (point) (point-min))
                 (eq (char-before (point)) ?#))
        ;; start = right after # (where tag name begins or would begin)
        (setq start (point)))

      ;; Only return bounds if we found a # before the prefix
      (when start
        (cons start end)))))

(defun supertag-completion--get-completion-table (prefix)
  "Return the candidate list for PREFIX.
The list contains existing tags not yet on the current node. When PREFIX
is non-empty and matches no existing tag, PREFIX itself is prepended as a
propertized \"new tag\" candidate. Using the typed text as the candidate
(instead of a [Create New Tag] placeholder) keeps it visible under
corfu/orderless filtering and means the UI inserts the real tag name."
  (let* ((safe-prefix (or prefix ""))
         (node-id (org-id-get))
         (current-tags (when node-id (supertag-completion--get-node-tags node-id)))
         (all-tags (supertag-completion--get-all-tags))
         (available-tags (if current-tags
                             (seq-remove (lambda (tag) (member tag current-tags)) all-tags)
                           all-tags))
         (should-add-new (and (not (string-empty-p safe-prefix))
                             (not (member safe-prefix available-tags))
                             (not (member safe-prefix current-tags)))))
    (if should-add-new
        (cons (propertize safe-prefix 'is-new-tag t) available-tags)
      available-tags)))

(defun supertag-completion--post-completion-action (selected-string)
  "The single, unified post-completion action.
The completion UI has already inserted SELECTED-STRING into the buffer.
For both existing and new tags this is the real tag name (new candidates
are the typed text itself), so no buffer correction is needed."
  (let* ((is-new (get-text-property 0 'is-new-tag selected-string))
         (tag-name (substring-no-properties selected-string))
         (node-id (org-id-get-create)))

    (when (and tag-name (not (string-empty-p tag-name)) node-id)

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
            ;;    that always includes [Create New Tag] in results.
            ;;    Built to handle all completion actions for corfu/company compatibility.
            (lambda (str pred action)
              (cond
               ;; Handle boundaries (corfu/company compatibility)
               ((eq (car-safe action) 'boundaries) nil)
               ;; Return metadata (both corfu and company use this for display)
               ((eq action 'metadata)
                '(metadata
                  (category . supertag-tag)
                  (display-sort-function . identity)
                  (cycle-sort-function . identity)
                  (company-kind . (lambda (_cand) 'keyword))
                  (annotation-function
                   . (lambda (cand)
                       (if (get-text-property 0 'is-new-tag cand)
                           " [new]"
                         " [tag]")))))
               ;; Return all candidates (for display).
               ;; Use the LIVE input STR (not the captured PREFIX) so the
               ;; "new tag" candidate tracks what the user is typing under
               ;; corfu's incremental filtering.
               ((eq action t)
                (supertag-completion--get-completion-table str))
               ;; Test for exact match
               ((eq action 'lambda)
                (test-completion str (supertag-completion--get-completion-table str) pred))
               ;; Try completion (return common prefix or t if unique)
               ((null action)
                (try-completion str (supertag-completion--get-completion-table str) pred))
               ;; Boundaries and other actions (handles (boundaries . "") etc.)
               (t
                (complete-with-action action
                                     (supertag-completion--get-completion-table str)
                                     str pred))))

            ;; 2. Company-specific: explicit prefix length hint.
            ;;    Company uses this to know how much of the prefix to keep
            ;;    when the user types more characters. Corfu ignores this safely.
            :company-prefix-length (- end start)

            ;; 3. A SINGLE, UNIFIED :exit-function. This is also
            ;;    universally understood by all completion frameworks.
            :exit-function
            (lambda (selected-string status)
              ;; Accept any "successful" exit. Corfu uses 'finished;
              ;; default completion-at-point uses 'sole / 'exact; pressing
              ;; SPC after typing a brand-new #tag exits with no status
              ;; (status = nil) — that path must also count, otherwise
              ;; new tags never reach the database until the user types
              ;; C-M-i again. The only state we explicitly skip is
              ;; 'unknown (incremental keystroke filter, not a real exit).
              (unless (eq status 'unknown)
                (supertag-completion--post-completion-action selected-string)))))))

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

;;;###autoload
(defun supertag-complete-tag ()
  "Manually trigger #tag completion at point.
Bind this to TAB in org-mode if you want explicit trigger instead of auto-popup.

  (define-key org-mode-map (kbd \"TAB\") #'supertag-complete-tag)

Works with corfu, company, and default completion."
  (interactive)
  (if-let ((bounds (supertag-completion--get-prefix-bounds)))
      (completion-at-point)
    ;; No #tag prefix found, fall through to default completion or indent
    (if (and (eq last-command-event ?\t)
             (fboundp 'org-cycle))
        (call-interactively #'org-cycle)
      (call-interactively #'completion-at-point))))

;;; supertag-ui-completion.el ends here
