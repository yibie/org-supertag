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

(defconst supertag-completion--new-tag-suffix "  [Create New Tag]"
  "Visible suffix used to flag the \"create new tag\" candidate.")

(defun supertag-completion--get-completion-table (prefix)
  "Return the candidate list for PREFIX.
The list contains existing tags not yet on the current node. When
PREFIX is non-empty and matches no existing tag, the bare PREFIX
itself is prepended as the new-tag candidate, carrying two text
properties:
- `is-new-tag' (and `new-tag-name') so the exit function knows it;
- `display' holding \"PREFIX  [Create New Tag]\" so the popup and
  any corfu preview render the full label while the underlying
  candidate (what gets inserted into the buffer) stays just PREFIX.

This split is crucial: with `corfu-preview-current' set to `'insert'
corfu inserts the literal candidate string into the buffer as the
user keeps typing. If the candidate literally contained
\"  [Create New Tag]\", buffer contents like #org-supertag would
break the moment the user typed `-' (which is not in
`corfu-continue-commands' and so commits the preview).

orderless still sees the bare PREFIX in the candidate, so a leading
match like \"or\" against \"org-supertag\" continues to work."
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
        (let ((new-cand (propertize safe-prefix
                                    'is-new-tag t
                                    'new-tag-name safe-prefix)))
          ;; Insert new-tag at position 2 (index 1) so it never becomes
          ;; the default pick; if no existing tags, it stays alone.
          (if available-tags
              (cons (car available-tags) (cons new-cand (cdr available-tags)))
            (list new-cand)))
      available-tags)))

(defun supertag-completion--post-completion-action (selected-string)
  "Post-completion action invoked after the UI inserts SELECTED-STRING.
SELECTED-STRING is the bare tag name in all cases (the visible
\"[Create New Tag]\" label lives on the candidate's `display'
property, not in the candidate string itself, so the buffer only
ever contains the real tag name)."
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
                  (company-kind . (lambda (cand)
                                    (if (get-text-property 0 'is-new-tag cand)
                                        'snippet
                                      'keyword)))
                  (annotation-function
                   . (lambda (cand)
                       (if (get-text-property 0 'is-new-tag cand)
                           (propertize "  [+NEW]" 'face 'warning)
                         "  [tag]")))))
               ;; Return all candidates (for display).
               ;; Two gotchas to handle here:
               ;;
               ;; 1. orderless enumerates by calling TABLE with STR=""
               ;;    and filters with its own regexp. Gating the
               ;;    new-tag candidate on STR being non-empty makes it
               ;;    invisible to orderless.
               ;; 2. corfu caches CAPF data for the duration of the
               ;;    popup session and re-calls only the TABLE function
               ;;    on subsequent keystrokes — not the outer
               ;;    `supertag-completion-at-point'. A closure over
               ;;    `prefix' therefore freezes at popup-open time, so
               ;;    "#zz" expanded to "#zzzfr" still shows the
               ;;    "zz  [Create New Tag]" candidate instead of
               ;;    "zzzfr  [Create New Tag]".
               ;;
               ;; Solution: re-read the prefix from the live buffer on
               ;; every TABLE call. `get-prefix-bounds' walks backward
               ;; from point to the leading `#', so the value is always
               ;; current. Fall back to the captured PREFIX (mainly for
               ;; non-interactive callers and tests).
               ((eq action t)
                (let* ((live-bounds (and (not (minibufferp))
                                         (supertag-completion--get-prefix-bounds)))
                       (live-prefix (if live-bounds
                                        (buffer-substring-no-properties
                                         (car live-bounds) (cdr live-bounds))
                                      prefix)))
                  (supertag-completion--get-completion-table live-prefix)))
               ;; Test for exact match. NEVER report the user input as an
               ;; exact match against the "[Create New Tag]" candidate
               ;; — that would convince the UI that completion is done
               ;; and it would auto-commit the labeled candidate.
               ((eq action 'lambda)
                (let ((cands (supertag-completion--get-completion-table str)))
                  (and (test-completion str cands pred)
                       ;; not satisfied if the only match is the new-tag entry
                       (not (and (= (length cands) 1)
                                 (get-text-property
                                  0 'is-new-tag (car cands)))))))
               ;; Try completion (return common prefix or t if unique).
               ;; CRITICAL: if the only matching candidate is our
               ;; new-tag entry, returning t (or the bare prefix as a
               ;; "complete" match) lets corfu commit it silently
               ;; without showing the popup. Force the popup by
               ;; pretending the completion has not finished.
               ((null action)
                (let* ((cands (supertag-completion--get-completion-table str))
                       (sole-new-tag
                        (and (= (length cands) 1)
                             (get-text-property
                              0 'is-new-tag (car cands)))))
                  (if sole-new-tag
                      ;; Tell the UI "STR is what we have so far",
                      ;; equivalent to no further progress — popup
                      ;; appears.
                      str
                    (try-completion str cands pred))))
               ;; Boundaries and other actions (handles (boundaries . "") etc.)
               (t
                (complete-with-action action
                                     (supertag-completion--get-completion-table str)
                                     str pred))))

            ;; 2. Company-specific: explicit prefix length hint.
            ;;    Company uses this to know how much of the prefix to keep
            ;;    when the user types more characters. Corfu ignores this safely.
            :company-prefix-length (- end start)

            ;; 3. EXCLUSIVE: tell completion-at-point that once we are
            ;;    inside a #tag context, no other CAPF should run.
            ;;    Without this, cape-dabbrev / cape-keyword / pcomplete
            ;;    get appended to our candidate list, their candidates
            ;;    can override our annotation/metadata, and the
            ;;    "[Create New Tag]" entry gets hidden or stripped of
            ;;    its label by the cape merging layer.
            :exclusive 'yes

            ;; 4. A SINGLE, UNIFIED :exit-function. This is also
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
;;; Auto-record on tag boundary
;;;----------------------------------------------------------------------

(defun supertag-completion--auto-record-on-boundary ()
  "Record the `#prefix' right behind point if the user just ended it.
Triggered from `post-self-insert-hook'. The corfu popup may have shown
a `[new]' candidate, but if the user ignored it and typed SPC/RET/etc.
to continue writing, the CAPF `:exit-function' is never called and the
tag would be lost until the user re-triggered completion with C-M-i.
This hook closes that gap: when the just-inserted character ends a
`#tag' context, sync the node and add the tag (idempotent for existing
tags, creates the tag entity if new)."
  (when (and (derived-mode-p 'org-mode)
             (not (supertag-completion--valid-tag-char-p (char-before)))
             (> (point) 2)
             ;; The char just before the separator must be a valid tag
             ;; char — otherwise we are not on a tag boundary.
             (supertag-completion--valid-tag-char-p (char-before (1- (point)))))
    (save-excursion
      (backward-char) ; step over the separator we just typed
      (when-let* ((bounds (supertag-completion--get-prefix-bounds))
                  (prefix (buffer-substring-no-properties
                           (car bounds) (cdr bounds)))
                  (_ (not (string-empty-p prefix))))
        (condition-case err
            (let ((node-id (org-id-get-create)))
              (when node-id
                (unless (supertag-node-get node-id)
                  (when (fboundp 'supertag-node-sync-at-point)
                    (supertag-node-sync-at-point)))
                (let ((node-tags (supertag-completion--get-node-tags node-id)))
                  (unless (member prefix node-tags)
                    (when (fboundp 'supertag-ops-add-tag-to-node)
                      (supertag-ops-add-tag-to-node
                       node-id prefix :create-if-needed t))))))
          (error
           (message "supertag-completion: auto-record failed: %S" err)))))))

;;;----------------------------------------------------------------------
;;; Setup
;;;----------------------------------------------------------------------

;;;###autoload
(defun supertag-completion-setup ()
  "Setup completion for org-supertag."
  (add-hook 'completion-at-point-functions
            #'supertag-completion-at-point nil t)
  (add-hook 'post-self-insert-hook
            #'supertag-completion--auto-record-on-boundary nil t))

;;;###autoload
(define-minor-mode supertag-ui-completion-mode
  "Enhanced tag completion for org-supertag."
  :lighter " ST-C"
  (if supertag-ui-completion-mode
      (supertag-completion-setup)
    (remove-hook 'completion-at-point-functions
                 #'supertag-completion-at-point t)
    (remove-hook 'post-self-insert-hook
                 #'supertag-completion--auto-record-on-boundary t)))

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

;;;----------------------------------------------------------------------
;;; Fallback: bypass corfu entirely
;;;----------------------------------------------------------------------

;;;###autoload
(defun supertag-tag-insert ()
  "Pick or create a #tag via minibuffer, bypassing corfu/company.
Use when the popup completion does not show the \"[Create New Tag]\"
option in your UI stack. Prompts for a tag name with completion
against existing tags. Typing a brand-new name and confirming with
RET creates and records the tag immediately."
  (interactive)
  (let* ((all (supertag-completion--get-all-tags))
         (node-id (org-id-get-create))
         (current (when node-id (supertag-completion--get-node-tags node-id)))
         (available (if current
                        (seq-remove (lambda (t) (member t current)) all)
                      all))
         (input (completing-read "Tag (RET on a typed name creates it): "
                                 available nil nil)))
    (when (and input (not (string-empty-p input)))
      (let ((is-new (not (member input all))))
        (unless (supertag-node-get node-id)
          (when (fboundp 'supertag-node-sync-at-point)
            (supertag-node-sync-at-point)))
        (when (looking-back "[^#]" 1)
          (insert "#"))
        (insert input " ")
        (when (fboundp 'supertag-ops-add-tag-to-node)
          (supertag-ops-add-tag-to-node node-id input :create-if-needed t))
        (message "%s tag '%s' added to node %s"
                 (if is-new "New" "Existing") input node-id)))))

;;;###autoload
(defun supertag-completion-debug ()
  "Dump the full completion pipeline for the `#prefix' at point.
Run this with the cursor sitting just after a `#typedprefix' (do not
delete or move anything) and paste the contents of *Messages* back to
the maintainer. The output shows: the detected bounds, the live prefix,
the full candidate list our CAPF returns, and what
`completion-all-completions' under your active `completion-styles' keeps
after filtering — i.e. exactly what the popup should display."
  (interactive)
  (let ((bounds (supertag-completion--get-prefix-bounds)))
    (if (not bounds)
        (message "[supertag-debug] No #prefix bounds at point (no leading #?).")
      (let* ((start (car bounds))
             (end (cdr bounds))
             (prefix (buffer-substring-no-properties start end))
             (table (supertag-completion--get-completion-table prefix))
             (first (car table))
             (styles completion-styles)
             (md (let ((capf (supertag-completion-at-point)))
                   (when capf
                     (funcall (nth 2 capf) prefix nil 'metadata))))
             (filtered (completion-all-completions
                        prefix table nil (length prefix) md)))
        ;; completion-all-completions returns a partial list with the
        ;; final cdr possibly set to a base-size integer; normalize.
        (let ((flat (let (acc (c filtered))
                      (while (consp c)
                        (push (car c) acc)
                        (setq c (cdr c)))
                      (nreverse acc))))
          (message "[supertag-debug] ─────────────────────────────")
          (message "[supertag-debug] prefix bounds : %S..%S" start end)
          (message "[supertag-debug] live prefix   : %S" prefix)
          (message "[supertag-debug] completion-styles: %S" styles)
          (message "[supertag-debug] raw candidates  : %d total" (length table))
          (message "[supertag-debug]   [0] %S  is-new-tag=%S name=%S"
                   (substring-no-properties (or first ""))
                   (and first (get-text-property 0 'is-new-tag first))
                   (and first (get-text-property 0 'new-tag-name first)))
          (message "[supertag-debug]   first 5 raw  : %S"
                   (mapcar #'substring-no-properties (cl-subseq table 0 (min 5 (length table)))))
          (message "[supertag-debug] after-filter   : %d candidates"
                   (length flat))
          (message "[supertag-debug]   first 5 kept : %S"
                   (mapcar #'substring-no-properties (cl-subseq flat 0 (min 5 (length flat)))))
          (message "[supertag-debug] new-tag in raw? : %s"
                   (if (cl-some (lambda (c)
                                  (get-text-property 0 'is-new-tag c))
                                table)
                       "YES" "no"))
          (message "[supertag-debug] new-tag in kept?: %s"
                   (if (cl-some (lambda (c)
                                  (get-text-property 0 'is-new-tag c))
                                flat)
                       "YES" "NO  ← filter dropped it"))
          (message "[supertag-debug] candidate width : %d chars (corfu-max-width may truncate)"
                   (length (substring-no-properties (or first ""))))
          (message "[supertag-debug] corfu installed?: %s%s"
                   (if (featurep 'corfu) "yes" "no")
                   (if (boundp 'corfu-max-width)
                       (format ", corfu-max-width=%S" corfu-max-width)
                     ""))
          (message "[supertag-debug] cape installed? : %s" (if (featurep 'cape) "yes" "no"))
          (message "[supertag-debug] marginalia?     : %s" (if (and (boundp 'marginalia-mode) marginalia-mode) "ON" "off"))
          (message "[supertag-debug] capf-functions  : %S"
                   (mapcar (lambda (f) (if (symbolp f) f 'lambda))
                           completion-at-point-functions))
          (message "[supertag-debug] ─────────────────────────────"))))))

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
