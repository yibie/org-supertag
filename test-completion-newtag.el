;;; test-completion-newtag.el --- self-check for #newtag exit path -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q --eval '(package-initialize)' -L . -l test-completion-newtag.el

(require 'cl-lib)
(setq supertag-data-directory (make-temp-file "supertag-test-" t))
(require 'org)
(require 'supertag-ui-completion)
(require 'supertag-core-store)
(require 'supertag-ops-tag)
(require 'supertag-ops-node)

;; ponytail: simulate a buffer with an org node + a typed "#newtag" and
;; drive the CAPF :exit-function directly, the same way completion-at-point
;; does on a successful exit.

(defvar test-completion-added-tag nil)
(defvar test-completion-insert-count 0)

;; Stub out the heavy ops; we only care that they were called with the
;; right tag name. Also count post-insert space insertions so we can
;; detect the "double-space" failure mode.
(advice-add 'supertag-ops-add-tag-to-node :override
            (lambda (_node-id tag &rest _) (setq test-completion-added-tag tag) t))
(advice-add 'org-id-get-create :override (lambda (&rest _) "fake-node-id"))
(advice-add 'org-id-get        :override (lambda (&rest _) "fake-node-id"))
(advice-add 'supertag-node-get :override (lambda (&rest _) '(:id "fake-node-id")))

(defun test-completion--drive-exit (status)
  "Simulate the UI committing the new-tag candidate at point.
The candidate is \"NAME  [Create New Tag]\" carrying `is-new-tag' and
`new-tag-name'. After commit, the buffer must contain only \"NAME\"."
  (setq test-completion-added-tag nil)
  (with-temp-buffer
    (org-mode)
    (insert "* node\n")
    (insert "Some text #")
    (let ((candidate (propertize (concat "newtag"
                                         supertag-completion--new-tag-suffix)
                                 'is-new-tag t
                                 'new-tag-name "newtag")))
      (insert candidate)
      (when (memq status '(finished exact sole nil))
        (supertag-completion--post-completion-action candidate))
      (when (memq status '(finished exact sole nil))
        (cl-assert (string-match-p "#newtag" (buffer-string))
                   nil "buffer missing #newtag: %s" (buffer-string))
        (cl-assert (not (string-match-p "Create New Tag" (buffer-string)))
                   nil "suffix not stripped from buffer: %s"
                   (buffer-string))))))

;;; --- 1. Corfu-style finish ---
(test-completion--drive-exit 'finished)
(cl-assert (equal test-completion-added-tag "newtag")
           nil "finished: tag not added (got %S)" test-completion-added-tag)

;;; --- 2. Default completion exact match ---
(test-completion--drive-exit 'exact)
(cl-assert (equal test-completion-added-tag "newtag")
           nil "exact: tag not added (got %S)" test-completion-added-tag)

;;; --- 3. THE BUG: user types `#newtag` and presses SPC. Many UIs
;;;        report status = nil. With the old code (memq on
;;;        '(finished exact sole)) this was silently dropped. ---
(test-completion--drive-exit nil)
(cl-assert (equal test-completion-added-tag "newtag")
           nil "nil status: tag not added — regression returned (got %S)"
           test-completion-added-tag)

;;; --- 4. Incremental filter ticks (status 'unknown) must NOT
;;;        commit, otherwise every keystroke would re-add the tag. ---
(test-completion--drive-exit 'unknown)
(cl-assert (null test-completion-added-tag)
           nil "unknown status incorrectly committed tag (got %S)"
           test-completion-added-tag)

(message "OK exit-function path: commits on finished/exact/nil; skips on unknown.")

;;; --- 5. THE REAL USER SCENARIO ---
;;; corfu shows a [new] candidate, user IGNORES it and types SPC to
;;; keep writing. :exit-function is never called. Our post-self-insert
;;; hook must catch this and record the tag anyway.
(setq test-completion-added-tag nil)
(with-temp-buffer
  (org-mode)
  (supertag-ui-completion-mode 1)
  (insert "Some text #ignoredtag")
  ;; Simulate SPC self-insert (last-command-event = ?\s, point advances)
  (let ((last-command-event ?\s))
    (insert " ")
    (run-hooks 'post-self-insert-hook)))
(cl-assert (equal test-completion-added-tag "ignoredtag")
           nil "post-self-insert hook did not record ignored #newtag (got %S)"
           test-completion-added-tag)

;;; --- 6. Hook must NOT fire on plain prose (no preceding #tag) ---
(setq test-completion-added-tag nil)
(with-temp-buffer
  (org-mode)
  (supertag-ui-completion-mode 1)
  (insert "ordinary sentence")
  (let ((last-command-event ?\s))
    (insert " ")
    (run-hooks 'post-self-insert-hook)))
(cl-assert (null test-completion-added-tag)
           nil "hook fired on non-tag text (got %S)" test-completion-added-tag)

;;; --- 7. Hook must NOT re-add an existing tag on the same node ---
(setq test-completion-added-tag nil)
(advice-remove 'supertag-node-get #'ignore)
(advice-add 'supertag-node-get :override
            (lambda (&rest _) '(:id "fake-node-id" :tags ("already"))))
(with-temp-buffer
  (org-mode)
  (supertag-ui-completion-mode 1)
  (insert "Recap #already")
  (let ((last-command-event ?\s))
    (insert " ")
    (run-hooks 'post-self-insert-hook)))
(cl-assert (null test-completion-added-tag)
           nil "hook re-added existing tag (got %S)" test-completion-added-tag)

(message "OK auto-record-on-boundary: catches ignored corfu candidate, skips prose and duplicates.")

;;; --- 8. The new-tag candidate string starts with the user's prefix
;;;        followed by "  [Create New Tag]" — this shape lets orderless
;;;        and basic styles match the user's input on the leading
;;;        prefix while keeping the visible label in the candidate.
(advice-remove 'supertag-node-get #'ignore)
(advice-add 'supertag-node-get :override
            (lambda (&rest _) '(:id "fake-node-id" :tags nil)))
(let* ((cands (supertag-completion--get-completion-table "branding"))
       (first (car cands))
       (literal (substring-no-properties first)))
  (cl-assert (string-prefix-p "branding" literal)
             nil "candidate does not start with prefix: %S" literal)
  (cl-assert (string-match-p "\\[Create New Tag\\]" literal)
             nil "candidate missing visible label: %S" literal)
  (cl-assert (get-text-property 0 'is-new-tag first)
             nil "new-tag candidate missing 'is-new-tag property")
  (cl-assert (equal (get-text-property 0 'new-tag-name first) "branding")
             nil "new-tag candidate missing 'new-tag-name property"))

;;; --- 9. Crucially: orderless-style filtering on user input "branding"
;;;        must KEEP the new-tag candidate in the list. ---
(let* ((cands (supertag-completion--get-completion-table "branding"))
       ;; basic/substring matchers
       (filtered (let ((completion-styles '(basic substring)))
                   (all-completions "branding" cands))))
  (cl-assert (cl-some (lambda (c) (string-prefix-p "branding" c)) filtered)
             nil "new-tag candidate filtered out by basic/substring: %S"
             filtered))

(message "OK new-tag candidate \"PREFIX  [Create New Tag]\" survives filtering.")

;;; --- 10. CRITICAL: when the only surviving candidate is the new-tag
;;;        entry, the CAPF table must NOT report the user input as
;;;        completed — otherwise corfu / completion-at-point silently
;;;        commits the labeled candidate into the buffer without ever
;;;        showing the popup. This is the bug the user kept reporting.
(advice-remove 'supertag-completion--get-all-tags #'ignore)
(advice-add 'supertag-completion--get-all-tags :override
            (lambda () (list "idea" "tips")))
(with-temp-buffer
  (org-mode)
  (insert "Some text #goodgoodgood")
  (let* ((capf (supertag-completion-at-point))
         (table (nth 2 capf))
         (try-result (funcall table "goodgoodgood" nil nil))
         (lambda-result (funcall table "goodgoodgood" nil 'lambda)))
    ;; try-completion must NOT return the labeled candidate; otherwise
    ;; the UI would auto-insert "  [Create New Tag]" into the buffer.
    (cl-assert (or (eq try-result t)
                   (and (stringp try-result)
                        (not (string-match-p "Create New Tag" try-result))))
               nil "try-completion leaked labeled candidate: %S" try-result)
    ;; test-completion must NOT report 'finished' on a sole new-tag,
    ;; otherwise completion-at-point treats it as already complete.
    (cl-assert (not lambda-result)
               nil "test-completion incorrectly marked sole new-tag as exact: %S"
               lambda-result)))

(message "OK sole new-tag does not trigger silent auto-commit.")

;;; --- 11. THE BUG ORDERLESS WAS HIDING ---
;;; orderless enumerates the table with action=t and STR="" (uses its
;;; own regexp for filtering, not a prefix). If the CAPF table gates
;;; the new-tag candidate on STR being non-empty, the candidate is
;;; absent from the enumerated set and orderless can never reveal it
;;; — no matter what the user types. The CAPF must enumerate based on
;;; the captured PREFIX (live user input that triggered the CAPF),
;;; not on the STR orderless passes for filtering.
(setq test-automation-added-tag nil)
(with-temp-buffer
  (org-mode)
  (insert "Foo #thinkinging")
  (let* ((capf (supertag-completion-at-point))
         (table (nth 2 capf))
         ;; This is exactly how orderless enumerates: action=t, STR="".
         (enumerated (funcall table "" nil t)))
    (cl-assert (cl-some (lambda (c) (get-text-property 0 'is-new-tag c))
                        enumerated)
               nil "new-tag candidate missing when orderless enumerates with STR=\"\":\n  %S"
               (mapcar #'substring-no-properties enumerated))))

(message "OK CAPF includes new-tag candidate when orderless enumerates with STR=\"\".")
(kill-emacs 0)
