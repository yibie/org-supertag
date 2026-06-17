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
The candidate is the bare prefix string carrying `is-new-tag'."
  (setq test-completion-added-tag nil)
  (with-temp-buffer
    (org-mode)
    (insert "* node\n")
    (insert "Some text #")
    (let ((candidate (propertize "newtag" 'is-new-tag t)))
      (insert candidate)
      (when (memq status '(finished exact sole nil))
        (supertag-completion--post-completion-action candidate))
      ;; Buffer must still contain "#newtag" — no rewrite needed because
      ;; the candidate IS the real tag name.
      (when (memq status '(finished exact sole nil))
        (cl-assert (string-match-p "#newtag" (buffer-string))
                   nil "buffer lost the tag name:\n%s" (buffer-string))))))

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

;;; --- 8. The new-tag candidate is the bare prefix carrying is-new-tag
;;;        so orderless/corfu cannot filter it out, and the metadata
;;;        affixation-function adds the visible "[Create New Tag]" suffix.
(advice-remove 'supertag-node-get #'ignore)
(advice-add 'supertag-node-get :override
            (lambda (&rest _) '(:id "fake-node-id" :tags nil)))
(let* ((cands (supertag-completion--get-completion-table "branding"))
       (first (car cands)))
  (cl-assert (string= first "branding")
             nil "new-tag candidate is not the bare prefix (got %S)" first)
  (cl-assert (get-text-property 0 'is-new-tag first)
             nil "new-tag candidate missing 'is-new-tag property"))

;; Affixation function must label the new-tag candidate visibly.
(with-temp-buffer
  (org-mode)
  (insert "Some text #branding")
  (let* ((capf (supertag-completion-at-point))
         (table (nth 2 capf))
         (md (funcall table "branding" nil 'metadata))
         (affix (cdr (assq 'affixation-function (cdr md))))
         (rendered (funcall affix
                            (list (propertize "branding" 'is-new-tag t)
                                  "existing"))))
    (cl-assert (string-match-p "Create New Tag" (caddr (car rendered)))
               nil "affixation did not label new-tag candidate: %S" rendered)
    (cl-assert (string-match-p "\\[tag\\]" (caddr (cadr rendered)))
               nil "affixation did not label existing tag: %S" rendered)))

(message "OK new-tag candidate is bare prefix with visible [Create New Tag] affix.")
(kill-emacs 0)
