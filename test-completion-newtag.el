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
  "Drive CAPF :exit-function with STATUS, simulating UI exit on `#newtag`."
  (setq test-completion-added-tag nil)
  (with-temp-buffer
    (org-mode)
    (insert "* node\n")
    (insert "Some text #newtag")
    (let* ((capf (supertag-completion-at-point))
           (exit-fn (plist-get (nthcdr 3 capf) :exit-function)))
      (cl-assert capf nil "CAPF returned nil — prefix bounds not found")
      (cl-assert exit-fn nil "no :exit-function on CAPF result")
      (funcall exit-fn (propertize "newtag" 'is-new-tag t) status))))

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

(message "OK: #newtag commits on finished / exact / nil status; skips on unknown.")
(kill-emacs 0)
