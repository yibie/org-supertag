;;; transaction-test.el --- ERT tests for real transaction rollback -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for the S1 "transaction地基" hardening work
;; (phase-git-sync-20260713 plan, section "S1 事务地基修复"):
;;
;;   1. `supertag-with-transaction' (supertag-core-transform.el) used to be a
;;      *fake* transaction: its docstring promised rollback on error, but the
;;      unwind-protect cleanup only reset `supertag--transaction-active' and
;;      `supertag--transaction-log' — no restoration code at all.
;;   2. `supertag--with-transaction' (supertag-core-persistence.el) was a
;;      *real* transaction, but paid for correctness with a full-store deep
;;      copy on every use.
;;
;; This file exercises the fix: `supertag-with-transaction' now records the
;; pre-write value of every store path the first time it is touched (at the
;; low-level store seam in supertag-core-store.el — put-entity, remove-entity,
;; put/remove-field-value, and the whole-collection replace/clear paths), and
;; restores all of them in reverse order on error. `supertag--with-transaction'
;; is now an obsolete alias for the same macro.
;;
;; Every test runs inside an isolated temp directory/store; none of them ever
;; touch the user's real `~/.emacs.d'. Follows the
;; `supertag-hardening-test--with-temp-env' fixture pattern from
;; test/persistence-hardening-test.el.
;;
;; Run:
;;   ./test/run-tests.sh tx
;;   emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/transaction-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ht)
(require 'cl-lib)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-core-transform)
(require 'supertag-core-persistence) ; For the obsolete `supertag--with-transaction' alias
(require 'supertag-ops-node)
(require 'supertag-automation)

;;; --- Shared fixture ---

(defmacro tx-test--with-temp-env (&rest body)
  "Run BODY with a clean, isolated store and subscriber/rule registry.
Rebinds `supertag-data-directory'/`supertag-db-file'/`supertag--store' (as in
`supertag-hardening-test--with-temp-env') plus `supertag--subscribers' and
`supertag--rule-index' to fresh tables, so any `supertag-subscribe' or
automation rule registered by a test (or by requiring supertag-automation.el)
is automatically undone when BODY finishes — no manual unsubscribe
bookkeeping, and no leakage between tests or into the rest of the test suite."
  (declare (indent 0))
  `(let* ((tmp (file-name-as-directory (make-temp-file "supertag-tx-test" t)))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag--store nil)
          (supertag--store-origin nil)
          (supertag--transaction-active nil)
          (supertag--transaction-log nil)
          (supertag--transaction-seen nil)
          (supertag--subscribers (ht-create))
          (supertag--rule-index (make-hash-table :test 'equal))
          (supertag-db--auto-save-timer nil))
     (unwind-protect
         (progn
           (supertag--ensure-store)
           ,@body)
       (when (timerp supertag-db--auto-save-timer)
         (cancel-timer supertag-db--auto-save-timer))
       (ignore-errors (delete-directory tmp t)))))

(defun tx-test--node (id title)
  "Return a minimal node plist for ID/TITLE."
  (list :id id :type :node :title title :file "/tmp/f.org"))

;;; --- 1. Basic rollback: error mid-body restores every touched entity ---

(ert-deftest tx-test-rollback-restores-update-and-create ()
  "An update and a creation inside a failed transaction are both undone."
  (tx-test--with-temp-env
    (let ((a0 (tx-test--node "A" "Original A")))
      (supertag-store-put-entity :nodes "A" a0)
      (let ((count-before (hash-table-count (supertag-store-get-collection :nodes))))
        (should-error
         (supertag-with-transaction
           (supertag-store-put-entity :nodes "A" (tx-test--node "A" "Modified A"))
           (supertag-store-put-entity :nodes "B" (tx-test--node "B" "New B"))
           (error "boom")))
        ;; A is restored byte-for-byte (well, `equal') to its pre-transaction value.
        (should (equal a0 (supertag-store-get-entity :nodes "A")))
        ;; B was created inside the failed transaction and must be gone again.
        (should (null (supertag-store-get-entity :nodes "B")))
        ;; Entity count is back to what it was before the transaction.
        (should (= count-before (hash-table-count (supertag-store-get-collection :nodes))))))))

(ert-deftest tx-test-rollback-restores-deletion ()
  "A deletion inside a failed transaction resurrects the entity, unchanged."
  (tx-test--with-temp-env
    (let ((a0 (tx-test--node "A" "Original A")))
      (supertag-store-put-entity :nodes "A" a0)
      (should-error
       (supertag-with-transaction
         (supertag-store-remove-entity :nodes "A")
         (should (null (supertag-store-get-entity :nodes "A"))) ; gone mid-transaction
         (error "boom")))
      (should (equal a0 (supertag-store-get-entity :nodes "A"))))))

(ert-deftest tx-test-rollback-lands-on-original-not-intermediate ()
  "Multiple writes to the same entity in one transaction roll back to the
*first* pre-transaction value, not an intermediate one."
  (tx-test--with-temp-env
    (let ((a0 (tx-test--node "A" "V0")))
      (supertag-store-put-entity :nodes "A" a0)
      (should-error
       (supertag-with-transaction
         (supertag-store-put-entity :nodes "A" (tx-test--node "A" "V1"))
         (supertag-store-put-entity :nodes "A" (tx-test--node "A" "V2"))
         (supertag-store-put-entity :nodes "A" (tx-test--node "A" "V3"))
         (error "boom")))
      (should (equal a0 (supertag-store-get-entity :nodes "A")))
      (should (equal "V0" (plist-get (supertag-store-get-entity :nodes "A") :title))))))

(ert-deftest tx-test-rollback-pure-creation-leaves-no-trace ()
  "A transaction that only creates entities leaves none behind on error."
  (tx-test--with-temp-env
    (let ((count-before (hash-table-count (supertag-store-get-collection :nodes))))
      (should-error
       (supertag-with-transaction
         (supertag-store-put-entity :nodes "NEW1" (tx-test--node "NEW1" "x"))
         (supertag-store-put-entity :nodes "NEW2" (tx-test--node "NEW2" "y"))
         (error "boom")))
      (should (null (supertag-store-get-entity :nodes "NEW1")))
      (should (null (supertag-store-get-entity :nodes "NEW2")))
      (should (= count-before (hash-table-count (supertag-store-get-collection :nodes)))))))

;;; --- 2. Field-value seam: creation/removal of the per-node bucket ---

(ert-deftest tx-test-rollback-field-value-creation-removes-empty-bucket ()
  "Rolling back the first field ever written for a node removes the whole
per-node bucket again, instead of leaving an empty shell that would inflate
the :field-values entity count."
  (tx-test--with-temp-env
    (let ((count-before (hash-table-count (supertag-store-get-collection :field-values))))
      (should-error
       (supertag-with-transaction
         (supertag-store-put-field-value "node-x" "field-a" "hello")
         (error "boom")))
      (should (= count-before (hash-table-count (supertag-store-get-collection :field-values))))
      (should (null (supertag-store-get-field-value "node-x" "field-a"))))))

(ert-deftest tx-test-rollback-field-value-update-restores-old-value ()
  "Rolling back an update to an existing field value restores the old value,
and does not remove the node's other, untouched field values."
  (tx-test--with-temp-env
    (supertag-store-put-field-value "node-x" "field-a" "original")
    (supertag-store-put-field-value "node-x" "field-untouched" "keep-me")
    (should-error
     (supertag-with-transaction
       (supertag-store-put-field-value "node-x" "field-a" "changed")
       (error "boom")))
    (should (equal "original" (supertag-store-get-field-value "node-x" "field-a")))
    (should (equal "keep-me" (supertag-store-get-field-value "node-x" "field-untouched")))))

(ert-deftest tx-test-rollback-field-value-removal-restores-value ()
  "Rolling back a field-value removal restores the removed value."
  (tx-test--with-temp-env
    (supertag-store-put-field-value "node-x" "field-a" "original")
    (should-error
     (supertag-with-transaction
       (supertag-store-remove-field-value "node-x" "field-a")
       (error "boom")))
    (should (equal "original" (supertag-store-get-field-value "node-x" "field-a")))))

;;; --- 3. Nesting: inner transaction joins the outer, no early commit ---

(ert-deftest tx-test-nested-transaction-outer-rollback-undoes-inner ()
  "An inner `supertag-with-transaction' that completes successfully still has
its changes undone when the *outer* transaction later fails — proving both
runs share one rollback log rather than the inner committing independently."
  (tx-test--with-temp-env
    (let ((a0 (tx-test--node "A" "Original A")))
      (supertag-store-put-entity :nodes "A" a0)
      (should-error
       (supertag-with-transaction
         (supertag-with-transaction
           (supertag-store-put-entity :nodes "A" (tx-test--node "A" "Changed by inner")))
         ;; Inner transaction "committed" (no error), but we're still inside
         ;; the outer one, and the outer body goes on to fail.
         (should (equal "Changed by inner"
                        (plist-get (supertag-store-get-entity :nodes "A") :title)))
         (error "outer boom")))
      (should (equal a0 (supertag-store-get-entity :nodes "A"))))))

(ert-deftest tx-test-nested-transaction-does-not-flush-early ()
  "Nesting does not trigger a batch-notification flush at the inner call —
only the outermost transaction commits and flushes, exactly once."
  (tx-test--with-temp-env
    (supertag-store-put-entity :nodes "A" (tx-test--node "A" "V0"))
    (let ((flush-count 0))
      (cl-letf* ((orig (symbol-function 'supertag--notify-batch-changes))
                 ((symbol-function 'supertag--notify-batch-changes)
                  (lambda () (cl-incf flush-count) (funcall orig))))
        (supertag-with-transaction
          (supertag-with-transaction
            (supertag-store-put-entity :nodes "A" (tx-test--node "A" "V1")))
          (supertag-store-put-entity :nodes "A" (tx-test--node "A" "V2"))))
      (should (= 1 flush-count)))))

;;; --- 4. Success path: single flush, log cleared ---

(ert-deftest tx-test-success-path-clears-transaction-state ()
  "After a successful transaction, the active flag, log, and seen-set are all
reset to nil so the next transaction starts clean."
  (tx-test--with-temp-env
    (supertag-with-transaction
      (supertag-store-put-entity :nodes "A" (tx-test--node "A" "V1")))
    (should (null supertag--transaction-active))
    (should (null supertag--transaction-log))
    (should (null supertag--transaction-seen))))

(ert-deftest tx-test-failure-path-clears-transaction-state ()
  "After a rolled-back transaction, transaction state is still fully reset."
  (tx-test--with-temp-env
    (should-error
     (supertag-with-transaction
       (supertag-store-put-entity :nodes "A" (tx-test--node "A" "V1"))
       (error "boom")))
    (should (null supertag--transaction-active))
    (should (null supertag--transaction-log))
    (should (null supertag--transaction-seen))))

;;; --- 5. Automation joins the enclosing transaction ---

(ert-deftest tx-test-automation-side-effect-joins-enclosing-transaction ()
  "A mutation made synchronously by an automation rule — triggered by a write
inside `supertag-with-transaction' — is captured by the *same* rollback log
and undone when the transaction fails. This is the key correctness property
the S1 plan calls out under \"automation 产生的连锁修改并入同一事务\": it works
because the transaction seam is a dynamically-scoped flag checked by the
low-level store primitives themselves, not something threaded explicitly
through call sites, so any synchronous reaction (automation included) that
writes through those primitives is automatically covered."
  (tx-test--with-temp-env
    ;; Wire up the live automation dispatcher exactly like
    ;; `supertag-automation-init' does, without its unrelated scheduler/cache
    ;; side effects.
    (supertag-subscribe :store-changed #'supertag-automation--handle-entity-change)
    (let* ((node (supertag-node-create (list :title "Trigger node" :file "/tmp/f.org")))
           (node-id (plist-get node :id)))
      (supertag-automation-create
       (list :name "rollback-test-rule"
             :trigger (list :on-tag-added "rb-tag")
             :actions (list (list :action :update-property
                                  :params (list :property :auto-marker :value "set")))))
      (let ((before (supertag-node-get node-id)))
        (should-error
         (supertag-with-transaction
           (supertag-node-add-tag node-id "rb-tag")
           ;; The automation ran synchronously inside the write above; verify
           ;; it actually fired before we blow up the transaction, otherwise
           ;; this test would pass vacuously.
           (should (equal "set" (plist-get (plist-get (supertag-node-get node-id) :properties)
                                           :auto-marker)))
           (error "boom")))
        ;; Both supertag-node-add-tag's :tags change AND the automation's
        ;; :properties change must be undone together.
        (should (equal before (supertag-node-get node-id)))
        (should (null (member "rb-tag" (plist-get (supertag-node-get node-id) :tags))))
        (should (null (plist-get (plist-get (supertag-node-get node-id) :properties)
                                 :auto-marker)))))))

;;; --- 6. Obsolete alias: old macro name still works ---

(ert-deftest tx-test-obsolete-alias-still-rolls-back ()
  "`supertag--with-transaction' (now an obsolete alias for
`supertag-with-transaction') still does real per-entity rollback."
  (tx-test--with-temp-env
    (let ((a0 (tx-test--node "A" "Original A")))
      (supertag-store-put-entity :nodes "A" a0)
      (should-error
       (with-suppressed-warnings ((obsolete supertag--with-transaction))
         (supertag--with-transaction
           (supertag-store-put-entity :nodes "A" (tx-test--node "A" "Modified A"))
           (error "boom"))))
      (should (equal a0 (supertag-store-get-entity :nodes "A"))))))

(provide 'transaction-test)

;;; transaction-test.el ends here
