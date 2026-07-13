;;; conflicts-test.el --- ERT tests for supertag-conflicts.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for P1-5 of .phrase/phases/phase-git-sync-20260713/PLAN.md's
;; review list: closing the loop on `:sync-conflicts' records that
;; supertag-merge.el (S3) already writes but that nothing previously let a
;; user see or resolve.
;;
;; Every conflict fixture below is built by actually running
;; `supertag-merge-3way' over crafted base/ours/theirs inputs (writing the
;; merged result through the real canonical writer and back in via
;; `supertag-load-store'), NOT by hand-assembling `:sync-conflicts' plists --
;; so these tests break if supertag-merge.el's record shape ever drifts,
;; per the review item's explicit instruction.
;;
;; Every test runs inside an isolated temp directory/store; none of them
;; ever touch the user's real `~/.emacs.d'. Follows the
;; `tx-test--with-temp-env' / `supertag-hardening-test--with-temp-env'
;; fixture pattern from test/transaction-test.el and
;; test/persistence-hardening-test.el.
;;
;; Run:
;;   ./test/run-tests.sh conflicts
;;   emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/conflicts-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ht)
(require 'cl-lib)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-core-transform)
(require 'supertag-core-persistence)
(require 'supertag-merge)
(require 'supertag-conflicts)
(require 'supertag-doctor)

;;; --- Shared fixture (mirrors tx-test--with-temp-env / hardening-test's) ---

(defmacro conflicts-test--with-temp-env (&rest body)
  "Run BODY with persistence/transaction state redirected into an isolated
temp dir and a clean store, exactly like the existing
transaction-test.el/persistence-hardening-test.el fixtures. NOT rebinding
`supertag-persistence-after-load-hook' -- these tests specifically want the
real, globally-registered `supertag-conflicts--notify-after-load' listener
to fire, to prove the load-time visibility wiring actually works end to
end.

Also neutralizes the sync-state-layer save guard for the whole BODY (not
just one `supertag-load-store' call): `supertag--persistence-guard-violations'
refuses to save whenever it thinks the sync-state layer has not been
loaded for the current vault, which -- exactly as noted in
test/persistence-hardening-test.el's own auto-migrate test -- always fires
in an isolated test that never loads the sync module. These tests need
real, un-skipped saves to actually happen (to verify the \"after each
resolution batch: supertag-save-store\" requirement), so this guard is
neutralized for the whole fixture rather than just around one call."
  (declare (indent 0))
  `(let* ((tmp (file-name-as-directory (make-temp-file "supertag-conflicts-test" t)))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag-db-verify-after-save t)
          (supertag-db-lock t)
          (supertag-db-auto-migrate nil)
          (supertag--store nil)
          (supertag--store-origin nil)
          (supertag--db-lock-conflict nil)
          (supertag--db-locked-file nil)
          (supertag--transaction-active nil)
          (supertag--transaction-log nil)
          (supertag--transaction-seen nil)
          (supertag-db--auto-save-timer nil))
     (unwind-protect
         (cl-letf (((symbol-function 'supertag--persistence--expected-sync-state-file)
                    (lambda () nil)))
           ,@body)
       (supertag--db-release-lock)
       (when (timerp supertag-db--auto-save-timer)
         (cancel-timer supertag-db--auto-save-timer))
       (ignore-errors (delete-directory tmp t)))))

;;; --- Building realistic fixtures via the real merge core ---

(defun conflicts-test--parsed (root entities)
  "Build a `supertag-merge--parsed' directly from ROOT plist and ENTITIES.
ENTITIES is a list of ((COLLECTION . ID) . DATA). Mirrors
`supertag-merge-test--parsed' in test/merge-test.el."
  (let ((table (ht-create)))
    (dolist (e entities) (puthash (car e) (cdr e) table))
    (supertag-merge--make-parsed :root root :entities table)))

(defun conflicts-test--load-merged (base ours theirs)
  "Merge BASE/OURS/THEIRS (each a `supertag-merge--parsed') with the real
`supertag-merge-3way', write the result to `supertag-db-file' via the real
canonical writer, and load it for real via `supertag-load-store' -- so the
live store's `:sync-conflicts' collection (and every other entity) is
built by the actual production merge + persistence code paths. The
sync-state guard is neutralized exactly like
`supertag-hardening-test-auto-migrate-stamps-version-and-snapshots-once'
does, since these tests never load the sync module. Returns the
`(merged . conflicts)' pair `supertag-merge-3way' itself returned."
  (let* ((result (supertag-merge-3way base ours theirs))
         (merged (car result))
         (store (supertag-merge--to-store merged)))
    (supertag-persistence-ensure-data-directory)
    (with-temp-buffer
      (set-buffer-file-coding-system 'utf-8-unix)
      (supertag--persistence--write-canonical-store store (current-buffer))
      (write-region (point-min) (point-max) supertag-db-file nil 'silent))
    (cl-letf (((symbol-function 'supertag--persistence--expected-sync-state-file)
               (lambda () nil)))
      (supertag-load-store))
    result))

(defun conflicts-test--node (id title &rest extra)
  "Return a minimal node plist for ID/TITLE plus EXTRA plist keys."
  (append (list :id id :type :node :title title :file "/tmp/f.org") extra))

(defun conflicts-test--assoc (field-id order)
  "Build one tag-field-association plist for FIELD-ID/ORDER."
  (list :field-id field-id :order order))

(defun conflicts-test--conflict-with-key (key)
  "Return the one recorded conflict whose `:key' is KEY (`equal'-compared),
or nil. Small helper so tests can pick a specific conflict out of
`supertag-conflicts-list' without hand-computing its `:id'."
  (cl-find-if (lambda (c) (equal (plist-get c :key) key)) (supertag-conflicts-list)))

;;; --- 1. Query helpers ---

(ert-deftest conflicts-test-list-empty-when-no-collection ()
  "`supertag-conflicts-list'/`-count' are empty/zero on a fresh store, and
never error even though `:sync-conflicts' does not exist yet."
  (conflicts-test--with-temp-env
    (supertag--ensure-store)
    (should (null (supertag-conflicts-list)))
    (should (= 0 (supertag-conflicts-count)))
    ;; Merely asking must not have created the collection.
    (should (null (gethash :sync-conflicts supertag--store)))))

;;; --- 2. Field-level conflict on a node title ---

(defun conflicts-test--seed-title-conflict ()
  "Load a store with exactly one field-level conflict: node \"n1\"'s
`:title' differs on both sides from a common base; `:file' is untouched so
it is NOT also a conflict."
  (let* ((a (conflicts-test--node "n1" "Original"))
         (b (conflicts-test--node "n1" "Ours Title"))
         (c (conflicts-test--node "n1" "Theirs Title"))
         (base (conflicts-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (conflicts-test--parsed nil (list (cons (cons :nodes "n1") b))))
         (theirs (conflicts-test--parsed nil (list (cons (cons :nodes "n1") c)))))
    (conflicts-test--load-merged base ours theirs)))

(ert-deftest conflicts-test-field-conflict-use-theirs-applies-and-removes-record ()
  "use-theirs on a plain field conflict writes theirs' value onto the
entity and removes the conflict record."
  (conflicts-test--with-temp-env
    (conflicts-test--seed-title-conflict)
    (should (= 1 (supertag-conflicts-count)))
    (let* ((conflict (car (supertag-conflicts-list)))
           (id (plist-get conflict :id)))
      (should (equal id "nodes/n1/title"))
      (should (eq (plist-get conflict :kind) :field-conflict))
      ;; No :modified-at on either side -> tiebreak defaults to ours.
      (should (equal (plist-get (supertag-store-get-entity :nodes "n1") :title) "Ours Title"))
      (should (eq :applied (supertag-conflicts--resolve-one id :use-theirs)))
      (should (equal (plist-get (supertag-store-get-entity :nodes "n1") :title) "Theirs Title"))
      (should (= 0 (supertag-conflicts-count)))
      (should (null (supertag-store-get-entity :sync-conflicts id))))))

(ert-deftest conflicts-test-field-conflict-atomicity-rollback-on-error ()
  "An error injected between applying the resolved value and removing the
conflict record rolls back the WHOLE transaction: the entity write is
undone too, and the conflict record survives untouched."
  (conflicts-test--with-temp-env
    (conflicts-test--seed-title-conflict)
    (let* ((id "nodes/n1/title")
           (before-entity (supertag-store-get-entity :nodes "n1"))
           (before-conflict (supertag-store-get-entity :sync-conflicts id)))
      (should (equal (plist-get before-entity :title) "Ours Title"))
      (cl-letf (((symbol-function 'supertag-store-remove-entity)
                 (lambda (&rest _args) (error "simulated failure removing conflict record"))))
        (should-error (supertag-conflicts--resolve-one id :use-theirs)))
      ;; Rolled back: entity is exactly what it was before the attempt (the
      ;; apply step's :title write to "Theirs Title" must not have survived).
      (should (equal before-entity (supertag-store-get-entity :nodes "n1")))
      ;; The conflict record itself was never actually removed (the stub
      ;; intercepted that call), so it is trivially still present too.
      (should (equal before-conflict (supertag-store-get-entity :sync-conflicts id)))
      (should (= 1 (supertag-conflicts-count))))))

;;; --- 3. :delete-vs-modify (whole-entity) ---

(defun conflicts-test--seed-delete-vs-modify ()
  "Load a store where ours deleted node \"n2\" and theirs modified it:
resurrected with theirs' content, plus one `:delete-vs-modify' conflict."
  (let* ((a (conflicts-test--node "n2" "Original"))
         (c (conflicts-test--node "n2" "Theirs Edit"))
         (base (conflicts-test--parsed nil (list (cons (cons :nodes "n2") a))))
         (ours (conflicts-test--parsed nil nil))
         (theirs (conflicts-test--parsed nil (list (cons (cons :nodes "n2") c)))))
    (conflicts-test--load-merged base ours theirs)))

(ert-deftest conflicts-test-delete-vs-modify-use-theirs-keeps-modified ()
  "use-theirs on a delete-vs-modify conflict keeps the resurrected,
modified content and removes the record."
  (conflicts-test--with-temp-env
    (conflicts-test--seed-delete-vs-modify)
    (let* ((conflict (car (supertag-conflicts-list)))
           (id (plist-get conflict :id)))
      (should (equal id "nodes/n2"))
      (should (eq (plist-get conflict :kind) :delete-vs-modify))
      (should (eq (plist-get conflict :ours) :supertag-merge/absent))
      (should (eq :applied (supertag-conflicts--resolve-one id :use-theirs)))
      (should (equal (plist-get (supertag-store-get-entity :nodes "n2") :title) "Theirs Edit"))
      (should (= 0 (supertag-conflicts-count))))))

(ert-deftest conflicts-test-delete-vs-modify-use-ours-re-deletes ()
  "use-ours on a delete-vs-modify conflict re-applies the deletion (ours'
side, which is the absent-placeholder here) instead of erroring or
leaving the resurrected entity in place."
  (conflicts-test--with-temp-env
    (conflicts-test--seed-delete-vs-modify)
    (let ((id "nodes/n2"))
      (should (supertag-store-get-entity :nodes "n2")) ; resurrected by the merge
      (should (eq :applied (supertag-conflicts--resolve-one id :use-ours)))
      (should (null (supertag-store-get-entity :nodes "n2")))
      (should (= 0 (supertag-conflicts-count))))))

;;; --- 4. Whole-entity/hash-shaped (:field-values) ---

(defun conflicts-test--seed-hash-shaped-conflict ()
  "Load a store with one whole-value conflict on a :field-values entity
(a frozen hash table, which freezes to a single-pseudo-key plist -- see
supertag-merge.el's Commentary \"Conflict granularity\" point 2)."
  (let* ((base-val (list :supertag-hash-table (list (cons "f1" "old1") (cons "f2" "old2"))))
         (ours-val (list :supertag-hash-table (list (cons "f1" "ours1") (cons "f2" "old2"))))
         (theirs-val (list :supertag-hash-table (list (cons "f1" "theirs1") (cons "f2" "old2"))))
         (base (conflicts-test--parsed nil (list (cons (cons :field-values "node1") base-val))))
         (ours (conflicts-test--parsed nil (list (cons (cons :field-values "node1") ours-val))))
         (theirs (conflicts-test--parsed nil (list (cons (cons :field-values "node1") theirs-val)))))
    (conflicts-test--load-merged base ours theirs)))

(ert-deftest conflicts-test-hash-shaped-whole-value-use-ours-thaws-hash-table ()
  "Resolving a :field-values whole-value conflict rebuilds a REAL live hash
table for the entity (not the frozen `(:supertag-hash-table ...)' printable
form), with the chosen side's field values."
  (conflicts-test--with-temp-env
    (conflicts-test--seed-hash-shaped-conflict)
    (let* ((conflict (car (supertag-conflicts-list)))
           (id (plist-get conflict :id)))
      (should (eq (plist-get conflict :kind) :field-conflict))
      (should (eq (plist-get conflict :key) :supertag-hash-table))
      ;; Already ours by the tiebreak (no :modified-at) -- resolving
      ;; use-ours is a content no-op but must still thaw + remove the record.
      (should (eq :applied (supertag-conflicts--resolve-one id :use-ours)))
      (should (equal "ours1" (supertag-store-get-field-value "node1" "f1")))
      (should (equal "old2" (supertag-store-get-field-value "node1" "f2")))
      (let ((raw (gethash "node1" (supertag-store-get-collection :field-values))))
        (should (hash-table-p raw)))
      (should (= 0 (supertag-conflicts-count))))))

;;; --- 5. :tag-field-associations: field-id slot + list order ---

(defun conflicts-test--seed-assoc-field-and-order-conflicts ()
  "Load a store with TWO conflicts on the same tag: one on field-id \"f0\"'s
association value, and one on the list's overall order (a genuine 3-way
rotation cycle, exactly like merge-test.el's own order-conflict test)."
  (let* ((base-list (list (conflicts-test--assoc "f0" 0)
                          (conflicts-test--assoc "f1" 1)
                          (conflicts-test--assoc "f2" 2)))
         (ours-list (list (conflicts-test--assoc "f2" 2)
                          (conflicts-test--assoc "f0" 10)
                          (conflicts-test--assoc "f1" 1)))
         (theirs-list (list (conflicts-test--assoc "f1" 1)
                            (conflicts-test--assoc "f2" 2)
                            (conflicts-test--assoc "f0" 20)))
         (base (conflicts-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1") base-list))))
         (ours (conflicts-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1") ours-list))))
         (theirs (conflicts-test--parsed
                  nil (list (cons (cons :tag-field-associations "tag1") theirs-list)))))
    (conflicts-test--load-merged base ours theirs)))

(ert-deftest conflicts-test-tag-field-associations-field-conflict-use-theirs ()
  "use-theirs on the \"f0\" slot conflict upserts theirs' association plist
into the live ordered list and removes the record, leaving the other
slots and the list's own order untouched."
  (conflicts-test--with-temp-env
    (conflicts-test--seed-assoc-field-and-order-conflicts)
    (should (= 2 (supertag-conflicts-count)))
    (let* ((conflict (conflicts-test--conflict-with-key "f0")))
      (should conflict)
      (should (equal (plist-get conflict :collection) :tag-field-associations))
      (should (equal (plist-get conflict :entity-id) "tag1"))
      (should (eq (plist-get conflict :kind) :field-conflict))
      (should (eq :applied (supertag-conflicts--resolve-one (plist-get conflict :id) :use-theirs)))
      (let* ((assocs (supertag-store-get-tag-field-associations "tag1"))
             (f0 (cl-find-if (lambda (a) (equal (plist-get a :field-id) "f0")) assocs)))
        (should (= 20 (plist-get f0 :order)))
        ;; f1/f2 slots were never conflicting -- still present, unchanged.
        (should (cl-find-if (lambda (a) (equal (plist-get a :field-id) "f1")) assocs))
        (should (cl-find-if (lambda (a) (equal (plist-get a :field-id) "f2")) assocs)))
      (should (= 1 (supertag-conflicts-count))))))

(ert-deftest conflicts-test-tag-field-associations-order-conflict-use-theirs ()
  "use-theirs on the `:order' conflict reorders the live list to match
theirs' recorded field-id sequence, restricted to field-ids currently
present."
  (conflicts-test--with-temp-env
    (conflicts-test--seed-assoc-field-and-order-conflicts)
    (let* ((conflict (conflicts-test--conflict-with-key :order)))
      (should conflict)
      (should (equal (plist-get conflict :theirs) '("f1" "f2" "f0")))
      (should (eq :applied (supertag-conflicts--resolve-one (plist-get conflict :id) :use-theirs)))
      (let ((ids (mapcar (lambda (a) (plist-get a :field-id))
                         (supertag-store-get-tag-field-associations "tag1"))))
        (should (equal ids '("f1" "f2" "f0"))))
      (should (= 1 (supertag-conflicts-count))))))

;;; --- 6. Resolving against a missing target ---

(ert-deftest conflicts-test-resolve-missing-target-drops-record-no-error ()
  "Resolving a conflict whose target node was deleted since the merge
silently downgrades to dropping the stale record -- no error, whichever
action was requested."
  (conflicts-test--with-temp-env
    (conflicts-test--seed-title-conflict)
    (let ((id "nodes/n1/title"))
      (supertag-store-remove-entity :nodes "n1") ; simulate "deleted since the merge"
      (should (eq :dropped (supertag-conflicts--resolve-one id :use-ours)))
      (should (= 0 (supertag-conflicts-count)))
      (should (null (supertag-store-get-entity :nodes "n1"))))))

;;; --- 7. Bulk resolution ---

(ert-deftest conflicts-test-bulk-use-theirs-all-resolves-everything ()
  "`supertag-conflicts-use-theirs-all' resolves every conflict (including
one whose target is missing, via the automatic drop-only path), leaves
the count at 0, and actually saves (dirty flag clear afterward)."
  (conflicts-test--with-temp-env
    (let* ((a1 (conflicts-test--node "n1" "Original"))
           (b1 (conflicts-test--node "n1" "Ours Title"))
           (c1 (conflicts-test--node "n1" "Theirs Title"))
           (a4 (conflicts-test--node "n4" "Original4"))
           (c4 (conflicts-test--node "n4" "Theirs Edit4"))
           (a5 (conflicts-test--node "n5" "Original5"))
           (b5 (conflicts-test--node "n5" "Ours5"))
           (c5 (conflicts-test--node "n5" "Theirs5"))
           (base (conflicts-test--parsed
                  nil (list (cons (cons :nodes "n1") a1)
                            (cons (cons :nodes "n4") a4)
                            (cons (cons :nodes "n5") a5))))
           (ours (conflicts-test--parsed
                  nil (list (cons (cons :nodes "n1") b1)
                            (cons (cons :nodes "n5") b5)))) ; ours deleted n4
           (theirs (conflicts-test--parsed
                    nil (list (cons (cons :nodes "n1") c1)
                              (cons (cons :nodes "n4") c4)
                              (cons (cons :nodes "n5") c5)))))
      (conflicts-test--load-merged base ours theirs)
      (should (= 3 (supertag-conflicts-count)))
      ;; Simulate "n5 got deleted after the merge, before the user resolved it".
      (supertag-store-remove-entity :nodes "n5")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (supertag-conflicts-use-theirs-all))
      (should (= 0 (supertag-conflicts-count)))
      (should (equal (plist-get (supertag-store-get-entity :nodes "n1") :title) "Theirs Title"))
      (should (equal (plist-get (supertag-store-get-entity :nodes "n4") :title) "Theirs Edit4"))
      (should (null (supertag-store-get-entity :nodes "n5")))
      ;; "After each resolution batch: supertag-save-store" -- verify it
      ;; actually ran and cleared the dirty flag (guards were satisfied
      ;; since `conflicts-test--load-merged' went through a real
      ;; `supertag-load-store').
      (should-not (supertag-dirty-p)))))

;;; --- 8. Doctor rendering ---

(ert-deftest conflicts-test-doctor-renders-conflicts-section-with-conflicts ()
  "Doctor's \"9. Sync Conflicts\" section shows the count and per-conflict
detail when conflicts exist."
  (conflicts-test--with-temp-env
    (conflicts-test--seed-title-conflict)
    (let* ((buf (supertag-doctor t))
           (text (with-current-buffer buf (buffer-string))))
      (should (string-match-p "9\\. Sync Conflicts" text))
      (should (string-match-p "Count: 1" text))
      (should (string-match-p "nodes/n1/title" text))
      (should (string-match-p "supertag-conflicts-resolve" text)))))

(ert-deftest conflicts-test-doctor-renders-none-when-empty ()
  "Doctor's \"9. Sync Conflicts\" section cleanly reports \"None.\" on a
conflict-free store."
  (conflicts-test--with-temp-env
    (supertag--ensure-store)
    (let* ((buf (supertag-doctor t))
           (text (with-current-buffer buf (buffer-string))))
      (should (string-match-p "9\\. Sync Conflicts" text))
      (should (string-match-p "None\\." text)))))

;;; --- 9. Load-time visibility ---

(defmacro conflicts-test--capture-messages (&rest body)
  "Run BODY and return the *Messages* buffer text logged during it.
Reads the real `*Messages*' buffer (every `message' call appends there
regardless of batch/interactive mode) rather than overriding `message'
itself via `cl-letf': redefining a subr's `symbol-function' triggers
Emacs's native-compilation trampoline machinery, which is not reliably
available in every batch/CI environment (observed to fail here with a
missing `libgccjit'/`emutls_w' toolchain) -- reading the buffer sidesteps
that entirely."
  (declare (indent 0))
  `(let ((supertag-conflicts-test--messages-start
          (with-current-buffer (messages-buffer) (point-max))))
     ,@body
     (with-current-buffer (messages-buffer)
       (buffer-substring-no-properties supertag-conflicts-test--messages-start (point-max)))))

(ert-deftest conflicts-test-load-time-message-fires-when-conflicts-present ()
  "Loading a store that carries recorded sync conflicts messages once,
with a count and a pointer to `M-x supertag-conflicts-resolve' -- proving
`supertag-conflicts--notify-after-load' is actually wired onto
`supertag-persistence-after-load-hook' (see supertag-conflicts.el's
Commentary, \"Load-time visibility\")."
  (conflicts-test--with-temp-env
    (let* ((a (conflicts-test--node "n1" "Original"))
           (b (conflicts-test--node "n1" "Ours Title"))
           (c (conflicts-test--node "n1" "Theirs Title"))
           (base (conflicts-test--parsed nil (list (cons (cons :nodes "n1") a))))
           (ours (conflicts-test--parsed nil (list (cons (cons :nodes "n1") b))))
           (theirs (conflicts-test--parsed nil (list (cons (cons :nodes "n1") c))))
           (result (supertag-merge-3way base ours theirs))
           (store (supertag-merge--to-store (car result))))
      (supertag-persistence-ensure-data-directory)
      (with-temp-buffer
        (set-buffer-file-coding-system 'utf-8-unix)
        (supertag--persistence--write-canonical-store store (current-buffer))
        (write-region (point-min) (point-max) supertag-db-file nil 'silent))
      (let ((log (conflicts-test--capture-messages (supertag-load-store))))
        (should (string-match-p "1 sync conflict" log))
        (should (string-match-p "supertag-conflicts-resolve" log))))))

(ert-deftest conflicts-test-load-time-message-silent-when-no-conflicts ()
  "Loading a conflict-free store never mentions sync conflicts."
  (conflicts-test--with-temp-env
    (let* ((a (conflicts-test--node "n1" "Same"))
           (base (conflicts-test--parsed nil (list (cons (cons :nodes "n1") a))))
           (ours (conflicts-test--parsed nil (list (cons (cons :nodes "n1") (copy-sequence a)))))
           (theirs (conflicts-test--parsed nil (list (cons (cons :nodes "n1") (copy-sequence a))))))
      (let ((log (conflicts-test--capture-messages
                   (conflicts-test--load-merged base ours theirs))))
        (should (= 0 (supertag-conflicts-count)))
        (should-not (string-match-p "sync conflict" log))))))

(provide 'conflicts-test)

;;; conflicts-test.el ends here
