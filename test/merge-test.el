;;; merge-test.el --- ERT tests for the S3a pure 3-way merge core -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for supertag-merge.el, the pure semantic 3-way merge core for
;; supertag-db.el (.phrase/phases/phase-git-sync-20260713/PLAN.md, "S3 语义
;; merge driver" -> "S3a 纯合并核心").  Covers:
;;   - one ERT test per row of the plan's merge decision table;
;;   - the field-type equality matrix (canonicalization-equal never
;;     conflicts; genuinely different values always do);
;;   - conflict record shape, deterministic conflict ids, and idempotence;
;;   - symmetry of `supertag-merge-3way' under swapping ours/theirs, and the
;;     one documented exception (the ours-preference tiebreak);
;;   - the batch driver entry point, including corrupted-input safety;
;;   - legacy-format base input mixed with canonical ours/theirs;
;;   - a 5k-node scale/perf guard.
;;
;; Run:
;;   ./test/run-tests.sh merge
;;   emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/merge-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ht)
(require 'cl-lib)

(defconst supertag-merge-test--repo-dir
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name default-directory)))
  "The org-supertag repository root, captured at load time.
`load-file-name' is only valid while this file is actively being loaded --
NOT later, when a test function runs -- so this must be a constant
computed here at top level, not recomputed inside a test/helper.")

(add-to-list 'load-path supertag-merge-test--repo-dir)

(require 'supertag-core-persistence)
(require 'supertag-merge)

;;; --- Shared helpers ---

(defmacro supertag-merge-test--with-temp-dir (var &rest body)
  "Bind VAR to a fresh temp directory for BODY; delete it afterward."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "supertag-merge-test" t))))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defun supertag-merge-test--parsed (root entities)
  "Build a `supertag-merge--parsed' directly from ROOT plist and ENTITIES.
ENTITIES is a list of ((COLLECTION . ID) . DATA)."
  (let ((table (ht-create)))
    (dolist (e entities) (puthash (car e) (cdr e) table))
    (supertag-merge--make-parsed :root root :entities table)))

(defun supertag-merge-test--read-file-bytes (file)
  "Return FILE's literal byte contents."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun supertag-merge-test--build-node-store (ids &optional edit-fn)
  "Return a store hash table with one :node entity per id in IDS.
EDIT-FN, if given, is called as (EDIT-FN ID BASE-PLIST) and must return the
plist to store for that id; defaults to a plain untouched node."
  (let ((store (ht-create)) (nodes (ht-create)))
    (puthash :version "5.0.0" store)
    (dolist (id ids)
      (let ((plist (list :id id :type :node :title (format "Node %s" id)
                          :file "/tmp/f.org")))
        (puthash id (if edit-fn (funcall edit-fn id plist) plist) nodes)))
    (puthash :nodes nodes store)
    store))

(defun supertag-merge-test--write-store (store path)
  "Write STORE to PATH using the real S2 canonical writer."
  (with-temp-buffer
    (set-buffer-file-coding-system 'utf-8-unix)
    (supertag--persistence--write-canonical-store store (current-buffer))
    (write-region (point-min) (point-max) path nil 'silent)))

(defun supertag-merge-test--write-legacy-store (store path)
  "Write STORE to PATH using the legacy single-`prin1' format."
  (with-temp-file path
    (let ((print-escape-nonascii t) (print-length nil) (print-level nil)
          (print-circle t))
      (prin1 store (current-buffer)))))

(defun supertag-merge-test--parse-store (store dir name)
  "Write STORE to DIR/NAME.el (canonical) and parse it back via the real path."
  (let ((path (expand-file-name (concat name ".el") dir)))
    (supertag-merge-test--write-store store path)
    (supertag-merge--parse-file path)))

(defun supertag-merge-test--entity (parsed collection id)
  (supertag-merge--entity-get parsed collection id))

(defun supertag-merge-test--conflict-keys (conflicts)
  "Return the sorted list of :key values from CONFLICTS (list of (id . data))."
  (sort (mapcar (lambda (c) (plist-get (cdr c) :key)) conflicts)
        (lambda (a b) (string< (format "%s" a) (format "%s" b)))))

(defconst supertag-merge-test--time-a '(27000 1 0 0))
(defconst supertag-merge-test--time-b '(27000 2 0 0))
(defconst supertag-merge-test--time-c '(27000 3 0 0))

;;; --- 1. Decision-table rows (entity level) ---

(ert-deftest supertag-merge-test-row-unchanged-unchanged ()
  "base=A ours=A theirs=A -> A, no conflicts."
  (let* ((n (list :id "n1" :type :node :title "T" :modified-at supertag-merge-test--time-a))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") n))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") n))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") n))))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))
    (should (equal n (supertag-merge-test--entity (car result) :nodes "n1")))))

(ert-deftest supertag-merge-test-row-ours-changed-theirs-unchanged ()
  "base=A ours=B theirs=A -> B (single-side change), no conflicts."
  (let* ((a (list :id "n1" :title "Original"))
         (b (list :id "n1" :title "Ours Edit"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") b))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))
    (should (equal b (supertag-merge-test--entity (car result) :nodes "n1")))))

(ert-deftest supertag-merge-test-row-theirs-changed-ours-unchanged ()
  "base=A ours=A theirs=C -> C (single-side change), no conflicts."
  (let* ((a (list :id "n1" :title "Original"))
         (c (list :id "n1" :title "Theirs Edit"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") c))))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))
    (should (equal c (supertag-merge-test--entity (car result) :nodes "n1")))))

(ert-deftest supertag-merge-test-row-both-changed-same-value ()
  "base=A ours=B theirs=B -> B, no conflict (both sides made the identical edit)."
  (let* ((a (list :id "n1" :title "Original"))
         (b (list :id "n1" :title "Same New Title"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") b))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") (copy-sequence b)))))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))
    (should (equal b (supertag-merge-test--entity (car result) :nodes "n1")))))

(ert-deftest supertag-merge-test-row-both-changed-differently-field-level ()
  "base=A ours=B theirs=C, differing fields -> field-level merge.
One field (:title) conflicts (both sides changed it differently); another
field (:file) was only touched by ours -> adopted without conflict."
  (let* ((a (list :id "n1" :title "Original" :file "/tmp/a.org"
                  :modified-at supertag-merge-test--time-a))
         (b (list :id "n1" :title "Ours Title" :file "/tmp/ours.org"
                  :modified-at supertag-merge-test--time-b))
         (c (list :id "n1" :title "Theirs Title" :file "/tmp/a.org"
                  :modified-at supertag-merge-test--time-c))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") b))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") c))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result))
         (merged (supertag-merge-test--entity (car result) :nodes "n1")))
    ;; Only :title is a true conflict; :file (ours-only change) is not, and
    ;; :modified-at (both changed, differently) is its own conflict too.
    (should (equal (supertag-merge-test--conflict-keys conflicts) '(:modified-at :title)))
    (should (equal (plist-get merged :file) "/tmp/ours.org"))
    ;; theirs has the newer :modified-at -> theirs wins both conflicting keys.
    (should (equal (plist-get merged :title) "Theirs Title"))
    (should (equal (plist-get merged :modified-at) supertag-merge-test--time-c))))

(ert-deftest supertag-merge-test-row-deleted-one-side-unchanged-other ()
  "base=A ours=deleted theirs=A (unchanged) -> deleted, no conflict."
  (let* ((a (list :id "n1" :title "Original"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (supertag-merge-test--parsed nil nil))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))
    (should (eq (supertag-merge-test--entity (car result) :nodes "n1")
                supertag-merge--absent))))

(ert-deftest supertag-merge-test-row-deleted-one-side-unchanged-other-symmetric ()
  "base=A ours=A (unchanged) theirs=deleted -> deleted, no conflict."
  (let* ((a (list :id "n1" :title "Original"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (theirs (supertag-merge-test--parsed nil nil))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))
    (should (eq (supertag-merge-test--entity (car result) :nodes "n1")
                supertag-merge--absent))))

(ert-deftest supertag-merge-test-row-deleted-vs-modified-resurrects ()
  "base=A ours=deleted theirs=C (modified) -> resurrect C + delete-vs-modify conflict."
  (let* ((a (list :id "n1" :title "Original"))
         (c (list :id "n1" :title "Theirs Edit"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (supertag-merge-test--parsed nil nil))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") c))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result)))
    (should (equal c (supertag-merge-test--entity (car result) :nodes "n1")))
    (should (= 1 (length conflicts)))
    (let ((data (cdr (car conflicts))))
      (should (eq (plist-get data :kind) :delete-vs-modify))
      (should (null (plist-get data :key)))
      (should (eq (plist-get data :ours) :supertag-merge/absent))
      (should (equal (plist-get data :theirs) c))
      (should (equal (plist-get data :base) a))
      (should (equal (plist-get data :id) "nodes/n1")))))

(ert-deftest supertag-merge-test-row-deleted-vs-modified-resurrects-symmetric ()
  "base=A ours=B (modified) theirs=deleted -> resurrect B + delete-vs-modify conflict."
  (let* ((a (list :id "n1" :title "Original"))
         (b (list :id "n1" :title "Ours Edit"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") b))))
         (theirs (supertag-merge-test--parsed nil nil))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result)))
    (should (equal b (supertag-merge-test--entity (car result) :nodes "n1")))
    (should (= 1 (length conflicts)))
    (let ((data (cdr (car conflicts))))
      (should (eq (plist-get data :kind) :delete-vs-modify))
      (should (equal (plist-get data :ours) b))
      (should (eq (plist-get data :theirs) :supertag-merge/absent)))))

(ert-deftest supertag-merge-test-row-both-deleted ()
  "base=A ours=deleted theirs=deleted -> deleted, no conflict."
  (let* ((a (list :id "n1" :title "Original"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (supertag-merge-test--parsed nil nil))
         (theirs (supertag-merge-test--parsed nil nil))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))
    (should (eq (supertag-merge-test--entity (car result) :nodes "n1")
                supertag-merge--absent))))

(ert-deftest supertag-merge-test-row-added-both-sides-same-value-no-base ()
  "no base; ours=B theirs=B (same new id, same value) -> B, no conflict."
  (let* ((b (list :id "n2" :title "New Node"))
         (base (supertag-merge-test--parsed nil nil))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n2") b))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n2") (copy-sequence b)))))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))
    (should (equal b (supertag-merge-test--entity (car result) :nodes "n2")))))

(ert-deftest supertag-merge-test-row-added-both-sides-different-value-field-level ()
  "no base; ours=B theirs=C (same new id, different value) -> field-level merge."
  (let* ((b (list :id "n2" :title "Ours New" :file "/tmp/x.org"))
         (c (list :id "n2" :title "Theirs New" :file "/tmp/x.org"))
         (base (supertag-merge-test--parsed nil nil))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n2") b))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n2") c))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result))
         (merged (supertag-merge-test--entity (car result) :nodes "n2")))
    (should (equal (supertag-merge-test--conflict-keys conflicts) '(:title)))
    (should (equal (plist-get merged :file) "/tmp/x.org"))
    ;; No :modified-at on either side to break the tie -> falls back to ours.
    (should (equal (plist-get merged :title) "Ours New"))
    (let ((data (cdr (car conflicts))))
      ;; No common ancestor entity at all -> every field's base reads as a
      ;; plain missing/nil value (the same nil-vs-missing collapse the
      ;; equality matrix exercises), not the delete-vs-modify placeholder
      ;; (that placeholder is reserved for an entity-level deletion).
      (should (null (plist-get data :base))))))

(ert-deftest supertag-merge-test-row-added-single-side-no-base ()
  "no base; only ours has the new id -> adopted, no conflict."
  (let* ((b (list :id "n3" :title "Only Ours"))
         (base (supertag-merge-test--parsed nil nil))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n3") b))))
         (theirs (supertag-merge-test--parsed nil nil))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))
    (should (equal b (supertag-merge-test--entity (car result) :nodes "n3")))))

;;; --- 2. Root scalar merge, including :version higher-wins ---

(ert-deftest supertag-merge-test-root-scalar-single-side-change ()
  "Root :version changed only by theirs -> adopted, no conflict."
  (let* ((base (supertag-merge-test--parsed (list :version "5.0.0") nil))
         (ours (supertag-merge-test--parsed (list :version "5.0.0") nil))
         (theirs (supertag-merge-test--parsed (list :version "5.1.0") nil))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))
    (should (equal (plist-get (supertag-merge--parsed-root (car result)) :version) "5.1.0"))))

(ert-deftest supertag-merge-test-root-version-conflict-picks-higher ()
  "Root :version changed differently on both sides -> higher version wins + conflict."
  (let* ((base (supertag-merge-test--parsed (list :version "5.0.0") nil))
         (ours (supertag-merge-test--parsed (list :version "5.0.1") nil))
         (theirs (supertag-merge-test--parsed (list :version "5.2.0") nil))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result)))
    (should (equal (plist-get (supertag-merge--parsed-root (car result)) :version) "5.2.0"))
    (should (= 1 (length conflicts)))
    (should (equal (car (car conflicts)) "root/version"))
    (should (eq (plist-get (cdr (car conflicts)) :kind) :field-conflict))))

;;; --- 3. Field-type equality matrix ---
;; For each type: canonicalization-equal values across ours/theirs never
;; conflict (even when the two sides differ in representation but not
;; meaning); genuinely different values always do.

(defun supertag-merge-test--assert-no-conflict-then-conflict (mk-entity same-other-value diff-value)
  "Shared driver for one equality-matrix row.
MK-ENTITY is (lambda (field-value) ...) building an entity plist with
:modified-at and the field under test set to FIELD-VALUE.  Runs two
sub-merges: one where ours/theirs both carry (possibly differently
represented but) equal values -- expect zero conflicts; and one where
theirs' value is DIFF-VALUE -- expect exactly one conflict."
  (let* ((base-val (funcall mk-entity :base))
         (ours-eq (funcall mk-entity same-other-value))
         (theirs-eq (funcall mk-entity same-other-value))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") base-val))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") ours-eq))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") theirs-eq))))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result))))
  (let* ((base-val (funcall mk-entity :base))
         (ours-changed (funcall mk-entity same-other-value))
         (theirs-changed (funcall mk-entity diff-value))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") base-val))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") ours-changed))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") theirs-changed))))
         (result (supertag-merge-3way base ours theirs)))
    (should (= 1 (length (cdr result))))))

(ert-deftest supertag-merge-test-equality-matrix-string ()
  (supertag-merge-test--assert-no-conflict-then-conflict
   (lambda (v) (list :id "n1" :field (if (eq v :base) "base" (format "%s" v))))
   "same" "different"))

(ert-deftest supertag-merge-test-equality-matrix-number-int ()
  (supertag-merge-test--assert-no-conflict-then-conflict
   (lambda (v) (list :id "n1" :field (if (eq v :base) 0 v)))
   42 99))

(ert-deftest supertag-merge-test-equality-matrix-number-float ()
  (supertag-merge-test--assert-no-conflict-then-conflict
   (lambda (v) (list :id "n1" :field (if (eq v :base) 0.0 v)))
   3.14 2.71))

(ert-deftest supertag-merge-test-equality-matrix-boolean ()
  (supertag-merge-test--assert-no-conflict-then-conflict
   (lambda (v) (list :id "n1" :field (if (eq v :base) nil v)))
   t :other-non-nil))

(ert-deftest supertag-merge-test-equality-matrix-nil-vs-missing ()
  "A field explicitly nil on one side and entirely absent on the other side
of the same slot must NOT be treated as a conflict."
  (let* ((base (supertag-merge-test--parsed
                nil (list (cons (cons :nodes "n1") (list :id "n1")))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :nodes "n1") (list :id "n1" :field nil)))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :nodes "n1") (list :id "n1")))))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))))

(ert-deftest supertag-merge-test-equality-matrix-iso-date-string ()
  (supertag-merge-test--assert-no-conflict-then-conflict
   (lambda (v) (list :id "n1" :field (if (eq v :base) "2026-01-01" v)))
   "2026-07-13" "2026-07-14"))

(ert-deftest supertag-merge-test-equality-matrix-emacs-time-list ()
  (supertag-merge-test--assert-no-conflict-then-conflict
   (lambda (v) (list :id "n1" :field (if (eq v :base) '(1 2 3 4) v)))
   supertag-merge-test--time-b supertag-merge-test--time-c))

(ert-deftest supertag-merge-test-equality-matrix-options-list ()
  (supertag-merge-test--assert-no-conflict-then-conflict
   (lambda (v) (list :id "n1" :field (if (eq v :base) '("x") v)))
   '("a" "b") '("c" "d")))

(ert-deftest supertag-merge-test-equality-matrix-node-reference-id ()
  (supertag-merge-test--assert-no-conflict-then-conflict
   (lambda (v) (list :id "n1" :field (if (eq v :base) "node-base-uuid" v)))
   "node-ref-uuid-1" "node-ref-uuid-2"))

(ert-deftest supertag-merge-test-equality-matrix-plain-list-value ()
  (supertag-merge-test--assert-no-conflict-then-conflict
   (lambda (v) (list :id "n1" :field (if (eq v :base) '(1 2 3) v)))
   '(4 5 6) '(7 8 9)))

(ert-deftest supertag-merge-test-equality-matrix-nested-plist-key-order-insensitive ()
  "A nested plist field with keys in a different order but equal content on
both sides must NOT conflict (canonicalization sorts keys before compare)."
  (let* ((base (supertag-merge-test--parsed
                nil (list (cons (cons :nodes "n1")
                                 (list :id "n1" :meta (list :alpha 1 :zeta 2))))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :nodes "n1")
                                 (list :id "n1" :meta (list :zeta 2 :alpha 1))))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :nodes "n1")
                                   (list :id "n1" :meta (list :alpha 1 :zeta 2))))))
         (result (supertag-merge-3way base ours theirs)))
    (should (null (cdr result)))))

;;; --- 4. Conflict record shape, deterministic ids, idempotence ---

(ert-deftest supertag-merge-test-conflict-record-shape ()
  "A field conflict's record has exactly the documented keys/values."
  (let* ((a (list :id "n1" :title "Original"))
         (b (list :id "n1" :title "Ours"))
         (c (list :id "n1" :title "Theirs"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") b))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") c))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result))
         (data (cdr (car conflicts))))
    (should (equal (plist-get data :collection) :nodes))
    (should (equal (plist-get data :entity-id) "n1"))
    (should (equal (plist-get data :key) :title))
    (should (equal (plist-get data :ours) "Ours"))
    (should (equal (plist-get data :theirs) "Theirs"))
    (should (equal (plist-get data :base) "Original"))
    (should (eq (plist-get data :kind) :field-conflict))
    (should (null (plist-get data :detected-at)))
    (should (equal (plist-get data :id) "nodes/n1/title"))
    ;; The conflict is also present in the merged store under :sync-conflicts.
    (should (equal data (supertag-merge-test--entity (car result) :sync-conflicts "nodes/n1/title")))))

(ert-deftest supertag-merge-test-conflict-id-deterministic-across-collections ()
  "Conflict ids encode collection/entity/key so different entities never collide."
  (let* ((a1 (list :id "n1" :title "O1")) (b1 (list :id "n1" :title "B1")) (c1 (list :id "n1" :title "C1"))
         (a2 (list :id "n1" :title "O2")) (b2 (list :id "n1" :title "B2")) (c2 (list :id "n1" :title "C2"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a1)
                                                        (cons (cons :tags "n1") a2))))
         (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") b1)
                                                        (cons (cons :tags "n1") b2))))
         (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") c1)
                                                          (cons (cons :tags "n1") c2))))
         (result (supertag-merge-3way base ours theirs))
         (ids (sort (mapcar #'car (cdr result)) #'string<)))
    (should (equal ids '("nodes/n1/title" "tags/n1/title")))))

(ert-deftest supertag-merge-test-idempotent-same-inputs-byte-identical ()
  "Merging the same three inputs twice produces byte-identical serialized output."
  (supertag-merge-test--with-temp-dir dir
    (let* ((a (list :id "n1" :title "Original" :modified-at supertag-merge-test--time-a))
           (b (list :id "n1" :title "Ours" :modified-at supertag-merge-test--time-b))
           (c (list :id "n1" :title "Theirs" :modified-at supertag-merge-test--time-c))
           (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
           (ours (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") b))))
           (theirs (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") c))))
           (out1 (expand-file-name "out1.el" dir))
           (out2 (expand-file-name "out2.el" dir)))
      (supertag-merge--write-merged (car (supertag-merge-3way base ours theirs)) out1)
      (supertag-merge--write-merged (car (supertag-merge-3way base ours theirs)) out2)
      (should (equal (supertag-merge-test--read-file-bytes out1)
                     (supertag-merge-test--read-file-bytes out2))))))

;;; --- 5. Symmetry and tiebreak ---

(ert-deftest supertag-merge-test-symmetry-when-winner-determined-by-modified-at ()
  "The WINNING value written into the merged entity is the same regardless
of which side is passed as ours vs theirs, whenever :modified-at
unambiguously decides the winner (title here has a single, non-time-valued
conflicting field, so :modified-at itself is the only other conflicting
key and it always resolves to whichever side is chronologically later,
independent of argument order).  Byte-for-byte equality of the whole
merged FILE is intentionally not asserted here: `:sync-conflicts' records
always label values by argument position (:ours/:theirs), so those two
records necessarily swap contents when the arguments swap -- that is
correct, not a symmetry violation (see the tiebreak test below, which
checks that both values remain present under both labels)."
  (let* ((a (list :id "n1" :title "Original" :modified-at supertag-merge-test--time-a))
         (left (list :id "n1" :title "Left Edit" :modified-at supertag-merge-test--time-b))
         (right (list :id "n1" :title "Right Edit" :modified-at supertag-merge-test--time-c))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (p-left (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") left))))
         (p-right (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") right))))
         (result-1 (supertag-merge-3way base p-left p-right))
         (result-2 (supertag-merge-3way base p-right p-left))
         (merged-1 (supertag-merge-test--entity (car result-1) :nodes "n1"))
         (merged-2 (supertag-merge-test--entity (car result-2) :nodes "n1")))
    ;; :modified-at time-c (right) is strictly newer than time-b (left) in
    ;; BOTH runs, regardless of which one was passed as "ours" -> the same
    ;; winning content either way.
    (should (equal merged-1 merged-2))
    (should (equal (plist-get merged-1 :title) "Right Edit"))
    (should (= 2 (length (cdr result-1))))
    (should (= 2 (length (cdr result-2))))))

(ert-deftest supertag-merge-test-tiebreak-prefers-ours-when-not-comparable ()
  "When :modified-at is absent/equal on both conflicting sides, the value
written into the entity is whichever side was passed as OURS -- so
swapping ours/theirs changes the winning value.  Both values still survive
in the conflict record either way (zero silent loss)."
  (let* ((a (list :id "n1" :title "Original"))
         (left (list :id "n1" :title "Left Edit"))
         (right (list :id "n1" :title "Right Edit"))
         (base (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") a))))
         (p-left (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") left))))
         (p-right (supertag-merge-test--parsed nil (list (cons (cons :nodes "n1") right))))
         (result-1 (supertag-merge-3way base p-left p-right))
         (result-2 (supertag-merge-3way base p-right p-left)))
    (should (equal (plist-get (supertag-merge-test--entity (car result-1) :nodes "n1") :title)
                   "Left Edit"))
    (should (equal (plist-get (supertag-merge-test--entity (car result-2) :nodes "n1") :title)
                   "Right Edit"))
    ;; Both conflict records still name both values regardless of direction.
    (let ((data-1 (cdr (car (cdr result-1))))
          (data-2 (cdr (car (cdr result-2)))))
      (should (equal (list (plist-get data-1 :ours) (plist-get data-1 :theirs))
                      '("Left Edit" "Right Edit")))
      (should (equal (list (plist-get data-2 :ours) (plist-get data-2 :theirs))
                      '("Right Edit" "Left Edit"))))))

;;; --- 6. Legacy-format input ---

(ert-deftest supertag-merge-test-legacy-base-canonical-ours-theirs ()
  "BASE in the legacy single-`prin1' format, OURS/THEIRS canonical -> merges fine."
  (supertag-merge-test--with-temp-dir dir
    (let* ((base-store (supertag-merge-test--build-node-store '("n1")))
           (ours-store (supertag-merge-test--build-node-store
                        '("n1") (lambda (_id p) (plist-put (copy-sequence p) :title "Ours Title"))))
           (theirs-store (supertag-merge-test--build-node-store '("n1")))
           (base-path (expand-file-name "base.el" dir)))
      (supertag-merge-test--write-legacy-store base-store base-path)
      (let* ((base (supertag-merge--parse-file base-path))
             (ours (supertag-merge-test--parse-store ours-store dir "ours"))
             (theirs (supertag-merge-test--parse-store theirs-store dir "theirs"))
             (result (supertag-merge-3way base ours theirs)))
        (should (null (cdr result)))
        (should (equal (plist-get (supertag-merge-test--entity (car result) :nodes "n1") :title)
                       "Ours Title"))))))

;;; --- 7. Driver-level tests (shell out to a real batch Emacs) ---

(defun supertag-merge-test--run-driver (base ours theirs)
  "Run the real batch merge driver against BASE/OURS/THEIRS paths.
Returns (EXIT-CODE . OUTPUT-STRING)."
  (with-temp-buffer
    (let ((exit (call-process "emacs" nil t nil
                              "-Q" "--batch"
                              "-L" supertag-merge-test--repo-dir
                              "-l" "supertag-merge.el"
                              "-f" "supertag-merge-driver-main"
                              base ours theirs)))
      (cons exit (buffer-string)))))

(ert-deftest supertag-merge-test-driver-merges-and-reports-conflicts ()
  "The real batch driver process merges three temp files, exit 0, conflict
recorded, and *Messages*-equivalent stderr/stdout has no DB-init noise."
  (supertag-merge-test--with-temp-dir dir
    (let* ((base-store (supertag-merge-test--build-node-store '("n1")))
           (ours-store (supertag-merge-test--build-node-store
                        '("n1") (lambda (_id p) (plist-put (copy-sequence p) :title "Ours"))))
           (theirs-store (supertag-merge-test--build-node-store
                          '("n1") (lambda (_id p) (plist-put (copy-sequence p) :title "Theirs"))))
           (base-path (expand-file-name "base.el" dir))
           (ours-path (expand-file-name "ours.el" dir))
           (theirs-path (expand-file-name "theirs.el" dir)))
      (supertag-merge-test--write-store base-store base-path)
      (supertag-merge-test--write-store ours-store ours-path)
      (supertag-merge-test--write-store theirs-store theirs-path)
      (let* ((run (supertag-merge-test--run-driver base-path ours-path theirs-path))
             (exit (car run))
             (output (cdr run)))
        (should (= exit 0))
        ;; No org-supertag.el / DB-init / auto-save chatter.
        (should-not (string-match-p "auto-save\\|Loading org-supertag\\|supertag-db-file" output))
        (let ((merged (supertag-merge--parse-file ours-path)))
          ;; Neither side set :modified-at, so the tiebreak defaults to ours.
          (should (equal (plist-get (supertag-merge-test--entity merged :nodes "n1") :title)
                         "Ours"))
          (should (= 1 (hash-table-count
                        (let ((h (ht-create)))
                          (maphash (lambda (k _v) (when (eq (car k) :sync-conflicts) (puthash k t h)))
                                   (supertag-merge--parsed-entities merged))
                          h)))))))))

(ert-deftest supertag-merge-test-driver-corrupted-theirs-exits-1-ours-untouched ()
  "A corrupted THEIRS file makes the driver exit 1 and leave OURS byte-untouched."
  (supertag-merge-test--with-temp-dir dir
    (let* ((base-store (supertag-merge-test--build-node-store '("n1")))
           (ours-store (supertag-merge-test--build-node-store '("n1")))
           (base-path (expand-file-name "base.el" dir))
           (ours-path (expand-file-name "ours.el" dir))
           (theirs-path (expand-file-name "theirs.el" dir)))
      (supertag-merge-test--write-store base-store base-path)
      (supertag-merge-test--write-store ours-store ours-path)
      (with-temp-file theirs-path (insert "not a valid ((( sexp store"))
      (let* ((before (supertag-merge-test--read-file-bytes ours-path))
             (run (supertag-merge-test--run-driver base-path ours-path theirs-path))
             (exit (car run))
             (after (supertag-merge-test--read-file-bytes ours-path)))
        (should (= exit 1))
        (should (equal before after))))))

;;; --- 8. Scale guard: 5k nodes, disjoint edits ---

(defconst supertag-merge-test--scale-n 5000)

(defun supertag-merge-test--build-scale-store (edit-range)
  "Build a 5k-node store; nodes whose index is in EDIT-RANGE (a cons
MIN . MAX, inclusive) get an edited title; all others are untouched."
  (supertag-merge-test--build-node-store
   (cl-loop for i from 0 below supertag-merge-test--scale-n
            collect (format "node%05d" i))
   (lambda (id p)
     (let ((i (string-to-number (substring id 4))))
       (if (and (>= i (car edit-range)) (<= i (cdr edit-range)))
           (plist-put (copy-sequence p) :title (format "Edited %s" id))
         p)))))

(ert-deftest supertag-merge-test-scale-5k-disjoint-edits-fast-and-conflict-free ()
  "Three 5k-node variants with disjoint edits merge in well under 5s, with
zero conflicts and the full, correct entity count."
  (supertag-merge-test--with-temp-dir dir
    (let* ((base-store (supertag-merge-test--build-node-store
                         (cl-loop for i from 0 below supertag-merge-test--scale-n
                                  collect (format "node%05d" i))))
           (ours-store (supertag-merge-test--build-scale-store (cons 0 2499)))
           (theirs-store (supertag-merge-test--build-scale-store (cons 2500 4999)))
           (base (supertag-merge-test--parse-store base-store dir "base"))
           (ours (supertag-merge-test--parse-store ours-store dir "ours"))
           (theirs (supertag-merge-test--parse-store theirs-store dir "theirs"))
           (start (current-time))
           (result (supertag-merge-3way base ours theirs))
           (elapsed (float-time (time-subtract (current-time) start))))
      (message "scale guard: 3x%d-node merge took %.3fs" supertag-merge-test--scale-n elapsed)
      (should (< elapsed 5.0))
      (should (null (cdr result)))
      (let ((count 0))
        (maphash (lambda (k _v) (when (eq (car k) :nodes) (cl-incf count)))
                 (supertag-merge--parsed-entities (car result)))
        (should (= count supertag-merge-test--scale-n))))))

;;; --- 9. :tag-field-associations: the confirmed P0 corruption repro ---
;; `:tag-field-associations' entity values are ORDERED LISTS OF PLISTS, e.g.
;; `((:field-id "f0" :order 0))', NOT a plist themselves.  Before the fix,
;; `supertag-merge-3way' decomposed every conflicting entity with
;; `plist-get'/`plist-put' unconditionally, which -- on this shape --
;; silently produced an interleaved-nil garbage list with zero conflict
;; records.  These tests pin the fixed behavior: a real, deterministic
;; ordered-set-by-`:field-id' merge.

(defun supertag-merge-test--assoc (field-id order)
  "Build one tag-field-association plist for FIELD-ID/ORDER."
  (list :field-id field-id :order order))

(defun supertag-merge-test--no-nils-p (list)
  "Return non-nil if no element of LIST is nil (guards against the
interleaved-nil corruption the P0 bug produced)."
  (not (memq nil list)))

(ert-deftest supertag-merge-test-tag-field-associations-legacy-shape-falls-back-whole ()
  "Legacy string associations are preserved wholesale, never skipped."
  (let* ((base-value '("f0"))
         (ours-value '("f0" "f1"))
         (theirs-value '("f0" "f2"))
         (base (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1") base-value))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1") ours-value))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :tag-field-associations "tag1") theirs-value))))
         (result (supertag-merge-3way base ours theirs))
         (data (cdr (car (cdr result)))))
    (should (equal (supertag-merge-test--entity
                    (car result) :tag-field-associations "tag1")
                   ours-value))
    (should (= 1 (length (cdr result))))
    (should (null (plist-get data :key)))
    (should (equal (plist-get data :base) base-value))
    (should (equal (plist-get data :ours) ours-value))
    (should (equal (plist-get data :theirs) theirs-value))))

(ert-deftest supertag-merge-test-tag-field-associations-invalid-shapes-fall-back-whole ()
  "Any malformed side forces a whole-value conflict without data loss."
  (dolist
      (case
       (list
        (list "malformed-base"
              (list '(:field-id "f0" :order))
              (list (supertag-merge-test--assoc "f0" 1))
              (list (supertag-merge-test--assoc "f0" 2)))
        (list "duplicate-ours"
              (list (supertag-merge-test--assoc "f0" 0))
              (list (supertag-merge-test--assoc "f0" 1)
                    (supertag-merge-test--assoc "f0" 2))
              (list (supertag-merge-test--assoc "f0" 3)))
        (list "empty-id-theirs"
              (list (supertag-merge-test--assoc "f0" 0))
              (list (supertag-merge-test--assoc "f0" 1))
              (list (supertag-merge-test--assoc "" 2)))))
    (cl-destructuring-bind (name base-value ours-value theirs-value) case
      (ert-info ((format "shape: %s" name))
        (let* ((base (supertag-merge-test--parsed
                      nil (list (cons (cons :tag-field-associations "tag1") base-value))))
               (ours (supertag-merge-test--parsed
                      nil (list (cons (cons :tag-field-associations "tag1") ours-value))))
               (theirs (supertag-merge-test--parsed
                        nil (list (cons (cons :tag-field-associations "tag1") theirs-value))))
               (result (supertag-merge-3way base ours theirs))
               (data (cdr (car (cdr result)))))
          (should (equal (supertag-merge-test--entity
                          (car result) :tag-field-associations "tag1")
                         ours-value))
          (should (= 1 (length (cdr result))))
          (should (null (plist-get data :key)))
          (should (equal (plist-get data :base) base-value))
          (should (equal (plist-get data :ours) ours-value))
          (should (equal (plist-get data :theirs) theirs-value)))))))

(ert-deftest supertag-merge-test-tag-field-associations-p0-repro ()
  "The exact reviewer repro: base/ours/theirs each independently name a
DIFFERENT single field-id.  Must merge to a valid ordered list containing
ours' and theirs' additions (f0 was dropped by both sides -> mutual
delete, no conflict), never an interleaved-nil garbage list, and with zero
conflicts (no two sides ever touched the same field-id)."
  (let* ((base (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1")
                                 (list (supertag-merge-test--assoc "f0" 0))))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1")
                                 (list (supertag-merge-test--assoc "f1" 1))))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :tag-field-associations "tag1")
                                   (list (supertag-merge-test--assoc "f2" 2))))))
         (result (supertag-merge-3way base ours theirs))
         (merged (supertag-merge-test--entity (car result) :tag-field-associations "tag1")))
    (should (null (cdr result)))
    (should (listp merged))
    (should (supertag-merge-test--no-nils-p merged))
    (should (= 2 (length merged)))
    (should (equal merged (list (supertag-merge-test--assoc "f1" 1)
                                 (supertag-merge-test--assoc "f2" 2))))))

(ert-deftest supertag-merge-test-tag-field-associations-disjoint-additions-no-conflict ()
  "base has f0; ours additionally adds f1, theirs additionally adds f2 ->
all three present in the merge, zero conflicts."
  (let* ((base (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1")
                                 (list (supertag-merge-test--assoc "f0" 0))))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1")
                                 (list (supertag-merge-test--assoc "f0" 0)
                                       (supertag-merge-test--assoc "f1" 1))))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :tag-field-associations "tag1")
                                   (list (supertag-merge-test--assoc "f0" 0)
                                         (supertag-merge-test--assoc "f2" 2))))))
         (result (supertag-merge-3way base ours theirs))
         (merged (supertag-merge-test--entity (car result) :tag-field-associations "tag1")))
    (should (null (cdr result)))
    (should (equal merged (list (supertag-merge-test--assoc "f0" 0)
                                 (supertag-merge-test--assoc "f1" 1)
                                 (supertag-merge-test--assoc "f2" 2))))))

(ert-deftest supertag-merge-test-tag-field-associations-concurrent-inserts-keep-both-orders ()
  "Concurrent inserts between the same anchors retain both side orders."
  (let* ((a (supertag-merge-test--assoc "a" 0))
         (x (supertag-merge-test--assoc "x" 1))
         (b (supertag-merge-test--assoc "b" 2))
         (c (supertag-merge-test--assoc "c" 3))
         (base (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1") (list a c)))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1") (list a x c)))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :tag-field-associations "tag1") (list a b c)))))
         (result (supertag-merge-3way base ours theirs))
         (merged (supertag-merge-test--entity
                  (car result) :tag-field-associations "tag1")))
    (should (null (cdr result)))
    (should (equal merged (list a x b c)))))

(ert-deftest supertag-merge-test-tag-field-associations-same-field-id-conflict ()
  "Both sides modify f0's :order differently -> a conflict record naming
that field-id specifically, ours written into the merged list."
  (let* ((base (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1")
                                 (list (supertag-merge-test--assoc "f0" 0))))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1")
                                 (list (supertag-merge-test--assoc "f0" 1))))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :tag-field-associations "tag1")
                                   (list (supertag-merge-test--assoc "f0" 2))))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result))
         (merged (supertag-merge-test--entity (car result) :tag-field-associations "tag1")))
    (should (= 1 (length conflicts)))
    (should (equal merged (list (supertag-merge-test--assoc "f0" 1))))
    (let ((data (cdr (car conflicts))))
      (should (equal (plist-get data :collection) :tag-field-associations))
      (should (equal (plist-get data :entity-id) "tag1"))
      (should (equal (plist-get data :key) "f0"))
      (should (eq (plist-get data :kind) :field-conflict))
      (should (equal (plist-get data :ours) (supertag-merge-test--assoc "f0" 1)))
      (should (equal (plist-get data :theirs) (supertag-merge-test--assoc "f0" 2)))
      (should (equal (plist-get data :base) (supertag-merge-test--assoc "f0" 0)))
      (should (equal (plist-get data :id)
                     "tag-field-associations/tag1/field/f0")))))

(ert-deftest supertag-merge-test-tag-field-associations-order-conflict ()
  "Same field-ids on all three sides, but ours and theirs each reorder them
differently from base AND from each other -> ours' order wins, plus one
extra `:order' conflict record (values themselves are untouched, so no
per-field-id conflicts)."
  (let* ((f0 (supertag-merge-test--assoc "f0" 0))
         (f1 (supertag-merge-test--assoc "f1" 1))
         (f2 (supertag-merge-test--assoc "f2" 2))
         (base (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1") (list f0 f1 f2)))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1") (list f2 f0 f1)))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :tag-field-associations "tag1") (list f1 f2 f0)))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result))
         (merged (supertag-merge-test--entity (car result) :tag-field-associations "tag1")))
    (should (= 1 (length conflicts)))
    (should (equal merged (list f2 f0 f1)))
    (let ((data (cdr (car conflicts))))
      (should (equal (plist-get data :key) :order))
      (should (eq (plist-get data :kind) :field-conflict))
      (should (equal (plist-get data :ours) '("f2" "f0" "f1")))
      (should (equal (plist-get data :theirs) '("f1" "f2" "f0")))
      (should (equal (plist-get data :base) '("f0" "f1" "f2"))))))

(ert-deftest supertag-merge-test-tag-field-associations-order-ids-do-not-collide ()
  "A field named order and the list-order metadata keep distinct conflicts."
  (let* ((base-order (supertag-merge-test--assoc "order" 0))
         (ours-order (supertag-merge-test--assoc "order" 1))
         (theirs-order (supertag-merge-test--assoc "order" 2))
         (f1 (supertag-merge-test--assoc "f1" 1))
         (f2 (supertag-merge-test--assoc "f2" 2))
         (base (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1")
                                 (list base-order f1 f2)))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :tag-field-associations "tag1")
                                 (list f2 ours-order f1)))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :tag-field-associations "tag1")
                                   (list f1 f2 theirs-order)))))
         (result (supertag-merge-3way base ours theirs))
         (ids (sort (mapcar #'car (cdr result)) #'string<)))
    (should (equal ids '("tag-field-associations/tag1/field/order"
                         "tag-field-associations/tag1/meta/order")))
    (dolist (id ids)
      (should (supertag-merge-test--entity (car result) :sync-conflicts id)))))

;;; --- 10. Hash-table-shaped entities (:field-values / legacy :fields):
;;; recursive per-map-key/leaf granularity (the dogfood-repro fix), with
;;; whole-LEAF-value granularity as the terminal fallback for a genuine
;;; non-map leaf disagreement ---
;;
;; PRE-FIX BEHAVIOR (pinned by this suite's git history): every conflict on
;; one of these entities produced exactly ONE whole-entity conflict record
;; naming the `:supertag-hash-table' pseudo-key itself, no matter how many
;; (or how few) of the underlying hash table's keys actually differed.  Live
;; GitHub dogfooding found this to be a real product gap: machine A setting
;; field "priority" and machine B concurrently setting field "deadline" on
;; the SAME node both write into this same frozen-hash entity, and the two
;; DIFFERENT fields collided as one whole-value conflict instead of merging
;; -- exactly contradicting the "merge field-by-field" promise, because the
;; only test coverage for this shape (this section) exercised a plist NODE
;; entity, never the `:fields' storage `supertag-field-set' actually writes.
;;
;; POST-FIX BEHAVIOR: a conflict on the entity's own `:supertag-hash-table'
;; pseudo-key recurses map-key-by-map-key (`supertag-merge--merge-hash-marker-map');
;; a key touched by only one side is adopted without conflict; the same key
;; changed differently on both sides recurses AGAIN when both sides' values
;; are themselves frozen hash-marker forms (the legacy `:fields' node -> tag
;; -> field nesting); a genuine, non-map leaf disagreement is the true
;; conflict, now recorded with the full key path, not the pseudo-key.

(ert-deftest supertag-merge-test-hash-shaped-entity-single-leaf-conflict-still-narrow ()
  "UPDATED regression test (see this section's Commentary): this exact
scenario -- a :field-values entity where exactly one nested key (\"f1\")
is set differently on both sides and another (\"f2\") is untouched -- is
now GRANULAR, not whole-value: only \"f1\" conflicts (recorded with a
key-path conflict naming \"f1\", not the `:supertag-hash-table' pseudo-key),
\"f2\" survives untouched with zero conflict, and the merged entity ends up
equal to `ours-val' only because \"f1\"'s conflict tiebreak (no
:modified-at present -> ours-preference) happens to pick the same value
`ours-val' already had -- not because the whole entity was ever taken
wholesale.  This is the direct, deliberately-updated descendant of the old
`supertag-merge-test-hash-shaped-entity-whole-value-conflict': its old
assertion (`:key' is the `:supertag-hash-table' pseudo-key) is precisely
the whole-value behavior the dogfood fix replaces for map-shaped
disagreements, so it is updated here rather than kept as-is."
  (let* ((base-val (list :supertag-hash-table (list (cons "f1" "old1") (cons "f2" "old2"))))
         (ours-val (list :supertag-hash-table (list (cons "f1" "ours1") (cons "f2" "old2"))))
         (theirs-val (list :supertag-hash-table (list (cons "f1" "theirs1") (cons "f2" "old2"))))
         (base (supertag-merge-test--parsed
                nil (list (cons (cons :field-values "node1") base-val))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :field-values "node1") ours-val))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :field-values "node1") theirs-val))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result))
         (merged (supertag-merge-test--entity (car result) :field-values "node1"))
         (data (cdr (car conflicts))))
    ;; Exactly one conflict -- "f2" never touched, so never conflicts.
    (should (= 1 (length conflicts)))
    (should (equal (plist-get data :collection) :field-values))
    (should (equal (plist-get data :entity-id) "node1"))
    ;; The leaf key itself, NOT the whole-entity pseudo-key.
    (should (equal (plist-get data :key) "f1"))
    (should (equal (plist-get data :key-path) '("f1")))
    (should (equal (plist-get data :ours) "ours1"))
    (should (equal (plist-get data :theirs) "theirs1"))
    (should (equal (plist-get data :base) "old1"))
    (should (equal (plist-get data :id) "field-values/node1/f1"))
    (should (equal merged ours-val))))

(ert-deftest supertag-merge-test-dogfood-different-fields-same-node-clean-merge ()
  "THE flagship dogfood repro (live GitHub dogfooding, two-machine
scenario): machine A runs `supertag-field-set' setting \"priority\" on tag
\"project\"; machine B concurrently sets \"deadline\" on the SAME tag on
the SAME node.  Both write into the legacy `:fields' collection, whose
entity value is a nested hash table (node -> tag -> field -> value),
freezing to nested `:supertag-hash-table' marker forms.  Before the fix
this collided as ONE whole-entity conflict (A's field landing only in the
conflict record, not the store) -- exactly the case the README's \"merge
field-by-field\" promise was supposed to cover, and exactly what E2E-2
missed by testing plist node entities instead of this `:fields' shape.
After the fix: base has {project: {status}}; ours adds \"priority\";
theirs adds \"deadline\" -- merged has ALL THREE fields under \"project\",
with ZERO conflicts."
  (let* ((base-val
          (list :supertag-hash-table
                (list (cons "project"
                            (list :supertag-hash-table
                                  (list (cons "status" "todo")))))))
         (ours-val
          (list :supertag-hash-table
                (list (cons "project"
                            (list :supertag-hash-table
                                  (list (cons "priority" "high")
                                        (cons "status" "todo")))))))
         (theirs-val
          (list :supertag-hash-table
                (list (cons "project"
                            (list :supertag-hash-table
                                  (list (cons "deadline" "2026-08-01")
                                        (cons "status" "todo")))))))
         (base (supertag-merge-test--parsed
                nil (list (cons (cons :fields "node1") base-val))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :fields "node1") ours-val))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :fields "node1") theirs-val))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result))
         (merged (supertag-merge-test--entity (car result) :fields "node1"))
         (project-fields (cadr (cdr (assoc "project" (cadr merged))))))
    (should (null conflicts))
    (should (equal (sort (mapcar #'car project-fields) #'string<)
                   '("deadline" "priority" "status")))
    (should (equal (cdr (assoc "deadline" project-fields)) "2026-08-01"))
    (should (equal (cdr (assoc "priority" project-fields)) "high"))
    (should (equal (cdr (assoc "status" project-fields)) "todo"))))

(ert-deftest supertag-merge-test-fields-different-tags-clean-merge ()
  "Deeper nesting, still zero conflicts: ours edits a field under tag1;
theirs edits a (different) field under a DIFFERENT tag, tag2.  The two
sides never touch the same tag at all, so this already resolves at the
first (tag-id) recursion level as two single-side changes -- confirming
the recursion cascades correctly through the top `:supertag-hash-table'
pseudo-key without needing to go a level deeper for THIS scenario."
  (let* ((base-val
          (list :supertag-hash-table
                (list (cons "tag1" (list :supertag-hash-table (list (cons "status" "todo"))))
                      (cons "tag2" (list :supertag-hash-table (list (cons "status" "todo")))))))
         (ours-val
          (list :supertag-hash-table
                (list (cons "tag1" (list :supertag-hash-table (list (cons "status" "doing"))))
                      (cons "tag2" (list :supertag-hash-table (list (cons "status" "todo")))))))
         (theirs-val
          (list :supertag-hash-table
                (list (cons "tag1" (list :supertag-hash-table (list (cons "status" "todo"))))
                      (cons "tag2" (list :supertag-hash-table (list (cons "status" "done")))))))
         (base (supertag-merge-test--parsed
                nil (list (cons (cons :fields "node1") base-val))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :fields "node1") ours-val))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :fields "node1") theirs-val))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result))
         (merged (supertag-merge-test--entity (car result) :fields "node1"))
         (tag1-fields (cadr (cdr (assoc "tag1" (cadr merged)))))
         (tag2-fields (cadr (cdr (assoc "tag2" (cadr merged))))))
    (should (null conflicts))
    (should (equal (cdr (assoc "status" tag1-fields)) "doing"))
    (should (equal (cdr (assoc "status" tag2-fields)) "done"))))

(ert-deftest supertag-merge-test-fields-same-field-leaf-conflict ()
  "True leaf conflict: both sides set the SAME field under the SAME tag to
DIFFERENT values.  Exactly one conflict, its id naming the full key path,
ours written in place (no :modified-at -> ours-preference tiebreak), and
theirs fully preserved in the conflict record (zero data loss)."
  (let* ((base-val
          (list :supertag-hash-table
                (list (cons "project" (list :supertag-hash-table (list (cons "status" "todo")))))))
         (ours-val
          (list :supertag-hash-table
                (list (cons "project" (list :supertag-hash-table (list (cons "status" "doing")))))))
         (theirs-val
          (list :supertag-hash-table
                (list (cons "project" (list :supertag-hash-table (list (cons "status" "done")))))))
         (base (supertag-merge-test--parsed
                nil (list (cons (cons :fields "node1") base-val))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :fields "node1") ours-val))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :fields "node1") theirs-val))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result))
         (merged (supertag-merge-test--entity (car result) :fields "node1"))
         (project-fields (cadr (cdr (assoc "project" (cadr merged)))))
         (data (cdr (car conflicts))))
    (should (= 1 (length conflicts)))
    (should (equal (plist-get data :id) "fields/node1/project/status"))
    (should (equal (plist-get data :collection) :fields))
    (should (equal (plist-get data :entity-id) "node1"))
    (should (equal (plist-get data :key) "status"))
    (should (equal (plist-get data :key-path) '("project" "status")))
    (should (eq (plist-get data :kind) :field-conflict))
    (should (equal (plist-get data :ours) "doing"))
    (should (equal (plist-get data :theirs) "done"))
    (should (equal (plist-get data :base) "todo"))
    ;; Ours written in place; theirs still fully present above, not lost.
    (should (equal (cdr (assoc "status" project-fields)) "doing"))))

(ert-deftest supertag-merge-test-fields-delete-vs-modify-field-level ()
  "Delete-vs-modify at the FIELD level: ours deletes \"priority\" entirely
(the key is simply absent from ours' tag alist); theirs concurrently
modifies it.  Same resurrect-plus-conflict semantics as an entity-level
delete-vs-modify, just one level down: theirs' value survives in the
merged store, and one `:delete-vs-modify' conflict is recorded naming the
full key path."
  (let* ((base-val
          (list :supertag-hash-table
                (list (cons "project"
                            (list :supertag-hash-table
                                  (list (cons "priority" "low") (cons "status" "todo")))))))
         (ours-val
          (list :supertag-hash-table
                (list (cons "project"
                            (list :supertag-hash-table (list (cons "status" "todo")))))))
         (theirs-val
          (list :supertag-hash-table
                (list (cons "project"
                            (list :supertag-hash-table
                                  (list (cons "priority" "high") (cons "status" "todo")))))))
         (base (supertag-merge-test--parsed
                nil (list (cons (cons :fields "node1") base-val))))
         (ours (supertag-merge-test--parsed
                nil (list (cons (cons :fields "node1") ours-val))))
         (theirs (supertag-merge-test--parsed
                  nil (list (cons (cons :fields "node1") theirs-val))))
         (result (supertag-merge-3way base ours theirs))
         (conflicts (cdr result))
         (merged (supertag-merge-test--entity (car result) :fields "node1"))
         (project-fields (cadr (cdr (assoc "project" (cadr merged)))))
         (data (cdr (car conflicts))))
    (should (= 1 (length conflicts)))
    (should (eq (plist-get data :kind) :delete-vs-modify))
    (should (equal (plist-get data :key) "priority"))
    (should (equal (plist-get data :key-path) '("project" "priority")))
    (should (equal (plist-get data :id) "fields/node1/project/priority"))
    (should (eq (plist-get data :ours) :supertag-merge/absent))
    (should (equal (plist-get data :theirs) "high"))
    (should (equal (plist-get data :base) "low"))
    ;; Resurrected: theirs' modified value survives in the merged store.
    (should (equal (cdr (assoc "priority" project-fields)) "high"))
    (should (equal (cdr (assoc "status" project-fields)) "todo"))))

(ert-deftest supertag-merge-test-fields-determinism-argument-order-and-idempotence ()
  "Determinism/idempotence for the new nested-hash-map path: (1) re-merging
the same three inputs twice produces byte-identical serialized output
(idempotence, exactly like the existing plist-entity test); (2) merging
with ours/theirs swapped still yields exactly one conflict naming the same
key path, with only the OURS/THEIRS labels (and, per the documented
tiebreak, the winning value) swapping -- never a different shape or a
different number of conflicts."
  (supertag-merge-test--with-temp-dir dir
    (let* ((base-val
            (list :supertag-hash-table
                  (list (cons "project" (list :supertag-hash-table (list (cons "status" "todo")))))))
           (left-val
            (list :supertag-hash-table
                  (list (cons "project" (list :supertag-hash-table (list (cons "status" "doing")))))))
           (right-val
            (list :supertag-hash-table
                  (list (cons "project" (list :supertag-hash-table (list (cons "status" "done")))))))
           (base (supertag-merge-test--parsed
                  nil (list (cons (cons :fields "node1") base-val))))
           (p-left (supertag-merge-test--parsed
                    nil (list (cons (cons :fields "node1") left-val))))
           (p-right (supertag-merge-test--parsed
                     nil (list (cons (cons :fields "node1") right-val))))
           (out1 (expand-file-name "out1.el" dir))
           (out2 (expand-file-name "out2.el" dir)))
      ;; Idempotence: same arguments, twice, byte-identical output.
      (supertag-merge--write-merged (car (supertag-merge-3way base p-left p-right)) out1)
      (supertag-merge--write-merged (car (supertag-merge-3way base p-left p-right)) out2)
      (should (equal (supertag-merge-test--read-file-bytes out1)
                     (supertag-merge-test--read-file-bytes out2)))
      ;; Argument-order swap: same conflict shape/count, ours-preference
      ;; tiebreak flips the winning value (no :modified-at anywhere here).
      (let* ((result-1 (supertag-merge-3way base p-left p-right))
             (result-2 (supertag-merge-3way base p-right p-left))
             (conflicts-1 (cdr result-1))
             (conflicts-2 (cdr result-2))
             (data-1 (cdr (car conflicts-1)))
             (data-2 (cdr (car conflicts-2)))
             (merged-1 (supertag-merge-test--entity (car result-1) :fields "node1"))
             (merged-2 (supertag-merge-test--entity (car result-2) :fields "node1")))
        (should (= 1 (length conflicts-1)))
        (should (= 1 (length conflicts-2)))
        (should (equal (plist-get data-1 :id) (plist-get data-2 :id)))
        (should (equal (plist-get data-1 :id) "fields/node1/project/status"))
        (should (equal (list (plist-get data-1 :ours) (plist-get data-1 :theirs))
                       '("doing" "done")))
        (should (equal (list (plist-get data-2 :ours) (plist-get data-2 :theirs))
                       '("done" "doing")))
        (should (equal (cdr (assoc "status" (cadr (cdr (assoc "project" (cadr merged-1))))))
                       "doing"))
        (should (equal (cdr (assoc "status" (cadr (cdr (assoc "project" (cadr merged-2))))))
                       "done"))))))

;;; --- 11. Safety net: non-plist, non-tag-field-associations even-length
;;; list shapes never guessed to be a plist ---
;; For each handcrafted shape below, BASE/OURS/THEIRS differ pairwise (a
;; genuine three-way conflict).  None of these are plist-shaped (each
;; fails `supertag--persistence--plist-p' for a different reason -- some
;; because the very first element isn't a keyword, one deliberately
;; because a LATER even-index element isn't a keyword even though the
;; first one is) and none use the `:tag-field-associations' collection, so
;; every one of them must fall through to the whole-value safety net:
;; exactly one whole-entity conflict record, and the merged value must be
;; exactly one side taken wholesale -- never an interleaved-nil result.

(defconst supertag-merge-test--nonplist-shape-triples
  (list
   (list "plain-ints"
         '(1 2 3 4) '(1 2 3 40) '(1 2 3 400))
   (list "plain-strings"
         '("a" "b" "c" "d") '("a" "b" "c" "dd") '("a" "b" "c" "ddd"))
   (list "list-of-bare-plists"
         '((:field-id "f0") (:field-id "f1"))
         '((:field-id "f0") (:field-id "f9"))
         '((:field-id "f0") (:field-id "f99")))
   (list "list-of-plists-under-wrong-collection"
         (list (supertag-merge-test--assoc "f0" 0) (supertag-merge-test--assoc "f1" 1))
         (list (supertag-merge-test--assoc "f0" 0) (supertag-merge-test--assoc "f1" 11))
         (list (supertag-merge-test--assoc "f0" 0) (supertag-merge-test--assoc "f1" 111)))
   (list "keyword-first-but-not-every-even-index"
         (list :field-id "f0" (list :nested 1) "v")
         (list :field-id "f0" (list :nested 2) "v")
         (list :field-id "f0" (list :nested 3) "v"))
   (list "all-nil"
         '(nil nil nil nil) '(nil nil nil t) (list nil nil nil :marker))
   (list "booleans"
         '(t nil t nil) '(t nil t t) (list t nil t :marker))
   (list "conses"
         '((1 . 2) (3 . 4)) '((1 . 2) (3 . 40)) '((1 . 2) (3 . 400)))
   (list "vectors"
         (list [1 2] [3 4]) (list [1 2] [3 40]) (list [1 2] [3 400]))
   (list "string-keys"
         '("k1" "v1" "k2" "v2") '("k1" "v1" "k2" "v22") '("k1" "v1" "k2" "v222")))
  "10 handcrafted (NAME BASE OURS THEIRS) triples: proper, even-length
lists that must never be misclassified as plists.")

(ert-deftest supertag-merge-test-nonplist-shapes-safety-net ()
  "Property-style loop over `supertag-merge-test--nonplist-shape-triples':
every shape must merge without interleaving nils, and must record a
conflict whenever (as constructed here) the merged result differs from a
side taken wholesale."
  (dolist (triple supertag-merge-test--nonplist-shape-triples)
    (cl-destructuring-bind (name base-val ours-val theirs-val) triple
      (ert-info ((format "shape: %s" name))
        ;; None of these are plist-shaped: pin that down directly first,
        ;; so a future change to the discriminator can't silently make
        ;; this test vacuous.
        (should-not (supertag--persistence--plist-p base-val))
        (should-not (supertag--persistence--plist-p ours-val))
        (should-not (supertag--persistence--plist-p theirs-val))
        (let* ((base (supertag-merge-test--parsed
                      nil (list (cons (cons :nodes "n1") base-val))))
               (ours (supertag-merge-test--parsed
                      nil (list (cons (cons :nodes "n1") ours-val))))
               (theirs (supertag-merge-test--parsed
                        nil (list (cons (cons :nodes "n1") theirs-val))))
               (result (supertag-merge-3way base ours theirs))
               (conflicts (cdr result))
               (merged (supertag-merge-test--entity (car result) :nodes "n1")))
          ;; Never an interleaved-nil garbage result: the merged value is
          ;; always exactly one side taken wholesale (never a
          ;; plist-decomposed hybrid, which is what would introduce spurious
          ;; nils from mismatched plist-get/plist-put on a non-plist list)
          ;; -- and since base/ours/theirs are pairwise distinct here, a
          ;; conflict is always recorded.  (A blanket "no nil elements"
          ;; check would be wrong here since some shapes, e.g. "all-nil"
          ;; and "booleans", legitimately contain nil as real data --
          ;; byte-exact equality with a whole legitimate side is the
          ;; correct, stronger guarantee.)
          (should (listp merged))
          (should (or (equal merged ours-val) (equal merged theirs-val)))
          (should (= 1 (length conflicts)))
          (should (eq (plist-get (cdr (car conflicts)) :kind) :field-conflict))
          (should (null (plist-get (cdr (car conflicts)) :key)))
          ;; No :modified-at on either side -> ours-preference tiebreak.
          (should (equal merged ours-val)))))))

;;; --- 12. Determinism/idempotence on the new (ordered-list / safety-net)
;;; paths ---

(ert-deftest supertag-merge-test-idempotent-ordered-list-inputs-byte-identical ()
  "Re-running the merge on the same :tag-field-associations + safety-net
inputs twice produces byte-identical serialized output, exactly like the
existing plist-entity idempotence test."
  (supertag-merge-test--with-temp-dir dir
    (let* ((base (supertag-merge-test--parsed
                  nil (list (cons (cons :tag-field-associations "tag1")
                                   (list (supertag-merge-test--assoc "f0" 0)))
                            (cons (cons :nodes "n1") '(1 2 3 4)))))
           (ours (supertag-merge-test--parsed
                  nil (list (cons (cons :tag-field-associations "tag1")
                                   (list (supertag-merge-test--assoc "f1" 1)))
                            (cons (cons :nodes "n1") '(1 2 3 40)))))
           (theirs (supertag-merge-test--parsed
                    nil (list (cons (cons :tag-field-associations "tag1")
                                     (list (supertag-merge-test--assoc "f2" 2)))
                              (cons (cons :nodes "n1") '(1 2 3 400)))))
           (out1 (expand-file-name "out1.el" dir))
           (out2 (expand-file-name "out2.el" dir)))
      (supertag-merge--write-merged (car (supertag-merge-3way base ours theirs)) out1)
      (supertag-merge--write-merged (car (supertag-merge-3way base ours theirs)) out2)
      (should (equal (supertag-merge-test--read-file-bytes out1)
                     (supertag-merge-test--read-file-bytes out2))))))

(provide 'merge-test)

;;; merge-test.el ends here
