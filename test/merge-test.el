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

(provide 'merge-test)

;;; merge-test.el ends here
