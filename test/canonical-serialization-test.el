;;; canonical-serialization-test.el --- ERT tests for S2 canonical DB serialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for the S2 "canonical, deterministic, line-per-entity
;; serialization" work (.phrase/phases/phase-git-sync-20260713/PLAN.md
;; "S2 规范化序列化"): the on-disk DB format written by
;; `supertag--persistence-write-store-atomically' must be:
;;   - deterministic: same logical content -> byte-identical file, on any
;;     machine, regardless of hash-table/insertion iteration order;
;;   - line-per-entity: one `git diff' line per changed entity/field;
;;   - loadable both ways: old (single prin1 hash-table) files still load,
;;     and this build's saves are readable by the format-detecting loader.
;;
;; Every test runs inside an isolated temp directory; none of them ever
;; touch the user's real `~/.emacs.d'.
;;
;; Run:
;;   ./test/run-tests.sh canon
;;   emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/canonical-serialization-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ht)
(require 'cl-lib)
(require 'benchmark)

(when load-file-name
  (add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name))))

(require 'supertag-core-store)
(require 'supertag-core-persistence)

;;; --- Shared helpers ---

(defmacro supertag-canon-test--with-temp-env (&rest body)
  "Run BODY with persistence state redirected into an isolated temp dir.
Mirrors the fixture pattern in test/persistence-hardening-test.el."
  (declare (indent 0))
  `(let* ((tmp (file-name-as-directory (make-temp-file "supertag-canon-test" t)))
          (supertag-data-directory tmp)
          (supertag-db-file (expand-file-name "supertag-db.el" tmp))
          (supertag-db-backup-directory (expand-file-name "backups" tmp))
          (supertag-db-verify-after-save t)
          (supertag-db-lock nil)
          (supertag-db-auto-migrate nil)
          (supertag--store nil)
          (supertag--store-origin nil)
          (supertag--db-lock-conflict nil)
          (supertag--db-locked-file nil))
     (unwind-protect
         (progn ,@body)
       (supertag--db-release-lock)
       (ignore-errors (delete-directory tmp t)))))

(defun supertag-canon-test--read-file-bytes (file)
  "Return the literal contents of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun supertag-canon-test--file-lines (file)
  "Return FILE's contents as a list of lines (no trailing empty element)."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun supertag-canon-test--write-legacy-db (file store)
  "Write STORE into FILE using the OLD single-`prin1'-of-a-hash-table format."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-escape-nonascii t)
          (print-length nil)
          (print-level nil)
          (print-circle t))
      (prin1 store (current-buffer)))))

;; --- Rich fixture: exercises every shape the pre-check found in the store
;; (plain entity plists, nested plists, lists-of-plists, and the legacy
;; nested-hash-table :fields / :field-values collections) ---

(defun supertag-canon-test--build-store (node-ids &optional reverse-order)
  "Return a fresh store hash table populated deterministically from NODE-IDS.
When REVERSE-ORDER is non-nil, entities within each collection are
inserted in the reverse of their natural order — this is used by the
determinism test to prove output order does not depend on insertion
order, only on entity id."
  (let* ((store (ht-create))
         (nodes (ht-create))
         (tags (ht-create))
         (field-values (ht-create))
         (fields (ht-create))
         (tag-field-assocs (ht-create))
         (boards (ht-create))
         (ordered-ids (if reverse-order (reverse node-ids) node-ids)))
    (puthash :version "5.0.0" store)
    (dolist (id ordered-ids)
      ;; Deliberately out-of-alphabetical-order plist keys, including one
      ;; nested plist, to exercise recursive key sorting.
      (puthash id
               (list :type :node
                     :title (format "Node %s" id)
                     :id id
                     :tags (list "tag-b" "tag-a")
                     :meta (list :zeta 1 :alpha 2)
                     :file "/tmp/f.org")
               nodes))
    (puthash "tag-a" (list :id "tag-a" :type :tag :name "tag-a" :extends nil) tags)
    (puthash "tag-b" (list :id "tag-b" :type :tag :name "tag-b" :extends nil) tags)
    (puthash "tag-a"
             (list (list :field-id "priority" :order 1)
                   (list :field-id "effort" :order 0))
             tag-field-assocs)
    (puthash "board-1" (list :id "board-1" :name "Board One" :type :board) boards)
    ;; :field-values -- node-id -> hash(field-id -> value); a nested hash
    ;; table as entity VALUE (not a plist), per the pre-check finding.
    (dolist (id ordered-ids)
      (let ((node-table (ht-create)))
        (puthash "priority" (format "P-%s" id) node-table)
        (puthash "effort" (length id) node-table)
        (puthash id node-table field-values)))
    ;; :fields (legacy) -- node-id -> hash(tag-id -> hash(field-name -> value)):
    ;; three-level nested hash tables.
    (dolist (id ordered-ids)
      (let ((node-table (ht-create))
            (tag-table (ht-create)))
        (puthash "Priority" "High" tag-table)
        (puthash "tag-a" tag-table node-table)
        (puthash id node-table fields)))
    (puthash :nodes nodes store)
    (puthash :tags tags store)
    (puthash :field-values field-values store)
    (puthash :fields fields store)
    (puthash :tag-field-associations tag-field-assocs store)
    (puthash :boards boards store)
    store))

;;; --- 1. Determinism ---

(ert-deftest supertag-canon-test-determinism-across-insertion-order ()
  "Same logical content, inserted in different order, serializes identically."
  (supertag-canon-test--with-temp-env
    (let ((store-a (supertag-canon-test--build-store '("n1" "n2" "n3" "n4" "n5") nil))
          (store-b (supertag-canon-test--build-store '("n1" "n2" "n3" "n4" "n5") t))
          (file-a (expand-file-name "a.el" supertag-data-directory))
          (file-b (expand-file-name "b.el" supertag-data-directory)))
      (with-temp-buffer
        (supertag--persistence--write-canonical-store store-a (current-buffer))
        (write-region (point-min) (point-max) file-a nil 'silent))
      (with-temp-buffer
        (supertag--persistence--write-canonical-store store-b (current-buffer))
        (write-region (point-min) (point-max) file-b nil 'silent))
      (should (equal (supertag-canon-test--read-file-bytes file-a)
                     (supertag-canon-test--read-file-bytes file-b))))))

(ert-deftest supertag-canon-test-determinism-repeated-serialization ()
  "Serializing the same store twice in a row produces byte-identical output."
  (supertag-canon-test--with-temp-env
    (let ((store (supertag-canon-test--build-store '("n1" "n2" "n3")))
          (file-1 (expand-file-name "1.el" supertag-data-directory))
          (file-2 (expand-file-name "2.el" supertag-data-directory)))
      (with-temp-buffer
        (supertag--persistence--write-canonical-store store (current-buffer))
        (write-region (point-min) (point-max) file-1 nil 'silent))
      (with-temp-buffer
        (supertag--persistence--write-canonical-store store (current-buffer))
        (write-region (point-min) (point-max) file-2 nil 'silent))
      (should (equal (supertag-canon-test--read-file-bytes file-1)
                     (supertag-canon-test--read-file-bytes file-2))))))

;;; --- 2. Roundtrip ---

(ert-deftest supertag-canon-test-roundtrip-save-load-save-byte-identical ()
  "save -> load -> save produces byte-identical files; store is semantically equal."
  (supertag-canon-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (setq supertag--store (supertag-canon-test--build-store '("n1" "n2" "n3")))
    (supertag--persistence-write-store-atomically supertag-db-file)
    (let ((first-bytes (supertag-canon-test--read-file-bytes supertag-db-file))
          (orig-nodes (hash-table-count (gethash :nodes supertag--store))))
      (supertag--record-store-origin :ok)
      (supertag-load-store)
      (should (= orig-nodes (hash-table-count (supertag-store-get-collection :nodes))))
      ;; Sample field value survives the roundtrip.
      (let* ((fv-root (supertag-store-get-collection :field-values))
             (n1-table (gethash "n1" fv-root)))
        (should (hash-table-p n1-table))
        (should (equal (gethash "priority" n1-table) "P-n1")))
      (supertag-mark-dirty)
      (supertag--persistence-write-store-atomically supertag-db-file)
      (should (equal first-bytes (supertag-canon-test--read-file-bytes supertag-db-file))))))

;;; --- 3. Legacy compat ---

(ert-deftest supertag-canon-test-legacy-format-loads-then-saves-canonical ()
  "An old single-`prin1' DB file loads fine; the next save writes canonical."
  (supertag-canon-test--with-temp-env
    (let ((legacy-store (ht-create))
          (nodes (ht-create)))
      (puthash "A" (list :id "A" :type :node :title "t" :file "/tmp/f") nodes)
      (puthash "B" (list :id "B" :type :node :title "t2" :file "/tmp/f2") nodes)
      (puthash :nodes nodes legacy-store)
      (puthash :version "5.0.0" legacy-store)
      (supertag-canon-test--write-legacy-db supertag-db-file legacy-store)
      ;; Confirm it really is the OLD format (starts with `#s(', not `(').
      (let ((first-char (with-temp-buffer
                           (insert-file-contents supertag-db-file)
                           (char-after (point-min)))))
        (should (eq first-char ?#)))
      (supertag--record-store-origin :ok)
      (supertag-load-store)
      (should (= 2 (hash-table-count (supertag-store-get-collection :nodes))))
      (supertag-mark-dirty)
      (supertag--persistence-write-store-atomically supertag-db-file)
      (let ((first-char (with-temp-buffer
                           (insert-file-contents supertag-db-file)
                           (char-after (point-min)))))
        (should (eq first-char ?\;)))
      (let ((second-line (nth 1 (supertag-canon-test--file-lines supertag-db-file))))
        (should (string-match-p "canonical format" second-line)))
      ;; Reload the now-canonical file and confirm content survived.
      (supertag-load-store)
      (should (= 2 (hash-table-count (supertag-store-get-collection :nodes)))))))

;;; --- 4. Single-line diff ---

(ert-deftest supertag-canon-test-single-field-change-is-single-line-diff ()
  "Changing one field on one node changes exactly one line in the DB file."
  (supertag-canon-test--with-temp-env
    (let ((store (supertag-canon-test--build-store '("n1" "n2" "n3" "n4" "n5")))
          (file-before (expand-file-name "before.el" supertag-data-directory))
          (file-after (expand-file-name "after.el" supertag-data-directory)))
      (with-temp-buffer
        (supertag--persistence--write-canonical-store store (current-buffer))
        (write-region (point-min) (point-max) file-before nil 'silent))
      (let* ((nodes (gethash :nodes store))
             (n3 (gethash "n3" nodes)))
        (puthash "n3" (plist-put (copy-sequence n3) :title "CHANGED TITLE") nodes))
      (with-temp-buffer
        (supertag--persistence--write-canonical-store store (current-buffer))
        (write-region (point-min) (point-max) file-after nil 'silent))
      (let ((lines-before (supertag-canon-test--file-lines file-before))
            (lines-after (supertag-canon-test--file-lines file-after)))
        (should (= (length lines-before) (length lines-after)))
        (let ((diff-count 0))
          (cl-loop for a in lines-before
                   for b in lines-after
                   unless (equal a b)
                   do (cl-incf diff-count))
          (should (= 1 diff-count)))))))

;;; --- 5. Sorted invariants ---

(ert-deftest supertag-canon-test-collection-lines-sorted-by-id ()
  "Entity lines within a collection appear in `string<' order of their id."
  (supertag-canon-test--with-temp-env
    (let ((store (supertag-canon-test--build-store '("n5" "n1" "n3" "n2" "n4")))
          (file (expand-file-name "s.el" supertag-data-directory)))
      (with-temp-buffer
        (supertag--persistence--write-canonical-store store (current-buffer))
        (write-region (point-min) (point-max) file nil 'silent))
      (let* ((lines (supertag-canon-test--file-lines file))
             (node-ids
              (cl-loop for line in lines
                       when (string-match "^(:collection :nodes :id \"\\([^\"]+\\)\"" line)
                       collect (match-string 1 line))))
        (should (equal node-ids '("n1" "n2" "n3" "n4" "n5")))))))

(ert-deftest supertag-canon-test-plist-keys-sorted-recursively ()
  "A sampled entity line's plist keys (top-level and nested) are sorted."
  (supertag-canon-test--with-temp-env
    (let ((store (supertag-canon-test--build-store '("n1")))
          (file (expand-file-name "s.el" supertag-data-directory)))
      (with-temp-buffer
        (supertag--persistence--write-canonical-store store (current-buffer))
        (write-region (point-min) (point-max) file nil 'silent))
      (let* ((lines (supertag-canon-test--file-lines file))
             (node-line (cl-find-if (lambda (l) (string-match-p ":collection :nodes" l)) lines))
             (form (car (read-from-string node-line)))
             (data (plist-get form :data)))
        ;; Top-level :data keys sorted: :file < :id < :meta < :tags < :title < :type
        (let (keys)
          (cl-loop for (k _v) on data by #'cddr do (push k keys))
          (setq keys (nreverse keys))
          (should (equal keys (sort (copy-sequence keys)
                                    (lambda (a b) (string< (format "%s" a) (format "%s" b)))))))
        ;; Nested :meta plist keys sorted too: :alpha < :zeta
        (let ((meta (plist-get data :meta)))
          (should (equal meta '(:alpha 2 :zeta 1))))))))

;;; --- 6. verify-after-save still works (regression guard) ---

(ert-deftest supertag-canon-test-verify-after-save-still-detects-mismatch ()
  "The verify-after-save step still catches a mismatch with the canonical writer."
  (supertag-canon-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (setq supertag--store (supertag-canon-test--build-store '("n1" "n2")))
    (should-error
     (cl-letf (((symbol-function 'supertag--count-nodes)
                (lambda () 999)))
       (supertag--persistence-write-store-atomically supertag-db-file)))
    (should (null (directory-files supertag-data-directory nil "\\.tmp")))))

(ert-deftest supertag-canon-test-atomic-save-then-verify-roundtrip-ok ()
  "A normal atomic save with verify-after-save enabled succeeds and reloads."
  (supertag-canon-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (setq supertag--store (supertag-canon-test--build-store '("n1" "n2" "n3")))
    (supertag--persistence-write-store-atomically supertag-db-file)
    (let ((loaded (supertag--persistence--try-read-store supertag-db-file)))
      (should (= 3 (hash-table-count (gethash :nodes loaded)))))))

;;; --- 7. Perf guard: canonical save vs plain prin1 dump, same run ---

(defconst supertag-canon-test-perf-node-count 5000
  "Node count for the perf-guard benchmark (5k, per the task's runtime note).")

(defun supertag-canon-test--build-perf-store ()
  "Return a synthetic, deterministic store with N nodes/tags/relations/fields.
Mirrors the shape (not the exact numbers) of test/perf-benchmark.el's
10k-node dataset generator, scaled down and self-contained so this test
file does not need to require the heavier view/index modules."
  (let* ((store (ht-create))
         (nodes (ht-create))
         (tags (ht-create))
         (relations (ht-create))
         (field-values (ht-create))
         (n supertag-canon-test-perf-node-count))
    (puthash :version "5.0.0" store)
    (dotimes (j 50)
      (let ((tag-id (format "tag%02d" j)))
        (puthash tag-id (list :id tag-id :type :tag :name tag-id :extends nil) tags)))
    (dotimes (i n)
      (let* ((id (format "node%05d" i))
             (tag-id (format "tag%02d" (mod i 50))))
        (puthash id
                 (list :id id :type :node
                       :title (format "Node %05d some title words here" i)
                       :tags (list tag-id) :file "/tmp/perf.org"
                       :level 1 :olp nil)
                 nodes)
        (when (< (mod i 10) 3)
          (let ((node-table (ht-create)))
            (puthash "Priority" (nth (mod i 3) '("Low" "Medium" "High")) node-table)
            (puthash "Effort" (1+ (mod i 10)) node-table)
            (puthash id node-table field-values)))))
    (dotimes (r 1000)
      (let* ((from-idx (mod (* r 7) n))
             (to-idx (mod (+ (* r 7) 1) n))
             (rel-id (format "rel%05d" r)))
        (puthash rel-id
                 (list :id rel-id :type :reference
                       :from (format "node%05d" from-idx)
                       :to (format "node%05d" to-idx))
                 relations)))
    (puthash :nodes nodes store)
    (puthash :tags tags store)
    (puthash :relations relations store)
    (puthash :field-values field-values store)
    store))

(defun supertag-canon-test--ensure-compiled ()
  "Byte-compile the S2 canonical serializer's hot-path functions in place.
`test/run-tests.sh' never byte-compiles library source before loading
it, and interpreted-vs-compiled ran roughly 4x apart for these functions
while developing the perf guard below — without this, the guard's
pass/fail would hinge on unrelated ambient state (whether a stray
`.elc' happens to already sit next to supertag-core-persistence.el
from an earlier manual compile) rather than on the actual cost of the
canonical format. A real end-user install runs compiled (`package.el'
byte-compiles on install), so forcing that state here also matches the
representative case rather than the worst one."
  (dolist (fn '(supertag--persistence--write-canonical-store
                supertag--persistence--canonicalize-value
                supertag--persistence--canonicalize-maybe-atom
                supertag--persistence--rebuild-sorted-plist
                supertag--persistence--freeze-hash-table
                supertag--persistence--plist-pairs-or-nil
                supertag--persistence--sort-key
                supertag--persistence--sort-pairs-by-key))
    (unless (byte-code-function-p (symbol-function fn))
      (byte-compile fn))))

(defmacro supertag-canon-test--measure (n &rest body)
  "Run BODY N times as already-BYTE-COMPILED code.
Return a list of per-run elapsed wall-clock seconds.

BODY is `byte-compile'd exactly ONCE into a zero-arg closure, then each
of the N runs is individually timed via `benchmark-run' around a plain
`funcall' of that closure — see test/perf-benchmark.el's
`supertag-perf-benchmark--measure' for the full rationale (this is the
same pattern, copied because this test file intentionally avoids
requiring the heavier perf-benchmark.el).

This indirection is NOT optional here: the canonicalizer measured ~4x
slower interpreted than byte-compiled in practice while
developing this guard, and test/run-tests.sh never byte-compiles source
files before loading them — so comparing an interpreted canonical writer
against the plain `prin1' PRIMITIVE (whose own speed does not depend on
the calling code's compilation state at all) would make this perf guard
pass or fail based on whether a stray `.elc' happened to be lying around,
not on the actual cost of the canonical format. Compiling both sides of
the comparison here makes the result reproducible regardless of that."
  (declare (indent 1))
  `(let ((supertag-canon-test--fn (byte-compile (lambda () ,@body)))
         times)
     (dotimes (_ ,n)
       (push (car (benchmark-run 1 (funcall supertag-canon-test--fn))) times))
     (nreverse times)))

(ert-deftest supertag-canon-test-perf-canonical-vs-plain-dump ()
  "Canonical save is < 2x the cost of a plain `prin1' dump, same run/machine."
  (supertag-canon-test--ensure-compiled)
  (supertag-canon-test--with-temp-env
    (let* ((store (supertag-canon-test--build-perf-store))
           (runs 5)
           (plain-times
            (supertag-canon-test--measure runs
              (with-temp-buffer
                (let ((print-escape-nonascii t)
                      (print-length nil)
                      (print-level nil)
                      (print-circle t))
                  (prin1 store (current-buffer))))))
           (canonical-times
            (supertag-canon-test--measure runs
              (with-temp-buffer
                (supertag--persistence--write-canonical-store store (current-buffer)))))
           (plain-min (apply #'min plain-times))
           (canonical-min (apply #'min canonical-times)))
      (message "canonical-serialization perf guard (N=%d nodes): plain-min=%.4fs canonical-min=%.4fs ratio=%.2fx"
               supertag-canon-test-perf-node-count plain-min canonical-min
               (if (> plain-min 0) (/ canonical-min plain-min) 0.0))
      ;; Small absolute slack alongside the 2x multiplier so this does not
      ;; flake when `plain-min' itself is only a few milliseconds.
      (should (<= canonical-min (+ (* 2.0 plain-min) 0.05))))))

(provide 'canonical-serialization-test)

;;; canonical-serialization-test.el ends here
