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

;;; --- 3.5. P1-8: honest breaking format upgrade (5.0.0 -> 6.0.0) ---
;;
;; Regression tests for the review finding that S2's original "old versions
;; can read the new format -- no version bump needed" claim was FALSE: a
;; pre-6.0 (<= 5.9.x) reader does exactly ONE `read' of the file and gets
;; only the FIRST top-level form -- against the canonical, line-per-entity
;; format that is the root scalar line, never any entity. See
;; `supertag-data-version''s docstring and
;; .phrase/phases/phase-git-sync-20260713/PLAN.md "S2 规范化序列化" (修订
;; 2026-07-13) for the full writeup this covers:
;;   1. the data version bump itself (so `supertag--maybe-auto-migrate' at
;;      least fires against a genuinely stale-versioned old database);
;;   2. the one-time `supertag-db-preformat6-*' downgrade snapshot, which
;;      additionally covers a database already stamped at the current
;;      version whose on-disk FORMAT is nonetheless still legacy;
;;   3. the `:supertag-format' / `:incompatible-notice' sentinel forced onto
;;      every canonical save's root scalar line;
;;   4. a from-source simulation of the actual pre-6.0 reader, proving it
;;      does NOT recover a valid-looking store from a canonical file.

(defun supertag-canon-test--old-reader-single-read (path)
  "Replicate org-supertag <= 5.9.x's `supertag--persistence--try-read-store'.
See commit c1224f5 in this project's git history (the last commit before
git-sync S2 introduced the canonical, line-per-entity format and the
format-sniffing/multi-form reader that goes with it): that function's
ENTIRE body was `(with-temp-buffer (insert-file-contents path) (goto-char
(point-min)) (let ((read-circle t)) (read (current-buffer))))' -- exactly
ONE `read' call, no format sniffing, no loop. Against a >= 6.0 canonical
file (one top-level sexp PER LINE, not one sexp for the whole store), that
single `read' returns only the FIRST line/form; every subsequent
`(:collection ...)' entity line is simply never reached by this call."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((read-circle t))
      (read (current-buffer)))))

(ert-deftest supertag-canon-test-header-embeds-data-version ()
  "The canonical format header's second (comment) line embeds the CURRENT
`supertag-data-version', not just the frozen format-generation number --
see `supertag--persistence-canonical-format-header'."
  (supertag-canon-test--with-temp-env
    (let ((store (supertag-canon-test--build-store '("n1")))
          (file (expand-file-name "s.el" supertag-data-directory)))
      (with-temp-buffer
        (supertag--persistence--write-canonical-store store (current-buffer))
        (write-region (point-min) (point-max) file nil 'silent))
      (let ((second-line (nth 1 (supertag-canon-test--file-lines file))))
        (should (string-match-p "canonical format" second-line))
        (should (string-match-p (regexp-quote supertag-data-version) second-line))))))

(ert-deftest supertag-canon-test-root-scalar-line-carries-format-sentinel ()
  "The root scalar line always carries `:supertag-format' and
`:incompatible-notice' (P1-8), forced in unconditionally by
`supertag--persistence--write-canonical-store' regardless of whether the
in-memory store itself ever set those keys -- so a pre-6.0 reader's single
`read' of this line at least has a chance of surfacing something
actionable if the raw form is ever inspected."
  (supertag-canon-test--with-temp-env
    (let ((store (supertag-canon-test--build-store '("n1")))
          (file (expand-file-name "s.el" supertag-data-directory)))
      (with-temp-buffer
        (supertag--persistence--write-canonical-store store (current-buffer))
        (write-region (point-min) (point-max) file nil 'silent))
      (let* ((lines (supertag-canon-test--file-lines file))
             ;; Header is exactly 2 lines; the root scalar line is next.
             (root-line (nth 2 lines))
             (form (car (read-from-string root-line))))
        (should (equal (plist-get form :supertag-format) 1))
        (should (stringp (plist-get form :incompatible-notice)))
        (should (> (length (plist-get form :incompatible-notice)) 0))
        ;; The store built by the fixture stamped :version "5.0.0" itself;
        ;; that value is NOT overridden (only the two sentinel keys are).
        (should (equal (plist-get form :version) "5.0.0"))))))

(ert-deftest supertag-canon-test-old-reader-does-not-recover-entities ()
  "A simulated pre-6.0 single-`read' load of a real, non-trivial >= 6.0
canonical file recovers ZERO nodes, even though the file has several --
the read only ever consumes the root scalar line, and coercing that
result (via `supertag--coerce-store-table', BYTE-FOR-BYTE UNCHANGED since
before git-sync S2 -- see the git history cited in
`supertag-canon-test--old-reader-single-read') never produces a usable
:nodes collection. This is the exact 'entities silently vanish' failure
mode the P1-8 version bump / snapshot / sentinel above cannot retroactively
fix in an already-released old build, only make loud where possible."
  (supertag-canon-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (setq supertag--store
          (supertag-canon-test--build-store
           '("n1" "n2" "n3" "n4" "n5" "n6" "n7" "n8" "n9" "n10")))
    (supertag--persistence-write-store-atomically supertag-db-file)
    (let* ((old-form (supertag-canon-test--old-reader-single-read supertag-db-file))
           (old-store (supertag--coerce-store-table old-form)))
      ;; The single `read' got a plist (the root scalar line), not a hash
      ;; table -- nothing here even LOOKS like the whole store.
      (should (listp old-form))
      (should (keywordp (car old-form)))
      (should (equal (plist-get old-form :version) "5.0.0"))
      ;; Coerced exactly the way the old build's `supertag-load-store' did,
      ;; this becomes a hash table with NO usable :nodes bucket -- i.e. a
      ;; store that LOOKS successfully loaded (no error) but has silently
      ;; lost all ten real nodes still sitting in the rest of the file.
      (should (hash-table-p old-store))
      (let ((nodes (gethash :nodes old-store)))
        (should (or (null nodes)
                    (not (hash-table-p nodes))
                    (= 0 (hash-table-count nodes))))))))

(ert-deftest supertag-canon-test-old-reader-result-refused-by-save-guard ()
  "If an old (<= 5.9.x) build adopted the empty-looking store from the
previous test as its live `supertag--store' and then tried to save, the
SAME \"Protective skip\" guard `supertag-save-store' has carried since
BEFORE git-sync S2 even existed (0 live nodes vs. a non-trivial on-disk
file) refuses the write outright -- the real, non-trivial canonical file
already on disk is left byte-for-byte untouched. This is the actual
mechanism that keeps the P1-8 scenario from being worse than 'entities
look gone in that one session': it can never silently escalate into 'and
then that session overwrote the real data', because this guard already
predates S2 and was never bypassed by anything in this change."
  (supertag-canon-test--with-temp-env
    (supertag-persistence-ensure-data-directory)
    (setq supertag--store
          (supertag-canon-test--build-store
           '("n1" "n2" "n3" "n4" "n5" "n6" "n7" "n8" "n9" "n10")))
    (supertag--persistence-write-store-atomically supertag-db-file)
    ;; Confirm the on-disk file really is "non-trivial" by the guard's own
    ;; > 1KB threshold before relying on that threshold below.
    (should (> (file-attribute-size (file-attributes supertag-db-file)) 1024))
    (let* ((old-form (supertag-canon-test--old-reader-single-read supertag-db-file))
           (old-store (supertag--coerce-store-table old-form))
           (before (supertag-canon-test--read-file-bytes supertag-db-file)))
      (setq supertag--store old-store)
      (supertag--record-store-origin :ok)
      (supertag-mark-dirty)
      ;; Neutralize the separate, unrelated "sync-state not loaded for
      ;; current vault" guard (this isolated test never loads the sync
      ;; module) -- same technique test/persistence-hardening-test.el's
      ;; auto-migrate test uses -- so the refusal observed below is
      ;; specifically attributable to the node-count "Protective skip"
      ;; guard, not incidentally to that other one.
      (cl-letf (((symbol-function 'supertag--persistence--expected-sync-state-file)
                 (lambda () nil)))
        (supertag-save-store))
      (should (equal before (supertag-canon-test--read-file-bytes supertag-db-file))))))

(ert-deftest supertag-canon-test-preformat6-snapshot-created-once ()
  "The FIRST canonical save over a legacy-format on-disk DB snapshots that
legacy file to backups/supertag-db-preformat6-*.el exactly once; a second
save (now over an already-canonical file) does not add a duplicate."
  (supertag-canon-test--with-temp-env
    (let ((legacy-store (ht-create))
          (nodes (ht-create)))
      (puthash "A" (list :id "A" :type :node :title "t" :file "/tmp/f") nodes)
      (puthash :nodes nodes legacy-store)
      (puthash :version supertag-data-version legacy-store)
      (supertag-canon-test--write-legacy-db supertag-db-file legacy-store))
    (supertag--record-store-origin :ok)
    (supertag-load-store)
    (should (= 1 (hash-table-count (supertag-store-get-collection :nodes))))
    (supertag-mark-dirty)
    (supertag--persistence-write-store-atomically supertag-db-file)
    (let ((snaps (directory-files supertag-db-backup-directory nil
                                   "\\`supertag-db-preformat6-.*\\.el\\'")))
      (should (= 1 (length snaps))))
    ;; Second save: the on-disk file is canonical now, so no further
    ;; snapshot is taken.
    (supertag-mark-dirty)
    (supertag--persistence-write-store-atomically supertag-db-file)
    (let ((snaps (directory-files supertag-db-backup-directory nil
                                   "\\`supertag-db-preformat6-.*\\.el\\'")))
      (should (= 1 (length snaps))))))

(ert-deftest supertag-canon-test-preformat6-snapshot-survives-daily-cleanup ()
  "A `preformat6' snapshot is never deleted by `supertag-cleanup-old-backups'
-- its filename deliberately does not match that function's daily-backup
date regex (see `supertag--persistence--snapshot-preformat6')."
  (supertag-canon-test--with-temp-env
    (let ((legacy-store (ht-create))
          (nodes (ht-create)))
      (puthash "A" (list :id "A" :type :node :title "t" :file "/tmp/f") nodes)
      (puthash :nodes nodes legacy-store)
      (puthash :version supertag-data-version legacy-store)
      (supertag-canon-test--write-legacy-db supertag-db-file legacy-store))
    (supertag--record-store-origin :ok)
    (supertag-load-store)
    (supertag-mark-dirty)
    (supertag--persistence-write-store-atomically supertag-db-file)
    (let* ((snaps (directory-files supertag-db-backup-directory t
                                    "\\`supertag-db-preformat6-.*\\.el\\'"))
           (snapshot (car snaps))
           (old-time (time-subtract (current-time) (days-to-time 30)))
           (supertag-db-backup-keep-days 3))
      (should (= 1 (length snaps)))
      ;; Backdate the snapshot so it WOULD be cleaned up if its name
      ;; matched the daily-backup regex (it deliberately does not).
      (set-file-times snapshot old-time)
      (supertag-cleanup-old-backups)
      (should (file-exists-p snapshot)))))

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
  "Canonical save stays within 8x of a plain `prin1' dump, same run/machine."
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
      ;; This is a catastrophic-regression guard (the bug it exists for
      ;; was a 14.5x sort implementation), not a benchmark. The ratio is
      ;; environment-sensitive: ~1.9x locally on Emacs 30+ native-comp,
      ;; 5.7x measured on the Emacs 28.2 CI runner (older byte-code
      ;; interpreter, shared hardware). Budget 8x + absolute slack keeps
      ;; headroom under the failure mode while tolerating slow runners.
      (should (<= canonical-min (+ (* 8.0 plain-min) 0.05))))))

(provide 'canonical-serialization-test)

;;; canonical-serialization-test.el ends here
