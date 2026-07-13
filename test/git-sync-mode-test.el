;;; git-sync-mode-test.el --- Tests for the S4 vault migration, clone, and auto-sync loop -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; S4 of .phrase/phases/phase-git-sync-20260713/PLAN.md ("S4 用户旅程" /
;; "S4 自动同步循环"). Covers the three additions this phase makes to
;; `supertag-git.el':
;;
;;   1. vault layout migration (`supertag-git--migrate-layout' /
;;      `supertag-git--do-migrate-layout', wired into `supertag-git-setup')
;;   2. `supertag-git-clone'
;;   3. `supertag-git-sync-mode' (the debounced commit / periodic pull /
;;      push-reject-retry / offline-degradation / org-text-conflict-guard
;;      automatic loop)
;;
;; Like test/git-integration-test.el, every test here drives a REAL `git'
;; binary against real temp-directory repositories (skipped via `ert-skip'
;; when `git' is not on `exec-path') -- this suite specifically is about
;; proving the wiring into real git plumbing, not re-testing the pure merge
;; core (already covered by test/merge-test.el) or the per-clone
;; `.gitattributes'/`.gitignore'/driver configuration (already covered by
;; test/git-integration-test.el).
;;
;; ## Batch-safety for `supertag-git-sync-mode'
;;
;; `supertag-git-sync-mode' normally runs all git operations
;; asynchronously (`make-process') and reacts to real timers/focus events.
;; Neither works deterministically in `ert-run-tests-batch-and-exit' (no
;; command loop pumping process sentinels, no real user focus events), so
;; every test below that touches the mode's machinery binds
;; `supertag-git-sync--synchronous' to t (documented on that variable in
;; supertag-git.el as existing exactly for this purpose) and fires timer
;; FUNCTIONS directly via `funcall'/direct call instead of waiting for
;; `run-with-timer' to elapse, per this task's explicit instructions.
;;
;; Run:
;;   ./test/run-tests.sh git
;;   emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/git-sync-mode-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ht)
(require 'cl-lib)

(defconst supertag-git-sync-test--repo-dir
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name default-directory)))
  "The org-supertag repository root, captured at load time -- see the
identical constant/rationale in test/git-integration-test.el.")

(add-to-list 'load-path supertag-git-sync-test--repo-dir)

(require 'supertag-core-persistence)
(require 'supertag-merge)
(require 'supertag-git)
(require 'supertag-doctor)
(require 'supertag-services-sync)

;; `supertag-git-sync-test-setup-remote-url-pushes' /
;; `-setup-empty-remote-url-is-local-only' below stub the interactive
;; `read-string' prompt via `cl-letf' (there being no other seam --
;; `supertag-git-setup--configure-remote' deliberately calls it directly,
;; exactly like a real interactive invocation would). `read-string' is a
;; primitive (subr); on a native-comp Emacs, redefining any subr's
;; function cell that gets called from natively-compiled Lisp requires an
;; on-the-fly "trampoline" to be native-compiled, which in turn depends on
;; a working local gcc/libgccjit toolchain -- broken or absent on some
;; machines (observed failure: "ld: library 'emutls_w' not found") for
;; reasons entirely unrelated to this test's own logic. Interpreted Lisp
;; (which is what `-l file.el' runs, as opposed to a `.eln'/`.elc') always
;; honors a `cl-letf'-redefined subr correctly regardless of this
;; variable -- disabling trampoline generation here only removes a native-
;; compilation optimization step this test suite does not need, never
;; correctness.
(if (boundp 'native-comp-enable-subr-trampolines)
    (setq native-comp-enable-subr-trampolines nil)
  (with-no-warnings (setq comp-enable-subr-trampolines nil)))

;; See test/git-integration-test.el's identical declaration + rationale:
;; `supertag-git.el' only ever reads this via `boundp', so a plain
;; top-level `defvar' here is what makes `let'-binding it below a genuine
;; dynamic binding rather than an ordinary lexical one under this file's
;; own `lexical-binding: t'. Harmless no-op given `supertag-services-sync.el'
;; (required above) already provides the real `defcustom' for it.
(defvar org-supertag-sync-directories nil)

;;; --- Small process/git helpers (same pattern as test/git-integration-test.el) ---

(defmacro supertag-git-sync-test--with-temp-dir (var &rest body)
  "Bind VAR to a fresh temp directory for BODY; delete it recursively
afterward (tolerating leftover lock files / merge-conflict state)."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "supertag-git-sync-test" t))))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defmacro supertag-git-sync-test--deftest (name docstring &rest body)
  "Like `ert-deftest', but NAME's BODY only runs when a `git' executable is
found on `exec-path'."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,name ()
     ,docstring
     (unless (executable-find "git")
       (ert-skip "git executable not found on exec-path"))
     ,@body))

(defun supertag-git-sync-test--git (dir &rest args)
  "Run `git -C DIR ARGS...'. Returns (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (let ((exit (apply #'call-process "git" nil t nil "-C" dir args)))
      (cons exit (buffer-string)))))

(defun supertag-git-sync-test--git! (dir &rest args)
  "Like `supertag-git-sync-test--git' but assert success via `should'.
Returns just the OUTPUT string."
  (let ((result (apply #'supertag-git-sync-test--git dir args)))
    (should (= 0 (car result)))
    (cdr result)))

(defun supertag-git-sync-test--init-bare (dir)
  "`git init --bare' at DIR, HEAD pre-pointed at `main'."
  (make-directory dir t)
  (supertag-git-sync-test--git! dir "init" "-q" "--bare")
  (supertag-git-sync-test--git! dir "symbolic-ref" "HEAD" "refs/heads/main"))

(defun supertag-git-sync-test--init-worktree (dir)
  "`git init' at DIR with a deterministic local identity/branch."
  (make-directory dir t)
  (supertag-git-sync-test--git! dir "init" "-q")
  (supertag-git-sync-test--git! dir "symbolic-ref" "HEAD" "refs/heads/main")
  (supertag-git-sync-test--git! dir "config" "user.name" "Supertag Test")
  (supertag-git-sync-test--git! dir "config" "user.email" "supertag-test@example.com")
  (supertag-git-sync-test--git! dir "config" "commit.gpgsign" "false"))

(defun supertag-git-sync-test--build-store (ids)
  "Return a store hash table with one minimal :node entity per id in IDS."
  (let ((store (ht-create)) (nodes (ht-create)))
    (puthash :version "5.0.0" store)
    (dolist (id ids)
      (puthash id (list :id id :type :node :title (format "Node %s" id)
                        :priority "low" :file "/tmp/f.org")
               nodes))
    (puthash :nodes nodes store)
    store))

(defun supertag-git-sync-test--write-store (store path)
  "Write STORE to PATH using the real S2 canonical writer."
  (let ((dir (file-name-directory path)))
    (unless (file-directory-p dir) (make-directory dir t)))
  (with-temp-buffer
    (set-buffer-file-coding-system 'utf-8-unix)
    (supertag--persistence--write-canonical-store store (current-buffer))
    (write-region (point-min) (point-max) path nil 'silent)))

(defun supertag-git-sync-test--read-store (path)
  "Read PATH back via the real loader."
  (supertag--persistence--try-read-store path))

(defun supertag-git-sync-test--edit-node (path id plist-edits)
  "Load the store at PATH, apply PLIST-EDITS to node ID, write it back."
  (let* ((store (supertag-git-sync-test--read-store path))
         (nodes (gethash :nodes store))
         (data (copy-sequence (gethash id nodes))))
    (cl-loop for (k v) on plist-edits by #'cddr
             do (setq data (plist-put data k v)))
    (puthash id data nodes)
    (supertag-git-sync-test--write-store store path)))

(defun supertag-git-sync-test--commit-edit (dir message)
  "`git add -A && git commit -q -m MESSAGE' in DIR."
  (supertag-git-sync-test--git! dir "add" "-A")
  (supertag-git-sync-test--git! dir "commit" "-q" "-m" message))

;;; ================================================================
;;; Part 1: vault layout migration
;;; ================================================================

(supertag-git-sync-test--deftest supertag-git-sync-test-migrate-layout-success
    "A database living outside any git repository, with a single
`org-supertag-sync-directories' root configured, is migrated into
`<root>/.supertag/supertag-db.el': the copy is verified loadable, this
session's wiring (`supertag-db-file' et al) is switched to the new
location, the live store is reloaded from it, and the OLD file is renamed
\(never deleted) to a `.migrated-<timestamp>' tombstone. A second
migration attempt is then a no-op (`:skipped-in-repo')."
  (supertag-git-sync-test--with-temp-dir dir
    (let* ((vault-root (file-name-as-directory (expand-file-name "vault" dir)))
           (outside-dir (file-name-as-directory (expand-file-name "outside-db" dir)))
           (outside-db (expand-file-name "supertag-db.el" outside-dir))
           (org-supertag-sync-directories (list vault-root))
           (supertag-data-directory outside-dir)
           (supertag-db-file outside-db)
           (supertag-db-backup-directory (expand-file-name "backups" outside-dir))
           (supertag--store nil)
           (supertag--store-origin nil))
      (make-directory vault-root t)
      (make-directory outside-dir t)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) outside-db)
      (let ((result (supertag-git--migrate-layout)))
        (should (eq :migrated (plist-get result :status)))
        (should (equal (plist-get result :new-db-file) supertag-db-file))
        (should (string-match-p "\\.supertag/supertag-db\\.el\\'" supertag-db-file))
        (should (file-exists-p supertag-db-file))
        ;; Old file gone from its original path -- renamed, not deleted.
        (should-not (file-exists-p outside-db))
        (should (plist-get result :tombstone))
        (should (file-exists-p (plist-get result :tombstone)))
        (should (string-match-p "\\.migrated-[0-9]+\\'" (plist-get result :tombstone)))
        ;; Wiring switched AND the live store reloaded from the new location
        ;; -- without any manual `supertag-load-store' call by this test.
        (should (hash-table-p supertag--store))
        (should (gethash "n1" (gethash :nodes supertag--store))))
      ;; Idempotent: db-file now lives inside the repo, so a second run
      ;; just reports the already-migrated state and does nothing further.
      (let ((second (supertag-git--migrate-layout)))
        (should (eq :skipped-in-repo (plist-get second :status)))
        (should-not (file-exists-p outside-db))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-migrate-layout-failure-untouched
    "When the migration target cannot be written to (simulated here via an
unwritable `.supertag/' directory), migration reports `:failed' and the
OLD database file is left completely byte-for-byte untouched -- no
wiring change, no tombstone rename."
  (when (= 0 (user-uid))
    (ert-skip "running as root: filesystem permission checks do not apply"))
  (supertag-git-sync-test--with-temp-dir dir
    (let* ((vault-root (file-name-as-directory (expand-file-name "vault" dir)))
           (outside-dir (file-name-as-directory (expand-file-name "outside-db" dir)))
           (outside-db (expand-file-name "supertag-db.el" outside-dir))
           (org-supertag-sync-directories (list vault-root))
           (supertag-data-directory outside-dir)
           (supertag-db-file outside-db)
           (supertag-db-backup-directory (expand-file-name "backups" outside-dir))
           (supertag--store nil)
           (supertag--store-origin nil)
           (target-dir (expand-file-name ".supertag/" vault-root))
           original-bytes)
      (make-directory vault-root t)
      (make-directory outside-dir t)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) outside-db)
      (setq original-bytes (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally outside-db)
                             (buffer-string)))
      (make-directory target-dir t)
      (set-file-modes target-dir #o555) ; read+execute only: copy-file into it must fail
      (unwind-protect
          (let ((result (supertag-git--migrate-layout)))
            (should (eq :failed (plist-get result :status)))
            (should (stringp (plist-get result :error))))
        (set-file-modes target-dir #o755)) ; restore so temp-dir cleanup can delete it
      ;; Old file: still there, byte-for-byte identical, DB wiring unchanged.
      (should (equal supertag-db-file outside-db))
      (should (file-exists-p outside-db))
      (let ((after-bytes (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally outside-db)
                           (buffer-string))))
        (should (equal original-bytes after-bytes)))
      (should-not (directory-files outside-dir nil "\\.migrated-")))))

;;; ----------------------------------------------------------------
;;; Part 1b (P0-2 regression): migration must not lose unsaved
;;; in-memory changes -- see `supertag-git--do-migrate-layout''s
;;; Commentary in supertag-git.el for the fixed flush/write/verify
;;; sequence this pins down.
;;; ----------------------------------------------------------------

(supertag-git-sync-test--deftest supertag-git-sync-test-migrate-layout-preserves-dirty-changes
    "An UNSAVED in-memory edit (`supertag-dirty-p' true) made after loading
the store must survive migration -- the migrated database must reflect
the CURRENT in-memory value, not whatever is still on disk. This pins
down BOTH halves of the P0-2 fix: (a) the dirty flush to the OLD file via
the real `supertag-save-store' path actually runs and clears the flag,
and (b) the NEW file is written directly from the in-memory store via
`supertag--persistence-write-store-atomically' (never a stale `copy-file'
of whatever the old file happened to contain a moment before)."
  (supertag-git-sync-test--with-temp-dir dir
    (let* ((vault-root (file-name-as-directory (expand-file-name "vault" dir)))
           (outside-dir (file-name-as-directory (expand-file-name "outside-db" dir)))
           (outside-db (expand-file-name "supertag-db.el" outside-dir))
           (org-supertag-sync-directories (list vault-root))
           (supertag-data-directory outside-dir)
           (supertag-db-file outside-db)
           (supertag-db-backup-directory (expand-file-name "backups" outside-dir))
           (supertag--store nil)
           (supertag--store-origin nil))
      (make-directory vault-root t)
      (make-directory outside-dir t)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) outside-db)
      ;; Load the store into memory, then mutate a title WITHOUT saving --
      ;; this is the "unsaved in-memory change" the migration must not lose.
      (supertag-load-store)
      (should-not (supertag-dirty-p))
      (let* ((nodes (gethash :nodes supertag--store))
             (data (copy-sequence (gethash "n1" nodes))))
        (puthash "n1" (plist-put data :title "mutated in memory") nodes))
      (supertag-mark-dirty)
      (should (supertag-dirty-p))
      ;; Neutralize the unrelated sync-state guard (same trick used by
      ;; `test/persistence-hardening-test.el''s auto-migrate test) so the
      ;; internal flush this migration performs actually succeeds --
      ;; without this, `supertag--persistence-guard-violations' always
      ;; reports "sync-state not loaded for current vault" in this
      ;; standalone test environment, which is exactly the OTHER test
      ;; below's scenario, not this one's.
      (cl-letf (((symbol-function 'supertag--persistence--expected-sync-state-file)
                 (lambda () nil)))
        (let ((result (supertag-git--migrate-layout)))
          (should (eq :migrated (plist-get result :status)))
          ;; The flush succeeded: dirty is cleared, not carried over
          ;; silently unsaved.
          (should-not (supertag-dirty-p))
          (let* ((on-disk (supertag--persistence--try-read-store
                           (plist-get result :new-db-file)))
                 (node (gethash "n1" (gethash :nodes on-disk))))
            (should (equal "mutated in memory" (plist-get node :title))))
          (should (equal "mutated in memory"
                         (plist-get (gethash "n1" (gethash :nodes supertag--store))
                                    :title))))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-migrate-layout-aborts-when-save-guard-blocks
    "When the in-memory store is dirty AND the safety-net flush cannot
actually save (a real guard violation -- `supertag-sync--state-source' is
explicitly nil here, reusing the same sync-state trick
`test/persistence-hardening-test.el' relies on, just for the OPPOSITE
purpose: forcing the guard to fire rather than neutralizing it),
migration must ABORT outright rather than proceed to write a target from
a state it could not actually confirm was persisted: the old file is left
byte-for-byte untouched, no `.supertag/' target database is created, this
session's wiring is unchanged, and the failure names the guard reason."
  (supertag-git-sync-test--with-temp-dir dir
    (let* ((vault-root (file-name-as-directory (expand-file-name "vault" dir)))
           (outside-dir (file-name-as-directory (expand-file-name "outside-db" dir)))
           (outside-db (expand-file-name "supertag-db.el" outside-dir))
           (org-supertag-sync-directories (list vault-root))
           (supertag-data-directory outside-dir)
           (supertag-db-file outside-db)
           (supertag-db-backup-directory (expand-file-name "backups" outside-dir))
           (supertag--store nil)
           (supertag--store-origin nil)
           (supertag-sync--state-source nil)
           original-bytes)
      (make-directory vault-root t)
      (make-directory outside-dir t)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) outside-db)
      (supertag-load-store)
      (let* ((nodes (gethash :nodes supertag--store))
             (data (copy-sequence (gethash "n1" nodes))))
        (puthash "n1" (plist-put data :title "mutated in memory") nodes))
      (supertag-mark-dirty)
      (setq original-bytes (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally outside-db)
                             (buffer-string)))
      (let ((result (supertag-git--migrate-layout)))
        (should (eq :failed (plist-get result :status)))
        (should (stringp (plist-get result :error)))
        (should (string-match-p "sync-state not loaded" (plist-get result :error))))
      ;; The dirty flag is never cleared -- the save really was skipped,
      ;; not silently treated as having succeeded.
      (should (supertag-dirty-p))
      ;; Old file byte-for-byte untouched.
      (let ((after-bytes (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally outside-db)
                           (buffer-string))))
        (should (equal original-bytes after-bytes)))
      ;; No target database created, no tombstone, wiring unchanged.
      (should-not (file-exists-p (expand-file-name ".supertag/supertag-db.el" vault-root)))
      (should-not (directory-files outside-dir nil "\\.migrated-"))
      (should (equal supertag-db-file outside-db)))))

(supertag-git-sync-test--deftest supertag-git-sync-test-migrate-layout-refuses-unsafe-origin
    "Migration must not legitimize an untrusted in-memory store.
Even when the placeholder store is clean, missing, failed, and mismatched
origins must fail the existing persistence guard before the direct writer
creates a target or tombstones the only real database."
  (supertag-git-sync-test--with-temp-dir dir
    (let* ((vault-root (file-name-as-directory (expand-file-name "vault" dir)))
           (outside-dir (file-name-as-directory (expand-file-name "outside-db" dir)))
           (outside-db (expand-file-name "supertag-db.el" outside-dir))
           (target-db (expand-file-name ".supertag/supertag-db.el" vault-root))
           (org-supertag-sync-directories (list vault-root))
           (supertag-data-directory outside-dir)
           (supertag-db-file outside-db)
           (supertag-db-backup-directory (expand-file-name "backups" outside-dir))
           (supertag--store (supertag-git-sync-test--build-store '("placeholder")))
           (supertag--store-origin nil)
           original-bytes)
      (make-directory vault-root t)
      (make-directory outside-dir t)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("real-node")) outside-db)
      (setq original-bytes
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally outside-db)
              (buffer-string)))
      (cl-letf (((symbol-function 'supertag--persistence--expected-sync-state-file)
                 (lambda () nil)))
        (dolist (scenario
                 (list (cons (list :status :failed :db-file outside-db)
                             "last load status :failed")
                       (cons nil "store origin missing")
                       (cons (list :status :ok
                                   :db-file (expand-file-name "another-db.el" outside-dir))
                             "db-file mismatch")))
          (setq supertag--store-origin (car scenario))
          (let ((result (supertag-git--migrate-layout)))
            (should (eq :failed (plist-get result :status)))
            (should (string-match-p (regexp-quote (cdr scenario))
                                    (plist-get result :error))))
          (should (file-exists-p outside-db))
          (should-not (file-exists-p target-db))
          (should-not (directory-files outside-dir nil "\\.migrated-"))))
      (should (equal outside-db supertag-db-file))
      (should (equal original-bytes
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally outside-db)
                       (buffer-string)))))))

;;; ================================================================
;;; Part 2: supertag-git-clone
;;; ================================================================

(supertag-git-sync-test--deftest supertag-git-sync-test-clone-loads-existing-db
    "Cloning a remote whose vault already has a valid `.supertag/supertag-db.el'
loads it directly (no rebuild) and reports matching node/tag counts."
  (supertag-git-sync-test--with-temp-dir dir
    (let* ((bare (expand-file-name "bare.git" dir))
           (seed (file-name-as-directory (expand-file-name "seed" dir)))
           (seed-data-dir (expand-file-name ".supertag/" seed))
           (seed-db (expand-file-name "supertag-db.el" seed-data-dir)))
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--init-worktree seed)
      (make-directory seed-data-dir t)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1" "n2")) seed-db)
      (supertag-git--configure-clone seed seed-db)
      (supertag-git-sync-test--git! seed "add" "-A")
      (supertag-git-sync-test--git! seed "commit" "-q" "-m" "seed")
      (supertag-git-sync-test--git! seed "remote" "add" "origin" bare)
      (supertag-git-sync-test--git! seed "push" "-q" "origin" "main")
      (let* ((clone-dir (expand-file-name "clone" dir))
             (org-supertag-sync-directories nil)
             (supertag-data-directory (expand-file-name "unused/" dir))
             (supertag-db-file (expand-file-name "unused/supertag-db.el" dir))
             (supertag-db-backup-directory (expand-file-name "unused/backups" dir))
             (supertag--store nil)
             (supertag--store-origin nil))
        (let ((result (supertag-git-clone bare clone-dir)))
          (should (plist-get result :loaded))
          (should-not (plist-get result :rebuilt))
          (should (= 2 (plist-get result :node-count)))
          (should (file-exists-p (expand-file-name ".supertag/supertag-db.el" clone-dir)))
          (should (equal supertag-db-file (expand-file-name ".supertag/supertag-db.el" clone-dir))))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-clone-corrupt-db-rebuilds
    "Cloning a remote whose committed `.supertag/supertag-db.el' is corrupt
\(not a readable store) takes the rebuild path: the existing sync scanner
\(`supertag-sync-full-rescan') rebuilds nodes from the cloned org files."
  (supertag-git-sync-test--with-temp-dir dir
    (let* ((bare (expand-file-name "bare.git" dir))
           (seed (file-name-as-directory (expand-file-name "seed" dir)))
           (seed-data-dir (expand-file-name ".supertag/" seed))
           (seed-db (expand-file-name "supertag-db.el" seed-data-dir))
           (org-file (expand-file-name "notes.org" seed)))
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--init-worktree seed)
      (make-directory seed-data-dir t)
      (with-temp-file seed-db (insert "this is not a valid supertag store\n"))
      (with-temp-file org-file
        (insert "* Node One\n:PROPERTIES:\n:ID: 22222222-2222-2222-2222-222222222222\n:END:\nSome body text.\n"))
      (supertag-git--configure-clone seed seed-db)
      (supertag-git-sync-test--git! seed "add" "-A")
      (supertag-git-sync-test--git! seed "commit" "-q" "-m" "seed with corrupt db")
      (supertag-git-sync-test--git! seed "remote" "add" "origin" bare)
      (supertag-git-sync-test--git! seed "push" "-q" "origin" "main")
      (let* ((clone-dir (file-name-as-directory (expand-file-name "clone" dir)))
             (org-supertag-sync-directories (list clone-dir))
             (org-supertag-sync-directories-mode 'unified)
             (supertag-data-directory (expand-file-name "unused/" dir))
             (supertag-db-file (expand-file-name "unused/supertag-db.el" dir))
             (supertag-db-backup-directory (expand-file-name "unused/backups" dir))
             (supertag--store nil)
             (supertag--store-origin nil)
             (supertag-sync--state-source nil))
        (let ((result (supertag-git-clone bare clone-dir)))
          (should-not (plist-get result :loaded))
          (should (plist-get result :rebuilt))
          (should (>= (plist-get result :node-count) 1)))))))

;;; ================================================================
;;; Part 2b: supertag-git-setup completes the machine-1 journey --
;;; remote configuration, initial commit, push (P1-6)
;;; ================================================================

(supertag-git-sync-test--deftest supertag-git-sync-test-setup-remote-url-pushes
    "`supertag-git-setup', given a remote URL from the (stubbed) prompt,
configures `origin`, creates the vault's initial commit through the
EXACT SAME scoped-staging pathspec logic `supertag-git-sync-mode' uses
for its own auto-commits (`supertag-git-setup--commit-all' /
`supertag-git-sync--commit-pathspecs'), and pushes it: the remote's HEAD
ends up identical to the local HEAD, the pushed tree contains
`.gitattributes' and the database, and an untracked junk file sitting in
the vault is NEVER swept into that commit."
  (supertag-git-sync-test--with-temp-dir dir
    (let* ((bare (expand-file-name "bare.git" dir))
           (root (file-name-as-directory (expand-file-name "vault" dir)))
           (db (expand-file-name "supertag-db.el" root))
           (org-file (expand-file-name "notes.org" root))
           (junk-file (expand-file-name "junk.txt" root)))
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--init-worktree root)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db)
      (with-temp-file org-file (insert "* Heading\nBody.\n"))
      (with-temp-file junk-file (insert "not tracked -- must never be committed\n"))
      (let ((supertag-db-file db)
            (supertag-data-directory root)
            (org-supertag-sync-directories nil))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) bare)))
          (supertag-git-setup)))
      ;; `origin' configured to the bare remote.
      (should (equal bare (string-trim
                           (supertag-git-sync-test--git! root "config" "--get" "remote.origin.url"))))
      ;; Local HEAD matches remote HEAD -- the push actually landed.
      (let ((local-head (string-trim (supertag-git-sync-test--git! root "rev-parse" "HEAD")))
            (remote-head (string-trim (supertag-git-sync-test--git! bare "rev-parse" "HEAD"))))
        (should (equal local-head remote-head)))
      ;; Tracking configured (`push -u`): the branch has an upstream now.
      (should (supertag-git--ok-p
               (supertag-git-sync-test--git root "rev-parse" "--abbrev-ref" "@{upstream}")))
      ;; Pushed tree includes .gitattributes, the database, and the org
      ;; file -- but NOT the untracked junk file.
      (let ((tree (supertag-git-sync-test--git! bare "ls-tree" "-r" "--name-only" "HEAD")))
        (should (string-match-p "\\.gitattributes" tree))
        (should (string-match-p "supertag-db\\.el" tree))
        (should (string-match-p "notes\\.org" tree))
        (should-not (string-match-p "junk\\.txt" tree))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-setup-empty-remote-url-is-local-only
    "`supertag-git-setup', given an empty string from the (stubbed)
remote-URL prompt, still configures the merge driver/.gitattributes/
.gitignore as normal, but does NOT create a remote, an initial commit, or
attempt any push -- an explicit, error-free, local-only vault."
  (supertag-git-sync-test--with-temp-dir dir
    (let* ((root (file-name-as-directory (expand-file-name "vault" dir)))
           (db (expand-file-name "supertag-db.el" root))
           (org-file (expand-file-name "notes.org" root)))
      (supertag-git-sync-test--init-worktree root)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db)
      (with-temp-file org-file (insert "* Heading\nBody.\n"))
      (let ((supertag-db-file db)
            (supertag-data-directory root)
            (org-supertag-sync-directories nil))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
          (supertag-git-setup)))
      ;; No origin configured.
      (should-not (supertag-git--ok-p
                   (supertag-git-sync-test--git root "config" "--get" "remote.origin.url")))
      ;; No commit was created -- HEAD is still unborn.
      (should-not (supertag-git--ok-p (supertag-git-sync-test--git root "rev-parse" "HEAD")))
      ;; The driver/.gitattributes/.gitignore steps still ran normally,
      ;; unaffected by skipping the remote step.
      (should (equal "supertag-db.el merge=supertag-db"
                     (string-trim
                      (with-temp-buffer
                        (insert-file-contents (expand-file-name ".gitattributes" root))
                        (buffer-string))))))))

;;; ================================================================
;;; Part 3: supertag-git-sync-mode
;;; ================================================================

(supertag-git-sync-test--deftest supertag-git-sync-test-commit-debounce-single-commit
    "Two rapid changes each call `supertag-git-sync--schedule-commit'; only
ONE debounce timer is ever pending (the second call cancels/replaces the
first). Firing the debounce function directly produces exactly ONE new
commit; firing it again with nothing new staged produces NO empty commit."
  (supertag-git-sync-test--with-temp-dir root
    (supertag-git-sync-test--init-worktree root)
    (let* ((db (expand-file-name "supertag-db.el" root))
           (supertag-git-sync--synchronous t)
           (supertag-git-sync--vault-root root)
           (supertag-git-sync--in-flight nil)
           (supertag-git-sync--pending-push-count 0)
           (supertag-git-sync--offline-warned nil)
           (supertag-git-sync--conflict-commit-warned nil)
           (supertag-git-sync--commit-timer nil)
           ;; `supertag-git-sync--commit-pathspecs' (P0-3) needs
           ;; `supertag-db-file' pointed at this vault's database to stage
           ;; it -- exactly what a real `supertag-git-sync-mode' session
           ;; already has by the time it is enabled.
           (supertag-db-file db)
           (supertag-data-directory root))
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db)
      (supertag-git--configure-clone root db)
      (supertag-git-sync-test--git! root "add" "-A")
      (supertag-git-sync-test--git! root "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--edit-node db "n1" '(:title "edited once"))
      (supertag-git-sync--schedule-commit)
      (let ((first-timer supertag-git-sync--commit-timer))
        (should first-timer)
        (supertag-git-sync-test--edit-node db "n1" '(:title "edited twice"))
        (supertag-git-sync--schedule-commit)
        ;; Debounce restarted: still exactly one pending timer, and it is a
        ;; NEW one (the old one was cancelled, not left running alongside).
        (should supertag-git-sync--commit-timer)
        (should-not (eq first-timer supertag-git-sync--commit-timer)))
      ;; Batch-safe: cancel the real timer and fire its function directly.
      (cancel-timer supertag-git-sync--commit-timer)
      (setq supertag-git-sync--commit-timer nil)
      (supertag-git-sync--fire-commit)
      (should-not supertag-git-sync--in-flight)
      (let ((log (supertag-git-sync-test--git! root "log" "--oneline")))
        (should (= 2 (length (split-string (string-trim log) "\n" t)))))
      ;; Nothing staged now -- firing again must not create an empty commit.
      (supertag-git-sync--fire-commit)
      (let ((log (supertag-git-sync-test--git! root "log" "--oneline")))
        (should (= 2 (length (split-string (string-trim log) "\n" t))))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-push-reject-retry-succeeds
    "B's push is rejected because the remote advanced (A pushed first);
`supertag-git-sync--push' fetches, merges, and retries the push exactly
once, succeeding -- both A's and B's edits end up present, and the
pending-push counter clears."
  (supertag-git-sync-test--with-temp-dir root
    (let* ((bare (expand-file-name "bare.git" root))
           (a (file-name-as-directory (expand-file-name "a" root)))
           (b (file-name-as-directory (expand-file-name "b" root)))
           (db-a (expand-file-name "supertag-db.el" a))
           (db-b (expand-file-name "supertag-db.el" b)))
      (supertag-git-sync-test--init-worktree a)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1" "n2")) db-a)
      (supertag-git--configure-clone a db-a)
      (supertag-git-sync-test--git! a "add" "-A")
      (supertag-git-sync-test--git! a "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--git! a "remote" "add" "origin" bare)
      (supertag-git-sync-test--git! a "push" "-q" "origin" "main")
      (supertag-git-sync-test--git! root "clone" "-q" bare b)
      (supertag-git-sync-test--git! b "config" "user.name" "Supertag Test")
      (supertag-git-sync-test--git! b "config" "user.email" "supertag-test@example.com")
      (supertag-git-sync-test--git! b "config" "commit.gpgsign" "false")
      (supertag-git--configure-clone b db-b)
      ;; A advances the remote first.
      (supertag-git-sync-test--edit-node db-a "n1" '(:title "A advanced remote"))
      (supertag-git-sync-test--commit-edit a "A advances remote")
      (supertag-git-sync-test--git! a "push" "-q" "origin" "main")
      ;; B commits its own, disjoint edit locally (never pushed yet) --
      ;; B's push below will therefore be rejected as non-fast-forward.
      (supertag-git-sync-test--edit-node db-b "n2" '(:title "B local edit"))
      (supertag-git-sync-test--commit-edit b "B commits locally")
      (let ((supertag-git-sync--synchronous t)
            (supertag-git-sync--in-flight t) ; as `--push' expects (caller already set it)
            (supertag-git-sync--pending-push-count 1)
            (supertag-git-sync--offline-warned nil)
            ;; `--push''s retry path runs `supertag-git-sync--after-merge',
            ;; which reads `supertag-db-file' -- bind it to B's OWN db so
            ;; this never touches (or even reads) a real user database.
            (supertag-db-file db-b)
            (supertag-data-directory b))
        (supertag-git-sync--push b)
        (should (= 0 supertag-git-sync--pending-push-count))
        (should-not supertag-git-sync--in-flight))
      (let ((store (supertag-git-sync-test--read-store db-b)))
        (should (equal "A advanced remote"
                       (plist-get (gethash "n1" (gethash :nodes store)) :title)))
        (should (equal "B local edit"
                       (plist-get (gethash "n2" (gethash :nodes store)) :title))))
      ;; B's retried push actually landed on the remote.
      (let ((remote-log (supertag-git-sync-test--git! bare "log" "--oneline")))
        (should (>= (length (split-string (string-trim remote-log) "\n" t)) 3))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-pull-merge-reloads-store
    "A pulls-and-merges a remote DB change on B; `supertag-git-sync--pull'
reloads the live in-memory store from the merged file itself -- the test
never calls `supertag-load-store' manually."
  (supertag-git-sync-test--with-temp-dir root
    (let* ((bare (expand-file-name "bare.git" root))
           (a (file-name-as-directory (expand-file-name "a" root)))
           (b (file-name-as-directory (expand-file-name "b" root)))
           (db-a (expand-file-name "supertag-db.el" a))
           (db-b (expand-file-name "supertag-db.el" b)))
      (supertag-git-sync-test--init-worktree a)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db-a)
      (supertag-git--configure-clone a db-a)
      (supertag-git-sync-test--git! a "add" "-A")
      (supertag-git-sync-test--git! a "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--git! a "remote" "add" "origin" bare)
      (supertag-git-sync-test--git! a "push" "-q" "origin" "main")
      (supertag-git-sync-test--git! root "clone" "-q" bare b)
      (supertag-git-sync-test--git! b "config" "user.name" "Supertag Test")
      (supertag-git-sync-test--git! b "config" "user.email" "supertag-test@example.com")
      (supertag-git-sync-test--git! b "config" "commit.gpgsign" "false")
      (supertag-git--configure-clone b db-b)
      (supertag-git-sync-test--edit-node db-a "n1" '(:title "changed by A"))
      (supertag-git-sync-test--commit-edit a "A changes n1")
      (supertag-git-sync-test--git! a "push" "-q" "origin" "main")
      (let* ((supertag-git-sync--synchronous t)
             (supertag-git-sync--vault-root b)
             (supertag-git-sync--in-flight nil)
             (supertag-git-sync--offline-warned nil)
             (supertag-db-file db-b)
             (supertag-data-directory b)
             ;; Stale, pre-pull in-memory snapshot -- must be overwritten by
             ;; the merge's own reload, not by anything this test calls.
             (supertag--store (supertag-git-sync-test--build-store '("n1")))
             (supertag--store-origin nil))
        (supertag-git-sync--pull)
        (should-not supertag-git-sync--in-flight)
        (should (equal "changed by A"
                       (plist-get (gethash "n1" (gethash :nodes supertag--store)) :title)))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-conflicted-org-file-guarded
    "A and B both edit the SAME line of the same org file differently
\(no merge driver applies to plain `.org' text, so git's default merge
leaves literal conflict markers). After B pulls+merges,
`supertag-git-sync--conflicted-org-files' names that file, the
sync-scanner-skipping advice refuses to import it (while still allowing
an unrelated file through), and `supertag-doctor' lists it."
  (supertag-git-sync-test--with-temp-dir root
    (let* ((bare (expand-file-name "bare.git" root))
           (a (file-name-as-directory (expand-file-name "a" root)))
           (b (file-name-as-directory (expand-file-name "b" root)))
           (db-a (expand-file-name "supertag-db.el" a))
           (db-b (expand-file-name "supertag-db.el" b))
           (org-rel "notes.org"))
      (supertag-git-sync-test--init-worktree a)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db-a)
      (with-temp-file (expand-file-name org-rel a) (insert "* Heading\nOriginal line.\n"))
      (supertag-git--configure-clone a db-a)
      (supertag-git-sync-test--git! a "add" "-A")
      (supertag-git-sync-test--git! a "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--git! a "remote" "add" "origin" bare)
      (supertag-git-sync-test--git! a "push" "-q" "origin" "main")
      (supertag-git-sync-test--git! root "clone" "-q" bare b)
      (supertag-git-sync-test--git! b "config" "user.name" "Supertag Test")
      (supertag-git-sync-test--git! b "config" "user.email" "supertag-test@example.com")
      (supertag-git-sync-test--git! b "config" "commit.gpgsign" "false")
      (supertag-git--configure-clone b db-b)
      (with-temp-file (expand-file-name org-rel a) (insert "* Heading\nA's version.\n"))
      (supertag-git-sync-test--commit-edit a "A edits notes.org")
      (supertag-git-sync-test--git! a "push" "-q" "origin" "main")
      (with-temp-file (expand-file-name org-rel b) (insert "* Heading\nB's version.\n"))
      (supertag-git-sync-test--commit-edit b "B edits notes.org")
      (let* ((supertag-git-sync--synchronous t)
             (supertag-git-sync--vault-root b)
             (supertag-git-sync--in-flight nil)
             (supertag-git-sync--offline-warned nil)
             (supertag-git-sync--conflicted-org-files nil)
             (supertag-db-file db-b)
             (supertag-data-directory b)
             (supertag--store (supertag-git-sync-test--build-store '("n1")))
             (supertag--store-origin nil)
             (conflicted-abs (expand-file-name org-rel b)))
        (supertag-git-sync--pull)
        (should (member conflicted-abs supertag-git-sync--conflicted-org-files))
        ;; The advice refuses the conflicted file but still lets an
        ;; unrelated one through to the real import function.
        (let ((imported nil))
          (cl-letf (((symbol-function 'dummy-orig-fn)
                     (lambda (file &rest _) (setq imported file))))
            (funcall #'supertag-git-sync--skip-conflicted-file-advice
                     #'dummy-orig-fn conflicted-abs)
            (should-not imported)
            (funcall #'supertag-git-sync--skip-conflicted-file-advice
                     #'dummy-orig-fn "/some/other/unrelated.org")
            (should (equal imported "/some/other/unrelated.org"))))
        ;; Doctor lists it.
        (let ((supertag-git-sync-mode t)
              (buf (supertag-doctor t)))
          (with-current-buffer buf
            (should (string-match-p (regexp-quote conflicted-abs) (buffer-string)))))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-modify-delete-org-conflict-guarded
    "An unmerged Org file is unsafe even when Git emits no text markers.
A real modify/delete conflict must enter the live conflict set, scanner
blacklist, and doctor report until the index conflict is resolved."
  (supertag-git-sync-test--with-temp-dir root
    (let* ((db (expand-file-name "supertag-db.el" root))
           (org-rel "notes.org")
           (org-file (expand-file-name org-rel root)))
      (supertag-git-sync-test--init-worktree root)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db)
      (with-temp-file org-file (insert "* Original\n"))
      (supertag-git--configure-clone root db)
      (supertag-git-sync-test--git! root "add" "-A")
      (supertag-git-sync-test--git! root "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--git! root "checkout" "-q" "-b" "delete-side")
      (supertag-git-sync-test--git! root "rm" "-q" "--" org-rel)
      (supertag-git-sync-test--git! root "commit" "-q" "-m" "delete notes")
      (supertag-git-sync-test--git! root "checkout" "-q" "main")
      (with-temp-file org-file (insert "* Modified on main\n"))
      (supertag-git-sync-test--commit-edit root "modify notes")
      (let ((merge-result (supertag-git-sync-test--git root "merge" "--no-edit" "delete-side")))
        (should-not (= 0 (car merge-result)))
        (should (equal (list org-rel) (supertag-git-sync--unmerged-paths root)))
        (should-not (supertag-git-sync--file-has-conflict-markers-p org-file))
        (let ((supertag-db-file db)
              (supertag-data-directory root)
              (supertag-git-sync--vault-root root)
              (supertag-git-sync--conflicted-org-files nil))
          (cl-letf (((symbol-function 'supertag-sync-check-now) #'ignore))
            (supertag-git-sync--after-merge merge-result root))
          (should (equal (list org-file) supertag-git-sync--conflicted-org-files))
          (let ((imported nil))
            (cl-letf (((symbol-function 'dummy-orig-fn)
                       (lambda (&rest _) (setq imported t))))
              (supertag-git-sync--skip-conflicted-file-advice
               #'dummy-orig-fn org-file)
              (should-not imported)))
          (let ((buf (supertag-doctor t)))
            (with-current-buffer buf
              (should (string-match-p (regexp-quote org-file) (buffer-string))))))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-doctor-cold-loads-git-diagnostics
    "Loading doctor alone must still inspect Git state on first use.
The Git module is loaded lazily by the report section, and an unresolved
Org conflict already on disk is visible without enabling sync mode."
  (supertag-git-sync-test--with-temp-dir root
    (let* ((db (expand-file-name "supertag-db.el" root))
           (org-file (expand-file-name "notes.org" root)))
      (supertag-git-sync-test--init-worktree root)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db)
      (with-temp-file org-file (insert "* Original\n"))
      (supertag-git--configure-clone root db)
      (supertag-git-sync-test--git! root "add" "-A")
      (supertag-git-sync-test--git! root "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--git! root "checkout" "-q" "-b" "other")
      (with-temp-file org-file (insert "* Other\n"))
      (supertag-git-sync-test--commit-edit root "other edit")
      (supertag-git-sync-test--git! root "checkout" "-q" "main")
      (with-temp-file org-file (insert "* Main\n"))
      (supertag-git-sync-test--commit-edit root "main edit")
      (should-not (= 0 (car (supertag-git-sync-test--git
                             root "merge" "--no-edit" "other"))))
      (unwind-protect
          (progn
            (unload-feature 'supertag-doctor t)
            (unload-feature 'supertag-git t)
            (require 'supertag-doctor)
            (should-not (featurep 'supertag-git))
            (let ((default-directory root)
                  (supertag-db-file db)
                  (supertag-data-directory root)
                  (org-supertag-sync-directories (list root)))
              (let ((buf (supertag-doctor t)))
                (should (featurep 'supertag-git))
                (with-current-buffer buf
                  (should-not (string-match-p "supertag-git\\.el not loaded"
                                              (buffer-string)))
                  (should (string-match-p (regexp-quote org-file)
                                          (buffer-string)))))))
        (require 'supertag-git)
        (require 'supertag-doctor)))))

(supertag-git-sync-test--deftest supertag-git-sync-test-mode-off-cleans-up
    "Disabling `supertag-git-sync-mode' cancels both timers (asserted nil),
removes its hooks, and flushes (rather than silently drops) any pending
debounced commit."
  (supertag-git-sync-test--with-temp-dir root
    (supertag-git-sync-test--init-worktree root)
    (let ((db (expand-file-name "supertag-db.el" root)))
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db)
      (supertag-git--configure-clone root db)
      (supertag-git-sync-test--git! root "add" "-A")
      (supertag-git-sync-test--git! root "commit" "-q" "-m" "initial")
      (let ((supertag-git-sync--synchronous t)
            (supertag-db-file db)
            (supertag-data-directory root))
        (unwind-protect
            (progn
              (supertag-git-sync-mode 1)
              (should supertag-git-sync-mode)
              (should supertag-git-sync--vault-root)
              (should supertag-git-sync--pull-timer)
              (should (memq #'supertag-git-sync--on-file-saved after-save-hook))
              (should (memq #'supertag-git-sync--on-db-saved
                            supertag-persistence-after-save-hook))
              ;; Simulate a pending debounced commit.
              (supertag-git-sync-test--edit-node db "n1" '(:title "pending change"))
              (supertag-git-sync--schedule-commit)
              (should supertag-git-sync--commit-timer)
              (supertag-git-sync-mode 0)
              (should-not supertag-git-sync-mode)
              (should-not supertag-git-sync--commit-timer)
              (should-not supertag-git-sync--pull-timer)
              (should-not (memq #'supertag-git-sync--on-file-saved after-save-hook))
              (should-not (memq #'supertag-git-sync--on-db-saved
                                supertag-persistence-after-save-hook))
              ;; Flush: the pending change was committed before shutdown,
              ;; not silently dropped.
              (let ((log (supertag-git-sync-test--git! root "log" "--oneline")))
                (should (= 2 (length (split-string (string-trim log) "\n" t))))))
          (when supertag-git-sync-mode (supertag-git-sync-mode 0)))))))

;;; ----------------------------------------------------------------
;;; Part 3b (P0-3 regression): auto-commit must never ship unresolved
;;; conflict markers, and must never sweep untracked non-vault files
;;; into a commit -- see `supertag-git-sync--fire-commit''s docstring
;;; in supertag-git.el for the fixed pre-stage refusal / scoped
;;; staging / post-stage marker scan this pins down.
;;; ----------------------------------------------------------------

(supertag-git-sync-test--deftest supertag-git-sync-test-commit-refuses-unresolved-conflict-then-proceeds
    "`supertag-git-sync--fire-commit' must refuse to stage or commit
ANYTHING while ROOT still has a real, unresolved merge conflict (A and B
edit the SAME line of the same org file, so B's fetch+merge leaves
literal `<<<<<<<' markers in `notes.org' and an unmerged index entry) --
the old unconditional `git add -A' would otherwise mark that conflict
resolved and commit the literal markers into history. A one-shot warning
is issued and `in-flight' is cleared, never left stuck. Once the conflict
is resolved manually (`git checkout --theirs' + `git add', exactly as a
user/magit would), the very next `--fire-commit' call proceeds normally
and completes the merge commit."
  (supertag-git-sync-test--with-temp-dir root
    (let* ((bare (expand-file-name "bare.git" root))
           (a (file-name-as-directory (expand-file-name "a" root)))
           (b (file-name-as-directory (expand-file-name "b" root)))
           (db-a (expand-file-name "supertag-db.el" a))
           (db-b (expand-file-name "supertag-db.el" b))
           (org-rel "notes.org"))
      (supertag-git-sync-test--init-worktree a)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db-a)
      (with-temp-file (expand-file-name org-rel a) (insert "* Heading\nOriginal line.\n"))
      (supertag-git--configure-clone a db-a)
      (supertag-git-sync-test--git! a "add" "-A")
      (supertag-git-sync-test--git! a "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--git! a "remote" "add" "origin" bare)
      (supertag-git-sync-test--git! a "push" "-q" "origin" "main")
      (supertag-git-sync-test--git! root "clone" "-q" bare b)
      (supertag-git-sync-test--git! b "config" "user.name" "Supertag Test")
      (supertag-git-sync-test--git! b "config" "user.email" "supertag-test@example.com")
      (supertag-git-sync-test--git! b "config" "commit.gpgsign" "false")
      (supertag-git--configure-clone b db-b)
      (with-temp-file (expand-file-name org-rel a) (insert "* Heading\nA's version.\n"))
      (supertag-git-sync-test--commit-edit a "A edits notes.org")
      (supertag-git-sync-test--git! a "push" "-q" "origin" "main")
      (with-temp-file (expand-file-name org-rel b) (insert "* Heading\nB's version.\n"))
      (supertag-git-sync-test--commit-edit b "B edits notes.org")
      (let* ((supertag-git-sync--synchronous t)
             (supertag-git-sync--vault-root b)
             (supertag-git-sync--in-flight nil)
             (supertag-git-sync--offline-warned nil)
             (supertag-git-sync--conflict-commit-warned nil)
             (supertag-git-sync--conflicted-org-files nil)
             (supertag-db-file db-b)
             (supertag-data-directory b)
             (supertag--store (supertag-git-sync-test--build-store '("n1")))
             (supertag--store-origin nil)
             (head-before-commit nil))
        ;; Get B into a genuine conflicted-merge state.
        (supertag-git-sync--pull)
        (should (member org-rel (supertag-git-sync--unmerged-paths b)))
        (setq head-before-commit (string-trim (supertag-git-sync-test--git! b "rev-parse" "HEAD")))
        ;; Firing the debounced commit while unresolved must refuse
        ;; entirely: nothing staged, no commit, a one-shot warning,
        ;; `in-flight' cleared rather than left stuck.
        (supertag-git-sync--fire-commit)
        (should-not supertag-git-sync--in-flight)
        (should supertag-git-sync--conflict-commit-warned)
        ;; Nothing changed about the unmerged state itself -- the refusal
        ;; happened before any staging was attempted, not merely after an
        ;; attempted-then-undone one.
        (should (equal (supertag-git-sync--unmerged-paths b) (list org-rel)))
        (should (equal head-before-commit
                       (string-trim (supertag-git-sync-test--git! b "rev-parse" "HEAD"))))
        (should (> (length (supertag-git-sync-test--git! b "diff" "--name-only" "--diff-filter=U"))
                   0))
        ;; Resolve the conflict manually, exactly as a user/magit would.
        (supertag-git-sync-test--git! b "checkout" "--theirs" "--" org-rel)
        (supertag-git-sync-test--git! b "add" "--" org-rel)
        ;; The next fire-commit proceeds normally and completes the merge:
        ;; exactly ONE new commit, a genuine 2-parent merge commit whose
        ;; first parent is the pre-resolution HEAD (i.e. it actually
        ;; completes THIS merge, not some unrelated commit). Checked via
        ;; `rev-parse'/`%P' rather than counting total reachable commits
        ;; via `git log --oneline', which -- being a MERGE -- also newly
        ;; includes the upstream commit(s) it merged in, not just +1.
        (supertag-git-sync--fire-commit)
        (should-not supertag-git-sync--in-flight)
        (should-not supertag-git-sync--conflict-commit-warned)
        (let ((head-after-commit (string-trim (supertag-git-sync-test--git! b "rev-parse" "HEAD")))
              (parents (split-string
                        (string-trim (supertag-git-sync-test--git! b "log" "-1" "--format=%P" "HEAD"))
                        " " t)))
          (should-not (equal head-before-commit head-after-commit))
          (should (= 2 (length parents)))
          (should (member head-before-commit parents)))
        (should (= 0 (length (supertag-git-sync-test--git! b "diff" "--name-only" "--diff-filter=U"))))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-commit-scopes-staging-away-from-untracked-junk
    "The auto-commit's staging must be scoped (`supertag-git-sync--commit-pathspecs'),
never a bare `git add -A': an untracked, non-`.org', non-database file
living in the vault (simulating a stray credentials/secret file) must
NEVER be swept into an automatic commit, while a genuine concurrent
`.org' edit in the same debounce cycle IS committed normally."
  (supertag-git-sync-test--with-temp-dir root
    (supertag-git-sync-test--init-worktree root)
    (let ((db (expand-file-name "supertag-db.el" root))
          (org-file (expand-file-name "notes.org" root))
          (junk-file (expand-file-name "fake-credentials.txt" root)))
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db)
      (with-temp-file org-file (insert "* Heading\nOriginal.\n"))
      (supertag-git--configure-clone root db)
      (supertag-git-sync-test--git! root "add" "-A")
      (supertag-git-sync-test--git! root "commit" "-q" "-m" "initial")
      ;; A genuine org edit AND a stray junk file both appear before the
      ;; next debounce cycle fires.
      (with-temp-file org-file (insert "* Heading\nEdited.\n"))
      (with-temp-file junk-file (insert "SECRET=hunter2\n"))
      (let ((supertag-git-sync--synchronous t)
            (supertag-git-sync--vault-root root)
            (supertag-git-sync--in-flight nil)
            (supertag-git-sync--pending-push-count 0)
            (supertag-git-sync--offline-warned nil)
            (supertag-git-sync--conflict-commit-warned nil)
            (supertag-git-sync--commit-timer nil)
            (supertag-db-file db)
            (supertag-data-directory root))
        (supertag-git-sync--fire-commit)
        (should-not supertag-git-sync--in-flight))
      (let ((shown (supertag-git-sync-test--git! root "show" "--stat" "--oneline" "HEAD")))
        (should (string-match-p "notes\\.org" shown))
        (should-not (string-match-p "fake-credentials" shown)))
      ;; The junk file is still on disk (never deleted) -- just never
      ;; committed; it remains untracked.
      (should (file-exists-p junk-file))
      (should (string-match-p "\\`\\?\\? fake-credentials\\.txt"
                              (supertag-git-sync-test--git! root "status" "--porcelain" "--" junk-file))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-commit-refuses-prestaged-out-of-scope-file
    "Auto-commit must not own or rewrite a user's unrelated index state.
If a non-Supertag file is already staged, the cycle refuses before staging
the Org edit; the staged secret remains staged and no commit is created.
After the user unstages it, the next cycle commits only the Org edit."
  (supertag-git-sync-test--with-temp-dir root
    (supertag-git-sync-test--init-worktree root)
    (let ((db (expand-file-name "supertag-db.el" root))
          (org-file (expand-file-name "notes.org" root))
          (secret-file (expand-file-name "secret.txt" root)))
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db)
      (with-temp-file org-file (insert "* Original\n"))
      (supertag-git--configure-clone root db)
      (supertag-git-sync-test--git! root "add" "-A")
      (supertag-git-sync-test--git! root "commit" "-q" "-m" "initial")
      (with-temp-file org-file (insert "* Edited\n"))
      (with-temp-file secret-file (insert "TOKEN=do-not-commit\n"))
      (supertag-git-sync-test--git! root "add" "--" "secret.txt")
      (should (equal "secret.txt\n"
                     (supertag-git-sync-test--git! root "diff" "--cached" "--name-only")))
      (let ((supertag-db-file db))
        (should-not (supertag-git-sync--auto-commit-path-p root "secret.txt")))
      (let ((head-before (string-trim
                          (supertag-git-sync-test--git! root "rev-parse" "HEAD")))
            (supertag-git-sync--synchronous t)
            (supertag-git-sync--vault-root root)
            (supertag-git-sync--in-flight nil)
            (supertag-git-sync--pending-push-count 0)
            (supertag-git-sync--offline-warned nil)
            (supertag-git-sync--conflict-commit-warned nil)
            (supertag-db-file db)
            (supertag-data-directory root))
        (supertag-git-sync--fire-commit)
        (should-not supertag-git-sync--in-flight)
        (should supertag-git-sync--conflict-commit-warned)
        (should (equal head-before
                       (string-trim
                        (supertag-git-sync-test--git! root "rev-parse" "HEAD"))))
        (should (equal "secret.txt\n"
                       (supertag-git-sync-test--git! root "diff" "--cached" "--name-only")))
        (should (equal "notes.org\n"
                       (supertag-git-sync-test--git! root "diff" "--name-only")))
        (supertag-git-sync-test--git! root "reset" "-q" "--" "secret.txt")
        (supertag-git-sync--fire-commit)
        (should-not supertag-git-sync--conflict-commit-warned)
        (should-not
         (equal head-before
                (string-trim
                 (supertag-git-sync-test--git! root "rev-parse" "HEAD"))))
        (let ((shown (supertag-git-sync-test--git! root "show" "--name-only" "--format=" "HEAD")))
          (should (string-match-p "notes\\.org" shown))
          (should-not (string-match-p "secret\\.txt" shown)))))))

;;; ----------------------------------------------------------------
;;; Part 3c (P1-7 regression): ahead/behind is derived fresh from git
;;; every cycle, and a pull cycle pushes accumulated local commits --
;;; not just fetches/merges -- so neither an offline episode nor an
;;; Emacs restart ever orphans them. See the ahead/behind Commentary
;;; above `supertag-git-sync--rev-count' in supertag-git.el.
;;; ----------------------------------------------------------------

(supertag-git-sync-test--deftest supertag-git-sync-test-pull-catches-up-after-offline-episode
    "A's remote becomes temporarily unreachable (origin URL pointed at a
nonexistent path) while a local commit is made; a pull cycle attempted
while still \"offline\" fails harmlessly (fetch itself fails -- offline
degradation, no push attempted). Once the remote is restored, the VERY
NEXT pull cycle both fetches successfully and pushes the accumulated
local commit (P1-7 offline recovery) -- with no new edit needed to
trigger it -- landing it on the remote and zeroing the pending count."
  (supertag-git-sync-test--with-temp-dir root
    (let* ((bare (expand-file-name "bare.git" root))
           (a (file-name-as-directory (expand-file-name "a" root)))
           (db-a (expand-file-name "supertag-db.el" a))
           (unreachable (expand-file-name "no-such-remote" root)))
      (supertag-git-sync-test--init-worktree a)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db-a)
      (supertag-git--configure-clone a db-a)
      (supertag-git-sync-test--git! a "add" "-A")
      (supertag-git-sync-test--git! a "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--git! a "remote" "add" "origin" bare)
      (supertag-git-sync-test--git! a "push" "-q" "-u" "origin" "main")
      ;; Simulate going offline: `origin' now points nowhere reachable.
      (supertag-git-sync-test--git! a "remote" "set-url" "origin" unreachable)
      (supertag-git-sync-test--edit-node db-a "n1" '(:title "made while offline"))
      (supertag-git-sync-test--commit-edit a "offline commit")
      (let ((supertag-git-sync--synchronous t)
            (supertag-git-sync--vault-root a)
            (supertag-git-sync--in-flight nil)
            (supertag-git-sync--pending-push-count 0)
            (supertag-git-sync--offline-warned nil)
            (supertag-db-file db-a)
            (supertag-data-directory a))
        ;; Cycle while still "offline": fetch itself fails; nothing pushed.
        (supertag-git-sync--pull)
        (should-not supertag-git-sync--in-flight)
        (should supertag-git-sync--offline-warned)
        ;; Reconnect.
        (supertag-git-sync-test--git! a "remote" "set-url" "origin" bare)
        (supertag-git-sync--pull)
        (should-not supertag-git-sync--in-flight)
        (should-not supertag-git-sync--offline-warned)
        (should (= 0 supertag-git-sync--pending-push-count)))
      (let ((remote-log (supertag-git-sync-test--git! bare "log" "--oneline")))
        (should (= 2 (length (split-string (string-trim remote-log) "\n" t))))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-restart-does-not-orphan-unpushed-commit
    "A commit exists locally, ahead of `origin`, that was never pushed --
simulating an Emacs session that committed but quit before its own push
finished. After resetting all session-local git-sync state to simulate a
fresh Emacs restart (the OLD, buggy behavior: `supertag-git-sync--pending-push-count'
starts back at a lying 0), ONE pull cycle must still discover -- fresh
from git, never from that zeroed session counter -- that this vault is
ahead, push it, and report the correct (zero, post-push) count."
  (supertag-git-sync-test--with-temp-dir root
    (let* ((bare (expand-file-name "bare.git" root))
           (a (file-name-as-directory (expand-file-name "a" root)))
           (db-a (expand-file-name "supertag-db.el" a)))
      (supertag-git-sync-test--init-worktree a)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db-a)
      (supertag-git--configure-clone a db-a)
      (supertag-git-sync-test--git! a "add" "-A")
      (supertag-git-sync-test--git! a "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--git! a "remote" "add" "origin" bare)
      (supertag-git-sync-test--git! a "push" "-q" "-u" "origin" "main")
      ;; A commit made "in a previous session" that never got pushed --
      ;; created directly via git, never through
      ;; `supertag-git-sync--fire-commit', so nothing in THIS test run
      ;; ever incremented any session counter for it.
      (supertag-git-sync-test--edit-node db-a "n1" '(:title "committed but never pushed"))
      (supertag-git-sync-test--commit-edit a "orphaned commit")
      ;; Simulate a fresh Emacs restart: all session-local git-sync state
      ;; reset to its just-initialized defaults.
      (let ((supertag-git-sync--synchronous t)
            (supertag-git-sync--vault-root a)
            (supertag-git-sync--in-flight nil)
            (supertag-git-sync--pending-push-count 0)
            (supertag-git-sync--offline-warned nil)
            (supertag-db-file db-a)
            (supertag-data-directory a))
        (supertag-git-sync--pull)
        (should-not supertag-git-sync--in-flight)
        (should (= 0 supertag-git-sync--pending-push-count)))
      (let ((remote-log (supertag-git-sync-test--git! bare "log" "--oneline")))
        (should (= 2 (length (split-string (string-trim remote-log) "\n" t))))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-enable-pushes-orphaned-commit-immediately
    "Enabling `supertag-git-sync-mode' itself -- not waiting for the first
timer tick -- must discover and push a commit that is already ahead of
`origin', e.g. one made by a previous Emacs session that quit before its
own push completed (P1-7 item 3: restarting Emacs must not orphan local
commits)."
  (supertag-git-sync-test--with-temp-dir root
    (let* ((bare (expand-file-name "bare.git" root))
           (a (file-name-as-directory (expand-file-name "a" root)))
           (db-a (expand-file-name "supertag-db.el" a)))
      (supertag-git-sync-test--init-worktree a)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1")) db-a)
      (supertag-git--configure-clone a db-a)
      (supertag-git-sync-test--git! a "add" "-A")
      (supertag-git-sync-test--git! a "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--git! a "remote" "add" "origin" bare)
      (supertag-git-sync-test--git! a "push" "-q" "-u" "origin" "main")
      (supertag-git-sync-test--edit-node db-a "n1" '(:title "made before an emacs restart"))
      (supertag-git-sync-test--commit-edit a "orphaned commit")
      (let ((supertag-git-sync--synchronous t)
            (supertag-db-file db-a)
            (supertag-data-directory a))
        (unwind-protect
            (progn
              (supertag-git-sync-mode 1)
              (should supertag-git-sync-mode)
              (should (= 0 supertag-git-sync--pending-push-count)))
          (when supertag-git-sync-mode (supertag-git-sync-mode 0))))
      (let ((remote-log (supertag-git-sync-test--git! bare "log" "--oneline")))
        (should (= 2 (length (split-string (string-trim remote-log) "\n" t))))))))

(supertag-git-sync-test--deftest supertag-git-sync-test-pull-cycle-diverged-fetch-merge-push
    "B has an unpushed local commit AND the remote has independently
advanced (A pushed first) -- a genuine diverged-history case. ONE
`supertag-git-sync--pull' cycle must fetch, detect `behind', merge
\(auto-resolving via the semantic merge driver, since A and B touched
different nodes), and then -- because the merge leaves B's branch ahead
by its own local commit plus the merge commit -- push, landing BOTH
sides' commits on the remote within that single cycle (P1-7 item 2)."
  (supertag-git-sync-test--with-temp-dir root
    (let* ((bare (expand-file-name "bare.git" root))
           (a (file-name-as-directory (expand-file-name "a" root)))
           (b (file-name-as-directory (expand-file-name "b" root)))
           (db-a (expand-file-name "supertag-db.el" a))
           (db-b (expand-file-name "supertag-db.el" b)))
      (supertag-git-sync-test--init-worktree a)
      (supertag-git-sync-test--write-store
       (supertag-git-sync-test--build-store '("n1" "n2")) db-a)
      (supertag-git--configure-clone a db-a)
      (supertag-git-sync-test--git! a "add" "-A")
      (supertag-git-sync-test--git! a "commit" "-q" "-m" "initial")
      (supertag-git-sync-test--init-bare bare)
      (supertag-git-sync-test--git! a "remote" "add" "origin" bare)
      (supertag-git-sync-test--git! a "push" "-q" "origin" "main")
      (supertag-git-sync-test--git! root "clone" "-q" bare b)
      (supertag-git-sync-test--git! b "config" "user.name" "Supertag Test")
      (supertag-git-sync-test--git! b "config" "user.email" "supertag-test@example.com")
      (supertag-git-sync-test--git! b "config" "commit.gpgsign" "false")
      (supertag-git--configure-clone b db-b)
      ;; A advances the remote first (disjoint node from B's upcoming edit).
      (supertag-git-sync-test--edit-node db-a "n1" '(:title "A advanced remote"))
      (supertag-git-sync-test--commit-edit a "A advances remote")
      (supertag-git-sync-test--git! a "push" "-q" "origin" "main")
      ;; B commits its own, disjoint edit locally -- never pushed.
      (supertag-git-sync-test--edit-node db-b "n2" '(:title "B local edit"))
      (supertag-git-sync-test--commit-edit b "B commits locally")
      (let ((supertag-git-sync--synchronous t)
            (supertag-git-sync--vault-root b)
            (supertag-git-sync--in-flight nil)
            (supertag-git-sync--pending-push-count 0)
            (supertag-git-sync--offline-warned nil)
            (supertag-db-file db-b)
            (supertag-data-directory b))
        (supertag-git-sync--pull)
        (should-not supertag-git-sync--in-flight)
        (should (= 0 supertag-git-sync--pending-push-count)))
      (let ((store (supertag-git-sync-test--read-store db-b)))
        (should (equal "A advanced remote"
                       (plist-get (gethash "n1" (gethash :nodes store)) :title)))
        (should (equal "B local edit"
                       (plist-get (gethash "n2" (gethash :nodes store)) :title))))
      (let ((remote-log (supertag-git-sync-test--git! bare "log" "--oneline")))
        (should (>= (length (split-string (string-trim remote-log) "\n" t)) 3))))))

(provide 'git-sync-mode-test)

;;; git-sync-mode-test.el ends here
