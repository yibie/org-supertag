;;; git-integration-test.el --- E2E tests for supertag-git.el / the merge driver wired into real git -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; S3b of .phrase/phases/phase-git-sync-20260713/PLAN.md ("S3 语义 merge
;; driver" -> "S4 用户旅程" test plan). Unlike test/merge-test.el (which
;; exercises `supertag-merge-3way' and the batch driver entry point
;; directly, with hand-built base/ours/theirs files), this file drives REAL
;; `git' repositories end to end: bare remote + two clones, real commits,
;; real `git fetch'/`git merge', with the merge driver configured the exact
;; way `supertag-git-setup' configures it
;; (`supertag-git--configure-clone', factored out of `supertag-git.el' for
;; exactly this purpose).
;;
;; Every test here shells out to a real `git' binary (skipped entirely, via
;; `ert-skip', if `git' is not on `exec-path' -- see
;; `supertag-git-test--skip-unless-git') and, for driver-configured
;; scenarios, a real batch Emacs subprocess (the actual configured
;; `merge.supertag-db.driver' command line, spawned BY git itself when a
;; merge touches the attributed path) -- this is deliberately not mocked,
;; since the entire point of this suite is to prove the wiring between
;; `supertag-git-setup'/`supertag-git-check' and real git merge machinery,
;; not just the pure merge core (already covered by test/merge-test.el).
;;
;; Run:
;;   ./test/run-tests.sh git
;;   emacs -batch -L . --eval "(package-initialize)" \
;;     -l test/git-integration-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ht)
(require 'cl-lib)

(defconst supertag-git-test--repo-dir
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name default-directory)))
  "The org-supertag repository root, captured at load time.
Needed both to extend `load-path' (so this file's own `require's work when
run standalone) and as the `-L' argument baked into the merge driver
command line that `supertag-git--configure-clone' writes into each test
clone's `.git/config' -- the exact thing a real `supertag-git-setup' would
compute via `locate-library' on THIS machine.")

(add-to-list 'load-path supertag-git-test--repo-dir)

(require 'supertag-core-persistence)
(require 'supertag-merge)
(require 'supertag-git)

;; `org-supertag-sync-directories' is normally defined by
;; supertag-services-sync.el; `supertag-git.el' only ever checks
;; `(boundp ...)' on it (by design -- see its Commentary on the V1
;; single-root check), and this test file follows the same minimal-require
;; discipline rather than pulling in the whole sync module. This top-level
;; `defvar' is what makes it a genuine special (dynamically bound)
;; variable for the `let'-bindings below -- without it the byte compiler
;; would treat those as ordinary lexical bindings, which
;; `supertag-git-setup--pick-root' (reading the dynamic global) would never
;; see. Harmless no-op if some other test file in the same batch run
;; already loaded the real `defcustom' first: `defvar' only ever sets the
;; value when the symbol is still unbound.
(defvar org-supertag-sync-directories nil)

;;; --- Small process/git helpers ---

(defmacro supertag-git-test--with-temp-dir (var &rest body)
  "Bind VAR to a fresh temp directory for BODY; delete it (recursively,
tolerating a merge left mid-conflict or other git-internal file
permission oddities) afterward."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "supertag-git-test" t))))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defmacro supertag-git-test--deftest (name docstring &rest body)
  "Like `ert-deftest', but NAME's BODY only runs when a `git' executable is
found on `exec-path' -- every test in this file needs one, and per the
task's instructions such tests must skip (not fail) in its absence."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,name ()
     ,docstring
     (unless (executable-find "git")
       (ert-skip "git executable not found on exec-path"))
     ,@body))

(defun supertag-git-test--git (dir &rest args)
  "Run `git -C DIR ARGS...'. Returns (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (let ((exit (apply #'call-process "git" nil t nil "-C" dir args)))
      (cons exit (buffer-string)))))

(defun supertag-git-test--git! (dir &rest args)
  "Like `supertag-git-test--git' but assert success via `should'.
Returns just the OUTPUT string. Use the non-asserting variant directly for
any step whose failure/success is itself under test (e.g. `git merge' in
the conflict tests)."
  (let ((result (apply #'supertag-git-test--git dir args)))
    (should (= 0 (car result)))
    (cdr result)))

(defun supertag-git-test--init-bare (dir)
  "`git init --bare' at DIR, with HEAD pre-pointed at `main' so clones check
out a predictable branch name regardless of this machine's
`init.defaultBranch'."
  (make-directory dir t)
  (supertag-git-test--git! dir "init" "-q" "--bare")
  (supertag-git-test--git! dir "symbolic-ref" "HEAD" "refs/heads/main"))

(defun supertag-git-test--init-worktree (dir)
  "`git init' at DIR (creating it) with a deterministic local identity/branch,
independent of this machine's global git config (author/committer name,
no GPG signing, HEAD pre-pointed at the unborn `main' branch)."
  (make-directory dir t)
  (supertag-git-test--git! dir "init" "-q")
  (supertag-git-test--git! dir "symbolic-ref" "HEAD" "refs/heads/main")
  (supertag-git-test--git! dir "config" "user.name" "Supertag Test")
  (supertag-git-test--git! dir "config" "user.email" "supertag-test@example.com")
  (supertag-git-test--git! dir "config" "commit.gpgsign" "false"))

;;; --- Store build/edit helpers (same pattern as test/merge-test.el) ---

(defun supertag-git-test--build-store (ids)
  "Return a store hash table with one minimal :node entity per id in IDS."
  (let ((store (ht-create)) (nodes (ht-create)))
    (puthash :version "5.0.0" store)
    (dolist (id ids)
      (puthash id (list :id id :type :node :title (format "Node %s" id)
                        :priority "low" :file "/tmp/f.org")
               nodes))
    (puthash :nodes nodes store)
    store))

(defun supertag-git-test--write-store (store path)
  "Write STORE to PATH using the real S2 canonical writer."
  (with-temp-buffer
    (set-buffer-file-coding-system 'utf-8-unix)
    (supertag--persistence--write-canonical-store store (current-buffer))
    (write-region (point-min) (point-max) path nil 'silent)))

(defun supertag-git-test--read-store (path)
  "Read PATH back via the real loader (understands both on-disk formats)."
  (supertag--persistence--try-read-store path))

(defun supertag-git-test--edit-node (path id plist-edits)
  "Load the store at PATH, `plist-put' each (KEY VALUE) pair in PLIST-EDITS
onto node ID's data, and write the result back to PATH."
  (let* ((store (supertag-git-test--read-store path))
         (nodes (gethash :nodes store))
         (data (copy-sequence (gethash id nodes))))
    (cl-loop for (k v) on plist-edits by #'cddr
             do (setq data (plist-put data k v)))
    (puthash id data nodes)
    (supertag-git-test--write-store store path)))

(defun supertag-git-test--delete-node (path id)
  "Load the store at PATH, remove node ID entirely, write it back."
  (let* ((store (supertag-git-test--read-store path)))
    (remhash id (gethash :nodes store))
    (supertag-git-test--write-store store path)))

(defun supertag-git-test--sync-conflicts (path)
  "Return the :sync-conflicts collection of the store at PATH as a list of
\(ID . DATA), or nil if absent/empty."
  (let* ((store (supertag-git-test--read-store path))
         (coll (gethash :sync-conflicts store))
         result)
    (when (hash-table-p coll)
      (maphash (lambda (id data) (push (cons id data) result)) coll))
    result))

;;; --- Scenario setup: bare remote + clone A (driver configured) + clone B ---

(cl-defun supertag-git-test--setup-scenario
    (root &key (ids '("n1" "n2")) (configure-b t))
  "Set up BARE + CLONE-A + CLONE-B under ROOT, matching the exact
per-clone configuration flow `supertag-git-setup' performs
(`supertag-git--configure-clone'), and push an initial commit (a store
with one node per id in IDS) from A through BARE to B.

CONFIGURE-B controls whether B's OWN clone also gets the merge driver
configured in its `.git/config' -- pass nil for the S3 \"degradation\"
scenario (E2E-5), where B deliberately behaves like a clone whose owner
never ran `supertag-git-setup'.

Returns a plist: (:bare BARE :a A :b B :db-a DB-A :db-b DB-B)."
  (let* ((bare (expand-file-name "bare.git" root))
         (a (file-name-as-directory (expand-file-name "a" root)))
         (b (file-name-as-directory (expand-file-name "b" root)))
         (db-a (expand-file-name "supertag-db.el" a))
         (db-b (expand-file-name "supertag-db.el" b)))
    (supertag-git-test--init-bare bare)
    (supertag-git-test--init-worktree a)
    (supertag-git-test--write-store (supertag-git-test--build-store ids) db-a)
    ;; Exercise the exact same configuration path `supertag-git-setup' uses.
    (supertag-git--configure-clone a db-a)
    (supertag-git-test--git! a "add" "-A")
    (supertag-git-test--git! a "commit" "-q" "-m" "initial")
    (supertag-git-test--git! a "remote" "add" "origin" bare)
    (supertag-git-test--git! a "push" "-q" "origin" "main")
    (supertag-git-test--git! root "clone" "-q" bare b)
    (supertag-git-test--git! b "config" "user.name" "Supertag Test")
    (supertag-git-test--git! b "config" "user.email" "supertag-test@example.com")
    (supertag-git-test--git! b "config" "commit.gpgsign" "false")
    (when configure-b
      (supertag-git--configure-clone b db-b))
    (list :bare bare :a a :b b :db-a db-a :db-b db-b)))

(defun supertag-git-test--commit-edit (dir message)
  "`git add -A && git commit -q -m MESSAGE' in DIR."
  (supertag-git-test--git! dir "add" "-A")
  (supertag-git-test--git! dir "commit" "-q" "-m" message))

(defun supertag-git-test--fetch-and-merge (dir)
  "`git fetch origin' then `git merge --no-edit origin/main' in DIR.
Returns the merge's (EXIT-CODE . OUTPUT) -- NOT asserted here, since
several tests need to inspect a deliberately-failing merge."
  (supertag-git-test--git! dir "fetch" "-q" "origin")
  (supertag-git-test--git dir "merge" "--no-edit" "origin/main"))

;;; --- E2E-1: disjoint nodes converge, both edits present ---

(supertag-git-test--deftest supertag-git-test-e2e-1-disjoint-nodes-converge
    "A edits node n1, B edits node n2 (never the same entity); after
fetch+merge on B, both edits are present and no conflict is recorded."
  (supertag-git-test--with-temp-dir root
    (let* ((s (supertag-git-test--setup-scenario root))
           (a (plist-get s :a)) (b (plist-get s :b))
           (db-a (plist-get s :db-a)) (db-b (plist-get s :db-b)))
      (supertag-git-test--edit-node db-a "n1" '(:title "A edited n1"))
      (supertag-git-test--commit-edit a "A edits n1")
      (supertag-git-test--git! a "push" "-q" "origin" "main")
      (supertag-git-test--edit-node db-b "n2" '(:title "B edited n2"))
      (supertag-git-test--commit-edit b "B edits n2")
      (let ((merge-result (supertag-git-test--fetch-and-merge b)))
        (should (= 0 (car merge-result))))
      (let ((store (supertag-git-test--read-store db-b)))
        (should (equal "A edited n1" (plist-get (gethash "n1" (gethash :nodes store)) :title)))
        (should (equal "B edited n2" (plist-get (gethash "n2" (gethash :nodes store)) :title)))
        (should (= 0 (hash-table-count (or (gethash :sync-conflicts store) (ht-create)))))))))

;;; --- E2E-2: different fields of the same node converge, no conflicts ---

(supertag-git-test--deftest supertag-git-test-e2e-2-different-fields-converge
    "A and B both edit node n1, but different fields; merge converges with
no :sync-conflicts entries."
  (supertag-git-test--with-temp-dir root
    (let* ((s (supertag-git-test--setup-scenario root))
           (a (plist-get s :a)) (b (plist-get s :b))
           (db-a (plist-get s :db-a)) (db-b (plist-get s :db-b)))
      (supertag-git-test--edit-node db-a "n1" '(:title "A title"))
      (supertag-git-test--commit-edit a "A edits title")
      (supertag-git-test--git! a "push" "-q" "origin" "main")
      (supertag-git-test--edit-node db-b "n1" '(:priority "high"))
      (supertag-git-test--commit-edit b "B edits priority")
      (should (= 0 (car (supertag-git-test--fetch-and-merge b))))
      (let* ((store (supertag-git-test--read-store db-b))
             (n1 (gethash "n1" (gethash :nodes store))))
        (should (equal "A title" (plist-get n1 :title)))
        (should (equal "high" (plist-get n1 :priority)))
        (should (null (supertag-git-test--sync-conflicts db-b)))))))

;;; --- E2E-3: same field, both changed -> exactly one conflict, both values ---

(supertag-git-test--deftest supertag-git-test-e2e-3-same-field-conflict
    "A and B both edit node n1's :title to DIFFERENT values; the merged file
still loads, and :sync-conflicts has exactly one entry recording both
values (nothing silently lost)."
  (supertag-git-test--with-temp-dir root
    (let* ((s (supertag-git-test--setup-scenario root))
           (a (plist-get s :a)) (b (plist-get s :b))
           (db-a (plist-get s :db-a)) (db-b (plist-get s :db-b)))
      (supertag-git-test--edit-node db-a "n1" '(:title "A title"))
      (supertag-git-test--commit-edit a "A edits title")
      (supertag-git-test--git! a "push" "-q" "origin" "main")
      (supertag-git-test--edit-node db-b "n1" '(:title "B title"))
      (supertag-git-test--commit-edit b "B edits title")
      ;; Merge driver reports success (exit 0) even though a real conflict
      ;; was recorded -- see supertag-merge.el's Commentary: conflicts are
      ;; :sync-conflicts data for the user, not a driver failure.
      (should (= 0 (car (supertag-git-test--fetch-and-merge b))))
      ;; The merged file must still be a loadable database (no literal git
      ;; conflict markers were ever written to it).
      (let ((store (supertag-git-test--read-store db-b)))
        (should (hash-table-p (gethash :nodes store))))
      (let ((conflicts (supertag-git-test--sync-conflicts db-b)))
        (should (= 1 (length conflicts)))
        (let ((data (cdr (car conflicts))))
          (should (eq :nodes (plist-get data :collection)))
          (should (equal "n1" (plist-get data :entity-id)))
          (should (eq :title (plist-get data :key)))
          (should (member "A title" (list (plist-get data :ours) (plist-get data :theirs))))
          (should (member "B title" (list (plist-get data :ours) (plist-get data :theirs)))))))))

;;; --- E2E-4: delete vs modify -> resurrect + conflict entry ---

(supertag-git-test--deftest supertag-git-test-e2e-4-delete-vs-modify
    "A deletes node n2; B modifies node n2. Merge resurrects it (never
silently drops user edits) and records exactly one delete-vs-modify
conflict."
  (supertag-git-test--with-temp-dir root
    (let* ((s (supertag-git-test--setup-scenario root))
           (a (plist-get s :a)) (b (plist-get s :b))
           (db-a (plist-get s :db-a)) (db-b (plist-get s :db-b)))
      (supertag-git-test--delete-node db-a "n2")
      (supertag-git-test--commit-edit a "A deletes n2")
      (supertag-git-test--git! a "push" "-q" "origin" "main")
      (supertag-git-test--edit-node db-b "n2" '(:title "B kept editing n2"))
      (supertag-git-test--commit-edit b "B edits n2")
      (should (= 0 (car (supertag-git-test--fetch-and-merge b))))
      (let* ((store (supertag-git-test--read-store db-b))
             (n2 (gethash "n2" (gethash :nodes store))))
        (should n2)
        (should (equal "B kept editing n2" (plist-get n2 :title))))
      (let ((conflicts (supertag-git-test--sync-conflicts db-b)))
        (should (= 1 (length conflicts)))
        (let ((data (cdr (car conflicts))))
          (should (eq :delete-vs-modify (plist-get data :kind)))
          (should (equal "n2" (plist-get data :entity-id)))
          (should (null (plist-get data :key))))))))

;;; --- E2E-5: driver NOT configured on B (degradation path) ---

(supertag-git-test--deftest supertag-git-test-e2e-5-degradation-disjoint-converges
    "B never ran `supertag-git-setup' (no merge driver configured in its
`.git/config', even though `.gitattributes' still names one, inherited
from the clone). Per the plan's documented degradation dividend, git's
own default line-oriented 3-way merge still converges cleanly when the
two sides touched DIFFERENT entities.

Uses THREE nodes (n1/n2/n3), editing the first and last while leaving
the middle one untouched: with only two ADJACENT canonical-format lines
changed and no unchanged line between them, git's default line-merge
algorithm has no context to tell the two single-line edits apart and
folds them into one conflicting hunk even though the two EDITS
themselves never touched the same entity -- this is a property of
line-oriented diff3, not of anything Org-Supertag controls, and is
exactly why real databases (routinely far more than 3 entities, so any
given pair of edits is overwhelmingly unlikely to be gap-free) converge
in practice; the untouched middle node here simply reproduces that
\"has other unrelated entities as context\" condition realistically."
  (supertag-git-test--with-temp-dir root
    (let* ((s (supertag-git-test--setup-scenario root :ids '("n1" "n2" "n3") :configure-b nil))
           (a (plist-get s :a)) (b (plist-get s :b))
           (db-a (plist-get s :db-a)) (db-b (plist-get s :db-b)))
      (should-not (plist-get (supertag-git-check db-b) :driver-configured-p))
      (supertag-git-test--edit-node db-a "n1" '(:title "A edited n1"))
      (supertag-git-test--commit-edit a "A edits n1")
      (supertag-git-test--git! a "push" "-q" "origin" "main")
      (supertag-git-test--edit-node db-b "n3" '(:title "B edited n3"))
      (supertag-git-test--commit-edit b "B edits n3")
      (should (= 0 (car (supertag-git-test--fetch-and-merge b))))
      (let ((store (supertag-git-test--read-store db-b)))
        (should (equal "A edited n1" (plist-get (gethash "n1" (gethash :nodes store)) :title)))
        (should (equal "Node n2" (plist-get (gethash "n2" (gethash :nodes store)) :title)))
        (should (equal "B edited n3" (plist-get (gethash "n3" (gethash :nodes store)) :title)))))))

(supertag-git-test--deftest supertag-git-test-e2e-5-degradation-same-entity-refused-by-loader
    "B never ran `supertag-git-setup'; A and B both edit the SAME node's
SAME field. Git's default text merge -- the only merge available without
the semantic driver -- writes literal conflict markers into the file.
The persistence loader's guard (`supertag--persistence--try-read-store')
must refuse to load that file with a clear, distinguishable error rather
than silently treating it as an empty/corrupt store, AND this must not be
able to lead to a destructive save that clobbers the original candidates."
  (supertag-git-test--with-temp-dir root
    (let* ((s (supertag-git-test--setup-scenario root :configure-b nil))
           (a (plist-get s :a)) (b (plist-get s :b))
           (db-a (plist-get s :db-a)) (db-b (plist-get s :db-b)))
      (supertag-git-test--edit-node db-a "n1" '(:title "A title"))
      (supertag-git-test--commit-edit a "A edits n1 title")
      (supertag-git-test--git! a "push" "-q" "origin" "main")
      (supertag-git-test--edit-node db-b "n1" '(:title "B title"))
      (supertag-git-test--commit-edit b "B edits n1 title")
      ;; Git's default merge reports a conflict (non-zero exit); the file is
      ;; left with literal `<<<<<<<'/`======='/`>>>>>>>' markers.
      (let ((merge-result (supertag-git-test--fetch-and-merge b)))
        (should-not (= 0 (car merge-result))))
      (let ((original-bytes (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally db-b)
                              (buffer-string))))
        (should (string-match-p "^<<<<<<< " original-bytes))
        ;; The loader must refuse this file with the distinguishable
        ;; condition, not a bare `error' and not a silent empty read.
        (let ((err (should-error (supertag--persistence--try-read-store db-b)
                                 :type 'supertag-persistence-conflict-markers-error)))
          (should (string-match-p "supertag-git-setup" (format "%s" err)))
          (should (string-match-p "git checkout --merge" (format "%s" err))))
        ;; Simulate the real load path end to end: `supertag-load-store'
        ;; must record this as a FAILED load (never a benign ":new" empty
        ;; vault -- see that function's commentary for why the distinction
        ;; matters), and any subsequent save attempt must be refused rather
        ;; than clobbering the still-intact-on-disk conflicted file.
        (let* ((supertag-db-file db-b)
               (supertag-data-directory (file-name-directory db-b))
               (supertag-db-backup-directory (expand-file-name "backups" (file-name-directory db-b)))
               (supertag--store nil)
               (supertag--store-origin nil))
          ;; Neutralize the separate, unrelated "sync-state not loaded for
          ;; current vault" guard (this isolated test never loads the sync
          ;; module) -- same technique
          ;; test/persistence-hardening-test.el's auto-migrate test uses --
          ;; so the save refusal asserted below is specifically attributable
          ;; to the `:failed' origin status this change introduces, not
          ;; incidentally to that other guard.
          (cl-letf (((symbol-function 'supertag--persistence--expected-sync-state-file)
                     (lambda () nil)))
            (supertag-load-store db-b)
            (should (eq :failed (plist-get supertag--store-origin :status)))
            (should (hash-table-p supertag--store))
            ;; The store is a fresh, canonically-shaped empty store (all ten
            ;; collections present but empty -- `supertag--record-store-origin'
            ;; itself lazily materializes them via `supertag-store-get-collection'
            ;; -> `supertag--ensure-store' as a side effect of counting nodes,
            ;; even on this failure path), NOT literally a zero-key hash
            ;; table. What matters is that no NODE data survived.
            (should (= 0 (supertag--count-nodes)))
            ;; Even after the (empty, placeholder) in-memory store is marked
            ;; dirty and node(s) are added -- the scenario that would defeat
            ;; the separate byte-size "Protective skip" guard in
            ;; `supertag-save-store', since that guard only fires when the
            ;; live node count is still exactly 0 -- the guard-violations
            ;; check (triggered by the recorded `:failed' origin) refuses
            ;; the save first.
            (puthash "brand-new" (list :id "brand-new" :type :node :title "t")
                     (or (gethash :nodes supertag--store)
                         (let ((h (ht-create))) (puthash :nodes h supertag--store) h)))
            (supertag-mark-dirty)
            ;; `funcall-interactively' so `called-interactively-p' is true
            ;; inside `supertag-save-store', exercising the loud
            ;; `user-error' refusal path (a plain non-interactive/timer call
            ;; would merely log-and-skip -- also non-destructive, but not an
            ;; `error' to assert against).
            (should-error (funcall-interactively #'supertag-save-store db-b) :type 'user-error))
          ;; The on-disk file is byte-for-byte unchanged by that refused save.
          (let ((after-bytes (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally db-b)
                              (buffer-string))))
            (should (equal original-bytes after-bytes))))))))

;;; --- E2E-6: fresh third clone converges to the same merged truth ---

(supertag-git-test--deftest supertag-git-test-e2e-6-fresh-clone-matches-merged-truth
    "After A and B's edits are merged (on B) and pushed back, a brand-new
third clone C loads a store whose node count and field values match the
merged truth already observed on B."
  (supertag-git-test--with-temp-dir root
    (let* ((s (supertag-git-test--setup-scenario root))
           (a (plist-get s :a)) (b (plist-get s :b)) (bare (plist-get s :bare))
           (db-a (plist-get s :db-a)) (db-b (plist-get s :db-b))
           (c (file-name-as-directory (expand-file-name "c" root))))
      (supertag-git-test--edit-node db-a "n1" '(:title "A edited n1"))
      (supertag-git-test--commit-edit a "A edits n1")
      (supertag-git-test--git! a "push" "-q" "origin" "main")
      (supertag-git-test--edit-node db-b "n2" '(:title "B edited n2"))
      (supertag-git-test--commit-edit b "B edits n2")
      (should (= 0 (car (supertag-git-test--fetch-and-merge b))))
      (supertag-git-test--git! b "push" "-q" "origin" "main")
      (let* ((truth (supertag-git-test--read-store db-b))
             (truth-nodes (gethash :nodes truth)))
        (supertag-git-test--git! root "clone" "-q" bare c)
        (let* ((db-c (expand-file-name "supertag-db.el" c))
               (fresh (supertag-git-test--read-store db-c))
               (fresh-nodes (gethash :nodes fresh)))
          (should (= (hash-table-count truth-nodes) (hash-table-count fresh-nodes)))
          (maphash (lambda (id data)
                     (should (equal (plist-get data :title)
                                    (plist-get (gethash id fresh-nodes) :title))))
                   truth-nodes))))))

;;; --- E2E-7: broken driver script -> git reports failure, worktree recoverable ---

(supertag-git-test--deftest supertag-git-test-e2e-7-broken-driver-recoverable
    "A driver that writes partial garbage and exits 1 makes `git merge' fail;
the failure must be cleanly recoverable (`git merge --abort' works, the
worktree returns to a clean pre-merge state) and the repository must not
be left corrupted."
  (supertag-git-test--with-temp-dir root
    (let* ((s (supertag-git-test--setup-scenario root))
           (a (plist-get s :a)) (b (plist-get s :b))
           (db-a (plist-get s :db-a)) (db-b (plist-get s :db-b)))
      ;; Overwrite B's driver with a broken script (written to a temp file
      ;; that outlives this `let'; the whole temp dir is torn down on exit).
      (let* ((script (expand-file-name "broken-driver.sh" root)))
        (with-temp-file script
          (insert "#!/bin/sh\n"
                  "# args: $1=BASE $2=OURS $3=THEIRS -- write partial\n"
                  "# garbage into OURS and fail, simulating a driver that\n"
                  "# crashed mid-write.\n"
                  "printf 'CORRUPTED-PARTIAL' > \"$2\"\n"
                  "exit 1\n"))
        (set-file-modes script #o755)
        (supertag-git-test--git! b "config" "merge.supertag-db.driver"
                                 (concat (shell-quote-argument script) " %O %A %B")))
      (supertag-git-test--edit-node db-a "n1" '(:title "A title"))
      (supertag-git-test--commit-edit a "A edits n1")
      (supertag-git-test--git! a "push" "-q" "origin" "main")
      (supertag-git-test--edit-node db-b "n1" '(:title "B title"))
      (supertag-git-test--commit-edit b "B edits n1")
      (let ((merge-result (supertag-git-test--fetch-and-merge b)))
        (should-not (= 0 (car merge-result))))
      ;; The path must show up as unmerged.
      (let ((status (cdr (supertag-git-test--git b "status" "--porcelain"))))
        (should (string-match-p "supertag-db\\.el" status)))
      ;; Recoverable: abort cleanly restores the pre-merge worktree.
      (should (= 0 (car (supertag-git-test--git b "merge" "--abort"))))
      (let ((status (supertag-git-test--git! b "status" "--porcelain")))
        (should (string= "" (string-trim status))))
      ;; Repository integrity: no corruption, B's own commit is intact.
      (should (= 0 (car (supertag-git-test--git b "fsck" "--no-progress"))))
      (let ((store (supertag-git-test--read-store db-b)))
        (should (equal "B title" (plist-get (gethash "n1" (gethash :nodes store)) :title)))))))

;;; --- Unit: setup idempotence, multi-root refusal, outside-repo refusal ---

(supertag-git-test--deftest supertag-git-test-unit-configure-clone-idempotent
    "Running `supertag-git--configure-clone' twice on the same repo/db-file
never duplicates a `.gitattributes'/`.gitignore' line, and reports empty
*-added lists the second time."
  (supertag-git-test--with-temp-dir dir
    (supertag-git-test--init-worktree dir)
    (let ((db (expand-file-name "supertag-db.el" dir)))
      (supertag-git-test--write-store (supertag-git-test--build-store '("n1")) db)
      (let ((first (supertag-git--configure-clone dir db)))
        (should (plist-get first :gitattributes-added))
        (should (plist-get first :gitignore-added)))
      (let ((second (supertag-git--configure-clone dir db)))
        (should (null (plist-get second :gitattributes-added)))
        (should (null (plist-get second :gitignore-added))))
      (let* ((lines (supertag-git--file-lines (expand-file-name ".gitattributes" dir)))
             (matches (cl-remove-if-not
                       (lambda (l) (string-match-p "merge=supertag-db\\'" l))
                       lines)))
        (should (= 1 (length matches))))
      (let* ((lines (supertag-git--file-lines (expand-file-name ".gitignore" dir)))
             (matches (cl-remove-if-not (lambda (l) (string= l "backups/")) lines)))
        (should (= 1 (length matches)))))))

(supertag-git-test--deftest supertag-git-test-unit-multi-root-refusal
    "`supertag-git-setup--pick-root' refuses outright when
`org-supertag-sync-directories' configures more than one root (the V1
single-root vault limitation)."
  (supertag-git-test--with-temp-dir dir
    (let* ((root1 (expand-file-name "root1" dir))
           (root2 (expand-file-name "root2" dir))
           (db-dir (expand-file-name "elsewhere" dir))
           (org-supertag-sync-directories (list root1 root2)))
      (make-directory root1 t) (make-directory root2 t) (make-directory db-dir t)
      (should-error (supertag-git-setup--pick-root db-dir) :type 'user-error))))

(supertag-git-test--deftest supertag-git-test-unit-outside-repo-refusal
    "`supertag-git-setup--pick-root' refuses when the single configured
sync root does not actually contain the database directory, rather than
silently `git init'-ing somewhere the database does not live."
  (supertag-git-test--with-temp-dir dir
    (let* ((root (file-name-as-directory (expand-file-name "vaultroot" dir)))
           (db-dir (file-name-as-directory (expand-file-name "elsewhere/dbdir" dir)))
           (org-supertag-sync-directories (list root)))
      (make-directory root t) (make-directory db-dir t)
      (should-error (supertag-git-setup--pick-root db-dir) :type 'user-error))))

(provide 'git-integration-test)

;;; git-integration-test.el ends here
