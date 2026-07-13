;;; supertag-git.el --- Git integration for the supertag-db semantic merge driver -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; S3b of .phrase/phases/phase-git-sync-20260713/PLAN.md ("S3 语义 merge
;; driver" -> everything except the pure merge core itself, which is
;; `supertag-merge.el', S3a). This file wires that pure core into a real git
;; checkout: `.gitattributes', **per-clone** `.git/config' merge-driver
;; configuration, `.gitignore' for local-only state, and the
;; `supertag-git-check' diagnostic plist consumed by both `supertag-git-setup'
;; itself and `supertag-doctor'.
;;
;; ## Scope: the full S4 "one URL" user journey, end-to-end
;;
;; This file implements the whole of the plan's "S4 用户旅程" (S3b's
;; original per-clone-plumbing scope, described just below, plus
;; everything S4 added on top of it): repo-layout migration (moving
;; `supertag-db-file' into a vault-rooted `.supertag/' directory --
;; `supertag-git--migrate-layout'), machine 1's `supertag-git-setup'
;; driving that migration through an initial commit, `remote add`/
;; `set-url', and `push -u`, machine N's `supertag-git-clone', and the
;; opt-in `supertag-git-sync-mode' auto commit/fetch/merge/push loop. See
;; the plan's "S4 用户旅程" section for the full user-facing narrative.
;;
;; ## V1 limitation: single-root vaults only
;;
;; Per the plan's explicit decision, git sync in this version only supports
;; a vault backed by a SINGLE `org-supertag-sync-directories' root (or none
;; configured at all, in which case the database's own directory is used).
;; Multiple configured roots are refused outright rather than guessed at —
;; seeing `.phrase/phases/phase-git-sync-20260713/PLAN.md' section "S4 用户
;; 旅程" / "V1 限制" is the documented escape hatch (consolidate to one root,
;; or do not enable git sync yet).
;;
;; ## Why the driver must be configured on EVERY clone
;;
;; `.gitattributes' (which path uses which merge driver) is a tracked file
;; and travels with the repository. `merge.<name>.driver' (what command that
;; driver actually runs) lives in `.git/config', which git NEVER syncs
;; between clones — and the command line this file writes there embeds an
;; absolute, machine-local path to wherever `supertag-merge.el' happens to
;; live on THIS machine (found via `locate-library', which walks
;; `load-path'). This is, per the plan, the single easiest step to forget;
;; `supertag-git-check' (and `supertag-doctor' section 8) exist specifically
;; to make the omission visible instead of silently degrading (see
;; "Degradation" below).
;;
;; ## Degradation when the driver is NOT configured
;;
;; This is not a bolted-on fallback; it falls directly out of S2's
;; canonical, one-entity-per-line database format (see the commentary above
;; `supertag--persistence-canonical-format-header' in
;; supertag-core-persistence.el). Even with `.gitattributes' pointing at an
;; undefined driver name (or no `.gitattributes' entry at all), git's own
;; default line-oriented 3-way text merge already converges cleanly whenever
;; two sides touched DIFFERENT entities/lines — only an edit to the SAME
;; line (the same entity) produces literal `<<<<<<<'/`=======' /`>>>>>>>'
;; markers inside the file. `supertag-core-persistence.el''s loader
;; (`supertag--persistence--try-read-store', via
;; `supertag--persistence--buffer-has-conflict-markers-p') detects that case
;; and refuses to load the file with a message pointing back at
;; `supertag-git-setup' and `git checkout --merge', rather than silently
;; treating a conflict-marked file as an empty store (see that file's
;; commentary for how this interacts with the candidate-fallback/
;; protective-skip load and save paths).
;;
;; ## Package-manager independence
;;
;; This file is intentionally NOT part of the same require chain as
;; `org-supertag.el' proper (no `org' integration, no UI) so that
;; `supertag-git-check' stays cheap to call from `supertag-doctor', and so
;; the driver command line this file writes only ever needs `-L
;; <package-dir>' (see `supertag-git--package-dir'), matching the
;; independently-documented invocation at the top of `supertag-merge.el'.

;;; Code:

(require 'cl-lib)
(require 'supertag-core-persistence)

(defgroup supertag-git nil
  "Git integration for Org-Supertag's semantic database merge driver."
  :group 'org-supertag)

;; --- Free-variable declarations for symbols owned by org-supertag.el /
;; supertag-services-sync.el ---
;;
;; This file deliberately does NOT `require' `org-supertag' or
;; `supertag-services-sync' (see the Commentary, "Package-manager
;; independence"): every reference to a symbol owned by either of those
;; files below is guarded at runtime with `boundp'/`fboundp' and simply
;; degrades to a no-op when that fuller package has not been loaded (e.g. a
;; standalone batch test of just this file). A plain, value-less `defvar'
;; changes nothing about that behavior; it only tells the byte-compiler
;; these symbols are genuinely special (dynamically bound) variables, which
;; matters in exactly one place below: `supertag-git--apply-data-directory-wiring'
;; binds `supertag--config-guard-allow' with a plain `let' to replicate
;; `supertag-config-guard--with-allow' (org-supertag.el's own macro, which
;; this file cannot call directly without requiring that whole package —
;; see that function's docstring). Under `lexical-binding: t' (this file's
;; default), a `let' on an UNDECLARED symbol creates an ordinary lexical
;; binding instead of a dynamic one — invisible to org-supertag.el's own
;; `supertag-config-guard--watch', which reads the symbol dynamically —
;; silently defeating the bypass in compiled code specifically (interpreted
;; code would happen to work, which is exactly the kind of latent
;; compile/interpret divergence this declaration exists to prevent; see
;; test/git-integration-test.el's identical technique/rationale for
;; `org-supertag-sync-directories'). Plain reads/writes of the other
;; symbols below never need this treatment (proven empirically: a
;; `boundp'/`fboundp'-guarded read or `setq' of an undeclared symbol
;; compiles warning-free; only `let'-binding one does not).
(defvar supertag--config-guard-allow)

;; `supertag-load-store'/`supertag-save-store'/`supertag-store-get-collection'
;; are real functions already pulled in transitively by the top-level
;; `(require 'supertag-core-persistence)' above -- no declaration needed.
;; Everything below genuinely lives in files this file does not require.
(declare-function supertag-config-guard--capture "org-supertag")
(declare-function supertag-vault--vault-mode-p "org-supertag")
(declare-function supertag-sync-full-rescan "supertag-services-sync")
(declare-function supertag-sync-check-now "supertag-services-sync")
(declare-function supertag-sync--process-single-file "supertag-services-sync")
(declare-function supertag-sync-save-state "supertag-services-sync")

;;; --- Small git process helpers ---

(defun supertag-git--executable ()
  "Return the git executable path, or nil if not found on `exec-path'."
  (executable-find "git"))

(defun supertag-git--run (dir &rest args)
  "Run git ARGS with DIR as the repository/working directory (`git -C DIR').
Returns (EXIT-CODE . TRIMMED-OUTPUT). Signals an error if git itself cannot
be found (this is a hard prerequisite, not a soft/degrade-able condition —
unlike a missing merge driver, there is no meaningful fallback for
\"no git binary\")."
  (let ((git (supertag-git--executable)))
    (unless git
      (error "supertag-git: `git' executable not found on `exec-path' -- install git first"))
    (with-temp-buffer
      (let ((exit (apply #'call-process git nil t nil "-C" (expand-file-name dir) args)))
        (cons exit (string-trim (buffer-string)))))))

(defun supertag-git--ok-p (result)
  "Return non-nil if RESULT (an (EXIT . OUTPUT) cons from `supertag-git--run')
succeeded."
  (eq 0 (car result)))

(defun supertag-git--repo-toplevel (dir)
  "Return the git worktree toplevel containing DIR, or nil if DIR is not
inside any git worktree (or git is unavailable, or DIR does not exist)."
  (when (and (supertag-git--executable) (file-directory-p dir))
    (let ((result (supertag-git--run dir "rev-parse" "--show-toplevel")))
      (when (supertag-git--ok-p result)
        (file-name-as-directory (expand-file-name (cdr result)))))))

(defun supertag-git--truename-dir (dir)
  "Return DIR as an absolute, trailing-slash-terminated truename.
Using `file-truename' (not just `expand-file-name') matters on macOS in
particular, where temp directories often live under a `/var' that is
itself a symlink to `/private/var' -- a plain string-prefix ancestor check
without resolving that symlink would spuriously fail."
  (file-name-as-directory (file-truename (expand-file-name dir))))

(defun supertag-git--ancestor-p (root path)
  "Return non-nil if ROOT is PATH itself, or an ancestor directory of PATH."
  (let ((r (supertag-git--truename-dir root))
        (p (supertag-git--truename-dir path)))
    (string-prefix-p r p)))

(defun supertag-git--set-config (dir key value)
  "Set git config KEY to VALUE in DIR's repository (local, i.e. `.git/config' —
never `--global'). Signals an error on failure."
  (let ((result (supertag-git--run dir "config" key value)))
    (unless (supertag-git--ok-p result)
      (error "supertag-git: `git config %s' in %s failed: %s" key dir (cdr result)))))

(defun supertag-git--get-config (dir key)
  "Return git config KEY's value in DIR's repository, or nil if unset/on error."
  (let ((result (supertag-git--run dir "config" "--get" key)))
    (when (supertag-git--ok-p result) (cdr result))))

(defun supertag-git--init-repo (root)
  "Run `git init' at ROOT (creating ROOT first if needed). Signals on failure."
  (unless (file-directory-p root)
    (make-directory root t))
  (let ((result (supertag-git--run root "init")))
    (unless (supertag-git--ok-p result)
      (error "supertag-git: `git init' at %s failed: %s" root (cdr result)))))

;;; --- Sync-root configuration (V1 single-root check) ---

(defun supertag-git--sync-roots ()
  "Return the configured, non-empty `org-supertag-sync-directories' entries.
Returns nil when the variable is unbound, nil, or contains no usable
strings -- all treated the same as \"no configured root\"."
  (when (and (boundp 'org-supertag-sync-directories)
             (listp org-supertag-sync-directories))
    (delq nil (mapcar (lambda (d) (and (stringp d) (> (length d) 0) d))
                       org-supertag-sync-directories))))

(defun supertag-git--multiple-sync-roots-p ()
  "Return non-nil if more than one sync root is configured.
This is the V1-unsupported case."
  (> (length (supertag-git--sync-roots)) 1))

;;; --- Package directory / driver command (machine-local, per-clone) ---

(defun supertag-git--package-dir ()
  "Return the absolute directory containing `supertag-merge.el' on THIS machine.
Found via `locate-library', which walks `load-path' without loading the
file. Recomputed fresh every call (never cached to a synced file) — the
whole reason each clone needs its own `.git/config' entry is that this
path is machine-local."
  (let ((lib (locate-library "supertag-merge")))
    (unless lib
      (error "supertag-git: cannot locate supertag-merge.el on `load-path' -- is org-supertag properly installed?"))
    (file-name-directory (expand-file-name lib))))

(defun supertag-git--driver-command (package-dir)
  "Return the `merge.supertag-db.driver' command line for PACKAGE-DIR.
Built with `concat', not `format': the literal `%O'/`%A'/`%B' git
placeholders in the tail are not `format' directives, and passing them
through `format' would error (`%O' is not a recognized spec)."
  (concat "emacs -Q --batch -L " (shell-quote-argument (directory-file-name package-dir))
          " -l supertag-merge.el -f supertag-merge-driver-main %O %A %B"))

;;; --- .gitattributes / .gitignore idempotent line management ---

(defun supertag-git--file-lines (file)
  "Return FILE's contents as a list of lines, or nil if FILE does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t))))

(defun supertag-git--ensure-lines (file lines)
  "Ensure each of LINES appears verbatim somewhere in FILE, appending any
that are missing (creating FILE, and its parent directory, if absent).
Returns the sublist of LINES that were newly appended -- empty when FILE
already contained all of them, which is what makes callers of this
function (`supertag-git--configure-clone') idempotent: running setup twice
never duplicates a line."
  (let* ((existing (supertag-git--file-lines file))
         (missing (cl-remove-if (lambda (l) (member l existing)) lines)))
    (when missing
      (let ((dir (file-name-directory file)))
        (unless (file-directory-p dir) (make-directory dir t)))
      (with-temp-buffer
        (when (file-exists-p file)
          (insert-file-contents file))
        (goto-char (point-max))
        (unless (or (= (point-min) (point-max)) (bolp))
          (insert "\n"))
        (dolist (l missing) (insert l "\n"))
        (write-region (point-min) (point-max) file nil 'silent)))
    missing))

;;; --- Per-clone configuration core (shared by the command and tests) ---

(defun supertag-git--configure-clone (repo-root db-file)
  "Idempotently configure THIS clone's repository at REPO-ROOT to
semantically merge DB-FILE via `supertag-merge.el'.

This is the single code path used by both the interactive
`supertag-git-setup' command and the git-integration test suite's E2E
harness (`supertag-git--configure-clone' factored out specifically so
tests exercise the exact same configuration logic setup does, per the
plan).

Only touches `.gitattributes' and `.gitignore' (working-tree files, safe
to `git add') plus THIS repo's local `.git/config' -- never runs `git add'
or `git commit' itself, and never requires DB-FILE to already exist.

Returns a plist:
  (:repo-root ROOT :relative-path REL
   :gitattributes-file FILE :gitattributes-added (LINE ...)
   :driver-package-dir DIR :driver-command CMD
   :gitignore-file FILE :gitignore-added (LINE ...))
The two *-added slots list only the lines that were newly appended by
THIS call -- empty on a re-run, which is how idempotence is verified.

REPO-ROOT and DB-FILE are both resolved through `file-truename' before
the relative `.gitattributes' path is computed: REPO-ROOT frequently
arrives as a truename already (everything derived from
`supertag-git--repo-toplevel' is -- git prints resolved paths), while
DB-FILE typically arrives as the user-configured, possibly
symlink-traversing spelling (`/tmp/...' vs `/private/tmp/...' on macOS
being the canonical example). Mixing the two produces a
repository-escaping `../../..' relative path, which `.gitattributes'
matches against NOTHING -- the semantic merge driver then silently never
applies. Found by a real-environment E2E run, not code review."
  (let* ((root (supertag-git--truename-dir repo-root))
         (rel (file-relative-name (file-truename (expand-file-name db-file)) root))
         (attrs-file (expand-file-name ".gitattributes" root))
         (attrs-line (format "%s merge=supertag-db" rel))
         (attrs-added (supertag-git--ensure-lines attrs-file (list attrs-line)))
         (package-dir (directory-file-name (supertag-git--package-dir)))
         (driver-cmd (supertag-git--driver-command package-dir)))
    (supertag-git--set-config root "merge.supertag-db.name"
                              "Org-Supertag semantic 3-way database merge")
    (supertag-git--set-config root "merge.supertag-db.driver" driver-cmd)
    (let* ((db-dir (file-name-directory (expand-file-name db-file)))
           (ignore-file (expand-file-name ".gitignore" db-dir))
           (ignore-added (supertag-git--ensure-lines
                          ignore-file
                          (list "sync-state.el" "backups/" "supertag-presence.json" ".#*"))))
      (list :repo-root root
            :relative-path rel
            :gitattributes-file attrs-file
            :gitattributes-added attrs-added
            :driver-package-dir package-dir
            :driver-command driver-cmd
            :gitignore-file ignore-file
            :gitignore-added ignore-added))))

;;; --- S4 vault layout migration (DB must live INSIDE the repo) ---
;;
;; Per the plan's "S4 用户旅程" -> "仓库布局": the merge driver requires the
;; database file to be a tracked, in-repo path, but the pre-S4 common case
;; is a database living at `~/.emacs.d/org-supertag/' -- entirely outside
;; whatever git repository the user's `.org' vault lives in. Migration is:
;;
;;   0. if the LIVE in-memory store has unsaved changes (`supertag-dirty-p'),
;;      flush them to the OLD file first via the normal `supertag-save-store'
;;      path, then re-check `supertag-dirty-p' -- a save that is silently
;;      skipped (guard violation, lock conflict, protective empty-store
;;      skip, ...) still leaves the flag set, and that must ABORT the
;;      migration rather than let step 1 below copy a database that does
;;      not reflect what is actually in memory (see
;;      `supertag--persistence-guard-violations' for the reasons named in
;;      the abort message).
;;   1. write the NEW database from the CURRENT in-memory store directly,
;;      via `supertag--persistence-write-store-atomically' -- never a
;;      `copy-file' of whatever the OLD file happens to contain on disk,
;;      which (even after step 0's flush) is one extra hop removed from
;;      "what is actually in memory right now". `copy-file' is used ONLY
;;      as a fallback for the case where no store has been loaded into
;;      this session at all yet (so there is no in-memory store to
;;      serialize from).
;;   2. verify the new file is loadable (never trust a bare `copy-file' or
;;      a bare atomic write without reading it back)
;;   3. only once verified: flip this session's persistence wiring
;;      (`supertag-data-directory'/`supertag-db-file'/etc, the same
;;      config-guard-bypassed pattern `supertag-vault--apply' uses),
;;      reload the live store from the new location, and confirm BOTH that
;;      the reload's recorded origin status is `:ok' AND (when a store was
;;      already loaded pre-migration) that its node count matches the
;;      pre-migration count captured in step 0 -- any disagreement aborts
;;      exactly like a failed verification would.
;;   4. only once step 3 has itself fully succeeded: rename (never delete)
;;      the OLD database file to a `.migrated-<timestamp>' tombstone
;;
;; Any failure at or after the wiring flip in step 3 restores this
;; session's wiring to its pre-migration snapshot (`supertag-git--capture-wiring'
;; / `supertag-git--restore-wiring') and reloads the store from the
;; (now-restored) OLD file, via an `unwind-protect' state machine -- so a
;; failure never leaves the session pointed at a new file while the OLD
;; file (and whatever real data it holds) sits untouched and unreferenced.
;; Any failure BEFORE the wiring flip leaves the OLD file completely
;; untouched (no wiring changed, nothing renamed) -- see
;; `supertag-git--do-migrate-layout'.

(defun supertag-git--apply-data-directory-wiring (data-dir)
  "Point this session's persistence variables at DATA-DIR (a directory).

Mirrors `supertag-vault--apply' in org-supertag.el (config-guard bypassed
via a dynamic `let' on `supertag--config-guard-allow', then the guard's
expected-state snapshot re-captured so it agrees with the new values) --
but computes DATA-DIR itself very differently: `supertag-vault--apply'
always derives a vault's data directory from a content hash of its sync
root under the shared base data directory (`vaults/<id>/'), which does not
match what git sync requires (`<repo-root>/.supertag/', a *predictable*,
human-inspectable, in-repo path). This function does not call
`supertag-vault--apply' or replicate its vault-registry bookkeeping at
all — see this file's Commentary for why (V1 is single-root only, and
`org-supertag.el' is deliberately not part of this file's require chain).

Safe to call whether or not `org-supertag.el' has been loaded at all: the
config-guard bypass and vault-mode bookkeeping are both best-effort no-ops
in that case (this matters for this file's own standalone batch tests)."
  (let ((data-dir (file-name-as-directory (expand-file-name data-dir))))
    (let ((supertag--config-guard-allow t))
      (setq supertag-data-directory data-dir)
      (setq supertag-db-file (expand-file-name "supertag-db.el" data-dir))
      (setq supertag-db-backup-directory (expand-file-name "backups" data-dir))
      (when (boundp 'supertag-sync-state-file)
        (setq supertag-sync-state-file (expand-file-name "sync-state.el" data-dir))))
    (when (fboundp 'supertag-config-guard--capture)
      (funcall #'supertag-config-guard--capture))
    ;; Best-effort: keep the single-root "registry" (there is no other one
    ;; in this V1 -- see the Commentary on `org-supertag-sync-directories'
    ;; being the vault registry) pointed at the vault root this data
    ;; directory now belongs to, when vault mode happens to be active.
    (when (and (fboundp 'supertag-vault--vault-mode-p)
               (funcall #'supertag-vault--vault-mode-p)
               (boundp 'org-supertag-active-sync-directory))
      (let ((roots (supertag-git--sync-roots)))
        (when (= (length roots) 1)
          (let ((supertag--config-guard-allow t))
            (setq org-supertag-active-sync-directory (car roots))))))))

(defun supertag-git--capture-wiring ()
  "Return a plist snapshotting this session's persistence wiring variables,
so `supertag-git--restore-wiring' can put them back if a migration step
fails AFTER `supertag-git--apply-data-directory-wiring' has already
flipped them. Mirrors exactly the set of variables that function writes."
  (list :data-directory (and (boundp 'supertag-data-directory) supertag-data-directory)
        :db-file (and (boundp 'supertag-db-file) supertag-db-file)
        :db-backup-directory (and (boundp 'supertag-db-backup-directory)
                                  supertag-db-backup-directory)
        :sync-state-file (and (boundp 'supertag-sync-state-file)
                              supertag-sync-state-file)
        :active-sync-directory (and (boundp 'org-supertag-active-sync-directory)
                                    org-supertag-active-sync-directory)))

(defun supertag-git--restore-wiring (snapshot)
  "Restore persistence wiring variables from SNAPSHOT (as returned by
`supertag-git--capture-wiring'), bypassing the config guard the same way
`supertag-git--apply-data-directory-wiring' does -- this is the undo half
of that function, used when a migration fails after the wiring was
already flipped (see `supertag-git--do-migrate-layout')."
  (let ((supertag--config-guard-allow t))
    (when (boundp 'supertag-data-directory)
      (setq supertag-data-directory (plist-get snapshot :data-directory)))
    (when (boundp 'supertag-db-file)
      (setq supertag-db-file (plist-get snapshot :db-file)))
    (when (boundp 'supertag-db-backup-directory)
      (setq supertag-db-backup-directory (plist-get snapshot :db-backup-directory)))
    (when (boundp 'supertag-sync-state-file)
      (setq supertag-sync-state-file (plist-get snapshot :sync-state-file)))
    (when (boundp 'org-supertag-active-sync-directory)
      (setq org-supertag-active-sync-directory (plist-get snapshot :active-sync-directory))))
  (when (fboundp 'supertag-config-guard--capture)
    (funcall #'supertag-config-guard--capture)))

(defun supertag-git--do-migrate-layout (old-db-file root)
  "Perform the flush/write/verify/wire/reload/tombstone sequence, migrating
OLD-DB-FILE into `<ROOT>/.supertag/supertag-db.el'.

Only ever called by `supertag-git--migrate-layout' once ROOT has already
been confirmed to be the single configured sync root and NOT already an
ancestor directory of OLD-DB-FILE. Returns a plist; see that function's
docstring for the full set of possible `:status' values. Never signals --
every failure mode (including one deliberately injected by a test, e.g. an
unwritable target directory, an unsaveable dirty store, or a post-reload
node-count mismatch) is caught and reported as `:status :failed' plus a
human-readable `:error' string, with the OLD file provably untouched and
this session's wiring provably restored whenever it had already been
flipped (see this file's Commentary above this section for the full
sequence)."
  (condition-case err
      (let* ((target-dir (expand-file-name ".supertag/" root))
             (target-db (expand-file-name "supertag-db.el" target-dir))
             (old-exists (file-exists-p old-db-file))
             (store-loaded-p (and (boundp 'supertag--store) (hash-table-p supertag--store)))
             (pre-migration-node-count (and store-loaded-p (supertag--count-nodes))))
        (when (file-exists-p target-db)
          (error "migration target %s already exists -- resolve manually before retrying (are two migrations racing?)"
                 target-db))
        (when (and old-exists store-loaded-p)
          (let ((reasons (supertag--persistence-guard-violations old-db-file)))
            (when reasons
              (error "refusing to migrate the in-memory database: %s"
                     (mapconcat #'identity reasons "; ")))))
        (unless (file-directory-p root)
          (make-directory root t))
        (unless (supertag-git--repo-toplevel root)
          (supertag-git--init-repo root))
        (let ((toplevel (or (supertag-git--repo-toplevel root)
                             (error "`git init' at %s did not result in a git worktree -- this should not happen"
                                    root))))
          (make-directory target-dir t)
          (if (not old-exists)
              ;; Fresh vault: nothing on disk to copy/verify/tombstone yet --
              ;; just point the wiring at the new location so the rest of
              ;; `supertag-git-setup' (and the very next save) creates the
              ;; database there directly. Nothing destructive happens here
              ;; (no old file to lose, no wiring to restore on failure).
              (progn
                (supertag-git--apply-data-directory-wiring target-dir)
                (list :status :migrated :old-db-file old-db-file :new-db-file target-db
                      :repo-root toplevel :tombstone nil :fresh-vault t))
            ;; Step 0: flush any unsaved in-memory changes to the OLD file
            ;; first, via the normal save path -- then verify the flush
            ;; actually cleared the dirty flag. A save can be silently
            ;; skipped (guard violation, lock conflict, the protective
            ;; empty-store skip, ...) without signaling anything, which is
            ;; why this checks `supertag-dirty-p' again afterward instead of
            ;; trusting `supertag-save-store' to have either raised or
            ;; succeeded.
            (when (supertag-dirty-p)
              (when (fboundp 'supertag-save-store)
                (funcall #'supertag-save-store old-db-file))
              (when (supertag-dirty-p)
                (error "refusing to migrate: the in-memory database has unsaved changes that could not be saved first (%s) -- resolve the issue (see M-x supertag-doctor), save manually, and retry"
                       (let ((reasons (supertag--persistence-guard-violations old-db-file)))
                         (if reasons
                             (mapconcat #'identity reasons "; ")
                           "save was skipped for an unlogged reason -- see *Messages* for the most recent \"Supertag\" message")))))
            ;; Step 1: write the NEW file from the CURRENT in-memory store
            ;; directly when one is loaded (never a `copy-file' of the OLD
            ;; file, which even post-flush is one hop removed from "what is
            ;; actually in memory") -- `copy-file' is the fallback ONLY for
            ;; the no-store-loaded-yet case.
            (if store-loaded-p
                (supertag--persistence-write-store-atomically target-db)
              (copy-file old-db-file target-db nil t))
            ;; Step 2: verify the new file is actually loadable BEFORE
            ;; touching any wiring or the old file -- a write/copy that
            ;; "succeeded" onto a filesystem that silently truncates, or a
            ;; target that was already subtly corrupt, must not be trusted
            ;; just because the syscall returned.
            (let ((verify-error nil))
              (condition-case verr
                  (unless (hash-table-p (supertag--persistence--try-read-store target-db))
                    (setq verify-error "migrated database did not parse into a store hash table"))
                (error (setq verify-error (error-message-string verr))))
              (when verify-error
                (ignore-errors (delete-file target-db))
                (error "migrated database at %s failed verification: %s" target-db verify-error)))
            ;; Verified loadable: only now flip the wiring. Everything from
            ;; here on is guarded by an `unwind-protect' that restores the
            ;; wiring (and reloads the store from the OLD file) unless the
            ;; whole sequence, including the tombstone rename, reaches
            ;; `:committed'.
            (let ((wiring-snapshot (supertag-git--capture-wiring))
                  (committed nil))
              (unwind-protect
                  (progn
                    (supertag-git--apply-data-directory-wiring target-dir)
                    ;; Reload the live store from the new location (this
                    ;; also re-acquires the multi-instance lock and
                    ;; refreshes cross-machine presence for the new path,
                    ;; via `supertag-load-store' itself).
                    (when (fboundp 'supertag-load-store)
                      (funcall #'supertag-load-store))
                    ;; Step 3: the reload must report `:ok', and -- when a
                    ;; store was already loaded pre-migration -- its node
                    ;; count must match the pre-migration count captured
                    ;; above. Either failing means the reload landed on
                    ;; something other than what was actually migrated;
                    ;; never tombstone the old file in that case.
                    (let ((origin-status (and (boundp 'supertag--store-origin)
                                              (plist-get supertag--store-origin :status)))
                          (post-node-count (supertag--count-nodes)))
                      (unless (eq origin-status :ok)
                        (error "reload of the migrated database at %s reported status %s (expected :ok)"
                               target-db origin-status))
                      (when (and store-loaded-p
                                 (numberp pre-migration-node-count)
                                 (/= post-node-count pre-migration-node-count))
                        (error "node count mismatch after migration reload: had %d node(s) before, %d after (%s) -- refusing to tombstone the old database"
                               pre-migration-node-count post-node-count target-db)))
                    ;; Step 4: only after a successful reload AND a matching
                    ;; node count do we touch the OLD file at all -- renamed
                    ;; to a tombstone, NEVER deleted.
                    (let* ((timestamp (format-time-string "%Y%m%d%H%M%S"))
                           (tombstone (concat old-db-file ".migrated-" timestamp)))
                      (condition-case terr
                          (rename-file old-db-file tombstone)
                        (error
                         (message "supertag-git: migration to %s succeeded, but renaming the old database (%s) to a tombstone failed: %s -- the old file is harmlessly left in place (it is no longer read by anything)."
                                  target-db old-db-file (error-message-string terr))
                         (setq tombstone nil)))
                      (setq committed t)
                      (list :status :migrated :old-db-file old-db-file :new-db-file target-db
                            :repo-root toplevel :tombstone tombstone)))
                (unless committed
                  ;; Something after the wiring flip failed: restore the
                  ;; wiring to its pre-migration snapshot and reload the
                  ;; store from the (now-restored) OLD file, so this
                  ;; session's in-memory state matches its wiring again --
                  ;; never left pointed at a new file it just gave up on
                  ;; while `supertag-db-file' itself already says otherwise.
                  ;; Also remove the unverified/unused target so a retry
                  ;; does not trip the "target already exists" guard above.
                  (supertag-git--restore-wiring wiring-snapshot)
                  (when (fboundp 'supertag-load-store)
                    (funcall #'supertag-load-store))
                  (ignore-errors (delete-file target-db))))))))
    (error
     (list :status :failed :old-db-file old-db-file :repo-root root
           :error (error-message-string err)))))

(defun supertag-git--migrate-layout ()
  "Migrate `supertag-db-file' into the git vault layout, if needed and
possible; a safe, idempotent no-op otherwise.

Returns a plist with at least `:status' and `:old-db-file'. `:status' is
one of:

  :skipped-in-repo           `supertag-db-file' is already inside SOME git
                              worktree (whether or not that happens to be
                              `<root>/.supertag/' specifically -- an
                              already-tracked-elsewhere-in-repo database is
                              left exactly where it is, matching
                              `supertag-git-setup''s pre-existing
                              behavior). Idempotent: this is what every
                              re-run after a successful migration reports.
  :skipped-no-root           no `org-supertag-sync-directories' root is
                              configured to migrate towards.
  :skipped-multi-root        more than one root is configured (V1
                              limitation; refused elsewhere by
                              `supertag-git-setup--pick-root' too).
  :skipped-root-missing      the single configured root does not exist as
                              a directory yet.
  :skipped-already-under-root the single configured root already contains
                              `supertag-db-file' -- no relocation needed;
                              `supertag-git-setup--pick-root' will
                              find/init the repo at ROOT itself.
  :migrated                  the migration (or, for a not-yet-existing
                              database, the wiring switch alone) succeeded;
                              see `:new-db-file'/`:tombstone'/`:repo-root'.
  :failed                    see `:error' (a human-readable string); the
                              OLD file is guaranteed untouched (see
                              `supertag-git--do-migrate-layout')."
  (let* ((old-db-file (expand-file-name supertag-db-file))
         (old-db-dir (file-name-directory old-db-file)))
    (if (supertag-git--repo-toplevel old-db-dir)
        (list :status :skipped-in-repo :old-db-file old-db-file)
      (let ((roots (supertag-git--sync-roots)))
        (cond
         ((> (length roots) 1)
          (list :status :skipped-multi-root :old-db-file old-db-file))
         ((null roots)
          (list :status :skipped-no-root :old-db-file old-db-file))
         (t
          (let ((root (file-name-as-directory (expand-file-name (car roots)))))
            (cond
             ((not (file-directory-p root))
              (list :status :skipped-root-missing :old-db-file old-db-file :repo-root root))
             ((supertag-git--ancestor-p root old-db-dir)
              (list :status :skipped-already-under-root :old-db-file old-db-file :repo-root root))
             (t
              (supertag-git--do-migrate-layout old-db-file root))))))))))

;;; --- supertag-git-check: non-interactive diagnostic ---

(defun supertag-git-check (&optional file)
  "Return a plist describing this clone's git-sync configuration state.
FILE defaults to `supertag-db-file'. Used by both `supertag-git-setup'
\(to decide what needs doing) and `supertag-doctor' (to report status).

Keys:
  :in-repo-p                       DB's directory is inside a git worktree
  :repo-root                       that worktree's toplevel, or nil
  :relative-path                   DB path relative to :repo-root, or nil
  :driver-configured-p             `merge.supertag-db.driver' set in THIS clone
  :gitattributes-entry-present-p   `.gitattributes' has the DB's merge= line
  :db-tracked-p                    DB path is tracked by git (`git ls-files')
  :remote-configured-p             at least one `git remote' is configured
  :multiple-sync-roots-p           V1-unsupported multi-root vault configured"
  (let* ((db-file (expand-file-name (or file supertag-db-file)))
         (db-dir (file-name-directory db-file))
         (multi (supertag-git--multiple-sync-roots-p))
         (toplevel (supertag-git--repo-toplevel db-dir)))
    (if (not toplevel)
        (list :in-repo-p nil :repo-root nil :relative-path nil
              :driver-configured-p nil :gitattributes-entry-present-p nil
              :db-tracked-p nil :remote-configured-p nil
              :multiple-sync-roots-p multi)
      ;; Truename BOTH sides of the relative-path computation, for the same
      ;; symlink-mismatch reason documented on `supertag-git--configure-clone'
      ;; (whose `.gitattributes' entry this :relative-path is compared
      ;; against): `toplevel' is already a truename (git resolves it), so a
      ;; non-truenamed DB-FILE under a symlinked prefix would yield a bogus
      ;; `../..'-escaping rel here and a spurious "entry missing" report.
      (let* ((rel (file-relative-name (file-truename db-file) toplevel))
             (driver (supertag-git--get-config toplevel "merge.supertag-db.driver"))
             (attrs-lines (supertag-git--file-lines (expand-file-name ".gitattributes" toplevel)))
             (attrs-present (cl-some
                             (lambda (l) (string-match-p
                                         (concat "^" (regexp-quote rel) "[ \t]+merge=supertag-db\\'")
                                         l))
                             attrs-lines))
             (tracked (supertag-git--ok-p
                       (supertag-git--run toplevel "ls-files" "--error-unmatch" "--" rel)))
             (remotes (supertag-git--run toplevel "remote")))
        (list :in-repo-p t
              :repo-root toplevel
              :relative-path rel
              :driver-configured-p (and (stringp driver) (> (length driver) 0) t)
              :gitattributes-entry-present-p (and attrs-present t)
              :db-tracked-p tracked
              :remote-configured-p (and (supertag-git--ok-p remotes)
                                       (> (length (cdr remotes)) 0))
              :multiple-sync-roots-p multi)))))

;;; --- supertag-git-setup: interactive command ---

(defconst supertag-git--report-buffer-name "*Supertag Git Setup*"
  "Name of the buffer used to render the `supertag-git-setup' report.")

(defun supertag-git-setup--pick-root (db-dir)
  "Return the repo root to `git init' at for DB-DIR, or signal a clear error.
Refuses (rather than guessing) when: multiple sync roots are configured
\(V1 limitation); a single sync root is configured but does not actually
contain DB-DIR (the vault-layout requirement -- git sync needs the
database physically inside the chosen repo); or no root can be inferred
and we cannot prompt (`noninteractive')."
  (when (supertag-git--multiple-sync-roots-p)
    (user-error "supertag-git-setup: refusing -- `org-supertag-sync-directories' configures multiple roots (%s). Git sync (V1) only supports a single-root vault; see .phrase/phases/phase-git-sync-20260713/PLAN.md \"S4 用户旅程\" / \"V1 限制\" for how to consolidate."
                (mapconcat #'identity (supertag-git--sync-roots) ", ")))
  (let ((roots (supertag-git--sync-roots)))
    (cond
     ((= (length roots) 1)
      (let ((root (file-name-as-directory (expand-file-name (car roots)))))
        (unless (supertag-git--ancestor-p root db-dir)
          (user-error "supertag-git-setup: refusing -- the configured sync root %s does not contain the database directory %s. Git sync (V1) requires the database to live inside the single vault root; see .phrase/phases/phase-git-sync-20260713/PLAN.md \"S4 用户旅程\" / \"仓库布局\"."
                      root db-dir))
        root))
     (noninteractive
      (error "supertag-git-setup: %s is not inside a git repository and no usable `org-supertag-sync-directories' root is configured to infer one; run this interactively to be prompted for a root, or `git init` the vault yourself first"
             db-dir))
     (t
      (let ((root (file-name-as-directory
                   (expand-file-name
                    (read-directory-name
                     "No git repo found for the Org-Supertag database; initialize one at: "
                     db-dir nil t)))))
        (unless (supertag-git--ancestor-p root db-dir)
          (user-error "supertag-git-setup: refusing -- %s does not contain the database directory %s. Git sync (V1) requires the database to live inside the chosen repo root."
                      root db-dir))
        root)))))

;;; --- supertag-git-setup: remote / initial commit / push (S4 machine 1) ---
;;
;; The rest of the plan's "S4 用户旅程" -> "机器 1" flow, on top of the
;; per-clone `.gitattributes'/`.gitignore'/driver configuration above:
;; configure (or confirm) an `origin' remote, create the vault's initial
;; commit via the exact same scoped-staging pathspec logic
;; `supertag-git-sync-mode' uses for its own auto-commits (never a bare
;; `git add -A' -- see `supertag-git-sync--commit-pathspecs', defined
;; later in this file in the auto-sync section; calling it here from code
;; defined earlier in the file is an ordinary forward reference, resolved
;; at call time like any other Lisp symbol -- not a forward DECLARATION,
;; which is only needed for symbols owned by some OTHER file), and
;; `push -u origin <branch>'. Deliberately no retry loop here: an
;; auth/network failure gets one clear diagnostic message pointing at
;; manual `git push' / ssh-agent / credential-helper troubleshooting --
;; automatic retrying on reconnect is `supertag-git-sync-mode''s job, not
;; this one-shot setup command's.

(defun supertag-git-setup--existing-origin-url (root)
  "Return ROOT's configured `remote.origin.url', or nil if `origin' is not
configured at all."
  (supertag-git--get-config root "remote.origin.url"))

(defun supertag-git-setup--remote-add (root url)
  "Run `git remote add origin URL' in ROOT. Signals a clear error on failure."
  (let ((result (supertag-git--run root "remote" "add" "origin" url)))
    (unless (supertag-git--ok-p result)
      (error "supertag-git-setup: `git remote add origin %s' failed: %s" url (cdr result)))))

(defun supertag-git-setup--remote-set-url (root url)
  "Run `git remote set-url origin URL' in ROOT. Signals a clear error on
failure."
  (let ((result (supertag-git--run root "remote" "set-url" "origin" url)))
    (unless (supertag-git--ok-p result)
      (error "supertag-git-setup: `git remote set-url origin %s' failed: %s" url (cdr result)))))

(defun supertag-git-setup--configure-remote (root)
  "Prompt for an `origin' remote URL for ROOT and configure it, per the
plan's machine-1 journey. The prompt is pre-filled with the CURRENTLY
configured `origin' URL, if any (so accepting it with RET is a no-op,
and the user can also see at a glance what is already set); clearing the
minibuffer to empty and pressing RET is an explicit, valid choice to skip
remote setup entirely (local-only vault -- git sync via the merge driver
still works fine for a single machine, or via manual `git remote add' /
`magit' later).

Returns the configured URL (a string) on success, or nil when the user
chose to skip (empty input) -- callers use nil to skip the initial
commit/push step too, exactly per the plan (\"若给出 URL\" gates all of
remote-add/commit/push together, not just the remote-add step alone)."
  (let* ((existing (supertag-git-setup--existing-origin-url root))
         (input (string-trim
                 (read-string "Git remote URL for `origin' (empty = local-only, skip for now): "
                              existing))))
    (cond
     ((zerop (length input)) nil)
     ((not existing)
      (supertag-git-setup--remote-add root input)
      input)
     ((equal input existing) existing)
     (t
      (if (y-or-n-p (format "supertag-git-setup: `origin' is already set to %s -- change it to %s? "
                            existing input))
          (progn (supertag-git-setup--remote-set-url root input) input)
        existing)))))

(defun supertag-git-setup--commit-all (root message)
  "Stage everything `supertag-git-sync--commit-pathspecs' currently allows
under ROOT (`.org' files, `supertag-db-file' itself, `.gitattributes',
`.gitignore' -- never a bare, unconditional `git add -A'; see that
function's docstring for the full rationale, shared verbatim with
`supertag-git-sync-mode''s own auto-commits) and commit with MESSAGE if
anything actually ends up staged. Returns non-nil if a commit was
created, nil if there was nothing new to commit (e.g. re-running setup
with nothing changed since the last commit)."
  (let ((pathspecs (supertag-git-sync--commit-pathspecs root)))
    (when pathspecs
      (let ((add-result (apply #'supertag-git--run root "add" "-A" "--" pathspecs)))
        (unless (supertag-git--ok-p add-result)
          (error "supertag-git-setup: `git add' failed: %s" (cdr add-result))))
      (let ((diff-result (supertag-git--run root "diff" "--cached" "--quiet")))
        (cond
         ((supertag-git--ok-p diff-result) nil) ; exit 0 -- nothing staged
         ((/= 1 (car diff-result))
          (error "supertag-git-setup: could not inspect the staged diff: %s" (cdr diff-result)))
         (t
          (let ((commit-result (supertag-git--run root "commit" "-q" "-m" message)))
            (unless (supertag-git--ok-p commit-result)
              (error "supertag-git-setup: `git commit' failed: %s" (cdr commit-result)))
            t)))))))

(defun supertag-git-setup--current-branch (root)
  "Return ROOT's current branch name (`git rev-parse --abbrev-ref HEAD'),
or nil on failure. Works even before ROOT's first commit -- HEAD is a
symbolic ref to the branch name from `git init'/`symbolic-ref' onward,
resolvable without any commit existing yet."
  (let ((result (supertag-git--run root "rev-parse" "--abbrev-ref" "HEAD")))
    (when (supertag-git--ok-p result) (cdr result))))

(defun supertag-git-setup--push (root)
  "Push ROOT's current branch to `origin' with `-u' (so it starts tracking
it), per the plan's machine-1 journey. On success, returns
`(:status :ok :branch BRANCH)'. On failure (most commonly auth/network:
no ssh-agent, no credential helper configured, remote unreachable, ...),
returns `(:status :failed :branch BRANCH :error STRING)' -- this function
deliberately does NOT retry; a one-shot setup command retrying a push
that failed for a persistent reason (bad credentials, unreachable host)
would just hang or spam identically-failing attempts. `supertag-git-setup'
turns the `:failed' case into a diagnostic message pointing at manual
`git push' / ssh-agent / credential-helper troubleshooting; automatic
retry-on-reconnect is `supertag-git-sync-mode''s job, not this command's."
  (let ((branch (supertag-git-setup--current-branch root)))
    (unless branch
      (error "supertag-git-setup: could not determine the current branch to push"))
    (let ((result (supertag-git--run root "push" "-u" "origin" branch)))
      (if (supertag-git--ok-p result)
          (list :status :ok :branch branch)
        (list :status :failed :branch branch :error (cdr result))))))

;;;###autoload
(defun supertag-git-setup ()
  "Configure the current vault's git repository for semantic supertag-db merges.

Idempotent: safe to run again on this same clone (it will report what it
already had configured and skip re-adding duplicate lines), and this is
also the command every OTHER clone/machine must run once for itself --
`merge.supertag-db.driver' lives in `.git/config', which git never syncs
between clones (see the Commentary in this file for why).

Steps performed (each reported as it happens):
1. Locate `supertag-db-file''s directory; if it is not already inside a
   git worktree, offer to `git init' one -- at the single configured
   `org-supertag-sync-directories' root if there is exactly one and it
   already contains the database, otherwise by prompting (refusing
   outright, rather than guessing, when multiple sync roots are
   configured or no in-repo root can be inferred; see
   `supertag-git-setup--pick-root').
2. Write/append a `.gitattributes' entry marking the database's path for
   the `supertag-db' merge driver.
3. Write THIS CLONE's `.git/config' `merge.supertag-db.*' entries,
   pointing at wherever `supertag-merge.el' lives on THIS machine.
4. Append `.gitignore' entries (local-only state that must never be
   synced) next to the database file.
5. Prompt for an `origin' remote URL (pre-filled with whatever is already
   configured, if anything; empty input explicitly skips this and every
   remaining step below, leaving a valid local-only vault -- see
   `supertag-git-setup--configure-remote'). When a URL is given: configure
   it (`git remote add`, or `set-url` with confirmation if `origin'
   already pointed somewhere else), create the vault's initial commit via
   the exact same scoped-staging pathspec logic `supertag-git-sync-mode'
   uses for its own auto-commits (`supertag-git-setup--commit-all' /
   `supertag-git-sync--commit-pathspecs' -- never a bare `git add -A'),
   then `git push -u origin <current-branch>'. A push failure (almost
   always auth/network: no ssh-agent, no credential helper, unreachable
   host, ...) is reported as a diagnostic pointing at manual `git push'
   troubleshooting -- this step never retries.

Also performs, as its new Step 0 (S4), a one-time vault layout migration
when `supertag-db-file' lives outside any git repository but a single
`org-supertag-sync-directories' root is configured: the database is
copied to `<root>/.supertag/supertag-db.el', verified loadable, this
session's persistence wiring is switched over and the store reloaded from
the new location, and only then is the OLD file renamed (never deleted) to
a `.migrated-<timestamp>' tombstone -- see `supertag-git--migrate-layout'
for the full decision table and `supertag-git--do-migrate-layout' for the
failure-safety chain. Idempotent like everything else here: once migrated,
subsequent runs see the database already inside the repo and skip this
step. Still refuses outright (unchanged from before S4) if, after that
migration step, the database turns out to live outside the git repo it
finds/creates -- e.g. multiple sync roots configured (V1 limitation) or no
root at all with a non-interactive caller."
  (interactive)
  (unless (supertag-git--executable)
    (user-error "supertag-git-setup: `git' executable not found; install git first"))
  (let (report)
    (cl-flet ((note (fmt &rest args) (push (apply #'format fmt args) report)))
      ;; Step 0 (S4): layout migration. This may change `supertag-db-file'
      ;; out from under us, so everything below re-reads it fresh
      ;; afterward rather than reusing a value captured before this call.
      (let ((migration (supertag-git--migrate-layout)))
        (pcase (plist-get migration :status)
          (:skipped-in-repo
           (note "Database already inside a git repository; skipping the S4 layout migration."))
          (:skipped-already-under-root
           (note "Database already lives under the configured vault root %s; skipping layout migration (git init/configuration below will still run)."
                 (plist-get migration :repo-root)))
          (:migrated
           (note "Migrated database into the git vault layout: %s -> %s.%s"
                 (plist-get migration :old-db-file)
                 (plist-get migration :new-db-file)
                 (cond
                  ((plist-get migration :tombstone)
                   (format " Old file renamed to %s (kept as a tombstone, not deleted)."
                           (plist-get migration :tombstone)))
                  ((plist-get migration :fresh-vault) "")
                  (t " (old file could not be renamed to a tombstone; see the preceding message -- it is safely unused but still on disk)"))))
          (:failed
           (note "Layout migration to the git vault layout FAILED (old database left completely untouched): %s"
                 (plist-get migration :error)))
          (_ nil))) ; :skipped-no-root / :skipped-multi-root / :skipped-root-missing: nothing new to report here; the existing steps below handle or refuse these exactly as before S4
      (let* ((db-file (expand-file-name supertag-db-file))
             (db-dir (file-name-directory db-file)))
        (unless (file-directory-p db-dir)
          (make-directory db-dir t))
        (let ((toplevel (supertag-git--repo-toplevel db-dir)))
          (if toplevel
              (note "Already inside a git repository: %s" toplevel)
            (let ((root (supertag-git-setup--pick-root db-dir)))
              (note "No existing git repository found for %s." db-dir)
              (supertag-git--init-repo root)
              (note "Ran `git init` at %s." root)
              (setq toplevel (supertag-git--repo-toplevel db-dir))
              (unless toplevel
                (error "supertag-git-setup: `git init' at %s did not result in %s being inside a git worktree -- this should not happen"
                       root db-dir))))
          (let ((cfg (supertag-git--configure-clone toplevel db-file)))
            (note "Relative DB path within repo: %s" (plist-get cfg :relative-path))
            (note "%s"
                  (if (plist-get cfg :gitattributes-added)
                      (format ".gitattributes: added entry (%s)" (plist-get cfg :gitattributes-file))
                    ".gitattributes: entry already present (skipped)"))
            (note "merge.supertag-db.driver configured for THIS CLONE ONLY, using package dir %s. This setting lives in `.git/config' and is NEVER synced by git -- you must run `supertag-git-setup' again on every other clone/machine."
                  (plist-get cfg :driver-package-dir))
            (note "%s"
                  (if (plist-get cfg :gitignore-added)
                      (format ".gitignore: added %s (in %s)"
                              (plist-get cfg :gitignore-added)
                              (file-name-directory (plist-get cfg :gitignore-file)))
                    ".gitignore: entries already present (skipped)")))
          ;; Step 5 (S4 machine-1 journey): remote configuration + initial
          ;; commit + push. Gated together on a non-empty URL, per the
          ;; plan -- an explicit empty input skips ALL of this, leaving a
          ;; valid local-only vault (the merge driver above already works
          ;; for a single machine; nothing here is required for that).
          (let ((remote-url (supertag-git-setup--configure-remote toplevel)))
            (if (not remote-url)
                (note "No git remote configured -- this vault is local-only for now. Run `supertag-git-setup' again later once you have a remote URL, or configure one yourself (`git remote add origin <url>`) and push manually.")
              (note "Remote `origin' configured: %s" remote-url)
              (let ((committed (supertag-git-setup--commit-all
                                 toplevel (format "supertag-git-setup: initial commit (%s)" (system-name)))))
                (note "%s" (if committed
                               "Created the initial commit (org files, database, .gitattributes/.gitignore)."
                             "Nothing new to commit (already up to date)."))
                (let ((push-result (supertag-git-setup--push toplevel)))
                  (pcase (plist-get push-result :status)
                    (:ok
                     (note "Pushed branch `%s' to `origin' (now tracking it)."
                           (plist-get push-result :branch)))
                    (:failed
                     (note "`git push -u origin %s' FAILED: %s -- this is usually an auth or network problem. Test manually with `git push' in a shell at %s; check that your ssh-agent has the right key loaded (or your credential helper is configured) and that the remote is reachable. Setup will NOT retry automatically -- push manually once fixed, or re-run `supertag-git-setup', or enable `supertag-git-sync-mode' once a manual push works."
                           (plist-get push-result :branch)
                           (string-trim (plist-get push-result :error))
                           toplevel))))))))))
    (setq report (nreverse report))
    (dolist (line report) (message "supertag-git-setup: %s" line))
    (unless noninteractive
      (with-current-buffer (get-buffer-create supertag-git--report-buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Supertag Git Setup Report\n" (make-string 26 ?=) "\n\n")
          (dolist (line report) (insert "- " line "\n"))
          (goto-char (point-min)))
        (display-buffer (current-buffer))))
    report))

;;; --- S4 vault registry (V1 single-root) ---
;;
;; There is no separate "registry" data structure in this codebase (see the
;; migration Commentary above): `org-supertag-sync-directories' -- a plain
;; list of vault root directories -- IS the registry. Registering a newly
;; cloned vault means appending its root to that list (if not already
;; there) and pointing this session's persistence wiring at
;; `<root>/.supertag/', reusing the exact same
;; `supertag-git--apply-data-directory-wiring' the layout migration above
;; uses.

(defun supertag-git--register-vault-root (root)
  "Best-effort register ROOT (a freshly cloned repository) as this
session's git-sync vault: append it to `org-supertag-sync-directories'
\(if bound and not already present) and switch persistence wiring to
`<ROOT>/.supertag/'. Degrades to just the wiring switch when
`org-supertag-sync-directories' is not bound (e.g. `org-supertag.el' /
`supertag-services-sync.el' not loaded in this session)."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (when (boundp 'org-supertag-sync-directories)
      (let ((supertag--config-guard-allow t)
            (current (and (listp org-supertag-sync-directories)
                          org-supertag-sync-directories)))
        (unless (member root (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
                                     current))
          (setq org-supertag-sync-directories (append current (list root))))))
    (supertag-git--apply-data-directory-wiring (expand-file-name ".supertag/" root))
    root))

;;; --- supertag-git-clone: interactive command (S4, machine N) ---

(defun supertag-git--run-clone (remote-url local-dir)
  "Run `git clone REMOTE-URL LOCAL-DIR'. Signals an error including git's
own stderr/stdout on failure -- never silently continues past a failed
clone into configuring a nonexistent repository."
  (unless (supertag-git--executable)
    (error "supertag-git-clone: `git' executable not found; install git first"))
  (let ((parent (file-name-directory (directory-file-name (expand-file-name local-dir)))))
    (when (and parent (not (file-directory-p parent)))
      (make-directory parent t)))
  (with-temp-buffer
    (let ((exit (call-process (supertag-git--executable) nil t nil
                               "clone" remote-url (directory-file-name (expand-file-name local-dir)))))
      (unless (= exit 0)
        (error "supertag-git-clone: `git clone %s %s' failed: %s"
               remote-url local-dir (string-trim (buffer-string)))))))

(defun supertag-git--rebuild-from-org-files ()
  "Rebuild the database from the vault's org files via the existing sync
scanner (`supertag-sync-full-rescan'), used by `supertag-git-clone' when
the cloned repository's database is missing or fails to parse. Returns
non-nil (the scanner's own return value, whatever that is) on success, or
nil when the scanner is not loaded in this session (`supertag-services-sync.el'
not required -- see this file's Commentary on package independence); the
caller reports that degraded case to the user rather than silently doing
nothing."
  (when (fboundp 'supertag-sync-full-rescan)
    (funcall #'supertag-sync-full-rescan)))

(defun supertag-git--current-node-tag-counts ()
  "Return (NODE-COUNT . TAG-COUNT) from the live in-memory store, or (0 . 0)
if the relevant collections are not (yet) hash tables."
  (let* ((nodes (ignore-errors (supertag-store-get-collection :nodes)))
         (tags (ignore-errors (supertag-store-get-collection :tags))))
    (cons (if (hash-table-p nodes) (hash-table-count nodes) 0)
          (if (hash-table-p tags) (hash-table-count tags) 0))))

;;;###autoload
(defun supertag-git-clone (remote-url local-directory)
  "Clone REMOTE-URL into LOCAL-DIRECTORY as a new Org-Supertag git vault.

This is machine N of the plan's \"一个 URL 完成配置\" (\"one URL to
finish setup\") user journey -- the counterpart to `supertag-git-setup' on
machine 1, which already pushed a vault laid out per
`supertag-git--migrate-layout''s `<repo-root>/.supertag/supertag-db.el'
convention.

Steps:
1. `git clone REMOTE-URL LOCAL-DIRECTORY' (refuses if LOCAL-DIRECTORY
   already exists and is non-empty, rather than cloning into it and
   producing a confusing git error).
2. Configure THIS clone's merge driver + `.gitattributes'/`.gitignore' for
   `<LOCAL-DIRECTORY>/.supertag/supertag-db.el'
   (`supertag-git--configure-clone', the exact same per-clone step
   `supertag-git-setup' performs -- required on every machine regardless
   of who ran `supertag-git-setup' originally, see this file's Commentary
   on why).
3. Register LOCAL-DIRECTORY as this session's vault
   (`supertag-git--register-vault-root').
4. Load the database from `.supertag/supertag-db.el' if it exists and
   parses; otherwise (missing, e.g. a fresh empty remote, or corrupt)
   rebuild it from the cloned org files via the existing sync scanner
   (`supertag-git--rebuild-from-org-files').
5. Report the resulting node/tag counts.

Returns a plist: (:repo-root ROOT :db-file FILE :loaded BOOL :rebuilt BOOL
:node-count N :tag-count M)."
  (interactive
   (list (read-string "Git remote URL: ")
         (expand-file-name (read-directory-name "Clone into directory: " default-directory))))
  (let* ((local (file-name-as-directory (expand-file-name local-directory))))
    (when (and (file-directory-p local)
               (directory-files local nil directory-files-no-dot-files-regexp))
      (user-error "supertag-git-clone: %s already exists and is not empty" local))
    (supertag-git--run-clone remote-url local)
    (message "supertag-git-clone: cloned %s into %s." remote-url local)
    (let* ((data-dir (expand-file-name ".supertag/" local))
           (db-file (expand-file-name "supertag-db.el" data-dir)))
      (supertag-git--configure-clone local db-file)
      (message "supertag-git-clone: configured the semantic merge driver for THIS clone.")
      (supertag-git--register-vault-root local)
      (let* ((parses-p (and (file-exists-p db-file)
                            (condition-case nil
                                (hash-table-p (supertag--persistence--try-read-store db-file))
                              (error nil))))
             (loaded nil)
             (rebuilt nil))
        (if parses-p
            (progn
              (when (fboundp 'supertag-load-store)
                (funcall #'supertag-load-store))
              (setq loaded t)
              (message "supertag-git-clone: database loaded from %s." db-file))
          (message "supertag-git-clone: database at %s is missing or unreadable; rebuilding from the cloned org files..."
                   db-file)
          (if (supertag-git--rebuild-from-org-files)
              (progn
                (setq rebuilt t)
                (when (fboundp 'supertag-save-store)
                  (funcall #'supertag-save-store))
                (message "supertag-git-clone: rebuild complete."))
            (message "supertag-git-clone: sync scanner not loaded in this session; once org-supertag has fully started, run `M-x supertag-sync-full-rescan' manually to rebuild.")))
        (let* ((counts (supertag-git--current-node-tag-counts))
               (node-count (car counts))
               (tag-count (cdr counts)))
          (message "supertag-git-clone: done -- %d node(s), %d tag(s) in %s."
                   node-count tag-count local)
          (list :repo-root local :db-file db-file :loaded loaded :rebuilt rebuilt
                :node-count node-count :tag-count tag-count))))))

;;; --- supertag-git-sync-mode: automatic commit/fetch/merge/push loop (S4) ---
;;
;; Off by default (opt-in). Correctness lives entirely in the merge driver
;; (S3/supertag-merge.el) and the vault layout (S4 above) -- this mode is
;; purely a convenience automation layer on top of both; a user who prefers
;; to `magit-push'/`magit-pull' by hand gets identical correctness with
;; this mode left off.
;;
;; ## Serialization (no queue engineering, per the plan)
;;
;; Exactly one git operation chain (commit-then-push, or fetch-then-maybe-
;; merge, or a push's own fetch/merge/retry) runs at a time, guarded by the
;; single flag `supertag-git-sync--in-flight'. A trigger that arrives while
;; busy is simply DROPPED -- not queued -- on the understanding that the
;; next timer tick (pull every `supertag-git-sync-pull-interval' seconds;
;; commit debounce restarts on every new change anyway) will catch
;; whatever was missed. This is a deliberate simplicity choice, not an
;; oversight.
;;
;; ## Async, never blocking (with a synchronous escape hatch for tests)
;;
;; All git subprocess calls in this section go through
;; `supertag-git-sync--run-git', which uses `make-process' (never
;; blocking the UI thread) UNLESS `supertag-git-sync--synchronous' is
;; non-nil, in which case it uses a blocking `call-process' instead. Batch
;; ERT tests cannot pump Emacs' normal command loop the way an interactive
;; session does (process sentinels only run between commands / during
;; `sit-for'/`accept-process-output'), so this file's own tests `let'-bind
;; that variable to drive the exact same callback-continuation code
;; deterministically, per this task's explicit instructions. Production
;; code never sets it.

(defgroup supertag-git-sync nil
  "Automatic git commit/fetch/merge/push loop for Org-Supertag git vaults."
  :group 'supertag-git)

(defcustom supertag-git-sync-pull-interval 300
  "Seconds between automatic background `git fetch' (+ merge if behind)
attempts while `supertag-git-sync-mode' is enabled. Also triggered (at
most once every `supertag-git-sync-focus-pull-min-interval' seconds) when
Emacs regains focus."
  :type 'integer
  :group 'supertag-git-sync)

(defcustom supertag-git-sync-commit-debounce 30
  "Seconds of quiet after the LAST detected change before
`supertag-git-sync-mode' auto-commits. A single timer that restarts on
every new change (never accumulates multiple pending timers) -- see
`supertag-git-sync--schedule-commit'."
  :type 'integer
  :group 'supertag-git-sync)

(defcustom supertag-git-sync-focus-pull-min-interval 60
  "Minimum seconds between two focus-triggered pulls (rate limit)."
  :type 'integer
  :group 'supertag-git-sync)

(defvar supertag-git-sync--synchronous nil
  "When non-nil, `supertag-git-sync--run-git' runs git synchronously
\(`call-process') instead of asynchronously (`make-process'). See this
section's Commentary for why batch ERT tests need this; production code
must never set it globally (tests `let'-bind it for the duration of one
test only).")

(defvar supertag-git-sync--vault-root nil
  "Repo root this session's `supertag-git-sync-mode' operates on, captured
via `supertag-git-check' when the mode is enabled. Nil when the mode is
off.")

(defvar supertag-git-sync--commit-timer nil
  "The single pending debounce timer for the next auto-commit, or nil.")

(defvar supertag-git-sync--pull-timer nil
  "The repeating timer for periodic fetch(+merge), or nil.")

(defvar supertag-git-sync--last-focus-pull-time nil
  "`float-time' of the last focus-triggered pull attempt, or nil.")

(defvar supertag-git-sync--in-flight nil
  "Non-nil while a git operation chain is already running for this vault.
Guards ENTRY only -- see this section's Commentary on serialization.")

(defvar supertag-git-sync--pending-push-count 0
  "DISPLAY CACHE ONLY, shown in the modeline lighter (e.g. \" STG\\u21913\").
Not a source of truth: it is refreshed from git's own ahead-of-upstream
commit count (`git rev-list --count @{upstream}..HEAD') every pull cycle
\(`supertag-git-sync--refresh-pending-count', called from
`supertag-git-sync--pull' and once immediately when the mode is enabled)
-- 0 when there is no upstream configured at all (a local-only vault,
which also means no push is ever attempted). An earlier version of this
variable was instead incremented by hand on every local commit and reset
on every successful push, which meant it always LIED as 0 immediately
after any Emacs restart regardless of how many real unpushed commits
existed -- see this section's Commentary and P1-7 in the review.")

(defvar supertag-git-sync--conflicted-org-files nil
  "Absolute paths of org files, among the most recent merge's changed
paths, that still contain unresolved git conflict markers. Excluded from
sync-scanner import (see the advice on `supertag-sync--process-single-file'
below) and listed by `supertag-doctor'.")

(defvar supertag-git-sync--offline-warned nil
  "Non-nil once a fetch/push failure has been reported for the CURRENT
offline episode. Reset to nil the next time an operation succeeds, so
degrading offline and recovering are each reported exactly once (never
once per retry) -- see `supertag-git-sync--note-offline'.")

;; Forward declaration: the real definition (with its full docstring) is
;; the `define-minor-mode' near the end of this section -- declared here,
;; ahead of `supertag-git-sync--enable'/`--disable' below which both
;; `setq'/read it, purely so the byte-compiler knows it is a genuine
;; special variable at the point those functions are compiled (a plain,
;; value-less `defvar' does not change the value `define-minor-mode' later
;; installs).
(defvar supertag-git-sync-mode nil)

;;; --- Async git runner ---

(defun supertag-git-sync--run-git (dir args callback)
  "Run `git -C DIR ARGS' and call CALLBACK with (EXIT-CODE . OUTPUT).
Asynchronous (`make-process', never blocking) unless
`supertag-git-sync--synchronous' is non-nil, in which case it blocks via
`call-process' and calls CALLBACK before returning -- see this section's
Commentary. For the async path CALLBACK runs once, from the process
sentinel, after the process has fully exited."
  (let ((git (supertag-git--executable)))
    (unless git
      (funcall callback (cons 1 "git executable not found")))
    (if supertag-git-sync--synchronous
        (let ((result (with-temp-buffer
                        (let ((exit (apply #'call-process git nil t nil "-C" dir args)))
                          (cons exit (buffer-string))))))
          (funcall callback result))
      (let ((buf (generate-new-buffer " *supertag-git-sync*")))
        (make-process
         :name "supertag-git-sync"
         :buffer buf
         :command (append (list git "-C" dir) args)
         :noquery t
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             (let* ((exit (process-exit-status proc))
                    (output (with-current-buffer (process-buffer proc) (buffer-string))))
               (when (buffer-live-p (process-buffer proc))
                 (kill-buffer (process-buffer proc)))
               (funcall callback (cons exit output))))))))))

;;; --- Offline degradation messaging (one message per state change) ---

(defun supertag-git-sync--note-offline (op)
  "Report an OP (\"fetch\" or \"push\") failure exactly once per offline
episode, not once per retry."
  (unless supertag-git-sync--offline-warned
    (setq supertag-git-sync--offline-warned t)
    (message "supertag-git-sync: %s failed (offline, or remote unreachable?) -- staying local; will retry automatically. Pending local commits: %d."
             op supertag-git-sync--pending-push-count)))

(defun supertag-git-sync--clear-offline-warning ()
  "Report recovery exactly once, the first time an operation succeeds
again after `supertag-git-sync--note-offline' fired."
  (when supertag-git-sync--offline-warned
    (setq supertag-git-sync--offline-warned nil)
    (message "supertag-git-sync: back online.")))

(defun supertag-git-sync--report-failure (op result)
  "Report a non-offline-looking git failure (e.g. `git add'/`git commit'
themselves failing, which is unusual and worth a message every time,
unlike the expected/common fetch-push offline case)."
  (message "supertag-git-sync: %s failed: %s" op (string-trim (cdr result))))

;;; --- Conflict detection (shared: commit-time refusal, doctor, post-merge) ---
;;
;; All of this is deliberately computed FRESH from git's own live state
;; every time it is asked for, never trusted from a cached/session-local
;; variable alone -- `supertag-git-sync--conflicted-org-files' below is
;; still maintained as a cache (existing callers/`supertag-doctor' read
;; it), but it is session-local Lisp state that vanishes on an Emacs
;; restart while an unresolved conflicted repository on disk does not.
;; `supertag-git-sync--fire-commit' (refusing to auto-commit) and
;; `supertag-doctor' (reporting current status, including after a
;; restart) both call `supertag-git-sync--unmerged-paths'/
;; `supertag-git-sync--live-conflicted-org-files' directly instead of
;; relying on whatever this session happens to remember.

(defun supertag-git-sync--unmerged-paths (root)
  "Return paths still marked unmerged (conflicted) in ROOT's index
\(`git diff --name-only --diff-filter=U'). This works whether or not the
overall `git merge' command itself reported success: a merge DRIVER can
leave the database path cleanly, automatically resolved (and hence absent
from this list, even with `:sync-conflicts' recorded inside it -- see
supertag-merge.el) while some OTHER path in the SAME merge -- typically
plain `.org' prose, which has no merge driver -- is left with literal
conflict markers, an unmerged index entry, and a non-zero overall `git
merge' exit code. `ORIG_HEAD..HEAD' is deliberately NOT used here (unlike
an earlier version of this function): a merge left mid-conflict never
advances HEAD at all, so that diff would see nothing.

Also exactly the check `supertag-git-sync--fire-commit' uses BEFORE ever
staging anything: an unresolved conflict from a merge that happened in a
PREVIOUS Emacs session (or outside Emacs entirely, e.g. `git pull` on the
command line) leaves the same unmerged index entries, and must block
auto-commit exactly the same way."
  (let ((result (supertag-git--run root "diff" "--name-only" "--diff-filter=U")))
    (when (supertag-git--ok-p result)
      (split-string (cdr result) "\n" t))))

(defun supertag-git-sync--file-has-conflict-markers-p (file)
  "Return non-nil if FILE contains literal, unresolved git conflict
markers. Reuses `supertag--persistence--buffer-has-conflict-markers-p'
\(already proven correct for the DB loader's own guard -- see
supertag-core-persistence.el) rather than re-implementing the same
three-line-prefix scan; that function only looks at buffer text, so it
works identically for an org file as for the database file."
  (with-temp-buffer
    (insert-file-contents file)
    (supertag--persistence--buffer-has-conflict-markers-p)))

(defun supertag-git-sync--live-conflicted-org-files (root)
  "Return `.org' files under ROOT that are still unmerged (per
`supertag-git-sync--unmerged-paths'), computed FRESH from git's current
index state every time this is called.  Text markers are not required:
modify/delete and rename/delete conflicts have unmerged index entries but
may contain no marker text at all.  Callable at any time (right after a
merge, from `supertag-git-sync--fire-commit', or from `supertag-doctor'
in a session that never even turned `supertag-git-sync-mode' on), unlike
the session-local `supertag-git-sync--conflicted-org-files' cache, which only
ever gets written by `supertag-git-sync--after-merge' and is simply gone
after an Emacs restart regardless of what is actually on disk."
  (let (conflicted)
    (dolist (rel (supertag-git-sync--unmerged-paths root))
      (when (string-match-p "\\.org\\'" rel)
        (push (expand-file-name rel root) conflicted)))
    (nreverse conflicted)))

(defvar supertag-git-sync--conflict-commit-warned nil
  "Non-nil once `supertag-git-sync--fire-commit' has already reported a
refusal to auto-commit for the CURRENT unresolved-conflict episode (either
an unmerged path found before staging, or a literal conflict marker found
in staged content after it). Reset to nil once no unmerged paths remain,
so the refusal is reported once per episode, not once per debounce retry
-- the same one-message-per-state-change pattern as
`supertag-git-sync--offline-warned'.")

(defun supertag-git-sync--note-commit-refused (reason)
  "Report REASON (a human-readable string) for refusing to auto-commit,
exactly once per unresolved-conflict episode."
  (unless supertag-git-sync--conflict-commit-warned
    (setq supertag-git-sync--conflict-commit-warned t)
    (message "supertag-git-sync: refusing to auto-commit -- %s" reason)))

(defun supertag-git-sync--clear-commit-conflict-warning ()
  "Clear the one-shot refusal warning once the conflict episode is over."
  (when supertag-git-sync--conflict-commit-warned
    (setq supertag-git-sync--conflict-commit-warned nil)))

;;; --- Commit (debounced) ---

(defun supertag-git-sync--schedule-commit ()
  "(Re)start the single debounce timer for the next auto-commit. Any
already-pending timer is cancelled first, so the commit fires
`supertag-git-sync-commit-debounce' seconds after the LAST change, never
the first -- per the plan's \"debounce 30s\"."
  (when supertag-git-sync--vault-root
    (when supertag-git-sync--commit-timer
      (cancel-timer supertag-git-sync--commit-timer))
    (setq supertag-git-sync--commit-timer
          (run-with-timer supertag-git-sync-commit-debounce nil
                           #'supertag-git-sync--fire-commit))))

(defun supertag-git-sync--staged-conflict-markers-p (root)
  "Return non-nil if the content just staged in ROOT's index
\(`git diff --cached') introduces literal git conflict markers. A
belt-and-suspenders check run AFTER staging, in case something slipped
past the pre-stage `supertag-git-sync--unmerged-paths' guard in
`supertag-git-sync--fire-commit' (e.g. conflict-marker-shaped text
introduced some other way, not by an actual in-progress git merge). Only
lines the diff ADDS (a leading `+') count, so a marker that already existed
in a previous commit's context lines is not a false positive."
  (let ((result (supertag-git--run root "diff" "--cached")))
    (unless (supertag-git--ok-p result)
      (error "could not inspect staged content: %s" (string-trim (cdr result))))
    (string-match-p "^\\+<<<<<<< \\|^\\+=======$\\|^\\+>>>>>>> "
                    (cdr result))))

(defun supertag-git-sync--pathspec-matches-p (root pathspec)
  "Return non-nil if PATHSPEC matches at least one path git already knows
about in ROOT (tracked, modified, deleted, or untracked-but-not-ignored).

This exists purely to work around a real `git add' behavior (verified
against a real git binary, not assumed): `git add -A -- PATHSPEC' FATALLY
errors with a \"did not match any files\" message -- staging NOTHING, not
even the other pathspecs in the same invocation -- when
PATHSPEC (literal or `*.org'-style glob) matches nothing at all on disk,
even under `-A'. A brand-new vault with no `.org' files yet, or one whose
`.gitignore' has not been written yet, are both real cases this guards
against, not merely defensive: see `supertag-git-sync--commit-pathspecs',
which uses this to filter its candidate list down to only what actually
exists before ever invoking `git add'."
  (let ((result (supertag-git--run root "ls-files" "--cached" "--others"
                                    "--deleted" "--exclude-standard" "--"
                                    pathspec)))
    (and (supertag-git--ok-p result) (> (length (cdr result)) 0))))

(defun supertag-git-sync--commit-candidate-pathspecs (root)
  "Return the complete allowlist of pathspecs auto-commit may own in ROOT."
  (let* ((true-root (supertag-git--truename-dir root))
         (db-file (file-truename (expand-file-name supertag-db-file)))
         (db-dir (file-name-directory db-file)))
    (delete-dups
     (list "*.org"
           (file-relative-name db-file true-root)
           (file-relative-name (file-truename (expand-file-name ".gitattributes" root))
                               true-root)
           (file-relative-name (file-truename (expand-file-name ".gitignore" db-dir))
                               true-root)))))

(defun supertag-git-sync--commit-pathspecs (root)
  "Return the currently matching auto-commit pathspecs under ROOT.
The complete allowlist comes from
`supertag-git-sync--commit-candidate-pathspecs'; unmatched candidates are
filtered because `git add' rejects the entire invocation when one literal
pathspec matches nothing.

Deliberately narrow -- replacing an earlier, unconditional `git add -A'
-- per this mode's docstring: only `.org' files under ROOT (recursively,
via git's own unanchored `*.org' pathspec matching -- this is passed
straight to `call-process'/`make-process', never a shell, so the glob is
never shell-expanded and DOES match recursively), `supertag-db-file'
itself, and the two `supertag-git-setup'-managed config files
\(`.gitattributes' at ROOT, `.gitignore' next to the database -- see
`supertag-git--configure-clone') are ever staged. Any OTHER untracked file
inside the vault -- credentials, attachments, IDE/OS junk, ... -- is
deliberately left alone and NEVER swept into an automatic commit.

Both ROOT and every file path below are resolved through `file-truename'
before computing a relative pathspec -- ROOT here is typically
`supertag-git-sync--vault-root', which came from `supertag-git-check' /
`supertag-git--repo-toplevel', itself ALWAYS a truename (see
`supertag-git--truename-dir'). Without matching that on the file side
too, a `supertag-db-file' that is lexically equal but not
truename-identical to ROOT (e.g. one side under a symlinked `/tmp' or
`/var' on macOS, the other resolved through it) produces a bogus,
repository-escaping `../../...' relative pathspec that git rejects
outright -- silently dropping the database from every auto-commit."
  (let ((candidates (supertag-git-sync--commit-candidate-pathspecs root)))
    (cl-remove-if-not (lambda (p) (supertag-git-sync--pathspec-matches-p root p))
                       candidates)))

(defun supertag-git-sync--auto-commit-path-p (root path)
  "Return non-nil when repository-relative PATH is owned by auto-commit."
  (or (string-match-p "\\.org\\'" path)
      (member path (cdr (supertag-git-sync--commit-candidate-pathspecs root)))))

(defun supertag-git-sync--fire-commit ()
  "The debounce timer function: scoped `git add -A --' (never a bare,
unconditional `git add -A' -- see `supertag-git-sync--commit-pathspecs')
then commit ONLY if something is actually staged (`git diff --cached
--quiet' guard -- never an empty commit), then push. Batch tests call
this directly (never wait for the real timer), per this task's
instructions. A no-op when no vault is active or another git op is
already in flight (the next timer tick will catch up, per this section's
Commentary on serialization).

Refuses to stage or commit ANYTHING, entirely, when ROOT's index still
has unresolved conflicted (unmerged) paths from a previous merge --
whether that merge happened in this session, an earlier Emacs session, or
outside Emacs entirely (e.g. a manual `git pull'); staging over that with
`git add' would mark the conflict resolved and commit literal `<<<<<<<'
markers into history. As a second, belt-and-suspenders check AFTER
staging (in case something conflict-marker-shaped slipped past the first
one), the newly staged content itself is scanned for literal conflict
markers and, if found, only this mode's pathspecs are unstaged rather than
committed. All refusals are reported at most once per episode (see
`supertag-git-sync--note-commit-refused')."
  (setq supertag-git-sync--commit-timer nil)
  (when (and supertag-git-sync--vault-root (not supertag-git-sync--in-flight))
    (let* ((root supertag-git-sync--vault-root)
           (unmerged (supertag-git-sync--unmerged-paths root))
           (staged-result (supertag-git--run root "diff" "--cached" "--name-only" "-z"))
           (staged-paths (and (supertag-git--ok-p staged-result)
                              (split-string (cdr staged-result) "\0" t)))
           (outside-staged
            (and staged-paths
                 (cl-remove-if
                  (lambda (path) (supertag-git-sync--auto-commit-path-p root path))
                  staged-paths))))
      (cond
       (unmerged
          (supertag-git-sync--note-commit-refused
           (format "%d path(s) still unresolved from a merge conflict: %s. Resolve manually (magit / `git checkout --merge'), then the next debounce cycle will commit normally."
                   (length unmerged) (mapconcat #'identity unmerged ", "))))
       ((not (supertag-git--ok-p staged-result))
        (supertag-git-sync--note-commit-refused
         (format "could not inspect the existing git index safely: %s"
                 (string-trim (cdr staged-result)))))
       (outside-staged
        (supertag-git-sync--note-commit-refused
         (format "the git index already contains out-of-scope path(s): %s. Commit or unstage them manually first; their staged state was left untouched."
                 (mapconcat #'identity outside-staged ", "))))
       (t
        (setq supertag-git-sync--in-flight t)
        (let ((pathspecs (supertag-git-sync--commit-pathspecs root)))
          (if (not pathspecs)
              ;; Nothing this mode is willing to stage currently exists on
              ;; disk (e.g. a brand-new vault with no `.org' files yet) --
              ;; nothing to commit, exactly like the "nothing staged" case
              ;; below.
              (progn
                (supertag-git-sync--clear-commit-conflict-warning)
                (setq supertag-git-sync--in-flight nil))
            (supertag-git-sync--run-git
             root (append (list "add" "-A" "--") pathspecs)
             (lambda (add-result)
               (if (not (supertag-git--ok-p add-result))
                   (progn (setq supertag-git-sync--in-flight nil)
                          (supertag-git-sync--report-failure "git add -A" add-result))
                 (supertag-git-sync--run-git
                  root (list "diff" "--cached" "--quiet")
                  (lambda (diff-result)
                    (cond
                     ((supertag-git--ok-p diff-result)
                      ;; Exit 0 = nothing staged -- nothing to commit.
                      (supertag-git-sync--clear-commit-conflict-warning)
                      (setq supertag-git-sync--in-flight nil))
                     ((/= 1 (car diff-result))
                      (apply #'supertag-git--run root
                             (append (list "reset" "--") pathspecs))
                      (supertag-git-sync--note-commit-refused
                       (format "could not inspect the staged diff safely: %s"
                               (string-trim (cdr diff-result))))
                      (setq supertag-git-sync--in-flight nil))
                     (t
                      (condition-case err
                          (if (supertag-git-sync--staged-conflict-markers-p root)
                              (progn
                                (apply #'supertag-git--run root
                                       (append (list "reset" "--") pathspecs))
                                (supertag-git-sync--note-commit-refused
                                 "staged content still contains literal git conflict markers (<<<<<<< / ======= / >>>>>>>) -- unstaged; resolve manually, then the next debounce cycle will retry.")
                                (setq supertag-git-sync--in-flight nil))
                            (supertag-git-sync--clear-commit-conflict-warning)
                            (let ((commit-message
                                   (format "supertag-sync: %s %s" (system-name)
                                           (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
                              (supertag-git-sync--run-git
                               root (list "commit" "-q" "-m" commit-message)
                               (lambda (commit-result)
                                 (if (not (supertag-git--ok-p commit-result))
                                     (progn (setq supertag-git-sync--in-flight nil)
                                            (supertag-git-sync--report-failure "git commit" commit-result))
                                   ;; Refresh the ahead-count display cache
                                   ;; from git itself (never a manual
                                   ;; increment -- see
                                   ;; `supertag-git-sync--pending-push-count')
                                   ;; before pushing; we just committed, so
                                   ;; this is guaranteed to be >= 1 (or 0,
                                   ;; local-only, if there is no upstream --
                                   ;; `supertag-git-sync--push' below will
                                   ;; then simply fail harmlessly and report
                                   ;; offline/no-remote).
                                   (supertag-git-sync--refresh-pending-count root)
                                   (supertag-git-sync--push root))))))
                        (error
                         (apply #'supertag-git--run root
                                (append (list "reset" "--") pathspecs))
                         (supertag-git-sync--note-commit-refused
                          (error-message-string err))
                         (setq supertag-git-sync--in-flight nil)))))))))))))))))

;;; --- Push, with one fetch/merge/retry on rejection ---

(defun supertag-git-sync--rejected-p (result)
  "Return non-nil if RESULT (from a `git push') looks like an ordinary
non-fast-forward rejection (remote advanced -- retry via fetch+merge+push
makes sense), as opposed to a network/auth failure (retrying the exact
same push would just fail again the same way)."
  (and (not (supertag-git--ok-p result))
       (string-match-p "rejected\\|non-fast-forward\\|fetch first\\|fetch-first"
                       (cdr result))))

(defun supertag-git-sync--push (root)
  "Push ROOT's current branch. Called with `supertag-git-sync--in-flight'
already t. On success: reset the pending-push counter and clear any
offline warning. On an ordinary rejection (remote advanced): fetch, merge
\(never rebase, per the plan), and retry the push exactly ONCE; if that
retry still fails, give up silently for this cycle -- the pending count
display cache is left as whatever it was most recently refreshed to
\(still accurate: nothing here changed how many commits are actually
ahead), shown in the modeline, and the next pull cycle's own
ahead/behind refresh (`supertag-git-sync--refresh-pending-count', see
`supertag-git-sync--maybe-push-after-cycle') will both correct the
display and retry the push. On anything else (network/auth failure):
treat as offline degradation. Always clears
`supertag-git-sync--in-flight' exactly once, on every branch."
  (supertag-git-sync--run-git
   root (list "push")
   (lambda (push-result)
     (cond
      ((supertag-git--ok-p push-result)
       (setq supertag-git-sync--pending-push-count 0)
       (supertag-git-sync--clear-offline-warning)
       (setq supertag-git-sync--in-flight nil))
      ((supertag-git-sync--rejected-p push-result)
       (supertag-git-sync--run-git
        root (list "fetch")
        (lambda (fetch-result)
          (if (not (supertag-git--ok-p fetch-result))
              (progn (supertag-git-sync--note-offline "push (fetch during retry)")
                     (setq supertag-git-sync--in-flight nil))
            (supertag-git-sync--run-git
             root (list "merge" "--no-edit" "@{upstream}")
             (lambda (merge-result)
               (supertag-git-sync--after-merge merge-result root)
               (if (not (supertag-git--ok-p merge-result))
                   (setq supertag-git-sync--in-flight nil)
                 (supertag-git-sync--run-git
                  root (list "push")
                  (lambda (retry-result)
                    (if (supertag-git--ok-p retry-result)
                        (progn (setq supertag-git-sync--pending-push-count 0)
                               (supertag-git-sync--clear-offline-warning))
                      nil) ; give up silently for this cycle; pending count stays
                    (setq supertag-git-sync--in-flight nil))))))))))
      (t
       (supertag-git-sync--note-offline "push")
       (setq supertag-git-sync--in-flight nil))))))

;;; --- Ahead/behind (derived fresh from git every cycle -- P1-7) ---
;;
;; Neither `supertag-git-sync--pending-push-count' (the modeline display
;; cache) nor whether to push at all is ever decided from session-local
;; bookkeeping: both are derived, every cycle, from git's own
;; `rev-list --count' against `@{upstream}' -- exactly the ahead/behind
;; numbers `git status' itself would report. This is what makes the
;; count survive an Emacs restart (a fresh session has no memory of
;; commits made -- or pushes attempted and dropped -- in a previous one,
;; but git's own history on disk does), and what makes offline recovery
;; automatic: reconnect, and the very next pull cycle both fetches AND
;; pushes whatever accumulated locally while offline, without waiting for
;; a brand new local edit to trigger `supertag-git-sync--fire-commit''s
;; own push. `rev-list --count' against `@{upstream}' is purely local git
;; plumbing (no network access -- it reads the last-fetched remote-
;; tracking ref, not the remote itself), so, like the pre-existing
;; `supertag-git-sync--behind-p' this generalizes, it is always run
;; synchronously via `supertag-git--run' even from within an async
;; callback chain.

(defun supertag-git-sync--rev-count (root range)
  "Return the integer count from `git rev-list --count RANGE' in ROOT, or
nil if the command fails -- in particular when RANGE references
`@{upstream}' and the current branch has no upstream configured at all
\(a local-only vault: this is the signal every caller below uses to mean
\"no push is ever attempted, and the lighter shows nothing pending\")."
  (let ((result (supertag-git--run root "rev-list" "--count" range)))
    (and (supertag-git--ok-p result) (string-to-number (string-trim (cdr result))))))

(defun supertag-git-sync--behind-p (root)
  "Return non-nil if ROOT's HEAD is behind its upstream, immediately after
a fetch."
  (let ((n (supertag-git-sync--rev-count root "HEAD..@{upstream}")))
    (and n (> n 0))))

(defun supertag-git-sync--refresh-pending-count (root)
  "Recompute ROOT's ahead-of-upstream commit count via git and refresh the
DISPLAY CACHE `supertag-git-sync--pending-push-count' from it (0 when
there is no upstream configured at all). Returns the ahead count (an
integer), or nil when there is no upstream."
  (let ((ahead (supertag-git-sync--rev-count root "@{upstream}..HEAD")))
    (setq supertag-git-sync--pending-push-count (or ahead 0))
    ahead))

(defun supertag-git-sync--maybe-push-after-cycle (root)
  "End-of-cycle step: refresh the ahead-count display cache and push when
ahead > 0. Shared by `supertag-git-sync--pull' (both its \"nothing to
merge\" and \"just merged\" paths) and `supertag-git-sync--enable' (an
immediate catch-up check right when the mode turns on) -- this is what
lets commits accumulated while offline, or a commit whose OWN push never
finished before a previous Emacs session ended, get pushed on the very
next cycle once connectivity/an upstream returns, rather than silently
sitting local until some unrelated new edit happens to trigger
`supertag-git-sync--fire-commit''s own push. Always clears
`supertag-git-sync--in-flight' exactly once (`supertag-git-sync--push'
does so itself on every branch when it runs; this function does so
directly on the no-push branch)."
  (let ((ahead (supertag-git-sync--refresh-pending-count root)))
    (if (and ahead (> ahead 0))
        (supertag-git-sync--push root)
      (setq supertag-git-sync--in-flight nil))))

;;; --- Pull (timer + rate-limited focus-in) ---

(defun supertag-git-sync--pull ()
  "One fetch (+ merge if behind, + push if ahead) cycle. A no-op when no
vault is active or another git op is already in flight.

The trailing \"+ push if ahead\" step (P1-7) is what covers OFFLINE
RECOVERY: a run of local commits made while the remote was unreachable
(each individual push attempt having failed and degraded silently, per
this mode's offline handling) are not orphaned forever waiting for a new
edit -- reconnecting means the very next timer tick's fetch succeeds,
and `supertag-git-sync--maybe-push-after-cycle' then pushes everything
that piled up, whether or not anything was behind to merge first."
  (when (and supertag-git-sync--vault-root (not supertag-git-sync--in-flight))
    (setq supertag-git-sync--in-flight t)
    (let ((root supertag-git-sync--vault-root))
      (supertag-git-sync--run-git
       root (list "fetch")
       (lambda (fetch-result)
         (if (not (supertag-git--ok-p fetch-result))
             (progn (supertag-git-sync--note-offline "fetch")
                    (setq supertag-git-sync--in-flight nil))
           (supertag-git-sync--clear-offline-warning)
           (if (supertag-git-sync--behind-p root)
               (supertag-git-sync--run-git
                root (list "merge" "--no-edit" "@{upstream}")
                (lambda (merge-result)
                  (supertag-git-sync--after-merge merge-result root)
                  (supertag-git-sync--maybe-push-after-cycle root)))
             (supertag-git-sync--maybe-push-after-cycle root))))))))

(defun supertag-git-sync--maybe-focus-pull ()
  "Run one pull cycle on regaining focus, rate-limited to at most once per
`supertag-git-sync-focus-pull-min-interval' seconds."
  (when (and (bound-and-true-p supertag-git-sync-mode) supertag-git-sync--vault-root)
    (let ((now (float-time)))
      (when (or (null supertag-git-sync--last-focus-pull-time)
                (>= (- now supertag-git-sync--last-focus-pull-time)
                    supertag-git-sync-focus-pull-min-interval))
        (setq supertag-git-sync--last-focus-pull-time now)
        (supertag-git-sync--pull)))))

;;; --- Post-merge handling: DB reload + org text-conflict guard ---

(defun supertag-git-sync--after-merge (merge-result root)
  "Common post-merge handling for both the periodic/focus pull path and
the push-reject retry path. Runs regardless of MERGE-RESULT's exit code
\(unlike an earlier version of this function): a merge that leaves `.org'
text conflicts is EXACTLY the case this function most needs to react to,
and git reports that outcome via a non-zero `git merge' exit -- gating
this function on success would skip conflict detection in precisely the
scenario it exists for.
- Reload the store (`supertag-load-store', which also refreshes the
  multi-instance lock and cross-machine presence) whenever
  `supertag-db-file' itself is NOT among the still-unmerged paths -- the
  common case, since the merge driver (or, absent one, a clean disjoint
  text-merge) resolves it automatically even when other paths in the same
  merge are left conflicted. A harmless no-op reload when the database
  did not actually change.
- Recompute `supertag-git-sync--conflicted-org-files' fresh via
  `supertag-git-sync--live-conflicted-org-files' (so the advice below
  refuses to let the sync scanner import any still-conflicted file)
  instead of ever importing a conflict-marked file, then nudge the
  scanner (`supertag-sync-check-now', if available) to pick up whatever
  else DID merge cleanly."
  (ignore merge-result)
  (let* ((unmerged (supertag-git-sync--unmerged-paths root))
         (db-rel (ignore-errors (file-relative-name (expand-file-name supertag-db-file) root))))
    (when (and (not (and db-rel (member db-rel unmerged)))
               (file-exists-p supertag-db-file)
               (fboundp 'supertag-load-store))
      ;; Never let a reload failure escape this function: it runs inside an
      ;; async process sentinel / this-mode's own callback chain, where an
      ;; uncaught error would both surface nowhere useful AND permanently
      ;; wedge `supertag-git-sync--in-flight' (the caller's `setq nil' right
      ;; after calling this function would simply never run).
      (condition-case err
          (funcall #'supertag-load-store)
        (error (message "supertag-git-sync: store reload after merge failed: %s"
                        (error-message-string err)))))
    (setq supertag-git-sync--conflicted-org-files
          (supertag-git-sync--live-conflicted-org-files root))
    ;; Same reasoning: the sync scanner is a convenience nudge, not
    ;; load-bearing (the existing auto-sync timer will pick up whatever
    ;; this fails to) -- it must never be able to abort this function.
    (when (fboundp 'supertag-sync-check-now)
      (condition-case err
          (funcall #'supertag-sync-check-now)
        (error (message "supertag-git-sync: sync-scanner nudge failed (next cycle will catch up): %s"
                        (error-message-string err)))))))

(defun supertag-git-sync--skip-conflicted-file-advice (orig-fn file &rest args)
  "`:around' advice for `supertag-sync--process-single-file', added only
while `supertag-git-sync-mode' is enabled: never let the sync scanner
import a file the most recent merge left with unresolved conflict
markers (see `supertag-git-sync--conflicted-org-files') -- the same
\"refuse rather than silently ingest garbage\" guard philosophy as the DB
loader's own conflict-marker check, applied to org files."
  (if (member (expand-file-name file) supertag-git-sync--conflicted-org-files)
      (progn
        (message "supertag-git-sync: skipping import of %s -- unresolved merge conflict; resolve manually (see M-x supertag-doctor), then it will be picked up again."
                 file)
        nil)
    (apply orig-fn file args)))

;;; --- Commit triggers: DB save hook + vault-scoped after-save-hook ---

(defun supertag-git-sync--on-db-saved ()
  "Hook function for `supertag-persistence-after-save-hook'."
  (when (bound-and-true-p supertag-git-sync-mode)
    (supertag-git-sync--schedule-commit)))

(defun supertag-git-sync--on-file-saved ()
  "`after-save-hook' function, vault-scoped: only schedules a commit when
the just-saved buffer's file is inside this session's git-sync vault
root -- saves anywhere else in Emacs must never trigger a vault commit."
  (when (and (bound-and-true-p supertag-git-sync-mode)
             supertag-git-sync--vault-root
             buffer-file-name
             (supertag-git--ancestor-p supertag-git-sync--vault-root buffer-file-name))
    (supertag-git-sync--schedule-commit)))

;;; --- Lifecycle ---

(defun supertag-git-sync--lighter ()
  "Modeline lighter: \" STG\" normally, \" STG\\u2191N\" while N commits
are pending push."
  (if (> supertag-git-sync--pending-push-count 0)
      (format " STG↑%d" supertag-git-sync--pending-push-count)
    " STG"))

(defun supertag-git-sync--enable ()
  "Validate `supertag-git-check' and, if it passes, wire up all hooks and
timers. Refuses (turning the mode back off and messaging what to run
first) when `supertag-db-file' is not yet inside a git repository at
all -- everything else (driver not configured, no remote yet) is reported
but does not block enabling, since git's own default merge still degrades
acceptably (see supertag-git.el's top-level Commentary).

Also computes this vault's true ahead-of-upstream count immediately
\(async unless `supertag-git-sync--synchronous') and pushes right away if
it turns out we are ahead (P1-7): a previous Emacs session's commit whose
own push never completed (network hiccup, or Emacs simply quit before
the debounce/push chain finished) must not sit orphaned, local-only,
until some unrelated new edit happens to trigger the next commit -- see
`supertag-git-sync--maybe-push-after-cycle'."
  (let ((status (supertag-git-check)))
    (if (not (plist-get status :in-repo-p))
        (progn
          (message "supertag-git-sync-mode: `supertag-db-file' is not inside a git repository yet -- run `M-x supertag-git-setup' first.")
          (setq supertag-git-sync-mode nil))
      (unless (plist-get status :driver-configured-p)
        (message "supertag-git-sync-mode: warning -- the semantic merge driver is not configured for THIS clone; run `M-x supertag-git-setup' here too (see M-x supertag-doctor \"Git Sync\")."))
      (setq supertag-git-sync--vault-root (plist-get status :repo-root))
      (setq supertag-git-sync--offline-warned nil)
      (setq supertag-git-sync--conflict-commit-warned nil)
      (setq supertag-git-sync--conflicted-org-files nil)
      (add-hook 'supertag-persistence-after-save-hook #'supertag-git-sync--on-db-saved)
      (add-hook 'after-save-hook #'supertag-git-sync--on-file-saved)
      (when (fboundp 'supertag-sync--process-single-file)
        (advice-add 'supertag-sync--process-single-file :around
                    #'supertag-git-sync--skip-conflicted-file-advice))
      (setq supertag-git-sync--pull-timer
            (run-with-timer supertag-git-sync-pull-interval
                             supertag-git-sync-pull-interval
                             #'supertag-git-sync--pull))
      (add-function :after after-focus-change-function #'supertag-git-sync--maybe-focus-pull)
      (unless supertag-git-sync--in-flight
        (setq supertag-git-sync--in-flight t)
        (supertag-git-sync--maybe-push-after-cycle supertag-git-sync--vault-root))
      (message "supertag-git-sync-mode: enabled for %s." supertag-git-sync--vault-root))))

(defun supertag-git-sync--disable ()
  "Cancel all timers (flushing any pending debounced commit first, rather
than silently dropping it), remove all hooks/advice, and clear vault
state."
  (when supertag-git-sync--commit-timer
    (cancel-timer supertag-git-sync--commit-timer)
    (setq supertag-git-sync--commit-timer nil)
    (supertag-git-sync--fire-commit))
  (when supertag-git-sync--pull-timer
    (cancel-timer supertag-git-sync--pull-timer)
    (setq supertag-git-sync--pull-timer nil))
  (remove-hook 'supertag-persistence-after-save-hook #'supertag-git-sync--on-db-saved)
  (remove-hook 'after-save-hook #'supertag-git-sync--on-file-saved)
  (remove-function after-focus-change-function #'supertag-git-sync--maybe-focus-pull)
  (when (fboundp 'supertag-sync--process-single-file)
    (advice-remove 'supertag-sync--process-single-file
                   #'supertag-git-sync--skip-conflicted-file-advice))
  (setq supertag-git-sync--vault-root nil)
  (message "supertag-git-sync-mode: disabled."))

;;;###autoload
(define-minor-mode supertag-git-sync-mode
  "Automatic git commit/fetch/merge/push loop for the current Org-Supertag
vault (S4 of .phrase/phases/phase-git-sync-20260713/PLAN.md). Off by
default (opt-in) -- see this file's Commentary above this mode's
definition for the full design (serialization, async, offline
degradation).

While enabled:
- Saving the database (`supertag-save-store' succeeding) or any file
  inside the vault schedules a debounced (`supertag-git-sync-commit-debounce'
  seconds) scoped `git add -A --' + commit (skipped when nothing is
  actually staged), followed by a push. Staging is deliberately narrow --
  ONLY `.org' files, `supertag-db-file' itself, and `.gitattributes'/
  `.gitignore' (see `supertag-git-sync--commit-pathspecs') -- so any OTHER
  untracked file inside the vault (credentials, attachments, IDE/OS junk,
  ...) is never swept into an automatic commit. The commit is refused
  entirely, with a one-shot warning, whenever the repository still has
  unresolved conflicted paths from a merge (in this session, an earlier
  one, or outside Emacs) -- staging over that would mark the conflict
  resolved and commit literal `<<<<<<<' markers into history; see
  `supertag-git-sync--fire-commit'.
- Every `supertag-git-sync-pull-interval' seconds, and at most once every
  `supertag-git-sync-focus-pull-min-interval' seconds on regaining focus,
  this fetches, merges (never rebases) if behind, and then PUSHES if
  ahead (P1-7) -- both the ahead and behind counts are re-derived fresh
  from git (`git rev-list --count`) every cycle, never trusted from a
  session-local counter alone. This is what makes a period offline (or a
  push that never finished before Emacs quit) recover on its own: the
  very next successful fetch after connectivity returns also pushes
  whatever commits piled up locally, without waiting for a brand new
  edit.
- Enabling the mode itself immediately performs this same ahead check and
  pushes right away if needed, so a fresh Emacs session never orphans
  commits a previous session made but could not push in time.
- The modeline lighter's pending count (\" STG\\u2191N\") is a DISPLAY
  CACHE refreshed from that same git-derived ahead count every cycle --
  after a restart it shows the true number as soon as the first cycle
  runs, rather than lying with 0 the way a purely session-local counter
  would.
- A rejected push retries once (fetch+merge+push); repeated failure or any
  network-looking failure degrades silently to local-only commits, with
  the pending count shown in the lighter.
- Org files left with unresolved merge conflict markers are never
  imported by the sync scanner; see M-x supertag-doctor, which derives
  this status FRESH from git every time it is run (survives an Emacs
  restart), not merely from this session's cache.

Disabling cancels all timers (flushing any pending debounced commit
first), removes all hooks/advice, and clears vault state."
  :global t
  :lighter (:eval (supertag-git-sync--lighter))
  :group 'supertag-git-sync
  (if supertag-git-sync-mode
      (supertag-git-sync--enable)
    (supertag-git-sync--disable)))

(provide 'supertag-git)

;;; supertag-git.el ends here
