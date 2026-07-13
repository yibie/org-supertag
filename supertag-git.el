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
;; ## Scope (deliberately narrow)
;;
;; This is SETUP-time plumbing only. It does NOT implement the S4 "one URL"
;; user journey: no repo-layout migration (moving `supertag-db-file' itself
;; into a vault-rooted `.supertag/' directory), no `remote add`/push, and no
;; `supertag-git-sync-mode' auto commit/fetch/push loop. Those are later
;; work; see the plan's "S4 用户旅程" section.
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
THIS call -- empty on a re-run, which is how idempotence is verified."
  (let* ((root (file-name-as-directory (expand-file-name repo-root)))
         (rel (file-relative-name (expand-file-name db-file) root))
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
;;   1. copy the database to `<repo-root>/.supertag/supertag-db.el'
;;   2. verify the COPY is loadable (never trust a bare `copy-file')
;;   3. only once verified: flip this session's persistence wiring
;;      (`supertag-data-directory'/`supertag-db-file'/etc, the same
;;      config-guard-bypassed pattern `supertag-vault--apply' uses) and
;;      reload the live store from the new location
;;   4. only once the reload above has itself succeeded: rename (never
;;      delete) the OLD database file to a `.migrated-<timestamp>' tombstone
;;
;; Any failure before step 4 leaves the OLD file completely untouched (no
;; wiring changed, nothing renamed) -- see `supertag-git--do-migrate-layout'.

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

(defun supertag-git--do-migrate-layout (old-db-file root)
  "Perform the copy/verify/wire/reload/tombstone sequence, migrating
OLD-DB-FILE into `<ROOT>/.supertag/supertag-db.el'.

Only ever called by `supertag-git--migrate-layout' once ROOT has already
been confirmed to be the single configured sync root and NOT already an
ancestor directory of OLD-DB-FILE. Returns a plist; see that function's
docstring for the full set of possible `:status' values. Never signals --
every failure mode (including one deliberately injected by a test, e.g. an
unwritable target directory) is caught and reported as `:status :failed'
plus a human-readable `:error' string, with the OLD file provably
untouched (nothing renames it until after a successful reload)."
  (condition-case err
      (let* ((target-dir (expand-file-name ".supertag/" root))
             (target-db (expand-file-name "supertag-db.el" target-dir))
             (old-exists (file-exists-p old-db-file)))
        (when (file-exists-p target-db)
          (error "migration target %s already exists -- resolve manually before retrying (are two migrations racing?)"
                 target-db))
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
              ;; database there directly.
              (progn
                (supertag-git--apply-data-directory-wiring target-dir)
                (list :status :migrated :old-db-file old-db-file :new-db-file target-db
                      :repo-root toplevel :tombstone nil :fresh-vault t))
            (copy-file old-db-file target-db nil t)
            ;; Verify the COPY (never the live in-memory store, which may
            ;; have unsaved changes not yet reflected in OLD-DB-FILE either)
            ;; is actually loadable BEFORE touching any wiring or the old
            ;; file -- a `copy-file' that "succeeded" onto a filesystem that
            ;; silently truncates, or a target that was already subtly
            ;; corrupt, must not be trusted just because the syscall
            ;; returned.
            (let ((verify-error nil))
              (condition-case verr
                  (unless (hash-table-p (supertag--persistence--try-read-store target-db))
                    (setq verify-error "migrated copy did not parse into a store hash table"))
                (error (setq verify-error (error-message-string verr))))
              (when verify-error
                (ignore-errors (delete-file target-db))
                (error "migrated copy at %s failed verification: %s" target-db verify-error)))
            ;; Verified loadable: only now flip the wiring and reload the
            ;; live store from the new location (this also re-acquires the
            ;; multi-instance lock and refreshes cross-machine presence for
            ;; the new path, via `supertag-load-store' itself).
            (supertag-git--apply-data-directory-wiring target-dir)
            (when (fboundp 'supertag-load-store)
              (funcall #'supertag-load-store))
            ;; Only after a successful reload do we touch the OLD file at
            ;; all -- renamed to a tombstone, NEVER deleted.
            (let* ((timestamp (format-time-string "%Y%m%d%H%M%S"))
                   (tombstone (concat old-db-file ".migrated-" timestamp)))
              (condition-case terr
                  (rename-file old-db-file tombstone)
                (error
                 (message "supertag-git: migration to %s succeeded, but renaming the old database (%s) to a tombstone failed: %s -- the old file is harmlessly left in place (it is no longer read by anything)."
                          target-db old-db-file (error-message-string terr))
                 (setq tombstone nil)))
              (list :status :migrated :old-db-file old-db-file :new-db-file target-db
                    :repo-root toplevel :tombstone tombstone)))))
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
      (let* ((rel (file-relative-name db-file toplevel))
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
                    ".gitignore: entries already present (skipped)"))))))
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
  "Number of local commits not yet successfully pushed. Shown in the
modeline lighter (e.g. \" STG\\u21913\").")

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

(defun supertag-git-sync--fire-commit ()
  "The debounce timer function: `git add -A' then commit ONLY if
something is actually staged (`git diff --cached --quiet' guard -- never
an empty commit), then push. Batch tests call this directly (never wait
for the real timer), per this task's instructions. A no-op when no vault
is active or another git op is already in flight (the next timer tick
will catch up, per this section's Commentary on serialization)."
  (setq supertag-git-sync--commit-timer nil)
  (when (and supertag-git-sync--vault-root (not supertag-git-sync--in-flight))
    (setq supertag-git-sync--in-flight t)
    (let ((root supertag-git-sync--vault-root))
      (supertag-git-sync--run-git
       root (list "add" "-A")
       (lambda (add-result)
         (if (not (supertag-git--ok-p add-result))
             (progn (setq supertag-git-sync--in-flight nil)
                    (supertag-git-sync--report-failure "git add -A" add-result))
           (supertag-git-sync--run-git
            root (list "diff" "--cached" "--quiet")
            (lambda (diff-result)
              (if (supertag-git--ok-p diff-result)
                  ;; Exit 0 = nothing staged -- nothing to commit.
                  (setq supertag-git-sync--in-flight nil)
                (let ((commit-message
                       (format "supertag-sync: %s %s" (system-name)
                               (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
                  (supertag-git-sync--run-git
                   root (list "commit" "-q" "-m" commit-message)
                   (lambda (commit-result)
                     (if (not (supertag-git--ok-p commit-result))
                         (progn (setq supertag-git-sync--in-flight nil)
                                (supertag-git-sync--report-failure "git commit" commit-result))
                       (cl-incf supertag-git-sync--pending-push-count)
                       (supertag-git-sync--push root))))))))))))))

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
stays incremented, shown in the modeline, and the next commit or pull
cycle will try again. On anything else (network/auth failure): treat as
offline degradation. Always clears `supertag-git-sync--in-flight' exactly
once, on every branch."
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

;;; --- Pull (timer + rate-limited focus-in) ---

(defun supertag-git-sync--behind-p (root)
  "Return non-nil if ROOT's HEAD is behind its upstream, immediately after
a fetch."
  (let ((result (supertag-git--run root "rev-list" "--count" "HEAD..@{upstream}")))
    (and (supertag-git--ok-p result)
         (> (string-to-number (cdr result)) 0))))

(defun supertag-git-sync--pull ()
  "One fetch (+ merge if behind) cycle. A no-op when no vault is active or
another git op is already in flight."
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
                  (setq supertag-git-sync--in-flight nil)))
             (setq supertag-git-sync--in-flight nil))))))))

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
advances HEAD at all, so that diff would see nothing."
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
- For any still-unmerged `.org' path that contains literal conflict
  markers: record it in `supertag-git-sync--conflicted-org-files' (so the
  advice below refuses to let the sync scanner import it) instead of ever
  importing a conflict-marked file, then nudge the scanner
  (`supertag-sync-check-now', if available) to pick up whatever else DID
  merge cleanly."
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
    (let (conflicted)
      (dolist (rel unmerged)
        (when (string-match-p "\\.org\\'" rel)
          (let ((abs (expand-file-name rel root)))
            (when (and (file-exists-p abs)
                       (supertag-git-sync--file-has-conflict-markers-p abs))
              (push abs conflicted)))))
      (setq supertag-git-sync--conflicted-org-files conflicted)
      ;; Same reasoning: the sync scanner is a convenience nudge, not
      ;; load-bearing (the existing auto-sync timer will pick up whatever
      ;; this fails to) -- it must never be able to abort this function.
      (when (fboundp 'supertag-sync-check-now)
        (condition-case err
            (funcall #'supertag-sync-check-now)
          (error (message "supertag-git-sync: sync-scanner nudge failed (next cycle will catch up): %s"
                          (error-message-string err))))))))

(defun supertag-git-sync--skip-conflicted-file-advice (orig-fn file &rest args)
  "`:around' advice for `supertag-sync--process-single-file', added only
while `supertag-git-sync-mode' is enabled: never let the sync scanner
import a file the most recent merge left with unresolved conflict
markers (see `supertag-git-sync--conflicted-org-files') -- the same
\"refuse rather than silently ingest garbage\" guard philosophy as the DB
loader's own conflict-marker check, applied to org files."
  (if (member (expand-file-name file) supertag-git-sync--conflicted-org-files)
      (progn
        (message "supertag-git-sync: skipping import of %s -- unresolved merge conflict markers; resolve manually (see M-x supertag-doctor), then it will be picked up again."
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
acceptably (see supertag-git.el's top-level Commentary)."
  (let ((status (supertag-git-check)))
    (if (not (plist-get status :in-repo-p))
        (progn
          (message "supertag-git-sync-mode: `supertag-db-file' is not inside a git repository yet -- run `M-x supertag-git-setup' first.")
          (setq supertag-git-sync-mode nil))
      (unless (plist-get status :driver-configured-p)
        (message "supertag-git-sync-mode: warning -- the semantic merge driver is not configured for THIS clone; run `M-x supertag-git-setup' here too (see M-x supertag-doctor \"Git Sync\")."))
      (setq supertag-git-sync--vault-root (plist-get status :repo-root))
      (setq supertag-git-sync--pending-push-count 0)
      (setq supertag-git-sync--offline-warned nil)
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
  seconds) `git add -A' + commit (skipped when nothing is actually
  staged), followed by a push.
- Every `supertag-git-sync-pull-interval' seconds, and at most once every
  `supertag-git-sync-focus-pull-min-interval' seconds on regaining focus,
  this fetches and merges (never rebases) if behind.
- A rejected push retries once (fetch+merge+push); repeated failure or any
  network-looking failure degrades silently to local-only commits, with
  the pending count shown in the lighter.
- Org files left with unresolved merge conflict markers are never
  imported by the sync scanner; see M-x supertag-doctor.

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
