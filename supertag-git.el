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

Does NOT relocate the database into the repository (that is a separate,
not-yet-implemented S4 step) and refuses outright if the database turns
out to live outside the git repo it finds/creates."
  (interactive)
  (unless (supertag-git--executable)
    (user-error "supertag-git-setup: `git' executable not found; install git first"))
  (let* ((db-file (expand-file-name supertag-db-file))
         (db-dir (file-name-directory db-file))
         (report nil))
    (cl-flet ((note (fmt &rest args) (push (apply #'format fmt args) report)))
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
                  ".gitignore: entries already present (skipped)")))))
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

(provide 'supertag-git)

;;; supertag-git.el ends here
