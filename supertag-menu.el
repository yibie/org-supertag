;;; supertag-menu.el --- Discoverable transient menu for Org-Supertag -*- lexical-binding: t; -*-

;; Keywords: convenience

;;; Commentary:

;; `supertag-menu' is a single entry point that surfaces the most useful
;; Org-Supertag commands in one discoverable `transient' pop-up, so users
;; do not have to hunt through `M-x' to remember command names.
;;
;; `transient' has shipped with Emacs since 28.1, so this file adds no new
;; dependency.
;;
;; Design notes:
;; - Commands are grouped into columns: Views, Tags & Fields, Search &
;;   Query, Capture, Sync & Maintenance, Git Sync, Automation, and an
;;   optional Setup group. Less-common commands (virtual columns,
;;   analytic demo views, database migration) live behind the Setup
;;   group's "More commands..." nested prefix (`supertag-menu-more')
;;   rather than crowding the top-level popup.
;; - Keys are two-character mnemonics of the form "<group><letter>"
;;   (e.g. "vt" for the table View, "ta" for Tag Add) so that every key
;;   across the whole menu is unique; the lowercase "q" is left untouched
;;   so `transient's default quit binding keeps working.
;; - Every command referenced here already exists elsewhere in
;;   Org-Supertag; none are (re)defined in this file. Commands whose
;;   owning feature carries an `;;;###autoload' cookie (or is guaranteed
;;   to already be loaded as part of Org-Supertag's own core `require'
;;   chain) are wired directly by symbol. Commands whose owning feature
;;   is NOT unconditionally loaded (or has no autoload cookie) are wired
;;   through a thin `supertag-menu--*' wrapper that `require's the owning
;;   feature before calling the real command interactively, so the menu
;;   works regardless of what has been loaded so far.
;; - The optional "Setup" group wires `supertag-setup' and
;;   `supertag-automation-insert-template', both developed alongside this
;;   file. Their suffixes are guarded with `:if (lambda () (fboundp ...))'
;;   so the menu degrades gracefully whether or not those features are
;;   present in a given checkout.

;;; Code:

(require 'transient)

;;; --- Forward declarations ---
;; These commands live in other Org-Supertag modules. None of those
;; modules are `require'd unconditionally here (to keep this file cheap
;; to load); each is either pulled in lazily by a wrapper below, or is
;; already guaranteed to be loaded by the time a real Org-Supertag session
;; calls `supertag-menu' (see the module-by-module notes below).

;; supertag-view-table.el (no autoload cookie; wrapped)
(declare-function supertag-view-table "supertag-view-table" (data-source &optional columns view-config named-views))
;; supertag-ui-commands.el (no autoload cookie; wrapped)
(declare-function supertag-view-kanban "supertag-ui-commands" ())
;; supertag-view-node.el (no autoload cookie; wrapped)
(declare-function supertag-view-node "supertag-view-node" ())
;; supertag-view-schema.el (;;;###autoload; also unconditionally required
;; by org-supertag.el, so it is safe to reference directly)
(declare-function supertag-view-schema "supertag-view-schema" ())
;; supertag-board.el (;;;###autoload; optional feature, requires the
;; external `websocket' package; guarded with :if fboundp below)
(declare-function supertag-board-mode "supertag-board" (&optional arg))
;; supertag-graph-ui.el (;;;###autoload; optional feature, requires the
;; external `websocket' package; guarded with :if fboundp below)
(declare-function supertag-graph-ui-open "supertag-graph-ui" ())

;; supertag-ui-commands.el (no autoload cookies; all wrapped)
(declare-function supertag-add-tag "supertag-ui-commands" (&optional beg end))
(declare-function supertag-remove-tag-from-node "supertag-ui-commands" ())
(declare-function supertag-change-tag-at-point "supertag-ui-commands" ())
(declare-function supertag-rename-tag "supertag-ui-commands" (&optional tag-id))
(declare-function supertag-delete-tag-everywhere "supertag-ui-commands" (&optional tag-id))
(declare-function supertag-ui-quick-edit-field "supertag-ui-commands" ())
(declare-function supertag-find-node "supertag-ui-commands" ())
(declare-function supertag-add-reference "supertag-ui-commands" ())
(declare-function supertag-insert-embed "supertag-ui-commands" ())
(declare-function supertag-convert-link-to-embed "supertag-ui-commands" ())
(declare-function supertag-capture "supertag-ui-commands" (&optional target-file headline))

;; supertag-concept.el (;;;###autoload; also unconditionally required by
;; org-supertag.el, so it is safe to reference directly)
(declare-function supertag-promote-concept "supertag-concept" (beg end))

;; supertag-ui-search.el (no autoload cookie; wrapped)
(declare-function supertag-search "supertag-ui-search" ())
;; supertag-ui-query-block.el (no autoload cookie; wrapped)
(declare-function supertag-insert-query-block "supertag-ui-query-block" ())
(declare-function supertag-insert-query-dblock "supertag-ui-query-block" ())
(declare-function supertag-query-build "supertag-query-library" ())
(declare-function supertag-query-run-saved "supertag-query-library" ())
(declare-function supertag-query-describe-syntax "supertag-query-library" ())

;; supertag-services-capture.el (;;;###autoload; also unconditionally
;; required by org-supertag.el, so it is safe to reference directly)
(declare-function supertag-capture-with-template "supertag-services-capture" (&optional template-key))

;; supertag-ui-commands.el / supertag-services-sync.el (;;;###autoload;
;; unconditionally required by org-supertag.el, so it is safe to
;; reference these directly)
(declare-function supertag-sync-check-now "supertag-ui-commands" ())
(declare-function supertag-sync-cleanup-database "supertag-ui-commands" ())
(declare-function supertag-sync-status "supertag-ui-commands" ())
(declare-function supertag-sync-full-rescan "supertag-services-sync" ())

;; supertag-doctor.el (;;;###autoload, but NOT part of org-supertag.el's
;; own `require' chain; wrapped for robustness)
(declare-function supertag-doctor "supertag-doctor" (&optional report-only))
;; supertag-core-persistence.el (no autoload cookie; wrapped)
(declare-function supertag-db-retry-lock "supertag-core-persistence" ())
;; supertag-core-persistence.el (owned by a teammate; `supertag-restore'
;; is developed alongside this iteration too, so it is wrapped exactly
;; like `supertag-db-retry-lock' above rather than assumed present)
(declare-function supertag-restore "supertag-core-persistence" ())

;; supertag-git.el (;;;###autoload, but NOT part of org-supertag.el's own
;; `require' chain; wrapped for robustness, same as `supertag-doctor')
(declare-function supertag-git-setup "supertag-git" ())
(declare-function supertag-git-clone "supertag-git" (remote-url local-directory))
(declare-function supertag-git-sync-mode "supertag-git" (&optional arg))

;; supertag-conflicts.el (;;;###autoload; also unconditionally required by
;; org-supertag.el, so it is safe to reference directly)
(declare-function supertag-conflicts-resolve "supertag-conflicts" ())
(declare-function supertag-conflicts-use-ours-all "supertag-conflicts" ())
(declare-function supertag-conflicts-use-theirs-all "supertag-conflicts" ())

;; supertag-automation-sync.el / supertag-automation.el (no autoload
;; cookie; unconditionally required by org-supertag.el, but wrapped
;; anyway since Org-Supertag's own menu entries for this file wrap every
;; non-autoloaded command)
(declare-function supertag-automation-sync-enable "supertag-automation-sync" ())
(declare-function supertag-automation-sync-disable "supertag-automation-sync" ())
(declare-function supertag-automation-recalculate-all-rollups "supertag-automation" ())
;; supertag-services-scheduler.el (no autoload cookie; wrapped)
(declare-function supertag-scheduler-start "supertag-services-scheduler" ())
(declare-function supertag-scheduler-stop "supertag-services-scheduler" ())
(declare-function supertag-scheduler-list-tasks "supertag-services-scheduler" ())

;; supertag-virtual-column.el (no autoload cookie; wrapped)
(declare-function supertag-virtual-column-create-interactive "supertag-virtual-column" ())
(declare-function supertag-virtual-column-edit-interactive "supertag-virtual-column" ())
(declare-function supertag-virtual-column-delete-interactive "supertag-virtual-column" ())
(declare-function supertag-virtual-column-list-interactive "supertag-virtual-column" ())

;; supertag-view-priority-matrix.el / supertag-view-progress-dashboard.el /
;; supertag-view-effort-distribution.el (no autoload cookie; wrapped).
;; Only `-demo' entry points exist for these views today, so the menu
;; wires and labels them honestly as demos.
(declare-function supertag-view-priority-matrix-demo "supertag-view-priority-matrix" ())
(declare-function supertag-view-progress-dashboard-demo "supertag-view-progress-dashboard" ())
(declare-function supertag-view-effort-distribution-demo "supertag-view-effort-distribution" ())

;; supertag-migration.el (;;;###autoload; also unconditionally required by
;; org-supertag.el, so it is safe to reference directly)
(declare-function supertag-migrate-database-to-new-arch "supertag-migration" ())
(declare-function supertag-batch-convert-properties-to-fields "supertag-migration" ())
(declare-function supertag-migration-add-ids-to-org-headings "supertag-migration" (directory))
;; supertag-migrate-tag-ids.el (no autoload cookie; NOT part of
;; org-supertag.el's own `require' chain; wrapped)
(declare-function supertag-migrate-tag-ids "supertag-migrate-tag-ids" ())

;; supertag-view-svg-tag.el / supertag-concept.el (;;;###autoload; also
;; unconditionally required by org-supertag.el, so it is safe to
;; reference these directly)
(declare-function supertag-svg-tag-mode-toggle "supertag-view-svg-tag" ())
(declare-function supertag-concept-link-mode "supertag-concept" (&optional arg))

;; supertag-setup.el and supertag-automation-templates.el are developed
;; alongside this file; guard every reference with `fboundp' and never
;; `require' them directly from here.
(declare-function supertag-setup "supertag-setup" ())
(declare-function supertag-automation-insert-template "supertag-automation-templates" ())
(declare-function supertag-automation-list-templates "supertag-automation-templates" ())

;;; --- Thin lazy-loading wrappers ---
;; Each wrapper `require's the owning feature (safe: these files have no
;; load-time side effects of their own -- unlike `org-supertag.el', which
;; runs `supertag-init' at load time) and then calls the real, already
;;-interactive command. This keeps `supertag-menu' usable even when only
;; part of Org-Supertag has been loaded so far.

(defmacro supertag-menu--defwrapper (name feature command doc)
  "Define NAME as a command that `require's FEATURE, then calls COMMAND.
DOC is used as the docstring of the generated wrapper."
  (declare (indent defun))
  `(defun ,name ()
     ,doc
     (interactive)
     (require ',feature)
     (call-interactively #',command)))

(supertag-menu--defwrapper supertag-menu--view-table
  supertag-view-table supertag-view-table
  "Open `supertag-view-table', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--view-kanban
  supertag-ui-commands supertag-view-kanban
  "Open `supertag-view-kanban', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--view-node
  supertag-view-node supertag-view-node
  "Open `supertag-view-node', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--add-tag
  supertag-ui-commands supertag-add-tag
  "Run `supertag-add-tag', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--remove-tag
  supertag-ui-commands supertag-remove-tag-from-node
  "Run `supertag-remove-tag-from-node', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--change-tag
  supertag-ui-commands supertag-change-tag-at-point
  "Run `supertag-change-tag-at-point', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--rename-tag
  supertag-ui-commands supertag-rename-tag
  "Run `supertag-rename-tag', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--delete-tag
  supertag-ui-commands supertag-delete-tag-everywhere
  "Run `supertag-delete-tag-everywhere', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--quick-edit-field
  supertag-ui-commands supertag-ui-quick-edit-field
  "Run `supertag-ui-quick-edit-field', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--search
  supertag-ui-search supertag-search
  "Run `supertag-search', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--insert-query-block
  supertag-ui-query-block supertag-insert-query-block
  "Run `supertag-insert-query-block', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--insert-query-dblock
  supertag-ui-query-block supertag-insert-query-dblock
  "Run `supertag-insert-query-dblock', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--query-build
  supertag-query-library supertag-query-build
  "Run `supertag-query-build', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--query-run-saved
  supertag-query-library supertag-query-run-saved
  "Run `supertag-query-run-saved', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--query-describe-syntax
  supertag-query-library supertag-query-describe-syntax
  "Run `supertag-query-describe-syntax', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--insert-embed
  supertag-ui-commands supertag-insert-embed
  "Run `supertag-insert-embed', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--convert-link-to-embed
  supertag-ui-commands supertag-convert-link-to-embed
  "Run `supertag-convert-link-to-embed', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--add-reference
  supertag-ui-commands supertag-add-reference
  "Run `supertag-add-reference', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--find-node
  supertag-ui-commands supertag-find-node
  "Run `supertag-find-node', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--capture
  supertag-ui-commands supertag-capture
  "Run `supertag-capture', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--doctor
  supertag-doctor supertag-doctor
  "Run `supertag-doctor', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--db-retry-lock
  supertag-core-persistence supertag-db-retry-lock
  "Run `supertag-db-retry-lock', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--restore
  supertag-core-persistence supertag-restore
  "Run `supertag-restore', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--git-setup
  supertag-git supertag-git-setup
  "Run `supertag-git-setup', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--git-clone
  supertag-git supertag-git-clone
  "Run `supertag-git-clone', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--git-sync-mode
  supertag-git supertag-git-sync-mode
  "Toggle `supertag-git-sync-mode', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--automation-sync-enable
  supertag-automation-sync supertag-automation-sync-enable
  "Run `supertag-automation-sync-enable', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--automation-sync-disable
  supertag-automation-sync supertag-automation-sync-disable
  "Run `supertag-automation-sync-disable', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--automation-recalculate-all-rollups
  supertag-automation supertag-automation-recalculate-all-rollups
  "Run `supertag-automation-recalculate-all-rollups', loading its feature
first if needed.")

(supertag-menu--defwrapper supertag-menu--scheduler-start
  supertag-services-scheduler supertag-scheduler-start
  "Run `supertag-scheduler-start', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--scheduler-stop
  supertag-services-scheduler supertag-scheduler-stop
  "Run `supertag-scheduler-stop', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--scheduler-list-tasks
  supertag-services-scheduler supertag-scheduler-list-tasks
  "Run `supertag-scheduler-list-tasks', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--virtual-column-create
  supertag-virtual-column supertag-virtual-column-create-interactive
  "Run `supertag-virtual-column-create-interactive', loading its feature
first if needed.")

(supertag-menu--defwrapper supertag-menu--virtual-column-edit
  supertag-virtual-column supertag-virtual-column-edit-interactive
  "Run `supertag-virtual-column-edit-interactive', loading its feature
first if needed.")

(supertag-menu--defwrapper supertag-menu--virtual-column-delete
  supertag-virtual-column supertag-virtual-column-delete-interactive
  "Run `supertag-virtual-column-delete-interactive', loading its feature
first if needed.")

(supertag-menu--defwrapper supertag-menu--virtual-column-list
  supertag-virtual-column supertag-virtual-column-list-interactive
  "Run `supertag-virtual-column-list-interactive', loading its feature
first if needed.")

(supertag-menu--defwrapper supertag-menu--view-priority-matrix-demo
  supertag-view-priority-matrix supertag-view-priority-matrix-demo
  "Run `supertag-view-priority-matrix-demo', loading its feature first if needed.")

(supertag-menu--defwrapper supertag-menu--view-progress-dashboard-demo
  supertag-view-progress-dashboard supertag-view-progress-dashboard-demo
  "Run `supertag-view-progress-dashboard-demo', loading its feature
first if needed.")

(supertag-menu--defwrapper supertag-menu--view-effort-distribution-demo
  supertag-view-effort-distribution supertag-view-effort-distribution-demo
  "Run `supertag-view-effort-distribution-demo', loading its feature
first if needed.")

(supertag-menu--defwrapper supertag-menu--migrate-tag-ids
  supertag-migrate-tag-ids supertag-migrate-tag-ids
  "Run `supertag-migrate-tag-ids', loading its feature first if needed.")

;;; --- The menu ---

;;;###autoload
(transient-define-prefix supertag-menu ()
  "Top-level discoverable menu for Org-Supertag commands."
  [["Views"
    ("vt" "Table"          supertag-menu--view-table)
    ("vk" "Kanban board"   supertag-menu--view-kanban)
    ("vn" "Node view"      supertag-menu--view-node)
    ("vs" "Schema"         supertag-view-schema)
    ("vw" "Whiteboard"     supertag-board-mode
     :if (lambda () (fboundp 'supertag-board-mode)))
    ("vg" "Graph UI"       supertag-graph-ui-open
     :if (lambda () (fboundp 'supertag-graph-ui-open)))
    ("vy" "Toggle SVG tags"      supertag-svg-tag-mode-toggle)
    ("vl" "Toggle concept links" supertag-concept-link-mode)]
   ["Tags & Fields"
    ("ta" "Add tag"            supertag-menu--add-tag)
    ("tr" "Remove tag"         supertag-menu--remove-tag)
    ("tc" "Change tag"         supertag-menu--change-tag)
    ("tR" "Rename tag (all)"   supertag-menu--rename-tag)
    ("tD" "Delete tag (all)"   supertag-menu--delete-tag)
    ("tf" "Quick edit field"   supertag-menu--quick-edit-field)
    ("tp" "Promote concept"    supertag-promote-concept)]]
  [["Search & Query"
    ("ss" "Search"                supertag-menu--search)
    ("sq" "Insert query block"    supertag-menu--insert-query-block)
    ("sd" "Insert dynamic query"  supertag-menu--insert-query-dblock)
    ("sb" "Build query (wizard)"  supertag-menu--query-build)
    ("sr" "Run saved query"       supertag-menu--query-run-saved)
    ("sy" "Query syntax help"     supertag-menu--query-describe-syntax)
    ("se" "Insert embed"          supertag-menu--insert-embed)
    ("sc" "Convert link to embed" supertag-menu--convert-link-to-embed)
    ("sl" "Add reference"         supertag-menu--add-reference)
    ("sf" "Find node"             supertag-menu--find-node)]
   ["Capture"
    ("cc" "Capture"               supertag-menu--capture)
    ("ct" "Capture with template" supertag-capture-with-template)]]
  [["Sync & Maintenance"
    ("mc" "Check & sync now"  supertag-sync-check-now)
    ("mr" "Full rescan"       supertag-sync-full-rescan)
    ("mx" "Cleanup database"  supertag-sync-cleanup-database)
    ("ms" "Sync status"       supertag-sync-status)
    ("md" "Doctor"            supertag-menu--doctor)
    ("ml" "Retry DB lock"     supertag-menu--db-retry-lock)]
   ["Git Sync"
    ("gs" "Setup git sync"       supertag-menu--git-setup)
    ("gc" "Clone vault"          supertag-menu--git-clone)
    ("gm" "Toggle sync mode"     supertag-menu--git-sync-mode)
    ("gr" "Resolve conflicts"    supertag-conflicts-resolve)
    ("go" "Use ours (all)"       supertag-conflicts-use-ours-all)
    ("gt" "Use theirs (all)"     supertag-conflicts-use-theirs-all)]]
  [["Automation"
    ("al" "List templates"       supertag-automation-list-templates
     :if (lambda () (fboundp 'supertag-automation-list-templates)))
    ("ae" "Enable auto-sync"     supertag-menu--automation-sync-enable)
    ("ad" "Disable auto-sync"    supertag-menu--automation-sync-disable)
    ("ar" "Recalculate rollups"  supertag-menu--automation-recalculate-all-rollups)
    ("as" "Start scheduler"      supertag-menu--scheduler-start)
    ("ax" "Stop scheduler"       supertag-menu--scheduler-stop)
    ("at" "List scheduled tasks" supertag-menu--scheduler-list-tasks)]
   ["Setup"
    ("zs" "Setup wizard"             supertag-setup
     :if (lambda () (fboundp 'supertag-setup)))
    ("zt" "Insert automation template" supertag-automation-insert-template
     :if (lambda () (fboundp 'supertag-automation-insert-template)))
    ("zr" "Restore from backup"      supertag-menu--restore)
    ("zm" "More commands..."         supertag-menu-more)]])

;;;###autoload
(transient-define-prefix supertag-menu-more ()
  "Secondary menu for less-common Org-Supertag commands.
Reached from `supertag-menu''s Setup group; kept separate so the
top-level popup does not get crowded with rarely-used virtual column,
analytic demo view, and database migration commands."
  [["Virtual Columns"
    ("vc" "Create"  supertag-menu--virtual-column-create)
    ("ve" "Edit"    supertag-menu--virtual-column-edit)
    ("vd" "Delete"  supertag-menu--virtual-column-delete)
    ("vl" "List"    supertag-menu--virtual-column-list)]
   ["Analytics"
    ("ap" "Priority matrix (demo)"      supertag-menu--view-priority-matrix-demo)
    ("ab" "Progress dashboard (demo)"   supertag-menu--view-progress-dashboard-demo)
    ("af" "Effort distribution (demo)"  supertag-menu--view-effort-distribution-demo)]
   ["Migration"
    ("ma" "Migrate DB to new arch"          supertag-migrate-database-to-new-arch)
    ("mp" "Convert properties to fields"    supertag-batch-convert-properties-to-fields)
    ("mi" "Add IDs to org headings"         supertag-migration-add-ids-to-org-headings)
    ("mt" "Migrate tag IDs"                 supertag-menu--migrate-tag-ids)]])

(provide 'supertag-menu)

;;; supertag-menu.el ends here
