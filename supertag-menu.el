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
;;   Query, Capture, Sync & Maintenance, and an optional Setup group.
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
(declare-function supertag-rename-tag "supertag-ui-commands" ())
(declare-function supertag-delete-tag-everywhere "supertag-ui-commands" ())
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

;; supertag-setup.el and supertag-automation-templates.el are developed
;; alongside this file; guard every reference with `fboundp' and never
;; `require' them directly from here.
(declare-function supertag-setup "supertag-setup" ())
(declare-function supertag-automation-insert-template "supertag-automation-templates" ())

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
     :if (lambda () (fboundp 'supertag-graph-ui-open)))]
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
   ["Setup"
    ("zs" "Setup wizard"             supertag-setup
     :if (lambda () (fboundp 'supertag-setup)))
    ("zt" "Insert automation template" supertag-automation-insert-template
     :if (lambda () (fboundp 'supertag-automation-insert-template)))]])

(provide 'supertag-menu)

;;; supertag-menu.el ends here
