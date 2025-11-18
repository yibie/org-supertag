# Org-SuperTag 5.0 – Structured knowledge management in pure Emacs Lisp

Org-SuperTag turns your plain Org headings into a **structured, queryable knowledge base** –  
without leaving Emacs, without external services, and without Python.

- Each `#tag` behaves like a **database table**
- Each heading is a **record**; its structured fields live in Org-SuperTag's database
- Views, queries and automations are all driven by this structured layer

This document is a **draft** of a new README for the 5.0 architecture.

---

## What is Org-SuperTag?

Traditional Org-mode gives you:

- Headlines
- TODO keywords
- Tag strings like `:project:research:`

Org-SuperTag adds a **strongly structured layer** on top:

```org
* Website Redesign #project
  - status: in-progress
  - priority: high
  - due: 2024-12-15
  - owner: @team
```

You still edit plain text Org files, but Org-SuperTag:

- Scans and synchronizes your files
- Stores a normalized representation in an in-memory database
- Provides fast queries and multiple views on top of it

The goal is: **Notion-level structure, Org-mode-level control.**

---

## Highlights in 5.0

5.0 is a major internal simplification while keeping the same user-facing ideas.

- **Pure Emacs Lisp**  
  No Python, no EPC, no virtualenvs. Everything runs inside Emacs.

- **Single source of truth**  
  All structured data is kept in a single hash-table–backed store,  
  instead of multiple partially synchronized SQLite tables.

- **Faster sync and simpler debugging**  
  No cross-process RPC means less overhead and fewer moving parts.

- **Same Org files, new engine**  
  The on-disk Org files remain the primary data; the internal engine is what changed.

For a deeper comparison between the old architecture and 5.0, see:

- `doc/COMPARE-NEW-OLD-ARCHITECTURE.md`
- `doc/COMPARE-NEW-OLD-ARCHITECHTURE_cn.md`

---

## 30‑second quick start

Minimal setup with straight.el:

```emacs-lisp
;; 1. Install
(straight-use-package '(org-supertag :host github :repo "yibie/org-supertag"))

;; 2. Configure where your Org files live
(setq org-supertag-sync-directories '("~/org/"))

;; 3. Initialize the database (run once)
M-x supertag-sync-full-initialize
```

After initialization:

- Add tags: `M-x supertag-add-tag`
- Open a node view: `M-x supertag-view-node`
- Run a query: `M-x supertag-search`

---

## Core concepts & data model

Org-SuperTag is built around a small set of concepts:

- **Node**  
  A single Org heading, uniquely identified and tracked across syncs.

- **Tag**  
  Written inline as `#tag` in the heading title. Every `#tag` acts like a logical table.

- **Field definition**  
  Part of a tag's schema (`:fields` list) that declares a field name, type, options,
  defaults and validation. Field definitions live on the tag, not in the Org file text.

- **Field value**  
  For each `(node-id, tag-id, field-name)` triple, Org-SuperTag stores a value in its
  `:fields` collection. These values are kept in the database and do **not** have to be
  rendered as `- key: value` lines in the Org file.

- **Schema & views**  
  How you choose to display and edit these fields (forms, tables, kanban, etc.).

Example (on disk vs in the database):

```org
* Attention Is All You Need #paper
```

On disk you only need the headline and `#paper` tag. In the Org-SuperTag database, this
node might have field values such as:

- `authors`: `"Vaswani et al."`
- `year`: `2017`
- `venue`: `"NIPS"`
- `status`: `"read"`
- `rating`: `5`

Internally this becomes something like:

- tag: `"paper"`
- field definitions on the tag: `authors`, `year`, `venue`, `status`, `rating`
- field values per node: one value for each `(node-id, "paper", field-name)` triple
- node metadata: file path, outline path, scheduled date, etc.

You can then:

- Filter all `#paper` with `status = unread`
- Sort by `year` or `rating`
- Display them in a custom view or query block

For the higher-level design and motivation behind query blocks, see:

- `doc/ABOUT-QUERY-BLOCK.md`
- `doc/ABOUT-QUERY-BLOCK_cn.md`

---

## Typical workflows

### Academic / research notes

```org
* Diffusion Models for XYZ #paper
```

On disk you keep the headline and tag. In the database you define fields on `#paper`
such as `authors`, `year`, `venue`, `status`, `topic`, and store per-node values for
those fields. You can then:

- List all `#paper` tagged as `generative-models`
- Filter by `status` (e.g. unread → reading queue)
- Sort by year or rating

### Project management

```org
* Rewrite sync layer #task #project
```

Tags like `#task` and `#project` can define fields such as `status`, `priority`,
`owner`, `project`, etc. Field values live in the Org-SuperTag database and are edited
via views (forms, tables, kanban), not by hand-writing `- key: value` lines.

Combine this with:

- `M-x supertag-view-kanban` for board-style views
- Custom queries for “high priority tasks this week”

### Meetings & logs

```org
* Sprint Planning 2024-11-15 #meeting
```

The `#meeting` tag can define fields like `date`, `participants`, `decisions`, and those
values are stored per node in the database. Later you can query:

- All `#meeting` in a date range
- All meetings that mention a project or person

---

## Queries & views

The underlying query engine is Lisp-based. At the lowest level, you can call:

```emacs-lisp
;; High priority projects
(supertag-search
 '(and (tag "project")
       (field "priority" "high")))

;; Unread papers
(supertag-search
 '(and (tag "paper")
       (field "status" "unread")))
```

On top of this engine, Org-SuperTag provides:

- **Interactive searches** (`M-x supertag-search`)
- **Views** such as node forms, tables and kanban boards
- **Query blocks** that live inside Org files and render dynamic results

See:

- `doc/COMPLETION-GUIDE.md`
- `doc/AUTOMATION-SYSTEM-GUIDE.md`
- `doc/CAPTURE-GUIDE.md`

for more examples of how queries integrate with capture, completion and automation.

---

## Key commands (preview)

This is not an exhaustive list, but it covers the most common entry points:

| Command                    | What it does                             |
| -------------------------- | ---------------------------------------- |
| `M-x supertag-add-tag`     | Add an inline `#tag` to the current node |
| `M-x supertag-view-node`   | Edit structured fields for the node      |
| `M-x supertag-search`      | Run a structured search                  |
| `M-x supertag-capture`     | Capture a new node with tags and fields  |
| `M-x supertag-view-table`  | Show tabular view for a set of nodes     |
| `M-x supertag-view-kanban` | Kanban-style view for tasks / projects   |

As the UI stabilizes, this section should be kept in sync with `supertag-ui-*.el`.

---

## Migration & compatibility

### From Org-SuperTag 4.x

5.0 uses a different internal storage engine. A one-time migration is required.

**Before you do anything: make a backup of your existing Org-SuperTag data directory.**

1. Locate your old data directory (usually under `~/.emacs.d/org-supertag/`)
2. Make a copy of it somewhere safe (or put it under version control)
3. In Emacs, load the migration code:

   ```emacs-lisp
   M-x load-file RET supertag-migration.el RET
   ```

4. Run the migration:

   ```emacs-lisp
   M-x supertag-migrate-database-to-new-arch RET
   ```

5. Restart Emacs

If the migration fails or the resulting data looks wrong, you can:

- Quit Emacs
- Restore your backup of the Org-SuperTag data directory
- Re-open Emacs and stay on the old version until the issue is resolved

### From plain Org / other tools

If you are coming from:

- Plain Org files with ad-hoc tags
- Other knowledge tools (Notion, Obsidian, Org-roam, etc.)

You can adopt Org-SuperTag incrementally:

1. Pick a single area (e.g. `projects` or `papers`)
2. Start adding inline `#tag` markers to the relevant headings
3. Define fields on those tags (via the tag/schema UI) and start filling values from
   node views
4. Gradually convert old entries when you touch them, validating results via search
   and views

There is no hard requirement to convert everything at once.

---

## Data storage & backup

Org-SuperTag treats your Org files as the **source of truth for node content and tags**.  
Structured field values, relations and views live in a companion database, stored under
a dedicated directory, typically:

- `~/.emacs.d/org-supertag/`

This directory contains:

- Indexed representations of nodes and tags
- Per-node field values (the structured data that does **not** live in Org text)
- Auxiliary state for sync and views
- Automatic snapshot/backup data

Recommendations:

- Put both your Org files and the Org-SuperTag data directory under regular backups
- If you use git for your Org files, consider excluding transient cache files and only
  keeping snapshot-style backups of the Org-SuperTag directory

If the internal database is corrupted, you can usually recover by:

```emacs-lisp
M-x supertag-sync-cleanup-database
M-x supertag-sync-full-initialize
```

This forces a full rescan from the Org files.

---

## Troubleshooting

Some common entry points when something looks wrong:

- **Database looks inconsistent / corrupted**

  ```emacs-lisp
  M-x supertag-sync-cleanup-database
  ```

- **You want to see what the database contains**

  ```emacs-lisp
  M-x supertag-search
  ```

- **Sync issues for a specific file**

  ```emacs-lisp
  M-x supertag-sync-analyze-file
  ```

For more advanced debugging and automation, refer to:

- `doc/AUTOMATION-SYSTEM-GUIDE.md`
- `doc/AUTOMATION-SYSTEM-GUIDE_cn.md`

---

## How does this compare to other tools?

Very short version:

| Tool     | Org-SuperTag angle                          |
| -------- | ------------------------------------------- |
| Org-roam | Org-roam is graph/links; SuperTag is tables |
| Obsidian | SuperTag is native to Emacs & Org           |
| Notion   | SuperTag is offline, programmable, and text |

Org-SuperTag is not trying to replace everything; it focuses on:

- **Structured fields on top of plain text**
- **Programmable queries and views**
- **Staying inside Emacs and Org-mode**

---

## Further reading

If you want to understand the system in depth, the following documents are good next steps:

- `doc/ABOUT-QUERY-BLOCK.md` / `doc/ABOUT-QUERY-BLOCK_cn.md`
- `doc/AUTOMATION-SYSTEM-GUIDE.md` / `doc/AUTOMATION-SYSTEM-GUIDE_cn.md`
- `doc/CAPTURE-GUIDE.md` / `doc/CAPTURE-GUIDE_cn.md`
- `doc/COMPARE-NEW-OLD-ARCHITECTURE.md` / `doc/COMPARE-NEW-OLD-ARCHITECHTURE_cn.md`

This draft README is meant to be the stable, user-facing overview that points into
those more detailed documents.
