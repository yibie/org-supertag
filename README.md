# Org-SuperTag 5.0 – Structured knowledge, inside Emacs, on your own files

[中文](./README_CN.md) | [English](./README.md)

Org-SuperTag turns your plain Org headings into a **structured, queryable knowledge base**.  
No external services. No Python. No lock-in. Your `.org` files stay yours — we just make them smarter.

> **⚠️ Upgrading from 5.2.0 or earlier?**  
> Before enabling `supertag-use-global-fields`, complete the **global field migration** first.  
> See [`doc/GLOBAL-FIELD-MIGRATION-GUIDE.md`](doc/GLOBAL-FIELD-MIGRATION-GUIDE.md) for step-by-step instructions.

> **Why this matters**: Ever tried to find "all papers I haven't read yet" across your notes? Or "all tasks due this week assigned to @alice"? Plain Org-mode can't do this without painful manual tagging and grep. Org-SuperTag makes it as easy as clicking a column header.

> **📖 Ready to dive in?** Start with **[A Day with Org-SuperTag](doc/A-DAY-WITH-ORG-SUPERTAG.org)** — a complete walkthrough of one person's daily workflow, with copy-paste Elisp you can tangle into your config. (中文版：[Org-SuperTag 的一天](doc/A-DAY-WITH-ORG-SUPERTAG_CN.org))

---

## What you get (and why it's easier)

| Without Org-SuperTag | With Org-SuperTag |
|---|---|
| Manually typing `:PROPERTIES:` drawers for every field | Type `#tag` once, define fields once, fill values in a Table View |
| `grep` + regex to find "high priority tasks this week" | `M-x supertag-search` — structured query, instant results |
| Copy-pasting between notes to link related items | `M-x supertag-add-reference` — one keystroke, bidirectional |
| Every new project means rebuilding your tracking system from scratch | Define a `#project` tag schema once, reuse forever |
| "Where did I write that meeting note?" | Query `#meeting` by date, participant, or decision |

**The core idea**: You keep writing Org files normally. Org-SuperTag reads them, builds a structured index, and gives you database-like views *on top of* your plain text.

---

## 30-second installation

```emacs-lisp
;; With straight.el
(straight-use-package '(org-supertag :host github :repo "yibie/org-supertag"))

;; Tell SuperTag where your Org files are
(setq org-supertag-sync-directories '("~/org/"))

;; Initialize once — this scans all your files
M-x supertag-sync-full-initialize
```

That's it. No API keys, no database server, no configuration wizard. Your existing Org files are already compatible.

---

## File nodes: Org-roam, Denote, or both

This setting only affects file-level nodes. Heading nodes continue to use Org IDs normally.

```emacs-lisp
;; Default: files with a top-level :ID: (Org-roam style)
(setq org-supertag-file-id-source 'org-roam)

;; Files with #+IDENTIFIER: (Denote style)
(setq org-supertag-file-id-source 'denote)

;; Mixed directory: recognize either format per file
(setq org-supertag-file-id-source 'auto)

;; Do not create file-level nodes
(setq org-supertag-file-id-source 'disabled)
```

Use `auto` when Org-roam and Denote files share a sync directory. Links are generated from each node's own identity: Org-ID nodes use `id:`, while Denote file nodes use `denote:`. A file without the selected persistent identity remains an ordinary Org file; SuperTag does not invent a temporary ID for it.

After changing the setting, run `M-x supertag-sync-full-rescan`.

---

## The three things you need to know

Org-SuperTag is built on three simple ideas:

### 1. `#tag` turns a heading into a record

```org
* Attention Is All You Need #paper
```

The `#paper` tag means "this heading belongs to the 'paper' collection." Like tagging in any system — but with superpowers.

### 2. Tags have fields (like database columns)

Once you've tagged something as `#paper`, you define what information you want to track:

```
authors  →  text
year     →  number
venue    →  text
status   →  select (unread / reading / done)
rating   →  number (1–5)
```

You define these **once per tag** (in the Schema View, `M-x supertag-view-schema`). Every `#paper` node automatically gets these fields.

### 3. Views let you browse, fill, and query your data

- **Table View** (`M-x supertag-view-table`): Like a spreadsheet for your tagged nodes. Sort by any column, filter, bulk edit.
- **Node View** (`M-x supertag-view-node`): Edit a single node's fields with auto-completion.
- **Kanban** (`M-x supertag-view-kanban`): Board-style view for workflow tags (`#task`, `#project`).

---

## Step-by-step: your first 5 minutes

Let's say you're a researcher. You have papers scattered across your notes.

### Step 1: Tag a paper

Go to any Org heading and run `M-x supertag-add-tag`, type `paper`:

```org
* Attention Is All You Need #paper
```

### Step 2: Define what a "paper" tracks

`M-x supertag-view-schema` → find `paper` → add fields:

| Field | Type |
|-------|------|
| `authors` | text |
| `year` | number |
| `status` | select: unread, reading, done |
| `rating` | number 1–5 |

### Step 3: Fill in data

`M-x supertag-view-table` → choose tag `paper`. You'll see a table with all `#paper` nodes. Click any cell to edit. Sort by year to find recent papers. Filter by `status = unread` to see your reading queue.

### Step 4: Tag more papers

Go to other paper headings, add `#paper`. They appear in the table automatically.

**That's it. You now have a queryable research library.** No copy-paste, no PROPERTIES drawers, no manual organization.

---

## Real workflows (with commands you can copy)

### 📚 Academic reading queue

```org
* Diffusion Models Survey #paper
* ViT Explained #paper
* CLIP Paper #paper
```

Define fields on `#paper`: `authors`, `year`, `status` (unread/reading/done), `rating`.

**Daily workflow**:
1. `M-x supertag-view-table` → tag `paper` → sort by `status`
2. Filter to `unread` → pick one → `o` to jump to the heading
3. After reading: click `status` cell → select `done` → rate it

**Why it's convenient**: You find papers by status and rating, not by scrolling through 50 headings and reading each title.

### 📋 Project task tracking

```org
* Rewrite sync layer #task #project
* Fix auth bug #task
* Deploy v2.1 #task
```

Define fields on `#task`: `status`, `priority`, `due`, `assignee`.

**Daily workflow**:
1. `M-x supertag-view-kanban` → tag `task` → columns by `status`
2. Drag tasks between columns as they progress
3. `M-x supertag-search` → `(and (tag "task") (field "priority" "high"))` for urgent items

**Why it's convenient**: Your tasks live in their natural Org files (meeting notes, project files), but you see them all in one board.

### 📝 Meeting notes with decisions

```org
* Sprint Planning 2024-11-15 #meeting
```

Define fields on `#meeting`: `date`, `participants`, `decisions`, `action-items`.

**Workflow**:
1. `M-x supertag-capture` → choose `meeting` template → fill fields
2. Later: `M-x supertag-search` → `(tag "meeting")` → filter by date range
3. Find "all decisions from Q4" in seconds

**Why it's convenient**: Meeting notes live where they belong (project files), but you query across all of them at once.

---

## Commands you'll use every day

| What you want to do | Command | What happens |
|---|---|---|
| Tag something | `M-x supertag-add-tag` | Adds `#tag` inline, node appears in that tag's table |
| See all nodes of a tag | `M-x supertag-view-table` | Spreadsheet view. Sort, filter, edit cells |
| Edit a node's fields | `M-x supertag-view-node` | Form view with completion, pickers, and validation |
| Board view | `M-x supertag-view-kanban` | Drag-and-drop between columns |
| Define tag fields | `M-x supertag-view-schema` | Add/remove fields, set types, configure inheritance |
| Capture new node | `M-x supertag-capture` | Quick entry with template, adds to your Org file |
| Search | `M-x supertag-search` | Structured query. Save results to file |
| Link related nodes | `M-x supertag-add-reference` | Bidirectional link between any two nodes |
| Promote selected text to a concept | `M-x supertag-promote-concept` | Creates/reuses a concept node, references it from the current node, and keeps the text plain |
| Highlight concept mentions | `M-x supertag-concept-link-mode` | Shows concept title/alias mentions as amber semantic highlights, not stored links |
| Act on the object at point | `M-x supertag-smart-key` | Opens or edits the current tag, node, field, link, button, or table cell |
| Full database rebuild | `M-x supertag-sync-full-rescan` | Safe — just re-reads your Org files |

Beyond single-command lookups, Org-Supertag has a small S-expression query
language for combining tags, fields, dates, and full-text search, e.g.
`(and (tag "task") (not (field "status" "done")))`. Write one in a
`org-supertag-query-block` babel block, save it with `M-x supertag-query-save`
for reuse, or build one interactively with `M-x supertag-query-build`. See
`doc/QUERY.md` for the full grammar.

Optional keybindings:

```emacs-lisp
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c s p") #'supertag-promote-concept)
  (define-key org-mode-map (kbd "C-c s o") #'supertag-concept-open-at-point))
```

### How concept mentions behave

- Promotion accepts non-empty text inside an Org node. It reuses a unique heading node or creates one, adds one explicit reference, and leaves the selected text unchanged. A same-title file node is not silently converted into a concept.
- Mentions are display-only. Org links, code/verbatim, comments and `COMMENT` subtrees, keywords/drawers, source blocks, and tables are not highlighted.
- A title or alias shared by multiple concepts is ambiguous. SuperTag does not choose a target from hash-table order; the mention stays plain and promotion reports the conflict.

After changing concept titles or aliases outside SuperTag, run `M-x supertag-concept-refresh` in enabled buffers.

### Semantic Smart Key

Run `M-x supertag-smart-key` on an inline tag, node, field, concept mention,
Org link, Emacs button, or table cell to perform its default action. With a
prefix argument (`C-u M-x supertag-smart-key`), it opens `supertag-menu`.
The command has no default keybinding, so existing Org and View keys are unchanged.

---

## Why this doesn't add friction

The most common fear about "structured tools" is: *"Will I spend more time organizing than actually working?"*

Org-SuperTag avoids this in three ways:

### 1. Your files are still plain Org

You never *have* to use the SuperTag views. Write Org normally. The `#tag` markers are just text. If you stop using SuperTag tomorrow, your files are 100% readable Org-mode — you just have some extra `#tag` annotations that don't hurt anything.

### 2. Fields are defined once, used everywhere

You define `status`, `priority`, `due` for `#task` **one time**. Every `#task` node you create from then on gets those fields automatically. The upfront cost is 30 seconds; the payoff is permanent.

### 3. Sync is automatic and safe

Org-SuperTag reads your files on a timer (configurable via `doc/SYNC-CONFIGURATION.md`). It never modifies your Org files unless you explicitly edit through a SuperTag view. If the database gets corrupted, `M-x supertag-sync-full-rescan` rebuilds it from scratch.

### Compare the effort

**Without SuperTag** — tracking papers:
- Manually write `:PROPERTIES:` drawer with `:authors:`, `:year:`, `:status:`
- `grep` for `status.*unread` across files
- No sorting, no filtering, no table view

**With SuperTag** — tracking papers:
- Add `#paper` to headings (2 seconds each)
- Define fields once in Schema View (30 seconds)
- Table View for sorting, filtering, editing (instant)

**The win**: For 10 papers, you save ~5 minutes of PROPERTIES typing and get a live-updating table view for free. For 100 papers, the difference is hours.

---

## When to go deeper

Org-SuperTag grows with you. Start simple, add power when you need it:

| After you're comfortable with... | Try this |
|---|---|
| Tags and Table View | **Automation** — rules that auto-fill fields based on conditions (`doc/AUTOMATION-SYSTEM-GUIDE.md`) |
| Manual capture | **Capture Templates** — predefined forms for common entries (`doc/CAPTURE-GUIDE.md`) |
| Basic queries | **Query Blocks** — embed live query results inside Org files (`doc/ABOUT-QUERY-BLOCK.md`) |
| Default views | **Custom Views** — build your own dashboards (`doc/VIEW_FRAMEWORK_DEV_GUIDE.md`) |
| Single vault | **Multi-Vault** — separate databases for work/personal (`doc/SYNC-CONFIGURATION.md`) |
| Writing plugins | **Plugin Guide** — extend with your own extractors and services (`doc/ORG-SUPERTAG-PLUGIN-GUIDE.md`) |

---

## Data storage (where things live)

| What | Where | Format |
|---|---|---|
| Your Org files | Whatever directories you configure | Plain `.org` text |
| Structured field values | `~/.emacs.d/org-supertag/supertag-db.el` | Emacs Lisp data |
| Sync state | `~/.emacs.d/org-supertag/sync-state.el` | File mtimes and hashes |
| Daily backups | `~/.emacs.d/org-supertag/backups/` | Timestamped DB snapshots |

**Org files own your text and structure; the database owns your typed data.** Org files (headings, body text, `:ID:` properties) are the source of truth for what a node *is*, and `M-x supertag-sync-full-rescan` can always re-derive nodes and tags from them. But schema definitions, field values, Table/Board view layouts, and automation rules live *only* in `supertag-db.el` — a rescan does not invent them from org text, because plain org text doesn't encode them. Losing the database without a backup or a synced copy loses that data for real, the same as losing any other file you can't regenerate; back it up (see above) or sync it (see below) accordingly.

**6.0 changed the on-disk format of `supertag-db.el` — this is a one-way upgrade.** Since 6.0, the database is written in a deterministic, one-entity-per-line format (what makes git-native sync's field-level merging possible). Older builds (5.9.x and earlier) cannot read entities out of this format — a 5.9.x Emacs pointed at a 6.0+ database will look like it loaded successfully but show an empty store, because the old code only reads the file's first line. Upgrading is safe and automatic (opening an old database with 6.0+ migrates and re-saves it), but **going back to 5.9.x afterward is not** unless you restore a pre-upgrade copy. Two safety nets exist for that: an automatic `backups/supertag-db-premigrate-<old-version>-<timestamp>.el` snapshot the moment an out-of-date database is first loaded, and a `backups/supertag-db-preformat6-<timestamp>.el` snapshot the moment a database still in the old file format is first re-saved (covering the case where the stored version already looked current but the file itself had not been re-saved yet). Neither is ever deleted by the daily-backup cleanup. To downgrade: quit Emacs, restore the newest matching snapshot over `supertag-db.el`, then reopen with the older build. `M-x supertag-doctor` reports both the current on-disk format and how many of each snapshot type exist.

---

## Syncing across machines

Two ways to keep `supertag-db.el` consistent across machines: **git-native sync** (recommended — it actually understands merges), or pointing a sync-folder service at the data directory (simpler to set up, but "last writer wins").

### Git-native sync (recommended)

**Machine 1** (first time setting this up):

```
M-x supertag-git-setup
```

This puts your vault under git: it initializes a repository if there isn't one already, migrates the database into `<repo-root>/.supertag/supertag-db.el` if it wasn't already tracked inside the repo, and configures a semantic merge driver for `supertag-db.el` so concurrent edits from different machines merge field-by-field instead of clobbering each other. It then prompts for a remote URL — give it one (any empty git remote: GitHub, a self-hosted server, a NAS) and it creates the first commit and pushes; leave the prompt empty to stay local-only for now (a fully valid, supported state — re-run the command later once you have a remote).

**Machine 2** (and every machine after that):

```
M-x supertag-git-clone
```

Give it the same remote URL and a local directory. It clones, configures the merge driver for *this* machine, and loads the database (or rebuilds it from the cloned org files if the database is missing or unreadable).

**Every clone must run its own setup.** `merge.supertag-db.driver` lives in `.git/config`, which git never syncs between clones — so `supertag-git-clone` configuring the driver on machine 2 isn't optional busywork, it's what makes *that* machine's merges semantic instead of falling back to git's default line-based text merge (see "Conflicts" below for what that fallback looks like).

**Optional automation:** `M-x supertag-git-sync-mode` runs a background loop that debounce-commits your changes, fetches/merges on a timer and on focus, and pushes — including catching up any commits that piled up while you were offline, without waiting for a new edit to trigger it. Without this mode, `git pull`/`git push` (or `magit-pull`/`magit-push`) by hand works identically; the mode is a convenience layer, not where correctness lives.

**Conflicts.** The database's own edits merge automatically in the common case — different nodes or fields touched on each side. When the *same* field is edited differently on both sides, or plain `.org` prose is edited on the same line by both sides, git leaves that file with a real, unresolved conflict: for `supertag-db.el` itself, it refuses to load until resolved (the error names the file and points here); for `.org` files, the sync scanner skips importing anything still conflict-marked rather than ingesting garbage. Either way, `M-x supertag-doctor` (section "8. Git Sync") lists exactly what's unresolved — resolve it by hand or with `magit`/`git checkout --merge`, same as any other git conflict.

**Upgrade all synced machines together.** The 6.0 database format (see "Data storage" above) is readable only by 6.0+. A 5.9.x machine that pulls a database saved by a 6.0 machine will *appear* to load it successfully but show an empty store — old code reads only the file's first line and never errors. Its save guards prevent actual data loss (an empty in-memory store refuses to overwrite a non-trivial file), but everything will look gone until you upgrade that machine. So: upgrade org-supertag on **every** machine that shares the vault before any of them saves under 6.0.

### Sync-folder services (Dropbox/iCloud/Syncthing)

If you'd rather not use git, you can keep `~/.emacs.d/org-supertag/` (or wherever `supertag-db-file` lives) inside a Dropbox/iCloud/Syncthing-style folder so it follows you between machines — know the tradeoffs before you rely on it:

**Safest mode: one writer at a time.** `supertag-db.el` is a single serialized file. The sync service's job is "replicate the whole file, last writer wins" — it has no idea two Emacs sessions edited different parts of it, so it cannot merge them. If both machines save, one save clobbers the other, silently. The reliable workflow is: **fully quit Emacs on machine A (`C-x C-c`, not just closing the frame) before you start editing on machine B.**

This matters even if you think you're "just reading" on machine A: the auto-save timer (`supertag-db-auto-save-interval`, default 300 seconds) writes the database in the background whenever anything in the session marked it dirty, so an Emacs process left open is a background writer whether you're actively typing or not.

**The 5.9.0 database lock does not cover this.** Since 5.9.0, Org-SuperTag takes an advisory lock (`supertag-db-lock`) on the database file to stop two Emacs instances *on the same machine* from stepping on each other. That lock is a local file-locking primitive (a symlink next to the DB file) — it only ever protects against a same-machine double-open, and most sync services don't propagate lock artifacts reliably or promptly enough for it to mean anything across machines.

**The presence warning.** To give sync-folder users at least a heads-up (not a lock — a sync service's multi-minute propagation delay means it can't physically be one), Org-SuperTag writes a small `supertag-presence.json` file next to the database recording which host last touched it and when. When you load the database and another host's presence looks like it was active in roughly the last 5 minutes (`supertag-presence-stale-seconds`), you'll see a loud warning naming that host and the risk. **What to do when you see it:** if you're sure the other machine is done (Emacs quit there), it's safe to proceed — the warning is one-shot and won't repeat until the other host claims presence again. If you're not sure, go quit Emacs on that other machine first. Run `M-x supertag-doctor` any time to see the current presence file's host, age, and verdict (own / foreign-active / foreign-stale). Set `supertag-presence-enable` to `nil` to turn this off entirely.

**Do not sync `sync-state.el` or `backups/`.** Both live in the same data directory as the database but are local, per-machine bookkeeping (`sync-state.el` tracks file mtimes/hashes for *this machine's* filesystem; `backups/` is disk space you don't need to duplicate across machines). If your sync tool syncs the whole data directory, exclude those two paths where the tool allows it; at worst, having them get overwritten just costs an extra full rescan, it doesn't lose data.

This is a stopgap, not a solution — real multi-machine sync needs something that understands merges, which is exactly what the git-native sync described above does. If concurrent editing across machines is what you're after, use that instead; a sync-folder service only ever gives you the single-writer discipline above.

---

## Migration from older versions

> **⚠️ 5.9.x → 6.0.0**: The database file format changed (see "Data storage" above) — upgrading is automatic, but downgrading afterward needs a restored backup. Back up `~/.emacs.d/org-supertag/` before upgrading if you might need to go back.

> **⚠️ 5.2.0 → 5.3.0**: Complete the [global field migration](doc/GLOBAL-FIELD-MIGRATION-GUIDE.md) before enabling `supertag-use-global-fields`.

### From SuperTag 4.x

```emacs-lisp
;; 1. Back up your data directory (~/.emacs.d/org-supertag/)
;; 2. Load and run migration
M-x load-file RET supertag-migration.el RET
M-x supertag-migrate-database-to-new-arch RET
```

### From plain Org files

No migration needed. Add `#tag` to headings, define fields, and start using views. Your existing files work as-is.

---

## Troubleshooting quick reference

| Problem | Fix |
|---|---|
| Database looks wrong | `M-x supertag-sync-cleanup-database` then `M-x supertag-sync-full-rescan` |
| Auto-sync not starting | Check `org-supertag-sync-directories` is set correctly |
| Specific file not syncing | `M-x supertag-sync-analyze-file` |
| Field values missing after rescan | Fields are DB-only; they survive rescan unless DB was wiped |
| Sync freezes Emacs | See `doc/SYNC-CONFIGURATION.md` for performance tuning |

---

## Comparison with other tools

| Tool | Org-SuperTag's difference |
|---|---|
| **Org-roam** | Org-roam is a graph of linked notes. SuperTag is structured tables on top of Org. They can coexist. |
| **Notion** | Notion locks your data in a proprietary cloud. SuperTag works offline on your own files. |
| **Obsidian** | Obsidian is a different editor. SuperTag is native Emacs — no context switching. |
| **org-ql** | org-ql queries Org properties inline. SuperTag stores field data separately, enabling views, automation, and a query DSL that doesn't litter your Org files. |

---

## Further reading

- **Sync configuration**: `doc/SYNC-CONFIGURATION.md`
- **📖 A Day with Org-SuperTag**: `doc/A-DAY-WITH-ORG-SUPERTAG.org` — complete workflow tutorial with tangleable Elisp
- **Automation rules**: `doc/AUTOMATION-SYSTEM-GUIDE.md`
- **Capture system**: `doc/CAPTURE-GUIDE.md`
- **Virtual columns**: `doc/VIRTUAL_COLUMNS.md`
- **Plugin development**: `doc/ORG-SUPERTAG-PLUGIN-GUIDE.md`
- **Architecture deep-dive**: `doc/ONTOLOGY-ARCHITECTURE_cn.md`
- **View framework**: `doc/VIEW_FRAMEWORK_DEV_GUIDE.md`
- **vs old architecture**: `doc/COMPARE-NEW-OLD-ARCHITECTURE.md`

---

Org-SuperTag is developed as free software under the GPLv3. Contributions, bug reports, and feature requests are welcome on GitHub.
