# Org-Supertag Sync Configuration Guide

This guide explains how Org-Supertag sync works, how to configure it for your
workflow, and how to troubleshoot common issues. It assumes you are familiar
with Org-mode but not necessarily with Org-Supertag internals.

## Table of Contents

1. [How Sync Works (in 30 Seconds)](#how-sync-works-in-30-seconds)
2. [Sync Modes](#sync-modes)
3. [Configuration Variables Reference](#configuration-variables-reference)
4. [Common Configuration Recipes](#common-configuration-recipes)
5. [Commands Reference](#commands-reference)
6. [Troubleshooting](#troubleshooting)
7. [Safety Guards](#safety-guards)

---

## How Sync Works (in 30 Seconds)

Org-Supertag maintains a local database of your Org headings (nodes), their
tags, properties, and relationships. The **sync service** reads your `.org`
files and keeps the database in sync with what is on disk.

- **Discovery**: Sync scans configured directories for `.org` files (by default,
  anything matching `\\.org$`).
- **Change detection**: Each file is hashed; only files whose content actually
  changed get re-parsed. Unchanged files are skipped.
- **Batch processing**: Work is split into small batches processed inside a
  single database transaction. This avoids freezing Emacs on large file sets.
- **Auto-save**: The database is periodically auto-saved and backed up (daily).

You can sync **manually** on demand, or let Org-Supertag sync **automatically**
on a timer (auto-sync).

---

## Sync Modes

### Mode 1: Manual vs Auto-Sync

Org-Supertag operates in one of two sync-trigger modes:

#### Manual Sync (default for uncustomized setups)

If you have not set `org-supertag-sync-directories`, auto-sync will **not** run.
You trigger sync explicitly when you want it:

- `M-x supertag-sync-full-rescan` — re-scan every managed `.org` file.
- Open an Org file and save it — the saved file is re-indexed on next tick
  (if auto-sync is on) or will be picked up by the next manual `full-rescan`.

This mode is **safe for large repositories** and **recommended for cautious
users**. You are always in control of when the database is updated.

#### Safe Auto-Sync (enabled when directories are configured)

When you set `org-supertag-sync-directories` to one or more directories,
auto-sync becomes active by default:

- Emacs startup waits a few seconds, then starts a periodic sync timer.
- Every `supertag-sync-auto-interval` seconds (default: 900 = 15 minutes),
  sync scans for new and modified files and processes them in small batches.
- Between timer ticks, sync also runs during Emacs idle time (after 
  `supertag-sync-idle-delay` seconds of inactivity).

Auto-sync **does not** run a full rescan on every tick — it only processes
files that changed since the last sync. This keeps the background load minimal.

To **disable** auto-start entirely while keeping your directory configuration:

```elisp
(setq supertag-sync-auto-start nil)
```

You can still manually start auto-sync at any time with
`M-x supertag-sync-start-auto-sync`.

---

### Mode 2: Unified vs Vault

`org-supertag-sync-directories-mode` controls how multiple sync directories
are treated:

#### Unified Mode (default)

```elisp
(setq org-supertag-sync-directories-mode 'unified)
```

All directories in `org-supertag-sync-directories` share a **single database**.
Nodes from different directory trees coexist in one store. This is the simplest
setup and works well when all your Org files belong to one "knowledge base."

#### Vault Mode

```elisp
(setq org-supertag-sync-directories-mode 'vaults)
```

Each directory in `org-supertag-sync-directories` becomes an **isolated vault**
with its own database and sync state. Only one vault is active at a time.

Benefits:
- Separate tagging domains (e.g., "Work Projects" vs "Personal Notes").
- Independent databases mean smaller stores and faster operations.
- Changing vaults is explicit — no accidental cross-contamination.

To switch vaults: `M-x supertag-vault-activate` (prompts you to pick a vault).

Auto-switching on buffer change is off by default. To enable it:

```elisp
(setq org-supertag-vault-auto-switch t)
```

> **Note**: Auto-switching loads the vault's entire database on every buffer
> change through an Org file, which can be expensive. Prefer explicit activation
> for larger vaults.

---

## Configuration Variables Reference

### Directory & Scope

| Variable | Default | Description |
|---|---|---|
| `org-supertag-sync-directories` | `nil` | List of absolute paths to monitor. If `nil`, auto-sync is effectively disabled. |
| `org-supertag-sync-directories-mode` | `'unified` | `unified` (one DB) or `vaults` (one DB per directory). |
| `supertag-sync-exclude-directories` | `nil` | Directories to exclude from sync. Takes precedence over include directories. |
| `supertag-sync-file-pattern` | `"\\.org$"` | Regex matching filenames to sync. Default: all `.org` files. Change to `"\\.org\\'\|\\.org_archive\\'"` to include archive files. |

### Vault-Specific

| Variable | Default | Description |
|---|---|---|
| `org-supertag-active-sync-directory` | `nil` | Active vault root (internal; set by `supertag-vault-activate`). |
| `org-supertag-vault-auto-switch` | `nil` | When `t`, entering an Org buffer auto-switches the active vault. |
| `org-supertag-vault-modeline-indicator` | `t` | Show which vault the current Org buffer belongs to in the mode line. |

### Sync Timing & Rhythm

| Variable | Default | Description |
|---|---|---|
| `supertag-sync-auto-interval` | `900` | Seconds between auto-sync timer ticks. Higher = less frequent background work. |
| `supertag-sync-idle-delay` | `1.0` | Seconds of Emacs idle time before an idle-triggered sync runs. |
| `supertag-sync-quiet-when-idle` | `t` | When `t`, suppress messages when an idle-triggered sync found nothing to do. |
| `supertag-sync-auto-start` | `t` | Whether to schedule auto-sync on Emacs startup. Requires `org-supertag-sync-directories` to be set. |
| `supertag-sync-auto-start-initial-delay` | `3` | Seconds to wait after startup before the first auto-start attempt. |
| `supertag-sync-auto-start-retry-interval` | `5` | Seconds between retries when sync directories are not yet available (e.g., network mounts). |
| `supertag-sync-auto-start-max-retries` | `24` | Max retries (~2 minutes total at default interval). Gives up if directories never become available. |

### Processing & Integrity

| Variable | Default | Description |
|---|---|---|
| `supertag-sync-hash-props` | `(:raw-value :olp :tags :todo :priority :content :properties)` | Properties included in the content hash for change detection. `:id` is always included. |
| `supertag-sync-auto-create-node` | `nil` | When `t`, headings without IDs get auto-generated IDs during sync. Disabled by default to avoid interfering with embed blocks. |
| `supertag-sync-node-creation-level` | `1` | Minimum heading level for auto-node-creation (1 = all levels). |

### Safety Guards

| Variable | Default | Description |
|---|---|---|
| `supertag-sync-snapshot-guard` | `t` | When `t`, destructive operations (deletions) require a complete snapshot of the sync state. |
| `supertag-sync-orphan-grace-seconds` | `3600` | Grace period (1 hour) before orphaned nodes can be deleted. Prevents accidental deletion of freshly-created nodes. |
| `supertag-sync-max-delete-ratio` | `0.5` | If orphan deletions would remove more than 50% of total nodes, the deletion pass is aborted. |
| `supertag-sync-max-delete-count` | `1000` | Hard cap on the number of orphan nodes that can be deleted in a single GC pass. |

### Tag Handling

| Variable | Default | Description |
|---|---|---|
| `supertag-tag-style` | `'inline` | How tags are written to Org headlines: `inline` (#tags), `org` (:tag:), `both`, or `auto`. |
| `supertag-sync-legacy-tags-policy` | `'read-only` | How to handle legacy `:tag:` in headlines: `read-only` (import, don't modify), `lazy-convert` (convert on edit), `preserve`, or `ignore`. |

### Internal State File

| Variable | Default | Description |
|---|---|---|
| `supertag-sync-state-file` | `sync-state.el` in data dir | File storing per-file modification timestamps and hashes. Auto-managed; you should not need to change this. |

---

## Common Configuration Recipes

### Recipe 1: Safe Defaults (Manual-Only)

For large repositories or cautious users who want full control:

```elisp
(use-package org-supertag
  :ensure nil  ; or :straight, depending on your setup
  :config
  ;; Do NOT set org-supertag-sync-directories — this keeps auto-sync off.
  ;; When you want to sync, run M-x supertag-sync-full-rescan manually.

  ;; Optionally disable the auto-start flag (already inert without directories,
  ;; but explicit is better):
  (setq supertag-sync-auto-start nil)

  ;; Safety: don't auto-create IDs
  (setq supertag-sync-auto-create-node nil))
```

### Recipe 2: Single-Directory Auto-Sync

For a moderate-sized Org directory (a few hundred files or fewer):

```elisp
(use-package org-supertag
  :ensure nil
  :config
  ;; Sync one directory
  (setq org-supertag-sync-directories '("~/org/"))

  ;; Keep unified mode (default)
  (setq org-supertag-sync-directories-mode 'unified)

  ;; Enable auto-sync (default: t, but explicit for clarity)
  (setq supertag-sync-auto-start t)

  ;; Sync every 30 minutes (instead of default 15)
  (setq supertag-sync-auto-interval 1800)

  ;; Wait 5 seconds of idle time before syncing
  (setq supertag-sync-idle-delay 5.0)

  ;; Tag style
  (setq supertag-tag-style 'inline)

  ;; First time: run M-x supertag-sync-full-rescan to build the initial database
  )
```

### Recipe 3: Multi-Directory Vaults (Isolated Databases)

For separate knowledge domains with independent databases:

```elisp
(use-package org-supertag
  :ensure nil
  :config
  ;; Two isolated vaults
  (setq org-supertag-sync-directories '("~/org/work/" "~/org/personal/"))
  (setq org-supertag-sync-directories-mode 'vaults)

  ;; Auto-switch vaults when moving between buffers
  (setq org-supertag-vault-auto-switch t)

  ;; Show vault name in mode line
  (setq org-supertag-vault-modeline-indicator t)

  ;; Auto-start sync for whichever vault is active
  (setq supertag-sync-auto-start t)

  ;; First time in each vault: switch to it (M-x supertag-vault-activate),
  ;; then run M-x supertag-sync-full-rescan to build the initial database.
  )
```

### Recipe 4: Large Repository (Performance-Tuned)

For repositories with thousands of Org files:

```elisp
(use-package org-supertag
  :ensure nil
  :config
  (setq org-supertag-sync-directories '("~/org/big-repo/"))

  ;; Start manually — don't auto-sync at startup
  (setq supertag-sync-auto-start nil)

  ;; When you do turn on auto-sync, be gentle:
  (setq supertag-sync-auto-interval 3600)   ; once per hour
  (setq supertag-sync-idle-delay 10.0)      ; wait 10s idle before acting

  ;; Exclude heavy directories
  (setq supertag-sync-exclude-directories
        '("~/org/big-repo/archive/"
          "~/org/big-repo/attachments/"))

  ;; First time: run M-x supertag-sync-full-rescan in a dedicated session.
  ;; This may take a while on first run. Progress messages appear in *Messages*.
  )
```

### Recipe 5: Exclude Directories by Pattern

To exclude certain subdirectories from all configured sync directories:

```elisp
(setq supertag-sync-exclude-directories
      '("~/org/archive/"
        "~/org/daily-logs/"
        "~/org/third-party-exports/"))
```

The exclusion check uses a path prefix match — anything under these directories
is skipped. Exclusions take priority over included directories.

---

## Commands Reference

### Sync Lifecycle

| Command | Description |
|---|---|
| `M-x supertag-sync-full-rescan` | Force a complete re-scan of every managed Org file. Builds or rebuilds the entire database. Displays a summary of nodes created, updated, and deleted. |
| `M-x supertag-sync-start-auto-sync` | Start the periodic auto-sync timer. Use with a prefix argument to override the interval: `C-u 600 M-x supertag-sync-start-auto-sync` (sync every 10 minutes). |
| `M-x supertag-sync-stop-auto-sync` | Stop the auto-sync timer. Sync stops until you call `start-auto-sync` or restart Emacs. |
| `M-x supertag-sync-cleanup-database` | Run full database maintenance: validate all nodes against source files (marks missing nodes as orphaned), then garbage-collect orphaned nodes. Safe to run at any time. |

### Vault Management

| Command | Description |
|---|---|
| `M-x supertag-vault-activate` | Switch to a different vault (prompts with a list). Only available when `org-supertag-sync-directories-mode` is `vaults`. Loads that vault's database and restarts auto-sync for it. |

### Diagnosis

| Command | Description |
|---|---|
| `M-x supertag-get-modified-files` | Return a list of files that have changed since their last sync. Useful for understanding what `full-rescan` would process. |

---

## Troubleshooting

### Problem: Emacs freezes at startup with "Supertag: directories ready; starting auto-sync"

**Cause**: Auto-sync is starting and attempting to scan a large directory tree.

**Solutions** (in order of impact):

1. **Disable auto-start** — the simplest fix:
   ```elisp
   (setq supertag-sync-auto-start nil)
   ```
   You can still sync manually with `M-x supertag-sync-full-rescan`.

2. **Run the first full-rescan in a dedicated session** — disable auto-start, restart
   Emacs, then run `M-x supertag-sync-full-rescan`. Monitor the `*Messages*` buffer
   for progress. Once the initial scan is done, re-enable auto-start.

3. **Increase timing intervals** to reduce the background load:
   ```elisp
   (setq supertag-sync-auto-interval 3600)  ; 1 hour
   (setq supertag-sync-idle-delay 10.0)     ; 10 seconds idle
   ```

4. **Exclude heavy subdirectories**:
   ```elisp
   (setq supertag-sync-exclude-directories '("~/org/archive/"))
   ```

### Problem: Sync says "directories unavailable" and never starts

**Cause**: One or more directories listed in `org-supertag-sync-directories`
don't exist (yet). Common with network mounts or symlinked directories that
aren't mounted at Emacs startup.

**Solutions**:

1. Verify each directory exists:
   ```
   M-: (mapcar #'file-exists-p org-supertag-sync-directories)
   ```

2. If directories are on a slow network mount, increase the retry window:
   ```elisp
   (setq supertag-sync-auto-start-max-retries 60)  ; ~5 minutes of retries
   ```

3. If the directories will never be available in this session, remove them from
   the list and restart auto-sync:
   ```elisp
   (setq org-supertag-sync-directories (remove "/unavailable/path" org-supertag-sync-directories))
   M-x supertag-sync-start-auto-sync
   ```

### Problem: The database doesn't match what's in my Org files

**Symptoms**: Tags missing from search results, old headings still appearing,
new headings not found.

**Solutions** (try in order):

1. **Run a full rescan**: `M-x supertag-sync-full-rescan`. This is the most
   thorough fix — it re-reads every managed file and rebuilds the node index.

2. **Clean up orphaned nodes**: `M-x supertag-sync-cleanup-database`. This
   removes database entries whose source files no longer exist.

3. **Check your file pattern**: If you use non-standard Org file extensions,
   make sure `supertag-sync-file-pattern` matches them:
   ```elisp
   (setq supertag-sync-file-pattern "\\.org\\'\\|\\.org_archive\\'")
   ```

4. **Check exclusion rules**: Files under `supertag-sync-exclude-directories`
   are silently skipped. Verify you haven't accidentally excluded your working
   directory:
   ```
   M-: supertag-sync-exclude-directories
   ```

### Problem: Sync seems slow (Emacs lags periodically)

**Cause**: Auto-sync is processing files during background ticks.

**Solutions**:

1. Increase the interval between sync ticks:
   ```elisp
   (setq supertag-sync-auto-interval 3600)  ; 1 hour
   ```

2. Increase the idle delay so sync only runs during longer idle periods:
   ```elisp
   (setq supertag-sync-idle-delay 10.0)
   ```

3. Temporarily stop auto-sync: `M-x supertag-sync-stop-auto-sync`.
   Resume when ready: `M-x supertag-sync-start-auto-sync`.

4. Exclude directories with frequently-changing files you don't need indexed:
   ```elisp
   (setq supertag-sync-exclude-directories '("~/org/logs/" "~/org/drafts/"))
   ```

### Problem: Mass deletion of nodes after a directory move/reorganization

**Symptom**: After moving Org files to a new location, running sync triggers the
safety guards and aborts deletions.

**What's happening**: The sync engine detects that files have disappeared from
the original path and wants to orphan their nodes. The safety guards
(`supertag-sync-max-delete-ratio` and `supertag-sync-max-delete-count`)
intervene to prevent accidental mass deletion.

**Solution**:

1. Add the new location to `org-supertag-sync-directories` before moving files.
2. Run `M-x supertag-sync-full-rescan` — this imports nodes at the new paths.
3. After confirming everything is correct, run `M-x supertag-sync-cleanup-database`
   to remove nodes still pointing to the old (now non-existent) paths.

### Emergency: Persistent timer errors or sync in a broken state

If sync timers get into a broken state (repeated errors, unexpected behavior):

1. Stop auto-sync: `M-x supertag-sync-stop-auto-sync`
2. Run a full rescan to rebuild state: `M-x supertag-sync-full-rescan`
3. Restart auto-sync: `M-x supertag-sync-start-auto-sync`

If problems persist across Emacs restarts, try clearing the sync state file:

1. Quit Emacs.
2. Delete the sync state file (usually `sync-state.el` in Org-Supertag's data
   directory — check `M-: supertag-sync-state-file` to find the exact path).
3. Restart Emacs and run `M-x supertag-sync-full-rescan` to rebuild state.

---

## Safety Guards

Org-Supertag has several built-in safety mechanisms to prevent accidental data
loss. Understanding these helps you tune them or work around them when
intentionally reorganizing your files.

### Orphan Grace Period

When a node's source file disappears (e.g., the file was moved or deleted), the
node becomes "orphaned" (its `:file` property is set to `nil`). The sync engine
will **not** delete orphaned nodes immediately. It waits
`supertag-sync-orphan-grace-seconds` (default: 3600 = 1 hour) before garbage
collection can remove them. During this window, if the file reappears (or a
full-rescan re-creates the node), the orphan period is reset.

To adjust the grace period:

```elisp
(setq supertag-sync-orphan-grace-seconds 300)  ; 5 minutes
```

### Mass Deletion Thresholds

Two gates prevent the sync engine from deleting too many nodes at once:

- **Ratio gate** (`supertag-sync-max-delete-ratio`, default `0.5`): If candidate
  deletions exceed 50% of the total node count, the entire deletion pass is aborted.
- **Count gate** (`supertag-sync-max-delete-count`, default `1000`): An absolute
  upper bound on deletions in a single garbage collection pass.

If either gate triggers, you'll see a warning in `*Messages*`. This usually
means you've moved or deleted a large number of Org files. See the
[Troubleshooting section](#problem-mass-deletion-of-nodes-after-a-directory-movereorganization)
above for the recommended recovery procedure.

Disable gates temporarily (at your own risk) to force-clean after an intentional
reorganization:

```elisp
(setq supertag-sync-max-delete-ratio 1.0)
(setq supertag-sync-max-delete-count most-positive-fixnum)
M-x supertag-sync-cleanup-database
;; Then restore the defaults.
```

### Snapshot Guard

When `supertag-sync-snapshot-guard` is `t` (default), destructive sync
operations (node deletions, state cleanups) require a "complete snapshot" —
meaning the sync engine has confirmed the state of all managed files before
removing anything.

In normal operation you should leave this on. It prevents the edge case where
a partially-loaded state leads to incorrect deletions.

### Internal Modification Detection

When Org-Supertag itself modifies a file (e.g., writing tags to a headline), it
records the modification timestamp internally. The sync engine skips these
files on the next scan cycle, avoiding unnecessary re-parsing of content it
just wrote.

---

## Further Reading

- **Main README**: `README.md` — project overview and installation.
- **Automation Guide**: `doc/AUTOMATION-SYSTEM-GUIDE.md` — for trigger-based
  workflows that may interact with sync.
- **Capture Guide**: `doc/CAPTURE-GUIDE.md` — for note capture workflows.
- **Phase Design Document**: `.phrase/phases/phase-sync-improve-20251216/spec_sync_improve_20251216.md`
  — the detailed design document that drove the sync improvements.
