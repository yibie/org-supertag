## Org-Supertag 5.2.0 – Global Field Database Migration

This guide describes how to upgrade an existing Org-Supertag database to the new global field model introduced in 5.2.0, where fields are first-class entities and no longer nested under individual tags.

> **Important:** Before running any migration, make a fresh backup of your Supertag data directory.

### 1. Enable the Global Field Model

Add the following to your Emacs configuration before loading `org-supertag`:

```elisp
(setq supertag-use-global-fields t)
```

This:
- Enables the new collections `:field-definitions`, `:tag-field-associations`, and `:field-values`.
- Makes ops/services/UI use the global field model as the source of truth.

### 2. Run the Migration in Dry-Run Mode

First, run a dry-run to see what would change without writing anything:

```elisp
(require 'org-supertag)
(require 'supertag-migration)

;; Ensure dry-run is enabled (default is t)
(setq supertag-migration-dry-run t)

;; Dry-run: scan and log, but do not write
(supertag-migration-run-global-fields)
```

Check:
- The minibuffer message for a quick summary.
- The `*supertag-migration*` buffer for detailed stats and any conflicts:
  - `fields=... associations=... values=... skipped=... conflicts=...`
  - `Conflicts: ...` entries indicate field definition collisions (same id, different type/config) or missing definitions.

If there are conflicts, resolve them (e.g., by adjusting field definitions) before proceeding.

### 3. Execute the Real Migration (Writes Enabled)

Once the dry-run output looks good and you have a backup:

```elisp
(require 'supertag-migration)

;; Turn off dry-run, or pass a prefix arg / FORCE-WRITE
(setq supertag-migration-dry-run nil)

;; Perform the actual migration (writes to the store)
(supertag-migration-run-global-fields t)
```

This will:
- Deduplicate tag-scoped field definitions into global field definitions in `:field-definitions`.
- Create ordered tag↔field associations in `:tag-field-associations`.
- Rewrite node-level values from nested `:fields` into flat `:field-values` (node-id → field-id → value).
- Log a summary and any conflicts to `*supertag-migration*`.

### 4. Verify and Continue Using Global Fields

After the migration:

- Keep `supertag-use-global-fields` set to `t` in your config.
- Open key views to verify data:
  - Table / Node / Kanban views show fields once per node (shared fields dedupe correctly).
  - Editing a field value triggers your existing automation rules (e.g., rules written with `field-equals` / `field-changed`).
  - Queries and capture flows see the expected field values.

If you see issues, consult:
- `doc/global-field-migration-rfc.md` – design decisions and conflict policy.
- `doc/global-field-migration-plan.md` / `doc/global-field-migration-tasks.md` – phased rollout plan and checklist.

Once you are confident in the new model, you can treat the global field collections as the authoritative storage going forward. Legacy `:fields` storage remains for compatibility and can be retired in a future release once all data has been verified.

