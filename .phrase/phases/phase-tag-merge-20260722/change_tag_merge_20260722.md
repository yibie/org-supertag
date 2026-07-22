# Changes — Tag Merge

## 2026-07-22 — Fix

- Files: `supertag-ops-node.el`, `test/tag-merge-test.el`
- Function: `supertag--validate-node-data`
- Change: permit `:title nil` only for valid level-0 file nodes, matching the established file-node contract. Merge no longer asks users to repair or invent titles before changing tags.
- Regression: `tag-merge-allows-untitled-file-node` reproduces the production failure through the real merge transaction.
- Verification: focused 14/14; full suite 292/292.
- Related: `issue017`, `task005`

## 2026-07-22 — Add/Modify

- Files: `supertag-ops-tag-merge.el`, `supertag-core-store.el`, `supertag-view-helper.el`, `test/tag-merge-test.el`
- Functions: `supertag-tag-merge-plan`, `supertag-tag-merge-execute`, `supertag-store-remove-legacy-tag-fields`
- Changes:
  - Added a no-write plan that resolves source fields, detects incompatible definitions, per-node value conflicts, inheritance cycles, missing/unwritable files, and unsaved affected buffers.
  - Added one transaction for target schema creation, legacy/global field migration, node tags, inheritance, relation identities, automations, loaded saved queries/views, and source deletion.
  - Reused existing Org tag rewriting; added temporary file snapshots so partial writes and derived-index failures restore both Store and files.
  - Tightened file and buffer tag rewriting to exact tag-token matches so merging `#task` cannot rewrite `#tasking`.
  - Added 13 focused ERT cases covering new/existing targets, conflict choices, multi-value fields, global fields, relation deduplication, incompatible definitions, and injected rollback failures.
- Risk: parseable structured references are migrated; unparsed free text is intentionally warning-only. Existing destination fields are never removed by this command.
- Verification: focused 13/13; related node/view/transaction 56/56; corrected full suite 291/291; new module byte-compiles without warnings; `checkdoc-file` and `git diff --check` pass.
- Related: `task001`, `task002`

## 2026-07-22 — Add/Modify

- Files: `supertag-view-schema.el`, `README.md`, `README_CN.md`, `test/run-tests.sh`
- Functions: `supertag-schema-merge-marked-tags` and its prompt/preview helpers
- Changes:
  - Reused Schema View's existing marks: `m m` marks tags and `m M` merges them.
  - Supports a marked/existing destination or a new destination, source-field selection, explicit definition/value resolution, one preview, and one destructive confirmation.
  - Updated contextual help, English/Chinese command tables, and stable test discovery.
- Verification: Schema keymap is covered by ERT; full suite 291/291.
- Related: `task003`, `task004`
