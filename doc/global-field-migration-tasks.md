# Global Field Migration – Task Tracker
Instructions: update `Status` (TODO/IN-PROGRESS/DONE), add notes when completing; record verification vs. objectives. IDs are atomic tasks (`taskNNN`) grouped by plan phases (see `doc/global-field-migration-plan.md`).

| ID      | Status | Description | Phase | Notes (verification, deltas) |
|---------|--------|-------------|-------|------------------------------|
| task001 | DONE   | Draft design RFC for global fields model, IDs/naming, ordering, conflict policy (ref Plan Phase 0). | P0 | RFC in `doc/global-field-migration-rfc.md` |
| task002 | DONE   | Decide legacy compatibility strategy (shims vs. hard cut) and rollout flag. | P0 | See RFC compat/flag section |
| task003 | DONE   | Define association ordering storage (per tag) and default field order resolution. | P0 | See RFC ordering rules |
| task004 | DONE   | Add global fields collection (CRUD in store/persistence) without wiring callers yet. | P1 | Collections + store helpers in `supertag-core-store.el` |
| task005 | DONE   | Add tag↔field association collection (CRUD, ordering metadata). | P1 | Store helpers in `supertag-core-store.el` |
| task006 | DONE   | Implement node→field value storage keyed by field id/name; keep legacy read path intact temporarily. | P1 | Store helpers in `supertag-core-store.el` |
| task007 | DONE   | Wire schema cache rebuild hooks for field/association changes. | P1 | Store writes trigger cache rebuild; load-store triggers rebuild when flag enabled |
| task008 | DONE   | Build migration script skeleton with dry-run and logging. | P2 | In `supertag-migration.el` (global field section), stats/logging |
| task009 | DONE   | Implement migration: dedupe field definitions into global fields (conflict resolution). | P2 | In `supertag-migration.el` |
| task010 | DONE   | Implement migration: create tag↔field associations with order. | P2 | In `supertag-migration.el` |
| task011 | DONE   | Implement migration: rewrite node field values to node→field records; handle node-reference resolution. | P2 | In `supertag-migration.el` (no special node-ref logic yet) |
| task012 | DONE   | Make migration idempotent/rerunnable; add summary reporting and backups/export. | P2 | Dry-run default, skip-on-existing, backup reminder/log in `supertag-migration.el` |
| task013 | DONE   | Refactor ops-field API to global-field CRUD; add association helpers. | P3 | Added `supertag-ops-global-field.el` (field CRUD, associations, node values) |
| task014 | DONE   | Update ops-tag to use associations (add/remove tag ensures defaults, order). | P3 | Field add/remove uses global defs+associations; move up/down reorders associations |
| task015 | DONE   | Update ops-node field accessors to use global field ids; retire tag-nested access. | P3 | Node add/remove tag init/clear global values; field get/set/remove route to global storage when flag on |
| task016 | TODO   | Provide optional compatibility shims for legacy tag-field callers (if chosen in P0). | P3 |  |
| task017 | DONE   | Update query service to parse/execute global-field filters; add tests. | P4 | Field filter now scans global field-values when flag on; ERT in `dev-note/query-global-field-tests.el` |
| task018 | DONE   | Update sync/import/export to dedupe shared global fields; property naming rules. | P4 | Export/import now use global field order per node, dedupe shared fields, global property lookup with legacy fallbacks |
| task019 | DONE   | Update capture service to prompt/apply fields from associations. | P4 | Capture prompts dedupe global fields by slug and use global-aware get/set; tests in `dev-note/capture-global-fields-tests.el` |
| task020 | DONE   | Update automation/behavior triggers to global-field change events. | P4 | Added global field event handling in `supertag-automation.el` and `supertag-automation-sync.el`; new conditions: `global-field-equals`, `global-field-changed`, `global-field-test`; supports `:field-values` path in rule index; tests in `dev-note/automation-global-field-tests.el` |
| task021 | DONE   | Update table/kanban views for global-field columns/groups; avoid duplicates. | P5 | Table columns use global slug keys and dedupe; Kanban grouping reads global values and uses options order |
| task022 | DONE   | Update node view/editor to merged field set per node with association order. | P5 | Node view dedupes shared global fields when rendering; counts use slug-based dedupe |
| task023 | DONE   | Update query UI and search autocomplete to global fields. | P5 | Kanban/query UI prompts dedupe global fields; completions avoid duplicate shared fields |
| task024 | IN-PROGRESS | Testing: core + migration + services + UI per Research §9; add automated coverage where possible. | P6 | Test plan in `dev-note/global-field-test-plan.md`; added ERT for query/capture/UI (`dev-note/query-global-field-tests.el`, `dev-note/capture-global-fields-tests.el`, `dev-note/ui-global-field-tests.el`) |
| task025 | TODO   | Documentation/changelog: upgrade guide, migration instructions, flags. | P6 |  |
| task026 | TODO   | Rollout: feature gate or opt-in flag; finalize removal of legacy paths after validation. | P6 |  |
| task027 | TODO   | Enforce :options normalization on field definitions to prevent option loss. | P3 | Done in `supertag-ops-tag.el` |
