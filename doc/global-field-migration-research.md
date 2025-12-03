# Global Field Migration – Technical Research

Goal: move from “fields belong to tags” to “global fields + tag→field associations + node→field values”. This doc maps the technical surface to inform planning.

## 1. Data model contrasts
- Current (nested): node → tag → field-value; definitions live on tags (`:fields` per tag) and values live under `:fields` collection keyed by node-id→tag-id→field-name.
- Target (flat): global field registry; tag↔field mapping; values keyed by node-id→field-id (or field-name); tag is metadata context, not the storage key.

## 2. Storage layer implications
- Collections to introduce: global fields table, tag↔field association table; field values keyed by field-id (or normalized name).
- Existing collections touched: `:fields`, `:tags`, `:nodes`, schema cache.
- Migration requirements: de-duplicate field definitions, build associations, rewrite values, keep rollbacks or backups.
- Constraints: field name uniqueness (global), validation for type/config, referential integrity for associations.

## 3. Schema & cache
- Schema needs to model global fields plus tag→field associations.
- Cache invalidation must handle updates to fields and associations; existing schema cache rebuild hooks will need to fire on field/association changes.
- Field ordering: currently per-tag order; in the new model store order per association or in a separate ordering map.

## 4. Operations API impact
- `supertag-ops-field.el`: becomes global-field CRUD; add association management helpers.
- `supertag-ops-tag.el`: add/remove tag must manage associations and default node values.
- `supertag-ops-node.el`: needs node→field value helpers independent of tag nesting.
- `supertag-ops-schema.el`: expose association operations and cache rebuilds.
- Backward compat: shims for legacy callers expecting tag-scoped fields, or a migration-only compatibility layer.

## 5. Services layer
- Query (`supertag-services-query.el`): parse/execute queries on global fields; enable “find nodes where Status=…”.
- Sync/import/export (`supertag-services-sync.el`, `supertag-service-org.el`): property naming, dedup across tags that share a field; ID vs name resilience.
- Capture (`supertag-services-capture.el`): prompt fields based on tag→field associations; write node→field values.
- Automation/behavior: triggers on field changes must reference global field IDs/names, not tag-scoped keys.

## 6. UI/view layer
- Table/Kanban (`supertag-view-table.el`, `supertag-view-kanban.el`): columns/groups by global field; avoid duplicates when multiple tags share a field.
- Node view/editor (`supertag-view-node.el`, `supertag-ui-commands.el`): render merged field set per node; editing writes node→field value; resolve field display order.
- Query UI (`supertag-ui-query-block.el`, `supertag-ui-search.el`): autocomplete global fields; display consistent labels.

## 7. Migration strategy considerations
- One-shot migration script: scan tag field defs, create global fields, build tag associations, rewrite node field values.
- Idempotency: safe to rerun? detect already-migrated data.
- Rollback: ability to export before/after; or keep legacy collections until cutover completes.
- Telemetry/logging: progress, conflicts, orphaned data.

## 8. Compatibility and risks
- Name collisions: same field name with differing types/options; need conflict resolution policy.
- Ordering differences: per-tag order vs global order; may need per-association order metadata.
- Node-reference fields: resolution must work with global field IDs.
- Performance: flat model improves queries, but cache rebuilds must be efficient.
- Behavior changes: any code assuming tag-bound fields will fail without shims.

## 9. Testing matrix
- Unit: field CRUD, association CRUD, node→field set/get, cache rebuild.
- Migration: before/after snapshots, conflict cases, rerun safety.
- Services: sync import/export with shared fields; capture path; automation triggers.
- UI: table/kanban rendering, node view edits, query autocomplete and execution.

