# Global Field Model – Design RFC (Phase 0)

Context: transition from tag-scoped fields to global fields with tag↔field associations and node↔field values. Informs Phase 1–6 (see plan) and resolves Phase 0 decisions. References research doc §§2–8.

## 1) Data model decisions
- Global Field ID: `field-id` is a sanitized, lowercase/underscored name (stable slug). Store display name separately (`:name`). Enforce global uniqueness case-insensitively.
- Collections:
  - `:fields` (global registry): plist keys `:id` (slug), `:name` (display), `:type`, `:config` (options/validators), `:created-at`, `:modified-at`.
  - `:tag-field` associations: entries with `:tag-id`, `:field-id`, `:order` (integer per tag), optional `:required` flag reserved.
  - Node field values: `:field-values` keyed by node-id → field-id → value. Value keyed by field-id (not tag), so shared fields dedupe naturally.
- Schema cache: cache global field map, tag→field ordered list, and node→merged-field ordered list builder. Rebuild on field or association change.
- Ordering: per-tag association order preserved (`:order`). Node render order = node tags in stored order → within each tag, association order → first occurrence wins (dedupe). Orphans (values without association) appended by field-id sort for safety.

## 2) Conflict and validation policy
- Name collisions: if two tag field defs sanitize to same `field-id` but differ in type/config, fail migration and report conflicts. Exact matches (same type/config) are merged.
- Type/config validation: reuse existing field type validators; ensure options/enum compatibility before merging.
- Node-reference resolution: values stay as IDs; display resolves titles. Migration logs unresolved titles if encountered.

## 3) Compatibility and rollout
- Feature flag: `supertag-use-global-fields` (defcustom, default nil until migration). Gate new storage and APIs; allow opt-in testing.
- Legacy shims: provide read-only compatibility layer for old tag-scoped getters during transition; writes go through global APIs. Marked deprecated and removed after rollout (Phase 6 task026).
- Import/export naming: prefer field-id keys (e.g., `:STATUS:`). Legacy `TAG_FIELD` and `ST_` are still parsed during migration/compat to avoid data loss.

## 4) API alignment (guidance for later phases)
- Field CRUD: move to global (`supertag-field-*`) operating on field-id.
- Association CRUD: new helpers `supertag-tag-add-field`, `supertag-tag-remove-field`, `supertag-tag-reorder-fields`.
- Node values: `supertag-node-set-field` / `supertag-node-get-field` by field-id; tag context used only for UI grouping/order.
- Query/sync: operate on field-id (with display name shown). Property exports use field-id keys; shared fields export once per node.

## 5) Migration guardrails
- Steps: dedupe defs → create fields → create associations with order → rewrite node values to field-id → drop/lock legacy after verification.
- Idempotent: detect existing field-id/associations; skip if matched; error on type conflict.
- Logging: counts per step, conflict list, unresolved node-references, summary for rerun safety.

## 6) Testing acceptance (for later phases)
- Must-pass: field CRUD roundtrip, association ordering, node set/get shared field, migration dry-run vs apply, sync/import/export with shared fields, query on shared field, UI dedupe rendering.

