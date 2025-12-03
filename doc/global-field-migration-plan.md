# Global Field Migration – Phased Plan
References: see `doc/global-field-migration-research.md` for background (§2 storage, §3 schema/cache, §4 ops API, §5 services, §6 UI, §7 migration, §8 risks, §9 testing).

## Phase 0 – Design sign-off
- Objectives: finalize target data model (global fields + tag↔field map + node→field values), conflict policies, ordering rules, naming/ID strategy.
- Scope: storage layout, schema cache shape, association ordering, backward-compat shims.
- Approach: produce design RFC referencing Research §2–3, §7–8; decide ID vs name keys and ordering metadata.

## Phase 1 – Storage & schema foundations
- Objectives: add global field collection, tag↔field association collection, flat node→field value storage APIs; cache rebuild hooks.
- Scope: `supertag-core-store.el`, `supertag-core-persistence.el`, `supertag-core-schema.el`, `supertag-core-state.el`.
- Approach: introduce new collections and accessors; keep legacy collections read-only until migration; add schema cache invalidation (Research §2–3).

## Phase 2 – Migration tooling
- Objectives: one-shot migration from tag-scoped fields to global fields; detect conflicts; idempotent rerun; optional rollback/export.
- Scope: new migration script (e.g., `supertag-migration-global-fields.el`) and CLI/command entrypoint.
- Approach: scan tag field defs → create global fields; build tag↔field links with order; rewrite node field values; log conflicts and unresolved references (Research §7–8).

## Phase 3 – Operations layer adoption
- Objectives: refactor ops APIs to global-field model; provide thin shims for legacy callers if needed.
- Scope: `supertag-ops-field.el`, `supertag-ops-tag.el`, `supertag-ops-node.el`, `supertag-ops-schema.el`.
- Approach: new APIs for field CRUD and association CRUD; node set/get uses field IDs; tag add/remove updates default values; optional compatibility wrappers (Research §4).

## Phase 4 – Services layer adaptation
- Objectives: make services consume global-field APIs; enable global-field queries and sync.
- Scope: `supertag-services-query.el`, `supertag-services-sync.el`, `supertag-services-capture.el`, automation services as needed.
- Approach: query parser/executor on field IDs/names; sync/import/export dedupe shared fields; capture prompts from associations (Research §5).

## Phase 5 – UI/view updates
- Objectives: render/edit using merged global-field set per node; avoid duplicates; support grouping by global fields.
- Scope: `supertag-view-table.el`, `supertag-view-kanban.el`, `supertag-view-node.el`, `supertag-ui-commands.el`, `supertag-ui-query-block.el`, `supertag-ui-search.el`.
- Approach: column/group definition on global fields; merged field order per node using association order; autocomplete uses global field list (Research §6).

## Phase 6 – Validation & rollout
- Objectives: verify end-to-end, document usage changes, provide rollback guidance.
- Scope: test matrix per Research §9; docs/changelog; opt-in flag or feature gate.
- Approach: write automated tests where available; manual flows (sync, capture, UI); publish upgrade guide and changelog; keep migration command guarded until validated.

