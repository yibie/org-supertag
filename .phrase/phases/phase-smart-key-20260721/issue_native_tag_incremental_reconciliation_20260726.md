# issue022 — Native tag incremental reconciliation is not authoritative

## Environment

- `supertag-sync-legacy-tags-policy` uses `read-only`, `lazy-convert`,
  `preserve` or `ignore`
- A headline has Org native `:tag:` metadata

## Reproduction

1. Run a full rescan so native tags enter the node `:tags`.
2. Edit and save the same headline, triggering incremental sync.
3. Compare the node `:tags` and its `:node-tag` relations.

## Expected vs actual

- Expected: the selected legacy policy defines one consistent result for
  full and incremental sync.
- Actual: the extractor reads native tags only with `:full-rescan-p`, while
  relation creation is additive and has no matching removal policy.

## Investigation

Task008 aligns inline tag extraction without deleting historical relations.
Adding relation cleanup there would make incremental sync treat its incomplete
native-tag input as authoritative and could remove valid user data.

## Proposed fix

- Define native-tag visibility for all four legacy policies.
- Make full and incremental extraction return the same authoritative tag set.
- Reconcile node-tag relations only after that contract is tested.
- Preserve tag definitions and their field schemas unless the user explicitly
  deletes them.

## Verification

- Pending task009: policy × full/incremental sync matrix.

## Tracking

- Task: `task009`
- Status: open
