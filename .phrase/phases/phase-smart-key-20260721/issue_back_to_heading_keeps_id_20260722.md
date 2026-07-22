# issue019 — back-to-heading keeps the Org ID

## Environment

- Org heading already tracked as an org-supertag node
- `M-x supertag-back-to-heading`

## Reproduction

1. Run `supertag-back-to-heading` on a node heading.
2. Confirm the Store node is deleted.
3. Inspect the heading's property drawer.

## Expected vs actual

- Expected: the heading keeps its text and subtree but loses `:ID:`; an otherwise empty `PROPERTIES` drawer disappears.
- Actual: only the Store node is deleted, while `:ID:` remains and permits later synchronization to recreate the node.

## Investigation and root cause

`supertag-back-to-heading` called `supertag-node-delete` but never removed the Org identity. The Store and Org file therefore disagreed about whether the heading was still a node.

## Fix

After Store deletion, `supertag-back-to-heading` deletes the `ID` with Org's native `org-entry-delete`. Org preserves unrelated properties and removes an otherwise empty drawer.

## Verification

- `supertag-back-to-heading-removes-id-and-empty-drawer` failed before the fix and passes afterward.
- `supertag-back-to-heading-preserves-other-properties` failed before the fix and passes afterward.
- Focused tests: 11/11 passed.
- Full stable ERT suite: 295/295 passed.
- Batch load, check-parens, byte compilation and `git diff --check` passed.

## Tracking

- Task: `task004`
- User confirmation: pending live-buffer verification
- Resolved At/By/Commit: pending
