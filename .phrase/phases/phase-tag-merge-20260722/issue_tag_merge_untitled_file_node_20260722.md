# issue017 — Tag merge rejects an untitled file node

## Environment

- org-supertag `main` after `9cc90e5`
- Existing file node with `:level 0` and `:title nil`
- Merge `#project` into existing `#prj`

## Reproduction

1. Keep a file-level Org ID in a file without `#+TITLE`.
2. Give that file node a source tag.
3. Merge the source tag into another tag from Schema View.

## Expected vs actual

- Expected: the file node keeps `:title nil`; only its tag references change.
- Actual: `supertag-node-update` signals `Node missing required :title field` and the merge rolls back.

## Investigation and root cause

The file-node contract deliberately defines `:title` as `#+TITLE` only, with no fallback. Therefore an untitled `:level 0` node is valid. `supertag--validate-node-data` applied the heading-node title invariant to every node update, so Merge exposed an existing mismatch between validation and the file-node model.

## Fix

Allow `:title nil` only when `:level` is `0`. Heading nodes remain subject to the existing strict title requirement. Merge continues through the normal transaction and does not synthesize or mutate a title.

## Verification

- `tag-merge-allows-untitled-file-node` failed before the fix and passes afterward.
- Focused tag-merge tests: 14/14.
- Full ERT suite: 292/292.

## Tracking

- Task: `task005`
- User confirmation: pending Schema View retry
- Resolved At/By/Commit: pending user confirmation and commit
