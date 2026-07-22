# issue018 — Node View creates IDs while opening

## Environment

- Org heading without an `ID` property
- `M-x supertag-view-node` or Smart Key Node Action

## Reproduction

1. Place point on a heading without an ID.
2. Open Node View directly or through `supertag-smart-key`.
3. Observe a new property drawer and ID in the Org buffer.

## Expected vs actual

- Expected: View activation is read-only. An untracked heading produces a clear user error and the Org text remains unchanged.
- Actual: the direct command and Smart Key both route through `org-id-get-create`.

## Investigation and root cause

`supertag-view-node` called the explicitly mutating `supertag-ui--get-node-at-point`, while the Smart Key Node Action independently called `org-id-get-create` when its read-only target contained no ID. The recognizer itself was read-only, but both View activation paths crossed the identity-write boundary.

## Fix

Node View now resolves only existing IDs. Smart Key rejects a node target without an ID instead of creating one. Explicit commands that create or sync nodes keep their existing behavior.

## Verification

- `supertag-node-view-does-not-create-id` failed before the fix and passes afterward.
- Focused Smart Key tests: 9/9.
- Full ERT suite: 293/293.

## Tracking

- Task: `task003`
- User confirmation: pending live-buffer verification
- Resolved At/By/Commit: pending
