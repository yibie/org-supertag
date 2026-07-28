# issue026 — Emacs Lisp function quotes pollute tag completion

## Environment

- Org buffer with tag completion and inline SVG rendering enabled
- Active vault contains prose examples such as `#'zettel-follow`

## Reproduction

1. Sync an Org heading whose paragraph contains Emacs Lisp function references.
2. Type `#` in a normal Org paragraph to open tag completion.
3. Observe `'zettel-follow`, `'zettel-export` and `'zettel-preview)` annotated as `[tag]`.

## Expected vs actual

- Expected: `#'function` remains Emacs Lisp syntax and never becomes a tag candidate.
- Actual: the whitespace-token matcher accepts any non-space after `#`; sync creates tag entities and completion lists their IDs.
- Expected: SVG badge text is subordinate to surrounding prose.
- Actual: the default `0.78` scale renders 16px text at a 20px frame character height.

## Investigation and root cause

The active vault confirms both the source paragraph and matching node/tag entities.
Rendering, extraction and completion did not share a tag-name validity check, so the
function-quote prefix passed all three paths. The SVG cache key also omitted the font
scale, allowing a previous size to survive a live scale change.

## Fix

- Reject tag names whose first character after `#` is `'` through one shared predicate.
- Apply that predicate to rendering, sync extraction and completion candidates.
- Keep historical Store entities untouched; filtering is reversible and avoids destructive cleanup.
- Reduce the default SVG font scale from `0.78` to `0.68` and include it in the cache key.

## Verification

- Focused self-check reproduced all three `#'zettel-*` false positives before the fix.
- The same command passes after the fix and verifies a 14px SVG label at 20px frame height.
- Focused extractor/Smart Key ERT: 30/30 passed.
- Full stable ERT: 314/314 passed.
- Main package batch load, changed-file `check-parens`, byte compilation and `git diff --check` passed;
  byte compilation retains pre-existing warnings only.
- Generated SVG before/after comparison: 93/100 visual verdict, above the 90 pass threshold.
- Live-buffer confirmation: pending.

## Tracking

- Task: `task011`
- User confirmation: pending live-buffer verification
- Resolved At/By/Commit: pending
