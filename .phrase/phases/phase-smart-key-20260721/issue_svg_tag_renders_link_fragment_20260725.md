# issue021 — Org link fragment renders as an SVG tag

## Environment

- Org buffer with `supertag-view-style-mode` and SVG tag rendering enabled
- Bracket link whose target contains a `#fragment`

## Reproduction

1. Insert `[[file:Copyright.xhtml#Copyright.xhtml][→ Copyright.xhtml]]`.
2. Enable or refresh inline tag styling.
3. Observe part of the hidden link target rendered as an SVG pill beside the link description.

## Expected vs actual

- Expected: the complete construct remains one Org link; its target is never styled as an inline tag.
- Actual: the broad `#[^[:space:]#]+` font-lock match reaches into the link target and adds an SVG `display` property.

## Investigation and root cause

All face, SVG and point-based tag paths share
`supertag-view-helper--valid-inline-tag-match-p`, but its link exclusion only
searched backward for `://`. Relative file links and other bracket links
therefore bypassed the guard.

## Fix

The shared validator now asks Org's native link recognizer whether the match
belongs to a link. The narrower URL-only helper was removed.

## Verification

- The focused self-check failed on the bracket-link fixture before the fix and passes afterward.
- Focused Smart Key ERT: 11/11 passed.
- Full stable ERT suite: 297/297 passed.
- Batch load, check-parens, byte compilation and `git diff --check` passed.

## Tracking

- Task: `task005`
- User confirmation: pending live-buffer verification
- Resolved At/By/Commit: pending
