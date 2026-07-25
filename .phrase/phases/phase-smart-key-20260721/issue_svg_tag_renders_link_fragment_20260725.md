# issue021 — Non-tag Org objects render as SVG tags

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
`supertag-view-helper--valid-inline-tag-match-p`, but the broad matcher was
controlled by independent source/table/comment/face/link exceptions. Relative
links exposed the first visible leak; the same model also admitted inline
code, macro/target objects, drawer content, example blocks and hashes embedded
in words or HTML entities.

## Boundary decision

- Accept: `#token` at line start or after whitespace, inside an Org headline or prose paragraph.
- Preserve: Unicode, emoji, hierarchy `/`, `C++` and non-whitespace punctuation in tag names.
- Reject: embedded/escaped hashes and every competing Org object or metadata context.
- Keep unchanged: sync extraction and persisted tag-name format; this issue only governs visual/point interpretation.

## Fix

The shared validator now checks one token boundary and one Org element
context. Four context helpers plus accumulated priority/link/face exceptions
were removed instead of extending the special-case list.

## Verification

- The focused self-check failed on the bracket-link fixture before the fix and passes afterward.
- The expanded 12-case matrix failed on embedded hashes before hardening and passes afterward.
- Focused Smart Key ERT: 11/11 passed.
- Full stable ERT suite: 297/297 passed.
- Batch load, check-parens, byte compilation and `git diff --check` passed.
- Actual font-lock property checks distinguish prose tags from inline code and embedded fragments.
- A 1000-line mixed-content font-lock smoke test completed in 0.177 seconds.

## Tracking

- Task: `task005`
- User confirmation: pending live-buffer verification
- Resolved At/By/Commit: pending
