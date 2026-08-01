# change_nested_tags_20260624

## 2026-08-01 (task015)

- **Action**: Make nested Tag completion and input navigate one namespace level at a time.
- **Files**:
  - Modify path helpers, completion, shared UI services, Tag command/capture/query/automation readers
  - Modify Schema/View/Table tag selectors and nested-tag completion tests
  - Update nested-tag issue, spec, task and change indexes
- **Behavior**:
  - `#a/` lists only direct children under `a/`; unrelated root Tags are excluded by the completion-table protocol.
  - Slash-terminated candidates navigate without writing; full real paths remain the only stored Tag IDs.
  - Normal candidates have no `[tag]` suffix; only a typed new path receives `[New]`.
  - Add/Change/Capture/Tag Field and query/view selectors share the same hierarchical reader.
- **Simplification**: One pure direct-child helper and one shared reader; no cache, index, Store field, namespace entity or dependency.
- **Verification**: focused ERT 19/19; related View/Query ERT 54/54; full ERT 335/335; completion/inline self-checks; byte compilation, `check-parens`, `git diff --check`; independent review approved.
- **Remaining**: issue009 stays open until the user confirms the corrected popup in their live Corfu configuration.

## 2026-07-29 (task013)

- **Action**: Complete nested tags from authoritative Store synchronization through
  Schema/completion/View/Table interaction and transactional branch rename.
- **Files**:
  - Add `supertag-core-tag-path.el`
  - Modify sync, tag ops/merge, completion, Schema/View/Table and exact text helpers
  - Expand `test/tag-path-test.el` and current phase/spec/issue/change docs
- **Behavior**:
  - Full paths remain canonical IDs; missing parents are virtual namespaces.
  - Namespace aggregation is explicit and read-only in Table; `:extends` remains inheritance.
  - Branch rename migrates descendants and typed tag references with collision/file rollback guards.
  - Exact delete/rename cannot truncate descendant tokens.
- **Verification**: nested-path ERT 15/15; full ERT 330/330; completion/Table
  checks; byte compile/check-parens/diff check; current real vault copy read-only
  verification; Schema visual verdict 92/100.
- **Remaining**: issue009 awaits user interaction confirmation; issue022 retains
  the separate `lazy-convert` file mutation contract.

## 2026-07-29

- **Action**: Replace the leaf-tag proposal with canonical full-path Tag IDs and implement explicit descendant queries.
- **Files**:
  - Modify `supertag-core-scan.el`
  - Modify `supertag-view-api.el`
  - Add `test/tag-path-test.el`
  - Modify `test/run-tests.sh`
  - Modify nested-tag decision/issue and current phase docs
- **Behavior**:
  - Exact tag queries remain exact by default.
  - Optional descendant queries respect `/` segment boundaries.
  - `:extends` remains schema inheritance; no Store migration, parent entity creation, or raw-path double-write occurs.
- **Verification**: focused ERT 4/4; full ERT 319/319; 10k-node benchmark about 24.86ms per descendant query.

## 2026-06-24

- **Action**: Add tech-refer for issue009 nested tags; update issue and ISSUES index.
- **Files**:
  - Add `.phrase/docs/tech-refer_nested_tags_20260624.md`
  - Modify `.phrase/docs/issue_nested_tags_20260624.md` (Investigation, Fix, Verification sections)
  - Modify `.phrase/docs/ISSUES.md` (update issue009 description)
- **Scope**: Documentation only; no code changes.
- **Behavior/Risk**: Establishes recommended approach (store leaf tag in node `:tags`, keep raw path in `:raw-tag-paths`, opt-in via `supertag-sync-nested-tags`). Decision pending user confirmation before implementation.
