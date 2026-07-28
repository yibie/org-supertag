# change_nested_tags_20260624

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
