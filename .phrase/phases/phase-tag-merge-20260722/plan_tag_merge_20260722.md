# Plan — Tag Merge

## Milestones

1. Add a merge-plan builder that validates participants, fields, inheritance and value conflicts.
2. Add one transactional executor using existing Store/Ops seams and existing Org tag rewriting.
3. Add the smallest Schema View adapter on the existing marked-items mechanism.
4. Lock legacy/global field behavior and rollback with ERT tests; update help and change records.
5. Keep the shared node validator aligned with the file-node contract so untitled level-0 nodes remain editable.

## Constraints

- No new dependency.
- Existing destination fields are never deleted by merge.
- No writes occur while conflicts remain unresolved.
- Existing rename/delete commands remain compatible.

## Risks and mitigation

- Partial multi-file writes: snapshot affected files and restore them on error.
- Duplicate relations: rebuild identities after endpoint substitution and deduplicate by `(from to type)`.
- Stale indexes: rebuild relation, schema and automation indexes after commit/rollback.
- Hidden free-text references: report warnings instead of unsafe replacement.

## Rollback

The store uses `supertag-with-transaction`; Org files use temporary snapshots. An exception restores both before reaching the user.
