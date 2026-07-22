# Spec — Tag Merge

## Summary

Add a destructive-but-recoverable Schema View operation that consolidates two or more tags into one destination without leaving the database and Org files out of sync.

## Goals

- Merge into a new or existing tag.
- Preserve all fields on an existing destination and let users select source fields to import.
- Resolve schema/value conflicts before mutation, including multi-value selection for fields already defined as multi-valued.
- Migrate nodes, field values, relations, inheritance, structured automation/view/query references, and affected Org text.
- Roll back store and file changes on failure.

## Non-goals

- Tag aliases or deprecated source tags.
- Changing field types/cardinality during merge.
- Blind replacement inside prose, source blocks, comments, or other unparsed free text.
- A new general migration framework or external dependency.

## User flow

1. Mark at least two tag rows in `M-x supertag-view-schema`.
2. Run `m M` (`Merge marked tags`).
3. Choose an existing destination or enter a new tag name.
4. Choose source fields; empty means all.
5. Resolve definition/value conflicts.
6. Review the preview and confirm.
7. Schema View refreshes on success. Cancellation or failure leaves data unchanged.

## Edge cases

- Destination is one of the marked tags.
- A node already has the destination tag.
- Multiple sources provide the same field with equal or different values.
- Sources have children or appear in the destination ancestry.
- Relations collapse to duplicates after endpoint replacement.
- An affected Org buffer has unsaved edits.
- A file disappears or becomes unwritable between preview and execution.
- A valid level-0 file node has no `#+TITLE`; merge preserves `:title nil` and updates its tags normally.

## Acceptance criteria

See `.omx/specs/deep-interview-tag-merge-20260722T023252Z.md`.
