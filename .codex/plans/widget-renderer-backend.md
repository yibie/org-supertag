# Widget Renderer Backend — Work Plan

## TL;DR (For humans)

- **What you'll get**: A real Widget DSL renderer used by the three built-in Dashboards and a Stream-shaped interactive fixture, with stable selection, native buttons/fields, and layout-safe rendering.
- **Approach**: Keep View Runtime renderer-agnostic; deepen only the DSL Adapter with immutable plist render specs, stable keys, deferred native primitive materialization, and full refresh.
- **What it will NOT do**: No `widget-extra`, VUI, hooks/context/reconciliation, or Search/Table/Kanban renderer migration.
- **Decisions I made**: Full redraw remains the default; built-in `button.el` handles links/actions, `widget.el` handles editable fields only; the graphical hands-on gate was approved on 2026-08-05.

## Scope

- IN: `supertag-view-framework.el`, three Dashboard adapters, Runtime/DSL ERT, focused benchmark/smoke, developer and phase docs.
- OUT: Search/Table/Kanban renderer rewrites, Schema product redesign, Stream product UI, third-party dependencies, component-local state framework.

## Task Dependency Graph

| Task | Depends On | Blocks | Reason |
| --- | --- | --- | --- |
| 1. Phase and baseline | None | 2–7 | Locks scope and current behavior |
| 2. Stable keyed selection | 1 | 5–6 | Logical identity is required before refreshable interaction |
| 3. Native button/field leaves | 1 | 4–6 | Supplies the interactive primitives |
| 4. Two-stage layout commit | 3 | 5–6 | Prevents temporary-buffer Widget loss |
| 5. Dashboard migration | 2–4 | 7 | Proves the DSL deletes real renderer duplication |
| 6. Stream-shaped fixture and benchmark | 2–4 | 7 | Proves interaction, refresh, and scale |
| 7. Full verification and docs | 5–6 | None | Release evidence and hands-on gate |

## Execution Waves

```text
Wave 1: Task 1
Wave 2: Task 2, then Task 3
Wave 3: Task 4
Wave 4: Task 5 and Task 6
Wave 5: Task 7
```

Execution is sequential in this session because the same Framework/test files are shared and native subagents are unavailable by session policy.

## Tasks

### Task 1: Phase and baseline

- **What**: Create PR/FAQ, spec, ADR, plan, task and change records; run existing View focused ERT.
- **Agent**: `executor` — solo implementation lane.
- **Depends**: None.
- **Acceptance**: Existing focused suite is green and documents link to the approved tech reference.
- **QA**: `./test/run-tests.sh view-runtime view`; `git diff --check`.
- **Commit**: Deferred until graphical hands-on approval.

### Task 2: Stable keyed selection

- **What**: Add optional `:key` range metadata and generic DSL capture/restore callbacks; keep Adapter selection opaque to Runtime.
- **Agent**: `executor`.
- **Depends**: Task 1.
- **Acceptance**: A refresh that changes preceding text restores point to the same keyed logical node and offset; missing key falls back safely.
- **QA**: Focused ERT for refresh and deletion fallback.
- **Commit**: Deferred until graphical hands-on approval.

### Task 3: Native button and editable-field leaves

- **What**: Use `button.el` for `:button`/`:link`, `widget.el` for `:editable-field`, compose keymaps without replacing View mode bindings, and call `widget-setup` once per completed render.
- **Agent**: `executor`.
- **Depends**: Task 1.
- **Acceptance**: Button activation invokes its callback; field commit returns the new value; refresh remains safe; text outside fields stays protected.
- **QA**: Focused ERT plus graphical `emacs -Q` keyboard smoke.
- **Commit**: Deferred until graphical hands-on approval.

### Task 4: Two-stage layout commit

- **What**: Render interactive leaves as measurable placeholders, complete columns/cards in text, then materialize native buttons/fields in the final buffer; remove temp-buffer native Widget creation.
- **Agent**: `executor`.
- **Depends**: Task 3.
- **Acceptance**: Button and field remain interactive inside columns/cards; width and border alignment remain correct; no dead Widget state survives refresh.
- **QA**: Focused ERT with interactive children in layout and repeated refresh.
- **Commit**: Deferred until graphical hands-on approval.

### Task 5: Dashboard migration

- **What**: Express Progress, Effort and Priority content as dynamic Widget render specs, register through the DSL backend, and delete their hand-written buffer renderer functions.
- **Agent**: `executor`.
- **Depends**: Tasks 2–4.
- **Acceptance**: Exact buffer names and required content remain; all three have Runtime instances; migration is net code deletion across Dashboard render paths.
- **QA**: Existing Dashboard Runtime ERT plus focused content regressions.
- **Commit**: Deferred until graphical hands-on approval.

### Task 6: Stream-shaped fixture and benchmark

- **What**: Replace the plain Stream fixture with keyed node text, tag link and edit button; measure 100/500/1000-node initial render and refresh without introducing a product command.
- **Agent**: `executor`.
- **Depends**: Tasks 2–4.
- **Acceptance**: Interaction and keyed restoration work with no Runtime special case; measurements and marker/overlay counts are recorded.
- **QA**: Runtime ERT, batch benchmark, graphical `emacs -Q` smoke.
- **Commit**: Deferred until graphical hands-on approval.

### Task 7: Verification and documentation

- **What**: Update Developer Guide, CHANGELOG and phase evidence; run static and full gates; leave a hands-on approval checklist.
- **Agent**: `executor`.
- **Depends**: Tasks 5–6.
- **Acceptance**: Focused/full ERT, byte compile, checkdoc, `git diff --check`, `.elc` zero and graphical smoke pass; no third-party dependency or legacy renderer remains in migrated Dashboards.
- **QA**: `./test/run-tests.sh all` plus graphical `Emacs.app -Q` smoke.
- **Commit**: Deferred until user explicitly approves hands-on results.

## Success Criteria

- [x] Stable keys preserve logical selection through full refresh.
- [x] Native buttons/fields remain interactive inside supported layouts.
- [x] Three Dashboards use the same DSL renderer backend.
- [x] Stream fixture needs no Runtime special case.
- [x] Dashboard renderer migration is net code deletion.
- [x] Automated quality and graphical smoke gates pass; user approved the hands-on result on 2026-08-05.
- [x] No new package dependency is introduced.
