# issue007 [x] 父标题变更后 add-reference 候选仍显示旧 `:olp`

## Summary

父标题改名后，子节点 `:olp` 未刷新，导致 add-reference 候选仍展示旧路径。

## Environment

- Repo: org-supertag
- Phase: `phase-view-dsl-20260204`
- Emacs: （待补充）
- OS: （待补充）
- org-supertag commit: （待补充）

## Repro

1. 在 note.org 中修改某父标题（影响子节点路径）。
2. 运行 `M-x supertag-sync-force-resync-current-file`。
3. 进入 `M-x supertag-add-reference` 的候选列表。
4. 观察：候选项仍显示旧的父路径。

## Expected vs Actual

- Expected
  - 候选项路径与当前父标题一致。
- Actual
  - 仍展示旧的 `:olp` 路径。

## Investigation

- `:olp` 仅在解析阶段计算。
- `supertag-node-hash` 未包含 `:olp`，导致父标题变更不触发节点更新。
- `supertag--merge-node-properties` 未把 `:olp` 视作标准字段，旧值会覆盖新值。

## Fix

- 在 `supertag-node-hash` 纳入 `:olp`。
- 在 `supertag--merge-node-properties` 的标准字段中加入 `:olp`。
- `supertag-node-hash` 接入 `supertag-sync-hash-props`（默认包含 `:olp`）。

## Verification

- [x] 重命名父标题 → `supertag-sync-force-resync-current-file` → add-reference 候选路径更新。
- [x] 旧候选缓存清理后不再出现旧路径。

## User Confirmation

- [x] 用户确认候选路径已随父标题更新

## Resolved

- Resolved At: 2026-02-05
- Resolved By: codex
- Commit: 587c4de

## Related

- Related phase: `phase-view-dsl-20260204`
- Related task: `task018`
