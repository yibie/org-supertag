# issue032 — Stream 编辑后创建时间与顺序漂移

## Environment

- Emacs 31.0.91，独立图形 `emacs -Q`
- 隔离内存 Store 与 disposable Org 文件
- Stream split、SVG、narrow、Node View、teardown 综合 smoke

## Repro

1. Store 中已有带固定 `:created-at` 的 source-backed node。
2. 在 Stream 当前节点按 `e`，修改正文并按 `C-c C-c`。
3. `supertag-node-sync-at-point` 将解析结果交给 `supertag-db-add-with-hash`。

## Expected vs Actual

- Expected: source 正文和 tags 更新，Store 的原 `:created-at` 保持不变，Stream 顺序稳定。
- Actual: 解析结果不携带 `:created-at`，`supertag-node-create` 为同 ID 节点写入当前时间，节点移到 Stream 末尾。

## Investigation

完整文件同步会先调用 `supertag--merge-node-properties`，因此保留 Store-only 创建时间；point sync 和 file-node upsert 直接走共享 `supertag-db-add-with-hash`，缺少同一不变量。窗口、renderer 与排序器均按收到的数据正确工作。

## Fix

`supertag-db-add-with-hash` 在覆盖已有 ID 时保留已有 `:created-at`。新节点仍由 `supertag-node-create` 生成创建时间；没有新增包装层或 Stream 特判。

## Verification

- `test/test-view-stream.el` 的真实 narrow edit 路径先红：实际时间变为当前时间。
- 修复后 `view-stream sync-worker node` 25/25。
- 待图形 smoke 重跑与用户实机确认后关闭 issue。

## Tracking

- Task: task007
- Resolved At/By/Commit: 待用户确认与提交
