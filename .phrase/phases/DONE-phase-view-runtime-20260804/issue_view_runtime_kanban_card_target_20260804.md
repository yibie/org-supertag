# issue031 — Kanban 并排卡片操作命中相邻列节点

## Environment

- Date: 2026-08-04
- Emacs: 31.0.91, graphical `emacs -Q`
- Base commit: `3e652a70e16b027af8e2d3291c1d46550827acf2`
- Phase: `DONE-phase-view-runtime-20260804`

## Reproduction

1. 在两列 Kanban 中让左右两列同一物理行各有一张卡片。
2. 将 point 放在左列卡片正文。
3. 执行 `supertag-view-kanban-move-card-right`。

## Expected vs Actual

- Expected: 操作左列 point 所在卡片。
- Actual: 旧实现向后搜索最近的 `┌`；由于两列顶边框位于同一行，它可能先命中右列边框，进而把右列节点当作当前卡片。

## Investigation

Kanban renderer 已在每一行卡片文本上保存稳定的 `node-id`、`supertag-entity-id` 和 `group-value`。`supertag-view-kanban--get-card-info` 仍重新解析显示字符，导致逻辑行与二维布局不一致。

## Fix

删除边框反向搜索，直接读取 point 上的 `node-id` 和 `group-value` 文本属性。没有新增状态或兼容分支。

## Verification

- Red: 新公共命令回归得到 `field-set-args=nil`，证明左列命令没有操作左列节点。
- Green: `./test/run-tests.sh view-kanban`，4/4 通过。
- Full: `./test/run-tests.sh all`，382/382 通过。
- Graphical: 独立 `emacs -Q`、`display-graphic-p=t`，Kanban 移动/恢复/selection/cleanup 场景通过；完整 View Runtime smoke 9/9 通过。

## User Confirmation

用户于 2026-08-05 批准实机验收，issue 关闭。

## Related

- Task: `task015`
- Commit: 本次 View Runtime / Widget Renderer 结项提交
