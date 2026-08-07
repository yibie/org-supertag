# issue034 — Stream 双列索引选择后正文不可见

## Environment

- Stream split 布局
- 左侧 companion index + 右侧主 Stream window
- 长正文流，目标节点位于当前主窗口视口之外

## Repro

1. 打开包含多个或长正文节点的 `M-x supertag-view-stream`。
2. 在左侧索引点击靠后的节点。
3. 观察右侧主 Stream window。

## Expected vs Actual

- Expected: 右侧窗口从目标节点标题开始显示，标题与正文开头均可见。
- Actual: 代码只更新 main buffer point；当左侧 index window 被选中时，右侧 window point/start 未同步，切回后目标节点可能落在窗口底部，正文不可见。

## Investigation

index click、`n`/`p` 与 refresh restore 都汇入 `supertag-view-stream--select-node`。该入口在 `with-current-buffer` 中执行 `goto-char`，但没有更新显示 main buffer 的非选中 window。

## Fix

`supertag-view-stream--select-node` 在定位 entity 后同步实际 main window 的 point/start。index click 与 `n`/`p` 继续共用该入口；没有新增滚动状态、helper 或 Runtime 分支。

## Verification

- 失败优先 ERT：真实选中 index window，激活位于 80 行正文后的节点。
- 断言 main window point/start 都等于目标 entity 起点。
- 修复前：main window point 为 1，目标 entity 起点为 1133，回归失败。
- 修复后：Stream 7/7、Stream + Runtime + View 47/47、full ERT 399/399。
- `byte-compile-error-on-warn=t`、checkdoc、2 files `check-parens`、`git diff --check` 与 repo-local `.elc` zero 通过。
- 自动化修复完成；待用户实机确认后关闭 issue。

## Tracking

- Task: task009
- Resolved At/By/Commit: 2026-08-07 / Codex / 本变更提交；issue 待用户确认
