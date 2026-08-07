# issue035 — Stream 切换 tag 后旧索引窗口残留

## Environment

- Nova Emacs 图形 frame
- Stream split 布局
- 已打开 Node View side window

## Repro

1. 执行 `M-x supertag-view-stream`，选择 `diary`。
2. 再次执行命令，选择 `fun`。
3. 观察窗口布局。

## Expected vs Actual

- Expected: `diary` 与 `fun` main buffer 都保留；frame 只显示当前 `fun` 的 index + main。
- Actual: `diary` index window 继续占用一列，`fun` 又创建新的 index window，重复切换会不断增加索引列。

## Investigation

main 与 index buffer name 已按 tag 隔离。问题位于 presentation lifecycle：公共命令把新 main 显示到旧 main window 后，直接为新 main 执行 split，却没有撤下上一 main 的 companion index window。

## Fix

公共命令在布局新 Stream 前，使用既有 `supertag-view-stream--remove-index` 撤下上一 context 的 companion；上一 main buffer 与 Runtime instance 继续存活，切回时按需重建 index。

## Verification

- 失败优先 ERT 连续打开 `diary`、`work`、`diary`。
- 每次切换断言旧 index window 不可见、当前 index window 可见、两个 main buffer 都存活且内容隔离。
- 修复前：`work` 打开后 `diary` index window 仍为 live window，且测试 frame 因剩余宽度不足把 `work` 降级为 plain。
- 修复后：Stream 8/8、Stream + Runtime + View 48/48、full ERT 400/400。
- `byte-compile-error-on-warn=t`、checkdoc、2 files `check-parens`、`git diff --check` 与 repo-local `.elc` zero 通过。
- 自动化修复完成；待用户按截图场景实机确认后关闭 issue。

## Tracking

- Task: task011
- Resolved At/By/Commit: 2026-08-07 / Codex / 本变更提交；issue 待用户确认
