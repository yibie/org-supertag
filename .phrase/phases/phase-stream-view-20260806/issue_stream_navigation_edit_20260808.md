# issue037 — Stream 自然导航与可取消编辑

## Summary

单列 Stream 在 `n`/`p` 导航时把光标行强制放到窗口顶部；按 `e` 打开的
indirect buffer 会继承折叠状态，并且只有 `C-c C-c`，没有类似 Org Capture 的
`C-c C-k` 取消语义。

## Environment

- org-supertag main after task012
- Emacs 31.0.91 / Org mode
- 单列 title-only Stream

## Reproduction

1. 打开至少两个节点的 Stream，在当前窗口按 `n`。
2. 实际：window start 被改到目标标题位置，光标行始终置顶。
3. 折叠一个源节点后在 Stream 按 `e`。
4. 实际：正文仍折叠；`C-c C-k` 调用 Org 默认命令，不能取消当前编辑。

## Expected

- 已可见标题间导航保持窗口自然位置，只有越出可见区时由 Emacs 原生滚动显示。
- `e` 显示展开的标题与正文。
- `C-c C-c` 确认并同步；`C-c C-k` 恢复进入编辑前的文本并返回。

## Investigation

1. `supertag-view-stream--select-node` 在设置 window point 后无条件把 window start
   设置为同一位置。
2. indirect buffer 继承 Org fold 状态，入口没有调用展开 API。
3. edit minor mode 只覆盖 `C-c C-c`；编辑直接共享 base buffer 文本，没有取消快照。

## Root Cause

selection 同时写 point 和 window start，把本应由 Emacs 管理的滚动派生状态当成业务
状态；edit boundary 只有“完成”路径，没有保存进入编辑前的最小可恢复数据。

## Fix

- selection 只设置 window point，不再写 window start。
- narrow 后使用 Org fold API 展开可见内容。
- edit buffer 保存进入时的 narrow 文本和 source modified 状态。
- `C-c C-k` 恢复快照、不触发 Store 同步，并与确认路径共用关闭及窗口恢复。

## Verification

- task013 regression-first Stream ERT 复现 window start 1→7、正文 invisible 与
  `C-c C-k` 错误绑定。
- 修复后 Stream ERT 9/9、相关 View 49/49、full ERT 401/401。
- strict byte compile、checkdoc、check-parens、`git diff --check` 与 repo-local
  `.elc` zero 通过。

## User Confirmation

Pending：需要在日常 Emacs 中确认自然滚动、展开正文、确认和取消四项交互。

## Related

- task013
- `supertag-view-stream.el`
- `test/test-view-stream.el`
