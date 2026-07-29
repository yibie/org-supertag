# issue028 [ ] Emacs 正常退出缺少 Git 同步保护

## Environment

- `supertag-git-sync-mode` 已启用
- commit debounce 默认 30 秒，Git 子进程异步运行
- 正常退出通过 `save-buffers-kill-emacs` 执行退出查询与 kill hooks

## Repro / Actual

保存受管 Org/数据库文件后，在 debounce 到期或异步 push 完成前退出。退出 hook
会保存 Store，但不会 flush Git mode 的 timer，也不会等待/查询 Git 状态。已提交
commit 可在下次启动补推；尚在 working tree 的改动不会由 mode-enable 的 ahead
检查提交，必须等下一次保存事件。

## Expected

提供立即同步命令；正常退出发现未完成同步时取消本次退出并启动同步，同时允许用户
明确选择保留本地状态退出。人工演练进一步确认：没有本地笔记改动时，不因单独的
debounce timer 或 upstream behind 启动退出同步；确需同步时，成功后自动走正常
退出。同步失败、冲突或同步期间出现新改动时保持 Emacs 打开。

## Verification

- 真实临时 Git remote：立即同步把 working tree 改动 commit 并 push。
- clean 状态允许退出，不显示误报。
- 未同步时选择同步会取消退出并完成 Git 链；选择 local-only 时允许退出且不丢文件。
- mode enable/disable 对称注册和移除退出 query。
- Store 尚未落盘或 Git 进程正在运行时，普通退出直接取消，不提供不安全的
  local-only 选择。
- Git 37/37、默认全量 315/315、临时目录 byte-compile 与 `git diff --check`
  通过。
- 2026-07-29 修订：behind-only 且无本地改动时直接退出；选择同步后成功自动调用
  正常退出；失败/新改动保持 Emacs；mode disable 清理 exit waiter。Git 37/37、
  默认全量 330/330、临时目录 byte-compile 通过。

关联任务：task006、task007。

- User Confirmation: 2026-07-29，用户确认自动同步正常，并要求无本地改动直接
  退出、退出同步成功后自动关闭 Emacs；最终行为待再次演练。
- Implementation Commit: `015db5a`
- Refinement Commit: `8388d55`
