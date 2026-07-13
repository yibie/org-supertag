# issue014 [ ] snapshot guard 延迟删除在重启或 worker 异常后失去重试

## Environment

- Branch: PR #181 `fix/cl-return-from-blocks`
- Baseline: `8eb837a`
- Emacs batch ERT

## Repro / Actual

在 partial snapshot 下，同一 Org 文件删除一个标题并修改另一个标题。single-file
processor 延迟删除并保留旧 hash，但 async processor 随后推进 state mtime。Emacs
重启会丢失内存 `deferred-files`，mtime 又让调度器判断文件未修改，删除永久不再重试。

另一路径中，complete snapshot 会先把 deferred entry 标成 `:queued`。async worker
在调用 processor 前已从队列弹出文件；若 processor 报错，entry 不会恢复为
`:pending`，后续 complete scan 也会永久跳过它。

## Expected

只要 destructive cleanup 尚未执行，持久化 sync-state 就必须继续让该文件进入后续
扫描；同一轮的非破坏性更新或一次 worker 异常都不能吞掉删除重试。

## Root Cause

`supertag-sync--process-single-file` 与 `supertag-sync--async-processor` 同时拥有
sync-state 推进权。前者正确保留旧 mtime，后者的重复 `supertag-sync-update-state`
又把它覆盖。

deferred scheduler 还复制了一套 `:pending`/`:queued` 状态，却没有在 worker 异常时
执行确认或回滚；该状态与真正的 async queue 分叉。

## Fix / Verification

- 删除 async processor 的重复 state 更新；single-file processor 成为唯一所有者。
- 删除 deferred scheduler 的 `:queued` 门控，复用 async queue 已有的去重行为；
  worker 异常后的下次 complete scan 会重新入队。
- 删除 legacy/guarded cleanup 中依据当前状态机永远不可达的 guard 分支。
- 删除不能正确表达 `cl-lib` block 语义的手写 walker，保留真实 byte-compiled
  行为回归。
- 把修复后的 aggregate 测试加入默认 `all`。
- 验证：两个新增回归均先失败后通过；`./test/run-tests.sh all` 116/116；aggregate 3/3；
  `bash -n test/run-tests.sh` 与 touched-file byte-compile 通过。

## User Confirmation

- [ ] 等待用户在合入或真实环境验证后确认。

## Resolution Status

- Implementation completed: 2026-07-14
- Implemented By: Codex
- Commit: `4ea00e7`
- Follow-up: worker error retry fix in PR #181 history.
- Issue remains open until user confirmation.

## Related

- PR: #181
- Task: task015
