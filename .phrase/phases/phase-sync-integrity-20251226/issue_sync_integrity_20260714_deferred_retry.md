# issue014 [ ] snapshot guard 下混合删除/更新在重启后失去重试

## Environment

- Branch: PR #181 `fix/cl-return-from-blocks`
- Baseline: `8eb837a`
- Emacs batch ERT

## Repro / Actual

在 partial snapshot 下，同一 Org 文件删除一个标题并修改另一个标题。single-file
processor 延迟删除并保留旧 hash，但 async processor 随后推进 state mtime。Emacs
重启会丢失内存 `deferred-files`，mtime 又让调度器判断文件未修改，删除永久不再重试。

## Expected

只要 destructive cleanup 尚未执行，持久化 sync-state 就必须继续让该文件进入后续
扫描；同一轮的非破坏性更新不能吞掉删除重试。

## Root Cause

`supertag-sync--process-single-file` 与 `supertag-sync--async-processor` 同时拥有
sync-state 推进权。前者正确保留旧 mtime，后者的重复 `supertag-sync-update-state`
又把它覆盖。

## Fix / Verification

- 删除 async processor 的重复 state 更新；single-file processor 成为唯一所有者。
- 删除 legacy/guarded cleanup 中依据当前状态机永远不可达的 guard 分支。
- 删除不能正确表达 `cl-lib` block 语义的手写 walker，保留真实 byte-compiled
  行为回归。
- 把修复后的 aggregate 测试加入默认 `all`。
- 验证：新增回归先失败后通过；`./test/run-tests.sh all` 115/115；aggregate 3/3；
  `bash -n test/run-tests.sh` 与 touched-file byte-compile 通过。

## User Confirmation

- [ ] 等待用户在合入或真实环境验证后确认。

## Resolution Status

- Implementation completed: 2026-07-14
- Implemented By: Codex
- Commit: `4ea00e7`
- Issue remains open until user confirmation.

## Related

- PR: #181
- Task: task015
