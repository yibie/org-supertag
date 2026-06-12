# issue006 automation async worker 报错 changed-props 未绑定

## Summary
在 async sync worker 触发 automation 时，`*Messages*` 报错 `Symbol’s value as variable is void: changed-props`，导致规则执行中断。

## Environment
- OS: macOS
- Emacs: 未确认
- Org-Supertag: dev（phase-automation-alignment）

## Repro
1. 启动自动同步/异步队列（例如 `supertag-sync-start-auto-sync`）。
2. 触发一次节点更新（无 tag/property 变化，仅内容或 metadata 更新）。
3. 观察 `*Messages*`：出现 `Error in supertag async worker: Symbol’s value as variable is void: changed-props`。

## Expected vs Actual
- Expected: automation 正常执行或跳过，无异常。
- Actual: async worker 报错，automation 中断。

## Investigation
`supertag-automation-sync--process-node-change` 的 `let*` 作用域在 property-change 分支结束处提前闭合，导致后续 “non-property changes fallback” 块引用的 `changed-props`/`added-tags`/`removed-tags` 变量未绑定。在无 tag/property 变化时触发该分支，直接抛 `void-variable`。

## Fix
- 调整 `supertag-automation-sync--process-node-change` 的括号结构，确保 node-change fallback 仍处于 `let*` 作用域内。

## Verification
1. 重跑复现步骤，不再出现 `changed-props` 报错。
2. 规则仍能执行或被正确跳过。
3. `supertag-automation-verbose` 开启时无异常日志。

## User Confirmation
- [x] Code review confirmed: let* scope fixed (task007)

## Resolved At/By/Commit
- 2026-06-10 / code review verification / implemented in phase-automation-alignment task007
