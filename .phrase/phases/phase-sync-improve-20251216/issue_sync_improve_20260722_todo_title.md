# issue020 [x] 后台同步把 Org TODO 关键字误存入 node title

## Environment

- Emacs: 31.0.90
- Org: 9.8.6
- Supertag sync: auto-sync / full-rescan

## Repro / Actual

Org 源文本：

```org
**** DOING 【成本明细表】导出后表格与输入内容不一致 #issue
```

Org 在真实 buffer 中可正确识别 `:todo "DOING"`，但同步后 DB 中为
`:todo nil`，`:title` / `:raw-value` / `:olp` 均包含 `DOING`。

## Expected

- `:todo` 为 `"DOING"`；
- node title 为 `【成本明细表】导出后表格与输入内容不一致`；
- full rescan 能修复已存入 DB 的错误字段。

## Root Cause

`supertag-sync--process-single-file` 在 `with-temp-buffer` 中读文件后直接调用
`supertag--parse-org-nodes-from-current-buffer`，但该 buffer 仍是 `fundamental-mode`。
因为 `org-todo-keywords-1` 没有初始化，Org parser 把 `DOING` 当作普通标题文本。

`supertag--parse-org-nodes` 包装层原本会进入 `org-mode`，但 auto-sync 热路径绕过了
该包装层。此外 full rescan 仍会根据 content hash 跳过未变文件，因此无法修复已有污染数据。

## Fix / Verification

- [x] 公共 current-buffer parser 自行进入无 hook 的 `org-mode` 并禁用临时 buffer cache。
- [x] full rescan 强制重解析 content hash 未变文件。
- [x] 回归测试在修复前稳定失败，修复后定向 ERT 23/23 通过。
- [x] 完整 ERT 297/297、diff check、check-parens 与 byte compile 通过。

## User Confirmation

- [x] 用户已在真实 vault 验证：现有 node title 中的 `DOING` 被移除，TODO 状态恢复。

## Resolution Status

- Implementation completed: 2026-07-22
- Implemented By: Codex
- Resolved At: 2026-07-22
- Resolved By: user verification
- Commit: task020 Lore commit

## Related

- Task: task020
- Parser: `supertag--parse-org-nodes-from-current-buffer`
- Recovery command: `supertag-sync-full-rescan`
