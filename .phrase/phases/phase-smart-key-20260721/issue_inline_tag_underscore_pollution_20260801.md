# issue030: 下划线 inline Tag 被改写并留下孤立 Tag entity

## Summary

Org 将 `#ai_suggestions` 中的 `_suggestions` 解析为 subscript object；同步层此前把所有非字符串 Org object 替换为 `x`，导致实际 Tag ID 变成 `aix`。修正解析后，旧污染 Tag entity 仍需由用户显式、安全地清理。

## Environment

- org-supertag `main`
- GNU Emacs 31.0.91 本地验证；CI 覆盖最低支持矩阵
- 真实样本：`/Users/chenyibin/Documents/notes/20260629T105208--diary-2025__diary.org`

## Reproduction

1. 在 headline 正文输入 `#ai_suggestions #smart_companion`。
2. 执行同步或直接调用 `supertag-extractor--tags`。
3. 旧实现返回 `("aix" "smartx")`。

## Expected vs Actual

- Expected：完整保留 `("ai_suggestions" "smart_companion")`，并从标题中移除完整 Tag token。
- Actual：subscript 被哨兵 `x` 替换，标题残留 `_suggestions` 等后缀。

## Investigation

- `supertag--inline-tag-prose-parts-text` 对所有非字符串 Org object 使用同一哨兵。
- 下划线既是合法 Tag 字符，也是 Org subscript 语法；AST parts 因而是有损边界，不能作为 Tag token 的身份来源。
- `+begin_quote` 属于旧解析器遗留；全量重扫会回收 node-tag 关系，但不会推断用户是否还需要一个独立 Tag entity。

## Root Cause

同步层先把 Org AST parts 降维成有损字符串，再运行 token 正则。Tag 的完整身份已经在降维时丢失，后续过滤无法恢复。

## Fix

1. 直接在原始 buffer 区间扫描完整 token，并用 `org-element-context` 只接受 headline/paragraph 直接拥有的匹配。
2. 标题清洗复用同一组绝对位置，保留 link/code 等 Org object 原文。
3. 提供 `supertag-tag-orphaned-ids` 和 `supertag-tag-delete-orphans`：保守扫描 Store、schema、关系及已加载配置；删除前复检并使用事务。
4. 提供 `M-x supertag-cleanup-orphaned-tags`：用户逐项选择并确认；不自动运行、不编辑 Org 文件。

## Verification

- 红灯已复现：旧实现得到 `("aix" "ax/cx" "bodyx" "smartx")`，清理 API 缺失。
- focused extractor/tag-merge ERT 35/35 通过。
- 全量 ERT 337/337 通过；inline tag self-check 通过。
- 真实笔记只读解析得到 `("ai_suggestions" "smart_companion")`，标题保持正确。
- 当前真实 Store 只读预览返回 `goodgoodgood`、`test/tag`、`to`、`too` 4 个候选；没有执行删除。
- 变更文件 `check-parens`、临时目录 byte compilation 与 `git diff --check` 通过；编译仅有既有 warning。
- 独立复审在修复保存查询加载顺序后 APPROVE，无剩余 finding。

## User Confirmation

- 2026-08-01：用户提供根因诊断并明确不能靠 completion 过滤遮蔽脏数据。
- 全量重扫与实际孤立 Tag 清理结果：Pending。

## Resolution

- Task: `task016`
- Commit: Pending
- Resolved At/By: Pending user confirmation
