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

1. 在 core transform 提供唯一 range-aware matcher：Sync 传入已有 parse tree，face/SVG/point 对当前行做 secondary Org parse；匹配在 link/code 等对象起点截断，sub/superscript 只对从外部开始的 token 透明。
2. 标题清洗复用同一组绝对位置，保留 link/code 等 Org object 原文。
3. 提供 `supertag-tag-orphaned-ids` 和 `supertag-tag-delete-orphans`：保守扫描 Store、schema、关系及已加载配置；删除前复检并使用事务。
4. 提供 `M-x supertag-cleanup-orphaned-tags`：用户逐项选择并确认；不自动运行、不编辑 Org 文件。
5. 使用 Store collection、saved-query form 与 View config 的公开枚举入口；schema `:fields` 参与保守引用扫描。
6. 每个删除在 `before-operation-hook` 后再次检查；整批删除的全部 `after-operation-hook` 完成后，再使用原始显式候选 ID 扫描引用与残留实体，失败则回滚。
7. transaction 使用非 fail-fast 的 hook runner：所有 rollback invariant handler 都执行完，再重新抛出第一个 hook 错误，保证 schema cache rebuild 不会被更早的异常跳过。
8. face 与 SVG 的 font-lock keyword 使用真正的 range-aware search matcher；matcher 返回前固定精确 group 0，不再依赖 face expression 内修改 match data。

## Verification

- 红灯已复现：旧实现得到 `("aix" "ax/cx" "bodyx" "smartx")`，清理 API 缺失。
- focused extractor/tag-merge ERT 35/35 通过。
- 全量 ERT 337/337 通过；inline tag self-check 通过。
- 真实笔记只读解析得到 `("ai_suggestions" "smart_companion")`，标题保持正确。
- 当前真实 Store 只读预览返回 `goodgoodgood`、`test/tag`、`to`、`too` 4 个候选；没有执行删除。
- 变更文件 `check-parens`、临时目录 byte compilation 与 `git diff --check` 通过；编译仅有既有 warning。
- 独立复审在修复保存查询加载顺序后 APPROVE，无剩余 finding。
- 用户复核的 object boundary、schema default、stale preview、hook TOCTOU 与 rollback cache 均已先红后绿；underscore + nested link 组合复现也已锁定。
- focused extractor/tag-merge ERT 42/42、全量 ERT 344/344 通过。
- 精确复现得到 `tags=("ai_suggestions" "body" "plain" "smart_companion")`，标题保留 `[[id:n][label]]`。
- 真实笔记再次只读解析得到 `("ai_suggestions" "smart_companion")`；真实 Store 只读预览仍为 4 个候选，没有执行删除。
- 1000 Tag 病理基准约 0.36s；逐字符 context 原型的 14.22s 已删除。
- 两路独立复审最终均 APPROVE；最新变更通过 byte compilation、`check-parens` 与 `git diff --check`。
- 用户进一步证明 `a18e6d8` 仍存在 after-hook、跨批次候选、point matcher 与 rollback hook fail-fast 四个缺口；四条精确回归均先红后绿。
- Sync/face/SVG/Smart Key 现共用 core range matcher；Smart Key 精确返回 `outer` 与 `ai_suggestions`，link 内 point 返回 nil。
- after-hook 新引用及“删除 b 时引用已删除 a”均触发整批回滚；较早 rollback hook 报错后 schema cache 仍恢复。
- focused extractor 22/22、Smart Key 12/12、Tag merge 23/23、transaction 20/20；全量 ERT 349/349，inline self-check、1000 行 font-lock smoke（约 0.29s）、byte compilation、package load、paren/diff checks 通过。
- 用户对 `a9257518` 的真实 `font-lock-ensure` 复核证明 face/SVG property 仍覆盖相邻 link；原 self-check 只读取 predicate 修改后的 match string，属于假绿。
- 新增两个真实 property extent ERT：face 与 SVG 均只覆盖 `#outer`，link opening 与 label 不再获得 `supertag-inline-face`/`display`；focused View 16/16、全量 ERT 351/351 通过。

## User Confirmation

- 2026-08-01：用户提供根因诊断并明确不能靠 completion 过滤遮蔽脏数据。
- 2026-08-01：用户复核否决当前清理安全性；在 object boundary、schema field reference、hook 后最终检查和 rollback cache 四项修复完成前，不得运行或推荐清理命令。
- 2026-08-01：上述复核项已在自动化测试与独立复审中关闭；实际用户数据仍等待全量重扫后的人工候选确认。
- 2026-08-01：用户以相同 commit/tree 否决 `a18e6d8`；要求新提交覆盖 after-hook、显式候选整批校验、共享前端 matcher 与 rollback handler 全执行。本轮未运行真实数据库清理。
- 2026-08-02：用户确认 cleanup transaction 四个安全缺陷已关闭，但以真实 font-lock extent 否决 `a9257518`；issue030 继续保持打开，直到 face/SVG 新提交复核通过。
- 全量重扫与实际孤立 Tag 清理结果：Pending。

## Resolution

- Task: `task016`
- Commit: Pending
- Resolved At/By: Pending user confirmation
