# issue009: 嵌套标签（标签/子标签）的自动解析与层级展示

## Summary

支持 `#project/active` 这样的路径式标签写法，以完整路径作为 Tag ID，并在读取侧派生 namespace 层级；路径层级与手动设置的字段继承 `:extends` 保持独立。

## Origin

来自 org-supertag 节点 [[id:70577FDB-05F5-4C7E-A31F-118F2DDB0B08][org-supertag 的标签支持嵌套标签 #idea]]。

## Motivation

- 标签多了容易混乱，嵌套是天然的整理方式
- 当前 `supertag-view-schema` 已支持父子标签和字段继承，但关系需要手动建立
- 路径式写法 (`a/b`) 在许多系统中已是惯例（Bear、Notion 等）
- 如果实现，可大幅降低标签体系的维护成本

## Concerns

1. **标题长度**：Emacs 用户习惯将标签直接放在标题行，`#area/emacs/packages` 写出来会很长，影响可读性
2. **缺乏常驻导航**：org-supertag 没有侧边栏式的标签树导航（类似 Bear Note），即使实现了嵌套解析，用户也无法在编辑 Org 文件时看到标签层级，实用价值打折
3. **显示一致性**：如果需要 "常态化显示" 嵌套关系，落在哪个 View（table / kanban / node view / schema）最合理？

## Current State

- 提取、同步、写回和 completion 将 `a/b/c` 作为一个完整 Tag ID 处理。
- `:extends` 是 schema/字段继承关系，不再承担路径 namespace 关系。
- 单节点/全文件同步、显式后代查询、Schema namespace 树、路径补全、
  View/Table 聚合入口和事务化分支重命名均已实现。
- 自动化、真实 Store 副本和 Schema 视觉验收完成。
- 2026-08-01 修复 completion 的扁平混排：候选按 namespace 直接子级逐层
  展示，`#diary/` 不再泄漏 `ATTACH`、`Apple` 等根级候选。主要 Tag 写入
  入口已复用同一套 namespace reader，自动化验收完成。
- 2026-08-03 修复已有平面 Tag 无法下钻：输入 `#diary` 时即使尚无
  `diary/...` 子 Tag，也会提供只导航的 `diary/` 候选。

## Scope (if implemented)

1. 完整路径保持为 Tag ID，不拆叶子、不双写。
2. 同步为完整路径建立 Tag entity 与 node-tag relation，并与当前节点标签集合对齐。
3. Schema View 从路径派生虚拟 namespace，提供子路径创建和后代聚合查看。
4. completion 提供 namespace 导航候选，但只为真实完整 Tag 落库。
5. View/Table 保留 `include-descendants` scope；后代聚合 Table 只读。
6. 分支重命名原子迁移 exact + descendants；精确删除使用完整 token 边界。
7. 默认精确查询、`:extends` 继承和 Store schema 保持不变。

## Status

- 2026-07-29 用户确认实施完整路径方案。
- task012 仅完成查询基础；2026-07-29 用户指出这还不构成真正的嵌套标签支持。
- task013 已完成数据后端、Schema、View/Table 与初版 completion 自动验收。
- task015 [x] 修复 completion-table 过滤协议；候选按 namespace 直接子级逐层展示；
  行内输入、Add/Change、Capture 与 Tag Field 共用层级 Tag reader。验证方式：
  focused ERT 覆盖 `#diary/` 无关候选、逐层候选、namespace 不落库和共享 reader；
  全量 ERT、byte compilation、check-parens 与真实 Store 候选探测。
- task017 [x] 精确匹配已有平面 Tag 时提供 `/` 子 namespace，允许在没有
  既存后代的情况下继续创建下一层；basic 与 Corfu/orderless 枚举均已锁定。

## Environment

N/A

## Reproduction

1. Store 中存在 `diary`、`diaryx`，但没有 `diary/...`。
2. 在 Org buffer 输入 `#diary` 并触发 CAPF。
3. 修复前只显示 `diary`、`diaryx`，无法从补全进入 `diary/`。
4. 预期同时显示只导航的 `diary/`，选择后可继续输入 leaf。

## Investigation

已进行技术预研，详情见 `tech-refer_nested_tags_20260624.md`。

关键发现：

1. **完整路径已经可用**：现有 Store 和 inline tag 边界允许 `/`，真实 vault 中已有 `Apple/Shortcut/语言` 等标签。
2. **叶子存储不可用**：`emacs/package` 与 `linux/package` 会冲突为同一个 `package`。
3. **namespace 与 inheritance 必须分离**：`/` 是路径包含，`:extends` 是显式字段继承。
4. **性能风险很低**：现有 tag query 本来就是 O(N) scan；显式后代查询只在该 scan 中增加段边界前缀判断。

## Root Cause

completion 只从已存在的完整路径派生 namespace；没有既存后代时，平面 Tag
不会产生 `/` 候选。数据模型支持新子路径，但 UI 没有暴露进入该 namespace 的入口。

## Fix

采用完整路径 + 派生 namespace 实现：

1. 保持 Node `:tags` 与 Tag entity ID 为完整路径。
2. 共享路径段边界、父路径和叶段语义。
3. 扩展 scan query 与 View Data API，以可选参数显式包含路径后代。
4. Schema、completion、View 与 Table 使用同一完整路径和 descendant query。
5. 同步和分支重命名维护 Store 与 Org 文本的一致性。
6. 精确查询默认行为不变；不新增配置、Store 字段、父实体或隐式 `:extends` 写入。
7. 当前输入精确匹配已有 Tag 时，额外派生一个只导航的 `/` 子 namespace 候选。

详细方案见 `.phrase/docs/tech-refer_nested_tags_20260624.md`。

## Verification

task012 自动化验证：
- `#emacs/package` 提取后仍是 `"emacs/package"`。
- 精确查询 `emacs` 不隐式命中 `emacs/package`。
- 显式后代查询 `emacs` 命中 `emacs/package` 和更深层路径，不命中 `emacs2/package`。
- focused ERT 4/4、全量 ERT 319/319 通过。
- 10k-node / 100 次查询基准：精确查询 0.265s，总计约 2.65ms/次；后代查询 2.486s，
  总计约 24.86ms/次，无需新增索引。

task013 验收：
- focused ERT 15/15：覆盖路径边界、单节点同步、原生 tag 策略、Schema namespace/
  inheritance 分离、completion 无写导航、View/Table scope、分支迁移/冲突回滚和精确删除。
- 全量稳定 ERT 330/330；completion 独立 self-check 与 Table ERT 通过。
- 12 个改动文件通过 `check-parens`、byte compilation 和 `git diff --check`。
- 当前真实 vault 的只读副本包含 101 tags/1554 nodes；`coding` exact=0、
  descendants=1，Schema 派生 `coding/` → `日志`；原文件与副本 SHA-1 前后不变。
- Schema View 截图视觉判定 92/100（pass）：namespace 缩进与 `:extends` 箭头/
  inherited fields 可清楚区分。

task015 验收：
- focused ERT 19/19，覆盖逐层 direct-child 候选、`#diary/` 过滤、namespace
  不落库、共享 reader 导航与虚拟 namespace 选择。
- View/Query 相关 ERT 54/54；全量 ERT 335/335；completion 与 inline-tag
  独立 self-check 通过。
- 12 个相关代码/测试文件通过非写入 byte compilation；所有改动文件通过
  `check-parens` 与 `git diff --check`。
- 当前真实 Store 中 `diary` 没有子路径；探测 `diary/` 返回空候选而非根级标签。
- 独立代码审查无阻塞问题，确认未新增依赖、缓存、Store 字段或 namespace entity。

task017 验收：
- 真实 CAPF fixture 使用 `("diary" "diaryx")` 与 buffer `#diary`，basic 和
  Corfu/orderless `action=t` 都返回带 namespace property 的 `diary/`。
- focused ERT 20/20、全量 ERT 352/352；byte compilation、`check-parens`、
  `git diff --check` 通过；编译生成的 `.elc` 已删除。

## User Confirmation

- 2026-07-29：确认采用“完整路径即 Tag ID、读取时推导层级、`:extends` 保持独立”的方案。
- 2026-08-01：实机确认初版 completion 的显示和输入仍是扁平的，task013
  前端验收不通过；进入 task015 修复。
- 2026-08-03：实机确认 `#diary` 仍无法显示可下钻的 nested namespace；进入 task017 修复。
- 端到端实现结果验收：Pending
