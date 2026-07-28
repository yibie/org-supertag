# issue009: 嵌套标签（标签/子标签）的自动解析与层级展示

## Summary

支持 `#project/active` 这样的路径式标签写法，自动建立父标签（`project`）和子标签（`active`）的层级关系，替代当前手动使用 `supertag-set-tag-parent` 的方式。

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

- 提取、同步、写回和 completion 已将 `a/b/c` 作为一个完整 Tag ID 处理。
- `:extends` 是 schema/字段继承关系，不再承担路径 namespace 关系。
- 缺失：查询层尚不能显式请求“当前 Tag + 路径后代”。

## Scope (if implemented)

1. 完整路径保持为 Tag ID，不拆叶子、不双写。
2. 查询 API：`supertag-find-tag-descendants`。
3. scan query 与 View Data API 显式支持 `include-descendants`。
4. 默认精确查询、`:extends` 继承和 Store schema 保持不变。

## Status

- 2026-07-29 用户确认实施完整路径方案。
- 实现与自动化验证完成；待用户在真实 vault 验收。

## Environment

N/A

## Reproduction

N/A (feature request)

## Investigation

已进行技术预研，详情见 `tech-refer_nested_tags_20260624.md`。

关键发现：

1. **完整路径已经可用**：现有 Store 和 inline tag 边界允许 `/`，真实 vault 中已有 `Apple/Shortcut/语言` 等标签。
2. **叶子存储不可用**：`emacs/package` 与 `linux/package` 会冲突为同一个 `package`。
3. **namespace 与 inheritance 必须分离**：`/` 是路径包含，`:extends` 是显式字段继承。
4. **性能风险很低**：现有 tag query 本来就是 O(N) scan；显式后代查询只在该 scan 中增加段边界前缀判断。

## Root Cause

N/A (feature request)

## Fix

采用最小读取侧实现：

1. 保持 Node `:tags` 与 Tag entity ID 为完整路径。
2. 新增 `supertag-find-tag-descendants`。
3. 扩展 scan query 与 View Data API，以可选参数显式包含路径后代。
4. 精确查询默认行为不变；不新增配置、Store 字段、父实体或 `:extends` 写入。

详细方案见 `.phrase/docs/tech-refer_nested_tags_20260624.md`。

## Verification

自动化验证：
- `#emacs/package` 提取后仍是 `"emacs/package"`。
- 精确查询 `emacs` 不隐式命中 `emacs/package`。
- 显式后代查询 `emacs` 命中 `emacs/package` 和更深层路径，不命中 `emacs2/package`。
- focused ERT 4/4、全量 ERT 319/319 通过。
- 10k-node / 100 次查询基准：精确查询 0.265s，总计约 2.65ms/次；后代查询 2.486s，
  总计约 24.86ms/次，无需新增索引。

真实 vault 验收：
- 在包含 `emacs/package` 的数据上调用
  `(supertag-view-api-nodes-by-tag "emacs" t)`，确认包含期望节点且精确查询结果不变。

## User Confirmation

- 2026-07-29：确认采用“完整路径即 Tag ID、读取时推导层级、`:extends` 保持独立”的方案。
- 实现结果验收：Pending
