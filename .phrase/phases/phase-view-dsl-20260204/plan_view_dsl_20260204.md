# plan_view_dsl_20260204

## Milestones

1. 建立 phase 文档（spec/plan/task/change），登记 CHANGE 索引。
2. 设计 DSL v2：组件模型、嵌套结构、函数绑定语义、context 边界。
3. 实现 DSL v2：嵌套渲染、容器组件、函数绑定、错误提示。
4. 提供统一刷新命令：可重复渲染当前 view + context。
5. 更新文档与示例：确保 20 行示例可运行并可手动刷新。

## Scope

- 核心框架：`supertag-view-framework.el`
- 示例/演示：`supertag-view-examples.el` / `supertag-view-examples-simple.el`（如需）
- 文档：`doc/VIEW_FRAMEWORK_DEV_GUIDE.md`
- 同步一致性：修复 `:olp` 变更未触发更新，保证 add-reference 候选路径刷新
- 测试（如存在）：`test/view-framework-test.el`（或最小手动验证清单）
- Phase 文档：本目录 `spec_*` / `plan_*` / `task_*` / `change_*`

## Priorities

- P0：DSL 嵌套 + 函数绑定可用，能表达 20 行嵌套视图。
- P1：统一刷新命令可用，错误提示清晰。
- P2：文档与示例更新到位，形成可复制路径。

## Risks & Dependencies

- DSL 复杂度膨胀：需限制能力边界（只做嵌套 + 绑定 + 手动刷新）。
- Context 边界不清：需明确可访问的字段与数据来源（tag/nodes/virtual columns/global fields）。
- 兼容性风险：尽量不破坏既有视图；如需破坏需文档说明。
- Emacs 渲染性能：嵌套结构可能带来性能压力，需保持最小实现。

## Rollback

- 回退 DSL v2 相关改动，恢复现有 view framework 行为。
- 文档回退到 v1 说明，并保留本 phase 文档作为历史记录。
