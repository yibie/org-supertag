# ADR：统一 View Runtime 采用现有 Framework + Adapter

- Status: Accepted
- Date: 2026-08-04
- Related: `pr_faq_view_runtime_20260804.md`

## Context

Search、Table、Kanban、Node 分别拥有 buffer 创建、display、refresh、subscription 和 cleanup。现有 `supertag-view-framework.el` 只有 registry/render/DSL 的部分能力，`supertag-view-api.el` 则已经提供正确的数据读取边界。继续独立实现会让 Stream 成为第五套生命周期。

## Decision

采用方案 C：统一 Runtime + 独立 Adapter。

- 深化现有 `supertag-view-framework.el`，不先创建平行 runtime 模块或 registry。
- Runtime 只拥有 View Instance、open、refresh、display、subscription handles、cleanup 与 selection transfer。
- Adapter 保留 state、renderer、mode/keymap 与 ops 调用。
- Presentation 使用 Emacs 原生 `display-buffer` action。
- 数据读取继续走 `supertag-view-api.el`；写入继续走既有 ops。
- 通过旧 public command wrapper 逐个迁移和回滚。

## Alternatives

### 保持四套独立 view

拒绝：不能解决重复生命周期、事件不一致和 subscription 泄漏，Stream 成本继续累积。

### 全部改写为 Widget DSL

拒绝：Renderer 差异不是当前根因；会把 Table/Node/Kanban 特有交互推入公共 DSL。

### 新建独立 `supertag-view-runtime.el`

当前拒绝：会与已有 framework registry/refresh 形成双核心。只有出现实际加载环、文件局部性恶化或可独立测试边界时才重新评估拆分。

### 一次性重写所有视图

拒绝：无法逐 Adapter 回滚，且会同时冲击 Table text properties、Node follow 和 Search origin。

## Consequences

### Positive

- 新视图不再重复 buffer/display/subscription/cleanup。
- 各 renderer 保持本地可理解性。
- 每个 Adapter 可独立迁移、测试和回滚。
- Stream 可以在下一阶段作为普通 Adapter 实现。

### Negative

- 迁移期会同时存在 Runtime instance 与部分旧 buffer-local 业务状态。
- `supertag-view-framework.el` 会暂时承担更多生命周期代码。
- 必须为隐性窗口、selection 和 cleanup 行为补测试。

## Constraints

- 不破坏现有 public command、mode、keymap、buffer name 与 display。
- 不修改现有 README/menu/smart-key 用户工作区改动。
- 不新增依赖。
- Renderer 不写 Store；state builder 不修改 buffer。

## Rollback

旧 command wrapper 保留到 phase 验收。任一 Adapter 迁移失败时，将该 wrapper 路由回旧 open/refresh 实现；其他已验证 Adapter 和 Runtime 不受影响。

## Amendment — 2026-08-05

所有具体 view 已有统一 Runtime 后，不再保留旧 Developer View 生命周期作为回滚路径。Progress Dashboard、Effort Distribution 与 Priority Matrix 迁移为普通 Adapter；随后删除旧宏、独立 buffer renderer、legacy refresh 和仅用于分流的 `:runtime` flag。

不提供 compatibility shim：旧 API 没有独立生命周期所有权，继续保留只会让新 view 可以绕过 Runtime 的 cleanup、refresh 与 display 契约。历史 phase 文档保留原决策记录，当前开发指南只描述统一 Runtime API。
