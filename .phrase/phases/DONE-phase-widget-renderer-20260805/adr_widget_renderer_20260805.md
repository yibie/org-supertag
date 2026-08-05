# ADR：Widget DSL 使用原生 Primitive 与延迟 Materialization

## Context

View Runtime 已统一 buffer、display、refresh、subscription cleanup 与 selection transfer，但 Widget DSL 主要服务 demo，三个 Dashboard 仍手写 renderer。Emacs Widget 适合 editable field，却不提供 layout/state/tree；直接在临时 buffer 创建 Widget 后复制文本会丢失 markers、overlays 与 callbacks。

## Decision

- Runtime 保持 renderer-agnostic。
- DSL render spec 使用不可变 plist/list 与稳定 `:key`。
- text/title/badge 直接 insert；button/link 使用 `button.el`；editable field 使用 `widget.el`。
- 交互 leaf 先输出定宽、带描述属性的 placeholder；layout 完成后在最终 buffer materialize。
- 第一个版本完整重绘，通过 Runtime capture/restore 保持逻辑位置。
- 三个 Dashboard 迁移为真实 DSL consumer；Search/Table/Kanban 保留 custom renderer。

## Rejected

- `widget-extra`：新增三项安装成本，停止积极开发，buffer ownership 与 Runtime 冲突。
- VUI：当前需求不需要第二套 component/mount/state/effect lifecycle；达到明确升级门槛时重新评估直接采用。
- 所有 View 统一 DSL：会把 cell/card/search 领域语义推入公共 renderer。
- 立即实现 incremental diff：没有 benchmark 证据，增加 identity/cache invalidation 复杂度。
- 临时 buffer 创建 native Widget 后复制：交互状态不能可靠迁移到最终 buffer。

## Consequences

- DSL 获得 Dashboard 与 Stream fixture 的真实复用证据。
- 完整 redraw 的成本必须被测量，但实现和正确性模型保持简单。
- `supertag-view-framework.el` 暂时继续包含 backend；只有迁移后文件局部性和 deletion test 证明拆分有收益才移动模块。
- 如果 Dashboard 迁移未净删除代码，停止扩张 DSL并回滚该部分。

## Rollback

保留 Runtime `render-fn` seam。任一 Dashboard 可退回本地 renderer，而不回退 Runtime；原生 primitive/backend 只在有真实消费者时保留。
