# PR/FAQ：让 Widget DSL 成为真实的 View Renderer Backend

## Status

- Approved: 2026-08-05，用户明确要求依据 `tech_refer_widget_renderer_20260805.md` 开始计划与实施。
- Predecessor: `DONE-phase-view-runtime-20260804`；其 hands-on gate 已于 2026-08-05 获得用户批准。

## Press Release

### Org-Supertag 用统一 Renderer Backend 构建 Dashboard 与交互式知识流

**2026-08-05 —** Org-Supertag 开始将现有 Widget DSL 从演示性工具变成真实的 View renderer backend。三个内置 Dashboard 和一个 Stream-shaped 交互 fixture 将共享同一套不可变 render spec、稳定节点身份和 Emacs 原生按钮/字段，同时继续通过统一 View Runtime 管理打开、刷新、订阅和关闭。

此前 Runtime 已消除了不同 View 重复实现 buffer 生命周期的问题，但各 Dashboard 仍手写相似 renderer，Widget DSL 主要存在于 demo 和测试中。直接把所有 View 改写成 DSL 会把 Table cell、Kanban card 和 Search match 等领域语义推入公共层；引入完整响应式 UI framework 又会复制 Runtime 已拥有的 state、subscription 与 teardown。

新的 renderer backend 只解决已经出现的问题：稳定 `:key` 让完整刷新后仍能回到同一逻辑节点；`button.el` 提供轻量链接与操作；`widget.el` 只承担成熟的 editable field；两阶段布局先测量 plist spec，再在最终 buffer 创建交互对象，避免临时 buffer 丢失 markers、overlays 和 callbacks。

“我们不是把 Emacs 变成浏览器，而是让已有的 Runtime 和文本显示原语各自承担擅长的工作。”项目负责人表示，“如果迁移 Dashboard 后没有删掉更多代码，DSL 就没有资格继续扩张。”

开发者仍使用 `supertag-view-register`、`supertag-view-open` 和 `supertag-view-refresh`。适合声明式渲染的 View 可以提供 Widget render spec；Search、Table 与 Kanban 继续保留自己的 renderer。所有实现先经过 ERT、规模测量和图形 `emacs -Q` 验证，用户明确批准后才允许提交或推送。

## Customer FAQ

### 用户会看到新的 UI 吗？

本阶段不重设计 Dashboard，也不发布 Stream 产品命令。现有 Dashboard buffer 名、核心内容和打开方式保持不变；新增交互只在测试 fixture 与未来可复用 primitive 中验证。

### 为什么不把所有 View 都改成 Widget DSL？

Search、Table、Kanban 的 renderer 包含匹配、cell、column、lane、card 与导航语义。把这些知识塞进 DSL 只会形成按 view type 分支的超级 Framework。本阶段只迁移结构化、主要只读且能实际删除重复代码的 Dashboard。

### 为什么不使用 `widget-extra`？

它增加 package、`dash` 与 `s`，未进入 MELPA，buffer helper 与 Runtime ownership 冲突，而且 simple table 不满足 Supertag Table contract。内置 `button.el`/`widget.el` 已覆盖本阶段真正需要的 leaf primitive。

### 为什么不直接使用 VUI？

VUI 解决 component-local state、hooks、effects、reconciliation 和 mount lifecycle，但当前知识 View 由 Store/Runtime 驱动，不需要第二套 root instance。若未来真实表单达到 tech reference 的升级门槛，将另开阶段评估由 VUI 替换 DSL，而不是逐项复制。

## Internal FAQ

### 核心数据结构是什么？

仍然是轻量 plist/list render spec。需要逻辑 identity 的节点增加 `:key`；renderer 不修改 spec。Runtime instance 继续是 View 生命周期的唯一事实来源。

### 如何解决 Widget 在临时 buffer 测量后失效？

交互 leaf 首先插入带描述数据的定宽占位文本。columns/card 完成最终排版后，renderer 扫描最终 buffer，将占位区域 materialize 成 text button 或 editable field，并统一调用一次 `widget-setup`。

### 会实现增量 diff 吗？

不会。第一版完整重绘并复用 Runtime selection restore。只有 100/500/1000-node 测量表明完整刷新不可接受，才讨论可见区域或 keyed region patch。

### 如何防止 DSL 继续膨胀？

Dashboard 迁移必须净删除 renderer 代码；至少 Dashboard 与 Stream fixture 两个真实形态复用 backend；Framework 不得出现按 view id/type 的特殊分支。不能满足就回滚或删除 DSL 能力。
