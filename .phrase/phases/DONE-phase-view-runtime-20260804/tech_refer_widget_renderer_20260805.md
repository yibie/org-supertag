# tech_refer_widget_renderer_20260805

## Status

- 类型：技术探索记录。
- 日期：2026-08-05。
- 状态：推荐方向，尚未批准实现或新增依赖。
- 读者任务：决定下一阶段是否用一个最小 renderer 实验，让 Widget DSL 获得真实调用方，同时避免在 Org-Supertag 内复制 VUI。

## Executive Conclusion

Emacs Widget 的主要问题不是输入控件不可用，而是缺少布局、状态传播、稳定节点身份和重绘后的上下文恢复。Org-Supertag 的 View Runtime 已经解决了 view-level state、Store subscription、refresh ordering、cleanup 与 Adapter selection restore，因此不需要从零建设响应式 UI framework。

推荐方向是把 Widget DSL 定位为一种 renderer backend：普通文本继续使用 buffer insertion，按钮和链接使用内置 `button.el`，只有真正需要 buffer 内编辑的字段使用内置 `widget.el`；DSL 以不可变 plist tree、稳定 `:key` 和两阶段布局补足组合能力。Search、Table、Kanban 继续保留 custom renderer，不为了统一外观而把领域交互塞进公共 DSL。

本次研究不建议依赖 `widget-extra`。它不是新的渲染模型，只是 `widget.el` 的便利组件集合；它未进入 MELPA、作者已停止积极开发、额外依赖 `dash` 与 `s`，其 buffer setup 与 Runtime ownership 冲突，并且不能替代 Supertag Table 的 entity/selection/editing contract。

VUI 确实系统性解决了文章中的问题，但它同时引入 component instance、local state、hooks、context、effects、async lifecycle、reconciliation、layout 与另一套 mount/unmount ownership。当前 View Runtime 与多数只读知识视图不需要这组成本；若后续出现复杂本地表单状态，再重新评估直接采用 VUI，不在现有 Framework 中逐项仿制。

## Research Question

当前 `supertag-view-framework.el` 同时拥有统一 View Runtime 与一个自定义 Widget DSL。Widget DSL 目前主要由 demo 和测试使用，生产 View 仍有独立 renderer。本次探索回答三个问题：

1. 是否应让 Framework 以 Emacs Widget 作为 renderer backend？
2. 是否应把所有 View renderer 改写成 Widget DSL？
3. `widget-extra` 或 VUI 是否值得成为新的运行时依赖？

## Sources

- Boris Buliga, [The Emacs Widget Library: A Critique and Case Study](https://www.d12frosted.io/posts/2025-11-26-emacs-widget-library)。
- GNU Emacs, [The Emacs Widget Library](https://www.gnu.org/software/emacs/manual/html_mono/widget.html)。
- d12frosted, [widget-extra](https://github.com/d12frosted/widget-extra)。
- d12frosted, [vui.el](https://github.com/d12frosted/vui.el) 与 [design document](https://github.com/d12frosted/vui.el/blob/master/docs/01-design-doc.org)。
- 本地实现：`supertag-view-framework.el`、`supertag-ui-search.el`、`supertag-view-table.el`、`supertag-view-kanban.el`、`supertag-view-node.el`。

## What Emacs Widget Does Well

- Widget 是带 text properties、overlays 和 markers 的 buffer text，保留 Emacs 原生搜索、复制、键盘宏、GUI/terminal 一致性。
- 对几百个线性 Widget，Emacs 的文本显示路径足够快。
- `define-widget`、property override 与 value conversion 适合构建 related field families。
- 内置 primitives 已覆盖 link、push button、editable field、toggle、checkbox、choice、list 与 group。
- `widget.el` 最值得复用的是经过长期验证的 in-buffer editable field，而不是把所有文本和按钮都包装成 Widget object。

## Pain Map Against Current Org-Supertag

| Article pain | Current capability | Gap / decision |
| --- | --- | --- |
| No state management | Runtime 已有 `input → state-fn → render-fn` | View-level 已解决；不增加 component-local state |
| No subscription propagation | Store event 通过 Adapter callback 触发 Runtime refresh | 已解决；renderer 不直接订阅 |
| Manual teardown | Runtime 持有 cleanup callbacks | 已解决 |
| Cursor surgery after redraw | Runtime 固定执行 capture → build → render → restore | Adapter 已解决；DSL 缺通用 stable key |
| No widget tree | DSL 已有递归 `:children` | 将它定义为不可变 render-spec tree，不建设 live DOM |
| Hierarchy without layout | DSL 已有 stack、columns、card、table | 当前 temp-buffer measure 不能承载 native Widget interactivity |
| Shared spec mutation | 当前 DSL 可从 config 反复 render | 规定 renderer 不修改输入；每次 state 生成新 spec |
| Full redraw | Runtime 默认完整重建 | 第一版接受；只有 benchmark 失败才增加局部更新 |
| No local derived state | Runtime state 可从 Store 派生 | 适合知识视图；复杂未提交表单状态仍是缺口 |
| No reactive component model | 无 hooks/context/effects | 明确不在本阶段实现；达到门槛时重新评估 VUI |

## Module Shape

```text
View Runtime
  owns: definition, instance, open, display, refresh, subscription cleanup
  calls: one Adapter render-fn

Adapter render-fn
  ├── Widget DSL renderer
  │     ├── insert/propertize: text, title, badge, separators
  │     ├── button.el: links and actions
  │     ├── widget.el: editable fields only
  │     └── Supertag layout: section, stack, columns, card, empty
  └── Custom renderer
        ├── Search match/origin semantics
        ├── Table cell/entity/column semantics
        └── Kanban lane/card/navigation semantics
```

Runtime 是生命周期 Module；Widget DSL renderer 是一个 Adapter implementation。Runtime 不应知道 widget type、layout、button action 或 field validation。Widget DSL 也不应创建 buffer、display window、持有 Store subscription 或安装第二套 kill lifecycle。

## Renderer Data Flow

```text
Store / View API
  ↓
state-fn
  ↓
immutable render spec
  ↓
measure layout
  ↓
render into final buffer
  ↓
widget-setup once

user action
  ↓
existing supertag ops
  ↓
Store event
  ↓
Runtime refresh
```

Widget callback 不直接寻找并修改其他 Widget。它只携带稳定 domain identity 调用既有 ops；Store 写入成功后由 Runtime 重新构建 state。这样 derived state 留在 state builder，显示留在 renderer，写入规则留在 ops。

## Render Spec Contract

保持轻量 plist/list，不新增 EIEIO、`cl-defstruct` vnode hierarchy 或兼容 VUI 的 component layer。

```elisp
(:type :section
 :key overview
 :children
 ((:type :text
   :key title
   :content "Package archives")
  (:type :link
   :key tag-emacs-package
   :label "#emacs/package"
   :target (:tag "emacs/package"))
  (:type :button
   :key edit-node
   :label "Edit"
   :action (:edit-node "node-id"))))
```

最小规则：

- `:type` 决定 leaf/container renderer。
- 需要交互、选择恢复或重复列表 identity 的节点必须有稳定 `:key`。
- `:key` 是同一 View Instance 内的逻辑 identity，不是 buffer position。
- config、state 与 render spec 均视为不可变值；renderer 不得 `plist-put` 修改调用方提供的对象。
- action data 保存 domain identity，由已有 command/ops 解释；DSL 不直接写 Store。
- container 接受 `:children`，但不拥有 component-local lifecycle。

## Stable Selection

Renderer 为有 key 的输出区域写入 `supertag-widget-key` text property。DSL Adapter 的 capture result 使用：

```elisp
(:key edit-node :offset 2)
```

刷新后先查找相同 key，再恢复区域内 offset；节点消失则按 Runtime contract 回退到安全 point。Table、Kanban、Node 继续使用各自现有 opaque selection，不改成 DSL key。

这个规则补足文章中的 cursor surgery，但复用现有 Runtime capture/restore seam，不新增全局 cursor manager。

## Layout Without Temporary Widget Creation

当前 DSL 的 `supertag-view--render-widgets-to-lines` 会在临时 buffer 渲染 children，再把文本行复制到最终布局。该方法只适合纯文本：native Widget 的 markers、overlays、field bookkeeping 和 callbacks 不能作为普通字符串安全迁移。

新的 layout renderer 应采用两阶段策略：

1. Measure：只从 render spec 与显示值计算 `string-width`、padding、column widths 和 truncation。
2. Commit：在最终 View buffer 的最终位置创建 text、button 或 editable field。

可测量 leaf 必须暴露确定宽度：文本和 button label 使用 `string-width`；editable field 使用显式 `:width`；未知动态宽度不得通过“在临时 buffer 创建 Widget”猜测。第一版 layout 只需要 vertical stack、fixed columns、card 与 table-like read-only rows，不建设 CSS/flex engine。

## Native Primitive Policy

| DSL leaf | Backend | Reason |
| --- | --- | --- |
| text/title/badge/separator | `insert` + text properties/faces | 不创建无收益的 Widget markers |
| link/action button | built-in `button.el` | 轻量、可点击、可键盘访问，避免 Widget marker 成本 |
| editable field | built-in `widget.el` `editable-field` | 复用成熟的 buffer 内编辑与 validation primitives |
| toggle/choice | 先使用 button + completing-read | 只有真实 inline editing requirement 才升级为 Widget |

Renderer 在一次完整 commit 后调用一次 `widget-setup`。View mode 必须组合 Widget/按钮导航与现有 Adapter map，不能像 `widget-extra` 的 buffer helper 那样直接 `use-local-map widget-keymap` 覆盖 major-mode bindings。

## Full Redraw Policy

第一版坚持完整重绘：它简单、与 Runtime state model 一致，而且 capture/restore 已提供正确性基础。不得为理论性能提前加入 vnode diff、dirty-region tracking、memo hooks 或 reconciliation cache。

需要单独测量的场景：

- 100、500、1000 个 Stream nodes 的首次 render 与 refresh。
- 大量 tag links/actions 时 marker 与 overlay 数量。
- 一个 editable field commit 后的 refresh 与 selection restore。
- 窗口 resize 后 columns/card 的重新布局。

只有用户可感知 refresh 超过项目后续定义的预算，才考虑 visible-region rendering 或 keyed region patch。优化不得牺牲标准搜索、复制和节点定位。

## Adapter Suitability

| View | DSL suitability | Recommendation |
| --- | --- | --- |
| Progress Dashboard | 高 | 第一批迁移，验证 stats/progress/empty/layout |
| Effort Distribution | 高 | 第一批迁移，验证 derived state 与 list |
| Priority Matrix | 高 | 第一批迁移，验证 sections/cards |
| Stream | 高 | 用 read-only node cards、tag links、edit button 验证真实价值 |
| Node | 中 | 只迁 field list/link/action leaf；保留 follow 与 field command semantics |
| Schema | 中 | 未来可用于 schema form；当前不进入 Runtime phase |
| Search | 低 | 保留 match/origin custom renderer |
| Table | 低 | 保留 entity/cell/column/editing custom renderer，可复用少量 leaf |
| Kanban | 低 | 保留 lane/card/navigation custom renderer，可复用 button/text primitive |

DSL 不需要成为唯一 renderer 才有意义。它必须至少服务多个真实结构化 View，并显著减少 renderer duplication；否则按照 deletion test 删除 DSL，而不是继续增加 widget types。

## Dependency Evaluation

### Built-in `widget.el` / `button.el`

- 分类：Emacs platform dependency，项目已经拥有。
- 优势：零安装成本、Emacs 29.1 baseline 可用、与 buffer/text/keymap 原语一致。
- 限制：没有 layout、reactive state 或 general widget tree；这些只由薄 DSL 补足当前需求。
- 结论：采用为 renderer implementation detail，不向 Runtime interface 暴露 Widget object。

### `widget-extra`

- 分类：外部 package，依赖 `dash` 与 `s`。
- 收益：提供 label、heading、field variants、buttons、fields-group、horizontal-choice 和 simple table。
- 成本：未进入 MELPA；README 明确停止积极开发；global widget type 名称宽泛；调用 `widget--should-indent-p` 私有符号；`widget-buffer-setup` 会切换/清空 buffer、kill local variables 和覆盖 local map；simple table 不满足 Supertag entity/selection contract。
- 结论：拒绝作为正式依赖。需要的少量 leaf 直接使用 built-in primitives，layout 由本地最小实现承担。

### VUI

- 分类：MELPA external package，Emacs 29.1+，无额外 package dependency。
- 收益：已提供 declarative tree、component/local state、hooks、context、effects、async cleanup、cursor-aware redraw、layout、resize refresh 和 developer tools。
- 成本：`vui.el` 主文件在研究时约 5836 行；它拥有 root instance、mode、mount/unmount、render scheduling 与 cleanup lifecycle，与现有 View Runtime 形成潜在双 ownership；迁移需要重新设计各 Adapter mode 和 instance relationship。
- 结论：当前不采用，也不在本地复制。复杂 local UI state 达到升级门槛时，单独评估“由 VUI 替换 Widget DSL renderer”而不是把 VUI features 逐项加入 Framework。

## Minimal Experiment

该实验属于后续实现阶段，当前文档不授权编码或新增依赖。

1. 为 DSL 加入稳定 `:key` 与 generic selection capture/restore。
2. 实现 built-in `button.el` 后端的 `:link`、`:button`。
3. 实现单一 built-in `widget.el` `:editable-field`，一次 render 只调用一次 `widget-setup`。
4. 将 layout 改成 render-spec measure → final-buffer commit，不创建临时 Widget。
5. 迁移三个 Dashboard，删除对应手写 renderer；迁移不能增加总体 renderer LOC。
6. 建立一个 Stream fixture：node title/body、tag link、edit button、refresh 后 stable key/point 恢复。
7. 在图形 `emacs -Q` 与 terminal batch 分别验证键盘导航、按钮、field commit、refresh、resize 与 teardown。
8. 记录代码量、首次 render、完整 refresh 和 marker/overlay 数量；结果达不到门槛则回滚实验并保留 custom renderer。

## Acceptance Gates For A Future Phase

- Dashboard 与 Stream fixture 至少两个真实 Adapter 通过同一 DSL renderer；不能只有 demo。
- 迁移后删除的手写 renderer 代码多于新增 backend/DSL 代码。
- Framework Runtime core 不出现 `pcase view-id` 或 Table/Kanban/Node 特例。
- Renderer 不写 Store、不创建窗口、不持有 subscription、不拥有第二套 kill lifecycle。
- Stable `:key` 能在 full refresh 后恢复 button/link/field 的逻辑位置。
- Interactive leaf 在 columns/card 中保留 button/widget functionality，不依赖临时 buffer string copy。
- 100/500/1000-node benchmark 和真实图形 `emacs -Q` 结果被记录。
- Emacs 29.1/29.4 CI、focused/full ERT、byte compile、checkdoc、package-lint 与 `git diff --check` 通过。
- 用户实机验收并明确批准后才提交或推送。

## Upgrade Triggers

满足任一条件时停止扩张薄 DSL，重新评估 VUI：

- 一个真实 View 需要三个以上相互依赖的未提交本地字段。
- 需要 component-local async timer/process/effect cleanup。
- 需要 collapsible/local state 在外部 Store refresh 后继续保持。
- 多个 View 都开始重复 state hooks、context propagation 或 keyed reconciliation。
- 完整重绘在真实 benchmark 中不可接受，而 visible-region/local patch 不能保持简单。

## Non-goals

- 不把 Search、Table、Kanban 强制改写成 Widget DSL。
- 不实现 React/VUI-compatible component interface。
- 不增加 hooks、context、effects、error boundary 或 general reconciliation。
- 不新增 `widget-extra`、`dash`、`s` 或 VUI dependency。
- 不用 deep widget inheritance 表达正交行为。
- 不为未来可能性引入 renderer base class、backend factory 或双 Runtime。

## Decision Summary

1. Runtime 继续保持 renderer-agnostic，只调用 Adapter `render-fn`。
2. Widget DSL 成为一个真实 renderer backend，而不是所有 View 的强制格式。
3. 使用 built-in `button.el` 渲染按钮/链接，使用 built-in `widget.el` 渲染 editable field，其他内容保持文本原语。
4. DSL 使用不可变 plist tree、稳定 `:key`、两阶段 layout 与完整重绘。
5. 先用 Dashboard + Stream fixture 证明代码删除、正确性和性能；证明失败则删除或收缩 DSL。
6. 当前拒绝 `widget-extra`，暂不采用 VUI；达到明确 local-state 升级门槛时重新评估 VUI。
