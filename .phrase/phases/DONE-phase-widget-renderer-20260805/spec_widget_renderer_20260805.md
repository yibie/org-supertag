# spec_widget_renderer_20260805

## Summary

本阶段把现有 Widget DSL 深化为一个真实 View renderer backend。它使用不可变 plist render spec、稳定 `:key`、内置 `button.el`/`widget.el` primitive 和两阶段 layout commit，服务三个内置 Dashboard 与一个 Stream-shaped fixture，同时保持 View Runtime renderer-agnostic。

## Goals

- DSL view 完整刷新后按稳定 key 恢复逻辑位置及区域内 offset。
- `:button`、`:link` 使用 `button.el`；`:editable-field` 使用 `widget.el`。
- 交互 leaf 在 columns/card 中从最终 buffer materialize，保留 callback、field 和 key metadata。
- Widget render 完成后只调用一次 `widget-setup`，刷新前安全清除旧 Widget bookkeeping。
- View mode 保留 Runtime/Adapter keymap ownership，并能键盘到达按钮与字段。
- Progress Dashboard、Effort Distribution、Priority Matrix 使用同一 DSL backend，删除手写 buffer renderer。
- Stream-shaped fixture 验证 text、tag link、edit button、refresh selection 与规模成本，无 Runtime 特例。

## Non-goals

- 不发布 Stream 产品 View 或命令。
- 不迁移 Search、Table、Kanban renderer。
- 不重设计 Schema View 或 Node editing。
- 不新增 `widget-extra`、VUI、`dash`、`s` 或其他 package。
- 不实现 hooks、context、effects、component-local state、virtual DOM diff 或 general reconciliation。
- 不改变 Store schema、数据格式或 ops write contract。

## Render Spec Contract

- Widget tree 是 plist/list 值；`:type` 必须存在。
- `:widgets` 可以是 tree，也可以是接收 context、返回新 tree 的函数。
- 需要 selection/interactivity/repeated-list identity 的节点必须提供稳定 `:key`。
- Renderer 不得修改调用方 config、context 或 render spec。
- callback 可以调用既有 command/ops，但 renderer 自身不得写 Store、display window 或持有 subscription。
- 未知 Widget type、无效 children 或无效 callback 必须给出明确 error，不静默降级。

## User Flows

### Flow A：打开声明式 View

1. Runtime 构建 state 并调用 Widget renderer。
2. Renderer 清除旧 Widget bookkeeping，解析动态 render spec，生成文本布局。
3. Renderer 在最终 buffer materialize button/field，并调用一次 `widget-setup`。
4. Runtime 发布 instance 并 display buffer。

### Flow B：刷新并恢复 keyed selection

1. Runtime capture DSL selection 为 `(:key KEY :offset N)`。
2. state builder 生成新 context/render spec。
3. Renderer 完整重绘。
4. restore 查找相同 key，恢复 offset；key 消失则回退 `point-min`。

### Flow C：激活按钮或链接

1. 用户鼠标或键盘激活 text button。
2. button callback 调用配置的 function。
3. callback 可通过既有 ops 写 Store；相关 Store event 由 Runtime subscription 触发 refresh。

### Flow D：提交 editable field

1. 用户只在 field 区域编辑。
2. Widget notify 将新值交给 `:on-change` function。
3. callback 负责 validation/ops；renderer 不把 Widget object 当作业务状态。
4. refresh 后按稳定 key 恢复到对应 field。

## Compatibility Contract

- 现有 Widget type、`supertag-widget-register` 与 `supertag-view-define-from-config` 保留。
- 三个 Dashboard 的 view id、name、buffer name、required body text 和 public demo command 保留。
- Runtime open/refresh/cleanup 顺序不增加 Widget 特例。
- Search/Table/Kanban/Node 的 mode、renderer、selection 与编辑行为不因本阶段改变。

## Acceptance Criteria

- 新 ERT 在实现前能复现 key 丢失、button/field 缺失和 layout interactivity 缺口。
- Keyed node 前方文本长度变化后 refresh 仍恢复相同 key/offset；key 删除时安全回退。
- Button/link callback 可由真实 `button-at`/`button-activate` 激活。
- Editable field 可由真实 Widget value/notify path提交，buffer 外部文本仍受保护。
- Button 和 editable field 位于 columns/card 后仍是 live interactive object。
- 连续 refresh 不累积 dead overlays、field bookkeeping 或 callback。
- 三个 Dashboard 使用 Widget DSL backend；对应手写 render function 被删除；迁移净减少 renderer LOC。
- Stream fixture 不修改 Runtime，实现 keyed text/link/button 与 refresh。
- 记录 100/500/1000-node initial render、refresh、overlay/marker 数据。
- Focused/full ERT、Emacs 29.1/29.4 CI、byte compile、checkdoc、package-lint、`git diff --check` 与 repo-local `.elc` zero 通过；package-lint 不可用时必须诚实记录。
- 图形 `emacs -Q` 完成 Dashboard/Stream/button/field/layout/refresh smoke，用户明确批准后才允许 commit/push。

## Verification Status — 2026-08-05

- Focused View ERT: 36/36。
- Full ERT: 385/385（沙箱内 6 项因无法写 `~/.emacs.d/.org-id-locations` 假失败；同一 runner 在允许该现有测试写入后全绿）。
- Byte compile/checkdoc: 本阶段 4 个发行 Elisp 文件通过；编译产生的 `.elc` 已删除。
- Package-lint: 本机不可用，未伪报通过。
- Graphical smoke: 独立 `Emacs.app -Q` 通过 Dashboard、button、field、columns/card、TAB、key restore 与 refresh。
- Benchmark: 100/500/1000-node refresh 均低于 0.1 秒；read-only fixture 无 Widget field/overlay。
- Hands-on approval: 用户于 2026-08-05 明确批准；task007 完成，可以按提交协议落地。
