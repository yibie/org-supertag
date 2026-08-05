# PR/FAQ：统一 View Runtime

- Status: Approved
- Approved At: 2026-08-04
- Approved By: Product owner
- Phase: `phase-view-runtime-20260804`
- Date: 2026-08-04
- Decision: 方案 C（统一运行时 + 独立 View Adapter）

## Press Release

### Org-Supertag 用统一 View Runtime 连接 Search、Node、Table 与 Kanban

**用户继续使用原有命令与界面；开发者只实现视图特有的数据与渲染，即可接入一致的打开、刷新、订阅和清理生命周期。**

Org-Supertag 今天启动统一 View Runtime 的开发。它将当前分别管理 buffer、窗口、刷新、订阅和清理的 Search、Node、Table、Kanban 接入同一个运行时，同时保留每个视图已有的外观、快捷键和编辑能力。

目前这些视图更像四个独立应用。它们各自创建 buffer、保存状态、选择显示窗口、监听 Store 事件并处理退出。用户因此会遇到不一致的刷新行为，维护者也必须在每个新视图里重复解决资源释放、光标恢复和窗口显示。若直接开发 Stream，它会成为第五套实现。

统一 View Runtime 只接管共同生命周期：注册视图、创建或复用 view instance、构建状态、调用 renderer、使用 Emacs 原生 `display-buffer` 展示、订阅数据变化、刷新并恢复选择、在 buffer 销毁时取消订阅。Search、Node、Table、Kanban 仍各自负责查询参数、渲染、按键和领域操作。

“我们不是把所有界面压成一种 Widget，而是把每个界面都重复实现的运行时收回来。”Org-Supertag 项目负责人表示，“这样 Table 仍是 Table，Node 仍是侧边 inspector；未来 Stream 只需提供 state 与 renderer，不再复制一套小型应用框架。”

用户无需迁移配置。`M-x supertag-search`、`M-x supertag-view-node`、`M-x supertag-view-table` 与 `M-x supertag-view-kanban` 保持可用，并继续打开熟悉的界面。迁移将逐个视图进行，每一步都可以退回原入口。

## Customer FAQ

### 1. 用户会看到新的界面吗？

不会。本阶段是运行时统一，不是 UI 重设计。已有 buffer 名称、窗口位置、快捷键、导航和编辑行为默认保持不变。

### 2. 为什么用户需要关心内部架构？

用户不需要学习新概念，但会得到更一致的刷新、退出和选择恢复行为。更重要的是，后续 Stream 等新视图可以更快落地，且不会带来另一套不一致的生命周期。

### 3. Node 的侧边栏、Table 的单元格编辑和 Kanban 的卡片操作会被削弱吗？

不会。这些属于 Adapter 的本地能力。Runtime 不解释字段、单元格或卡片，也不在 renderer 中代替现有 ops 执行写入。

### 4. 现有 View DSL 和 Widget 会被替换吗？

不会。View DSL 保留为一种 renderer Adapter，而不是成为所有视图必须使用的通用渲染器。已有自定义视图 API 需要保持兼容。

### 5. 本阶段会实现 Stream View 吗？

不会。Stream 是下一阶段的产品功能。本阶段的完成标准之一，是 Stream 可以作为新 Adapter 接入，而无需修改 Runtime。

### 6. Schema View 也会一起迁移吗？

不会。Schema View 是 schema 管理与编辑界面，不是本阶段首批 collection/inspector 视图。只有在统一 Runtime 被现有四个视图验证后，才评估是否迁移。

## Internal FAQ

### 1. 根因是什么？

仓库已有两个正确但不完整的基础：

- `supertag-view-api.el` 是 UI 无关的只读数据边界；
- `supertag-view-framework.el` 有 registry、render dispatch 与 Widget DSL。

缺失的是一等的 view instance。Search、Node、Table、Kanban 因此分别回答“buffer 属于谁、如何显示、何时刷新、订阅如何释放、选择如何恢复”。真正需要统一的是这些生命周期，而不是 renderer。

### 2. Runtime 的最小公开接口是什么？

目标接口只有三类能力：

```elisp
(supertag-view-register ...)
(supertag-view-open VIEW-ID INPUT &optional DISPLAY-ACTION)
(supertag-view-refresh &optional BUFFER)
```

现有交互命令继续作为兼容 wrapper。若现有 `supertag-view-register` 的语义无法无破坏扩展，可以保留旧注册入口并在内部适配；本阶段不为了命名整洁而破坏外部 API。

### 3. Runtime 与 Adapter 的边界是什么？

Runtime 负责：

- definition registry 与 view instance；
- buffer 创建、复用和 buffer-local instance；
- `display-buffer` presentation；
- refresh、selection capture/restore；
- subscription handle 与 `kill-buffer-hook` cleanup；
- 标准空状态与错误边界。

Adapter 负责：

- input normalization 与 state build；
- 在当前 view buffer 中渲染；
- mode、keymap、局部交互与 ops 调用；
- 视图特有的选择快照及恢复；
- 必要的事件过滤。

### 4. 固定生命周期是什么？

```text
resolve definition
  -> validate input
  -> build state
  -> create/reuse buffer
  -> install mode and instance
  -> render
  -> subscribe
  -> display

refresh
  -> capture selection
  -> rebuild state
  -> render
  -> restore selection

kill buffer
  -> run every unsubscribe function
  -> remove instance
```

Renderer 不打开窗口、不订阅事件、不写 Store；state builder 不修改 buffer。

### 5. 为什么不把全部视图改写成 Widget DSL？

Table、Kanban、Search 和 Node 的交互模型不同。统一 Widget 会把渲染差异抬进公共层，增加条件分支并扩大回归面。现有 DSL 已能作为自定义 dashboard renderer 使用，没有证据表明它适合替代 Table 的 cell editing 或 Node 的 follow 模式。

### 6. 为什么使用 Emacs 原生窗口能力？

`display-buffer`、`display-buffer-in-side-window`、buffer-local state、`kill-buffer-hook` 和窗口选择 hooks 已覆盖所需的 presentation 与 teardown。Runtime 只组织这些原语，不新增 Window Manager 或外部依赖。

### 7. 迁移顺序是什么？

高层顺序如下，原子任务在 PR/FAQ 批准后另行拆分：

1. 用 ERT 锁定 Runtime contract 与四个视图的现有生命周期；
2. 实现最小 Runtime，不改变任何公开命令；
3. 先迁移无实时订阅的 Search，验证 buffer/display contract；
4. 迁移 Table 的生命周期但保留 state、renderer、编辑和 text properties；
5. 迁移 Kanban 的生命周期并保留公开命令；
6. 最后迁移有 side-window、follow hook 和焦点恢复的 Node；
7. 将 Widget DSL 接为兼容 Adapter，并用一个最小测试 Adapter 验证 Stream-ready contract。

### 8. 哪些兼容性是不可破坏的？

- 四个现有 `M-x` 命令、buffer 名称、mode/keymap 和窗口放置；
- Search 返回原 buffer/point 的行为；
- Table 的 `entity-id`、`col-key` 等 Smart Key text properties；
- Node 不因查看而创建 Org ID，并保留 follow/字段焦点行为；
- Kanban 的分组与卡片操作；
- 现有 DSL 与 `define-supertag-view` 路径。

统一实体定位将增加公共 `supertag-entity-id` contract；迁移期间保留现有属性，不能用一次性重命名破坏 Smart Key。

### 9. 如何判断本阶段成功？

- 每个迁移后的 view buffer 只拥有一个 Runtime instance；
- `supertag-view-refresh` 可刷新四个视图并恢复用户选择；
- buffer 销毁后所有订阅均被释放，重复打开不会累积 subscriber；
- Store 正常更新能触发预期视图刷新，不再监听不存在或不匹配的事件；
- renderer 不展示窗口、不注册订阅，state builder 不修改 buffer；
- 现有命令、编辑、导航和 Smart Key 回归测试通过；
- 新建一个 Stream 形态的测试 Adapter 时，不需要修改 Runtime。

### 2026-08-05 决策更新：为什么不再保留旧 Developer View 路径？

`define-supertag-view` 的兼容承诺只用于 Adapter 迁移期。Progress Dashboard、Effort Distribution、Priority Matrix 全部接入 Runtime 后，这条旧路径不再提供回滚价值，反而允许 renderer 绕过统一的 buffer、refresh 和 cleanup 契约。因此本阶段在迁移三套 Dashboard 后直接删除旧宏、旧 buffer wrapper、legacy refresh 与 `:runtime` 分流，不提供兼容 shim；Widget DSL 继续保留并使用统一 Runtime。

### 10. 最大风险与回滚策略是什么？

最大风险是 Table text properties、Node follow/selection、Search origin 和订阅 cleanup 的隐性行为被迁移破坏。每个 Adapter 单独迁移并保留旧 renderer 与公开 wrapper；任何一步失败，都只回退该 Adapter 的 open/refresh 路由，不回退已经验证的 Runtime 或其他 Adapter。

## Topology Lock

本阶段允许独立成功或失败的顶层组件固定为：

1. View Runtime core；
2. Search Adapter；
3. Table Adapter；
4. Kanban Adapter；
5. Node Adapter；
6. Widget DSL compatibility Adapter + Stream-ready acceptance fixture。

不新增通用 Window 框架、Query Engine、Renderer 基类、事件总线或第三方依赖。`supertag-view-api.el` 继续作为数据读取边界，写操作继续走既有 ops。

## Evidence

- 本地代码：`supertag-view-framework.el`、`supertag-view-api.el`、`supertag-ui-search.el`、`supertag-view-node.el`、`supertag-view-table.el`、`supertag-view-kanban.el`。
- Vulpea UI 采用轻量 sidebar 生命周期和可注册 widget，而非统一所有 renderer：<https://github.com/d12frosted/vulpea-ui>。
- GNU Emacs 原生 side-window、buffer teardown 与窗口选择能力：
  - <https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers-in-Side-Windows.html>
  - <https://www.gnu.org/software/emacs/manual/html_node/elisp/Killing-Buffers.html>
  - <https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Hooks.html>

## Approval Gate

批准本 PR/FAQ 即表示确认：

- 采用“统一 Runtime + 独立 Adapter”；
- 本阶段迁移 Search、Table、Kanban、Node，并兼容 Widget DSL；
- Stream 与 Schema UI 实现不进入本阶段；
- 用户可见行为以兼容为默认，任何刻意变化必须单独写入 spec。

批准后再生成 `spec_*`、`plan_*`、`task_*`、`tech_refer_*`、`adr_*` 与 `change_*`，并登记全局索引。
