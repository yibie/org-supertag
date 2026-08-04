# spec_view_runtime_20260804

## Summary

本阶段将 Search、Table、Kanban、Node 从四套独立 buffer 生命周期迁移到统一 View Runtime。Runtime 只管理视图定义、view instance、buffer/display、刷新、订阅清理和选择恢复；各 Adapter 保留自己的 state、renderer、mode、keymap 与写操作。

本阶段不重做 UI，也不实现 Stream。完成后，Stream 必须能作为新 Adapter 接入而无需修改 Runtime。

## Terms

- **View Definition**：静态注册信息，描述 view id、mode、buffer、state、render 与可选生命周期回调。
- **View Instance**：某个 view buffer 中的运行时状态，至少包含 view id、input、state 与 cleanup callbacks。
- **Runtime**：拥有 open、refresh、display 与 teardown 的公共层。
- **Adapter**：Search、Table、Kanban、Node 或 Widget DSL 提供的本地实现。
- **Selection**：刷新前后需要恢复的用户位置；可由 entity id、cell、card、field 或 point 表示。

## Goals

- 提供统一的 `supertag-view-open` 与 `supertag-view-refresh` 生命周期。
- 每个 view buffer 只拥有一个 buffer-local View Instance。
- 所有 Store subscription 与 Adapter cleanup callbacks 在 buffer kill 时执行且只执行一次。
- Store 正常变更能触发需要更新的视图，不累积重复 subscriber。
- 刷新后尽量恢复当前 entity/cell/card/field；没有可恢复对象时使用安全 point fallback。
- 迁移 Search、Table、Kanban、Node，同时保留现有用户命令和交互。
- 保持 View DSL、Widget registry 与 `define-supertag-view` 兼容。
- 建立统一的 `supertag-entity-id` 定位契约，并在迁移期保留现有 `entity-id` 等属性。

## Non-goals

- 不实现 Stream View 或重设计 Schema View。
- 不把所有 renderer 改写成 Widget DSL。
- 不新增 Window Manager、Query Engine、事件总线、renderer 基类或第三方依赖。
- 不重写 Table cell editing、Kanban card operations、Node field editing 或 Search query semantics。
- 不在未测量前实现通用 diff/incremental renderer。
- 不借本阶段重命名全部旧 API、buffer 或 mode。

## User Flows

### Flow A：打开已有视图

1. 用户执行已有 `M-x` 命令。
2. 兼容 wrapper 将参数交给 `supertag-view-open`。
3. Runtime 创建或复用原有命名的 buffer，安装原 mode，渲染并按原方式显示。
4. 若输入无效，在创建订阅前给出 `user-error`；不得遗留半初始化 instance。

### Flow B：数据变化后自动刷新

1. Adapter 通过 Runtime 注册 Store subscription。
2. 相关 Store 事件到达后，Runtime 捕获 selection、重建 state、渲染并恢复 selection。
3. 不相关事件由 Adapter predicate 忽略。
4. 若刷新失败，保留可用 buffer，不遗留新增订阅，并向用户报告错误。

### Flow C：手动刷新

1. 用户在任一已迁移视图执行 `M-x supertag-view-refresh`。
2. Runtime 使用 View Instance 中的原 input 重建 state。
3. 视图更新，selection 尽量保持。
4. 非 Runtime buffer 调用该命令时给出明确 `user-error`。

### Flow D：退出视图

1. 用户使用原有退出键或 kill buffer。
2. Runtime 执行所有 subscription/cleanup callbacks。
3. Instance 从运行时索引移除。
4. 重复 kill、已失效 callback 或单个 callback 报错不得阻止其余 cleanup 执行。

### Flow E：现有视图编辑

1. 用户继续使用 Table cell、Kanban card、Node field 或 Search action。
2. Adapter 调用既有 ops 完成写入。
3. Runtime 只负责后续 refresh，不在 renderer 中执行 Store 写入。
4. 失败路径沿用各 Adapter 现有错误反馈与撤销能力。

## Compatibility Contract

| Surface | Required behavior |
| --- | --- |
| Commands | `supertag-search`、`supertag-view-table`、`supertag-view-kanban`、`supertag-view-node` 继续可调用 |
| Buffers | 保留现有固定或计算后的 buffer 名称 |
| Modes | 保留现有 major mode、keymap 与退出键 |
| Display | Search/Table/Kanban 保持原展示方式；Node 保持 side-window |
| Search | 保留 origin buffer/point 与返回行为 |
| Table | 保留 columns、sorting、editing 与 Smart Key text properties |
| Kanban | 保留 grouping、navigation 与 card operations |
| Node | 保留 follow、字段焦点、side-window 与“不因查看创建 Org ID” |
| DSL | 保留 registry、widgets、配置定义与手动刷新路径 |

## Runtime Contract

- View Definition 必须有稳定 `:id`、state builder 与 renderer；mode/buffer/display 可使用现有默认值。
- Runtime 在首次成功 render 前不得建立无法回收的 subscription。
- Runtime 持有 unsubscribe/cleanup callbacks；Adapter 不自行安装无归属的 kill hook。
- Generic refresh 的顺序固定为 capture → build → render → restore。
- State builder 可以读取 View API/Store，但不能修改 view buffer 或写 Store。
- Renderer 只能修改当前 view buffer，不能打开窗口、订阅事件或写 Store。
- Presentation 使用 Emacs 原生 `display-buffer` action。
- 正常 Store 更新默认使用实际存在的 `:store-changed` 事件；Adapter 可以进一步过滤。
- Runtime 不强制 Adapter 放弃已有的局部快速更新；`supertag-view-refresh` 始终提供正确的完整重建路径。

## Edge Cases

- Unknown view id：在 buffer/订阅创建前 `user-error`。
- State builder 返回空：renderer 显示 Adapter 现有空状态，不崩溃。
- Entity 在 capture 后被删除：restore 使用安全 point fallback。
- 同一 buffer 重复 open：替换旧 instance 前先 cleanup，不重复订阅。
- Buffer 已 kill 后事件到达：callback 安全退出，不访问 dead buffer。
- 多个 cleanup 中一个报错：继续执行其余 cleanup，再报告首个错误。
- Render 期间报错：不得发布新的半初始化 instance 或泄漏订阅。
- Node follow 快速切换：只展示最后一个有效 node，不创建 Org ID。
- Table/Smart Key：迁移期间同时保留旧属性与公共 `supertag-entity-id`。
- 本地旧 `.elc`：验证必须优先加载源码；不能用裸 Emacs 的陈旧 byte-code 结果判定失败。

## Acceptance Criteria

- Runtime contract ERT 覆盖 open、refresh、selection、reopen、cleanup、error rollback。
- Search、Table、Kanban、Node 都通过同一 Runtime 创建 View Instance。
- 四个公开命令与兼容性表中的行为通过自动化或明确的手动检查。
- 重复打开/关闭每个 view 后 subscriber 数量不增长。
- Table 与 Kanban 监听真实 Store 事件；正常 ops 写入后视图可刷新。
- Node subscription 能取消，follow/selection 行为通过回归测试。
- `supertag-view-refresh` 在四个视图可用，非 view buffer 报明确错误。
- Widget DSL 旧测试通过；最小 Stream-shaped fixture 仅新增 Adapter，不修改 Runtime。
- Focused ERT、全量 `./test/run-tests.sh all`、`git diff --check` 通过。
- CI 在 Emacs 29.1 与 29.4 通过。
