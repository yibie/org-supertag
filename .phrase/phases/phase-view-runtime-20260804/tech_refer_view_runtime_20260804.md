# tech_refer_view_runtime_20260804

## Existing Seams

- `supertag-view-api.el`：现有 UI-agnostic read/subscription facade，继续作为数据边界。
- `supertag-view-framework.el`：已有 view registry、render dispatch、buffer helper、refresh 与 Widget DSL；应深化而非另建平行 registry。
- `supertag-services-ui.el`：已有 data-only node state builder，可直接作为 Node Adapter seam。
- `supertag-view-table.el`：已有 table state builder、layout registry 与 renderer，迁移时保留。
- `supertag-core-store.el`：正常写入发出 `:store-changed`；Adapter 不应监听不存在的抽象事件。

## Options

### Option A：继续让每个 view 独立

- 优点：没有迁移成本。
- 缺点：订阅泄漏和事件不一致继续存在；Stream 会复制第五套生命周期。
- 结论：拒绝。

### Option B：所有 view 改写成 Widget DSL

- 优点：表面上只有一种 renderer。
- 缺点：Table、Kanban、Node 与 Search 的交互差异会进入公共 DSL，增加条件分支和回归面。
- 结论：拒绝。

### Option C：统一 Runtime + 独立 Adapter

- 优点：只统一真正重复的生命周期，保留 renderer locality，可逐个迁移和回滚。
- 缺点：迁移期需要兼容旧 buffer-local state 与 public command。
- 结论：采用。

## Proposed Approach

优先深化 `supertag-view-framework.el`；除非出现真实加载环或模块不可维护的证据，不新增 `supertag-view-runtime.el`。这样可以复用现有 registry、refresh 名称和 Widget DSL，避免第二套 framework。

### View Definition

沿用 plist registry。最小 definition：

```elisp
(:id VIEW-ID
 :mode MODE-FN
 :buffer-name BUFFER-NAME-FN
 :state-fn STATE-FN
 :render-fn RENDER-FN
 :display-action DISPLAY-ACTION
 :subscribe-fn SUBSCRIBE-FN
 :capture-selection-fn CAPTURE-FN
 :restore-selection-fn RESTORE-FN)
```

只有被实际 Adapter 使用的 optional key 才进入实现；不为未来可能性预留 callback。

### View Instance

使用一个 buffer-local plist，不引入 EIEIO 或新依赖：

```elisp
(:view-id VIEW-ID
 :input INPUT
 :state STATE
 :cleanup-fns (FN ...))
```

Instance 是 refresh 与 teardown 的唯一事实来源。Adapter 现有 buffer-local 变量可在迁移期继续保存 renderer/command 所需状态，但不得再拥有独立 subscription 生命周期。

### Public Interface

```elisp
(supertag-view-register DEFINITION)
(supertag-view-open VIEW-ID INPUT &optional DISPLAY-ACTION)
(supertag-view-refresh &optional BUFFER)
```

兼容现有 `supertag-view-register` 调用形状。如无法无歧义扩展，内部增加窄 helper，而不是破坏旧配置。

### Open Lifecycle

1. 查找 definition；unknown id 直接 `user-error`。
2. 验证 input 并构建 state。
3. 计算 buffer name，创建或复用 buffer。
4. 若 buffer 已有 instance，先 cleanup。
5. 安装 mode 与 buffer-local instance。
6. 在当前 buffer 中 render。
7. 建立 subscription，并把返回的 unsubscribe 放入 instance cleanup list。
8. 使用 definition 或调用方传入的 `display-buffer` action 展示。

若 render/subscription 中途失败，执行本次已登记 cleanup，不发布半初始化 instance。

### Refresh Lifecycle

1. 从目标 buffer 读取 instance 与 definition。
2. Adapter capture selection；允许返回 nil。
3. 用原 input 重建 state。
4. 在 `inhibit-read-only` 下调用 renderer。
5. 更新 instance state。
6. Adapter restore selection；实体已消失时 fallback 到可用 point。

Runtime 提供正确的完整重建路径；Table 等 Adapter 可以继续保留已验证的局部快速更新。

### Cleanup Lifecycle

- Runtime 在 view buffer 的 local `kill-buffer-hook` 中运行 instance cleanup。
- Cleanup callbacks 全部尝试执行；一个 callback 报错不能阻止其余 invariant cleanup。
- 清理后将 instance 置 nil，重复 cleanup 为 no-op。
- Subscription callback 必须检查 `buffer-live-p`。

### Presentation

- Search/Table/Kanban 使用其现有 display 行为对应的 `display-buffer` action。
- Node 使用 `display-buffer-in-side-window` 兼容 action，保留 side/slot/width。
- Runtime 不直接写 window layout policy；调用方或 definition 传原生 action。

### Entity and Selection Contract

- 新的公共实体属性为 `supertag-entity-id`。
- Table 迁移期继续设置 `entity-id`、`col-key`、`col-index`。
- Adapter selection 是 opaque value；Runtime 只负责保存和回传。
- Search/Stream 可用 entity id，Table 可用 entity id + column，Kanban 可用 card id，Node 可用 field/entity。

### Store Events

- 默认订阅 Store 实际发出的 `:store-changed`。
- Adapter subscription callback 负责判断变化是否影响当前 input。
- 不引入新的事件总线。
- Stream 产品阶段可评估 `:store-committed`，但本阶段 fixture 不依赖它。

## Adapter Migration Notes

### Search

- 保留 history、origin buffer/point、cards、mark/export 与 mode。
- Runtime 接管 result buffer create/display/instance/refresh。
- 初始版本不新增自动 subscription。

### Table

- 保留 `--build-state`、layout registry、renderer、columns、sorting、editing 与 cell text properties。
- Runtime 接管 open/display、instance、subscription handle 与 kill cleanup。
- 删除迁移后重复的 active lifecycle state；业务 buffer-local state可暂留。

### Kanban

- 保留 config、grouping、renderer、navigation 与 move operations。
- `supertag-ui-commands.el` 中的公开命令改为薄 wrapper。
- subscription 改由 Runtime 持有，并对齐真实 Store event。

### Node

- 复用 `supertag-view-build-node-state`。
- 保留 side-window config、follow semantics、field focus 与 renderer。
- Runtime 持有 Store unsubscribe 和 follow hook cleanup；查看不得创建 Org ID。

### Widget DSL

- 保留旧 registry/config/render API。
- 用同一 Runtime instance/refresh 包装已有 DSL buffer path。
- Stream-ready fixture 只实现 state/render/selection 的最小 Adapter，不创建产品命令。

## Verification

- Runtime unit：open、reopen、refresh、selection、cleanup-all、error rollback、dead-buffer callback。
- Adapter regression：Search origin、Table properties/editing、Kanban grouping/event、Node follow/no-ID/subscription dedupe。
- Compatibility：旧 framework/DSL ERT 全部继续通过。
- Full suite：权威 runner 清理 repo-local `.elc` 后执行全部 ERT。

## Trade-offs

- plist 比 struct 类型约束弱，但复用现有 registry、diff 最小；若真实错误表明 schema 不够，再考虑 struct。
- 完整 refresh 可能比局部 patch 慢，但先保证一致性；已存在且被测试的局部路径不删除。
- 深化现有 framework 会增加文件长度，但避免双 registry；只有出现加载环或职责无法局部理解时才拆文件。
