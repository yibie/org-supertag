# change_view_runtime_20260804

## 2026-08-04 — task013 — Modify / Delete

- Files:
  - `supertag-view-table.el`
  - `test/test-view-table.el`
  - `task_view_runtime_20260804.md`
- Functions: `supertag-view-table--get-columns-for-tag`、`supertag-view-table-edit-cell`
- Behavior: Table 读取/渲染列配置时不再创建或关联 `Refs` schema；虚拟 `Refs` 列仍始终可见，用户显式编辑该列时才通过既有 ops 初始化必要字段。
- Delete: 删除 read path 对 `supertag-view-table--ensure-refs-field` 的隐式调用。
- Verification: legacy/global 两种字段模式的 Store/event 快照回归通过；focused 55/55、full 381/381 通过。
- Risk: 首次编辑 `Refs` 会在操作边界创建 schema，并可能触发一次正常的 Store refresh；只读打开与刷新保持无副作用。

## 2026-08-04 — task011 — Modify / Add

- Files:
  - `supertag-view-framework.el`
  - `test/view-runtime-test.el`
  - `test/view-framework-test.el`
  - `task_view_runtime_20260804.md`
- Functions: `supertag-view-define-from-config`、`supertag-view-select-and-render`、`supertag-view-list-for-tag`、`supertag-view-framework-init`
- Behavior: 声明式 Widget DSL view 使用同一 Runtime instance/open/refresh；内部生产 Adapter 不进入自定义 view picker；Framework 初始化保留已注册 widget 类型。
- Add: 最小 Stream-shaped Adapter fixture 仅使用公开 Runtime definition keys，验证全文 body open/refresh 无需 Runtime 特例。
- Verification: DSL selection/open/refresh、picker filtering、Stream fixture 与既有 framework tests 通过；focused 55/55、full 381/381 通过。
- Risk: 本阶段只证明接入合同，不创建 Stream 产品命令或 UI。

## 2026-08-04 — task010 — Modify / Delete

- Files:
  - `supertag-view-node.el`
  - `supertag-ui-commands.el`
  - `test/test-view-node-runtime.el`
  - `task_view_runtime_20260804.md`
- Functions: Node state/render/open/display/selection/subscription callbacks；`supertag-view-node--show-side`、refresh/follow paths
- Behavior: Runtime 接管 Node side-window、instance、Store subscription、manual/local follow cleanup 与 field selection；保留 auto-show、field editing、no-ID 和 Evil/window integration，并增加 `supertag-entity-id`。
- Delete: 移除 mode 内重复 subscription、未保存 unsubscribe 的 event handler；Node renderer 不再绕 Table node-detail 后回调自身。
- Verification: `./test/run-tests.sh view-runtime view-table view-kanban view-node view smart-key`，49/49 通过。
- Risk: Node 对相关 Store collection 采用正确性优先的完整 refresh；若真实性能数据显示压力，再增加 entity predicate。

## 2026-08-04 — task009 — Add / Modify

- Files:
  - `test/test-view-node-runtime.el`
  - `test/run-tests.sh`
  - `task_view_runtime_20260804.md`
- Behavior: 新增 Node side buffer、重复 open subscription dedupe、follow hook cleanup、field selection restore 与公共 entity property 验收；复用既有 Smart Key no-ID 回归。
- Verification: 2 个 Runtime 验收在实现前按预期失败；既有 no-ID 测试继续由 `test/test-smart-key.el` 覆盖。
- Risk: 本任务只增加测试与 runner 登记，不修改 Node 实现。

## 2026-08-04 — task008 — Modify / Delete

- Files:
  - `supertag-view-kanban.el`
  - `supertag-ui-commands.el`
  - `test/test-view-kanban.el`
  - `task_view_runtime_20260804.md`
- Functions: Kanban state/render/open/selection/subscription callbacks；`supertag-view-kanban-refresh`
- Behavior: Runtime 接管 Kanban open/display/refresh/subscription/cleanup；公开命令只采集参数；保留 grouping/navigation/card move，并增加 `supertag-entity-id`。
- Delete: 移除错误的 `:node-updated` 本地订阅、unsubscribe buffer state 与全局 kill hook。
- Verification: `./test/run-tests.sh view-runtime view-table view-kanban view smart-key`，47/47 通过。
- Risk: Store change predicate 只覆盖影响 Kanban 数据/schema 的 collection，避免无关全量刷新。

## 2026-08-04 — task007 — Add / Modify

- Files:
  - `test/test-view-kanban.el`
  - `test/run-tests.sh`
  - `task_view_runtime_20260804.md`
- Behavior: 新增 Kanban grouping/render、selection、card move dispatch、真实 `:store-changed` refresh 与 kill unsubscribe 验收。
- Verification: 3 个验收在实现前按预期因缺少 Runtime open seam 而失败。
- Risk: 本任务只增加测试与 runner 登记，不修改 Kanban 实现。

## 2026-08-04 — task006 — Modify / Delete

- Files:
  - `supertag-view-table.el`
  - `test/test-view-table.el`
  - `task_view_runtime_20260804.md`
- Functions: Table Runtime input/render/display/selection/subscription callbacks；`supertag-view-table-refresh`
- Behavior: Runtime 接管 Table open/display/instance/refresh/subscription/cleanup；保留 state、layout、editing 与 Smart Key 属性，并增加 `supertag-entity-id`。
- Delete: 移除错误的 `:node-updated`/`:database-updated` 订阅、全局 kill hook、active-view hash 与实际整表重绘的伪局部更新链。
- Verification: `./test/run-tests.sh view-runtime view-table view smart-key`，44/44 通过。
- Risk: 自检发现既有取列路径可能写 schema，新增 task013 在 phase 结束前锁定并修正纯度。

## 2026-08-04 — task005 — Add / Modify

- Files:
  - `test/test-view-table.el`
  - `test/run-tests.sh`
  - `task_view_runtime_20260804.md`
- Behavior: 锁定 Table cell 的 `entity-id`/`col-key`/`col-index`，并新增 selection restore 与真实 `:store-changed` subscription/kill cleanup 验收。
- Verification: baseline 2/2 通过；两个迁移验收按预期失败，分别暴露刷新后 selection 丢失与 Store 事件/cleanup 不匹配。
- Risk: 本任务只增加测试与 runner 登记，不修改 Table 实现。

## 2026-08-04 — task004 — Modify

- Files:
  - `supertag-ui-search.el`
  - `test/view-runtime-test.el`
  - `task_view_runtime_20260804.md`
- Functions: `supertag-search`、`supertag-search-show-results`、`supertag-search--build-view-state`、`supertag-search--render-view`、Search selection callbacks
- Behavior: Search 使用 Runtime 管理同名 results buffer；手动刷新重新查询 Store；保留 origin/quit、cards、mode、mark/export，并增加 `supertag-entity-id`。
- Verification: `./test/run-tests.sh view-runtime view`，28/28 通过。
- Risk: Search 仍直接使用现有搜索数据函数，符合本阶段“不重写 query semantics”的范围。

## 2026-08-04 — task003 — Modify

- Files:
  - `supertag-view-framework.el`
  - `test/view-runtime-test.el`
  - `task_view_runtime_20260804.md`
- Functions: `supertag-view-refresh`、`supertag-view--refresh-instance`、`supertag-view--refresh-legacy`、`supertag-view--cleanup-instance`
- Behavior: Runtime refresh 从原 input 重建 state 并回传 opaque selection；kill/reopen 执行全部 cleanup；晚到事件忽略 dead buffer；render、subscribe 或 display 失败时不发布半 instance并回收已建资源。
- Verification: red→green 覆盖 state、subscribe 与 display failure rollback；最终 focused 55/55、full 381/381 通过。
- Risk: Runtime 尚未接管生产 Adapter；legacy refresh 路径保持不变。

## 2026-08-04 — task002 — Add / Modify

- Files:
  - `supertag-view-framework.el`
  - `test/view-runtime-test.el`
  - `test/run-tests.sh`
  - `task_view_runtime_20260804.md`
- Functions: `supertag-view-open`、`supertag-view--cleanup-instance`
- Behavior: 新增 Runtime open seam；根据 input 构建 state、创建/复用 buffer、安装 mode、渲染、展示并在 reopen 时替换旧 cleanup。
- Verification: `./test/run-tests.sh view-runtime view`，19/19 通过。
- Risk: 尚未接管旧视图；refresh、kill cleanup 与错误回滚由 task003 完成。

## 2026-08-04 — task001 — Add

- Files:
  - `pr_faq_view_runtime_20260804.md`
  - `spec_view_runtime_20260804.md`
  - `plan_view_runtime_20260804.md`
  - `tech_refer_view_runtime_20260804.md`
  - `adr_view_runtime_20260804.md`
  - `task_view_runtime_20260804.md`
  - `change_view_runtime_20260804.md`
  - `.phrase/docs/CHANGE.md`
- Behavior: 批准方案 C，锁定统一 Runtime + 独立 Adapter 的范围、兼容契约、迁移顺序、验证和回滚策略。
- Risk: 本条仅修改开发事实来源，不改变运行时行为。
