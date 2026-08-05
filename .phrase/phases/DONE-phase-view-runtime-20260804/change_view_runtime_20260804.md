# change_view_runtime_20260804

## 2026-08-05 — task012/task014 — Verify

- Verification: focused/full ERT、Emacs 29.1/29.4 CI、图形 `Emacs.app -Q` 9/9、legacy lifecycle 静态残留扫描、byte compile/checkdoc、`git diff --check` 与 repo-local `.elc` zero 均通过。
- Approval: 用户于 2026-08-05 明确批准实机结果；task012/task014 完成，View Runtime phase 可结项。
- Gap: `package-lint` 本机不可用，未为验收引入新依赖。

## 2026-08-05 — task017 — Add / Modify

- Files:
  - `tech_refer_widget_renderer_20260805.md`
  - `tech_refer_view_runtime_20260804.md`
  - `plan_view_runtime_20260804.md`
  - `task_view_runtime_20260804.md`
  - `.phrase/docs/CHANGE.md`
- Behavior: 无产品行为变化；保存 Emacs Widget、`widget-extra`、VUI 与当前 View Runtime/DSL 的技术探索，供后续 renderer phase 决策。
- Decision: Runtime 保持 renderer-agnostic；Widget DSL 作为可选 renderer backend，优先使用 built-in `button.el`/`widget.el`，以不可变 render spec、稳定 `:key`、两阶段 layout 和完整重绘解决当前痛点；不强制迁移 Search/Table/Kanban。
- Dependency: 当前拒绝 `widget-extra`，暂不采用或复制 VUI；达到复杂 local-state/async/reconciliation 门槛时重新评估 VUI。
- Verification: 文档包含来源、pain map、module seam、最小实验、acceptance gates、upgrade triggers 与 non-goals；`git diff --check` 通过。
- Risk: 这是研究建议而非批准实现；后续不得仅凭本记录新增依赖或扩张 Runtime，必须另开实现任务并先锁定真实 Dashboard/Stream workflow。

## 2026-08-05 — task016 — Modify / Delete

- Files:
  - `supertag-view-framework.el`
  - `supertag-view-progress-dashboard.el`
  - `supertag-view-effort-distribution.el`
  - `supertag-view-priority-matrix.el`
  - `supertag-ui-search.el`
  - `supertag-view-table.el`
  - `supertag-view-kanban.el`
  - `supertag-view-node.el`
  - `test/view-framework-test.el`
  - `test/view-runtime-test.el`
  - `doc/VIEW_FRAMEWORK_DEV_GUIDE.md`
  - `doc/examples/supertag-view-demo-dashboard.el`
  - `doc/A-DAY-WITH-ORG-SUPERTAG.org`
  - `doc/A-DAY-WITH-ORG-SUPERTAG_CN.org`
  - `CHANGELOG.org`
- Behavior: Progress Dashboard、Effort Distribution、Priority Matrix 作为普通 Runtime Adapter 注册；picker 一律走 `supertag-view-open`，refresh 只接受 View Instance；三个 demo 也走公开 open 路径。
- Delete: 删除 `define-supertag-view`、`supertag-view--with-buffer`、`supertag-view-render`、legacy rendering/buffer-local state、`supertag-view--refresh-legacy`、`:runtime` definition flag、对应实现耦合测试与过时开发文档。
- Simplify: DSL 与三套 Dashboard 共用一个 `supertag-view--rebuild-context` 状态规则；删除未使用的 config 临时值，并复用既有 line padding 修复 Widget table 的无效动态 format。
- Verification: 两个公共路径回归先红后绿；focused 33/33、full 382/382；四个核心文件 byte compile/checkdoc 通过；静态残留为零；Widget table smoke、`git diff --check`、repo-local `.elc` 为零；图形 `Emacs.app -Q` Dashboard 3/3，`display-graphic-p=t`。
- Risk: 这是有意的开发者 API 删除，不提供 deprecated alias；自定义旧 view 需按更新后的 Developer Guide 改为 `supertag-view-register` + `supertag-view-open`。Schema View 不在本任务范围。`package-lint` 本地未安装。

## 2026-08-04 — task015 — Modify / Delete

- Files:
  - `supertag-view-kanban.el`
  - `test/test-view-kanban.el`
  - `issue_view_runtime_kanban_card_target_20260804.md`
  - `task_view_runtime_20260804.md`
- Function: `supertag-view-kanban--get-card-info`
- Behavior: Kanban 卡片移动读取 point 所在卡片的稳定 `node-id`/`group-value`，不再因二维并排列布局误命中上一行相邻列卡片。
- Delete: 删除对装饰字符 `┌` 的反向搜索与由此产生的显示文本重解析。
- Verification: 新回归先以 `field-set-args=nil` 失败，再转绿；Kanban 4/4、full 382/382、图形 `emacs -Q` View Runtime smoke 9/9 通过。
- Risk: point 位于列间空白时现在明确没有卡片目标；这比猜测相邻卡片安全，既有卡片行与导航点均带稳定属性。

## 2026-08-04 — task014 — Add

- Files:
  - `manual_test_view_runtime_20260804.md`
  - `task_view_runtime_20260804.md`
  - `spec_view_runtime_20260804.md`
  - `plan_view_runtime_20260804.md`
- Behavior: 增加真实图形 Emacs 的 hands-on 验收步骤，覆盖窗口落点、焦点、按键、selection restore、写操作回退、Refs read purity 与 subscriber cleanup。
- Verification: Emacs 31.0.91 的独立图形 `emacs -Q`（`display-graphic-p=t`）完成 9/9：Runtime、Search、Table、Kanban、Node、Widget DSL、Refs purity 与 teardown 全部 PASS；完整结果记录于 `manual_test_view_runtime_20260804.md`。
- Risk: 用户明确批准仍是 commit/push 与 task012/task014 结项门槛。

## 2026-08-04 — task012 — Partial verification

- Files:
  - `task_view_runtime_20260804.md`
  - `spec_view_runtime_20260804.md`
  - `change_view_runtime_20260804.md`
  - `.phrase/docs/CHANGE.md`
- Behavior: 完成本地、跨版本 CI 与独立 blocker review 预检；hands-on 验收尚未完成，不结项。
- Verification:
  - 本地 Emacs 31：focused ERT 55/55、full ERT 381/381。
  - 静态：所有改动 Elisp `check-parens`、`git diff --check` 通过；repo-local `.elc` 为零。
  - 独立审查：P0/P1 为零；未发现双 lifecycle、subscription 泄漏或 read-path Store 写入。
  - GitHub Actions run 30889643751：提交 `3e652a70e16b027af8e2d3291c1d46550827acf2`，Emacs 29.1 与 29.4 jobs 均 success。
- Risk: CI 对 `actions/checkout@v4` 报 Node 20 deprecation annotation，GitHub 已强制使用 Node 24；与本次 Elisp 代码无关。真实窗口、焦点、按键与编辑回退仍需 task014 验证。

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
