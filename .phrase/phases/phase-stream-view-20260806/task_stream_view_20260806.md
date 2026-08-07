# task_stream_view_20260806

- task001 [x] 批准并锁定 Stream View phase
  - 依据：`pr_faq_stream_view_20260806.md`
  - 产出：PR/FAQ、spec、tech reference、plan、task、change 与全局 CHANGE 索引
  - 验证方式：文档边界一致；`git diff --check`
  - 影响范围：本 phase 文档，不改产品代码

- task002 [x] 锁定 descendant query、sorting 与 full-body render
  - 依据：spec Data Contract / Flow A
  - 产出：失败优先 ERT、data-only state、Runtime registration、Widget node blocks
  - 验证方式：真实 Store fixture；`diaryx` 反例；title/tag/body/key 属性断言
  - 影响范围：Stream module、focused test、runner、package require

- task003 [x] 实现 split/plain、导航与稳定选择
  - 依据：spec Flow B-C
  - 产出：index companion、window arrangement、`n`/`p`/`s`/`g`/`q`、selection overlays
  - 验证方式：public command workflow ERT；refresh/missing node/toggle/cleanup
  - 影响范围：Stream presentation，不改 Runtime

- task004 [x] 实现源节点 narrow 编辑与 Node View 入口
  - 依据：spec Flow D-E
  - 产出：indirect edit、`C-c C-c` return、`v`、Node View public node-ID entry
  - 验证方式：临时真实 Org file，child exclusion、base-buffer write、no autosave、field dispatch
  - 影响范围：Stream edit boundary、Node View public API

- task005 [x] 完成 subscription、错误边界与文档
  - 依据：spec Edge Cases / Compatibility Contract
  - 产出：Store refresh/cleanup、empty/missing file/narrow window 行为、README/CHANGELOG/guide
  - 验证方式：重复 open/refresh/toggle/quit 资源计数与用户错误断言
  - 影响范围：Stream module/tests/docs

- task006 [ ] 完成规模、全量、静态与图形实机验收
  - 依据：spec Acceptance Criteria、plan Quality Gates
  - 产出：100/500/1000 measurement、full ERT、compile/checkdoc、manual test record
  - 验证方式：focused/full/static/graphical `emacs -Q` 全部通过；用户确认可感知行为后结项
  - 影响范围：验收记录；确认前不关闭 phase

- task007 [x] 保留 source-backed upsert 的节点创建时间
  - 依据：issue032、spec Flow D
  - 产出：共享同步入口保留已有 `:created-at`，Stream narrow 编辑回归测试
  - 验证方式：先红后绿的 public Stream edit ERT；focused sync/node regression
  - 影响范围：`supertag-services-sync.el` 的既有 upsert 路径，不改变新节点创建语义

- task008 [x] 统一 `:extends` 后代查询并清理斜杠层级数据
  - 依据：issue009、spec Data Contract / Flow A、2026-08-07 用户决策
  - 产出：Stream/View/Table 只按传递 `:extends` 聚合；Schema 不再派生斜杠 namespace；真实 Store 迁移并备份
  - 验证方式：失败优先 ERT 覆盖 direct/deep descendants 与斜杠反例；focused/full/static；真实 Store/source 重载确认
  - 影响范围：共享 scan/View API、Schema/View/Table、nested/Stream tests、真实 Store 的两个遗留 Tag
