# task_view_runtime_20260804

- task001 [x] 建立并批准 View Runtime phase 文档
  - 依据：`pr_faq_view_runtime_20260804.md`
  - 产出：PR/FAQ、spec、plan、tech refer、ADR、task、change 与 CHANGE 索引
  - 验证方式：文档互相引用一致；`git diff --check` 通过
  - 影响范围：`.phrase/phases/phase-view-runtime-20260804/`、`.phrase/docs/CHANGE.md`

- task002 [x] 以 red→green 切片实现 Runtime open/reopen seam
  - 依据：spec「Flow A」「Runtime Contract」
  - 产出：unknown view、state→render、buffer/mode、reopen cleanup 的逐条失败优先测试与最小实现；runner 增加 focused filter
  - 验证方式：每条测试先失败再通过；原 `view-framework-test.el` 保持通过
  - 影响范围：`test/view-runtime-test.el`、`test/run-tests.sh`、`supertag-view-framework.el`

- task003 [x] 以 red→green 切片完成 refresh/selection/cleanup/error seam
  - 依据：spec「Flow B-D」「Edge Cases」、tech refer「Refresh/Cleanup Lifecycle」
  - 产出：manual refresh、selection restore、cleanup-all、dead-buffer callback、render/subscribe/display error rollback 的逐条测试与最小 Runtime 实现
  - 验证方式：runtime focused ERT 全部通过；旧 registry/DSL tests 继续通过
  - 影响范围：`test/view-runtime-test.el`、`supertag-view-framework.el`

- task004 [x] 迁移 Search Adapter
  - 依据：spec「Flow A」「Compatibility Contract」
  - 产出：Search 通过 Runtime 管理 result buffer/display/instance；保留 history、origin、cards、mark/export、quit
  - 验证方式：ERT 覆盖 open、origin return、quit cleanup、manual refresh；手动确认原命令与按键
  - 影响范围：`supertag-ui-search.el`、Search/runtime tests

- task005 [x] 锁定 Table 生命周期与 Smart Key 属性
  - 依据：spec「Compatibility Contract」「Entity and Selection Contract」
  - 产出：Table open/refresh/cleanup、selection、`entity-id`/`col-key`/`col-index` 的回归测试
  - 验证方式：迁移前测试通过或明确暴露现存 subscription 缺陷；不修改 Table 实现
  - 影响范围：`test/test-view-table.el`、runtime adapter tests

- task006 [x] 迁移 Table Adapter 生命周期
  - 依据：tech refer「Table」
  - 产出：Runtime 接管 Table open/display/instance/subscription/cleanup；保留 state、renderer、editing 与 text properties
  - 验证方式：task005、runtime、smart-key focused ERT 通过；重复 open/kill subscriber 不增长
  - 影响范围：`supertag-view-table.el`、Table/runtime tests

- task007 [x] 锁定 Kanban 分组、操作与真实 Store 事件
  - 依据：spec「Compatibility Contract」、tech refer「Store Events」
  - 产出：Kanban grouping/navigation、card operation dispatch、refresh event 与 cleanup 回归测试
  - 验证方式：测试能复现旧 `:node-updated` 与实际 `:store-changed` 的不匹配
  - 影响范围：Kanban/runtime tests

- task008 [x] 迁移 Kanban Adapter 生命周期
  - 依据：tech refer「Kanban」
  - 产出：Runtime 接管 open/display/instance/subscription/cleanup；公开命令变成薄 wrapper
  - 验证方式：task007 与 runtime focused ERT 通过；手动验证 card navigation/move
  - 影响范围：`supertag-view-kanban.el`、`supertag-ui-commands.el`、Kanban/runtime tests

- task009 [x] 锁定 Node side-window、follow、selection 与 no-ID 行为
  - 依据：spec「Compatibility Contract」「Edge Cases」
  - 产出：Node subscription dedupe/cleanup、follow、field focus、selection restore、查看不创建 Org ID 的回归测试
  - 验证方式：原 no-ID tests + 新 lifecycle tests 通过或明确暴露现存泄漏
  - 影响范围：Node/runtime tests、`test/test-smart-key.el`

- task010 [x] 迁移 Node Adapter 生命周期
  - 依据：tech refer「Node」
  - 产出：Runtime 接管 Node side-window display、instance、Store subscription 与 follow cleanup；复用 data-only node state
  - 验证方式：task009、runtime、smart-key focused ERT 通过；重复开关无 subscriber/hook 累积
  - 影响范围：`supertag-view-node.el`、Node/runtime tests

- task011 [x] 接入 Widget DSL 并证明 Stream-ready contract
  - 依据：PR/FAQ「本阶段会实现 Stream View 吗？」、spec「Acceptance Criteria」
  - 产出：旧 DSL view 使用 Runtime instance/refresh；最小 Stream-shaped test Adapter 不含产品 UI
  - 验证方式：旧 `view-framework-test.el` 通过；fixture 只注册 Adapter，不修改 Runtime
  - 影响范围：`supertag-view-framework.el`、framework/runtime tests

- task012 [x] 完成全量验收与 phase 回写
  - 依据：plan「Quality Gates」
  - 产出：focused/full ERT 结果、Emacs 29.1/29.4 CI、兼容自动化证据、独立 blocker review、实机验收与 change/task/spec 回写
  - 验证方式：focused/full/CI 与图形 `emacs -Q` 9/9 已通过；用户于 2026-08-05 明确批准 task014
  - 影响范围：测试、phase 文档、全局 CHANGE 索引

- task013 [x] 验证并消除 Runtime render/state 路径中的 Store 写入
  - 依据：spec「Runtime Contract」；task006 自检发现 Table 取列仍可能触发 `ensure-refs-field`
  - 产出：以事件/Store 快照测试证明 Table 读列不写 Store；Refs schema 初始化只在用户显式编辑 Refs 时执行；静态复核其他 Adapter 的 state/render 只读/当前 buffer 边界
  - 验证方式：legacy/global schema purity ERT、Adapter focused ERT 与全量 ERT 通过；既有 Refs 列与编辑路径保留
  - 影响范围：Table 初始化 seam、runtime adapter tests；其他 Adapter 仅在测试暴露问题时修改

- task014 [x] 完成 View Runtime 实机 hands-on 验收
  - 依据：用户指出自动化/CI 不能替代真实 Emacs 的窗口、焦点、按键与编辑回退验证
  - 产出：`manual_test_view_runtime_20260804.md` 中 Search、Table、Kanban、Node、Widget DSL、Refs purity 与 teardown 的图形 `emacs -Q` 实测记录
  - 验证方式：Emacs 31.0.91、`display-graphic-p=t` 的隔离 Store smoke 9/9 通过；用户于 2026-08-05 明确批准
  - 影响范围：实机运行状态与 phase 文档；默认不再修改产品代码

- task015 [x] 修复 Kanban 并排卡片的操作目标解析
  - 依据：issue031；task014 图形 smoke 暴露左列 point 会命中上一行右列边框
  - 产出：公开移动命令的双列失败回归；卡片定位直接读取 point 上稳定的 `node-id`/`group-value` 元数据
  - 验证方式：回归先红后绿；Kanban 4/4、full 382/382、图形 View Runtime smoke 9/9 通过
  - 影响范围：`supertag-view-kanban.el`、`test/test-view-kanban.el`、issue031 与 phase 验收文档

- task016 [x] 删除 Legacy Developer View 双生命周期
  - 依据：用户要求新 Runtime 完成后清理同类旧代码；plan「Legacy lifecycle cleanup order」；ADR 2026-08-05 amendment
  - 产出：三套 Dashboard 迁移为 Runtime Adapter；demo/当前开发文档迁移；删除旧宏、旧 render/buffer state、legacy refresh 与 `:runtime` 分支
  - 验证方式：公共 open/select/refresh 回归先以 2 个失败锁定旧分流，再转绿；focused 33/33、full 382/382、四个核心文件零 warning byte compile/checkdoc、静态引用扫描、`git diff --check`、Widget table smoke 与图形 `emacs -Q` Dashboard 3/3 通过
  - 影响范围：View Framework、三个 Dashboard、View tests、当前开发文档与本 phase 文档；不改 Schema View 和历史 phase 记录

- task017 [x] 保存 Widget renderer backend 技术探索
  - 依据：用户要求保存 Emacs Widget 优劣、`widget-extra` 依赖评估与本地解法的探索结论
  - 产出：`tech_refer_widget_renderer_20260805.md`，记录 Runtime/DSL pain map、原生 primitive policy、stable key、两阶段 layout、完整重绘策略、依赖结论、最小实验和升级门槛
  - 验证方式：来源链接、当前代码 seam、推荐/拒绝项和未来 acceptance gates 可独立追溯；`git diff --check` 通过
  - 影响范围：本 phase 技术参考、plan/task/change 与全局 CHANGE 索引；不修改产品代码、配置或依赖
