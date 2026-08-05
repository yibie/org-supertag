# task_widget_renderer_20260805

- task001 [x] 建立并锁定 Widget Renderer phase
  - 依据：`DONE-phase-view-runtime-20260804/tech_refer_widget_renderer_20260805.md` 与用户 2026-08-05 实施指令
  - 产出：PR/FAQ、spec、ADR、plan、task、change 与 `.codex` 执行计划
  - 验证方式：前置 View focused ERT 33/33；文档互相引用一致；`git diff --check`
  - 影响范围：本 phase 文档、`.codex/plans/widget-renderer-backend.md`、全局 CHANGE 索引

- task002 [x] 实现稳定 key selection
  - 依据：spec Flow B
  - 产出：`:key` range、DSL capture/restore、missing-key fallback
  - 验证方式：refresh 前方长度变化与 key 删除 ERT
  - 影响范围：Framework DSL、runtime tests

- task003 [x] 实现原生交互 leaf 与 mode/keymap
  - 依据：spec Flow C-D、ADR
  - 产出：`:button`、`:link`、`:editable-field`、一次 `widget-setup`、safe erase、键盘导航
  - 验证方式：真实 `button-at`/Widget notify/refresh ERT 与图形 smoke
  - 影响范围：Framework DSL、framework/runtime tests

- task004 [x] 实现两阶段 layout commit
  - 依据：spec Flow A、tech reference「Layout Without Temporary Widget Creation」
  - 产出：measurable placeholders、final-buffer materialization、columns/card interactivity
  - 验证方式：button/field 位于 layout 后仍可激活/编辑；连续 refresh 不泄漏
  - 影响范围：Framework layout/widgets、tests

- task005 [x] 迁移三个 Dashboard 并删除旧 renderer
  - 依据：PR/FAQ、spec Compatibility Contract
  - 产出：dynamic Widget specs、保留 view/buffer/content/demo contract、删除三套手写 render function
  - 验证方式：Dashboard Runtime/content ERT；renderer diff 净删除
  - 影响范围：三个 Dashboard、framework/runtime tests

- task006 [x] 完成 Stream fixture 与规模测量
  - 依据：spec Acceptance Criteria
  - 产出：keyed text/tag link/edit button fixture，100/500/1000-node initial/refresh/overlay 数据
  - 验证方式：Runtime 无特例；focused benchmark 与图形 smoke
  - 影响范围：runtime tests、phase evidence

- task007 [x] 完成文档、全量和实机验收
  - 依据：plan Quality Gates
  - 产出：Developer Guide、CHANGELOG、change/task/spec evidence、`manual_test_widget_renderer_20260805.md`
  - 验证方式：focused/full/static/compile/checkdoc/graphical gates；用户明确批准
  - 影响范围：文档与验收记录；批准前不 commit/push
  - 当前状态：自动化 385/385、图形 `Emacs.app -Q` smoke、byte compile、checkdoc 与 `.elc` zero 已通过；package-lint 本机不可用；用户于 2026-08-05 明确批准
