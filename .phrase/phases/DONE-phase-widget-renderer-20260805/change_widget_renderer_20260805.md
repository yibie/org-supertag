# change_widget_renderer_20260805

## 2026-08-05 — task007 — Verify

- Files: Developer Guide、CHANGELOG、本 phase 的 spec/task/change/manual test 与 `.codex` 执行计划。
- Verification: focused View ERT 36/36；full ERT 385/385；本阶段 4 个发行 Elisp 文件 byte compile/checkdoc 通过；图形 `Emacs.app -Q` Dashboard/Stream/button/field/layout/refresh smoke 通过；CJK field/card layout smoke 通过；repo-local `.elc` 为零；`git diff --check` 通过。
- Measurement: 100/500/1000 nodes 的 refresh 分别为 0.0062/0.0456/0.0794 秒，未达到局部 reconciliation 门槛。
- Approval: 用户于 2026-08-05 明确批准实机结果，解除 commit/push 门禁。
- Gap: `package-lint` 本机不可用，未伪报通过；未为此新增依赖。

## 2026-08-05 — task006 — Modify

- Files: `test/view-runtime-test.el`、本 change/spec/task 记录。
- Behavior: Stream-shaped fixture 现在通过公开 DSL/Runtime 路径渲染 keyed node body、tag link 与 edit button；刷新前插入新节点后仍恢复原逻辑节点。
- Measurement: 100/500/1000 nodes 的 initial render 为 0.0052/0.0373/0.0634 秒，refresh 为 0.0062/0.0456/0.0794 秒；对应 200/1000/2000 个 text button，0 editable field，0 overlay。
- Decision: 数据未触发局部 diff、virtual DOM 或 VUI 升级门槛，继续完整重绘。

## 2026-08-05 — task005 — Modify/Delete

- Files: `supertag-view-progress-dashboard.el`、`supertag-view-effort-distribution.el`、`supertag-view-priority-matrix.el`、`test/view-runtime-test.el`。
- Behavior: 三个 Dashboard 改为 dynamic Widget specs，保留 view id/name/buffer/required content/demo contract。
- Delete: 删除三套手写 `--render` 与直接 `supertag-view-register` 路径；三个 Dashboard 文件合计 215 additions / 218 deletions，净删除 3 行。
- Verification: 删除测试断言三个旧 renderer 不再定义，且 mode 统一为 `supertag-view-widget-mode`。

## 2026-08-05 — task002-task004 — Modify

- Files: `supertag-view-framework.el`、`test/view-runtime-test.el`。
- Behavior: 新增 literal stable `:key` range 与 capture/restore；`:widgets` 可动态生成 tree；`:button`/`:link` 使用 `button.el`，`:editable-field` 使用 `widget.el`。
- Layout: interactive leaf 先以带 text property 的可测量 placeholder 参与 columns/card 排版，再在最终 buffer materialize；refresh 前清除旧 Widget marker/overlay bookkeeping，完成后只调用一次 `widget-setup`。
- UX: 专用 major mode 组合 Special/Widget 行为，`TAB`/`S-TAB` 在 text button 与 editable field 之间移动；`'delete` 等函数同名 symbol key 被当作 literal。
- Unicode: editable field 将 `string-width` 显示列换算为 Widget 字符 padding；CJK 初值在 card 中保持边框宽度，超过显式 `:width` 的初值明确报错而不静默撑破 layout。
- Regression: 工作流 ERT 先复现 dynamic tree、callback 误解析、layout interactivity 与 button navigation 缺口，再完成修复。
- Risk: 保持完整重绘；未引入 component state、reconciliation、第三方 dependency 或 Runtime view-id 分支。

## 2026-08-05 — task001 — Add

- Files: `pr_faq_widget_renderer_20260805.md`、`spec_widget_renderer_20260805.md`、`adr_widget_renderer_20260805.md`、`plan_widget_renderer_20260805.md`、`task_widget_renderer_20260805.md`、`change_widget_renderer_20260805.md`、`.codex/plans/widget-renderer-backend.md`、`.phrase/docs/CHANGE.md`。
- Behavior: 无产品行为变化；建立由已批准 tech reference 驱动的 Widget renderer 实施阶段。
- Scope: stable key、原生 primitive、两阶段 layout、Dashboard 迁移、Stream fixture/measurement；明确排除第三方依赖与 Search/Table/Kanban renderer 改写。
- Verification: 实施前 `./test/run-tests.sh view-runtime view` 为 33/33；文档完成后运行 `git diff --check`。
- Risk: predecessor Runtime phase 的 commit/push hands-on gate 已于 2026-08-05 获得用户批准。
