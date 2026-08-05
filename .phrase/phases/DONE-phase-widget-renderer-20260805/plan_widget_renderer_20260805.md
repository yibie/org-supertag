# plan_widget_renderer_20260805

## Milestones

1. Phase/behavior baseline。
2. Stable keyed selection。
3. Native button/link/editable-field primitives。
4. Placeholder measure → final-buffer materialization。
5. Three Dashboard migration and deletion test。
6. Stream-shaped interaction and 100/500/1000-node measurements。
7. Full/static/graphical verification and approval gate。

## Execution Rules

- 每项用户可见行为先写会失败的公共路径 ERT。
- Runtime 不增加按 View 类型分支；Widget 特性全部留在 DSL Adapter implementation。
- 不同时修改 Search/Table/Kanban renderer。
- 不创建兼容 VUI 的 component interface。
- Dashboard 迁移必须删除旧 render function，不能长期保留双 renderer。
- 不在用户图形实机批准前 commit/push；该门禁已于 2026-08-05 满足。

## Quality Gates

```sh
./test/run-tests.sh view-runtime view
./test/run-tests.sh all
git diff --check
```

另需 target Elisp zero-warning byte compile/checkdoc、repo-local `.elc` zero、无新增 external dependency、100/500/1000-node measurements 与图形 `Emacs.app -Q` smoke。

## Risks

- Text property 在 split/pad/concat 中丢失：用真实 button/widget-at 回归验证最终 buffer，不只测中间字符串。
- Editable field refresh 触发 stale change hooks：erase 前清除 Widget bookkeeping并抑制 modification hooks。
- Field materialization 改变 layout width：field 使用最终 placeholder 的显式宽度，并测试 CJK/长值。
- Framework 膨胀：只新增稳定 key、三个 primitive 和一个 materialization pass；Dashboard deletion test 必须证明净收益。
- Complete refresh scale：先记录测量，不在数据前写 diff engine。

## Dependencies

- Platform only: Emacs 29.1+ `button.el`, `widget.el`, `wid-edit.el`。
- External packages: none。
