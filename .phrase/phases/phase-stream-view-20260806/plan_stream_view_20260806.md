# plan_stream_view_20260806

## Milestones

1. Phase/behavior baseline。
2. Query/state + full-body Widget rendering。
3. Stable navigation + split/plain companion presentation。
4. Source indirect/narrow edit + public Node View entry。
5. Subscription/cleanup + empty/error boundaries。
6. Documentation、performance、full/static/graphical verification。
7. task012：删除 companion/layout/full-body 投影，收敛为单列标题流。
8. task013：恢复列表自然滚动，并补齐可确认/取消的展开编辑会话。

## Implementation Order

1. 新增 public-path focused ERT 与 runner filter，先得到预期失败。
2. 新增 `supertag-view-stream.el` 的 data-only state builder 和 Runtime registration。
3. 以现有 Widget Renderer 完成正文 block，不改 Framework。
4. 完成 index buffer、window arrangement、selection overlay 与 thin commands。
5. 完成 source indirect edit 和 Node View public node-ID entry。
6. 完成 subscription/cleanup、文档和真实 `emacs -Q` smoke。

## task012 Cleanup Plan

1. 先把 focused ERT 锁定为单列标题、无 body/index/button/`s`，并证明旧实现失败。
2. 删除 index mode/window、layout input/state/toggle、full-body/tag widgets，只保留稳定 title key、导航与既有 `e`/`v`。
3. 更新当前产品文档；历史 change/task 记录保留原貌并由 task012 标注 superseded。
4. 运行 focused/full/static gates，确认无 index symbol、无 repo-local `.elc`，再提交并推送。

## task013 Correction Plan

1. 先用 public Stream ERT 复现可见标题被强制置顶、折叠正文未展开和 `C-c C-k` 缺失。
2. 删除 selection 对 window start 的写入；edit 入口展开 narrow 内容并保存取消快照。
3. `C-c C-c` 沿用既有同步确认；`C-c C-k` 恢复快照并复用同一关闭/窗口恢复路径。
4. 更新当前交互文档，运行 focused、full、compile/checkdoc/check-parens/diff gates。

## Quality Gates

```sh
./test/run-tests.sh view-stream view-runtime view
./test/run-tests.sh all
git diff HEAD
git diff --check
```

另需 target Elisp zero-warning byte compile/checkdoc、repo-local `.elc` zero、无新增 dependency、100/500/1000-node measurement 与图形 `Emacs.app -Q` hands-on。

## Risks

- title face 被 selection 覆盖：overlay face 只提供背景并保留底层 title face。
- indirect edit 暴露 child node：restriction end 使用下一个 heading，而非整个 subtree end。
- source ID/location stale：创建 indirect buffer 前验证 file 和 ID；失败不遗留 buffer/window。
- 窗口恢复破坏用户布局：打开前保存 window configuration；只有显式 `q` 恢复。
- abort 覆盖编辑前内容：快照只覆盖当前 narrow 节点，并恢复进入编辑前的 modified 状态；不写 Store、不自动保存。
- 代码膨胀：不新增 Framework API、Widget type、详情状态或替代索引；实现只保留主 Adapter 和 edit boundary。

## Rollback

删除 `supertag-view-stream.el`、对应 require、runner/test/docs 即可；Store、Runtime 和持久化格式无迁移。
