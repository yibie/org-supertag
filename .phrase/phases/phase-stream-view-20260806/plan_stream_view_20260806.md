# plan_stream_view_20260806

## Milestones

1. Phase/behavior baseline。
2. Query/state + full-body Widget rendering。
3. Stable navigation + split/plain companion presentation。
4. Source indirect/narrow edit + public Node View entry。
5. Subscription/cleanup + empty/error boundaries。
6. Documentation、performance、full/static/graphical verification。

## Implementation Order

1. 新增 public-path focused ERT 与 runner filter，先得到预期失败。
2. 新增 `supertag-view-stream.el` 的 data-only state builder 和 Runtime registration。
3. 以现有 Widget Renderer 完成正文 block，不改 Framework。
4. 完成 index buffer、window arrangement、selection overlay 与 thin commands。
5. 完成 source indirect edit 和 Node View public node-ID entry。
6. 完成 subscription/cleanup、文档和真实 `emacs -Q` smoke。

## Quality Gates

```sh
./test/run-tests.sh view-stream view-runtime view
./test/run-tests.sh all
git diff HEAD
git diff --check
```

另需 target Elisp zero-warning byte compile/checkdoc、repo-local `.elc` zero、无新增 dependency、100/500/1000-node measurement 与图形 `Emacs.app -Q` hands-on。

## Risks

- split companion stale：generic Runtime restore 必须重建 index，不在 renderer 跨 buffer 写。
- Org syntax face 被 selection 覆盖：overlay face 只提供背景并保留底层 syntax face。
- indirect edit 暴露 child node：restriction end 使用下一个 heading，而非整个 subtree end。
- source ID/location stale：创建 indirect buffer 前验证 file 和 ID；失败不遗留 buffer/window。
- layout cleanup 破坏用户窗口：打开前保存 window configuration；只有显式 `q` 恢复，Runtime reopen cleanup 不擅自恢复。
- 代码膨胀：不新增 Framework API、Widget type、state struct 或 layout class；实现只保留主 Adapter、companion 和 edit boundary。

## Rollback

删除 `supertag-view-stream.el`、对应 require、runner/test/docs 即可；Store、Runtime 和持久化格式无迁移。
