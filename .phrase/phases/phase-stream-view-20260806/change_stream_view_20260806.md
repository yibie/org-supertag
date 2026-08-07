# change_stream_view_20260806

## 2026-08-07 — task011 / issue035 — Modify

- Files: `supertag-view-stream.el`、`test/test-view-stream.el`、Stream spec/task/issue/manual records。
- Behavior: 切换 Stream tag 时撤下上一 tag 的 companion index window，只显示当前 index + main；上一 main buffer 与 Runtime instance 保持存活，切回时重建 index。
- Simplification: 公共命令复用既有 `supertag-view-stream--resolve-main-buffer` 与 `--remove-index`，没有新增窗口注册表或全局扫描。
- Verification: 截图场景回归先因旧 diary index 仍可见而失败；修复后 Stream 8/8、相关 48/48、full ERT 400/400；byte compile/checkdoc/check-parens/diff-check 通过，repo-local `.elc` zero。

## 2026-08-07 — task010 — Add / Confirm

- Files: `test/test-view-stream.el`、Stream spec/task/manual records。
- Behavior: 不同 tag 使用各自的 `*Supertag Stream: <tag>*` main buffer，内容与 Runtime input 隔离；重复打开同一 tag 复用原 buffer。
- Simplification: 现有 `:buffer-name-fn` 已以 tag 建立正确 identity，因此不修改生产代码、不新增实例注册表。
- Verification: 连续打开 `diary`、`work`、`diary` 的公共命令回归直接通过；Stream 8/8、相关 48/48、full ERT 400/400；test `check-parens`、diff-check 通过，repo-local `.elc` zero。

## 2026-08-07 — task009 / issue034 — Modify

- Files: `supertag-view-stream.el`、`test/test-view-stream.el`、Stream spec/task/issue/manual records。
- Behavior: 双列索引点击节点后，主窗口 point/start 同步到节点起点，标题与正文开头立即可见；`n`/`p` 复用同一选择规则。
- Simplification: 修复既有共享 `supertag-view-stream--select-node`，没有新增滚动状态、helper 或 Runtime 分支。
- Verification: 回归先以 main window point `1`、目标位置 `1133` 失败；修复后 Stream 7/7、相关 47/47、full ERT 399/399；byte compile/checkdoc/check-parens/diff-check 通过，repo-local `.elc` zero。

## 2026-08-07 — task008 / issue009 — Modify

- Files: shared scan/View API、Schema/View/Table selectors、Tag write boundaries、sync、nested/Stream tests and docs；真实 notes Store 与两个源 Org 文件。
- Behavior: Stream 的父 Tag 现在聚合所有传递 `:extends` 子标签节点；Schema 只显示真实 Tag 缩进树；新建、重命名、同步和批量导入都不能产生未知斜杠 Tag ID。
- Data: `Apple/Shortcut/语言` 与 `coding/日志` 已迁移为真实叶子 ID + `:extends` 链；迁移前创建唯一数据库恢复快照。
- Verification: focused ERT 84/84；full ERT 399/399；14 files byte compile；16 files `check-parens`；真实 `diary` 由 exact 2 扩展至 256 nodes，Store 斜杠 ID 2→0。

## 2026-08-06 — task002–task005 — Add / Modify

- Files: `supertag-view-stream.el`、`supertag-view-node.el`、`org-supertag.el`、`test/test-view-stream.el`、`test/run-tests.sh`、README/CHANGELOG/View guide。
- Behavior: 新增 `M-x supertag-view-stream`；精确 tag 与 `/` 后代按创建时间完整渲染，默认 26 列标题索引 + 主正文，支持 `n`/`p`、index click、`s`、`e`、`v`、`g`、`q`。
- Simplification: 主 buffer 是现有 View Runtime 的普通 Adapter，正文直接复用 Widget Renderer；companion index 只持有 title/node ID，不新增 Framework contract、第二 Runtime、第三方 widget 依赖或虚拟化。
- Verification: focused workflow ERT 9/9；独立图形 `emacs -Q` 验证 split/plain、SVG、Org 正文、稳定选择、narrow、Node View 与 teardown；visual-verdict 94/100 pass。

## 2026-08-06 — task007 / issue032 — Modify

- Files: `supertag-services-sync.el`、`test/test-view-stream.el`、`issue_stream_edit_resets_created_at_20260806.md`、spec/task/issue indexes。
- Behavior: source-backed upsert 现在保留已有节点的 `:created-at`；Stream narrow 编辑后不再因为同步而改变创建时间排序。
- Simplification: 在所有文件同步与 point sync 共用的 `supertag-db-add-with-hash` 固化一个不可变元数据规则，没有在 Stream renderer/window 层补偿排序。
- Verification: public Stream edit ERT 先以重置后的时间失败，再通过；`view-stream sync-worker node` 25/25。

## 2026-08-06 — task001 — Add

- Files: `pr_faq_stream_view_20260806.md`、`spec_stream_view_20260806.md`、`tech_refer_stream_view_20260806.md`、`plan_stream_view_20260806.md`、`task_stream_view_20260806.md`、`change_stream_view_20260806.md`、`.phrase/docs/CHANGE.md`。
- Behavior: 无产品行为变化；用户批准默认 split、plain toggle、descendant query、source narrow edit 与 Node View field boundary。
- Decision: Stream 作为普通 Runtime Adapter，Widget Renderer 完整重绘，split 由 Adapter companion 管理；不修改 Framework contract，不新增依赖或虚拟化层。
- Verification: 文档契约互相引用一致；`git diff --check`。
