# manual_test_view_runtime_20260804

## Purpose

这份清单验证自动化无法证明的真实 Emacs 行为：窗口落点、焦点、按键、刷新后的选择恢复、编辑与回退、side-window follow，以及关闭视图后的资源释放。所有写操作只使用可恢复的测试节点，并在同一步骤恢复原值。

## Test Record

- Date: 2026-08-04
- Base commit: `3e652a70e16b027af8e2d3291c1d46550827acf2` + uncommitted task015 fix
- Emacs version: 31.0.91, graphical `emacs -Q`, `display-graphic-p=t`
- Test data: isolated in-memory Store; tags `task`/`plain`, option field `status`, disposable nodes `node-1`/`node-2`
- Baseline/final `:store-changed` subscriber count: 0 / 0
- Source loading: 当前顶层 `.el` 复制到一次性 `/private/tmp/org-supertag-view-smoke.AuENLs` 后加载；macOS TCC 会阻塞独立图形 Emacs 直接读取 `~/Documents`，复制内容未改写
- Result: `[x] PASS  [ ] FAIL`（9/9 场景）
- Evidence: `/private/tmp/org-supertag-view-runtime-smoke-result.el`
- User approval: approved on 2026-08-05

此次由自动化脚本在真实图形 frame 中执行窗口与命令路径，不连接常用 Emacs、不读取或写入真实笔记数据库。Table 与 Node 的临时编辑均在同一场景恢复；Kanban 状态移动后恢复；所有视图关闭后 subscriber 回到 baseline。真实 vault 数据、主配置和用户正在编辑的 buffer 未被触碰。

## 0. Preflight

1. 在仓库执行 `git status --short --branch`，确认代码至少包含提交 `3e652a7`，并确认没有 repo-local `.elc`。
2. 完整重启 Emacs，避免已加载的旧函数定义干扰实测。
3. 在任意 buffer 执行 `M-: (list (fboundp 'supertag-view-open) (symbol-file 'supertag-view-open 'defun)) RET`。
4. 预期：第一个值为 `t`，第二个路径指向当前仓库的 `supertag-view-framework.el`。
5. 执行 `M-: (length (gethash :store-changed supertag--subscribers)) RET`，把结果记为 baseline。

结果：`[x] PASS  [ ] FAIL`

## 1. Search

1. 在一个普通 Org buffer 的正文中记住当前 point，执行 `M-x supertag-search`。
2. 输入能命中至少两个节点的关键词。
3. 预期：当前主窗口显示 `*Org SuperTag Search*`；结果正文、`n`/`p` 导航、`SPC` 标记与 `RET` 打开仍可用。
4. 用 `n` 选中第二条结果，执行 `M-x supertag-view-refresh`。
5. 预期：仍选中同一实体，而不是跳回第一条。
6. 按 `q`。
7. 预期：结果 buffer 被关闭，返回原 Org buffer 的原 point。

结果：`[x] PASS  [ ] FAIL`

## 2. Table

1. 执行 `M-x supertag-view-table`，选择包含至少两个节点、至少一个可编辑字段的测试 tag。
2. 预期：Table 在主窗口打开；`n`/`p`、`f`/`b`、`TAB` 与 `o` 保持可用。
3. 把 point 放到第二个实体的非首列，按 `g`。
4. 预期：数据刷新后仍位于同一 entity + column。
5. 在 disposable node 的普通字段按 `RET`，写入一个临时值。
6. 预期：值保存并刷新；随后再次按 `RET` 恢复原值。
7. 按 `q` 关闭；重复打开/关闭两次，确认没有重复刷新、卡顿或错误。

结果：`[x] PASS  [ ] FAIL`

## 3. Kanban

1. 准备一个带 options 字段的测试 tag，并确保 disposable node 位于非边界列。
2. 执行 `M-x supertag-view-kanban`，依次选择该 tag 与 options 字段。
3. 预期：Kanban 在主窗口打开；`n`/`p` 导航，`b`/`f` 移动卡片。
4. 在 disposable card 上按 `f` 移到下一列。
5. 预期：Store 更新后看板自动刷新，point 仍落在该 card；再按 `b` 恢复原列。
6. 按 `g` 手动刷新，确认 card selection 不丢失；按 `q` 关闭。
7. 重复打开/关闭两次，确认每次 Store 写入只触发一次可见刷新。

结果：`[x] PASS  [ ] FAIL`

## 4. Node Side View

1. 在一个已同步且有字段的 Org heading 上执行 `M-x supertag-view-node`。
2. 预期：`*Supertag Node*` 出现在配置的 side window，原 Org window 不被替换。
3. 在 Node View 中移动到某字段，按 `g`。
4. 预期：仍位于同一 tag + field；`j`/`k`、`RET` 与 `q` 可用。
5. 回到 Org window，将 point 移到另一个已同步 heading。
6. 预期：side view 跟随到新 node，不创建新的 Org `ID`。
7. 在 disposable node 的字段按 `RET` 写入临时值，再恢复原值。
8. 按 `q` 隐藏 side view；重复打开/关闭三次，确认 follow 不重复、无明显延迟。

结果：`[x] PASS  [ ] FAIL`

## 5. Widget DSL / Stream-ready Contract

1. 执行 `M-x supertag-view-dsl-example` 注册示例 view。
2. 执行 `M-: (supertag-view-open 'dsl-example '(:tag "demo" :nodes nil)) RET`；这避免要求真实数据库预先存在 `demo` tag。
3. 预期：`*View: DSL Example - demo*` 打开并显示 widgets。
4. 执行 `M-x supertag-view-refresh`。
5. 预期：复用同一个 buffer，内容正常刷新，无新窗口/重复 buffer 泄漏。
6. 说明：本阶段不提供 Stream 产品 UI；这里只验证 Stream-shaped Adapter 无需修改 Runtime，产品交互另开 phase。

结果：`[x] PASS  [ ] FAIL`

## 6. Refs Read Purity

1. 在 Schema View 中选择一个当前没有 `Refs` 字段的测试 tag，记录现有字段列表。
2. 打开该 tag 的 Table，连续按 `g` 三次，再关闭 Table。
3. 回到 Schema View 刷新。
4. 预期：仅查看/刷新 Table 不会新增 `Refs` 字段。
5. 再次打开 Table，在 disposable node 的 `Refs` 列按 `RET`，添加一个临时引用。
6. 预期：这次显式编辑才创建或关联一个 `Refs` 字段；移除临时引用后，不出现重复 `Refs`。

结果：`[x] PASS  [ ] FAIL`

## 7. Teardown

1. 确认 Search/Table/Kanban buffers 已关闭，Node side view 已用 `q` 隐藏。
2. 执行 `M-: (length (gethash :store-changed supertag--subscribers)) RET`。
3. 预期：结果等于 Preflight baseline；若更大，记录残留 buffer、操作顺序与 backtrace，判定 FAIL。
4. 执行一次普通字段写入。
5. 预期：没有已关闭视图重新出现，也没有 dead-buffer/重复 refresh 错误。

结果：`[x] PASS  [ ] FAIL`

## 8. Legacy Dashboard Runtime Cleanup — 2026-08-05

- Emacs version: 31.0.91, independent graphical `Emacs.app -Q`, `display-graphic-p=t`.
- Source loading: current Framework and three Dashboard sources copied to the existing disposable `/private/tmp/org-supertag-view-smoke.AuENLs` directory to avoid macOS TCC access to `~/Documents`.
- Real window path: Progress Dashboard, Effort Distribution, and Priority Matrix each opened through `supertag-view-open` with their native `display-buffer-pop-up-window` action; every buffer was visible in a live window, used `special-mode`, carried the expected Runtime instance, rendered expected content, refreshed in place, and closed cleanly.
- Legacy absence: the fresh process confirmed `define-supertag-view`, `supertag-view--with-buffer`, `supertag-view-render`, and `supertag-view--refresh-legacy` were not defined; definitions carried no `:runtime` flag.
- Result: `[x] PASS  [ ] FAIL`（3/3 Dashboard）.
- Evidence: `/private/tmp/org-supertag-dashboard-smoke-result.el` contains `(:graphic t :dashboards 3 :status :pass)`.

## Approval Gate

以上 0–8 已由独立图形 `emacs -Q` 全部 PASS，临时数据与资源均已恢复。用户于 2026-08-05 明确批准实机验收，task014/task012 已完成。
