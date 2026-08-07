# Stream View 实机验收

## Test Record

- Date: 2026-08-06
- Emacs: 31.0.91，独立图形 `emacs -Q`，`display-graphic-p=t`
- Data: 隔离内存 Store、disposable Org 文件；`diary`、`happy :extends diary`、`day :extends private :extends diary` 与反例 `diaryx`
- Source loading: 当前顶层 `.el` 复制到 `/private/tmp/org-supertag-stream-smoke.JEUoOi`，不读取真实 vault 或用户配置
- Automated graphical result: `[x] PASS  [ ] FAIL`
- Evidence: `/private/tmp/org-supertag-stream-smoke.JEUoOi/result.el`、`stream-final.png`
- Visual verdict: 94/100，pass；记录于 `.omx/state/stream-view/ralph-progress.json`
- User hands-on approval: pending

## Historical Verified Workflow（2026-08-06，已由 task012 取代）

1. 公共 `supertag-view-stream` 建立一个 Runtime main buffer 和一个 title-only companion index；index 26 列，正文占剩余宽度。
2. `diary` 返回三个真实节点，不包含 `diaryx`；header-line 显示 `#diary`、`3 nodes`、`split`。
3. 完整 Org paragraph、table、quote 与 `#happy` 可见；图形 frame 中 SVG `display` property 只覆盖 tag。
4. `n` 从首节点移动到 `stream-node-2`；main/index point 与 selection overlay 都携带同一稳定 node ID。
5. `s` 删除 companion 后切回 split 能重建；Runtime main instance 不变。
6. `e` 打开真实 indirect/narrow Org buffer，child heading 不在 restriction 内；编辑进入 base buffer，不自动写盘，`C-c C-c` 返回并刷新。
7. source sync 保留原 `:created-at`，重开 Stream 后节点仍在创建顺序中的第二位。
8. `v` 通过公开 node-ID 入口打开真实 Node View side window；关闭后 Stream 仍在。
9. `q` 后 main/index 均释放，`:store-changed` subscriber 从 1 回到 0；随后重开用于最终截图。

## task012 Current Contract（2026-08-08）

1. `supertag-view-stream` 只建立一个 Runtime main buffer，不创建 `*Supertag Stream Index: TAG*`。
2. main buffer 每行只显示节点标题；不显示正文、tag token、文件路径、Org 星号或带下划线的 button。
3. header-line 只显示 `#tag` 与 node count；Runtime input/state 不含 `:layout`。
4. `n`/`p` 按稳定 node ID 在标题间导航并把目标标题置于窗口顶部；`s` 未绑定。
5. `e` 继续打开完整源 Org 节点的 indirect/narrow buffer，child heading 不进入当前节点 restriction，且不自动保存。
6. `v`、`g`、`q` 与 Store subscription cleanup 保持原行为。
7. 不同 tag 使用不同 main buffer；重开同一 tag 复用原 buffer，不产生 companion window。

## Historical Full-body Performance Record

固定多段 Org 内容，隔离 Store，测量 Runtime open/refresh：

| Nodes | Open after native mode warm-up | Refresh |
|---:|---:|---:|
| 100 | 0.0077s | 0.0075s |
| 500 | 0.0381s | 0.0385s |
| 1000 | 0.0757s | 0.0957s |

独立 `emacs -Q` 第一次初始化 Org 派生 mode 有约 0.388s 固定成本，0 节点同样存在；它不是 Stream 内容规模成本。500-node 内容渲染与 refresh 均低于 0.2s 门槛，因此本阶段不增加分页或虚拟化。

## Automated Quality Gates

- task012 regression-first: 旧实现 Stream 4/8 pass、4/8 按预期失败（layout、body/tag、companion）；删除后 Stream 8/8 pass。
- task012 isolated full ERT: 临时 detached worktree 只应用 Stream code/test diff，400/400 pass。
- 当前脏工作区 full ERT: 398/400；两项失败来自用户未提交的 Dashboard 实验要求缺失的 `textui`，Stream 与其余 398 项通过，本次未修改该用户工作。
- task012 static: `supertag-view-stream.el` strict byte compile、checkdoc、code/test check-parens、`git diff --check` pass；repo-local `.elc` zero。
- Full ERT: 400/400 pass。
- Focused Stream + Node workflow: 9/9 pass。
- task009 index click regression: Stream 7/7、Stream + Runtime + View 47/47 pass；长正文后的目标节点同步 main window point/start。
- task010 per-tag buffer identity: Stream 8/8、Stream + Runtime + View 48/48 pass；`diary`/`work` 使用不同 buffer，重开 `diary` 复用原 buffer，input/正文互不串流。
- task011 active companion lifecycle: Stream 8/8、相关 48/48、full 400/400 pass；切换 tag 后仅当前 index window 可见，上一 main buffer 保持 live。
- `supertag-view-stream.el`: `byte-compile-error-on-warn=t` pass；checkdoc pass。
- 本次修改的 `supertag-db-add-with-hash` 与 `supertag-view-node-open`: 单函数 byte compile/checkdoc pass。
- `check-parens`: 所有修改的 Elisp 与测试 pass。
- `git diff --check`: pass；repo-local `.elc`: zero。
- `package-lint`: 当前隔离环境未安装，未新增依赖进行补装。
- Whole-file strict compile limitation: Emacs 31 在到达本次改动前即被既有 docstring 问题阻断（`supertag-services-sync.el:63`、`supertag-view-node.el:36`、`org-supertag.el:56`）；未把邻近清理扩大进本 phase。

## User Hands-on Gate

在用户日常 Emacs 中执行 `M-x supertag-view-stream`，选择一个含 `:extends` 后代且至少三个节点的真实 tag，确认只有单列标题、无下划线/正文/index，`n`/`p` 可导航，`e` 显示完整源节点，`v` 与 `q` 正常。用户明确回复通过前，task006、issue032 与 phase 均保持未完成。

issue034 与 issue035 已由用户批准的 task012 设计取代：companion index 及其 window/selection lifecycle 已删除，不再需要旧双列实机检查。
