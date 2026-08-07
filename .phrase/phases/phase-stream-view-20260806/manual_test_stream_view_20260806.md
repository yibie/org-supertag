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

## Verified Workflow

1. 公共 `supertag-view-stream` 建立一个 Runtime main buffer 和一个 title-only companion index；index 26 列，正文占剩余宽度。
2. `diary` 返回三个真实节点，不包含 `diaryx`；header-line 显示 `#diary`、`3 nodes`、`split`。
3. 完整 Org paragraph、table、quote 与 `#happy` 可见；图形 frame 中 SVG `display` property 只覆盖 tag。
4. `n` 从首节点移动到 `stream-node-2`；main/index point 与 selection overlay 都携带同一稳定 node ID。
5. `s` 删除 companion 后切回 split 能重建；Runtime main instance 不变。
6. `e` 打开真实 indirect/narrow Org buffer，child heading 不在 restriction 内；编辑进入 base buffer，不自动写盘，`C-c C-c` 返回并刷新。
7. source sync 保留原 `:created-at`，重开 Stream 后节点仍在创建顺序中的第二位。
8. `v` 通过公开 node-ID 入口打开真实 Node View side window；关闭后 Stream 仍在。
9. `q` 后 main/index 均释放，`:store-changed` subscriber 从 1 回到 0；随后重开用于最终截图。

## Performance Record

固定多段 Org 内容，隔离 Store，测量 Runtime open/refresh：

| Nodes | Open after native mode warm-up | Refresh |
|---:|---:|---:|
| 100 | 0.0077s | 0.0075s |
| 500 | 0.0381s | 0.0385s |
| 1000 | 0.0757s | 0.0957s |

独立 `emacs -Q` 第一次初始化 Org 派生 mode 有约 0.388s 固定成本，0 节点同样存在；它不是 Stream 内容规模成本。500-node 内容渲染与 refresh 均低于 0.2s 门槛，因此本阶段不增加分页或虚拟化。

## Automated Quality Gates

- Full ERT: 392/392 pass。
- Focused Stream + Node workflow: 9/9 pass。
- `supertag-view-stream.el`: `byte-compile-error-on-warn=t` pass；checkdoc pass。
- 本次修改的 `supertag-db-add-with-hash` 与 `supertag-view-node-open`: 单函数 byte compile/checkdoc pass。
- `check-parens`: 所有修改的 Elisp 与测试 pass。
- `git diff --check`: pass；repo-local `.elc`: zero。
- `package-lint`: 当前隔离环境未安装，未新增依赖进行补装。
- Whole-file strict compile limitation: Emacs 31 在到达本次改动前即被既有 docstring 问题阻断（`supertag-services-sync.el:63`、`supertag-view-node.el:36`、`org-supertag.el:56`）；未把邻近清理扩大进本 phase。

## User Hands-on Gate

在用户日常 Emacs 中执行 `M-x supertag-view-stream`，选择一个含 `/` 后代且至少三个节点的真实 tag，确认阅读密度、`s`、`n`/`p`、`e`、`v` 与 `q`。用户明确回复通过前，task006、issue032、phase、commit/push 均保持未完成。
