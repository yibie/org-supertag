# tech_refer_stream_view_20260806

## Existing Seams

- Query: `supertag-view-api-nodes-by-tag TAG t` 已使用 `supertag-tag-path-descendant-p`，无需新查询层。
- Lifecycle: `supertag-view-register/open/refresh` 已拥有 state、subscription、selection 与 cleanup。
- Rendering: `supertag-view-widget--render-tree` 已提供完整重绘、稳定 key、原生 button 和最终 buffer materialization。
- Source: node plist 已含 `:title`、`:tags`、`:content`、`:file`、`:level`、`:created-at`；正文展示不必重新读文件。
- Edit: `supertag-node--goto-location` 能在源 buffer 验证 ID；Org indirect buffer 能共享源文本和 undo。
- Fields: Node View 已拥有 tag/field/value 编辑。

## Proposed Shape

新增一个产品模块 `supertag-view-stream.el` 和一个 focused ERT 文件。Stream 注册为普通、不可在 Developer View picker 中选择的 Runtime Adapter；公开命令负责读 tag、打开主 buffer 并安排 split/plain presentation。

主正文通过现有 Widget Renderer 完整重绘。每个 node block 只有一个稳定 key（node ID），内容由 tags、title、body 和轻量 separator 组成。Stream mode 派生自 `org-mode` 以复用 Org font-lock 与现有 SVG tag minor mode，同时保持 buffer read-only 和 Stream 自己的薄命令 map。

split 不进入 Runtime。一个普通 `special-mode` companion index buffer 显示 title buttons，buffer-local 保存主 Stream buffer。主 Runtime 的 restore callback 在 refresh 完成后按最新 state 重建 index，并同步当前 node ID；因此 generic `supertag-view-refresh` 仍正确，不需要 post-render hook。

## Ownership

| State | Owner |
| --- | --- |
| tag、layout、nodes | Runtime input/state |
| complete body text | Store node `:content` |
| source edits/undo/save | base Org buffer |
| current node identity | node-ID text property + point |
| selection visuals | ephemeral overlays |
| index buffer/window | Stream Adapter cleanup |
| field/tag edits | Node View/ops |

## Node View Boundary

Node View 当前只有无参交互 toggle，内部已有 node-ID opening path。新增一个公开 `supertag-view-node-open NODE-ID`，让现有 toggle 和 Stream 共用；不复制 Node View registration/focus 逻辑，也不让 Stream 调用 Node View 私有函数。

## Sorting

有 `:created-at` 的节点按 `time-less-p` 升序；一个有时间、一个无时间时，有时间者在前；都无时间或时间相等时按 node ID 排序。排序只在 state build 中执行一次，不在 renderer 或 window sync 中重复。

## Narrow Editing

1. 从 Store node 获取 `:file`、`:level` 和 ID。
2. `find-file-noselect` 后通过现有 node location helper 验证真实位置。
3. 创建 indirect buffer，共享 base buffer 文本和 undo。
4. heading node narrow 到当前 heading 至下一个 heading；file node narrow 到整个文件。
5. 本地 edit minor mode 只提供 `C-c C-c` 返回；不自动保存，不复制内容。

## Rejected

- **扩 View Runtime 支持 multi-buffer View**：当前只有 Stream 的 presentation 需要 companion；会把产品布局污染生命周期核心。
- **把 index 注册为第二个 Runtime View**：产生重复 state/query/subscription，逻辑上仍是一个产品 View。
- **在 Stream buffer 直接编辑派生正文**：需要双向同步、冲突和保存语义，破坏 Org 单一事实来源。
- **从源文件重新解析正文用于展示**：Store 已有 `:content`，重复 IO/parse 只增加延迟和失效路径。
- **新增 Widget type/Framework hook**：现有 text/stack/separator/key 足够；真实缺口只在产品 Adapter。
- **虚拟滚动/分页**：没有真实 full-body 测量前不写优化层。

## Performance Gate

记录 100/500/1000 nodes、每个 node 固定多段 Org 内容的 initial render 与 refresh。MVP 先保留完整可搜索/copy 的 buffer；若 500-node 首帧或 refresh 明显超过 0.2 秒，再以测量结果单独设计 lazy materialization，不在当前实现预埋抽象。

## Decision

Stream 是一个 Runtime Adapter + 一个 companion index presentation。Framework 不改；Widget Renderer 继续完整重绘；Org 源 buffer 继续拥有编辑。这是当前最少的新状态和最少的失效路径。
