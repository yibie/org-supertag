# tech_refer_stream_view_20260806

## Existing Seams

- Query: `supertag-view-api-nodes-by-tag TAG t` 已使用 `supertag-tag-path-descendant-p`，无需新查询层。
- Lifecycle: `supertag-view-register/open/refresh` 已拥有 state、subscription、selection 与 cleanup。
- Rendering: `supertag-view-widget--render-tree` 已提供完整重绘、稳定 key、原生 button 和最终 buffer materialization。
- Source: node plist 已含 `:title`、`:file`、`:level`、`:created-at`；标题投影无需读取或渲染正文。
- Edit: `supertag-node--goto-location` 能在源 buffer 验证 ID；Org indirect buffer 能共享源文本和 undo。
- Fields: Node View 已拥有 tag/field/value 编辑。

## Proposed Shape

Stream 注册为普通、不可在 Developer View picker 中选择的 Runtime Adapter；公开命令只负责读 tag 并打开该 tag 的独立主 buffer。

主 buffer 通过现有 Widget Renderer 完整重绘。每个 node 只有一个稳定 key（node ID）和一个 title text widget。Stream mode 派生自 `org-mode`，保持 buffer read-only，并只提供 `n`/`p`/`e`/`v`/`g`/`q`。

完整正文继续由源 Org buffer 拥有。`e` 直接复用既有 indirect/narrow 编辑入口，因此单列标题流不需要 index、button、layout 状态、第二个 buffer 或新的详情展开状态。

## Ownership

| State | Owner |
| --- | --- |
| tag、nodes | Runtime input/state |
| title projection | Stream Widget Renderer |
| source edits/undo/save | base Org buffer |
| current node identity | node-ID text property + point |
| selection visuals | ephemeral overlays |
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

- **保留 companion index**：标题流本身已经是索引；第二个标题投影只增加下划线、窗口和同步状态。
- **增加正文展开状态**：`e` 已能打开完整源节点，另一个详情投影会复制已有能力。
- **在 Stream buffer 直接编辑派生正文**：需要双向同步、冲突和保存语义，破坏 Org 单一事实来源。
- **新增 Widget type/Framework hook**：现有 text/stack/key 足够。
- **虚拟滚动/分页**：标题投影没有测量到需要这类状态。

## Performance Gate

记录 100/500/1000 titles 的 initial render 与 refresh。若 500-node 首帧或 refresh 明显超过 0.2 秒，再以测量结果单独设计 lazy materialization，不在当前实现预埋抽象。

## Decision

Stream 是一个单 buffer Runtime Adapter。Framework 不改；Widget Renderer 只绘制带 node key 的标题；Org 源 buffer 继续拥有完整正文和编辑。这是当前最少的状态和失效路径。
