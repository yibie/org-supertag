# spec_stream_view_20260806

## Summary

Stream View 是按标签浏览节点标题的独立 View。它通过现有 View Runtime 管理单个主 buffer，通过现有 Widget Renderer 生成带稳定 node key 的标题流；完整正文只在用户按 `e` 时通过源 Org 节点的 indirect/narrow buffer 显示和编辑。

## Goals

- `M-x supertag-view-stream` 读取一个 tag，并包含通过 `:extends` 递归继承它的所有后代。
- 节点按 `:created-at` 升序排列；缺失时间时使用稳定 node ID 排序。
- 主 Stream 只显示无 Org 星号的节点标题，不显示正文、文件路径、标签 token 或下划线 button。
- Stream 只有单列主 buffer，不创建 companion index，不提供 split/plain 与 `s` 切换。
- `n`/`p` 在标题间导航，并同步 point 和轻微高亮；可见标题沿用窗口的自然滚动位置，不强制置顶。
- refresh 使用稳定 node ID 恢复位置；节点消失时回退到第一个节点。
- `e` 打开并展开源 Org 节点的 indirect/narrow 编辑 buffer；标题和正文可见，编辑作用于源 buffer，不自动保存。
- `C-c C-c` 确认 narrow 编辑、返回原窗口布局并刷新 Stream；`C-c C-k` 恢复进入编辑前的文本并取消。
- `v` 将当前 node ID 交给 Node View 修改 tag、field 和 field value。
- Store 相关变更自动刷新；退出或 kill 不残留 subscription 或 hook。

## Non-goals

- 不实现 query builder、关键词搜索、字段内联编辑或节点删除/移动。
- 不实现详情展开、正文预览、虚拟滚动、分页或增量 reconciliation。
- 不修改 View Runtime/Widget Renderer contract，不新增 View 类型分支。
- 不新增第三方依赖或布局配置。
- 不改 Search、Table、Kanban、Node、Schema View 或源数据格式。
- 不保留 companion 索引或用另一个投影替代它。

## Data Contract

Stream state 是数据 plist：

```elisp
(:tag TAG
 :nodes (NODE ...))
```

- `TAG` 来自 Runtime input。
- `NODE` 是 View API 返回的 Store node plist；renderer 只读。
- 主 buffer 的 `supertag-widget-key` 与 `supertag-entity-id` 都使用 node ID。
- Stream renderer 只读取 node ID/title；Store 中的 `:content` 不进入标题投影。
- Org 源 buffer 是正文编辑的唯一事实来源。
- `:created-at` 是 Store 的不可变创建元数据；source-backed upsert 不得重置它。
- `:extends` 是标签层级的唯一事实来源；斜杠只可出现在只读 display path，不构成 Store 后代关系。

## User Flows

### Flow A：打开标签流

1. 用户执行 `M-x supertag-view-stream` 并选择 tag。
2. Adapter 使用 `supertag-view-api-nodes-by-tag TAG t` 获取精确 tag 与传递 `:extends` 后代。
3. Runtime 创建单列主 buffer；Widget Renderer 只生成按创建时间排序的标题流。
4. header-line 只显示 `#tag` 与 node count。
5. tag 是 Stream buffer identity：不同 tag 使用不同 main buffer；重复打开同一 tag 复用并刷新原 buffer。
6. 切换到另一个 tag 时保留前一个 main buffer，并显示所选 tag 对应的独立 main buffer。

### Flow B：导航与刷新

1. `n`/`p` 读取当前 node key 并移动到相邻稳定 node ID。
2. `n`/`p` 只移动窗口 point；目标已可见时保持当前 window start，需要时由 Emacs 原生滚动保证可见。
3. 主 Stream 用仅含背景的 selection face 高亮当前标题。
4. Runtime refresh 按 node ID/offset capture → rebuild → render → restore。
5. node 消失时落到首个可用标题。

### Flow C：查看与编辑完整节点

1. 用户在当前节点按 `e`。
2. Adapter 根据 node ID 打开源文件、定位 ID，并建立 indirect buffer。
3. narrow 范围从当前 heading 到下一个 heading；不把 child node 暴露为当前节点正文，并显式展开标题与正文。
4. 用户使用正常 Org 编辑/undo；文件不自动保存。
5. `C-c C-c` 确认修改，关闭 indirect buffer、恢复原窗口配置并刷新 Stream。
6. `C-c C-k` 恢复进入编辑前的 narrow 文本，关闭 indirect buffer，不触发 Store 同步。
7. 确认同步保留原 `:created-at`，因此正文编辑不会改变 Stream 排序。

### Flow D：编辑字段

1. 用户在当前节点按 `v`。
2. Stream 调用 Node View 的公开 node-ID 入口。
3. Node View 继续拥有 field/tag 编辑语义；Store 事件刷新 Stream。

## Edge Cases

- `diary` 匹配 `diary` 以及 `happy :extends diary` 等后代；既不匹配 `diaryx`，也不把平面 ID `diary/legacy` 当作后代。
- 无节点时显示明确空状态；`n`/`p`/`e`/`v` 给出 `user-error`。
- node 无 title 时显示 `Untitled`；正文是否为空不影响标题流。
- node 无文件、文件不存在或 ID 无法定位时，`e` 在创建 indirect buffer 前报错。
- 长标题流导航到不可见节点时必须同步实际 Stream window point，由 Emacs 原生滚动显示目标；已可见节点不能被强制移到窗口顶部。
- 编辑前源 buffer 已折叠时，edit buffer 仍展开当前标题与正文；取消恢复文本和进入编辑前的 modified 状态。
- 连续打开多个 tag 时，各 main buffer 独立存活，彼此 input、标题与 Runtime instance 不串线。

## Compatibility Contract

- `supertag-view-open`、`supertag-view-refresh` 和 Runtime instance 结构不变。
- Widget Renderer 的注册表、primitive、key capture/restore 与 mode contract 不变。
- `supertag-view-node` 原交互命令保持行为；新增公开 node-ID 入口由原命令与 Stream 共用。
- `org-supertag.el` 只新增 Stream 模块 require；无 load-time window/buffer 副作用。
- 既有 Stream buffer 可同时存活；打开另一个 tag 不覆盖或改写前一个 tag 的 Runtime instance。

## Acceptance Criteria

- 公共 Stream 命令通过真实 Runtime 建立一个主 instance。
- 不同 tag 的公共命令返回不同且内容隔离的 main buffer；重复打开同一 tag 返回原 buffer。
- tag 切换后显示当前 tag 的单列 main buffer；前一个 main buffer 仍可切回。
- tag descendant query 使用传递 `:extends` 语义，并有 `diaryx` 与平面斜杠 ID 反例。
- renderer 只显示标题，不显示正文、file path、tag token、前导 Org 星号、button 或下划线。
- `s` 未绑定；`n`/`p`、refresh selection 与 missing-node fallback 通过工作流 ERT。
- 导航到已可见标题时 window start 不变；跨出可见区时目标仍可见。
- narrow 编辑测试证明标题与正文已展开、child heading 不在 restriction 内，确认修改落到 base buffer 且不自动保存。
- `C-c C-k` 取消测试证明编辑文本未保留、Store 未同步、原 modified 状态恢复。
- narrow 编辑后的同步保留原 `:created-at`。
- `v` 通过 Node View 的公开 node-ID 入口。
- 重复 open/refresh/quit 后无多余 subscriber；不会创建 Stream Index buffer。
- focused/full ERT、byte compile、checkdoc、`git diff --check`、repo-local `.elc` zero 通过。
- 真实图形 `emacs -Q` 完成单列标题、导航、`e` 完整源节点、Node View 与 teardown hands-on。
