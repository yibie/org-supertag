# spec_stream_view_20260806

## Summary

Stream View 是按标签连续阅读完整节点正文的独立 View。它通过现有 View Runtime 管理主 buffer 生命周期，通过现有 Widget Renderer 生成带稳定 node key 的正文流；默认分栏由一个只读索引 companion buffer 和主 Stream buffer 组成，plain 模式只保留主 buffer。

## Goals

- `M-x supertag-view-stream` 读取一个 tag，并包含通过 `:extends` 递归继承它的所有后代。
- 节点按 `:created-at` 升序排列；缺失时间时使用稳定 node ID 排序。
- 主 Stream 显示完整 `:content`、无 Org 星号的较大标题和完整 `#tag` token。
- 默认 split：左侧紧凑标题索引、右侧正文流；`s` 切换 plain/split。
- `n`/`p` 在节点间导航，并同步主视图、索引、窗口位置和轻微高亮。
- refresh 使用稳定 node ID 恢复位置；节点消失时回退到第一个节点。
- `e` 打开源 Org 节点的 indirect/narrow 编辑 buffer；编辑作用于源 buffer，不自动保存。
- `C-c C-c` 结束 narrow 编辑、返回原窗口布局并刷新 Stream。
- `v` 将当前 node ID 交给 Node View 修改 tag、field 和 field value。
- Store 相关变更自动刷新；退出或 kill 不残留 subscription、companion buffer、window 或 hook。

## Non-goals

- 不实现 query builder、关键词搜索、字段内联编辑或节点删除/移动。
- 不实现虚拟滚动、分页、增量 reconciliation 或正文截断。
- 不修改 View Runtime/Widget Renderer contract，不新增 View 类型分支。
- 不新增第三方依赖或持久化布局配置。
- 不改 Search、Table、Kanban、Node、Schema View 或源数据格式。
- 不把 companion 索引注册成第二个 Runtime View。

## Data Contract

Stream state 是数据 plist：

```elisp
(:tag TAG
 :layout split-or-plain
 :nodes (NODE ...))
```

- `TAG` 和 `LAYOUT` 来自 Runtime input。
- `NODE` 是 View API 返回的 Store node plist；renderer 只读。
- 主 buffer 的 `supertag-widget-key` 与 `supertag-entity-id` 都使用 node ID。
- companion index 只保存 node ID/title 投影与主 buffer 引用，不复制正文或字段。
- Org 源 buffer 是正文编辑的唯一事实来源。
- `:created-at` 是 Store 的不可变创建元数据；source-backed upsert 不得重置它。
- `:extends` 是标签层级的唯一事实来源；斜杠只可出现在只读 display path，不构成 Store 后代关系。

## User Flows

### Flow A：打开标签流

1. 用户执行 `M-x supertag-view-stream` 并选择 tag。
2. Adapter 使用 `supertag-view-api-nodes-by-tag TAG t` 获取精确 tag 与传递 `:extends` 后代。
3. Runtime 创建主 buffer；Widget Renderer 生成完整正文流。
4. presentation boundary 在当前主窗口左侧建立 index window，主 Stream 保持在右侧。
5. header-line 只显示 `#tag`、node count 与 split/plain 状态。

### Flow B：导航与刷新

1. `n`/`p` 读取当前 node key 并移动到相邻稳定 node ID。
2. 主 Stream 和 index 用仅含背景的 selection face 高亮同一 ID。
3. Runtime refresh 按 node ID/offset capture → rebuild → render → restore。
4. restore 后重建 index 投影；node 消失时落到首个可用节点。

### Flow C：切换布局

1. 用户按 `s`。
2. Adapter 只修改当前 Runtime input 的 `:layout`。
3. plain 删除 companion window/buffer；split 重新创建左侧 index。
4. 主 buffer、Runtime instance 和当前 node ID 不变。

### Flow D：编辑正文

1. 用户在当前节点按 `e`。
2. Adapter 根据 node ID 打开源文件、定位 ID，并建立 indirect buffer。
3. narrow 范围从当前 heading 到下一个 heading；不把 child node 暴露为当前节点正文。
4. 用户使用正常 Org 编辑/undo；文件不自动保存。
5. `C-c C-c` 关闭 indirect buffer、恢复原窗口配置并刷新 Stream。
6. 同步保留原 `:created-at`，因此正文编辑不会改变 Stream 排序。

### Flow E：编辑字段

1. 用户在当前节点按 `v`。
2. Stream 调用 Node View 的公开 node-ID 入口。
3. Node View 继续拥有 field/tag 编辑语义；Store 事件刷新 Stream。

## Edge Cases

- `diary` 匹配 `diary` 以及 `happy :extends diary` 等后代；既不匹配 `diaryx`，也不把平面 ID `diary/legacy` 当作后代。
- 无节点时显示明确空状态；`n`/`p`/`e`/`v` 给出 `user-error`。
- node 无 `:content` 时显示空正文；无 title 时显示 `Untitled`。
- node 无文件、文件不存在或 ID 无法定位时，`e` 在创建 indirect buffer 前报错。
- 当前窗口太窄不能 split 时保持 plain 并给出消息，不破坏现有窗口布局。
- refresh 时 companion/index 已被用户 kill，主 Stream 继续可用，并在下次切换 split 时重建。
- kill main buffer 必须清理 index；重复 cleanup 必须幂等。

## Compatibility Contract

- `supertag-view-open`、`supertag-view-refresh` 和 Runtime instance 结构不变。
- Widget Renderer 的注册表、primitive、key capture/restore 与 mode contract 不变。
- `supertag-view-node` 原交互命令保持行为；新增公开 node-ID 入口由原命令与 Stream 共用。
- `org-supertag.el` 只新增 Stream 模块 require；无 load-time window/buffer 副作用。

## Acceptance Criteria

- 公共 Stream 命令通过真实 Runtime 建立一个主 instance。
- tag descendant query 使用传递 `:extends` 语义，并有 `diaryx` 与平面斜杠 ID 反例。
- renderer 不显示 file path 或前导 Org 星号；title face 高于正文；完整 Org 内容与 `#tag` token 存在。
- split/plain、index click、`n`/`p`、refresh selection 与 missing-node fallback 通过工作流 ERT。
- narrow 编辑测试证明 child heading 不在 restriction 内，修改落到 base buffer，且不自动保存。
- narrow 编辑后的同步保留原 `:created-at`。
- `v` 通过 Node View 的公开 node-ID 入口。
- 重复 open/refresh/toggle/quit 后无多余 subscriber、index buffer 或 live window。
- focused/full ERT、byte compile、checkdoc、`git diff --check`、repo-local `.elc` zero 通过。
- 真实图形 `emacs -Q` 完成 split/plain、导航、SVG tag、Org 正文、narrow 编辑、Node View 与 teardown hands-on。
