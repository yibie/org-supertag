# PR/FAQ — Supertag Stream View

Status: Revised design approved by user on 2026-08-08
Date: 2026-08-06
Predecessors: `DONE-phase-view-runtime-20260804`、`DONE-phase-widget-renderer-20260805`

## Press Release

### Org-Supertag 用 Stream View 把一个标签变成按时间排列的标题流

**在一个清晰的单列列表中浏览父标签及其所有后代节点，按需打开完整 Org 节点。**

**2026-08-06 —** Org-Supertag 将推出独立的 Stream View。用户选择一个标签后，可以按创建顺序浏览带有该标签或任一 `:extends` 后代标签的节点标题；需要正文时，按 `e` 直接打开完整源节点。

当前 Search、Table 与 Kanban 擅长查找、比较和操作结构化数据，但缺少一个按时间快速扫过主题节点的入口。源文件把相关内容分散在不同位置；`:extends` 已经表达标签层级，却还没有一个清晰界面把整棵标签子树聚合成标题流。

Stream View 把标签当作浏览入口。一个单列主 buffer 只显示可识别的节点标题，不显示文件路径、Org 星号、正文、标签 token 或带下划线的 button。当前标题仅使用与 `supertag-search` 一致的轻微背景高亮。

“我们已经统一了 View Runtime 和 Widget Renderer，现在最重要的是减少重复界面。”Org-Supertag 项目负责人表示，“标题流本身就是索引；完整内容交给源 Org 节点，字段管理留给 Node View。”

用户执行 `M-x supertag-view-stream` 并选择标签。结果包含直接带有该标签的节点，以及所有通过 `:extends` 递归继承它的后代标签节点。`n`/`p` 在标题间导航。按 `e` 时，Stream View 使用源 Org 节点的 narrow/indirect 体验显示完整节点；完成后返回原来的 Stream 位置。需要修改字段或字段值时，用户从当前节点进入 `view-node`。

“以前索引和正文挤在一起，标题还带着一排下划线。现在我先扫清楚的标题，想看哪条就按 `e`；字段仍使用熟悉的 Node View。”一位 Emacs 用户表示。

用户可通过 `M-x supertag-view-stream` 开始使用；本阶段不改变现有 Search、Table、Kanban、Node 或源 Org 文件格式。

## FAQ

### Customer FAQ

#### 1. Stream View 与 Search 有什么区别？

Search 用于从关键词找到节点，并以摘要帮助筛选；Stream View 从一个标签或标签子树出发，按创建时间展示标题，目标是快速浏览。两者不合并为同一个命令。

#### 2. 父标签是否自动包含子标签？

是。选择 `diary` 会匹配 `diary` 本身以及通过 `:extends` 递归继承它的标签；不会匹配只是字符串前缀相同的 `diaryx`，也不会把斜杠字符串当作层级关系。

#### 3. 默认视图是什么？

只有一种视图：单列标题流。不会创建左侧 companion index，也没有 split/plain 或 `s` 切换。

#### 4. 标题流显示什么？

只显示节点的可读标题和轻微选中态，不显示源文件路径、Org 星号、正文、标签 token 或 button 下划线。

#### 5. 如何查看完整正文？

在当前标题按 `e`，打开源 Org 节点的 indirect/narrow buffer。段落、列表、链接、表格、quote 与代码块都由真实 Org buffer 显示，不在 Stream 中维护副本。

#### 6. 如何编辑正文？

在当前节点按 `e`，进入源 Org 节点的 narrow/indirect 编辑状态。编辑直接作用于原 Org 内容，保留正常 undo；Stream View 不维护正文副本，也不自动保存文件。退出编辑后回到原 Stream 节点和相对位置。

#### 7. 如何修改字段？

Stream View 不内嵌字段编辑器。用户在当前节点进入 `view-node` 修改 tag、field 和 field value，退出后由正常 Store 事件刷新 Stream。

#### 8. 删除或移动源节点后会怎样？

刷新时使用稳定 node identity 恢复位置；当前节点消失时落到第一个可用标题，没有结果时显示明确空状态。Stream View 不自行删除或移动源节点。

#### 9. 会修改现有文件或数据库格式吗？

不会。Stream View 读取现有 Store 索引和 Org 源文件，正文编辑仍走源 Org buffer；本阶段不新增持久化格式。

### Internal FAQ

#### 10. 为什么现在做，而不是继续扩充 Framework？

View Runtime、Widget Renderer、稳定 key 和原生交互 primitive 已经完成。Stream MVP 可以直接作为普通 Adapter 验证这些能力。除非真实产品路径暴露公共缺口，否则本阶段不修改 Framework contract。

#### 11. 是否会让 Stream、Search、Table、Kanban 共用同一个 renderer？

不会。它们共享 Runtime 生命周期，但保留适合自身内容的 renderer/adapter。统一行为模型不等于强迫所有视图使用同一种表现层。

#### 12. 是否实现虚拟滚动或增量 reconciliation？

不实现。当前投影只有标题；只有真实测量证明标题列表超出验收预算时，才以明确数据另开优化任务。

#### 13. 是否增加 `widget-extra`、VUI 或其他依赖？

不增加。使用现有 View Runtime、Widget Renderer 和 Emacs/Org 原生能力；新依赖必须解决已经测量且现有层无法承担的问题。

#### 14. narrow 编辑为什么不直接在 Stream buffer 中改？

Stream buffer 是派生阅读投影，直接修改会产生正文副本、同步和冲突语义。编辑源 Org 节点的 indirect/narrow buffer 可以复用 Org、undo、链接和保存行为，并保持单一事实来源。

#### 15. MVP 的完成标准是什么？

- 真实标签及后代标签查询正确，不误匹配相似前缀。
- 单列标题、导航和稳定位置恢复可用；不存在 companion index、`s` 或 button 下划线。
- Stream 不渲染正文；`e` 能打开完整源 Org 节点并返回。
- `e` 进入当前源节点的 narrow 编辑并能返回；字段入口打开 `view-node`。
- 重复打开、刷新、编辑和退出不泄漏 buffer、subscription 或 hook。
- focused/full ERT、静态检查与真实图形 `emacs -Q` hands-on 通过。

## Approved Product Decisions

1. Stream 只有单列标题流；不保留 companion index、split/plain 或 `s`。
2. 标签查询包含精确标签及全部传递 `:extends` 后代，结果按创建时间排序。
3. Stream 只读；`e` 打开完整源节点的 indirect/narrow buffer，字段编辑进入 `view-node`。
4. 标题流只显示标题和轻微选中态；不显示路径、正文、标签 token、Org 星号或 button 下划线。
5. 标题列表完整重绘；不引入详情状态、虚拟滚动、新依赖或 Framework 扩张。
