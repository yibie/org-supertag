# PR/FAQ — Supertag Stream View

Status: Approved by user on 2026-08-06
Date: 2026-08-06
Predecessors: `DONE-phase-view-runtime-20260804`、`DONE-phase-widget-renderer-20260805`

## Press Release

### Org-Supertag 用 Stream View 把一个标签变成可连续阅读的知识页面

**在同一个 Emacs 视图中浏览父标签及其所有后代节点，并在需要时只编辑当前 Org 节点。**

**2026-08-06 —** Org-Supertag 将推出独立的 Stream View。用户选择一个标签后，可以按创建顺序连续阅读带有该标签或任一后代标签的节点正文，而不必在搜索结果、源文件和 Node View 之间来回跳转。

当前 Search、Table 与 Kanban 擅长查找、比较和操作结构化数据，但不适合连续阅读笔记正文。普通搜索结果只展示摘要；源文件又把相关内容分散在不同位置。嵌套标签已经能表达 `emacs/package` 与 `emacs/package/elpa` 的关系，却还没有一个阅读界面把这棵标签子树聚合成连贯内容。

Stream View 把标签当作阅读入口。默认分栏模式在左侧提供简洁的节点索引，在右侧显示完整正文流；用户也可以切换为没有侧栏的 plain 模式。索引只显示可识别的标题，不显示文件路径或装饰符号；正文标题不带 Org 星号，但相对正文更醒目；标签继续显示为现有的 `#emacs/package` 样式。当前节点仅使用与 `supertag-search` 一致的轻微高亮。

“我们已经统一了 View Runtime 和 Widget Renderer，现在最有价值的验证不是再造一层框架，而是让用户真正通过标签阅读自己的知识。”Org-Supertag 项目负责人表示，“Stream View 只做连续阅读、定位和局部编辑，把字段管理留给已经成熟的 Node View。”

用户执行 `M-x supertag-view-stream` 并选择标签。若选择 `emacs/package`，结果包含直接带有 `emacs/package` 的节点，以及带有 `emacs/package/...` 后代标签的节点。`s` 在分栏与 plain 模式之间切换；导航会同步索引、正文和轻微选中态。按 `e` 时，Stream View 使用源 Org 节点的 narrow/indirect 编辑体验，只暴露当前节点；完成后返回原来的 Stream 位置。需要修改字段或字段值时，用户从当前节点进入 `view-node`。

“以前我为了重读一个主题，要打开搜索结果再逐条跳回文件。现在 `#emacs/package` 本身就是一页完整的阅读流；想改正文时只看到这一条，改字段时仍使用熟悉的 Node View。”一位 Emacs 用户表示。

用户可通过 `M-x supertag-view-stream` 开始使用；本阶段不改变现有 Search、Table、Kanban、Node 或源 Org 文件格式。

## FAQ

### Customer FAQ

#### 1. Stream View 与 Search 有什么区别？

Search 用于从关键词找到节点，并以摘要帮助筛选；Stream View 从一个标签或标签子树出发，展示完整正文，目标是连续阅读。两者不合并为同一个命令。

#### 2. 父标签是否自动包含子标签？

是。选择 `diary` 会匹配 `diary` 本身及 `diary/...`；不会匹配只是字符串前缀相同的 `diaryx`。界面保留完整标签路径，不把 `/` 改写成 `_`。

#### 3. 默认视图是什么？

默认是分栏：左侧为紧凑节点索引，右侧为完整正文流。按 `s` 切换到 plain 模式后，只保留正文流；再次按 `s` 恢复分栏。本阶段不增加第三种布局。

#### 4. 左侧索引显示什么？

只显示节点的可读标题和轻微选中态，不显示源文件路径、Org 星号或难以解释的状态符号。标签显示在正文节点中，继续复用现有 `#tag/path` 样式。

#### 5. 正文如何显示？

显示完整正文并保留可读的 Org 结构，包括段落、列表、链接、表格、quote 与代码块。节点标题不显示前导 `*`，但使用比正文更醒目的 face。结果按创建时间排序。

#### 6. 如何编辑正文？

在当前节点按 `e`，进入源 Org 节点的 narrow/indirect 编辑状态。编辑直接作用于原 Org 内容，保留正常 undo；Stream View 不维护正文副本，也不自动保存文件。退出编辑后回到原 Stream 节点和相对位置。

#### 7. 如何修改字段？

Stream View 不内嵌字段编辑器。用户在当前节点进入 `view-node` 修改 tag、field 和 field value，退出后由正常 Store 事件刷新 Stream。

#### 8. 删除或移动源节点后会怎样？

刷新时使用稳定 node identity 恢复位置；当前节点消失时落到最近的可用节点，没有结果时显示明确空状态。Stream View 不自行删除或移动源节点。

#### 9. 会修改现有文件或数据库格式吗？

不会。Stream View 读取现有 Store 索引和 Org 源文件，正文编辑仍走源 Org buffer；本阶段不新增持久化格式。

### Internal FAQ

#### 10. 为什么现在做，而不是继续扩充 Framework？

View Runtime、Widget Renderer、稳定 key 和原生交互 primitive 已经完成。Stream MVP 可以直接作为普通 Adapter 验证这些能力。除非真实产品路径暴露公共缺口，否则本阶段不修改 Framework contract。

#### 11. 是否会让 Stream、Search、Table、Kanban 共用同一个 renderer？

不会。它们共享 Runtime 生命周期，但保留适合自身内容的 renderer/adapter。统一行为模型不等于强迫所有视图使用同一种表现层。

#### 12. 是否实现虚拟滚动或增量 reconciliation？

首个版本不实现。先测量真实的 100/500/1000 节点完整正文数据；只有首帧或刷新成本超过验收预算时，才以明确数据另开优化任务。

#### 13. 是否增加 `widget-extra`、VUI 或其他依赖？

不增加。使用现有 View Runtime、Widget Renderer 和 Emacs/Org 原生能力；新依赖必须解决已经测量且现有层无法承担的问题。

#### 14. narrow 编辑为什么不直接在 Stream buffer 中改？

Stream buffer 是派生阅读投影，直接修改会产生正文副本、同步和冲突语义。编辑源 Org 节点的 indirect/narrow buffer 可以复用 Org、undo、链接和保存行为，并保持单一事实来源。

#### 15. MVP 的完成标准是什么？

- 真实标签及后代标签查询正确，不误匹配相似前缀。
- 分栏/plain 切换、导航同步和稳定位置恢复可用。
- 完整 Org 正文、标题和 `#tag/path` 的可读呈现符合已确认的轻量界面。
- `e` 进入当前源节点的 narrow 编辑并能返回；字段入口打开 `view-node`。
- 重复打开、刷新、切换、编辑和退出不泄漏 buffer、window、subscription 或 hook。
- focused/full ERT、静态检查与真实图形 `emacs -Q` hands-on 通过。

## Product Decisions Requiring Approval

1. 默认分栏，`s` 切换 plain；不记忆第三种布局。
2. 标签查询包含精确标签及全部 `/` 后代，结果按创建时间排序。
3. Stream 只读；正文编辑进入源节点的 indirect/narrow buffer，字段编辑进入 `view-node`。
4. 左侧仅显示标题和轻微选中态；不显示路径、Org 星号或额外状态符号。
5. MVP 完整重绘；不引入虚拟滚动、新依赖或 Framework 扩张。
