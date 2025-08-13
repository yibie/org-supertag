理解你的取舍取向：**从 org-supertag-view-table 的现实边界出发，挑对 Emacs 友好的增强项**，而不是“照搬” Bases/Logseq。下面是一份精简的产品开发文档，只保留对你当前实现（如 `org-supertag-view-table-mode`、`org-supertag-view--get-related-nodes`、`org-supertag-get-all-fields-for-tag`）最契合、成本可控、收益明显的功能点。

# 功能概述

* **定位**：为某个 tag 展示「节点 × 字段」的**属性矩阵**（Logseq 风格），并提供**键盘优先、轻量稳定**的交互增强（无工具条、少 overlay、零图片依赖）。
* **目标**：在不改变数据层的前提下（沿用你已有的取数链路），让用户能**选列/排序/分组/快速筛选/批量改值/行内编辑（跳转式）/保存视图预设**。
* **非目标（本阶段不做）**：卡片/封面、公式列 DSL、大规模图片渲染、复杂可视化。

# 功能设计逻辑

1. **职责分层**

   * *数据层*：继续用 `org-supertag-view--get-related-nodes` + `org-supertag-get-all-fields-for-tag` 产出行与字段。
   * *视图层*：新增一层**视图状态**（列清单与顺序、排序链、分组键、筛选条件、选中集），只影响渲染与交互。
   * *配置层*：\*\*视图预设（View Preset）\*\*可保存/应用到某 tag；可设为该 tag 的默认视图。

2. **渲染与性能原则**

   * 继续基于 `org-mode` 文本渲染（标准 Org 表或你现有的轻量网格），**避免大面积 overlay**；行选中/组头仅用 text-properties。
   * 刷新粒度：任何操作→**重建当前缓冲文本**（O 行×列），必要时提供 `:limit` 与分页。

3. **交互范式（键盘优先）**

   * 一个小型 **keymap**/Transient 入口（不常驻 UI）；核心动作为单键或两键位序列。
   * 失败/冲突在 minibuffer 给出清晰提示；不打断视图。

# 功能组件（精选，按优先级）

## A. 必做（第一轮）

1. **视图预设（View Presets）**

   * 定义：`(org-supertag-view-define NAME :columns … :sort … :group … :filters … :limit …)`
   * 应用：`org-supertag-view-apply`（将预设套到当前 tag 视图）；可设为 tag 默认。
   * 存储：buffer-local → 可序列化到项目文件（如 `.ostview`）或自定义变量。

2. **列管理（选择/顺序/持久化）**

   * 从 tag 的字段 schema 生成候选列；`c` 打开列表↑↓重排、空格勾选；保存到预设。
   * 渲染严格按当前顺序；默认列：`Node | 所有字段`或预设列。

3. **多列排序（稳定版）**

   * 表头所在列按 `s`/`S` 加入排序链（↑/↓/无循环），支持多列级联。
   * 数字/日期/字符串三类比较，空值置底（可定制）。

4. **分组与折叠（单层）**

   * `g` 选择分组字段；插入组头行，`TAB` 折叠/展开；`G` 清除分组。
   * 组内独立应用排序；底部显示组计数。

5. **行选择 + 批量操作（最小集）**

   * Dired 风格：`m/u/t` 标记/取消/反选，`x` 打开批量动作：

     * `a` 添加标签、`r` 移除标签
     * `f` 设字段值、`F` 清空字段值
     * `d` 设置日期型字段（调用 `org-read-date`）
   * 以 node-id 为主键，操作后局部刷新。

6. **行内编辑（跳转式）**

   * `e` 编辑光标单元格：根据字段类型弹输入（文本/数字/日期/枚举），写回节点后返回并刷新。
   * 不做表内直接输入，保证稳定与简单。

## B. 实用增强（第二轮）

7. **快速筛选（键盘化 Any/All）**

   * `/` 输入如 `owner:alice status!=Canceled updated<=7d`；`A` 切 AND/OR。
   * 小集合可本地过滤；大集合拼接到查询侧（与你的 query 结合）。

8. **类型与枚举支持（来自字段 schema）**

   * 字段支持 `:type '(enum date number text checkbox link url)`、`:options …`、`:default …`。
   * 影响：编辑器选择器、显示格式（日期相对/绝对）、排序类型。

9. **状态统计（底栏）**

   * 在当前筛选/分组下输出 “Total: N | By Status: …”。

## C. 可选（更后面再考虑）

10. **固定列（Pin）**：将 `Node` 固定在左侧（技术方案：左右两块文本并排，非 overlay）。
11. **虚拟列（轻公式）**：少量白名单函数（`now/today/days-between/prop/round`）做“剩余天数”等只读列；默认关闭。
12. **卡片视图**：纯文本卡片（不拉图片），仅在确有需求时实现。

# 实施要点（与现有代码贴合）

* **状态容器**（buffer-local）：`columns`、`sort-spec`、`group-key`、`filters`、`selected-ids`、`limit`、`preset-name`。
* **数据模型**：每行携带 `node-id title tags fields-alist`；列描述含 `name type getter formatter`.
* **键位建议**（`org-supertag-view-table-mode`）：

  * 刷新 `g`；列管理 `c`；排序 `s/S`；分组 `g/G`（可换 `C-g` 清组以避免冲突）；
  * 标记 `m/u/t`；批量 `x`；编辑 `e`；筛选 `/` 切 AND/OR `A`。
* **兼容点**：若当前实现仍有网格/图片调试键位，隔离到调试前缀（如 `C-c C-x …`），避免侵扰主流程。
* **小修建议**：`org-supertag-view-table-setup-keys` 中对 `org-supertag-view-table-mode-map` 的引用需与 `define-derived-mode` 里创建的局部 keymap 对齐，避免绑定失效。

---

**总结**：以上是**为 Emacs 量身取舍**的增强清单：保留 Logseq 的“表格基本功”和 Bases 的“视图可复用”思想，但不引入沉重 UI/图片/公式 DSL。先做 *预设/列管理/多列排序/分组/批量/跳转式编辑* 六件事，就能把 `org-supertag-view-table` 从“能看”提升到“好用”。如果确认路线，我可以按这份文档拆成两个迭代包（A 组必做、B 组增强），给出最小补丁骨架与 keymap。
