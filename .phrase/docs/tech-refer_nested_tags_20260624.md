# tech-refer: 嵌套标签 `#a/b/c` 的自动解析与层级展示

## Decision Update (2026-08-03, supersedes the 2026-07-29 frontend model)

### Parent-aware completion and Schema

- 用户按真实 Tag ID 直接搜索；`happy :extends diary` 可由输入 `happy` 命中。
- completion 用 Emacs 原生 affixation 显示 `diary/happy`，但候选值、Org token
  与 Store identity 始终仍是 `happy`。
- Schema View 使用一棵树：显式 `:extends` 是主父子关系；没有显式父级的旧
  完整路径 ID 才按 `/` 派生兼容层级。
- Schema 的新增 Child Tag 只写 `:extends`。旧 `a n` 路径创建入口删除，`a c`
  仅保留为同一命令的兼容快捷键。
- 继续使用当前 O(T) Tag 列表与 Store 读取；不新增缓存、索引、字段或迁移。

### Data Model

- Tag entity 的 `:id` 仍是唯一身份；父子关系复用现有 `:extends`，不新增第二套关系。
- `happy :extends diary` 的展示路径是 `diary/happy`，但展示路径不是 ID，也不写回 Org。
- 已有 `emacs/package` 等完整路径 ID 继续原样保存、查询和写回；本次不自动拆分或迁移。
- 一个真实 Tag 只有一个 `:extends` 父级；需要同名叶子属于多个父级时，继续使用不同的完整路径 ID。

### Query Semantics

- 现有查询保持精确匹配，避免改变已有视图和自动化行为。
- 调用方显式传入 `include-descendants` 时，`emacs` 同时命中 `emacs/package`、`emacs/package/elpa`，
  但不命中 `emacs2/package`。
- 继续使用现有 O(N) node scan；后代判断仅增加字符串前缀比较。只有 10k-node 基准证明交互延迟不可接受时，
  才考虑额外前缀索引。

### Minimal Implementation

1. 从现有 `:extends` 父链计算只读 display path，并以 affixation 接入 CAPF 和共享 reader。
2. Schema 先连接显式 `:extends`，再为无显式父级的完整路径连接 `/` 父级；连接时拒绝循环。
3. 保持实际候选字符串不变，选择 `diary/happy` 的视觉项仍只插入 `happy`。
4. 删除独立路径子标签创建入口，复用现有 Child Tag 命令。

### Rejected

- **继续逐层导航 namespace**：要求用户先输入父级，且与已有 `:extends` 父子关系重复。
- **把 display path 当候选值**：会把选择 `happy` 变成新 ID `diary/happy`，污染现有数据。
- **自动迁移旧完整路径 ID**：身份与引用迁移风险高；当前兼容回退已经满足展示。
- **自动从 `/` 写入 `:extends`**：会改变旧路径 Tag 的字段继承语义，本次只在读取侧兼容。

---

## Historical Proposal (2026-06-24, rejected by the 2026-07-29 decision)

## Context

- issue009 提出：希望支持 `#project/active` 这样的路径式标签写法，由 sync 自动建立父子层级，替代手动 `supertag-set-tag-parent`。
- 当前系统已具备：`:extends` 字段存储父子关系、`supertag-view-schema` 渲染标签树、字段继承已生效。
- 缺失：sync 阶段不识别 `#a/b/c` 路径，不会自动创建中间父标签与子标签的 `:extends` 关系。

## Goals

1. Sync 时解析 `#a/b/c`，自动创建 `a`、`b`、`c` 标签，并设置 `b :extends a`、`c :extends b`。
2. 不覆盖已有标签的字段定义与 `:extends` 关系。
3. 提供递归查询 API：`supertag-find-tag-descendants`。
4. 保持 Schema View 等现有视图对嵌套树的展示能力。

## Non-goals

- 不新增常驻侧边栏标签树（超出本次范围，可独立实现）。
- 不改写 org-supertag 的存储模型（继续使用 `:extends` + `:tags`）。

## Options

### 选项 A：节点 `:tags` 只存叶子标签（推荐）

`#project/active` 解析后，节点 `:tags` 只保留 `("active")`；`project` 与 `active` 的层级关系由 tag 定义中的 `:extends` 维护。

- 优点：
  - 标签列表最短，与现有 Schema View / 字段继承机制完全兼容。
  - 查询 `active` 直接命中；查询 `project` 可通过 descendant 扩展命中。
  - 数据模型一致：层级关系单一来源（tag 定义）。
- 缺点：
  - 写回文件时如果直接按 `:tags` 渲染，会把 `#project/active` 变成 `#active`，丢失用户书写形式。
  - 需要额外字段记录原始路径以支持回写。

### 选项 B：节点 `:tags` 存完整路径标签

`#project/active` 解析后，节点 `:tags` 保留 `("project/active")`。

- 优点：保留用户原始书写。
- 缺点：
  - `supertag-find-nodes-by-tag("project")` 不会命中，需要查询层做路径前缀匹配。
  - Schema View 中 `project/active` 会作为一个独立标签显示，需要额外拆分才能展示为层级。
  - 与现有 `:extends` 机制数据重复。

### 选项 C：节点 `:tags` 双写（叶子 + 路径）

`#project/active` → `:tags` 存 `("active")`，同时新增 `:raw-tag-paths` 存 `("project/active")`。

- 优点：查询和回写都自然。
- 缺点：数据模型变复杂，需维护两个字段的一致性。

### 选项 D：节点 `:tags` 存所有层级节点

`#project/active` → `:tags` 存 `("project" "active")`。

- 优点：查询 `project` 直接命中。
- 缺点：
  - 标签列表变长，标题行视觉上更拥挤。
  - `active` 单独出现时与 `project/active` 中的 `active` 查询结果一致，但写回时无法区分。

## Proposed Approach

采用 **选项 A + 原始路径回写支持**，分三阶段实施。

### 阶段 1：Sync 解析与自动建父子（MVP）

1. 新增 `defcustom supertag-sync-nested-tags`（默认 `nil`，opt-in）。
2. 在 `supertag-services-sync.el` 中新增 `supertag--expand-nested-tag-path`：
   - 拆分标签名为 `/` 分隔的段。
   - 对每一段调用 `supertag-tag-create`（已内置"存在则跳过"逻辑）。
   - 对非根段调用 `supertag--set-tag-parent` 建立 `:extends`。
   - 返回叶子标签名，替换原路径标签进入节点 `:tags`。
3. 在 `supertag--process-node-tags` 中，当 `supertag-sync-nested-tags` 为 `t` 时展开路径。
4. 新增 `:raw-tag-paths` 字段（或复用 `:properties`）记录原始路径，用于写回时保持用户书写。
5. 循环保护：展开路径时检查当前叶子不能是路径中任何前缀的父级，避免 `#a/b/a` 这类循环。

### 阶段 2：查询 API

1. 在 `supertag-core-scan.el` 新增 `supertag-find-tag-descendants(tag-id)`：
   - BFS/DFS 遍历 `:extends` 关系，返回所有后代 tag id 列表。
2. 在 `supertag-find-nodes-by-tag` 上新增可选参数 `include-descendants`：
   - 为 `t` 时，同时查询目标 tag 及其所有后代 tag 的节点。

### 阶段 3：视图适配

1. Schema View 已支持层级展示，无需改动。
2. table / kanban 视图可后续支持"按父标签聚合"，利用阶段 2 的 descendant API。
3. 可选：侧边栏标签树可独立作为后续功能。

## Interfaces & APIs

```elisp
(defcustom supertag-sync-nested-tags nil
  "When non-nil, parse #a/b/c path tags during sync and auto-create parent/child tag hierarchy.")

(defun supertag--expand-nested-tag-path (tag-name)
  "Expand TAG-NAME like #a/b/c into its leaf tag, creating parent/child relations.")

(defun supertag-find-tag-descendants (tag-id)
  "Return all descendant tag IDs of TAG-ID, recursively.")

(defun supertag-find-nodes-by-tag (tag-name &optional include-descendants)
  "Find nodes with TAG-NAME. If INCLUDE-DESCENDANTS is non-nil, include nodes with descendant tags.")
```

## Trade-offs

| 维度 | 方案 A（推荐） | 方案 B |
|---|---|---|
| 数据模型一致性 | 高 | 低 |
| 与现有 Schema View 兼容性 | 高 | 需额外拆分 |
| 写回保留原始路径 | 需 `:raw-tag-paths` 字段 | 自然 |
| 查询复杂度 | 低 | 高（需前缀匹配） |
| 标题长度 | 最短 | 中等 |

## Risks & Mitigations

| 风险 | 缓解 |
|---|---|
| 覆盖用户已有 `:extends` 关系 | `supertag-tag-create` 已跳过已存在标签；`supertag--set-tag-parent` 只对新创建或明确指定的子标签生效。 |
| 循环继承（`#a/b/a`） | 展开路径时检测：叶子不能是路径中任何前缀的父级。 |
| 标签名含 `/` 的歧义 | 固定 `/` 为层级分隔符；如需保留字面 `/`，提供转义或配置关闭 nested tags。 |
| 写回后丢失 `#project/active` 形式 | 引入 `:raw-tag-paths` 记录原始路径；写回时优先使用原始路径渲染。 |
| 性能（大量嵌套标签） | 创建标签时每个路径段 O(1)；descendant 查询最坏 O(T)，T 为标签总数，与现有 schema cache  rebuild 同量级。 |

## Historical Decision (Rejected 2026-07-29)

- 采用 **选项 A**：节点 `:tags` 只存叶子标签，层级关系由 tag 定义的 `:extends` 维护。
- 默认关闭，通过 `supertag-sync-nested-tags` opt-in。
- 必须实现 `:raw-tag-paths` 以保留文件中的原始路径写法。
- 分三阶段实施，先完成阶段 1 MVP。

## Related

- issue: `.phrase/docs/issue_nested_tags_20260624.md`
- ISSUES index: `.phrase/docs/ISSUES.md`
