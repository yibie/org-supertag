# Org-SuperTag 本体三层架构视图（数据 / 逻辑 / 行为）

> org-supertag is an ontological system where data defines what exists, logic defines what it means, and behavior defines what can be done.

本文把“本体论三分法”落到 org-supertag 当前代码结构上，给出可执行的架构视图与边界约束，并回答它与 `supertag-automation` 规则系统的差异。

## 0. 给初中生的 3 分钟版本（用户能得到什么）

把 org-supertag 想象成一个“整理知识的游戏”，你在玩的时候最关心三件事：

1. **数据层（Data）**：我有哪些“东西”？（笔记/节点、标签、字段、关系）  
   - 它只负责“记账”，不负责判断对不对。

2. **逻辑层（Logic）**：这些东西“意味着什么”？  
   - 它只负责“算出来”结论：比如“这条 ref 已经过期”“这个任务已经超期”。  
   - **重要：逻辑层只算，不改文件，不发通知。**

3. **行为层（Behavior）**：要不要“动手做事”？  
   - 它才会真的去改 Org、保存文件、发提醒、同步外部系统。

对用户最直接的好处是：

- **你能看到一个结论到底是怎么来的**（逻辑层算出来的派生事实/视图）。  
- **你能看到自动化为什么没跑**：是没触发（trigger miss）、条件不满足（condition fail）、被禁用（disabled），还是出错（error）。  
- **你可以先写“逻辑”（只读、可反复算），再决定要不要接“自动化”**（会改东西、会打扰你）。

一个简单例子（对应你贴的输出）：

- 你给一条笔记打了 `ref`。  
- 逻辑层可以算出：`ref` + 没做完 + 超过 N 天 = “过期参考（stale reference）”。  
- 行为层可以选择：当它变成“过期参考”时，提醒你或者把它放进一个待处理清单。

## 1. 三层定义（定义 + 约束）

### 1) 数据层（Data Ontology）
**回答：世界里有什么。**

- **允许**：实体/字段/关系定义、持久化与扫描、数据模型与索引。
- **禁止**：推理、规则、自动化、条件判断、UI 交互。
- **在 org-supertag 中的体现**：
  - `supertag-core-store.el`：单一真相源（store）
  - `supertag-core-schema.el`：实体类型与字段/关系的结构性定义
  - `supertag-core-persistence.el`：落盘/加载
  - `supertag-core-transform.el`：原子写入与标准化
  - `supertag-core-notify.el`：事件广播（无业务判断）
  - `supertag-core-state.el`：事务与通知状态管理

### 2) 逻辑层（Logic Ontology）
**回答：世界意味着什么。**

- **允许**：派生事实、约束、语义不变量、语义视图（只读）。
- **禁止**：直接修改 store、执行外部动作、UI 交互。
- **在 org-supertag 中的当前形态**（分散，但用户入口已经很清晰）：
  - `supertag-services-query.el`：`supertag-query` / `supertag-query-sexp`（语义视图/筛选器的主要实现）
  - `supertag-core-scan.el`：扫描式查询（内部 API、O(N) 扫描 store），是 query 的基础依赖之一
  - `supertag-services-formula.el`：公式求值（只读计算，适合“派生列/派生字段”）
  - `supertag-automation.el` 的条件评估与公式/rollup 计算逻辑（逻辑能力目前散落在行为模块中）

> 逻辑层的关键不是“Elisp 函数”，而是**可声明的语义**。它必须是可读、可重算、无副作用的。

#### 你说“逻辑层就是 supertag-query”——为什么在当前版本里基本成立

从用户体验来看，最直接接触到的逻辑能力就是“查询/视图”：你写一个条件表达式，系统给你算出一组结果。

- `supertag-query` 产出的是**可重复计算的结果集**（典型逻辑层输出）。
- `supertag-automation` 产出的是**动作与写入**（典型行为层输出，可能被禁用/漏触发）。

所以你把“逻辑层≈supertag-query”是合理的：当前版本里，派生事实/约束/不变量这些更完整的逻辑层形态还没有统一入口，部分能力分散在 automation 条件、rollup 计算、UI 过滤等处。

### 3) 行为层（Behavior Ontology）
**回答：我们如何干预现实。**

- **允许**：自动化执行、同步、调度、UI 交互、对外系统调用。
- **输入**：数据层对象 + 逻辑层结论。
- **输出**：对 Org 世界的修改或外部动作。
- **在 org-supertag 中的体现**：
  - `supertag-automation.el` / `supertag-automation-sync.el`：事件驱动规则与动作执行
  - `supertag-ops-*.el`：具象写操作（字段/节点/关系）
  - `supertag-services-sync.el`：同步机制（Org ↔ Store）
  - `supertag-services-capture.el`：捕获服务
  - `supertag-services-ui.el`：UI 组件服务
  - `supertag-services-scheduler.el`：任务调度器
  - `supertag-services-embed.el`：Embed 块服务
  - `supertag-service-org.el`：Org 缓冲区交互服务
  - `supertag-ui-*.el` / `supertag-view-*.el`：交互与视图

## 2. 模块映射（当前态）

| 层 | 主要职责 | 代表模块/文件 |
| :--- | :--- | :--- |
| **Data** | 数据模型、持久化、存取与变更通知 | `supertag-core-store.el`, `supertag-core-schema.el`, `supertag-core-persistence.el`, `supertag-core-transform.el`, `supertag-core-notify.el`, `supertag-core-state.el` |
| **Logic** | 派生事实、语义查询、只读计算 | `supertag-services-query.el`, `supertag-core-scan.el`, `supertag-services-formula.el`，以及 `supertag-automation.el` 中的条件/公式/rollup 逻辑 |
| **Behavior** | 自动化、同步、UI、对外动作 | `supertag-automation.el`, `supertag-automation-sync.el`, `supertag-ops-*.el`, `supertag-services-sync.el`, `supertag-services-capture.el`, `supertag-services-ui.el`, `supertag-services-scheduler.el`, `supertag-services-embed.el`, `supertag-service-org.el`, `supertag-ui-*.el`, `supertag-view-*.el` |

### 混杂点（需明确）

- 逻辑层能力目前部分**嵌在行为模块里**（例如 `supertag-automation--evaluate-condition`、rollup/公式计算）。
- 查询与公式是逻辑层能力，但调用入口在 UI/服务层（这是合理的“调用”，不是“归属”）。
- 行为层不应直接定义语义，只能消费语义；现状是“部分语义随规则存储”。

### 混杂点清单（函数级，当前态）

- `supertag-automation--evaluate-condition` / `supertag-automation--eval-single-condition` (`supertag-automation.el`)：条件语义解析属于逻辑层，但目前绑定在行为层执行引擎里。
- `supertag-automation--evaluate-formula` / `supertag-automation-calculate-formula` (`supertag-automation.el`)：公式求值属于逻辑层，且与 `supertag-services-formula.el` 的沙盒实现并存。
- `supertag-automation--apply-rollup-function` / `supertag-automation--execute-rollup-calculation` (`supertag-automation.el`)：rollup 计算语义属于逻辑层，但内嵌在自动化执行中。
- `supertag-relation-calculate-rollup` (`supertag-ops-relation.el`)：同时做 rollup 计算与写入更新，逻辑与行为未分离。
- `supertag-view-table--evaluate-filter-condition` / `supertag-view-table--compare-values` (`supertag-view-table.el`)：UI 内部实现过滤语义，和 `supertag-services-query.el` 的过滤/比较逻辑重复。
- `supertag-query--evaluate-filter-condition` / `supertag-query--compare-values` (`supertag-services-query.el`)：语义过滤实现属于逻辑层，但挂在 services 模块中（可接受但需要边界声明）。

### 拆分建议（最小）

- 提取“逻辑引擎”到独立模块（如 `supertag-logic-condition.el`、`supertag-logic-formula.el`、`supertag-logic-rollup.el`、`supertag-logic-filter.el`），保证**无副作用**。
- 行为层（automation/ops）只负责调度与写入：计算逻辑统一调用逻辑模块，避免重复实现。
- UI 过滤与 query 过滤共用同一逻辑函数，保持语义一致；UI 只负责交互与显示。
- 如果暂不新增模块，至少在 `supertag-services-query.el` 或 `supertag-services-formula.el` 中集中逻辑实现，并让 automation 作为调用方。

## 3. 运行流与边界

```
Org 文本/用户动作
  -> ops/services (行为入口)
  -> core-transform (原子变更)
  -> core-store (单一真相源)
  -> core-notify (事件广播)
  -> automation/scheduler (行为执行)
  -> UI/views (展示)

逻辑层：从 store 读取 -> 计算派生事实/视图 -> 供行为层/AI 使用
```

**边界要点**：
- 逻辑层只能“读”和“推导”，不能“写”。
- 行为层必须通过 ops/transform 修改数据，不能直接改 store。
- 任何 AI 行为都应基于“数据 + 逻辑结论”，而不是直接改 Org。

## 4. 与 `supertag-automation` 规则的区别

`supertag-automation` 是**行为层**系统，而不是逻辑层。两者的核心差异如下：

| 维度 | 逻辑层（Logic） | 自动化规则（Behavior） |
| :--- | :--- | :--- |
| **目的** | 定义“什么成立 / 什么成立的含义” | 定义“发生什么就做什么” |
| **副作用** | 无副作用，只读推导 | 有副作用，写入 store/触发外部动作 |
| **触发** | 可按需重算；不依赖事件 | 由事件触发（`:store-changed` / schedule） |
| **稳定性** | 作为语义基线，默认成立 | 可启用/关闭，属于可选行为 |
| **表达形态** | 声明式规则/约束/派生事实 | WHEN/IF/THEN 规则（触发 + 条件 + 动作） |

**关键点**：
- 自动化里的 `:condition` 看起来像逻辑，但它**只是执行门控**，不构成系统语义。
- 逻辑层可以表达“Task 必须属于一个 Project”这类不变量，但自动化只能“事后修补”。
- 自动化依赖事件上下文（如 `supertag-automation--current-event`），逻辑层应独立于事件、可重复计算。

**例子**：
- 逻辑层：`Project.status = "blocked"` if any related Task is blocked（只读派生）。
- 行为层：当 `Project.status` 变为 blocked 时通知用户或更新 TODO（写入/外部动作）。

## 5. 最小落地原则（可执行约束）

- **数据层只建模，不推理**：只负责“存在什么”。
- **逻辑层只推理，不动作**：只产出可声明的结论，供 UI/自动化/AI 消费。
- **行为层只执行，不解释**：只根据输入结论执行动作，不再“猜语义”。

> 这三条是“可落地”的工程约束：只要遵守，就能在现有代码结构上逐步把逻辑层从行为层中抽离出来。

### 5.1 逻辑层如何落地（以 `supertag-query` 为核心）

如果你认可“逻辑层≈查询/视图”，那么落地其实就是把“查询”变成用户能持续使用的三个入口：

1. **Query-Block（推荐，最符合逻辑层）**：把逻辑写进 Org 文档，结果可重复计算、可分享、可版本化。  
   - 入口：`M-x supertag-insert-query-block`（实现见 `supertag-ui-query-block.el`）  
   - 用法：在插入的 `org-supertag-query-block` 代码块里写查询 S-expression，`C-c C-c` 执行，得到表格结果。  

2. **交互式视图（Table / Search）**：把逻辑作为“视图条件”来用，体验更爽快，但默认不持久化。  
   - Table：`M-x supertag-view-table`（目前交互入口按 tag 打开，再用 `/` 过滤、`C-c v S` 保存为 named view；named view 目前是 buffer-local）。  
   - Search：`M-x supertag-search`（偏检索体验，不是严格的“逻辑视图”，但同样消费 query 能力）。  

3. **程序化/Agent 使用（Elisp / AI）**：把 query 当作可调用的“逻辑函数”。  
   - 入口：`supertag-query-sexp`（实现见 `supertag-services-query.el`）  
   - 实验：`supertag-test-view-query-sexp`（在 `supertag-test.el`，用最小 UI 展示 query 结果）  

4. **解释/诊断（实验，面向“为什么没跑”）**：把“逻辑结论”和“自动化执行原因”一次性展示出来。  
   - 入口：`M-x supertag-test-explain-current-node`（输出 Derived facts + Automation dry-run）  
   - 价值：当你看到 “trigger-miss / condition-fail”，你知道问题在“触发没对上”还是“条件不满足”，不用靠猜。  

落地顺序建议：
- 先用 Query-Block 固化“语义视图”（逻辑真相源）。  
- 再把常用 Query-Block 变成交互式 view 的入口（更爽的体验）。  
- 最后再接行为层：对某个逻辑视图的结果执行动作（这一步才是 automation/ops 的职责）。

## 6. 参考文件（当前态）

- 数据层：`supertag-core-store.el`, `supertag-core-schema.el`, `supertag-core-scan.el`, `supertag-core-transform.el`, `supertag-core-persistence.el`, `supertag-core-notify.el`, `supertag-core-state.el`
- 逻辑层：`supertag-services-query.el`, `supertag-services-formula.el`, `supertag-automation.el`（条件/rollup/公式）
- 行为层：`supertag-automation.el`, `supertag-automation-sync.el`, `supertag-ops-*.el`, `supertag-services-sync.el`, `supertag-services-capture.el`, `supertag-services-ui.el`, `supertag-services-scheduler.el`, `supertag-services-embed.el`, `supertag-service-org.el`, `supertag-ui-*.el`, `supertag-view-*.el`

## 7. 特殊模块（非三层架构）

以下模块不属于数据/逻辑/行为三层架构，而是提供基础设施、工具或特定功能：

### 7.1 基础设施层

**`supertag-core-async.el`**：异步任务队列
- 职责：提供异步任务处理能力，使用 idle timers 防止 UI 冻结
- 特点：基础设施模块，供 sync 等模块使用
- 边界：无业务逻辑，仅提供任务调度机制

**`supertag-services-scheduler.el`**：统一任务调度器
- 职责：管理所有后台和周期性任务（:interval / :daily）
- 特点：替代分散的 timer 实现，提供集中式任务注册
- 边界：属于行为层基础设施，不直接执行业务动作

### 7.2 工具/迁移

**`supertag-migration.el`**：数据迁移脚本
- 职责：从旧 `org-supertag-db.el` 格式迁移到新架构
- 特点：一次性运行脚本，非常规业务代码
- 边界：独立工具，不属于三层架构

**`supertag-migrate-tag-ids.el`**：标签 ID 迁移
- 职责：将 UUID-based 标签 ID 迁移到 name-based ID
- 特点：针对性迁移工具
- 边界：独立工具，不属于三层架构

### 7.3 AI 功能

**`supertag-rag.el`**：RAG（检索增强生成）
- 职责：提供 LLM 驱动的检索增强生成功能
- 特点：结合 gptel 和 supertag 查询系统
- 边界：属于行为层（AI 功能子类），消费数据层和逻辑层

### 7.4 遗留/废弃

**`supertag-compat.el`**：兼容性层
- 职责：保持向后兼容的 stub（已废弃）
- 特点：no-op 模式，仅防止配置破坏
- 边界：遗留代码，计划移除

### 7.5 入口/配置

**`org-supertag.el`**：主入口文件
- 职责：包入口、配置定义、依赖管理
- 特点：不包含业务逻辑，仅作为包的入口点
- 边界：配置层，不属于三层架构

### 7.6 测试

**`supertag-test.el`**：测试文件
- 职责：实验脚本（逻辑解释、automation dry-run、query 结果预览）
- 边界：测试代码，不属于生产架构
