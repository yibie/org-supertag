# Org-SuperTag 5.0 – 纯 Emacs Lisp 的结构化知识管理

Org-SuperTag 把普通的 Org 标题变成一个**可结构化查询的知识库** ——  
完全在 Emacs 里完成，不依赖外部服务，也不再需要 Python。

- 每个 `#tag` 像一张**数据库表**
- 每个标题是一个**记录（record）**；它的结构化字段保存在 Org-SuperTag 的数据库中
- 视图、查询和自动化都建立在这层结构化数据之上

本文件是 5.0 架构下 README 的**中文草稿**。

---

## Org-SuperTag 是什么？

传统 Org-mode 提供的是：

- 标题（headline）
- TODO 关键字
- `:project:research:` 这样的标签字符串

Org-SuperTag 在此基础上加了一层**强结构化层**：

```org
* Website Redesign #project
```

你在磁盘上只需要一个标题和一个 `#project` 标签。Org-SuperTag 会：

- 扫描并同步这些 Org 文件
- 在内存数据库中维护一个规范化的数据表示
- 在此之上提供快速查询和多种视图

目标很简单：**用 Org 的方式，做出 Notion 的结构化体验**。

---

## 5.0 的核心变化

5.0 是一次**内部架构的大幅简化**，在不改变核心理念的前提下，把复杂度砍掉了。

- **纯 Emacs Lisp**  
  不再需要 Python、EPC、virtualenv，所有逻辑都在 Emacs 内部。

- **单一数据源**  
  所有结构化数据都存放在一个基于 hash-table 的统一存储里，  
  不再是多套半同步的 SQLite 表。

- **同步更快、调试更简单**  
  没有跨进程 RPC，整体开销更低，出问题也更容易定位。

- **Org 文件不变，内部引擎重写**  
  磁盘上的 Org 仍然是主要数据载体，变化的是内部结构化存储的实现方式。

更详细的新旧架构对比参见：

- `doc/COMPARE-NEW-OLD-ARCHITECTURE.md`
- `doc/COMPARE-NEW-OLD-ARCHITECHTURE_cn.md`

---

## 30 秒上手

使用 straight.el 的最小配置：

```emacs-lisp
;; 1. 安装
(straight-use-package '(org-supertag :host github :repo "yibie/org-supertag"))

;; 2. 配置需要同步的 Org 目录
(setq org-supertag-sync-directories '("~/org/"))

;; 3. 初始化数据库（只需运行一次）
M-x supertag-sync-full-initialize
```

初始化完成后，你可以：

- 添加标签：`M-x supertag-add-tag`
- 打开节点视图：`M-x supertag-view-node`
- 运行查询：`M-x supertag-search`

---

## 核心概念与数据模型

Org-SuperTag 围绕几类核心实体设计：

- **Node（节点）**  
  一个 Org 标题（headline），在系统中有唯一 ID，并在多次同步之间被追踪。

- **Tag（标签）**  
  写在标题里的 `#tag` 文本。每一个 `#tag` 都像一张逻辑表。

- **Field definition（字段定义）**  
  存在于 tag 的 schema 中（` :fields` 列表），描述字段名称、类型、可选项、默认值和校验规则。  
  字段定义挂在 tag 上，**不会直接体现在 Org 文件文本中**。

- **Field value（字段值）**  
  对于每个 `(node-id, tag-id, field-name)` 三元组，Org-SuperTag 会在内部的 `:fields` 集合中存储一个值。  
  这些值存在于数据库中，**不要求在 Org 文件里以 `- key: value` 的形式呈现**。

- **Schema & Views（模式与视图）**  
  字段定义决定了可以存什么；视图决定你怎么在 UI 里编辑和展示这些字段（表单、表格、看板等）。

### 示例：磁盘上的 Org vs 数据库里的字段

```org
* Attention Is All You Need #paper
```

磁盘上的 Org 文件只需要这一行标题和 `#paper` 标签。  
在 Org-SuperTag 的数据库中，这个节点可能有如下字段值：

- `authors`: `"Vaswani et al."`
- `year`: `2017`
- `venue`: `"NIPS"`
- `status`: `"read"`
- `rating`: `5`

内部结构可以理解为：

- tag `"paper"` 上定义了字段：`authors`, `year`, `venue`, `status`, `rating`
- 对于每个节点，有一组 `(node-id, "paper", field-name)` → value 的字段值
- 节点本身还有文件路径、标题路径、scheduled/deadline 等元数据

基于这些数据，你可以：

- 过滤所有 `#paper` 且 `status = unread` 的节点
- 按 `year` 或 `rating` 排序
- 在自定义视图或查询块中展示这些内容

更多关于 Query Block 的设计动机和用法，参见：

- `doc/ABOUT-QUERY-BLOCK.md`
- `doc/ABOUT-QUERY-BLOCK_cn.md`

---

## 典型使用场景

### 学术 / 研究笔记

```org
* Diffusion Models for XYZ #paper
```

磁盘上只保留标题与 `#paper` 标签。  
在数据库里，你可以在 `#paper` 上定义字段：`authors`, `year`, `venue`, `status`, `topic` 等，并给每个节点填入具体值。

然后你可以：

- 列出所有 `topic = generative-models` 的论文
- 用 `status` 做阅读队列（`unread` → `reading` → `done`）
- 按 `year` 或 `rating` 排序

### 项目管理

```org
* Rewrite sync layer #task #project
```

`#task` / `#project` 这样的标签可以在 schema 中定义字段：`status`, `priority`, `owner`, `project` 等。  
字段值统一存储在 Org-SuperTag 数据库中，通过表单、表格、看板视图来编辑，而不是手写 `- key: value`。

配合：

- `M-x supertag-view-kanban` 做看板视图
- 自定义查询实现 “本周高优先级任务” 等过滤

### 会议与日志

```org
* Sprint Planning 2024-11-15 #meeting
```

在 `#meeting` 上定义字段：`date`, `participants`, `decisions` 等，  
对每条会议记录的对应节点填写字段值。之后可以：

- 查询某个时间范围内的所有 `#meeting`
- 按参与人或项目筛选会议

---

## 查询与视图

底层查询引擎是 Lisp 表达式。低层 API 可以直接调用：

```emacs-lisp
;; 高优先级项目
(supertag-search
 '(and (tag "project")
       (field "priority" "high")))

;; 未读论文
(supertag-search
 '(and (tag "paper")
       (field "status" "unread")))
```

在此之上，Org-SuperTag 提供：

- **交互式搜索**：`M-x supertag-search`
- **视图**：节点表单、表格视图、看板视图等
- **查询块（query block）**：直接写在 Org 文件中的动态查询块

更多与补全、自动化、捕获系统的联动，参见：

- `doc/COMPLETION-GUIDE.md`
- `doc/AUTOMATION-SYSTEM-GUIDE.md`
- `doc/CAPTURE-GUIDE.md`

---

## 常用命令（预览）

这里只列出最常用的入口，完整列表请以 `supertag-ui-*.el` 为准：

| Command                     | 功能说明                                   |
| --------------------------- | ------------------------------------------ |
| `M-x supertag-add-tag`     | 给当前节点添加一个内联 `#tag`             |
| `M-x supertag-view-node`   | 查看/编辑当前节点的结构化字段             |
| `M-x supertag-search`      | 运行结构化搜索                             |
| `M-x supertag-capture`     | 使用模板捕获带标签与字段的新节点         |
| `M-x supertag-view-table`  | 用表格视图展示一组节点                     |
| `M-x supertag-view-kanban` | 用看板视图展示任务 / 项目                  |

UI 稳定后，这一节应同步维护，以免命令说明与实际实现偏离。

---

## 迁移与兼容性

### 从 Org-SuperTag 4.x 迁移

5.0 使用了完全不同的内部存储引擎，需要一次性迁移。

**在做任何操作之前，请先备份旧的 Org-SuperTag 数据目录。**

1. 找到旧的数据目录（通常在 `~/.emacs.d/org-supertag/`）
2. 把整个目录拷贝到其他位置（或纳入版本控制）
3. 在 Emacs 中加载迁移代码：

   ```emacs-lisp
   M-x load-file RET supertag-migration.el RET
   ```

4. 运行迁移：

   ```emacs-lisp
   M-x supertag-migrate-database-to-new-arch RET
   ```

5. 重启 Emacs

如果迁移失败，或者迁移后的数据看起来不对，你可以：

- 关闭 Emacs
- 恢复备份的 Org-SuperTag 数据目录
- 重新打开 Emacs，暂时继续使用旧版本，直到问题解决

### 从纯 Org / 其他工具 迁移

如果你目前使用的是：

- 纯 Org 文件 + 临时标签
- 其他知识管理工具（Notion、Obsidian、Org-roam 等）

推荐按以下方式**渐进式采用**：

1. 先选择一个领域（例如 `projects` 或 `papers`）
2. 在相关标题上开始使用内联 `#tag`
3. 在对应 tag 上定义字段（通过 tag/schema UI），然后通过节点视图为这些节点填值
4. 之后在日常使用中，顺手把访问到的旧条目转成结构化格式，并用搜索/视图验证结果

整个过程不需要一次性“全量迁移”，可以完全按你自己的节奏推进。

---

## 数据存储与备份

Org-SuperTag 把 Org 文件视为**节点内容和标签的真实来源**。  
结构化字段值、关系以及视图配置则保存在一个配套的数据库目录中，通常是：

- `~/.emacs.d/org-supertag/`

该目录包含：

- 节点与标签的索引化表示
- 每个节点的字段值（这些结构化数据并不体现在 Org 文本中）
- 同步与视图的辅助状态
- 自动快照 / 备份数据

建议：

- 对 Org 文件和 Org-SuperTag 数据目录都做定期备份
- 如果用 git 管理 Org 文件，可以考虑只对 Org-SuperTag 目录做“快照式”备份，而不是把全部缓存文件纳入版本控制

如果内部数据库出现损坏，一般可以通过以下步骤恢复：

```emacs-lisp
M-x supertag-sync-cleanup-database
M-x supertag-sync-full-initialize
```

这样会从 Org 文件重新完整扫描并构建数据库。

---

## 故障排查

一些常见的排查入口：

- **数据库看起来不一致 / 损坏**

  ```emacs-lisp
  M-x supertag-sync-cleanup-database
  ```

- **想看看数据库里到底存了什么**

  ```emacs-lisp
  M-x supertag-search
  ```

- **某个文件的同步有问题**

  ```emacs-lisp
  M-x supertag-sync-analyze-file
  ```

更多与自动化相关的调试说明，参见：

- `doc/AUTOMATION-SYSTEM-GUIDE.md`
- `doc/AUTOMATION-SYSTEM-GUIDE_cn.md`

---

## 与其他工具的关系

极简版对比：

| 工具     | Org-SuperTag 的视角                         |
| -------- | ------------------------------------------- |
| Org-roam | Org-roam 更偏图/链接；SuperTag 更偏表和字段 |
| Obsidian | SuperTag 原生集成于 Emacs & Org            |
| Notion   | SuperTag 离线、可编程、纯文本               |

Org-SuperTag 不打算“替代一切”，它聚焦在：

- **在纯文本上叠加结构化字段层**
- **可编程的查询与视图**
- **完全留在 Emacs / Org 生态之内**

---

## 延伸阅读

如果你想深入理解整个系统，推荐从这些文档继续看起：

- `doc/ABOUT-QUERY-BLOCK.md` / `doc/ABOUT-QUERY-BLOCK_cn.md`
- `doc/AUTOMATION-SYSTEM-GUIDE.md` / `doc/AUTOMATION-SYSTEM-GUIDE_cn.md`
- `doc/CAPTURE-GUIDE.md` / `doc/CAPTURE-GUIDE_cn.md`
- `doc/COMPARE-NEW-OLD-ARCHITECTURE.md` / `doc/COMPARE-NEW-OLD-ARCHITECHTURE_cn.md`

本中文草稿的目标，是作为一个稳定的、面向用户的总览入口，  
把阅读路径引导到这些更细节的设计与使用文档中。


