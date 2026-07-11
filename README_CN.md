# Org-SuperTag 5.0 – 在 Emacs 里，把你的 Org 文件变成结构化知识库

[中文](./README_CN.md) | [English](./README.md)

Org-SuperTag 把普通的 Org 标题变成一个**可结构化查询的知识库**。不依赖外部服务，不需要 Python，你的 `.org` 文件始终是纯文本——我们只是让它变聪明了。

> **⚠️ 从 5.2.0 或更早版本升级？**  
> 在启用 `supertag-use-global-fields` 之前，请**先完成全局字段迁移**。  
> 具体步骤见 [`doc/GLOBAL-FIELD-MIGRATION-GUIDE_CN.md`](doc/GLOBAL-FIELD-MIGRATION-GUIDE_CN.md)。

> **为什么这很重要**：你有没有试过"找出所有还没读的论文"？或者"@小王负责的、本周到期的任务"？纯 Org-mode 做不到，除非你手动维护 PROPERTIES 抽屉再 grep。Org-SuperTag 让这件事变成点几下鼠标。

> **📖 准备开始？** 先读 **[Org-SuperTag 的一天](doc/A-DAY-WITH-ORG-SUPERTAG_CN.org)**——一个人的完整日常工作流，所有 Elisp 配置都可以 copy-paste，用 `C-c C-v C-t` 提取到你的配置里。(English: [A Day with Org-SuperTag](doc/A-DAY-WITH-ORG-SUPERTAG.org))

---

## 跟纯 Org-mode 比，到底方便在哪

| 没有 Org-SuperTag | 有 Org-SuperTag |
|---|---|
| 每个字段都要手写 `:PROPERTIES:` 抽屉 | 打一次 `#tag`，定义一次字段，之后在表格视图里填 |
| `grep` + 正则找"高优先级本周任务" | `M-x supertag-search`，结构化查询，秒出结果 |
| 不同笔记之间靠复制粘贴关联 | `M-x supertag-add-reference`，一键建立双向链接 |
| 每开一个新项目都要从零搭跟踪系统 | 定义一次 `#project` 的字段模板，终身复用 |
| "那个会议记录到底写在哪了？" | 按日期、参与人、决议查 `#meeting` |

**核心理念**：你继续像往常一样写 Org 文件。Org-SuperTag 在后台读取它们，构建结构化索引，然后给你一个"类数据库"的视图层——建立在你的纯文本之上。

---

## 30 秒安装

```emacs-lisp
;; 用 straight.el 安装
(straight-use-package '(org-supertag :host github :repo "yibie/org-supertag"))

;; 告诉 SuperTag 你的 Org 文件在哪
(setq org-supertag-sync-directories '("~/org/"))

;; 初始化一次——扫描你所有的文件
M-x supertag-sync-full-initialize
```

就这些。不需要 API key，不需要数据库服务器，不需要配置向导。**你现有的 Org 文件直接就能用。**

---

## File-node：兼容 Org-roam、Denote 或混合目录

这个设置只影响文件级节点；标题节点仍然照常使用 Org ID。

```emacs-lisp
;; 默认：识别文件开头的 :ID:（Org-roam 风格）
(setq org-supertag-file-id-source 'org-roam)

;; 识别 #+IDENTIFIER:（Denote 风格）
(setq org-supertag-file-id-source 'denote)

;; 混合目录：逐个文件自动识别两种格式
(setq org-supertag-file-id-source 'auto)

;; 完全不创建文件级节点
(setq org-supertag-file-id-source 'disabled)
```

Org-roam 与 Denote 文件放在同一同步目录时，使用 `auto`。链接由每个节点自己的身份决定：Org-ID 节点使用 `id:`，Denote 文件节点使用 `denote:`。文件没有所选的持久化身份时，它仍是普通 Org 文件；SuperTag 不会为它生成临时 ID。

修改设置后，执行 `M-x supertag-sync-full-rescan`。

---

## 三个核心概念（2 分钟搞懂）

Org-SuperTag 只建立在三个简单想法上：

### 1. `#tag` 把标题变成一条记录

```org
* Attention Is All You Need #paper
```

`#paper` 标签的意思是"这个标题属于 'paper' 这个集合"。就像你在任何系统里打标签一样——只不过这个标签有超能力。

### 2. 标签可以定义字段（就像数据库的列）

`#paper` 这个标签打好之后，你就可以定义要追踪什么信息：

```
authors  →  文本
year     →  数字
venue    →  文本
status   →  选择（未读 / 阅读中/ 已读）
rating   →  数字（1–5）
```

这些字段**每个标签只需要定义一次**（在 Schema View 里，`M-x supertag-view-schema`）。之后，每一个打上 `#paper` 的节点自动拥有这些字段。

### 3. 视图让你浏览、填写、查询数据

- **表格视图** (`M-x supertag-view-table`)：像一个针对你标签节点的电子表格。点列头排序，过滤，批量编辑。
- **节点视图** (`M-x supertag-view-node`)：编辑单个节点的字段，带自动补全和校验。
- **看板视图** (`M-x supertag-view-kanban`)：拖拽式的看板，适合 `#task`、`#project`。

---

## 一步一步：你的第一个 5 分钟

假设你是一个研究者，论文散落在各种笔记里。

### 第一步：给论文打标签

光标移到任意 Org 标题，`M-x supertag-add-tag`，输入 `paper`：

```org
* Attention Is All You Need #paper
```

### 第二步：定义"论文"要追踪什么

`M-x supertag-view-schema` → 找到 `paper` → 添加字段：

| 字段 | 类型 |
|------|------|
| `authors` | 文本 |
| `year` | 数字 |
| `status` | 选择：未读、阅读中、已读 |
| `rating` | 数字 1–5 |

### 第三步：填数据

`M-x supertag-view-table` → 选择标签 `paper`。你会看到一张包含所有 `#paper` 节点的表格。点任意单元格即可编辑。按 `year` 排序找最新论文。按 `status = 未读` 过滤看阅读队列。

### 第四步：给更多论文打标签

找到其他论文标题，加上 `#paper`。它们会自动出现在表格里。

**搞定。你现在有了一个可查询的研究文献库。** 整个过程没有手写 PROPERTIES 抽屉，没有复制粘贴，没有手动整理。

---

## 真实工作流（附可复制的命令）

### 📚 学术阅读队列

```org
* Diffusion Models Survey #paper
* ViT Explained #paper
* CLIP Paper #paper
```

在 `#paper` 上定义字段：`authors`、`year`、`status`（未读/阅读中/已读）、`rating`。

**日常使用**：
1. `M-x supertag-view-table` → 选 `paper` → 按 `status` 排序
2. 过滤到 `未读` → 挑一篇 → 按 `o` 跳转到标题
3. 读完：点 `status` 格 → 选 `已读` → 打分

**为什么方便**：你按状态和评分找论文，而不是在 50 个标题里来回翻、逐个读标题。

### 📋 项目任务跟踪

```org
* 重写同步层 #task #project
* 修复认证 bug #task
* 部署 v2.1 #task
```

在 `#task` 上定义字段：`status`、`priority`、`due`、`assignee`。

**日常使用**：
1. `M-x supertag-view-kanban` → 选 `task` → 按 `status` 分列
2. 拖拽任务在不同列之间推进
3. `M-x supertag-search` → `(and (tag "task") (field "priority" "high"))` 找紧急项

**为什么方便**：你的任务分散在不同的 Org 文件里（会议记录、项目文件），但在一个看板上你看到全部。

### 📝 会议记录与决议追踪

```org
* 2024-11-15 迭代规划 #meeting
```

在 `#meeting` 上定义字段：`date`、`participants`、`decisions`、`action-items`。

**日常使用**：
1. `M-x supertag-capture` → 选 `meeting` 模板 → 填字段
2. 之后：`M-x supertag-search` → `(tag "meeting")` → 按日期范围过滤
3. "Q4 所有决议" 几秒找到

**为什么方便**：会议记录在它们的自然位置（项目文件里），但你跨所有文件统一查询。

---

## 每天都会用的命令

| 你想做什么 | 命令 | 效果 |
|---|---|---|
| 打标签 | `M-x supertag-add-tag` | 添加 `#tag` 到标题，节点自动出现在该标签的表格里 |
| 看一个标签的所有节点 | `M-x supertag-view-table` | 电子表格视图。可排序、过滤、直接编辑单元格 |
| 编辑单个节点的字段 | `M-x supertag-view-node` | 表单视图，带自动补全、选择器和校验 |
| 看板视图 | `M-x supertag-view-kanban` | 拖拽式看板，列之间移动 |
| 定义标签字段 | `M-x supertag-view-schema` | 增删字段、设置类型、配置继承关系 |
| 快速捕获新节点 | `M-x supertag-capture` | 模板化快速录入，自动写入 Org 文件 |
| 搜索 | `M-x supertag-search` | 结构化查询，结果可导出到文件 |
| 关联节点 | `M-x supertag-add-reference` | 任意两节点间建立双向链接 |
| 将选中文本提升为概念 | `M-x supertag-promote-concept` | 创建/复用概念节点，从当前节点建立 reference，原文保持普通文本 |
| 高亮概念提及 | `M-x supertag-concept-link-mode` | 将概念 title/alias 的提及显示为琥珀色语义高亮，不落库为链接 |
| 全量重建数据库 | `M-x supertag-sync-full-rescan` | 安全——仅重新读取 Org 文件 |

可选快捷键示例：

```emacs-lisp
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c s p") #'supertag-promote-concept)
  (define-key org-mode-map (kbd "C-c s o") #'supertag-concept-open-at-point))
```

### Concept mention 的行为边界

- Promote 只接受 Org node 内的非空文本。它会复用唯一的同名标题节点或创建新节点，建立一条显式 reference，并保持选区原文不变；同名 file-node 不会被静默转换为 concept。
- Mention 只存在于显示层。Org link、code/verbatim、普通注释与 `COMMENT` 子树、keyword/drawer、source block 和表格都不会高亮。
- 多个 concept 共用 title 或 alias 时，该词存在歧义。SuperTag 不会根据 hash table 顺序任意选择目标；文本保持普通显示，promote 会明确报告冲突。

如果在 SuperTag 之外修改了 concept title 或 alias，请在已启用的 buffer 中执行 `M-x supertag-concept-refresh`。

---

## 为什么不会增加负担

"结构化工具"最常见的担忧是：**"我会不会花更多时间在整理上，而不是真正工作？"**

Org-SuperTag 从三个层面避免这个问题：

### 1. 你的文件始终是纯 Org

你**永远不需要**通过 SuperTag 的视图来操作。正常写 Org。`#tag` 标记只是文本。如果你明天不用 SuperTag 了，你的文件是 100% 可读的 Org-mode——只不过多了一些 `#tag` 标记而已，丝毫不影响。

### 2. 字段定义一次，永久复用

你在 `#task` 上定义 `status`、`priority`、`due` 只需**一次**。之后每一个 `#task` 节点自动拥有这些字段。前期投入 30 秒，收益是永久性的。

### 3. 同步自动且安全

Org-SuperTag 按定时器读取你的文件（可通过 `doc/SYNC-CONFIGURATION.md` 配置）。它**绝不**修改你的 Org 文件，除非你通过 SuperTag 视图显式编辑。如果数据库损坏了，`M-x supertag-sync-full-rescan` 从零重建。

### 算一笔账

**不用 SuperTag**——追踪论文：
- 手写 `:PROPERTIES:` 抽屉：`:authors:`、`:year:`、`:status:`
- 跨文件 `grep` `status.*unread`
- 不能排序，不能过滤，没有表格视图

**用 SuperTag**——追踪论文：
- 给标题加 `#paper`（每个 2 秒）
- Schema View 定义字段一次（30 秒）
- 表格视图：排序、过滤、编辑（即时）

**收益**：10 篇论文，省下约 5 分钟 PROPERTIES 打字时间，还白送一个实时更新的表格视图。100 篇论文，差距是小时级的。

---

## 进阶路线图

从简单开始，需要时再加能力：

| 当你熟悉了…… | 可以试试这个 |
|---|---|
| 标签和表格视图 | **自动化规则**——条件触发自动填字段 (`doc/AUTOMATION-SYSTEM-GUIDE_cn.md`) |
| 手动捕获 | **捕获模板**——预定义常用录入表单 (`doc/CAPTURE-GUIDE_cn.md`) |
| 基本查询 | **查询块**——在 Org 文件里嵌入动态查询结果 (`doc/ABOUT-QUERY-BLOCK_cn.md`) |
| 默认视图 | **自定义视图**——打造自己的仪表盘 (`doc/VIEW_FRAMEWORK_DEV_GUIDE.md`) |
| 单资料库 | **多 Vault**——工作/个人分开管理 (`doc/SYNC-CONFIGURATION.md`) |
| 写插件 | **插件开发指南**——自定义抽取器和扩展 (`doc/ORG-SUPERTAG-PLUGIN-GUIDE_cn.md`) |

---

## 数据存在哪里

| 什么数据 | 存在哪 | 格式 |
|---|---|---|
| 你的 Org 文件 | 你配置的目录 | 纯 `.org` 文本 |
| 结构化字段值 | `~/.emacs.d/org-supertag/supertag-db.el` | Emacs Lisp 数据 |
| 同步状态 | `~/.emacs.d/org-supertag/sync-state.el` | 文件 mtime 和 hash |
| 每日备份 | `~/.emacs.d/org-supertag/backups/` | 带时间戳的数据库快照 |

**你的 Org 文件始终是唯一真实数据源。** 数据库只是一个缓存，随时可以通过 `M-x supertag-sync-full-rescan` 重建。

---

## 从旧版本迁移

> **⚠️ 5.2.0 → 5.3.0**：在启用 `supertag-use-global-fields` 之前，请先完成[全局字段迁移](doc/GLOBAL-FIELD-MIGRATION-GUIDE_CN.md)。

### 从 SuperTag 4.x 升级

```emacs-lisp
;; 1. 备份数据目录 (~/.emacs.d/org-supertag/)
;; 2. 加载并运行迁移
M-x load-file RET supertag-migration.el RET
M-x supertag-migrate-database-to-new-arch RET
```

### 从纯 Org 文件开始

无需迁移。给标题加 `#tag`，定义字段，开始使用视图。你现有的文件原样兼容。

---

## 常见问题速查

| 问题 | 解决方法 |
|---|---|
| 数据库看起来不对 | `M-x supertag-sync-cleanup-database` 然后 `M-x supertag-sync-full-rescan` |
| 自动同步没启动 | 检查 `org-supertag-sync-directories` 是否正确配置 |
| 某个文件没同步 | `M-x supertag-sync-analyze-file` |
| 全量重建后字段值不见了 | 字段值在数据库中，重建后丢失说明数据库被清空过 |
| 同步导致 Emacs 卡顿 | 参见 `doc/SYNC-CONFIGURATION.md` 的性能调优 |

---

## 与其他工具的关系

| 工具 | Org-SuperTag 的定位 |
|---|---|
| **Org-roam** | Org-roam 是笔记关联图谱；SuperTag 是结构化表格。可以共存。 |
| **Notion** | Notion 把数据锁在云端。SuperTag 离线，数据在你自己的文件里。 |
| **Obsidian** | Obisidian 是另一个编辑器。SuperTag 原生在 Emacs 里，不用切换工具。 |
| **org-ql** | org-ql 查询 Org 内联属性。SuperTag 把字段数据单独存储，不污染 Org 文件，还支持视图和自动化。 |

---

## 延伸阅读

- **📖 Org-SuperTag 的一天**（中文）：`doc/A-DAY-WITH-ORG-SUPERTAG_CN.org` — 完整工作流教程，含可 tangle 的 Elisp 配置
- **📖 A Day with Org-SuperTag** (English)：`doc/A-DAY-WITH-ORG-SUPERTAG.org`
- **同步配置**：`doc/SYNC-CONFIGURATION.md`
- **自动化规则**：`doc/AUTOMATION-SYSTEM-GUIDE_cn.md`
- **捕获系统**：`doc/CAPTURE-GUIDE_cn.md`
- **虚拟列**：`doc/VIRTUAL_COLUMNS.md`
- **插件开发**：`doc/ORG-SUPERTAG-PLUGIN-GUIDE_cn.md`
- **架构深度解析**：`doc/ONTOLOGY-ARCHITECTURE_cn.md`
- **视图框架**：`doc/VIEW_FRAMEWORK_DEV_GUIDE.md`
- **新旧架构对比**：`doc/COMPARE-NEW-OLD-ARCHITECHTURE_cn.md`

---

Org-SuperTag 以 GPLv3 自由软件协议开发。欢迎在 GitHub 上贡献代码、提交 bug 或功能请求。
