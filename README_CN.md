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
| 操作光标下的对象 | `M-x supertag-smart-key` | 执行当前 tag、node、field、link、button 或 table cell 的默认动作 |
| 选择光标对象的相关动作 | `M-x supertag-assist` | 只显示对象相关动作，并保留完整菜单出口 |
| 全量重建数据库 | `M-x supertag-sync-full-rescan` | 安全——仅重新读取 Org 文件 |

除了单条命令，Org-Supertag 还提供一套小巧的 S-expression 查询语言，可以组合标签、字段、日期和全文搜索，例如
`(and (tag "task") (not (field "status" "done")))`。可以把它写进 `org-supertag-query-block`
babel 代码块，用 `M-x supertag-query-save` 保存以便复用，或者用 `M-x supertag-query-build`
交互式构建。完整语法见 `doc/QUERY.md`（英文）。

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

### 语义 Smart Key

把光标放在 inline tag、node、field、concept mention、Org link、Emacs button 或 table cell
上，执行 `M-x supertag-smart-key` 即可触发默认动作。执行 `M-x supertag-assist`，或以前缀参数
调用 `C-u M-x supertag-smart-key`，只会显示与当前对象相关的动作；其中始终保留完整
`supertag-menu` 的入口，光标下没有语义对象时也会直接回落到该菜单。两个命令都不设置默认按键，
因此不会覆盖 Org 与各 View 的既有局部键。

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

**Org 文件拥有你的文本和结构；数据库拥有你的结构化数据。** Org 文件（标题、正文、`:ID:` 属性）是节点"是什么"的唯一真实数据源，`M-x supertag-sync-full-rescan` 随时可以从它们重新推导出节点和标签。但 schema 定义、字段值、表格/看板视图布局、自动化规则**只**存在于 `supertag-db.el` 里——重扫不会凭空从 org 正文里造出它们，因为纯 org 文本根本没有编码这些信息。丢失数据库而又没有备份或同步副本，就是真的丢数据了，和丢失任何其他无法重新生成的文件一样；请按上文备份，或按下文同步。

---

## 多机同步

多机同步 `supertag-db.el` 有两种办法：**git 原生同步**（推荐——它真正懂合并），或者把数据目录放进同步文件夹服务（配置更简单，但"最后写入者赢"）。

### git 原生同步（推荐）

**机器 1**（第一次配置时）：

```
M-x supertag-git-setup
```

这会把你的 vault 纳入 git 管理：如果还没有仓库就初始化一个；如果数据库还没有被纳入仓库内，就把它搬迁到 `<repo-root>/.supertag/supertag-db.el`；并为 `supertag-db.el` 配置一个语义合并驱动，让不同机器上的并发编辑按字段合并，而不是互相覆盖。随后它会提示输入远端 URL——给它一个（任何空的 git 远端都行：GitHub、自建服务器、NAS）它就会创建首次提交并推送；留空则暂时保持本地状态（完全有效、受支持的状态——等有远端了再重新运行这个命令）。

**机器 2**（以及之后的每一台机器）：

```
M-x supertag-git-clone
```

给它同一个远端 URL 和一个本地目录。它会 clone、为*这台机器*配置合并驱动，并加载数据库（如果数据库缺失或无法读取，就从 clone 下来的 org 文件重建）。

**每个 clone 都必须自己跑一遍配置。** `merge.supertag-db.driver` 存在 `.git/config` 里，git 从不同步这个文件——所以机器 2 上 `supertag-git-clone` 配置驱动这一步不是可有可无的杂务，正是它让*这台*机器的合并变成语义合并，而不是退化成 git 默认的按行文本合并（退化后是什么样子见下面"冲突"一节）。

**可选的自动化：** `M-x supertag-git-sync-mode` 会开启一个后台循环，自动 debounce 提交你的改动、按定时器和焦点事件 fetch/merge、并推送——包括在联网恢复后一次性追赶断网期间积累的提交，不需要等一次新的编辑来触发。不开这个模式，手动 `git pull`/`git push`（或 `magit-pull`/`magit-push`）效果完全一样；这个模式只是便利层，不是正确性所在。

**冲突。** 数据库自身的编辑在常见情况下——两侧改的是不同节点或字段——会自动合并。当**同一个**字段在两侧被改成不同的值，或者纯 `.org` 正文在两侧改了同一行，git 会把那个文件留在真实的、未解决的冲突状态：对 `supertag-db.el` 本身，它会拒绝加载直到冲突解决（报错信息会点名文件并指向这里）；对 `.org` 文件，同步扫描器会跳过导入任何仍带冲突标记的文件，而不是把垃圾内容导入进来。不管哪种情况，`M-x supertag-doctor`（"8. Git Sync" 一节）都会列出具体哪些还没解决——像处理其他 git 冲突一样，手工解决或用 `magit`/`git checkout --merge`。

**所有同步机器必须一起升级。** 6.0 起数据库文件采用新的磁盘格式（确定性、每实体一行——正是它让字段级 git 合并成为可能），只有 6.0+ 能读。一台还在 5.9.x 的机器拉取到 6.0 机器保存的数据库后，会*看起来*加载成功但库是空的——旧代码只读文件首行且不报错。它的保存守卫能防住实际数据丢失（空内存库拒绝覆盖非平凡文件），但在那台机器升级之前，一切看上去都"消失"了。所以：在任何一台机器用 6.0 保存之前，把共享这个 vault 的**每一台**机器都升级到 6.0+。升级本身自动完成（首次加载旧库时自动迁移，并在 `backups/` 留下 `supertag-db-premigrate-*` 与 `supertag-db-preformat6-*` 两类永不被自动清理的降级快照；想回退旧版就把最新快照复制回 `supertag-db.el`）。`M-x supertag-doctor` 会报告当前磁盘格式与快照数量。

### 同步文件夹服务（Dropbox / iCloud / Syncthing）

如果你不想用 git，也可以把 `~/.emacs.d/org-supertag/`（或你配置的 `supertag-db-file` 所在目录）放进 Dropbox / iCloud / Syncthing 之类的同步文件夹，让它跟着你在多台机器间走——请先了解其中的取舍：

**最安全的做法：同一时间只有一台机器在写。** `supertag-db.el` 是单个序列化文件。同步服务的工作方式是"整份文件复制，最后写入者赢"——它并不知道两个 Emacs 会话分别改动了文件的哪些部分，所以无法帮你合并。只要两台机器都保存过，后保存的会静默覆盖先保存的。可靠的做法是：**在机器 B 上开始编辑之前，先在机器 A 上彻底退出 Emacs（`C-x C-c`，而不是只关掉窗口）。**

即使你觉得自己在机器 A 上"只是看看，没有编辑"，这条建议依然成立：自动保存定时器（`supertag-db-auto-save-interval`，默认 300 秒）只要会话中有任何改动被标记为脏（dirty），就会在后台把数据库写入磁盘——所以一个开着的 Emacs 进程本身就是一个后台写入者，不管你有没有在主动敲键盘。

**5.9.0 的数据库锁并不能解决这个问题。** 从 5.9.0 起，Org-SuperTag 会对数据库文件加一个建议性的锁（`supertag-db-lock`），防止*同一台机器*上的两个 Emacs 实例互相踩踏。这个锁是本机的文件锁机制（在数据库文件旁边放一个符号链接）——它只能保护"同机双开"，大多数同步服务既不保证可靠、也不保证及时地把锁文件本身同步过去，所以它对跨机器场景没有意义。

**presence（在场）告警。** 为了至少给同步文件夹的用户一个提醒（这不是锁——同步服务动辄几分钟的传播延迟决定了它在物理上不可能是锁），Org-SuperTag 会在数据库文件旁边写一个很小的 `supertag-presence.json` 文件，记录"最后是哪台主机碰过它、什么时候"。当你加载数据库时，如果发现另一台主机大约在最近 5 分钟内（`supertag-presence-stale-seconds`）还活跃过，就会弹出一条醒目的告警，点名那台主机并说明风险。**看到告警后怎么办：** 如果你确定另一台机器已经退出 Emacs，可以放心继续——这条告警只出现一次，不会重复弹出，直到另一台主机再次声明 presence 为止。如果不确定，先去那台机器上退出 Emacs。随时可以用 `M-x supertag-doctor` 查看当前 presence 文件记录的主机、距今时长和判定结果（本机 / 异机活跃 / 异机过期）。将 `supertag-presence-enable` 设为 `nil` 可以完全关闭这个功能。

**不要同步 `sync-state.el` 和 `backups/`。** 这两者虽然和数据库放在同一个数据目录下，但都是本机专属的记录（`sync-state.el` 追踪的是*这台机器*文件系统的 mtime/hash；`backups/` 只是磁盘占用，没必要在多台机器间重复保留）。如果你的同步工具是整个数据目录一起同步，请在工具允许的范围内把这两个路径排除掉；就算被覆盖了，最坏结果也只是多做一次全量重扫，不会丢数据。

这只是权宜之计，不是最终方案——真正的多机同步需要一个懂"合并"的传输层，而这正是上面的 git 原生同步做的事。如果你要的是多机并发编辑，请直接用它；同步文件夹服务能给你的，永远只是上面的单写者纪律。

---

## 从旧版本迁移

> **⚠️ 5.9.x → 6.0.0**：数据库文件格式已变更（见上文"多机同步"）——升级自动完成，但之后想降级需要恢复备份快照。如果可能需要回退，升级前请备份 `~/.emacs.d/org-supertag/`。多机共享 vault 的用户必须所有机器一起升级。

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
