# Org-SuperTag: 让 Org-mode 拥有现代笔记工具的超能力

[English](./README.md) | [中文](./README_CN.md)

## ⚠️ Org-SuperTag 5.0 升级说明

Org-SuperTag 5.0 版本进行了重大的架构重构，带来了显著的改进，但同时也有一些破坏性变更需要您注意：

### 🏗️ 核心架构变化

新版本采用了完全重新设计的架构，主要改进包括：

- **纯 Emacs Lisp 实现**：完全移除了 Python 依赖，代码库更轻量、更易维护（代码量减少了约 47%）
- **数据中心化架构**：引入了单一真相源 `supertag--store` 哈希表
- **单向数据流**：实现了严格的 Action -> Ops -> Transform -> Store -> Notify 流程，提高可预测性

详细的架构对比请查看 [新旧版本架构对比](doc/COMPARE-NEW-OLD-ARCHITECHTURE_cn.md)

### 🔄 数据库迁移要求

**在使用 Org-SuperTag 5.0 之前，您必须将现有数据库迁移到新格式：**

1. **迁移步骤**：
   - 加载迁移脚本：`M-x load-file RET supertag-migration.el RET`
   - 运行迁移命令：`M-x supertag-migrate-database-to-new-arch RET`
   - 当提示时选择您的旧版 `org-supertag-db.el` 文件
   - 系统将自动创建旧数据库的备份文件

2. **重要提醒**：迁移完成后，您**必须立即重启 Emacs**，以确保新架构能正常工作。

如果不执行此迁移，将导致与新版本不兼容，并可能导致数据丢失。

## 🚀 什么是 Org-SuperTag？

> Org-SuperTag 是一个革命性的 Org-mode 扩展，它将传统的标签系统升级为智能的知识管理引擎。  
>
> 想象一下：你的每个标签都能携带结构化数据，自动执行任务，并通过AI助手帮你发现知识间的隐藏联系。

### 🎯 核心理念：标签即数据库

传统 Org-mode 中，标签只是简单的文本标记。在 Org-SuperTag 中：

- 🏷️ **标签变成了数据表** - 每个标签可以定义字段和类型
- 🔗 **节点变成了数据记录** - 每个标题自动获得ID和结构化存储
- 🤖 **标签变成了智能助手** - 可以自动执行行为和任务
- 🔍 **查询变成了数据分析** - 支持复杂的关系查询和可视化

### ⚡ 30秒体验核心功能

```org
* 我的项目想法 #project
  :PROPERTIES:
  :ID: abc123
  :END:
  
  这是一个关于改进笔记系统的想法...
```

当你输入 `#project` 时，Org-SuperTag 自动：
1. 为标题添加唯一ID
2. 在数据库中创建节点记录
3. 建立标签关系
4. 提供字段编辑界面（状态、优先级、截止日期等）
5. 启用智能查询和可视化

### 🎬 功能演示

#### 📝 智能标签输入
**注意**：补全功能暂时不可用，因此我修改了例子。
```org
* 学习机器学习 （此时 M-x org-supertag-inline-add）
              
候选标签：
project 
learning 
research
```
- 选择一个标签后，会自动添加标签并添加到节点中。
- 输入一个新的标签，直接回车则在数据库中自动记录新标签，并将该标签添加到节点中。

#### 🗂️ 结构化字段管理
使用 `M-x org-supertag-view-node` 打开节点视图，将光标移动到 `#project` 标签下方的 `Fields` 字段，然后按照说明来编辑。

![结构化字段管理](./picture/figure16.png)

#### 🔍 强大的查询系统
使用 `M-x org-supertag-query` 打开查询视图，输入查询条件，然后 `C-c C-c` 执行。

![强大的查询系统](./picture/figure17.gif)

### 🎨 多样化视图系统

#### 📊 看板视图
使用 `M-x org-supertag-view-kanban` 打开看板视图，然后按照说明来操作。

![看板视图](./picture/figure19.gif)

#### ~~发现视图~~

该视图在 5.0 新版中暂时移除。

#### 💬 AI对话视图
使用 `M-x org-supertag-view-chat-open` 打开AI对话视图，然后按照说明来操作。

```org
你: 帮我总结一下所有进行中的项目
AI: 根据你的知识库，目前有3个进行中的项目：
    1. 机器学习项目 - 优先级高，截止12月31日
    2. 网站重构 - 优先级中，需要前端支持
    3. 数据分析 - 优先级低，等待数据源
    
    建议优先关注机器学习项目，截止日期较近。
```

##### AI 对话视图的命令系统

- **智能斜杠**：`/` 插入斜杠并可选择显示命令菜单
- **智能命令模式**：命令可以带参数立即执行
  - `/bs 微软` → 切换到 bs 模式并立即执行，将"微软"作为输入，后续对话保持在选定模式，直到使用 `/default` 切换
- **输入 /commands 看当前有什么命令**
- **输入 /define 可以自定义对话模式**
  - **支持多种格式**：
    - `/define name "prompt content"`
    - `/define name`（空提示）
    - `/define "name" "prompt"`（双引号格式）

你可以自由地创建自己的命令，这些命令都将命名为 .prompt 文件，并存储在 `~/.emacs.d/org-supertag/prompts/` 目录下。

### 🛠️ 快速开始

#### 第一步：安装配置

```shell
# 克隆仓库
git clone https://github.com/yibie/org-supertag.git ~/org-supertag
```

```emacs-lisp
(straight-use-package 'ht)
(straight-use-package 'gptel)

(straight-use-package '(org-supertag :host github :repo "yibie/org-supertag"))
(setq org-supertag-sync-directories '("Your/Path/To/Org-Files/"))
(eval-after-load 'gptel
  '(require 'org-supertag))
```

#### 第二步：创建你的第一个智能笔记

1. 打开任意 .org 文件
2. 创建标题：* 我的第一个项目
3. 输入 # 并选择或创建标签
4. 🎉 恭喜！你已经创建了一个智能节点

#### 第三步：探索强大功能

- `M-x org-supertag-view-node` - 查看节点详情（包含AI标签建议）
- `M-x org-supertag-query` - 智能搜索
- `M-x org-supertag-view-kanban` - 看板视图
- `M-x org-supertag-view-chat-open` - AI对话

### 🎯 使用场景

#### 📚 学术研究
```org
#paper + 字段[期刊, 影响因子, 阅读状态, 笔记]
#experiment + 字段[假设, 方法, 结果, 结论]
#idea + 字段[灵感来源, 可行性, 优先级]
```

#### 💼 项目管理  
```org
#project + 字段[状态, 优先级, 负责人, 截止日期]
#task + 字段[类型, 估时, 依赖, 完成度]
#meeting + 字段[参与者, 议题, 决议, 后续行动]
```

### 🚀 高级功能

#### 🤖 智能自动化系统 (Automation 2.0)

5.0 版本带来了全新的自动化系统，采用纯 Emacs Lisp 实现，性能更优，功能更强大：

- ✅ **统一的标签系统**：每个标签都是功能完备的"数据库"，拥有自定义字段和自动化能力
- ✅ **真正的事件驱动**：基于精确的数据变化实时响应，而非轮询扫描
- ✅ **自动规则索引**：在后台自动为规则建立高性能索引，无需用户关心性能优化细节
- ✅ **多重动作执行**：一条规则可以触发一系列按顺序执行的动作
- ✅ **计划任务**：支持基于时间和周期的自动化，由集成的调度器驱动
- ✅ **关系与计算**：支持双向关系、属性同步、Rollup 计算等高级功能
- ✅ **公式字段**：在表格视图中实时计算和显示数据，无需持久化存储

| 特性 | 旧版本 (Behavior) | 新版本 (Automation 2.0) |
|------|-------------|-------------|
| **模块结构** | 分散的多个模块，存在循环依赖 | 统一的单一模块，消除依赖问题 |
| **规则管理** | 手动附加到标签，需要用户管理 | 自动索引，系统智能管理 |
| **性能** | O(n) 遍历所有规则 | O(1) 索引查找，高性能 |
| **API一致性** | 多套不同的API接口 | 统一的API接口，学习成本低 |
| **维护性** | 复杂的模块间关系 | 简单的内聚设计，易于维护 |

详情请查看 [Automation System Guide](doc/AUTOMATION-SYSTEM-GUIDE_cn.md)

#### 📸 捕获系统 (Capture System)

5.0 版本引入了全新的捕获系统，支持动态模板和内容生成器：

- ✅ **模板驱动** - 使用预定义模板快速创建结构化节点
- ✅ **智能填充** - 自动从剪贴板、选区或函数获取内容
- ✅ **标签智能** - 交互式标签选择和自动完成
- ✅ **字段丰富** - 自动设置标签字段值

详情请查看 [Capture Guide](doc/CAPTURE-GUIDE_cn.md)

### ⌨️ 键盘快捷键

Org-SuperTag 提供了一套全面的键盘快捷键，可通过 `C-c s` 前缀键访问。按下 `C-c s` 后，您可以使用以下快捷键：

| 按键 | 命令 | 描述 |
|------|------|------|
| `C-c s a` | org-supertag-inline-add | 为当前节点添加标签 |
| `C-c s r` | org-supertag-inline-remove | 从当前节点删除标签 |
| `C-c s n` | org-supertag-inline-rename | 重命名标签 |
| `C-c s d` | org-supertag-inline-delete-all | 在所有地方删除标签 |
| `C-c s c` | org-supertag-inline-change-tag | 更改光标处的标签 |
| `C-c s C` | org-supertag-capture-direct | 直接捕获 |
| `C-c s t` | org-supertag-capture-template | 使用模板捕获 |
| `C-c s i` | org-supertag-insert-query-block | 插入查询块 |
| `C-c s m` | org-supertag-move-node-and-link | 移动节点并链接 |
| `C-c s A` | org-supertag-node-add-reference | 为节点添加引用 |
| `C-c s R` | org-supertag-node-remove-reference | 从节点删除引用 |
| `C-c s h` | org-supertag-node-back-to-heading | 返回标题 |
| `C-c s N` | org-supertag-node-create | 创建新节点 |
| `C-c s D` | org-supertag-node-delete | 删除节点 |
| `C-c s f` | org-supertag-node-find | 查找节点 |
| `C-c s o` | org-supertag-node-find-other-window | 在其他窗口查找节点 |
| `C-c s M` | org-supertag-node-move | 移动节点 |
| `C-c s u` | org-supertag-node-update | 更新光标处的节点 |
| `C-c s s` | org-supertag-query | 打开查询界面 |
| `C-c s e` | org-supertag-query-export-results-to-file | 导出查询结果到文件 |
| `C-c s E` | org-supertag-query-export-results-to-new-file | 导出查询结果到新文件 |
| `C-c s I` | org-supertag-query-insert-at-point | 在光标处插入查询 |
| `C-c s x` | org-supertag-tag-set-extends | 设置标签扩展 |
| `C-c s g` | org-supertag-view-chat-open | 打开聊天视图 |
| `C-c s v` | org-supertag-view-node | 查看节点详情 |
| `C-c s T` | org-supertag-view-table | 打开表格视图 |
| `C-c s k` | org-supertag-view-kanban | 打开看板视图 |
| `C-c s C-c` | org-supertag-clean-database | 清理数据库 |

所有快捷键都可通过 `C-c s` 前缀访问，使您能够轻松记住并高效使用 Org-SuperTag 的功能。

### 🔧 配置指南

#### 基础配置
```emacs-lisp
;; 核心配置
(setq org-supertag-sync-directories '("~/notes/" "~/projects/"))
```

#### AI 服务配置
```emacs-lisp
;; 控制是否启用 AI 服务
;; 设置为 nil 可禁用 AI 服务，默认为 t
(setq org-supertag-bridge-enable-ai nil)
```

#### 高级配置
```emacs-lisp
;; 自定义字段类型
(add-to-list 'org-supertag-field-types
  '(rating . (:validator org-supertag-validate-rating
              :formatter org-supertag-format-rating
              :description "评分 (1-5)")))

;; 自定义查询命令
(defun my-urgent-projects ()
  "查找所有紧急项目"
  (interactive)
  (org-supertag-query '(and (tag "project") (tag "urgent"))))
```

### 🆚 对比其他工具

| 功能 | Org-SuperTag | Org-roam | Obsidian | Notion |
|------|--------------|----------|----------|--------|
| 结构化数据 | ✅ 原生支持 | ❌ | ⚠️ 插件 | ✅ |
| 复杂查询 | ✅ S表达式 | ⚠️ 基础 | ⚠️ 基础 | ✅ |
| 自动化行为 | ✅ 强大 | ❌ | ⚠️ 有限 | ⚠️ 有限 |
| AI集成 | ✅ 深度集成 | ❌ | ⚠️ 插件 | ✅ |
| 离线使用 | ✅ | ✅ | ✅ | ❌ |
| 学习曲线 | ⚠️ 中等 | ⚠️ 中等 | ✅ 简单 | ✅ 简单 |

### 🤝 社区与支持

- 📖 [详细文档](https://github.com/yibie/org-supertag/wiki)
- 🐛 [问题反馈](https://github.com/yibie/org-supertag/issues)
- 💬 [社区讨论](https://github.com/yibie/org-supertag/discussions)

### Changelog
详细见 [CHANGELOG](./CHANGELOG.org)

#### 🆘 常见问题

##### Q: 数据库损坏怎么办？
A: 使用 =M-x org-supertag-recovery-full-suite= 进行完整恢复。

##### Q: 如何备份数据？
A: 数据库文件位于 =~/.emacs.d/org-supertag/=，定期备份即可。

##### Q: 如何获取AI标签建议？
A: 在节点视图（=M-x org-supertag-view-node=）中，点击"💡 Get AI Tag Suggestions"或按 =s= 键。这是手动触发的，不会干扰你的工作流程。

##### Q: AI功能需要什么配置？
A: AI功能使用默认的Ollama配置，无需额外设置。所有AI功能都集成在现有的视图系统中，使用简单直观。

### 🎉 立即开始

> 不要让复杂的功能吓到你！Org-SuperTag 的设计理念是"简单开始，逐步深入"。
>
> 从添加第一个 =#标签= 开始，逐步探索结构化数据、智能查询、AI助手等高级功能。
>
> 每一个功能都是为了让你的知识管理更加高效和智能。


---

*Made with ❤️ by [Yibie](https://github.com/yibie) | 受到 [Tana](https://tana.inc)、[ekg](https://github.com/ahyatt/ekg)、[org-node](https://github.com/meedstrom/org-node) 的启发*