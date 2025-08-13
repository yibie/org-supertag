# Org-SuperTag: 让 Org-mode 拥有现代笔记工具的超能力

**作者**: Yibie  
**邮箱**: yibie@outlook.com

[English](./README.org) | [中文](./README_CN.org)

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
```org
* 学习机器学习 #
              ↑ 输入#后自动补全
              
候选标签：
- #project (12个节点)
- #learning (8个节点) 
- #research (5个节点)
- #创建新标签...
```

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

#### 🔍 发现视图
使用 `M-x org-supertag-view-discover` 打开发现视图，然后按照说明来操作。

![发现视图](./picture/figure20.gif)

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

# 设置Python后端
cd ~/org-supertag/simtag
sh ./setup_uv.sh
```

```emacs-lisp
;; please add ht and epc to the header of the package docstring so users do not need to manually install those dependencies
(straight-use-package 'ht)
(straight-use-package 'epc)

(straight-use-package '(org-supertag :host github :repo "yibie/org-supertag"
                                     :files (:defaults "simtag" ".venv")
                                     :pre-build ("bash" "simtag/setup_uv.sh")))

;; in README it is "org-supertag.el", but actually we should use "org-supertag" here.
;; Beside, the double quote symbol is also not the canonical one resulting in lisp evaluation error.
(let ((package-path (locate-library "org-supertag")))
  (when package-path
    (let* ((package-dir (file-name-directory package-path))
            ;; the venv should be located at the project root, not under `simtag/`
           (python-path (expand-file-name ".venv/bin/python" package-dir)))
      (when (file-exists-p python-path)
        (setq org-supertag-bridge-python-command python-path)))))

;; load this package
(require 'org-supertag)
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

#### 🤖 智能行为系统

让标签自动执行任务：

```emacs-lisp
;; 定义"紧急"行为
(org-supertag-behavior-register "urgent"
  :trigger :on-add
  :actions '("设置TODO状态" "标记高优先级" "设置今日截止")
  :style '(:color "red" :icon "🔥"))
```

当你添加 =#urgent= 标签时，自动：
- 设置 TODO 状态
- 优先级改为 High  
- 截止日期设为今天
- 显示红色火焰图标

详情请查看 ![Advance Usage ‐ Behavior System Guide](https://github.com/yibie/org-supertag/wiki/Advance-Usage-%E2%80%90-Behavior-System-Guide)

#### 🔄 嵌入块系统

使用 `M-x org-supertag-embed-insert-block` 输入嵌入块。

在任意文件中嵌入其他节点的内容：

```org
,#+begin_embed_node: project-abc123 embed-001
这里会自动显示项目节点的内容，并保持同步更新
,#+end_embed_node
```

#### 🧠 AI智能助手

- *对话式查询*：=M-x org-supertag-view-chat-open= 用自然语言查询知识库
- *标签建议*：在节点视图中点击"💡 Get AI Tag Suggestions"或按 =s= 键
- *节点对话*：在节点视图中按 =c= 键与当前节点对话

#### 📊 查询块（嵌入式查询）

使用 `M-x org-supertag-insert-query-block` 在 Org 文档中直接插入查询块，按下 `C-c C-c` 执行，查看输出结果。

查询![语法详情](https://github.com/yibie/org-supertag/wiki/Org-SuperTag-Query-User-Guide)。

```org
;; 分析项目完成趋势
#+BEGIN_SRC org-supertag-query :results raw
(and (tag "project")
     (field "状态" "Done")
     (after "-3m"))
#+END_SRC

;; 找到知识盲区
#+BEGIN_SRC org-supertag-query :results raw
(and (tag "concept")
     (not (field "理解程度" "熟练"))
     (field "重要性" "高"))
#+END_SRC
```

#### 🖥️ 查询缓冲区（交互式界面）

使用 `M-x org-supertag-query` 打开交互式查询缓冲区，进行高级搜索和分析。

**主要功能：**
- **交互式搜索**：实时在标题、标签、内容和字段中匹配关键词
- **卡片式结果**：带边框的结果卡片可视化展示
- **导航浏览**：使用 `n`/`p` 键在结果间导航
- **选择标记**：按 `SPC` 键切换结果的选择状态
- **导出功能**：将选中的结果导出到文件，支持多种插入方式
- **查询历史**：自动查询历史记录和智能排序

**使用示例：**
1. `M-x org-supertag-query` - 打开查询界面
2. 输入搜索关键词（如"项目 紧急"）
3. 使用 `n`/`p` 键浏览结果
4. 按 `SPC` 键选择感兴趣的结果
5. 使用 `e f` 或 `e n` 导出选中项目

#### 🧪 已移除的复杂功能

> 为了提供更好的用户体验，我们移除了一些过于复杂和侵入性的功能：

#### 已简化的功能
- *自动标签建议* → 集成到节点视图中的手动标签建议
- *智能伴侣* → 简化为上下文分析功能
- *后台扫描* → 移除，改为按需处理

这些改变让Org-SuperTag更加专注于核心功能，减少干扰。

#### 实验性功能
- **AI工作流系统** (org-supertag-ai.el) - 基于Org标题的工作流定义

尚未具备实用性。
### 🔧 配置指南

#### 基础配置
```emacs-lisp
;; 核心配置
(setq org-supertag-sync-directories '("~/notes/" "~/projects/"))
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

- 📖 [[https://github.com/yibie/org-supertag/wiki][详细文档]]
- 🐛 [[https://github.com/yibie/org-supertag/issues][问题反馈]]
- 💬 [[https://github.com/yibie/org-supertag/discussions][社区讨论]]
- 🎥 [[https://www.youtube.com/playlist?list=xxx][视频教程]]

### Changelog
详细见 !(CHANGELOG)[./CHANGELOG.org]

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

*Made with ❤️ by [[https://github.com/yibie][Yibie]] | 受到 [[https://tana.inc][Tana]]、[[https://github.com/ahyatt/ekg][ekg]]、[[https://github.com/meedstrom/org-node][org-node]] 的启发*