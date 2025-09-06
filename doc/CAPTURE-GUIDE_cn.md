# Org-Supertag 捕获系统

## 🚀 什么是捕获系统？

Org-Supertag 捕获系统提供了一个强大而灵活的节点创建机制，支持动态模板、内容生成器和自动字段填充。该系统遵循数据分离原则，将节点内容与扩展属性分别存储在 Org 文件和数据库中。

### 🎯 核心理念

传统 Org-mode 中，创建节点需要手动输入标题、标签和属性。在 Org-Supertag 捕获系统中：

- 🔄 **模板驱动** - 使用预定义模板快速创建结构化节点
- 🧠 **智能填充** - 自动从剪贴板、选区或函数获取内容
- 🏷️ **标签智能** - 交互式标签选择和自动完成
- 📝 **字段丰富** - 自动设置标签字段值

### ⚡ 快速体验

```org
;; 使用模板快速创建任务节点
M-x supertag-capture-with-template RET t RET

;; 结果自动生成:
* 修复登录页面bug #task
  :PROPERTIES:
  :ID: a1b2c3d4-e5f6-7890-g1h2-i3j4k5l6m7n8
  :END:
  
  创建时间: 2025-09-06
  状态: 待开始
```

## 📖 使用指南

### 两种捕获方式

#### 1. 独立捕获 (`supertag-capture`)

最简单的节点创建方式，适合临时或一次性节点创建。

```
M-x supertag-capture
```

**操作流程**:
1. 输入节点标题
2. 选择标签（可选，支持多选）
3. 选择目标文件和插入位置
4. 系统创建带有ID的Org节点
5. 可选择进行字段值丰富

**适用场景**：
- 临时想法记录
- 简单任务创建
- 快速内容捕获

#### 2. 模板驱动捕获 (`supertag-capture-with-template`)

使用预定义模板快速创建结构化节点，适合重复性工作流程。

```
M-x supertag-capture-with-template
```

**操作流程**:
1. 选择已配置的模板
2. 根据模板规范提供内容
3. 系统自动生成完整节点
4. 可选择进行额外字段丰富

**适用场景**：
- 标准化文档创建
- 重复性任务记录
- 结构化信息收集

## 🎨 模板系统

### 模板配置结构

```elisp
(setq supertag-capture-templates
      '((模板键 "模板描述"
         :file "目标文件路径"
         :node-spec
         ((节点规范列表...)))))
```

### 节点规范详解

每个节点规范项目包含两个核心部分：

| 参数 | 类型 | 必需 | 描述 |
|------|------|------|------|
| `:part` | 符号 | 是 | 节点部分类型 (`:title`, `:tags`, `:body`, `:fields`) |
| `:get` | 列表 | 是 | 内容生成规范 |

### 内容生成器

内容生成器决定了如何为节点的各个部分填充内容。

#### 静态值生成器 `:static`

直接使用指定的值，不需要用户输入。

```elisp
;; 示例：固定标签
(:part :tags :get (:static ("工作" "重要")))

;; 示例：固定字段值
(:part :fields :get (:static (((:tag "项目" :field "状态" :value "进行中")))))
```

#### 交互式提示生成器 `:prompt`

提示用户输入内容，可设置默认值。

```elisp
;; 示例：输入标题
(:part :title :get (:prompt "任务标题: "))

;; 示例：输入标签（带默认值）
(:part :tags :get (:prompt "标签: " :initial-input "任务,"))

;; 示例：输入字段值
(:part :fields :get (:static (((:tag "任务" :field "优先级" :get (:prompt "优先级: "))))))
```

#### 剪贴板内容生成器 `:clipboard`

使用当前剪贴板内容作为节点正文。

```elisp
(:part :body :get (:clipboard))
```

#### 选区内容生成器 `:region`

使用当前选中的文本作为内容。如果没有选区则报错。

```elisp
(:part :body :get (:region))
```

#### 选区或剪贴板生成器 `:region-or-clipboard`

优先使用选区，如果没有选区则使用剪贴板。

```elisp
(:part :body :get (:region-or-clipboard))
```

#### 模板字符串生成器 `:template-string`

使用模板字符串生成内容，支持占位符替换。

```elisp
(:part :body :get (:template-string "创建日期: %date\n内容: %clipboard\n状态: 待处理"))
```

**支持的占位符**:

##### ⏰ 时间相关占位符
| 占位符 | 描述 | 示例输出 |
|--------|------|----------|
| `%date` | 当前日期 | 2025-09-06 |
| `%time` | 当前时间 | 14:30 |
| `%datetime` | 日期时间 | 2025-09-06 14:30 |
| `%timestamp` | 完整时间戳 | 2025-09-06 14:30:45 |
| `%week` | 当前周数 | W36 |
| `%month` | 当前月份 | September |
| `%year` | 当前年份 | 2025 |

##### 👤 用户信息占位符
| 占位符 | 描述 | 示例输出 |
|--------|------|----------|
| `%user` | 用户登录名 | chenyibin |
| `%fullname` | 用户全名 | Chen Yibin |
| `%hostname` | 系统主机名 | MacBook-Pro |

##### 📁 文件上下文占位符
| 占位符 | 描述 | 示例输出 |
|--------|------|----------|
| `%filename` | 当前缓冲区文件名 | project.org |
| `%filepath` | 当前缓冲区完整路径 | /Users/user/org/project.org |
| `%directory` | 当前缓冲区目录 | /Users/user/org/ |

##### 🔗 节点上下文占位符
| 占位符 | 描述 | 示例输出 |
|--------|------|----------|
| `%current-node-title` | 当前节点标题 | 项目管理系统 |
| `%current-node-id` | 当前节点ID | a1b2c3d4-e5f6-7890 |
| `%current-tags` | 当前节点标签列表 | 项目, 重要 |

##### 🛠️ 工具占位符
| 占位符 | 描述 | 示例输出 |
|--------|------|----------|
| `%clipboard` | 剪贴板内容 | [剪贴板中的文本] |
| `%random` | 随机4位数字 | 1234 |
| `%uuid` | 新生成的UUID | f47ac10b-58cc-4372 |

示例用法：
```elisp
;; 创建包含多种信息的模板
(:part :body :get (:template-string "报告日期: %date\n报告人: %fullname\n主机名: %hostname\n内容:\n%clipboard"))
```

#### 自定义函数生成器 `:function`

调用自定义函数生成内容。

```elisp
(:part :body :get (:function my-custom-content-generator))
```

### 字段设置规范

字段规范用于为节点自动设置标签字段值。

```elisp
(:part :fields :get (:static (((:tag "项目" :field "状态" :value "进行中")
                               (:tag "项目" :field "优先级" :value "高")))))
```

字段规范格式：
- `:tag` - 标签ID
- `:field` - 字段名称  
- `:value` - 字段值（静态）
- `:get` - 字段值生成器（动态）

## 📋 使用场景与模板示例

### 日常任务创建

```elisp
;; 模板配置
("t" "快速任务"
 :file "~/org/tasks.org"
 :node-spec
 ((:part :title :get (:prompt "任务: "))
  (:part :tags  :get (:static ("任务")))
  (:part :body  :get (:template-string "创建: %date\n状态: 待开始\n"))))
```

使用方法:
1. `M-x supertag-capture-with-template RET t RET`
2. 输入任务名称: "修复登录页面bug"
3. 系统自动生成结构化任务节点

### 学习笔记捕获

```elisp
;; 模板配置
("l" "学习笔记"
 :file "~/org/learning.org"
 :node-spec
 ((:part :title :get (:prompt "学习主题: "))
  (:part :tags  :get (:prompt "标签: " :initial-input "学习,"))
  (:part :body  :get (:region-or-clipboard))
  (:part :fields :get (:static (((:tag "学习" :field "难度" :get (:prompt "难度(1-5): "))
                                 (:tag "学习" :field "来源" :get (:prompt "学习来源: "))))))))
```

使用方法:
1. 复制一段学习资料到剪贴板
2. `M-x supertag-capture-with-template RET l RET`
3. 输入学习主题和相关信息
4. 系统自动生成学习笔记节点

### 会议记录模板

```elisp
;; 模板配置
("m" "会议记录"
 :file "~/org/meetings.org"
 :node-spec
 ((:part :title :get (:prompt "会议主题: "))
  (:part :tags  :get (:static ("会议")))
  (:part :body  :get (:template-string "时间: %date\n参与者: \n\n议程:\n\n讨论要点:\n\n行动项:\n"))
  (:part :fields :get (:static (((:tag "会议" :field "类型" :get (:prompt "会议类型: "))
                                 (:tag "会议" :field "状态" :value "已完成")))))))
```

### 项目规划模板

```elisp
("p" "项目规划"
 :file "~/org/projects.org" 
 :node-spec
 ((:part :title :get (:prompt "项目名称: "))
  (:part :tags  :get (:static ("项目" "规划")))
  (:part :body  :get (:template-string "开始日期: %date\n\n目标:\n\n里程碑:\n\n资源需求:\n"))
  (:part :fields :get (:static (((:tag "项目" :field "状态" :value "规划中")
                                 (:tag "项目" :field "负责人" :get (:prompt "负责人: "))))))))
```

### 灵感捕获模板

```elisp
("i" "灵感想法"
 :file "~/org/ideas.org"
 :node-spec
 ((:part :title :get (:prompt "想法标题: "))
  (:part :tags  :get (:prompt "分类标签: " :initial-input "想法,"))
  (:part :body  :get (:template-string "记录时间: %date\n\n详细描述:\n%clipboard\n\n相关思考:\n"))))
```

## ⚙️ 高级功能

### 字段自动填充

模板可以自动为节点设置字段值，支持静态值和动态生成：

```elisp
(:part :fields :get (:static (((:tag "项目" :field "状态" :value "进行中")
                               (:tag "项目" :field "创建者" :get (:function user-full-name))
                               (:tag "项目" :field "截止日期" :get (:prompt "截止日期: "))))))
```

### 交互式字段丰富

捕获完成后，用户可以继续添加字段值：

1. 系统提示是否要丰富节点
2. 用户选择节点上的标签
3. 选择该标签的可用字段
4. 输入字段值
5. 重复直到完成

### 智能位置选择

提供四种插入选项：
- **文件顶部**: 在文件开始处插入
- **文件末尾**: 在文件结尾插入  
- **标题下方**: 作为选定标题的子项目
- **标题之后**: 与选定标题同级

## 🔧 配置与扩展

### 基础配置

```elisp
;; 设置捕获模板
(setq supertag-capture-templates
      '(
        ;; 快速任务
        ("t" "快速任务"
         :file "~/org/tasks.org"
         :node-spec
         ((:part :title :get (:prompt "任务: "))
          (:part :tags  :get (:static ("任务")))
          (:part :body  :get (:template-string "创建: %date\n状态: 待开始\n"))))
        
        ;; 学习笔记
        ("l" "学习笔记"
         :file "~/org/learning.org"
         :node-spec
         ((:part :title :get (:prompt "学习主题: "))
          (:part :tags  :get (:prompt "标签: " :initial-input "学习,"))
          (:part :body  :get (:region-or-clipboard))
          (:part :fields :get (:static (((:tag "学习" :field "难度" :get (:prompt "难度(1-5): "))
                                         (:tag "学习" :field "来源" :get (:prompt "学习来源: "))))))))
        
        ;; 会议记录
        ("m" "会议记录"
         :file "~/org/meetings.org"
         :node-spec
         ((:part :title :get (:prompt "会议主题: "))
          (:part :tags  :get (:static ("会议")))
          (:part :body  :get (:template-string "时间: %date\n参与者: \n\n议程:\n\n讨论要点:\n\n行动项:\n"))
          (:part :fields :get (:static (((:tag "会议" :field "类型" :get (:prompt "会议类型: "))
                                         (:tag "会议" :field "状态" :value "已完成")))))))))
```

### 自定义内容生成器

创建自己的内容生成器以满足特定需求：

```elisp
(defun my-custom-generator ()
  "自定义内容生成器示例"
  (format "项目编号: PRJ-%d\n创建者: %s\n"
          (random 10000)
          (user-full-name)))

;; 在模板中使用:
(:part :body :get (:function my-custom-generator))
```

### 扩展模板变量

可以扩展模板字符串处理器来支持更多占位符：

```elisp
;; 添加 %time 占位符支持
(setq template (replace-regexp-in-string "%time" (format-time-string "%H:%M") template t t))
```

## 📚 API 参考

### 用户命令

| 命令 | 描述 | 使用方式 |
|------|------|----------|
| `supertag-capture` | 独立的捕获命令 | `M-x supertag-capture` |
| `supertag-capture-with-template` | 基于模板的捕获命令 | `M-x supertag-capture-with-template` |
| `supertag-capture-enrich-node` | 交互式丰富节点字段值 | `M-x supertag-capture-enrich-node RET node-id` |

### 内容生成器函数

| 函数 | 描述 |
|------|------|
| `supertag-capture--get-from-static (args)` | 静态值生成器 |
| `supertag-capture--get-from-prompt (args)` | 交互提示生成器 |
| `supertag-capture--get-from-clipboard ()` | 剪贴板内容生成器 |
| `supertag-capture--get-from-region ()` | 选区内容生成器 |
| `supertag-capture--get-from-region-or-clipboard ()` | 选区或剪贴板生成器 |
| `supertag-capture--get-from-template-string (args)` | 模板字符串处理器 |
| `supertag-capture--get-from-function (args)` | 自定义函数调用生成器 |

## 🆚 与 Org-Capture 的对比

| 特性 | Org-Capture | Supertag-Capture |
|------|-------------|------------------|
| 模板配置 | 静态字符串模板 | 动态内容生成器 |
| 标签支持 | 手动输入 | 交互选择+自动完成 |
| 字段管理 | 属性抽屉 | 数据库字段系统 |
| 内容来源 | 固定格式 | 多种生成器 |
| 扩展性 | 有限 | 高度可扩展 |

## 🐛 故障排除

### 常见问题

**问题**: 模板执行时报错 "Template doesn't exist"
**解决**: 检查 `supertag-capture-templates` 配置和模板键名

**问题**: 字段设置失败
**解决**: 确保标签存在且定义了相应字段

**问题**: 文件写入位置错误
**解决**: 检查目标文件是否存在且可写

### 调试技巧

1. 使用 `M-x supertag-capture` 测试基本功能
2. 检查 `*Messages*` 缓冲区的错误信息
3. 验证模板配置的语法正确性
4. 确认所有依赖的文件和标签存在

---

*本文档描述了 Org-Supertag v2.0 捕获系统的完整功能和使用方法。*