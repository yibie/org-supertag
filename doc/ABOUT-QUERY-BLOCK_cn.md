# Org Supertag Query-Block 示例

作者: 重构完成  
日期: 2025-08-23

## S-expression 查询功能重命名为 Query-Block

### 新的语言名称

现在使用 `org-supertag-query-block` 作为新的 Org Babel 语言名称：

```org-supertag-query-block
(tag "work")
```

### 向后兼容

旧的 `org-supertag-query` 语言名称仍然可以使用（向后兼容）：

```org-supertag-query
(and (tag "project") (field "priority" "high"))
```

### 支持的查询语法

#### 标签查询

```org-supertag-query-block
(tag "work")
```

#### 字段查询

```org-supertag-query-block
(field "status" "active")
```

#### 全文搜索

```org-supertag-query-block
(term "meeting")
```

#### 逻辑组合

逻辑组合是 Query-Block 的强大功能，支持复杂的查询条件：

##### AND 组合
```org-supertag-query-block
(and (tag "project") (field "priority" "high"))
```

##### OR 组合
```org-supertag-query-block
(or (tag "work") (tag "personal"))
```

##### NOT 组合
```org-supertag-query-block
(not (tag "archived"))
```

##### 嵌套逻辑组合
```org-supertag-query-block
(or 
  (and (tag "project") (field "status" "active"))
  (and (tag "task") (after "2025-06-01")))
```

##### 复杂逻辑组合示例
```org-supertag-query-block
(and 
  (or (tag "work") (tag "project"))
  (not (field "status" "completed"))
  (after "2025-01-01"))
```

#### 时间查询

时间查询支持多种时间相关的条件：

##### 指定日期之后
```org-supertag-query-block
(after "2025-01-01")
```

##### 指定日期之前
```org-supertag-query-block
(before "2025-12-31")
```

##### 日期范围
```org-supertag-query-block
(and (after "2025-01-01") (before "2025-12-31"))
```

##### 最近天数
```org-supertag-query-block
;; 查询最近7天创建的节点
(recent-days 7)
```

##### 特定月份
```org-supertag-query-block
;; 查询指定月份的节点
(in-month "2025-06")
```

##### 特定年份
```org-supertag-query-block
;; 查询指定年份的节点
(in-year "2025")
```

#### 复杂查询示例

```org-supertag-query-block
(or 
  (and (tag "project") (field "status" "active"))
  (and (tag "task") (after "2025-06-01")))
```

### 查询结果格式

查询结果默认以表格形式展示，包含节点标题、标签和查询的字段值：

```org-supertag-query-block
(field "priority" "high")
```

#### 查询结果样式示例

以下是一个模拟的查询结果展示：

| 标题 | 标签 | priority |
|------|------|----------|
| 重要项目A | project,work | high |
| 紧急任务B | task,work | high |
| 关键会议准备 | meeting,project | high |

#### 带时间条件的查询结果示例

```org-supertag-query-block
(and (tag "task") (after "2025-06-01"))
```

| 标题 | 标签 | 创建日期 |
|------|------|----------|
| 新任务1 | task,work | 2025-06-15 |
| 新任务2 | task,personal | 2025-07-20 |

### 独立查询窗口

除了 S-expression 查询块，还有独立的交互式查询窗口功能：

- `M-x supertag-query-interactive` - 打开交互式查询窗口
- 支持关键词搜索、卡片式显示、导航、标记和导出功能

#### 交互式查询特性

- 卡片式结果显示，包含标题、文件路径、标签和上下文预览
- 键盘导航：n/p 键上下移动
- 空格键标记节点，回车键访问节点
- 支持将选中的节点链接插入到文档中
- 支持导出选中结果到新文件

### 高级查询功能

#### 关系查询

```org-supertag-query-block
;; 查询与指定实体相关的所有实体
;; (注意：此功能需要在 supertag-services-query.el 中实现)
(tag "ProjectA")
```

#### 数据库记录查询

```org-supertag-query-block
;; 查询特定数据库中的所有记录
;; (注意：此功能需要在 supertag-services-query.el 中实现)
(tag "Tasks")
```

### 重命名总结

1. **S-expression 查询功能**重命名为 **query-block**
2. **独立查询窗口功能**保持不变，继续提供完整的交互式查询体验
3. **向后兼容**：旧的语言名称和函数仍然可以使用
4. **插入函数**：`supertag-insert-query-block` 现在生成新格式的代码块

### 实用技巧

#### 快速插入查询块

使用 `M-x supertag-insert-query-block` 命令可以快速插入一个查询块模板。

#### 查询历史

交互式查询窗口会保存查询历史，方便重复使用常用的查询。

#### 结果导出

查询块的结果可以导出为 Org 表格，交互式查询的结果可以导出为包含链接的新文件。

这样的设计既满足了重命名的需求，又保持了系统的完整性和向后兼容性。