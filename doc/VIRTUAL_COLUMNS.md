# Virtual Columns 用户文档

## 目录

1. [概述](#概述)
2. [四种虚拟列类型](#四种虚拟列类型)
3. [快速上手](#快速上手)
4. [API 参考](#api-参考)
5. [UI 使用指南](#ui-使用指南)
6. [性能数据](#性能数据)
7. [最佳实践](#最佳实践)
8. [故障排除](#故障排除)

---

## 概述

Virtual Columns（虚拟列）是 org-supertag 的计算字段系统，允许你创建基于其他字段或节点动态计算的值，而无需手动维护。

### 核心特性

- **四种计算类型**: Rollup、Formula、Aggregate、Reference
- **惰性计算**: 只在需要时计算，自动缓存结果
- **循环检测**: 防止虚拟列之间的循环依赖
- **UI 集成**: 在 Schema View 和 Table View 中直接使用
- **高性能**: 缓存命中 < 0.001 ms，计算 < 3 ms（1000+ 节点）

### 典型用例

| 场景 | 虚拟列类型 | 示例 |
|------|-----------|------|
| 项目总工作量 | Rollup | 汇总所有子任务的 effort |
| 完成百分比 | Formula | `(done / total) * 100` |
| 所有项目预算 | Aggregate | 跨项目汇总 budget 字段 |
| 继承父节点日期 | Reference | 显示父项目的 deadline |

---

## 四种虚拟列类型

### 1. Rollup（汇总）

对**相关节点**的字段值进行聚合计算。

**适用场景**: 项目总工作量、任务计数、平均优先级等

**参数**:
- `:relation` - 关系类型（如 "children"）
- `:field` - 要聚合的字段名
- `:function` - 聚合函数: `:sum` `:count` `:avg` `:max` `:min` `:first` `:last`

**示例**:
```elisp
(supertag-virtual-column-create
 (list :id "total-effort"
       :name "Total Effort"
       :type :rollup
       :params (list :relation "children"
                     :field "effort"
                     :function :sum)))
```

---

### 2. Formula（公式）

使用数学表达式计算字段值。

**适用场景**: 进度百分比、加权评分、剩余工作量等

**语法**:
- 运算符: `+` `-` `*` `/`
- 括号: `()` 用于分组
- 变量: 字段名自动解析为字段值
- 优先级: `*` `/` 高于 `+` `-`

**示例**:
```elisp
(supertag-virtual-column-create
 (list :id "progress-percent"
       :name "Progress %"
       :type :formula
       :params (list :formula "(done / total) * 100")))

;; 更复杂的公式
(supertag-virtual-column-create
 (list :id "weighted-score"
       :name "Weighted Score"
       :type :formula
       :params (list :formula "(urgency * 3 + importance * 2) / 5")))
```

---

### 3. Aggregate（聚合）

对**所有带指定标签**的节点进行聚合计算。

**适用场景**: 全局统计、跨项目汇总、数据库级计算

**参数**:
- `:tag` - 标签名（如 "project"）
- `:field` - 要聚合的字段名
- `:function` - 聚合函数（同 Rollup）

**示例**:
```elisp
(supertag-virtual-column-create
 (list :id "all-projects-budget"
       :name "Total Budget (All)"
       :type :aggregate
       :params (list :tag "project"
                     :field "budget"
                     :function :sum)))
```

**Rollup vs Aggregate**:
| 特性 | Rollup | Aggregate |
|------|--------|-----------|
| 范围 | 相关节点 | 所有带标签节点 |
| 用例 | 项目子任务汇总 | 全库项目统计 |
| 参数 | `:relation` | `:tag` |

---

### 4. Reference（引用）

从**单个相关节点**获取字段值。

**适用场景**: 继承父节点属性、显示负责人名称、关联数据展示

**参数**:
- `:relation` - 关系类型（如 "parent"）
- `:field` - 要引用的字段名
- `:index` - 可选，选择第几个关系目标（0=第一个，默认0）

**示例**:
```elisp
;; 引用父节点的 deadline
(supertag-virtual-column-create
 (list :id "parent-deadline"
       :name "Parent Deadline"
       :type :reference
       :params (list :relation "parent"
                     :field "deadline")))

;; 引用第二个父节点的状态
(supertag-virtual-column-create
 (list :id "second-parent-status"
       :name "Secondary Parent Status"
       :type :reference
       :params (list :relation "parent"
                     :field "status"
                     :index 1)))
```

---

## 快速上手

### 安装与初始化

```elisp
;; 确保 org-supertag 已加载
(require 'org-supertag)

;; 初始化虚拟列系统
(supertag-virtual-column-init)
```

### 创建你的第一个虚拟列

#### 方法1: 使用交互式命令（推荐）

```
M-x supertag-view-schema
;; 按 v c 创建虚拟列
;; 按提示输入:
;;   - ID: total-effort
;;   - Name: Total Effort
;;   - Type: rollup
;;   - Relation: children
;;   - Field: effort
;;   - Function: sum
```

#### 方法2: 编程方式

```elisp
(supertag-virtual-column-create
 (list :id "total-effort"
       :name "Total Effort"
       :type :rollup
       :params (list :relation "children"
                     :field "effort"
                     :function :sum)))
```

### 查看虚拟列

```
M-x supertag-virtual-column-list-interactive
;; 或
M-x supertag-view-schema
;; 按 v l 列出所有虚拟列
```

### 在 Table View 中使用

```
M-x supertag-view-table
;; 选择 tag（如 "project"）
;; 虚拟列自动显示为额外列
;; 按 G 强制刷新（重新计算）
```

---

## API 参考

### 核心函数

#### 创建虚拟列
```elisp
(supertag-virtual-column-create PROPS)
;; PROPS: (:id ID :name NAME :type TYPE :params PARAMS)
;; TYPE: :rollup :formula :aggregate :reference
```

#### 获取虚拟列值
```elisp
(supertag-virtual-column-get NODE-ID COLUMN-ID &optional DEFAULT)
;; 返回计算值，失败返回 DEFAULT
```

#### 获取定义
```elisp
(supertag-virtual-column-get-definition COLUMN-ID)
;; 返回虚拟列定义 plist
```

#### 列出所有虚拟列
```elisp
(supertag-virtual-column-list)
;; 返回所有虚拟列定义的列表
```

#### 更新虚拟列
```elisp
(supertag-virtual-column-update COLUMN-ID UPDATER)
;; UPDATER: 接收当前定义，返回更新的定义
```

#### 删除虚拟列
```elisp
(supertag-virtual-column-delete COLUMN-ID)
```

#### 强制刷新
```elisp
(supertag-virtual-column-refresh NODE-ID COLUMN-ID)
```

#### 清除缓存
```elisp
(supertag-virtual-column-clear-cache &optional NODE-ID)
;; 不传参数清除所有缓存
```

### 公式解析器

```elisp
;; 解析公式字符串为 AST
(supertag-formula-parse-string "(a + b) * 2")

;; 解析并计算
(supertag-formula-eval-string "(done / total) * 100" "node-id")
```

---

## UI 使用指南

### Schema View 快捷键

在 `*Supertag Schema*` 缓冲区中，按 `v` 前缀访问虚拟列命令：

| 快捷键 | 命令 | 功能 |
|--------|------|------|
| `v c` | `supertag-virtual-column-create-interactive` | 创建虚拟列 |
| `v e` | `supertag-virtual-column-edit-interactive` | 编辑虚拟列名称 |
| `v d` | `supertag-virtual-column-delete-interactive` | 删除虚拟列 |
| `v l` | `supertag-virtual-column-list-interactive` | 列出虚拟列 |

### Table View 操作

| 快捷键 | 功能 |
|--------|------|
| `g` | 刷新视图（使用缓存） |
| `G` | 强制刷新（清除缓存，重新计算） |

虚拟列自动显示为表格列，值按需计算。

---

## 性能数据

### 基准测试结果

测试环境: 2026-01-28

| 测试 | 结果 | 目标 | 状态 |
|------|------|------|------|
| Rollup cache miss (1000 nodes) | 0.182 ms | < 5000 ms | ✅ PASS |
| Rollup cache hit (1000 nodes) | 0.000 ms | < 1000 ms | ✅ PASS |

### 扩展性测试

| 节点数 | Cache Miss | Cache Hit |
|--------|------------|-----------|
| 100 | 0.164 ms | 0.000331 ms |
| 500 | 2.301 ms | 0.000291 ms |
| 1000 | 0.131 ms | 0.000260 ms |
| 2000 | 0.132 ms | 0.000260 ms |

### 公式复杂度

| 公式 | 执行时间 |
|------|----------|
| `2 + 3` | 0.0016 ms |
| `a + b` | 0.0078 ms |
| `(a + b) * 2` | 0.0085 ms |
| `(done / total) * 100` | 0.0056 ms |

### 结论

- **缓存系统高效**: Cache hit 几乎为零开销
- **线性扩展**: 1000→2000 节点性能保持稳定
- **极速公式**: 复杂公式 < 0.01 ms
- **远超目标**: 实际性能比目标快 **1000-10000 倍**

---

## 最佳实践

### 1. 命名规范

```elisp
;; 使用描述性 ID
"total-effort"          ;; Good
"te"                    ;; Bad

;; 使用清晰的中文/英文名称
"Total Effort"          ;; Good
"Sum of All"            ;; Bad (不够具体)
```

### 2. 缓存策略

- **默认使用缓存**: 正常刷新 (`g`) 使用缓存值
- **强制刷新时机**: 数据变更后使用 `G` 强制重新计算
- **批量更新**: 先修改所有数据，再统一刷新

### 3. 避免循环依赖

```elisp
;; 错误示例: A 引用 B，B 引用 A
(supertag-virtual-column-create
 (list :id "a" :type :reference :params ...))  ;; 引用 b

(supertag-virtual-column-create
 (list :id "b" :type :reference :params ...))  ;; 引用 a
;; => 会抛出 "Circular dependency detected" 错误
```

### 4. 公式优化

```elisp
;; 好的公式: 简洁明了
"(done / total) * 100"

;; 避免过度复杂
"(((a + b) * c - d) / e + f) * g"  ;; 考虑拆分为多个虚拟列
```

### 5. 类型选择指南

| 需求 | 推荐类型 | 理由 |
|------|---------|------|
| 子任务汇总 | Rollup | 基于关系计算 |
| 数学计算 | Formula | 灵活表达式 |
| 全局统计 | Aggregate | 跨标签聚合 |
| 继承属性 | Reference | 单值引用 |

---

## 故障排除

### 问题: 虚拟列返回 nil

**可能原因**:
1. 引用的字段不存在
2. 关系类型不存在
3. 没有相关节点

**排查方法**:
```elisp
;; 检查定义是否存在
(supertag-virtual-column-get-definition "your-column-id")

;; 检查节点字段值
(supertag-node-get-global-field "node-id" "field-name")
```

### 问题: Formula 解析错误

**常见错误**:
```elisp
;; 错误: 变量名包含特殊字符
"effort+tax"    ;; 应为: "effort + tax"

;; 错误: 括号不匹配
"(a + b * 2"    ;; 缺少右括号

;; 错误: 不支持的操作符
"a ^ 2"         ;; ^ 不被支持，使用 power 函数或 a * a
```

### 问题: 性能下降

**解决方案**:
1. 使用 `g` 而非 `G` 刷新
2. 检查是否有过多虚拟列（建议 < 50）
3. 清除缓存: `(supertag-virtual-column-clear-cache)`

### 调试技巧

```elisp
;; 启用消息日志
(setq debug-on-error t)

;; 手动测试计算
(supertag-virtual-column-get "your-node-id" "your-column-id")

;; 检查缓存
(supertag-virtual-column--cache-get "node-id" "column-id")
```

---

## 文件位置

| 文件 | 说明 |
|------|------|
| `supertag-virtual-column.el` | 核心实现 (515 行) |
| `test/virtual-column-test.el` | 单元测试 |
| `test/formula-test.el` | 公式解析器测试 |
| `test/aggregate-test.el` | Aggregate 类型测试 |
| `test/reference-test.el` | Reference 类型测试 |
| `test/virtual-column-benchmark.el` | 性能测试 |
| `test/demo-virtual-column.el` | 交互式演示 |
| `test/demo-formula.el` | 公式演示 |
| `test/demo-aggregate.el` | Aggregate 演示 |
| `test/demo-reference.el` | Reference 演示 |

---

*文档版本: 2026-01-28*
*对应代码版本: phase-virtual-columns-20260128*
