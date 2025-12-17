# ORG-SUPERTAG-PLUGIN-GUIDE（中文版）：完整插件开发文档

这份文档是 org-supertag 插件开发的事实来源（开发者指南）。

核心原则：

- DB/Store 是唯一真相源；
- 插件的主要扩展点是 *视图（View）*（任意 UI），而不是 schema；
- 插件必须通过 **UI 无关的 View Data API**（`supertag-view-api.el`）读取数据；
- 写入必须通过 **ops** API，并通常使用 `supertag-with-transaction`。

可运行参考插件：

- `doc/examples/supertag-view-demo-dashboard.el`

## 0）数据模型与约定

### 实体与存储

org-supertag 的系统状态集中存放在一个中心 store（底层是 hash table）。
在很多用户讨论里，我们也会把它称为“数据库”。

常见 collection：

- `:nodes` — 节点实体（plist）
- `:tags` — 标签实体（plist）
- `:relations` — 关系实体（plist）
- `:field-definitions` / `:tag-field-associations` / `:field-values` — 全局字段模型

### 实体表示

- 实体是 **plist**（property list），通常包含 `:id` 等关键字段；
- ID 是字符串；
- 字段 key 使用 keyword（例如 `:title`、`:file`、`:tags`）。

### 读写契约

- 读 API 返回的 plist，请当作不可变快照使用；
- 写 API 接收 plist 或 updater 函数，会更新 store 并触发事件。

## 1）读取 API（View Data API）

View Data API 是 **内部公开（internal public）** 且 **UI 无关** 的数据读取契约。
插件不管用什么 UI，都应以它为读取入口。

文件：

- `supertag-view-api.el`

### Query Spec

很多函数使用 `QUERY-SPEC` plist，例如：

- `(:type :tag :value "foo")` → 取拥有 tag "foo" 的节点
- `(:type :nodes)` → 取全部 node ids
- `(:type :tags)` → 取全部 tag ids

### API 清单（只读）

**数据集入口**

- `(supertag-view-api-list-tags) -> (list string)`  
  返回所有 tag name（排序后）。

- `(supertag-view-api-tag-id TAG-NAME) -> string-or-nil`  
  tag name → tag id。

- `(supertag-view-api-list-entity-ids QUERY-SPEC) -> (list string)`  
  获取一个数据集的 ID 列表入口。

**实体读取**

- `(supertag-view-api-get-entity TYPE ID) -> plist-or-nil`  
  读取单个实体。`TYPE` 支持别名：` :node/:nodes`、`:tag/:tags` 等。

- `(supertag-view-api-get-entities TYPE IDS) -> (list plist)`  
  批量读取（性能建议优先用它）。

**底层 collection（高级用法）**

- `(supertag-view-api-get-collection COLLECTION) -> hash-table`  
  返回底层 store 的 collection hash table（必须按只读使用）。
  仅在需要 scan/聚合时使用（例如反向引用扫描、schema 浏览）。

**字段读取**

- `(supertag-view-api-node-field-in-tag NODE-ID TAG-ID FIELD-NAME) -> value`  
  读取 node 在某个 tag 语境下的 field value。

**订阅**

- `(supertag-view-api-subscribe EVENT FN) -> unsubscribe-fn`  
  订阅变更事件。返回 `unsubscribe-fn`，插件应在 buffer 退出时调用它释放订阅。

## 2）写入 API（Ops 层）

插件要修改数据，必须走 ops API，而不是直接改 store。

### 事务（推荐）

多次写入应包在：

```elisp
(supertag-with-transaction
  ;; 多个 ops 写入
  ...)
```

目的：合并通知，减少 UI 抖动。

### 常用写入 API 清单

**Nodes**

- `(supertag-node-create PROPS) -> node-plist`  
- `(supertag-node-update NODE-ID UPDATER) -> node-plist-or-nil`  
- `(supertag-node-delete NODE-ID) -> deleted-node-or-nil`

**Tags**

- `(supertag-tag-create PROPS) -> tag-plist`  
- `(supertag-tag-update TAG-ID UPDATER) -> tag-plist-or-nil`  
- `(supertag-tag-delete TAG-ID) -> deleted-tag-or-nil`  
- `(supertag-tag-add-field TAG-ID FIELD-DEF) -> tag-plist`  
- `(supertag-tag-remove-field TAG-ID FIELD-NAME) -> tag-plist`

**Fields**

- `(supertag-field-set NODE-ID TAG-ID FIELD-NAME VALUE) -> VALUE`  
- `(supertag-field-set-many NODE-ID SPECS) -> plist`

**Relations**

- `(supertag-relation-add-reference FROM-ID TO-ID) -> relation-plist`  
- `(supertag-relation-delete RELATION-ID) -> deleted-relation-or-nil`

写入约定：

- ID 是字符串；
- UPDATER：输入旧 plist，返回新 plist（返回 nil 表示中止更新）；
- 多次写入优先用事务包裹。

## 2.5）Schema 注册（高级用法）

org-supertag 支持用户在初始化阶段注册/覆写 schema。
这主要面向高级场景（自定义实体类型或扩展校验/字段），不包含自动迁移机制。

推荐配置方式：

```elisp
(setq supertag-schema-registration-functions
      (list
       (lambda ()
         ;; 覆写/扩展已有 schema（默认是 merge）。
         (supertag-schema-register :node '(:my-field (:type :string :default "")))

         ;; 或注册一个全新的实体类型 + schema。
         (supertag-register-entity-type
          :my-entity
          '(:id (:type :string :required t)
            :name (:type :string :default "")))))))
```

## 3）事务系统（它实际意味着什么）

文件：

- `supertag-core-transform.el`

API：

- `(supertag-with-transaction ...)`

语义：

- 在事务 body 执行期间会**抑制通知**，结束后一次性发出 **batch 通知**；
- 会设置 `supertag--transaction-active` 并记录 transaction log；
- 目前重点在“合并通知/降低 UI 抖动”，不要在没有代码明确实现前假设完整 rollback 语义。

## 4）参考插件：非 table UI 的 dashboard

文件：

- `doc/examples/supertag-view-demo-dashboard.el`

它演示：

- `(supertag-view-api-nodes-by-tag TAG)` 读取节点 ID；
- `(supertag-view-api-get-entities :nodes IDS)` 批量读取实体；
- 订阅 `:node-updated` 并用 idle timer 节流刷新；
- 自定义 buffer 渲染与可点击跳转。

## 5）手动验证 checklist

1. 加载示例：

```elisp
(add-to-list 'load-path "/path/to/org-supertag/doc/examples/")
(require 'supertag-view-demo-dashboard)
```

2. 打开：

`M-x supertag-view-demo-dashboard-open`

3. 验证：

- dashboard 显示节点数量与可点击列表
- `RET` 跳转到节点
- 修改节点后 dashboard 自动刷新（或按 `g` 手动刷新）
