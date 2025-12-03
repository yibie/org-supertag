## Org-Supertag 5.2.0 – 全局字段数据库迁移指引

本指南说明从旧的「按 Tag 嵌套字段」模型升级到 5.2.0 引入的「全局字段模型」的步骤。在新的模型中，字段是独立的一等实体，不再附着在单个 Tag 下。

> **重要：** 在执行任何迁移操作前，请先为当前 Supertag 数据目录做一次完整备份。

### 1. 启用全局字段模型

在加载 `org-supertag` 之前，在 Emacs 配置中启用全局字段：

```elisp
(setq supertag-use-global-fields t)
```

这样会：
- 启用新的三个集合：`:field-definitions`、`:tag-field-associations` 和 `:field-values`。
- 让 ops / service / UI 都以全局字段模型为唯一真实来源。

### 2. 先运行一次 Dry-Run 迁移

先做一次「演练」迁移，不写入任何数据，只查看将要发生的变更：

```elisp
(require 'org-supertag)
(require 'supertag-migration)

;; 确认 dry-run 打开（默认即为 t）
(setq supertag-migration-dry-run t)

;; Dry-run：只扫描与记录，不写入
(supertag-migration-run-global-fields)
```

检查：

- minibuffer 中的简要统计；
- `*supertag-migration*` 缓冲区中的详细日志，关注：
  - `fields=... associations=... values=... skipped=... conflicts=...`
  - `Conflicts: ...` 表示字段定义冲突（同一 id 类型/配置不一致）或缺失定义等问题。

如有冲突，请先调整字段定义或数据，确保迁移结果符合预期后再继续。

### 3. 确认备份后执行真实迁移（写入）

当 dry-run 结果合理、并且你已经完成备份之后：

```elisp
(require 'supertag-migration)

;; 关闭 dry-run，或在调用时传入前缀参数 / FORCE-WRITE
(setq supertag-migration-dry-run nil)

;; 执行实际迁移（会写入到 store）
(supertag-migration-run-global-fields t)
```

这一步会：

- 将按 Tag 定义的字段去重合并，写入全局字段定义 `:field-definitions`。
- 为每个 Tag 生成有序的字段关联，写入 `:tag-field-associations`。
- 将旧的嵌套字段值（node → tag → field）重写为扁平的 `:field-values`（node-id → field-id → value）。
- 在 `*supertag-migration*` 里输出汇总统计和详细冲突信息。

### 4. 验证并继续使用全局字段模型

迁移完成后：

- 保持配置中 `supertag-use-global-fields` 为 `t`。
- 通过以下方式确认数据正确性：
  - 打开表格视图 / Node 视图 / 看板视图，检查字段展示是否正确且没有重复字段。
  - 修改某些字段值，确认已有的自动化规则（例如使用 `field-equals` / `field-changed` 的规则）能正常触发。
  - 执行你日常使用的查询与 capture 流程，看结果是否与预期一致。

如发现异常，可参考：

- `doc/global-field-migration-rfc.md` —— 全局字段设计与冲突处理策略。
- `doc/global-field-migration-plan.md` / `doc/global-field-migration-tasks.md` —— 分阶段迁移计划与任务清单。

在验证无误后，可以将新的全局字段集合视为唯一真实存储。旧的 `:fields` 存储会保留一段时间用于兼容，后续可在合适版本中正式移除。

