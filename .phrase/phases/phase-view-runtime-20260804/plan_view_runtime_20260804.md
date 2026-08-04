# plan_view_runtime_20260804

## Milestones

1. **Contract baseline**：锁定 Runtime 生命周期和四个已有视图的兼容行为。
2. **Runtime core**：深化现有 `supertag-view-framework.el`，实现 view instance、open、refresh、display 与 cleanup。
3. **Passive migration**：先迁移没有实时订阅的 Search。
4. **Collection migration**：迁移 Table、Kanban 的生命周期，保留各自 renderer 与编辑路径。
5. **Inspector migration**：迁移 Node 的 side-window、subscription 与 selection/follow 生命周期。
6. **Compatibility proof**：接入 Widget DSL，并用 Stream-shaped fixture 证明 Runtime 不需要特例。
7. **Release gate**：全量测试、Emacs 29.1/29.4 CI、文档与 phase 闭环。

## Scope

预计代码范围：

- `supertag-view-framework.el`
- `supertag-ui-search.el`
- `supertag-view-table.el`
- `supertag-view-kanban.el`
- `supertag-view-node.el`
- `supertag-ui-commands.el`（仅 Kanban 兼容入口）
- `org-supertag.el`（仅加载/注册需要时）
- `test/view-framework-test.el`
- 新增最少量的 runtime/adapter ERT 文件
- `test/run-tests.sh`（仅登记新测试与 focused filter）

明确不进入：Schema UI、Stream 产品实现、现有 README/menu/smart-key 工作区改动。

## Priorities

- P0：Runtime instance、refresh、cleanup 正确；旧命令与编辑行为不回归。
- P0：订阅不泄漏，真实 Store 更新事件能驱动刷新。
- P1：selection/entity contract 统一并保持 Smart Key 兼容。
- P1：Widget DSL 兼容与 Stream-ready fixture。
- P2：仅在测量证明需要时保留或增加局部更新优化。

## Execution Strategy

- 每次只迁移一个 Adapter；上一 Adapter focused ERT 通过后再进入下一项。
- 先写会失败的生命周期/兼容回归，再修改实现。
- Runtime 使用现有 framework、View API、buffer-local state 与 `display-buffer`，不建平行框架。
- Adapter renderer 与写操作尽量原样保留；迁移只抽离共同生命周期。
- 旧 public command 作为 wrapper，直到整个 phase 验收完成。

## Quality Gates

每个原子任务至少运行对应 focused ERT。阶段完成前运行：

```sh
find . -name '*.elc' -type f -delete
./test/run-tests.sh view-runtime view smart-key
./test/run-tests.sh all
git diff --check
```

`test/run-tests.sh` 设置 `load-prefer-newer`，是本阶段本地测试的权威入口；绕过 runner 的裸 Emacs 结果只能作为诊断信息。

## Risks & Mitigations

- **Table blast radius**：保留 state builder、renderer、editing 与 text properties，只迁生命周期。
- **Node hidden state**：先覆盖 follow、field focus、no-ID 与 subscription dedupe，再迁移。
- **Event mismatch**：以 Store 实际发出的 `:store-changed` 为默认，Adapter predicate 缩小刷新范围。
- **Double lifecycle**：迁移后删除 Adapter 内重复 open/subscribe/cleanup 路径，不长期保留两套主路径。
- **Framework 膨胀**：只加入已被多个 Adapter 使用的生命周期能力；视图特有分支留在 Adapter。
- **用户工作区冲突**：不修改 README、menu、smart-key 的既有未提交改动。

## Dependencies

- 内部：`supertag-view-api.el`、Store event API、既有 ops、各 view mode/renderer。
- 平台：Emacs 29.1+ 的 buffer-local variables、`display-buffer`、side windows、kill/window hooks。
- 外部：无新增 package。

## Rollback

- Runtime core 在 Adapter 迁移前可独立移除。
- 每个 public command 保留 wrapper；单个 Adapter 可退回旧 open/refresh 路径。
- 不在同一原子任务中迁移两个 Adapter。
- 若 Runtime 必须为某个 Adapter增加大量 view-specific 分支，停止迁移并回到 Adapter 本地处理。
