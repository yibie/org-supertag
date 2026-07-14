# plan_sync_integrity_20251226

## Milestones

1. 完整梳理当前同步数据流与破坏性变更路径。
2. 设计“同步快照状态机”（availability + completeness）。
3. 落地为最小改动的同步流水线：采样 → 对比 → 应用。
4. 兼容性验证与回滚策略确认。
5. sync-state 与 vault/data-directory 绑定并在切换时重载。
6. Table view Refs 列默认作为 node-reference 字段，支持直接编辑引用，并合并 add-reference 关系。
7. 保证 persistence 显式 DB 候选优先于 configured/default/legacy/snapshot，并保留失败诊断。
8. 将 file-node 稳定身份、链接编码与 backlink 存储语义拆到正确 seam。
9. 按每个 file-node 的 link type 验证持久身份，同时保留 legacy 数据的非破坏性兜底。

## Scope

- `supertag-services-sync.el` 同步流程与状态管理。
- `supertag-core-persistence.el` 的写盘风险边界评估（不改数据格式）。
- 启动/切换流程中的同步触发时机。
- View Table：Refs 列默认 node-reference 字段（可编辑），并在全局字段模式下自动创建/关联。
- Persistence：候选去重顺序与显式文件 failover 语义。
- File node：仅识别持久化身份；节点自身携带 link type；relation 不猜测兼容模式。

## Priorities

- P0: 目录不可用时不发生破坏性同步或写盘覆盖。
- P1: 保持文件删除语义与现有用户行为不变。
- P2: 简化同步流程与状态判断，减少特殊情况。
- P0: `supertag-load-store FILE` 必须先尝试 FILE，不能静默加载其他数据库。
- P0: file-node 身份变化必须淘汰旧节点，legacy node 不得因缺少新元数据被误删。

## State Machine (Design)

- 状态：`unavailable` / `partial` / `complete`。
- 采样阶段生成快照状态，再进入对比与应用。
- 只有 `complete` 允许删除、orphan 标记、sync-state 清理。
- `partial/unavailable` 只做增量更新或直接跳过，不产生破坏性变更。

## Risks & Dependencies

- 风险：误把“不可用”当“删除”仍会导致数据污染。
- 依赖：sync-state 与 store 的一致性需要可观察状态。
- 兼容性：必须维持旧数据与旧行为可用。
- 风险：手动切换 data-directory 时状态未重载导致错配。
- 风险：显式恢复文件与 snapshot 重名时，去重可能改变候选优先级并加载错误数据库。
- 风险：只按文件存在验证 file-node 会在身份变化后留下重复节点和错误链接。

## Rollback

- 新状态机以开关形式接入，保留旧流程入口可即时切回。
- 回滚只需关闭新状态判断；不做数据迁移，不改变现有 store 格式。
- 出现异常时先暂停自动同步，待目录可用后手动全量 rescan 恢复一致性。

## Feature Toggle

- `supertag-sync-snapshot-guard` 默认开启；关闭时回退旧同步入口。
- `supertag-sync--check-and-sync` 作为统一入口，内部选择新/旧流程。
