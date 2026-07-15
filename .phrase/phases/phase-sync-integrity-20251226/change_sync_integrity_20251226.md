# Sync Integrity Change Log

## 2026-07-14 — task016 / issue015

- Modify `supertag-services-sync.el`：file-node validation 改为读取节点自身的
  `:link-type`；Org-ID 校验顶部 `:ID:`，Denote 校验 `#+IDENTIFIER:`，未知或缺失
  link type 的 legacy node 继续以文件存在为保守兜底。
- Modify `test/sync-worker-regression-test.el`：锁定 Org-ID 与 Denote 身份替换后旧节点
  orphan、新节点成为文件唯一入口的行为；保留 legacy/live、deleted file 与 heading
  回归。
- Modify `CHANGELOG.org`：删除“所有 file-node 只按文件存在校验”的错误承诺，记录
  per-node identity 与 legacy fallback 的实际边界。
- Add `issue_sync_integrity_20260714_file_node_validation.md`：登记 PR #182 的身份变化
  回归与用户验收状态。

验证：sync-worker 6/6；完整 ERT 119/119；byte-compile、bash 语法和
`git diff --check` 通过。

## 2026-07-14 — task015 / issue014

- Modify `supertag-services-sync.el`：移除 async processor 的重复 sync-state 推进，
  保证 deferred deletion 的旧 mtime 跨重启保留；删除无回滚的 `:queued` 门控，
  让 worker 异常后的 deferred file 重新入队；删除两段不可达 guard。
- Modify `test/sync-worker-regression-test.el`：以真实混合删除/更新重启回归替换不可靠
  的 repo-wide 源码 walker；增加 worker 异常后重新入队回归。
- Modify `test/run-tests.sh`：aggregate 测试进入默认 `all`。
- Modify `CHANGELOG.org`：收窄为已经验证的行为承诺。

验证：完整 ERT 116/116；sync-worker 3/3；aggregate 3/3；byte-compile、bash 语法和
`git diff --check` 通过。实现提交：`4ea00e7`。

- 2026-07-11 Docs
  - Files: `README.md`, `README_CN.md`
  - Changes: 增加 file-node 的 org-roam/denote/auto/disabled 配置、混合目录建议、持久化身份边界与重扫步骤。
  - Verification: 中英文配置值逐项一致；`git diff --check` 通过。
  - Related: `issue011`, `task014`

- 2026-07-11 Modify
  - Files:
    - `org-supertag.el`
    - `supertag-services-sync.el`
    - `supertag-ops-node.el`
    - `supertag-ops-relation.el`
    - `supertag-ui-commands.el`
    - `supertag-ui-query-block.el`
    - `supertag-ui-search.el`
    - `supertag-view-table.el`
    - `test-denote-reference.el`
  - Changes:
    - file-node 只接受持久化的 file-level Org ID 或 Denote identifier，不再生成未写回文件的随机 ID；
    - 增加 `org-roam`/`denote`/`auto`/`disabled` policy，并把 `:link-type` 保存到节点；
    - 集中物理链接生成/匹配，混合 store 中每个节点独立使用 `id:` 或 `denote:`；
    - 保留旧 reciprocal backlink 写入，等待 task013 先解决旧自动 backlink 与用户正向链接不可区分的迁移问题。
  - Verification:
    - file-node/Denote 5/5、add-reference 4/4、file display 8/8、field reference 6/6、concept 4/4、persistence 5/5；
    - inline-tag self-check 与 `org-supertag.el` batch load 通过。
  - Related: `issue011`, `task011`, `task012`, `task013`

- 2026-07-11 Plan
  - Files: file-node identity/link/reference modules and regression tests
  - Changes:
    - 将不稳定 file-node ID、链接 codec 分散和物理 backlink 重复存储登记为 issue011；
    - 拆分 task011~task013，先锁定稳定身份和 link codec，再处理旧 reciprocal backlink 迁移。
  - Related: `issue011`, `task011`, `task012`, `task013`

- 2026-07-11 Add
  - Files:
    - `supertag-core-persistence.el`
    - `supertag-persistence-test.el`
    - `.phrase/docs/ISSUES.md`
    - `.phrase/docs/CHANGE.md`
    - `.phrase/phases/phase-sync-integrity-20251226/plan_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/task_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/issue_sync_integrity_20260711_persistence_failover.md`
  - Changes:
    - 候选去重保留首次出现顺序，恢复 explicit DB 的最高优先级；
    - 新增有效显式 DB 覆盖 configured DB 的直接回归测试；
    - persistence 5/5、concept 4/4、add-reference 4/4、inline-tag、denote-reference 与主包 batch load 通过。
  - Related: `task010`, `issue010`

- 2026-01-05 Modify
  - Files:
    - `supertag-core-persistence.el`
    - `org-supertag.el`
    - `.phrase/docs/ISSUES.md`
    - `.phrase/phases/phase-sync-integrity-20251226/issue_sync_integrity_20260105_db_not_loaded.md`
    - `.phrase/phases/phase-sync-integrity-20251226/task_sync_integrity_20251226.md`
  - Changes:
    - 修复/增强 DB 读盘：显式 `read-circle`，基于候选列表选取可读 DB，兼容 root key 别名；
    - `supertag--store-origin` 记录 `:loaded-from`/`:load-candidates`，启动重试输出 candidates，便于定位“文件存在但加载为空”。
  - Related: `task009` (task_sync_integrity_20251226), `issue005`

- 2025-12-31 Modify
  - Files:
    - `supertag-view-table.el`
    - `CHANGELOG.org`
    - `.phrase/phases/phase-sync-integrity-20251226/spec_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/plan_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/task_sync_integrity_20251226.md`
  - Changes:
    - Refs 列显示/编辑合并 `add-reference` 创建的 `:reference` 关系；
    - 编辑 Refs 时以关系集为基准同步 backlinks。
  - Related: `task008` (task_sync_integrity_20251226)
- 2025-12-31 Modify
  - Files:
    - `supertag-view-table.el`
    - `CHANGELOG.org`
    - `.phrase/phases/phase-sync-integrity-20251226/spec_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/plan_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/task_sync_integrity_20251226.md`
  - Changes:
    - Refs 列默认作为 `node-reference` 字段并可编辑；
    - 全局字段模式下自动创建/关联 `Refs` 字段；
    - Refs 行内容携带跳转元数据，`C-o` 可跳转引用目标。
  - Related: `task007` (task_sync_integrity_20251226)
- 2025-12-26 Modify
  - Files:
    - `supertag-services-sync.el`
    - `.phrase/phases/phase-sync-integrity-20251226/spec_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/plan_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/issue_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/task_sync_integrity_20251226.md`
  - Changes:
    - 绑定 sync-state 到 data-directory 并在切换时强制重载；
    - 补充 per-vault sync-state 约束与风险说明；
    - 新增并完成 task006 跟踪。
  - Related: `task006` (task_sync_integrity_20251226)
- 2025-12-26 Modify
  - Files:
    - `supertag-services-sync.el`
  - Changes:
    - 修复 `supertag-sync--ensure-state-format` 括号不匹配导致的加载错误。
    - 重写 `supertag-sync--process-single-file` 函数体以修复括号不匹配。
  - Related: `task004` (task_sync_integrity_20251226)
- 2025-12-26 Modify
  - Files:
    - `supertag-services-sync.el`
    - `.phrase/phases/phase-sync-integrity-20251226/task_sync_integrity_20251226.md`
  - Changes:
    - 引入同步快照采样/对比/应用流水线与状态守卫；
    - 不可用/不完整快照时禁用破坏性同步，并延迟删除验证；
    - 更新 task004 状态。
  - Related: `task004` (task_sync_integrity_20251226)
- 2025-12-26 Modify
  - Files:
    - `.phrase/phases/phase-sync-integrity-20251226/spec_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/plan_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/issue_sync_integrity_20251226.md`
  - Changes:
    - 明确状态机开关与入口函数命名；
    - 完善回滚开关说明。
  - Related: `task003` (task_sync_integrity_20251226)
- 2025-12-26 Modify
  - Files:
    - `.phrase/phases/phase-sync-integrity-20251226/spec_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/plan_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/issue_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/task_sync_integrity_20251226.md`
  - Changes:
    - 增加同步快照状态机定义与允许的变更边界；
    - 更新 task003 状态。
  - Related: `task003` (task_sync_integrity_20251226)
- 2025-12-26 Modify
  - Files:
    - `.phrase/phases/phase-sync-integrity-20251226/spec_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/plan_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/issue_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/task_sync_integrity_20251226.md`
  - Changes:
    - 补充兼容性约束与回滚策略；
    - 更新 task005 状态。
  - Related: `task005` (task_sync_integrity_20251226)
- 2025-12-26 Add
  - Files:
    - `.phrase/phases/phase-sync-integrity-20251226/spec_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/plan_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/task_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/change_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/issue_sync_integrity_20251226.md`
  - Reason: 为同步机制结构性回顾与修复建立独立 phase 与 issue 跟踪
  - Related: `task001` (task_sync_integrity_20251226)
- 2025-12-26 Modify
  - Files:
    - `.phrase/phases/phase-sync-integrity-20251226/issue_sync_integrity_20251226.md`
    - `.phrase/phases/phase-sync-integrity-20251226/task_sync_integrity_20251226.md`
  - Changes:
    - 补充同步数据结构与破坏性路径分析；
    - 更新 task002 状态。
  - Related: `task002` (task_sync_integrity_20251226)
