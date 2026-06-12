# task_sync_integrity_20251226

- task001 [x] 建立 sync-integrity phase 文档与 issue 记录  
  - 产出：`spec_sync_integrity_20251226.md` / `plan_sync_integrity_20251226.md` / `task_sync_integrity_20251226.md` / `change_sync_integrity_20251226.md` / `issue_sync_integrity_20251226.md`  
  - 验证方式：上述文件存在且内容与“同步机制结构性修复”目标一致（手动检查）  
  - 影响范围：仅内部文档结构

- task002 [x] 梳理同步数据结构与破坏性路径  
  - 产出：明确“快照→对比→应用”的数据流与当前破坏性点（含入口函数）  
  - 验证方式：issue 文档更新 Investigation，并给出路径清单  
  - 影响范围：文档与设计结论

- task003 [x] 设计同步快照状态机（availability/completeness）  
  - 产出：状态定义、状态转移、允许的变更类型与兼容性约束  
  - 验证方式：spec/plan/issue 中记录清晰决策  
  - 影响范围：设计文档

- task004 [x] 实现同步流水线改造  
  - 产出：同步流程拆分为采样/对比/应用；不可用时不执行破坏性变更  
  - 验证方式：代码路径检查 + 手动场景验证（目录不可用/恢复）  
  - 影响范围：`supertag-services-sync.el` 等核心同步路径

- task005 [x] 兼容性与回滚验证  
  - 产出：不破坏现有删除语义；保留旧数据兼容  
  - 验证方式：spec/plan/issue 中列出兼容性检查项与回滚策略  
  - 影响范围：文档与回归验证

- task006 [x] 每个 vault 独立 sync-state 文件与切换重载  
  - 产出：sync-state 跟随 data-directory；切换/手动变更时强制重载  
  - 验证方式：切换 data-directory 后 sync-state 文件路径更新且不会误删  
  - 影响范围：`supertag-services-sync.el` 与文档说明

- task007 [x] Table view Refs 列默认 node-reference 字段  
  - 产出：Refs 列固定在末尾且可编辑，默认使用 node-reference；全局字段模式自动创建/关联 `Refs`  
  - 验证方式：打开任意 tag 的 table view，Refs 列可编辑并可 `C-o` 跳转；全局字段模式下自动出现 `Refs` 定义  
  - 影响范围：`supertag-view-table.el`、`CHANGELOG.org`、phase 文档

- task008 [x] Refs 列合并 add-reference 关系  
  - 产出：Refs 列展示/编辑合并 `add-reference` 产生的 `:reference` 关系  
  - 验证方式：通过 `supertag-add-reference` 创建引用后，Refs 列立即可见并可编辑  
  - 影响范围：`supertag-view-table.el`、`CHANGELOG.org`、phase 文档

- task009 [x] 修复 DB 文件存在但加载为空（issue005）
  - 产出：
    - `supertag-core-persistence.el`：读盘显式 `read-circle`，候选 DB 解析，补充 store-origin 诊断信息
    - `org-supertag.el`：启动重试信息包含 candidates，并用显式 DB 路径重试
    - `issue_sync_integrity_20260105_db_not_loaded.md`：记录复现/根因/验证
  - 验证方式：
    - `M-x supertag-db-inspect-file` 显示 file 内 nodes > 0
    - 重启 Emacs 后 `Database status ... Nodes` 为非零
  - 影响范围：持久化加载路径、启动诊断日志
