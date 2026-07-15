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

- task010 [x] 修复 persistence 显式 DB 候选被 snapshot 重复项降级（issue010）
  - 产出：保持 `explicit → configured → default → legacy → snapshot` 的首次出现顺序；增加有效显式 DB 优先级回归测试
  - 验证方式：`supertag-persistence-test.el` 4/4 以上全部通过；显式有效 DB 覆盖 configured DB
  - 影响范围：`supertag-core-persistence.el`、`supertag-persistence-test.el`

- task011 [x] 禁止为无持久化身份的文件生成临时 file-node ID（issue011）
  - 产出：file-node policy 支持 org-roam/denote/auto/disabled；无匹配身份时不创建 file-node
  - 验证方式：同一无 ID 文件重复同步不会增加 file-node；Org-ID 与 Denote 身份保持稳定
  - 影响范围：`org-supertag.el`、`supertag-services-sync.el`

- task012 [x] 集中 file-node link codec（issue011）
  - 产出：节点保存 `:link-type`；UI/relation 根据目标节点生成和匹配 `id:`/`denote:`
  - 验证方式：同一 store 内 Org-ID file-node、Denote file-node 与 heading 分别生成正确链接；Denote reciprocal link 不再硬编码 `id:`
  - 影响范围：`supertag-ops-node.el`、`supertag-ui-commands.el`、`supertag-ops-relation.el`

- task013 [ ] 将 reciprocal backlink 从物理写入迁移为 relation 派生视图（issue011）
  - 产出：审计旧物理 backlink 与用户正向链接不可区分问题，确定兼容迁移路径后停止新增物化 backlink
  - 验证方式：reference 只保留一个方向的物理链接，反向视图由 relation index 查询；旧数据不丢失
  - 影响范围：relation 创建/删除、sync reference extraction、Node View backlink
  - 当前结论：不能直接停止写入；旧的自动 backlink 与用户手写正向链接语法相同，需先引入可识别的 ownership/migration 边界

- task014 [x] 在中英文 README 说明 file-node 兼容策略（issue011）
  - 产出：提供 org-roam/denote/auto/disabled 可复制配置、身份边界和重扫步骤
  - 验证方式：中英文配置值一致；Markdown diff 检查通过
  - 影响范围：`README.md`、`README_CN.md`

- task015 [x] 修复 snapshot guard 延迟删除丢失重试（issue014）
  - 产出：single-file processor 独占 sync-state 推进；async processor 不再覆盖
    deferred mtime；worker 失败后允许 deferred file 重新入队；删除不可达 guard 和
    不可靠源码 walker
  - 验证方式：跨重启与 worker 异常回归均 RED→GREEN；完整 ERT 116/116；aggregate 3/3
  - 影响范围：`supertag-services-sync.el`、`test/sync-worker-regression-test.el`、
    `test/run-tests.sh`、`CHANGELOG.org`

- task016 [x] 按 file-node 自身 link type 验证持久身份（issue015）
  - 产出：Org-ID/Denote file node 校验对应文件身份；缺少 link type 的 legacy node
    继续以文件存在为安全兜底
  - 验证方式：Org-ID 与 Denote 的匹配/变更回归；legacy/live、deleted file 与 heading
    回归；完整 ERT、byte-compile、`git diff --check`
  - 影响范围：`supertag-services-sync.el`、`test/sync-worker-regression-test.el`、
    `CHANGELOG.org`
  - 完成：Org-ID/Denote 身份变更回归、legacy/deleted/heading 回归和完整 ERT
    119/119 通过；issue015 保持待用户验收状态
