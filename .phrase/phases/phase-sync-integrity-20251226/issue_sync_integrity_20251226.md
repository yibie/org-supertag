# issue004 [ ] 同步在目录不可用时误判删除导致写盘污染

## Summary

当同步目录未挂载/不可用时，同步流程把“不可用”当成“文件删除”，批量标记 orphan 并写盘，导致数据库内容被污染甚至丢失。

## Environment

- Repo: org-supertag
- Phase: `phase-sync-integrity-20251226`
- Emacs: （待补充）
- OS: （待补充）
- org-supertag commit: （待补充）

## Repro

1. 配置 `org-supertag-sync-directories` 指向网络盘或可暂时不可用的目录。
2. 启动 Emacs（或切换 vault）时该目录不可用。
3. 自动同步启动（或手动触发 `supertag-sync--check-and-sync`）。
4. 观察：大量节点被标记 orphan，数据库被标记 dirty，随后保存写盘。

## Expected vs Actual

- Expected
  - 目录不可用时不进行破坏性同步，不修改 store，不写盘污染。
- Actual
  - “不可用”被当成“文件删除”，触发 orphan → dirty → save。

## Investigation (Current)

数据结构与权责
- 真实数据源：文件系统（org 文件）。  
- 内存库：`supertag--store`（节点/标签/关系/字段）。  
- 同步状态：`sync-state`（文件 mtime/size/hash）。  
- 缺口：不存在“目录可用性/快照完整性”的一等状态。

破坏性路径（不可用被误判为删除）
- 启动或 vault 切换时触发同步：`supertag-sync-start-auto-sync` 会进入周期性 `supertag-sync--check-and-sync`。  
- `supertag-sync--check-and-sync` 清理 sync-state：对 `file-exists-p` 为 nil 的文件调用 `supertag-sync--verify-file-nodes`。  
- `supertag-sync--verify-file-nodes` 在文件不存在时标记 orphan → `supertag-node-update` → `supertag-ops-commit` → `supertag-mark-dirty`。  
- 自动保存/退出时 `supertag-save-store` 写盘；它只防“节点数=0”，挡不住“节点仍在但被 orphan”的污染状态。

特殊情况列表（当前设计的“边界”）
- 目录未挂载/不可用时，文件系统返回“不存在”，但语义不等同于删除。  
- auto-start 有目录就绪检查，但 vault 激活路径可直接启动同步，绕过该 guard。  
- 同步流程把“观察”与“变更”混在一起，导致不可用时依旧执行破坏性变更。
- 手动切换 data-directory 后 sync-state 未同步切换，导致状态错配与删除误判。

## Fix (Planned)

- 引入同步快照状态（availability/completeness），只有完整快照允许破坏性变更。  
- 同步流程拆分为采样/对比/应用，避免混合逻辑。  
- 保持删除语义不变（Never break userspace）。
- sync-state 固定跟随 data-directory（per-vault），切换时强制重载。

## State Machine (Planned)

- 状态：`unavailable` / `partial` / `complete`。  
- `unavailable`：目录不可用，禁止删除/orphan/清理。  
- `partial`：扫描不完整，仅允许增量更新。  
- `complete`：允许完整对比与应用（含删除/orphan/GC）。

## Fix (Implemented)

- 引入 `supertag-sync-snapshot-guard` (defcustom，默认开启)。
- 快照状态机：`unavailable` / `partial` / `complete`。
- `supertag-sync--allow-destructive-p()` 仅在 complete 快照下允许破坏性操作。
- 同步入口 `supertag-sync--check-and-sync-guarded` 和 `supertag-sync-full-rescan` 均受 guard 保护。

## Compatibility & Rollback

- 数据格式与旧版 sync-state 保持兼容；缺失新字段不触发破坏性路径。  
- 新状态机可开关；出现问题可立即退回旧同步入口。  
- 回滚期间可暂停自动同步，待目录可用后手动全量 rescan。
- 开关变量：`supertag-sync-snapshot-guard`，默认开启；入口函数统一分发新/旧流程。  

## Verification

- 目录不可用时触发同步，store 无破坏性变更，DB 不被污染。  
- 目录可用且文件删除时，orphan/清理行为与现有一致。  
- 恢复目录可用后，sync 正常继续。

## User Confirmation

- [x] 代码审查确认：snapshot guard 已实现并全线接入同步入口

## Resolved

- Resolved At: 2026-06-10
- Resolved By: code review verification
- Commit: (代码已存在，实现于 phase-sync-integrity)

## Related

- Related phase: `phase-sync-integrity-20251226`
- Related tasks: `task002`~`task005` (task_sync_integrity_20251226)
