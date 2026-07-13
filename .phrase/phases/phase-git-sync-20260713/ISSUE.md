# issue013 [x] Git 原生同步 P0 边界仍可能静默丢失或越界提交

## Environment

- Branch: `sync/git-native`
- Baseline commits: `81bd1f2`, `ea70974`
- Emacs batch ERT + 真实临时 Git 仓库

## Repro / Actual

- legacy association 三方合并得到 `nil` 且零冲突；
- 双侧插入得到违反 theirs 顺序约束的列表且零冲突；
- 字段 `"order"` 与内部 `:order` 产生相同 conflict ID；
- modify/delete Org 冲突为 unmerged 但无 marker，doctor/scanner 集合为空；
- 预先 staged 的非 allowlist 文件被自动提交；
- migration 可从 `:failed` origin 直写新库并把新 origin 变成 `:ok`。

## Expected

任何无法自动收敛的变更都成为可见冲突；自动提交和迁移不得越过既有数据安全边界。

## Investigation / Root Cause

专用 collection 分派未验证 shape；顺序合并只 append 单侧元素；conflict ID 无类型
命名空间；Git 冲突集合错误依赖 marker；scoped `git add` 后仍用全 index commit；
迁移只用 dirty flag 决定是否检查 persistence guard。

## Fix / Verification

- `supertag-merge.el` 仅对已确认 shape 的 collection 做专用合并；association 按
  `:field-id` 合并并保留两侧顺序约束，字段键与内部元数据使用不同冲突命名空间。
- migration 从内存 store 原子写入并遵守 persistence guard；自动提交拒绝未解决
  index、越界 pre-staged 内容和 staged conflict marker，只 stage 自己的 allowlist。
- doctor/scanner 从 Git index 实时派生所有 unmerged Org 文件；文件身份统一使用
  truename，避免 macOS `/tmp`/`/private/tmp` 路径别名绕过导入与 DB 重载守卫。
- ERT：`./test/run-tests.sh all` 245/245；`supertag-git.el` batch byte-compile 通过。
- 真实环境：`ACCEPTANCE.md` 的 kill-mid-merge、20 轮双写 soak、离线 10 轮追赶均
  PASS，零数据丢失、零提交冲突标记、零越界 junk commit。

关联任务：task001。

- User Confirmation: 2026-07-13，用户确认 release-gate acceptance 完成且结论 PASS。
- Resolved At: 2026-07-13
- Resolved By: Git-sync P0/P1 implementation + release-gate verification
- Resolved Commit: `20cb4b5`, `849206a`（验收记录：`9024dea`）
