# issue023 — 快照恢复会破坏降级文件并绕过数据安全保护

## Environment

- Org-Supertag 6.0，`supertag-db-auto-migrate` 与 `supertag-db-lock` 使用默认值
- `backups/` 中存在 daily、premigrate 或 preformat6 快照
- 可能存在未落盘 Store 修改、当天 daily backup 或另一 Emacs 实例

## Reproduction

1. 运行 `M-x supertag-restore` 并确认恢复旧版 premigrate 快照。
2. 或在当天备份已存在、Store dirty、数据库由另一实例持锁时确认任意恢复。
3. 检查恢复后的磁盘格式、可逆恢复点与锁保护。

## Expected vs actual

- Expected: 降级快照保持旧格式；覆盖前持有数据库锁；每次恢复都有包含当前实际状态的
  唯一恢复点；旧根键快照的预览计数正确。
- Actual: 通用加载路径会立即自动迁移旧快照；daily backup 不能保存 dirty 状态且当天
  只创建一次；直接 `copy-file` 绕过锁；摘要未复用加载规范化；新增测试未进入 CI。

## Investigation

五个问题都收敛在恢复命令边界，无需改变通用保存、迁移或 daily backup 语义。恢复
在确认后应先取得既有 advisory lock，再按 dirty 状态选择序列化内存 Store 或复制
磁盘 DB；premigrate/preformat6 只在本次 reload 动态禁用自动迁移。

## Fix

- 每次确认恢复创建唯一 `supertag-db-prerestore-*`，并纳入恢复选择器。
- 覆盖前调用现有锁获取路径；冲突或锁获取失败时以 `user-error` 终止，并在覆盖与
  reload 完成之间保留同一把锁。
- premigrate/preformat6 reload 动态绑定 `supertag-db-auto-migrate=nil`。
- 摘要复用 `try-read -> coerce -> canonicalize`。
- `supertag-restore-test.el` 加入默认/`persist`/`restore` 测试入口。

## Verification

- `./test/run-tests.sh restore`: 13/13
- `./test/run-tests.sh persist`: 31/31
- `./test/run-tests.sh all`: 313/313
- core + restore test batch byte-compile: success（core 仅既有 warning）

## Tracking

- Task: `task002`
- Status: implementation complete; pending user validation in a real Emacs session
