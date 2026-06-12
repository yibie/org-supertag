# issue005 DB 文件存在但初始化加载为空

## Summary
启动时日志显示 DB 文件存在且有体积，但 `supertag--store` 仍被初始化为空（Nodes=0），导致所有查询/视图看起来“数据库没数据”。

## Environment
- Emacs: 30.x（用户环境出现 `eln-cache/30.2-*`）
- OS: macOS
- Org-Supertag: 5.2.0（`org-supertag.el` 头部版本）

## Repro
1. 确保 `supertag-data-directory` 下存在 `supertag-db.el`（非空，且内容包含 `#1=`/`#1#` 这类 circular print 标记）。
2. 启动 Emacs，观察 `*Messages*`：
   - `Initialized empty Org-Supertag store.`
   - `Database status: exists, Size: ..., Nodes: 0`

## Expected vs Actual
- Expected: DB 文件被读入后 `:nodes` hash-table count > 0。
- Actual: `:nodes` 为空 / 未加载，系统提示“数据库为空”并建议全量 rescan。

## Investigation
### 1) 写盘格式与读盘开关不匹配
`supertag-save-store` 使用 `(print-circle t)` 写盘，因此 DB 文件包含 `#n=`/`#n#` 语法。

若读盘时未显式绑定 `read-circle`，在某些 Emacs 配置/版本中会导致 `read` 失败，从而表现为“读不到数据”。原实现的诊断信息不足，容易被误判为“文件不存在/没数据”。

### 2) 加载路径/候选文件缺少可诊断信息
原 `supertag-load-store`：
- 只用 `(or file supertag-db-file)` 单点路径；
- 当加载失败或路径不一致时日志不包含“尝试读取的路径”；
- init 中出现 `:new` + “file exists” 的矛盾状态时，只会盲目重试，无法定位根因（路径漂移/不可读/候选不完整）。

## Fix
- `supertag-load-store`：
  - 显式 `read-circle t`；
  - 引入 DB 候选文件列表（explicit/configured/default/legacy/snapshot），选择第一个可读的常规文件；
  - 兼容 root key 别名（如 `nodes`/\"nodes\" -> `:nodes`）；
  - `supertag--store-origin` 记录 `:loaded-from` 与 `:load-candidates` 以便定位问题。
- `org-supertag.el`：
  - 当出现 “origin :new + DB exists” 时，输出 candidates 并用显式路径重试。
- `supertag-db-inspect-file`：
  - 同样绑定 `read-circle t`，并使用同一套候选逻辑定位实际检查的文件。

Implementation note:
- Emacs regexp 需要用 `\\( ... \\)` 分组。候选文件匹配的正则已修正为 `^supertag-db-.*\\.\\(el\\|db\\)$`，避免 `invalid-regexp "Unmatched ) or \\)"`。
- 候选文件选择显式 `expand-file-name` 后再做 `file-exists-p`/`file-readable-p` 检查，避免路径包含 `~` 时在部分环境下误判“不可读/不存在”。

## Verification
手动验证（用户侧）：
1. `M-x supertag-db-inspect-file`：
   - `Node count in file` 显示为预期的大于 0。
2. 重启 Emacs，观察 `*Messages*`：
   - `Database loaded from ...`
   - `Database status: exists, Size: ..., Nodes: <non-zero>`
3. `M-: supertag--store-origin`：
   - `:loaded-from` 指向实际读取的文件；
   - `:load-candidates` 包含期望路径列表。

## Risk / Compatibility
- 只增强读盘与诊断，不改变现有写盘格式；
- 兼容旧文件名/扩展名（`.db`）与 root key 别名；
- 不会主动覆盖用户 DB 文件（写盘仍受现有 protective guard 约束）。

## User Confirmation
- [x] Code review confirmed: read-circle + store-origin + candidates resolution 已实现

## Resolved
- Resolved At: 2026-06-10
- Resolved By: code review verification
- Commit: (代码已存在，实现于 phase-sync-integrity)
