# issue015 [ ] file-node 验证忽略持久身份并保留失效节点

## Environment

- PR: #182 `fix/validate-nodes-file-node-orphaning`
- Baseline: `ac914e3`
- Default Org-roam file-node policy and Denote-compatible stores

## Repro / Actual

把文件顶部的 `:ID:` 从 `old-file-id` 改为 `new-file-id`，同步会创建新 file node。
PR #182 的 validation 只检查文件路径是否存在，因此旧、新两个 level-0 node 都继续
指向同一文件；`supertag-find-file-node` 还可能选中旧 ID。

## Expected

- `:link-type id` 的 node 只在顶部 `:ID:` 仍等于自身 ID 时有效。
- `:link-type denote` 的 node 只在 `#+IDENTIFIER:` 仍等于自身 ID 时有效。
- 缺少 `:link-type` 的旧数据在文件存在时保留，避免再次触发 mass orphan。
- 文件删除时所有 file node 仍进入 orphan 流程。

## Investigation / Root Cause

PR #182 正确区分了 file node 与 heading node，但把所有 file node 的有效性缩减为
`file-exists-p`。这覆盖了当前已经持久化的 `:link-type` 语义，并回归了原本对
Org-ID 身份变化有效的清理行为。

## Planned Fix

复用现有 file header parser，由 node 自己的 `:link-type` 选择 Org ID 或 Denote
identifier；只有 legacy node 回退到文件存在判断。验证不依赖当前全局 policy。

## Verification

- [x] Org-ID identity matching / changed
- [x] Denote identity matching / changed
- [x] Legacy live file / deleted file / missing heading ID
- [x] Full ERT 119/119 and byte compilation

## User Confirmation

- [ ] 等待修复推送后由用户验收。

## Related

- PR: #182
- Task: task016
- Existing identity issue: issue011
