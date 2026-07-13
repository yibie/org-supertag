# Git 原生同步变更记录

## 2026-07-13 — task001 / issue013

- Modify `supertag-merge.el`：为 association 增加严格 shape 判别、按 `:field-id`
  的有序集合合并，以及字段/元数据冲突 ID 命名空间；未知 shape 回退到整体三方
  裁决，不再猜测 plist。
- Modify `supertag-git.el`：迁移遵守 persistence origin/guard 并从已加载 store
  原子写入；自动提交拒绝 unmerged index 与越界 pre-staged 内容，只操作 allowlist；
  Git/DB/Org 冲突路径统一以 truename 比较。
- Modify `supertag-doctor.el`：从当前 Git index 重新计算 Org 文本冲突，跨 Emacs
  重启仍可诊断。
- Modify `test/merge-test.el`, `test/git-sync-mode-test.el`：增加 legacy/异常 shape、
  双侧插入顺序、冲突 ID、modify/delete、越界 staging、迁移 guard、macOS 符号链接
  路径等回归。
- Add `ACCEPTANCE.md`：记录真实 Git/Emacs release-gate 的 kill-mid-merge、20 轮
  双写 soak、离线 10 轮追赶演练。

行为：两台机器的修改要么自动收敛，要么进入 doctor 可见且可裁决的冲突；自动提交
不拥有用户的其他 staged/untracked 文件。风险集中在 Git 认证与远端可用性，失败时
保留本地 commit 并在后续周期追赶，不以覆盖远端状态降级。

验证：`./test/run-tests.sh all` 245/245；Git 子套件 35/35；
`supertag-git.el` batch byte-compile；`ACCEPTANCE.md` 三个真实场景全部 PASS。

提交：`20cb4b5`, `849206a`；验收记录：`9024dea`。
