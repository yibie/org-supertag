# Git 原生同步变更记录

## 2026-07-28 — task004 / issue025

- Modify `org-supertag.el`：为 README 公开的 `supertag-doctor` 注册 runtime
  autoload，直接加载源码时无需 package-generated autoload 也能从 `M-x` 发现。
- Modify `test/git-sync-mode-test.el`：隔离命令发现回归增加 Doctor，并设置
  `load-prefer-newer`，防止仓库内陈旧 `.elc` 掩盖最新源码。
- Delete local generated artifacts：按用户要求删除仓库内 6 个未跟踪 `*.elc`；
  复查仓库内数量为 0，删除物均可由源码重新编译。

行为：Doctor 与 Git setup/clone/sync-mode 现在共享同一套显式延迟加载入口；源码
checkout 不再因缺少生成的 autoload 文件而丢失健康检查命令。风险：已经运行的
Emacs 仍需 `(require 'supertag-doctor)` 或重启后加载新入口。

验证：Git 36/36、默认全量 314/314；删除全部本地 `.elc` 后 Git 再次 36/36；
临时目录 batch byte-compile 与 `git diff --check` 通过。

## 2026-07-27 — task003 / issue024

- Modify `org-supertag.el`：为文档公开的 `supertag-git-setup`、
  `supertag-git-clone`、`supertag-git-sync-mode` 注册 runtime autoload；直接
  `require` 源码时也可从 `M-x` 发现命令，同时保持 Git 模块按需加载。
- Modify `test/git-sync-mode-test.el`：使用隔离 `emacs -Q` 加载入口文件，验证三个
  命令均为可交互 autoload，且 `supertag-git` 未被提前加载。

行为：源码 checkout、未生成 package autoload 的本地安装不再丢失 Git 同步入口。
风险：已运行的 Emacs 需要重新加载 `org-supertag`；当前会话可先
`M-: (require 'supertag-git)`。

验证：Git 子套件 36/36、默认全量 314/314；batch byte-compile 通过，仅有既有
warning；`git diff --check` 通过。

实机确认：2026-07-28，用户重新执行 `supertag-git-setup`，向重新创建的空远端
完成首次 push；issue024 关闭。

## 2026-07-27 — task002 / issue023

- Modify `supertag-core-persistence.el`：恢复覆盖前取得数据库锁并创建唯一
  `supertag-db-prerestore-*`；dirty Store 直接序列化到恢复点；premigrate 与
  preformat6 reload 禁用自动迁移；快照摘要复用 loader 的 root 规范化；覆盖与
  reload 全程保留同一 advisory lock，不留下竞争写窗口。
- Modify `test/supertag-restore-test.el`, `test/run-tests.sh`：增加降级免迁移、dirty
  恢复点、锁冲突、旧根键摘要回归，并接入默认/`persist`/`restore` 测试清单。
- Modify `README.md`, `README_CN.md`：明确降级恢复后立即退出并用旧版重开，以及
  锁拒绝与 pre-restore 可逆路径。

行为：恢复不再破坏选中的降级文件，也不会在另一实例持锁时覆盖 DB；每次确认恢复
都有独立、可再次选择的回滚点。风险：降级恢复后继续在 6.0 会话编辑并保存，仍会
按 6.0 canonical 格式写盘，因此命令与文档均要求立即退出。

验证：`./test/run-tests.sh restore` 13/13、`persist` 31/31、
`./test/run-tests.sh all` 313/313；core + restore test batch byte-compile 成功，
core 仅报告既有 warning。

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
