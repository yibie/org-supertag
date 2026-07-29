# Git 原生同步变更记录

## 2026-07-29 — task007 / issue028

- Modify `supertag-git-sync--pending-p`：退出判定只保留 Store dirty、受管 working
  tree 和 ahead commit；单独的 debounce timer 与 upstream behind 不再触发同步。
- Add one-shot exit waiter：用户选择同步或退出时已有 Git 链运行，等待器每 250ms
  仅检查既有异步链是否结束。成功且没有新本地工作时调用
  `save-buffers-kill-emacs`；失败、冲突、Store dirty 或新改动则取消自动退出。
- Modify mode disable：取消 exit waiter，避免关闭模式后遗留自动退出。
- Modify Git sync ERT：覆盖 behind-only clean exit、不调用 sync、同步完成自动
  退出、失败/新改动保持打开与 waiter 清理。
- Modify `README.md`, `README_CN.md`：更新无改动直接退出、成功自动关闭与失败
  保持打开的可见行为；只提交本任务对应 hunk，保留用户其他未提交修改。

行为：没有本地笔记变动时，`C-x C-c` 直接退出；需要交付本地内容时选择同步一次，
成功后 Emacs 自动关闭，不再要求第二次退出。网络失败或同步期间继续编辑不会误关
Emacs。风险：低层 `kill-emacs` 继续绕过 query；自动退出等待期间如果其他 buffer
产生修改，`save-buffers-kill-emacs` 仍会按 Emacs 标准流程询问保存。

验证：新增 behind-only 回归先在旧实现上失败；修复后 Git 37/37、默认全量
330/330；临时目录 batch byte-compile 成功（仅既有 warning），
`git diff --check` 通过。

## 2026-07-28 — task006 / issue028

- Add `supertag-git-sync-now`：保存 Store、取消 pending debounce，并复用既有
  guarded commit/push 或 fetch/merge/push 链立即同步；主入口注册 runtime
  autoload，源码安装可直接从 `M-x` 发现。
- Modify `supertag-git-sync-mode` lifecycle：启用时注册
  `kill-emacs-query-functions`，关闭时对称移除。正常退出会检查 Store dirty、
  pending timer/in-flight、受管 working tree 和本地 upstream ahead/behind。
- Modify `README.md`, `README_CN.md`：说明立即同步、退出选择与低层
  `kill-emacs` 的明确边界；仅提交本任务新增段落，保留文件中既有未提交改动。
- Modify `test/git-sync-mode-test.el`：真实临时 remote 覆盖立即 commit/push、
  clean exit、同步后取消本次退出、明确 local-only 退出、dirty Store/in-flight
  强制取消，以及 mode hook 清理。

行为：`C-x C-c` 不再悄悄跑赢 30 秒 debounce 或正在执行的 Git 链。若内容已安全
留在 working tree/local commit，用户仍可明确选择离线退出；Store 尚未落盘或 Git
进程仍运行时则拒绝普通退出，避免数据丢失或中断仓库操作。风险：Emacs 的低层
`kill-emacs` 按平台契约绕过 query；退出检查只比较最近 fetch 的 upstream，选择
立即同步后才通过现有网络链刷新远端。

验证：先确认新增回归在缺少实现时失败；实现后 Git 37/37、默认全量 315/315；
临时目录 batch byte-compile 成功（仅既有 warning），`git diff --check` 通过，
仓库内 `*.elc` 数量为 0。

## 2026-07-28 — task005 / issue027

- Modify `supertag-git.el`：移除 vault 目录之后紧邻的句号，启用提示从视觉上
  歧义的 `/notes/.` 变为实际路径 `/notes/`；内部 repo root 和目录比较不变。
- Modify local Nova config（仓库外）：在原有 Org-Supertag idle-load 块中调用
  `(supertag-git-sync-mode 1)`，以后每次启动加载插件时自动开启，而不是切换状态。

行为：提示现在精确反映内部路径；用户无需每次启动手动执行模式命令。风险：自动
同步仍依赖 setup 已完成、远端可用与本 clone 的 merge driver 配置，失败会沿用
既有模式提示与重试路径。

验证：Git 36/36、默认全量 314/314、Nova 配置 `check-parens`、
`git diff --check`；仓库内 `*.elc` 数量为 0。

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

实机确认：2026-07-28，用户在真实笔记 vault 中运行 Doctor；Git Sync 第 8 节
显示仓库/DB/driver/attributes/tracking/remote 全部正常，sync mode ON、pending
push 0、Text Conflicts none；issue025 关闭。

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
