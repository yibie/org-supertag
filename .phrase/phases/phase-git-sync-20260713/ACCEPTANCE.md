# Git 原生同步 — RELEASE-GATE 验收报告

日期：2026-07-13
执行人：release-gate acceptance run（本报告由 orchestration 脚本驱动的真实 E2E 执行产生，不是复述计划）

## 范围与方法

本报告执行 `.phrase/phases/phase-git-sync-20260713/PLAN.md` 中 S3 验收 E2E-7 与 S4 验收
三条此前从未被真正跑过的 E2E 场景。所有编排脚本为一次性/throwaway，位于
`/tmp/supertag-release-gate/`，仓库内唯一被创建/修改的文件是本文件。

**关键约束的遵守方式**：

- 场景由**独立的 `emacs -Q --batch` 进程**驱动 A、B 两端，每个进程通过
  `/tmp/supertag-release-gate/common/common.el` 加载真实的
  `supertag-core-persistence.el` / `supertag-merge.el` / `supertag-git.el` /
  `supertag-conflicts.el`，真实 `git` 二进制，真实裸仓库 + 两个 clone。
- 提交/推送/拉取一律通过 `supertag-git-sync--fire-commit` /
  `supertag-git-sync--pull` / `supertag-git-sync--push`（真实函数）驱动，只在
  `let`/`setq` 绑定 `supertag-git-sync--synchronous` 为 t（任务书明确允许——只绕过
  `make-process` 异步管线，不绕过任何判断逻辑）。
- 唯一被"绕过"的是 `supertag-save-store` 的 sync-state 门禁（本 harness 从不加载
  `supertag-services-sync.el`），做法与
  `test/git-sync-mode-test.el`/`test/persistence-hardening-test.el` 完全相同的手法
  （neutralize `supertag--persistence--expected-sync-state-file`，仅用于
  `supertag-conflicts-use-ours-all` 结尾会调用的真实 `supertag-save-store`）——这
  不是对被测代码路径的 stub，是复用测试套件自己验证过的 fixture 手法。
- 冲突制造使用直接的 store 哈希表读写 + `supertag--persistence--write-canonical-store`
  （与 `test/git-sync-mode-test.el` 的 `--write-store`/`--edit-node` helper 手法相同），
  而非完整 ops/transaction API——因为完整 API 需要 `supertag-services-sync.el` 的
  sync-state 基础设施，与本次验收目标（git 同步层，非 S1 事务层）无关，且会引入
  与被测代码无关的摩擦。字段编辑、新建节点、tag 操作（新建 `:tags` collection 实体 +
  写入 node 的 `:tags` 字段）均有覆盖。

## 基线测试

`./test/run-tests.sh all`：

- **开始时**：`Ran 244 tests, 244 results as expected, 0 unexpected` （244/244，全绿）
- **结束时**（本报告写作前的最后一次运行）：`Ran 244 tests, 244 results as expected, 0
  unexpected` （244/244，全绿）—— 无状态泄漏；`git status --porcelain` 全程干净，
  仓库内除本文件外没有任何改动。

---

## Scenario 1 — TRUE kill -9 mid-merge（E2E-7，诚实版）

**目标**：A、B 在 DB 上产生分叉（不同实体 + 同一实体同字段），B 端合并过程中把
**merge driver 子进程**和**发起合并的外层 Emacs 进程**都 kill -9，验证仓库不损坏、
worktree 可恢复、重试后干净收敛、零数据丢失。

**Artifacts**：
- 脚本：`/tmp/supertag-release-gate/scenario1/*.el`,
  `run-scenario1.sh`, `merge-driver-wrapper.sh`
- 运行时数据：`/tmp/supertag-release-gate/scenario1-run/`
  （`bare.git`、`a/`、`b/`、`pre-merge-b-db.snapshot`、`b-precommit-head`）
- 日志：`/tmp/supertag-release-gate/logs/scenario1/*.log`，
  完整编排器 transcript：`/tmp/supertag-release-gate/logs/scenario1-orchestrator.log`

### 搭建

- 裸仓库 + A worktree，baseline store 含 5000 个 filler 节点（用于拉宽 merge driver
  自身 parse/serialize 的真实墙钟耗时窗口，见下文"时序校准"）+ `shared-node-1` +
  `node-baseline`。A `git push -u`（**必须用 `-u`**：机器 1 首次 push 走
  `supertag-git-setup--push` 的真实实现，永远带 `-u`；本 harness 起初漏加，导致
  A 之后自己的 `git push`（无参数）因"当前分支没有 upstream"直接失败，从未真正触发
  过 push-rejected→fetch→merge 链路——这是本次执行中发现的第一个 harness 缺陷，已修复，
  记录于此以防复现）。
- B `git clone`，本 clone 配置 merge driver。
- A 编辑 `a-private-node`（新增，disjoint）+ `shared-node-1.status="A-changed"`，
  真实 `supertag-git-sync--fire-commit` 提交+推送成功。
- B（仍在 baseline）编辑 `b-private-node`（新增，disjoint）+
  `shared-node-1.status="B-changed"`（**同实体同字段不同值**，强制 driver 做真正的
  field-level 三方裁决），只 stage+commit（用真实
  `supertag-git-sync--commit-pathspecs` 计算的 pathspec，不是裸 `add -A`），**不推送**
  ——这一步之后的 DB 字节内容被快照为"pre-merge"基准。

### Kill 手法

一个 wrapper shell 脚本被临时配置为 B 端 `.git/config` 的
`merge.supertag-db.driver`：`exec` 真实驱动**之前**先把自己的 PID（`exec` 不换 PID）
和 git 传入的 `%A`（ours 临时文件，已解析为绝对路径——第二个 harness 缺陷：git 传入
的 `%A` 其实是**相对于仓库根**的路径如 `.merge_file_XXXXXX`，最初按绝对路径处理导致
"温度探测"glob 永远不命中任何东西，已修复）写入约定文件，`touch` 一个 ready 文件，
`sleep` 固定延迟后再 `exec` 真实驱动。编排器（bash）：

1. 后台启动"B 编辑+`supertag-git-sync--push`"这个外层 Emacs 进程（同步 git 调用，
   即任务书所说"OUTER git 进程的 emacs parent"）。
2. 轮询 ready 文件出现（~200-350ms 后出现，稳定）。
3. **立即** `kill -9` driver 子进程 + 该外层 Emacs 进程（不再等待/轮询驱动自己的
   `.merge-tmp` 临时文件出现——见下"时序校准"为什么放弃这个更精细的目标）。

### 时序校准（诚实记录，非事后美化）

最初设计尝试轮询 driver 自身原子写入用的
`(make-temp-file (concat path ".merge-tmp"))` 临时文件，力图把 kill 精确打在
"临时文件已创建、尚未 `rename-file`"这个最窄的窗口内。反复校准发现：**在本沙箱环境
中，任何并发的 bash 轮询循环（哪怕 50ms 间隔的粗粒度轮询）都会显著拖慢被观察的
git/emacs/driver 子进程**——一次干净、无人观测的完整链路跑 1.6-4.6 秒（取决于 filler
节点数），同一条链路被并发轮询观测时被观察到耗时 15 秒到 50+ 秒甚至不在慷慨预算内
完成。这使得"轮询直到看到临时文件再 kill"在本环境下不可靠：曾有一次轮询预算设得足够
大，driver 在轮询期间**完整成功跑完**（真实合并、真实提交、真实推送全部成功），随后
的 `kill -9` 打在一个早已正常退出的进程上，产生了一次名不副实的"kill 测试"（日志里
能看到完整的 `S1-B-PUSH-COMPLETED` 行）。

修复：编排器现在 gate 在"是否真正打断了它"（该 attempt 的日志里**不出现**
`S1-B-PUSH-COMPLETED` 完成标记）而不是"是否恰好撞见临时文件"，且检测到 ready 后立即
kill，不再额外等待。这保证每次 kill 都是货真价实地在合并/驱动仍存活时打断它，代价是
放弃了"精确命中 write-region 那个最窄子窗口"这个更苛刻的子目标——`temp-file-seen`
字段在报告里始终为信息性记录，不作为 pass/fail 依据。这被视为验收执行本身的一个已知
局限，如实写在这里，而不是掩盖。

### 结果（连续 3 次独立完整重跑，结果一致）

| 断言 | 结果 |
|---|---|
| 首次 attempt 即真正打断合并（无需重试） | 通过（`attempts_used=1`，日志确认无完成标记）|
| `git fsck --full --strict`（B 仓库）| 干净（exit 0，无输出）|
| kill 后 `MERGE_HEAD` / unmerged 路径 | 均不存在——git 自身在 driver 子进程异常退出后，
  对"这是本次合并里唯一改动的路径"这种情况选择了**整体回滚该次合并**（而非留下
  冲突态等待 `git merge --abort`），比 E2E-7 原始设想的"需要显式 abort"更安全 |
| `git merge --abort` | 报告"没有可 abort 的合并"（`fatal: There is no merge to abort
  (MERGE_HEAD missing)`）——印证上一条，git 已自行清理 |
| worktree DB 字节 vs pre-merge 快照 | **`:db-byte-identical-to-pre-merge t`**，
  1120007 字节两侧完全一致 |
| 重试（恢复真实 driver，B 端真实 `supertag-git-sync--pull`）| 干净成功，一次完成 |
| 收敛 | A/B/bare 三处 HEAD 完全相同 |
| 零数据丢失 | `a-private-node`、`b-private-node`、`node-baseline` 三者均存在于合并后
  的 store；`shared-node-1` 进入 `:sync-conflicts`，恰好 2 条记录
  （`:status` 和 driver 自动附带的 `:modified-by`），双方值（`"A-changed"` /
  `"B-changed"`，`"a"`/`"b"`）均完整保留在冲突记录里，未静默覆盖 |

**已知非致命异常**：在校准阶段（早期、后来被修复的轮询策略下）**曾观察到一次**
kill 打在 driver 已成功完成之后的情况（worktree 文件与 pre-merge 快照不同、但仍是
完全合法可加载的最终合并结果，无损坏、无冲突标记）——这不是数据安全问题，而是
"kill 没打中，操作本身正常完整地成功了"，已通过上述 gate 修复排除在最终结果之外，
如实记录于此。

**Scenario 1 结论：PASS**（含上述时序校准局限性的如实说明）。

---

## Scenario 2 — 20 轮双写者拉锯（S4 验收）

**Artifacts**：
- 脚本：`/tmp/supertag-release-gate/scenario2/*.el`, `run-scenario2.sh`
- 运行时数据：`/tmp/supertag-release-gate/scenario2-run/`
- 日志：`/tmp/supertag-release-gate/logs/scenario2/*.log`，
  完整 transcript：`/tmp/supertag-release-gate/logs/scenario2-orchestrator.log`
- 耗时：44.5 秒（20 轮 × 3 个独立 Emacs 进程/轮 + 收尾）

### 设计

每轮（独立 Emacs 进程，A、B 各自的编辑+提交都是分开的真实进程）：

1. A：字段编辑私有节点 `a-node-1`（每 3 轮追加新节点 `a-new-N`，每 4 轮做一次
   tag 操作：新建 `:tags` collection 实体并写入节点 `:tags` 字段）+ 真实
   `supertag-git-sync--fire-commit`（提交+推送）。
2. B（尚未拉取 A 的这轮改动）：同样模式编辑私有节点 `b-node-1`（disjoint 实体）
   + 真实 fire-commit——push 因 A 已推进远端而被拒绝，内部真实
   `supertag-git-sync--push` 的 fetch→merge→重推一次逻辑触发，含真正的
   merge driver 调用。
3. A：真实 `supertag-git-sync--pull`（fetch + merge-if-behind + push-if-ahead）收敛。
4. 全新进程：读取 A 的 DB，字节比较 A/B，读 `supertag-conflicts-count`。

第 5、10、15、20 轮为 overlap 轮：A、B 同时编辑**同一实体的同一字段**，但四轮分别
命中 4 个不同的 (实体, 字段) 组合（`shared-node-1/:status`、
`shared-node-1/:priority`、`shared-node-2/:status`、`shared-node-2/:priority`）——
确保 4 次 overlap 事件产生 4 条**不同**的冲突记录，而不是反复覆盖同一条。

### 结果

| 断言 | 结果 |
|---|---|
| 每轮收敛（A/B DB 字节相同）| **20/20 轮全部收敛**（`converged=t` × 20）|
| 冲突计数按轮次演变 | `1:0 2:0 3:0 4:0 5:1 6:1 7:1 8:1 9:1 10:2 11:2 12:2 13:2 14:2 15:3 16:3 17:3 18:3 19:3 20:4` —— 恰好在第 5/10/15/20 轮各新增 1 条，**总计 4 条**，与验收预期精确一致 |
| 冲突通过全新进程读取 `supertag-conflicts-count` 可见 | 是（`check-round.el` 每轮都是全新 `emacs -Q --batch` 进程）|
| 冲突标记从未被提交 | `git log --all -S'<<<<<<<'` 在 A、B 两仓库均为空输出
  （`:marker-hits-a "" :marker-hits-b ""`）|
| 未扫入 junk 文件 | `junk-a.txt`/`junk-b.txt`（初始提交**之后**才创建的未跟踪文件）
  自始至终未出现在推送到 bare 的树里（`:junk-in-tree nil`）——本 harness 第一次跑时
  把 junk 文件建在初始提交**之前**，被 00-init.el 的裸 `git add -A` 初始提交带进去，
  是一次 harness 时序缺陷（不是产品缺陷），已修复：junk 文件现在建在初始提交之后，
  真正考验的是后续 20 轮的**自动**、**受限 pathspec 的**提交 |
| 第 20 轮后：A 端 `supertag-conflicts-use-ours-all`（真实命令，`y-or-n-p` 按测试
  套件同样手法 stub 为 t）| `:count-before 6→4`（见下）`:count-after 0`，
  `supertag-dirty-p` 变回 nil（真实 `supertag-save-store` 落盘成功）|
| A 推送解决结果，B 拉取 | 成功，B 收敛，冲突计数在两侧均归零
  （`:conflict-count-a 0 :conflict-count-b 0`）|
| 最终字节收敛 | `:byte-identical t`，A/B/bare 三处 HEAD 相同 |

**已知（已修复）harness 缺陷记录**：最初的 overlap 编辑额外附带了一个恒定冲突的
`:modified-by` 字段（每次都是 `"a"` vs `"b"`），导致总冲突数变成 6 而非预期的 4——
这是本 harness 自己制造的多余冲突源，不是产品行为异常；移除后总数精确为 4，与
验收预期一致。

**Scenario 2 结论：PASS。**

---

## Scenario 3 — 离线 10 轮追赶（S4 验收）

**Artifacts**：
- 脚本：`/tmp/supertag-release-gate/scenario3/*.el`, `run-scenario3.sh`
- 运行时数据：`/tmp/supertag-release-gate/scenario3-run/`
- 日志：`/tmp/supertag-release-gate/logs/scenario3/*.log`，
  完整 transcript：`/tmp/supertag-release-gate/logs/scenario3-orchestrator.log`
- 耗时：6.1 秒

### 设计

1. A `git remote set-url origin <不存在的路径>`（断网模拟）。
2. 第 1-5 轮：**各自独立**的 Emacs 进程，每轮字段编辑（+周期性新节点/tag 操作）
   + 真实 `supertag-git-sync--fire-commit`（提交成功，push 静默失败/降级，符合
   `supertag-git-sync--note-offline` 设计）。每轮后用纯 git 本地信息
   （`git rev-list --count @{upstream}..HEAD`，不需要网络）独立验证 pending 计数。
3. 模拟"第 5 轮后 Emacs 重启"：**一个全新进程**里，先把
   `supertag-git-sync--pending-push-count` 强制设为 0（模拟刚重启、毫无记忆的会话
   撒的谎），然后调用真实 `supertag-git-sync--refresh-pending-count`，断言它从 git
   自己的历史正确算出 5（而不是继续相信那个谎）。**同一个全新进程**里接着继续跑
   第 6-10 轮（仍然离线）。
4. 恢复 A 的远端 URL。
5. 一次真实 `supertag-git-sync--pull`（fetch 成功、未落后、ahead>0 触发推送）。
6. 验证：远端 HEAD == A HEAD，pending 归零。
7. B（全新进程）真实 `supertag-git-sync--pull`，验证 10 轮的内容全部到达。

### 结果

| 断言 | 结果 |
|---|---|
| 每轮（1-5，各自独立进程）pending 单调增长，且由 git 独立推导 | `1,2,3,4,5`（与轮次
  号精确相等）|
| "重启撒谎"被真实函数纠正 | `S3-RESTART-TRUTH: (:lie-before 0
  :refreshed-ahead-count 5 :pending-var-after-refresh 5)` —— P1-7 restart-truth
  性质在小规模上得到直接验证 |
| 第 6-10 轮（同一新进程内继续，仍离线）pending 持续正确增长 | `6,7,8,9,10` |
| 恢复网络后一次 pull | `S3-PULL-A: (:pending-after 0 :ahead-after 0)` |
| 全部 10 轮真的落到远端 | `heads-equal t`（A HEAD == bare HEAD），
  `total-log-lines 11`（baseline + 10 轮，每轮恰好一次真实 commit）|
| B 拉取后内容完整 | 跨越全部 10 轮的实体均存在：`a-new-3`/`a-new-6`/`a-new-9`
  （第 3/6/9 轮新建节点）、`a-tag-4`/`a-tag-8`（第 4/8 轮 tag 操作）全部存在；私有
  节点标题为 `"a title round 10"`（最后一轮的值）；B 端 DB 与 A 端**字节完全相同** |

**Scenario 3 结论：PASS。**

---

## 总体结论

三个此前从未被真正执行过的 E2E 场景（S3 的 E2E-7 kill -9 诚实版，S4 的 20 轮双写拉锯，
S4 的离线 10 轮追赶）均已用真实 git 二进制、真实独立 Emacs 批处理进程、真实
`supertag-git-sync--*`/`supertag-merge`/`supertag-conflicts` 代码路径跑通，全部
断言通过，零数据丢失，零冲突标记入库，零 junk 文件误提交，收敛结果字节级一致。
过程中发现并修复了 5 处 harness（非产品）缺陷（初始 push 缺 `-u`、`%A` 相对路径
误当绝对路径、junk 文件时序、overlap 冲突字段选择、以及最重要的"轮询导致其被观察对象
本身变慢"这一沙箱环境特性，促使改为"检测到即立即 kill"而非"等待精确窗口"），均已在
上文如实记录而非掩盖；一处需要注意的验收措辞落差被明确记录：E2E-7 原文写"合并过程
kill -9 → git 与 DB 均无损"，实测中 git 自身在唯一改动路径的驱动崩溃后选择**整体
回滚合并**而非留下冲突态，这是比原始预期更强（而非更弱）的安全性质，已如实描述而非
简单打勾。

`./test/run-tests.sh all`：开始与结束均 244/244 全绿，无状态泄漏。

**总体裁决：PASS —— S3 E2E-7（诚实版）与 S4 的两条 20 轮/离线 10 轮验收场景全部通过，
零静默丢失，仓库干净。**
