# org-supertag Git 原生同步 — 实施计划

日期：2026-07-13
状态：已完成（2026-07-13）— S0–S4、P0/P1 复审加固与 release gate 均通过
前置版本：v5.9.0（原子保存 / 多实例锁 / 自动迁移 / doctor 已具备）

---

## 0. 背景与决策记录

### 问题

数据库是单个 `prin1` 序列化的 blob 文件（~1MB，实为单个大哈希表）。用户通过
Dropbox/iCloud 同步它时，同步盘的语义是"整文件、最后写者赢、无告警"——两台机器
并用必然静默丢数据。而不同步数据库又会丢失 DB-only 的用户事实（字段值、schema、
关系、boards、自动化规则）。多位用户已提出此需求（discussion #161 等）。

### 已否决的两个方案（ADR）

**ADR-1 否决"文本作为事实来源"（字段回写 property drawer + vault manifest）**
- 写放大：每次字段编辑改写 org 文件；定时自动化更新 200 节点 = 200 次文件 IO +
  200 次同步上传 + git diff 噪音。
- 单行 JSON property 在行粒度上不可合并，"文本冲突可手工解决"的承诺在字段数据上
  不成立。
- 丢掉数据库的核心优势（内存批量计算、类型系统、事务性批更新）。

**ADR-2 否决"自制语义事务日志复制"（tx 文件 + parent 链 + checkpoint）**
- 本质是在无序、最终一致的劣质传输层（文件同步盘）上重新实现 git：fork 检测、
  parent 链、幂等重放、GC 视界，全是 git 已解决二十年的问题。
- 深分叉（笔记本离线一周末）是常态而非异常，方案给出的"选一支"是数据丢失按钮。
- 扫描器（org 文件直接编辑）与事务日志构成双真值通道，收敛问题无解答。
- 工程量季度级，与 MELPA/MCP 路线冲突。

**ADR-3 采纳"git 原生同步"，理由：**
1. **push 拒绝 = 传输层事务语义**——静默覆盖在物理上不可能发生，这是文件同步盘
   永远给不了的。
2. **merge base（共同祖先）使语义合并从研究课题降为查表**：单侧改动直接采纳，
   双侧同改才是真冲突。CRDT/oplog 的主要存在理由是"没有 base"，git 有。
3. `.gitignore` 让本机状态（sync-state/锁/备份）就地留在 vault 目录且不参与同
   步——消除了 Dropbox 时代拆路径配方的全部复杂度。
4. 受众匹配：Emacs 用户 git 素养接近 100%，magit 是日常工具；Obsidian 社区最流
   行插件之一即 obsidian-git，需求与可行性有先例。
5. 免费获得完整历史（时间机/审计），优于 backups/ 目录。

### 措辞约定

对外文档一律说"**git 远端**"（自建 Gitea / NAS 裸仓库 / Codeberg / GitHub 均为
一等公民），不说"GitHub 同步"。

### 不放弃的兜底

不用 git 的用户（iCloud/Dropbox 直同步党）永远保留受支持的降级路径：单写者纪律
文档 + S0 的 presence 提示。iCloud 定位为 best-effort。

---

## 阶段总览

| 阶段 | 内容 | 规模 | 独立价值（不做同步也值得） |
|---|---|---|---|
| S0 | 止血：presence 提示 + 单写者文档 | ~2 天 | 现有同步盘用户从"静默丢"变"有告警" |
| S1 | 事务地基：修假回滚、统一 commit seam | ~1 周 | 修复已验证的正确性 bug |
| S2 | 规范化序列化：确定性、逐行、可 diff | 3–5 天 | DB 可 diff 可审计，消除字节不确定性 |
| S3 | 语义 merge driver（核心投入） | 2–3 周 | — |
| S4 | 自动同步循环 supertag-git-sync-mode | 1–2 周 | — |
| S5 | 锦上添花：历史时间机、checkpoint tag | 择机 | — |

S0–S2 与 MELPA 上架完全不冲突，可并行。S3 开工前需本计划批准的验收清单冻结。

---

## S0 止血（先于一切）

1. **presence 文件**：`<data-dir>/presence.json`（普通文件，内容
   `{host, updatedAt}`，可被任何同步盘传输）。加载时发现异主且 5 分钟内活跃 →
   显著告警；不阻塞功能（advisory，不是锁——同步盘的分钟级延迟决定了它只能是提
   示）。写入挂在现有 auto-save timer 上，成本一行。
2. **README 增补"多机同步"章节**：单写者纪律、换机前退出 Emacs（300 秒
   auto-save 定时器是隐形写者）、5.9.0 锁只保护同机双开。
3. doctor 增加 presence 状态一节。

验收：两台机器（或两个 HOME 模拟）交替启动，后启方看到前者的 presence 告警；
`run-tests.sh all` 全绿。

---

## S1 事务地基修复

已验证的事实：`supertag-with-transaction`（supertag-core-transform.el:61）
docstring 承诺回滚，实际 unwind-protect 只清 `supertag--transaction-log`，无任何
恢复代码——**假事务**。而 `supertag--with-transaction`
（supertag-core-persistence.el:1212）是真事务但用整库快照，重到不可日常使用。

1. 实现真回滚：每个被修改 path 首次写入时记录旧值（copy-tree 该 entity 即可，
   不快照整库），失败按逆序恢复。
2. 合并两套宏为一个入口（保留旧名为 obsolete alias），全库 grep 迁移调用点。
3. boards / automations / global-field 的修改路径统一走 `supertag-ops-commit`
   seam（现状散落直写 store 的点先盘点再收口）。
4. 事务内 automation 产生的连锁修改并入同一事务（为 S3 的实体级合并提供干净的
   变更边界，也为将来 MCP 写入提供唯一入口）。

验收（ERT，进 run-tests.sh 新 `tx)` 组）：
- 事务体中途 error → store 与事务前逐 entity `equal`。
- 嵌套修改同一 entity 多次 → 回滚到最初值。
- 事务成功 → 只发一次批量通知（现有行为不回退）。

---

## S2 规范化序列化

改造 `supertag--persistence-write-store-atomically` 的序列化器（保持原子写入 +
校验框架不动）：

1. **确定性**：collection 按固定顺序输出；每个 collection 内按 entity-id 字典序；
   plist 键按字典序重排后打印。相同逻辑内容 ⇒ 任何机器上字节相同。
2. **逐行**：文件首行为格式头注释（含 format 版本），此后每 entity 一行。单字段
   修改的 git diff 恰好命中一行。
3. **值规范化**：时间值统一打印格式；浮点用 `%S` 稳定表示；字符串转义规则固定。
   （此规范同时是 S3 三方比较的相等性定义，必须一次定清。）
4. **print-circle 决策**：逐 entity 打印使跨行共享结构引用（`#N=`）失效。需先验
   证 store 经 on-load coercion 后 entity 间无共享结构（预期如此），然后每行独立
   `prin1`（行内保留 print-circle 防御自引用）。若发现跨 entity 共享，先在
   coercion 层拆解——这本身是数据卫生改进。
5. **读取端零迁移**：sexp reader 对空白/换行不敏感，旧版本可读新格式、新版本可读
   旧格式。不 bump 数据版本，不触发迁移。

   > **修订 2026-07-13（P1-8 review 发现，此条为错误结论，不删除原文，仅订正）**：
   > 上面"旧版本可读新格式"一句是**假的**。旧版本（<= 5.9.x）的
   > `supertag--persistence--try-read-store` 对文件只做**一次** `read`
   > （见 git 历史 commit `c1224f5`，S2 引入前的最后一个提交），拿到的就是
   > `(read (current-buffer))` 返回的第一个、也是唯一一个顶层 sexp。canonical
   > 格式恰恰是"每 entity 一行"——也就是**每行一个独立顶层 sexp**——旧版本那一次
   > `read` 只会消费掉文件的第一行（根标量行，如
   > `(:version "6.0.0" ...)`），后面所有 `(:collection ...)` entity 行永远不会
   > 被读到。旧版本并不会报错：`supertag--coerce-store-table`
   > （同一函数自 S0 前到现在字节级未变）把这个 plist 强制转成一个哈希表，
   > 里面只有 `:version` 等根标量键，没有 `:nodes`/`:tags`/... —— 用户看到的是
   > 一个"成功加载"但没有任何实体的空库，数据在会话内**静默消失**。
   >
   > 采纳 review 给出的两个方案中的**方案一：诚实的破坏性格式升级**（不采用方案
   > 二重新设计格式——逐行属性正是让 git merge 可行的关键，不能动）。具体做法见
   > `supertag-core-persistence.el`：
   > 1. `supertag-data-version` 从 `"5.0.0"` bump 到 `"6.0.0"`，让
   >    `supertag--maybe-auto-migrate` 至少对"版本号确实过期"的旧库触发一次
   >    （附带它自带的 premigrate 快照）。
   > 2. 新增一次性 `supertag-db-preformat6-<timestamp>.el` 快照
   >    （`supertag--persistence--snapshot-preformat6`）：只要即将被覆盖的
   >    on-disk 文件仍是 legacy 格式，就在原子 rename 之前存一份——这条覆盖了
   >    "版本号已经是 6.0.0 但文件格式其实还没被重新保存过"的缺口，纯版本号判断
   >    覆盖不到。
   > 3. 每次 canonical 保存的根标量行强制带上
   >    `:supertag-format 1` / `:incompatible-notice "..."` 哨兵键
   >    （`supertag--persistence-write-canonical-store`）：不能改老版本的代码，
   >    但至少让任何"碰巧把原始 first-form 展示出来"的路径（如老版本的
   >    `supertag-db-inspect-file`）有机会显示一句可操作的提示，而不是一个无声的
   >    空 plist。
   >
   > 详见 `supertag-core-persistence.el` 中 `supertag-data-version`、
   > `supertag--persistence--write-canonical-store`、
   > `supertag--persistence--snapshot-preformat6` 的 docstring，以及
   > `test/canonical-serialization-test.el` 的 "P1-8" 测试小节（含对旧 reader
   > 行为的本地复现测试）。

验收：
- 同一 store 连续两次序列化字节相同；save→load→save 字节相同。
- 修改一个字段后 git diff 恰为一行替换。
- 10k 节点基准（test/perf-benchmark.el）：保存耗时 ≤ 现基线 2 倍（现 ~64ms）。
- 用真实用户库（先备份）做一次 load→save→load 冒烟，语义快照一致。

---

## S3 语义 merge driver（核心正确性资产，测试火力集中于此）

### 形态

- 新文件 `supertag-merge.el`：纯函数核心
  `(supertag-merge-3way base ours theirs)` → `(merged . conflicts)`，不依赖运行时
  store，可独立 batch 调用。
- 入口脚本：`emacs -Q --batch -l supertag-merge.el -f supertag-merge-driver-main
  %O %A %B`（git merge driver 协议：结果写回 %A，exit 0/1）。
- `supertag-git-setup` 命令：写 `.gitattributes`（`supertag-db.el merge=supertag-db`）
  并写 **每个 clone 的** `.git/config` driver 条目（merge driver 配置不随仓库传
  播——这是最易遗忘的坑，doctor 必须加检查项）。

### 合并算法（实体级三方）

对三份文件解析出 entity map 后，按 (collection, entity-id) 逐个裁决：

| base | ours | theirs | 结果 |
|---|---|---|---|
| A | A | A | A |
| A | B | A | B（单侧改） |
| A | A | C | C（单侧改） |
| A | B | B | B（双侧同改同值） |
| A | B | C | **进入字段级三方**（对 plist 逐键做同样裁决；同键双改不同值 = 真冲突） |
| A | 删 | A | 删 |
| A | 删 | C | **删改冲突**：保留 C + 记录冲突（复活策略，宁可多不可丢；tombstone 留作后续演进） |
| 无 | B | C（同 id 双新增不同值） | 字段级合并，冲突同上 |

### 真冲突的呈现

**不在 .el 数据文件里留 git 冲突标记**（loader 会当场解析失败）。真冲突写入
store 的新 collection `:sync-conflicts`（entity: 双方值 + 时间戳 + 来源），文件
本身永远保持可加载；merge driver exit 0。用户侧：加载时 message 告警 + doctor
新增 "Sync Conflicts" 一节逐条展示、单键选边或手工编辑。字段值默认采用
:modified-at 较新一侧填入正文位置（可用性优先），但另一侧完整保留在冲突记录中
——**无静默丢失**。

### 降级行为（重要设计红利）

S2 的逐行格式意味着**即使 driver 未配置**，git 默认文本合并对"不同实体各自修改"
也能行级正确合并；只有同一 entity 双改才产生文本冲突标记 → loader 检测到标记时
拒绝加载该文件并指引运行 `supertag-git-setup` + `git checkout --merge` 重合。
doctor 检查 driver 配置状态。

### 验收（本阶段的测试即产品）

单元（纯函数，覆盖上表每一行 + 各字段类型的相等性判定：string/number/boolean/
date/timestamp/options/node-reference/list）；集成（真 git 仓库三方演练）：

- E2E-1 两 clone 改不同节点 → merge 后字节级收敛一致。
- E2E-2 同节点不同字段 → 自动合并，无冲突记录。
- E2E-3 同节点同字段双改 → 合并成功加载成功，:sync-conflicts 恰一条，双值俱在，
  doctor 可见。
- E2E-4 删改冲突 → 复活 + 冲突记录。
- E2E-5 driver 未配置的 clone：不同实体修改 → 默认合并仍收敛；同实体修改 →
  loader 拒载并给出指引，原库不损。
- E2E-6 第三台机器 fresh clone → 加载 → 语义快照与源机一致。
- E2E-7 合并过程 kill -9 → git 与 DB 均无损（git 原子性 + S2 原子写）。
- 性能：10k 节点三方合并 < 5 秒（batch Emacs 启动含在内）。

---

## S4 用户旅程：一个 URL 完成配置

目标体验：机器 1 `M-x supertag-git-sync-setup` + 粘贴远端 URL；机器 2
`M-x supertag-git-sync-clone` + 同一 URL。用户仍需自备：空的 git 远端（自建/
GitHub/NAS 裸仓库均可）、已就绪的 git 认证（setup 只检测并指引，不实现认证）、
git 本体。

### 仓库布局（本阶段的前置设计决定）

merge driver 要求 DB 文件**在仓库内**，而当前 DB 住在 `~/.emacs.d/org-supertag/`
（仓库外）。git vault 采用如下布局，setup 负责一次性搬迁（复用 5.9.0 的
per-vault data-directory 机制，搬迁前自动备份）：

```
<vault-root>/                  ← git 仓库根
├── **/*.org                   ← 同步
├── .supertag/supertag-db.el   ← 同步（走 merge driver）
├── .supertag/sync-state.el    ← .gitignore
├── .supertag/backups/         ← .gitignore
├── .gitignore  .gitattributes ← setup 生成
```

**V1 限制：git 同步只支持单根 vault。** `org-supertag-sync-directories` 配了多个
根目录的用户需先归拢或暂不启用（文档明说）；多仓库编排不进第一版。

### `supertag-git-sync-setup`（机器 1）

已在 git 仓库中则跳过 init 只做增量配置（org 用户常已用 git 管笔记）；否则
`git init`。随后：生成 .gitignore/.gitattributes → 配置本 clone 的 merge driver
→ DB 搬迁入 `.supertag/` 并更新 vault 注册 → 首次 commit → remote add + push
（认证失败时给出诊断指引而不重试）。

### `supertag-git-sync-clone`（机器 N）

clone → 配置本 clone 的 driver → 注册 vault → 加载 DB（或损坏时重建）。

不开 `supertag-git-sync-mode` 的用户用 magit 手动推拉同样成立——同步正确性在
merge driver，自动化循环只是便利层。

## S4 自动同步循环 `supertag-git-sync-mode`

默认关闭，opt-in。职责：

1. **提交**：after-save（vault 内文件）debounce 30s 自动 commit（信息含 host）。
   DB 文件在 supertag-save-store 成功后一并纳入。
2. **拉取**：定时（默认 5 分钟）+ Emacs focus-in 时 `fetch`；落后则 `merge`
   （不 rebase——merge driver 语义更直接，历史美观不是目标）。
3. **推送**：commit 后 push；被拒 → fetch/merge/重试一轮；再失败 → 静默转为下轮，
   modeline 显示待推计数。
4. **org 文本冲突**（正文双改，git 标准冲突标记）：sync scanner 检测到标记的文件
   跳过导入 + doctor "Text Conflicts" 一节列出，等用户 magit/手工解决。
5. **凭据与网络**：完全依赖用户既有 git 配置（ssh/credential helper）；离线时全
   部操作静默降级为本地 commit，联网后追赶。
6. 文档：Working Copy（iOS）配合指南；`git gc` 建议；"git 远端"措辞。

验收：双 clone 双 Emacs 并开，两侧交替编辑字段与正文各 20 轮（脚本模拟），终态
收敛一致、冲突全部可见于 doctor、零静默丢失；断网 10 轮后联网追赶收敛。

---

## S5 择机（不进本期承诺）

- `supertag-time-machine`：基于 git 历史的库状态回看/单 entity 回滚。
- 发版时自动打 checkpoint tag。
- presence 信息改由 git log 推导（S0 文件届时可退役）。

---

## 风险清单

| 风险 | 缓解 |
|---|---|
| entity 间共享结构使逐行打印失真 | S2 第 4 条先验证/拆解；加断言测试 |
| merge driver 忘配（每 clone 手动） | supertag-git-setup + doctor 检查 + 降级行为兜底 |
| 字段值相等性语义（浮点/时间列表） | S2 值规范化即三方比较的相等性定义，先冻结 |
| 合并后加载触发 automation 重放 | 验证 load 路径不触发 :store-changed 自动化；补测试 |
| 仓库长年膨胀 | 文档 gc 指引；S5 checkpoint tag 后可浅克隆 |
| 不用 git 的用户被落下 | S0 路径永久保留为受支持降级 |
| DB 搬迁入仓库出错（setup 中断/路径冲突） | 搬迁前强制备份；搬迁 = 复制→验证可加载→更新注册→删旧，任一步失败回退 |
| 多目录 vault 用户误开 git 同步 | setup 检测多根配置直接拒绝并给出归拢指引 |

## 完成标准（一句话）

> 两台机器同时开着 Emacs 各自编辑，任何一侧的修改要么自动收敛、要么成为 doctor
> 里可见可裁决的冲突条目——不存在第三种（静默丢失）结局。

## P0 复审门槛（2026-07-13）

S0–S4 初版完成后的反例审查发现，完成标准还需同时覆盖以下边界：

1. `:tag-field-associations` 的 legacy/异常 shape 不得进入专用合并器后被跳过；
   无法证明 shape 安全时整体三方裁决。
2. 有序 association 的并发插入必须保留两侧顺序约束；冲突 ID 必须区分字段键与
   合并器元数据键。
3. 所有 Git unmerged Org 路径都必须进入 doctor 与 scanner 黑名单，不以文本
   conflict marker 为前提。
4. 自动提交只拥有自己的 allowlist；不得提交或 reset 用户预先 staged 的其他路径。
5. DB 布局迁移不得绕过既有 persistence origin/guard。

对应记录：`issue013` / `task001`。P0 修复落在 `20cb4b5`、`849206a`；最终
`./test/run-tests.sh all` 为 245/245，三项真实 Git release-gate 场景见
`ACCEPTANCE.md`，结论均为 PASS。
