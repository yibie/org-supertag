# issue009: 嵌套标签（标签/子标签）的自动解析与层级展示

## Summary

统一 org-supertag 的父子关系：真实 Tag ID 负责搜索和写入，`:extends` 是唯一层级来源；`a/b` 只允许作为父链的只读展示，不再作为持久化层级 ID。

## Origin

来自 org-supertag 节点 [[id:70577FDB-05F5-4C7E-A31F-118F2DDB0B08][org-supertag 的标签支持嵌套标签 #idea]]。

## Motivation

- 标签多了容易混乱，嵌套是天然的整理方式
- 当前 `supertag-view-schema` 已支持父子标签和字段继承，但关系需要手动建立
- 路径式写法 (`a/b`) 在许多系统中已是惯例（Bear、Notion 等）
- 如果实现，可大幅降低标签体系的维护成本

## Concerns

1. **标题长度**：Emacs 用户习惯将标签直接放在标题行，`#area/emacs/packages` 写出来会很长，影响可读性
2. **缺乏常驻导航**：org-supertag 没有侧边栏式的标签树导航（类似 Bear Note），即使实现了嵌套解析，用户也无法在编辑 Org 文件时看到标签层级，实用价值打折
3. **显示一致性**：如果需要 "常态化显示" 嵌套关系，落在哪个 View（table / kanban / node view / schema）最合理？

## Current State

- `:extends` 是 Schema、查询和字段继承共用的唯一父子关系。
- completion 可把 `happy :extends diary` 显示为 `diary/happy`，但实际写入仍是 `happy`。
- 新建、重命名和同步写库均拒绝产生未知斜杠 ID；旧斜杠数据只保留迁移能力。
- 自动化、真实 Store 副本和 Schema 视觉验收完成。
- 2026-08-01 修复 completion 的扁平混排：候选按 namespace 直接子级逐层
  展示，`#diary/` 不再泄漏 `ATTACH`、`Apple` 等根级候选。主要 Tag 写入
  入口已复用同一套 namespace reader，自动化验收完成。
- 2026-08-03 实机反馈证明逐层下钻仍不自然；改为输入真实 ID `happy`，候选
  显示父链 `diary/happy`，确认后仍只写入 `#happy`。
- Schema View 将 `happy :extends diary` 直接缩进到 `diary` 下，不再显示斜杠分支或派生虚拟父级。

## Scope (if implemented)

1. 真实 Tag ID 不含 `/`；父链只作为只读显示信息，不迁移、不双写。
2. completion 与共享 Tag reader 按真实 ID 搜索，以 `:extends` 父链作为 affixation。
3. Schema、View 与 Table 只读取显式 `:extends` 父子树，不派生虚拟 namespace。
4. 新增子标签统一写 `:extends`，不再提供独立的路径子标签入口。
5. View/Table 保留 `include-descendants` scope；后代聚合 Table 只读。
6. 遗留斜杠 ID 可原子迁移为真实 ID；新建与重命名不能产生新斜杠 ID。
7. 默认精确查询、`:extends` 继承和 Store schema 保持不变。

## Status

- 2026-07-29 用户确认实施完整路径方案。
- task012 仅完成查询基础；2026-07-29 用户指出这还不构成真正的嵌套标签支持。
- task013 已完成数据后端、Schema、View/Table 与初版 completion 自动验收。
- task015 [x] 修复 completion-table 过滤协议；候选按 namespace 直接子级逐层展示；
  行内输入、Add/Change、Capture 与 Tag Field 共用层级 Tag reader。验证方式：
  focused ERT 覆盖 `#diary/` 无关候选、逐层候选、namespace 不落库和共享 reader；
  全量 ERT、byte compilation、check-parens 与真实 Store 候选探测。
- task017 [x] 精确匹配已有平面 Tag 时提供 `/` 子 namespace，允许在没有
  既存后代的情况下继续创建下一层；basic 与 Corfu/orderless 枚举均已锁定。
- task018 [x] 以真实叶子 ID 直接搜索并显示 `:extends` 父链；Schema 合并父子
  关系与缩进，`/` 路径仅作旧数据兼容；task017 的逐层导航交互被替代。
- task019 [x] 修复 task018 affixation 对普通候选返回 `nil` suffix 导致的 Corfu
  `wrong-type-argument arrayp nil`；三列统一为字符串。
- task020 [x] 输入父级 `diary`/`diary/` 时渐进显示 `diary/happy` 等真实子标签；
  选中后在 sync/Store 之前归一化为叶子真实 ID。
- task023 [x] 将后代查询、Schema 和 View/Table scope 统一为传递 `:extends`；迁移真实 Store 中两个斜杠 ID，并阻止同步重新创建斜杠 ID。

## Environment

N/A

## Reproduction

1. Store 中存在 `diary` 与 `happy :extends diary`。
2. 在 Org buffer 输入 `#happy` 并触发 CAPF。
3. 修复前要先输入或选择 `diary/`，且 Schema 把 `happy -> diary` 与路径树分开显示。
4. 预期输入 `happy` 直接看到 `diary/happy`，选择后写入 `#happy`；Schema 中
   `happy` 直接缩进在 `diary` 下。

## Investigation

已进行技术预研，详情见 `tech-refer_nested_tags_20260624.md`。

关键发现：

1. **真实层级已存在**：`diary` 的子标签都由 `:extends` 表达，旧查询却只识别 `/` 前缀。
2. **遗留数据有限**：真实 Store 仅有 `Apple/Shortcut/语言`、`coding/日志` 两个斜杠 ID，叶子 ID 无冲突，可安全迁移。
3. **两套关系必然漂移**：Schema 与 completion 已展示 `:extends`，查询仍按 `/` 聚合，直接导致 Stream 漏节点。
4. **无需新索引**：现有 tag query 本来就是 O(N) scan；父链遍历可在同一次查询中完成。

## Root Cause

上一版虽已让 completion 和 Schema 优先展示 `:extends`，查询仍把 `/` 前缀当作后代，
Schema 仍为斜杠 ID 派生虚拟父级。因此 `happy :extends diary` 在界面中是子标签，
Stream 的 `include-descendants` 却看不到它，层级事实来源仍然分裂。

## Fix

采用真实 ID + 父链展示：

1. 从 `:extends` 计算只读 display path；cycle 时退回真实 ID。
2. CAPF 与共享 Tag reader 的候选值保持真实 ID，以 Emacs affixation 显示父链。
3. Schema、View 与 Table 只按 `:extends` 连接实际父子，不再派生 `/` namespace。
4. 删除独立路径子标签入口；`a n` 与兼容键 `a c` 调用同一个 Child Tag 命令。
5. 将真实 Store 的 `Apple/Shortcut/语言`、`coding/日志` 迁移为叶子 ID + `:extends` 链，并同步改写源 Org token；迁移前创建唯一恢复快照。

详细方案见 `.phrase/docs/tech-refer_nested_tags_20260624.md`。

## Verification

task012 自动化验证：
- `#emacs/package` 提取后仍是 `"emacs/package"`。
- 精确查询 `emacs` 不隐式命中 `emacs/package`。
- 显式后代查询 `emacs` 命中 `emacs/package` 和更深层路径，不命中 `emacs2/package`。
- focused ERT 4/4、全量 ERT 319/319 通过。
- 10k-node / 100 次查询基准：精确查询 0.265s，总计约 2.65ms/次；后代查询 2.486s，
  总计约 24.86ms/次，无需新增索引。

task013 验收：
- focused ERT 15/15：覆盖路径边界、单节点同步、原生 tag 策略、Schema namespace/
  inheritance 分离、completion 无写导航、View/Table scope、分支迁移/冲突回滚和精确删除。
- 全量稳定 ERT 330/330；completion 独立 self-check 与 Table ERT 通过。
- 12 个改动文件通过 `check-parens`、byte compilation 和 `git diff --check`。
- 当前真实 vault 的只读副本包含 101 tags/1554 nodes；`coding` exact=0、
  descendants=1，Schema 派生 `coding/` → `日志`；原文件与副本 SHA-1 前后不变。
- Schema View 截图视觉判定 92/100（pass）：namespace 缩进与 `:extends` 箭头/
  inherited fields 可清楚区分。

task015 验收：
- focused ERT 19/19，覆盖逐层 direct-child 候选、`#diary/` 过滤、namespace
  不落库、共享 reader 导航与虚拟 namespace 选择。
- View/Query 相关 ERT 54/54；全量 ERT 335/335；completion 与 inline-tag
  独立 self-check 通过。
- 12 个相关代码/测试文件通过非写入 byte compilation；所有改动文件通过
  `check-parens` 与 `git diff --check`。
- 当前真实 Store 中 `diary` 没有子路径；探测 `diary/` 返回空候选而非根级标签。
- 独立代码审查无阻塞问题，确认未新增依赖、缓存、Store 字段或 namespace entity。

task017 验收：
- 真实 CAPF fixture 使用 `("diary" "diaryx")` 与 buffer `#diary`，basic 和
  Corfu/orderless `action=t` 都返回带 namespace property 的 `diary/`。
- focused ERT 20/20、全量 ERT 352/352；byte compilation、`check-parens`、
  `git diff --check` 通过；编译生成的 `.elc` 已删除。

task018 验收：
- focused ERT 19/19，覆盖 `happy` 搜索、`diary/` affixation、真实 ID 保留、
  shared reader、统一 Schema 树、循环保护和单一 Child 命令。
- 全量 ERT 351/351；byte compilation、`check-parens`、`git diff --check` 通过；
  仓库无 `.elc`。
- 真实 Store 只读探测得到 `happy :extends diary`、display path `diary/happy`、
  CAPF 候选值 `happy` + 前缀 `diary/`，Schema 将 `happy` 放在 `diary` 子树。
- `diary/happy` 经源码、node 和保守 orphan scanner 确认为无引用后删除；DB 已备份，
  notes commit `b7bfdfe` 已推送。

task019 验收：
- 回归先以 `(cl-every #'stringp row)` 复现失败，再由共享 affixation producer 修复。
- 真实运行中的 Corfu 对新候选 `("ta" "" "  [New]")` 与普通候选
  `("task" "prj/" "")` 均完成格式化，无异常。
- focused ERT 19/19、全量 ERT 351/351、byte compile 与静态检查通过；仓库无 `.elc`。

task020 验收：
- focused ERT 21/21，覆盖父级枚举、alias property、buffer `#happy` 归一化、
  Store 写入 `happy` 以及真实 `diary/happy` ID 不被 alias 遮蔽。
- 真实 Corfu 输入 `diary` 枚举 `diary/exp`、`diary/happy`、`diary/record` 等，
  `diary/happy` 内部 ID 为 `happy`，formatter 无异常。
- 全量 ERT 353/353、byte compile 与静态检查通过；仓库无 `.elc`。

task021 验收：
- `#dia` 的 try-completion 返回 `diary`，候选中已有 Tag 及其子标签全部位于
  `dia [New]` 之前；真实 Corfu formatter 无异常。
- nil exit、取消和未知前缀后的分隔符均不调用新建；只有带 `is-new-tag` 的显式
  `[New]` 候选传入 `:create-if-needed t`。
- focused ERT 22/22、completion self-check、全量 ERT 354/354、byte compile、
  `check-parens`、`git diff --check` 通过；仓库无 `.elc`。

task022 验收：
- 捕获真实 Corfu state：旧顺序为 `diar [New]`、`diary`、children，证明 batch
  枚举未覆盖 Corfu 最后的 exact-candidate 置顶。
- 新候选使用不可见的 non-exact marker，display sorter 输出 `diary`、`dia [New]`、
  children；affixation 与 exit 分别在显示和写入前移除 marker。
- focused ERT 22/22、completion self-check、真实 `corfu--compute`/formatter 通过；
  full ERT、byte compile、`check-parens`、`git diff --check` 通过。

task023 验收：
- focused ERT 84/84、全量 ERT 399/399；覆盖 direct/deep `:extends`、平面斜杠反例、
  新建/重命名拒绝斜杠 ID、单点同步与批量导入的 display-path 归一化。
- 16 个改动 Elisp/test 文件通过 `check-parens`；14 个产品 Elisp 文件非写入 byte compile 通过，
  仅保留仓库既有 Emacs 31 过时宏/docstring 警告；`git diff --check` 通过。
- 真实 Store 迁移 `Apple/Shortcut/语言` → `语言 :extends Shortcut :extends Apple`、
  `coding/日志` → `日志 :extends coding`；源 Org token 同步改写，斜杠 Tag ID 从 2 降为 0。
- 恢复快照：`/Users/chenyibin/Documents/notes/.supertag/backups/supertag-db-prerestore-20260807-111830-GxuUKe.el`。
- 真实 `diary` 查询 exact=2、include-descendants=256，新增 254 个子标签节点；识别 13 个传递子标签。

## User Confirmation

- 2026-07-29：确认采用“完整路径即 Tag ID、读取时推导层级、`:extends` 保持独立”的方案。
- 2026-08-01：实机确认初版 completion 的显示和输入仍是扁平的，task013
  前端验收不通过；进入 task015 修复。
- 2026-08-03：实机确认 `#diary` 仍无法显示可下钻的 nested namespace；进入 task017 修复。
- 2026-08-03：实机确认逐层 namespace 仍不符合预期，要求直接输入 `happy` 显示
  `diary/happy`，并把 Schema 中的 parent-child 合并为一棵树；进入 task018。
- 2026-08-03：实机捕获 task018 普通候选 suffix 为 `nil` 导致 Corfu 崩溃；进入 task019。
- 2026-08-03：实机指出输入父级 `diary` 无法渐进发现子标签；进入 task020。
- 2026-08-03：实机发现 `#dia` 被当作默认新 Tag 写入，并明确要求新 Tag 只能通过
  completion 的 `[New]` 注册；进入 task021。
- 2026-08-04：实机确认 Corfu 最终仍把 `[New]` 放第一行，要求真实补全项第一、
  `[New]` 第二；进入 task022。
- 2026-08-07：明确 `:extends` 是唯一层级规则；取消完整路径 ID 的兼容后代语义，并授权清理真实数据库中的斜杠层级标签；进入 task023。
- 2026-08-07：task023 代码、数据迁移与自动化验收完成；issue 保持打开，等待用户实机确认 Stream/Schema。
- 端到端实现结果验收：Pending
