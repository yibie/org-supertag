# issue009: 嵌套标签（标签/子标签）的自动解析与层级展示

## Summary

统一 org-supertag 已有的父子关系：真实 Tag ID 负责搜索和写入，`:extends` 父链负责补全展示与 Schema 缩进；旧 `a/b` 完整路径 ID 继续兼容。

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

- 提取、同步、写回和 completion 将 `a/b/c` 作为一个完整 Tag ID 处理。
- `:extends` 同时是 Schema 中唯一的显式父子关系与字段继承关系。
- 单节点/全文件同步、显式后代查询、Schema namespace 树、路径补全、
  View/Table 聚合入口和事务化分支重命名均已实现。
- 自动化、真实 Store 副本和 Schema 视觉验收完成。
- 2026-08-01 修复 completion 的扁平混排：候选按 namespace 直接子级逐层
  展示，`#diary/` 不再泄漏 `ATTACH`、`Apple` 等根级候选。主要 Tag 写入
  入口已复用同一套 namespace reader，自动化验收完成。
- 2026-08-03 实机反馈证明逐层下钻仍不自然；改为输入真实 ID `happy`，候选
  显示父链 `diary/happy`，确认后仍只写入 `#happy`。
- Schema View 将 `happy :extends diary` 直接放入 `diary` 分支，不再用箭头把
  父子关系拆成第二种表现；旧完整路径仅在没有显式父级时作兼容回退。

## Scope (if implemented)

1. 真实 Tag ID 保持不变，不迁移、不双写；父链只是显示信息。
2. completion 与共享 Tag reader 按真实 ID 搜索，以 `:extends` 父链作为 affixation。
3. Schema View 优先使用显式 `:extends` 父子树；旧完整路径 ID 才派生虚拟 namespace。
4. 新增子标签统一写 `:extends`，不再提供独立的路径子标签入口。
5. View/Table 保留 `include-descendants` scope；后代聚合 Table 只读。
6. 分支重命名原子迁移 exact + descendants；精确删除使用完整 token 边界。
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

1. **完整路径已经可用**：现有 Store 和 inline tag 边界允许 `/`，真实 vault 中已有 `Apple/Shortcut/语言` 等标签。
2. **叶子存储不可用**：`emacs/package` 与 `linux/package` 会冲突为同一个 `package`。
3. **已有父子来源就是 `:extends`**：把 `/` namespace 与它并排展示会制造两套层级心智模型。
4. **性能风险很低**：现有 tag query 本来就是 O(N) scan；显式后代查询只在该 scan 中增加段边界前缀判断。

## Root Cause

上一版把“嵌套”建模为逐层进入 `/` namespace，但真实数据已经用
`happy :extends diary` 表达父子关系。completion 只匹配候选 ID，不展示父链；Schema
又把 `:extends` 画成箭头、把 `/` 画成缩进，因此输入和浏览都暴露了两套关系。

## Fix

采用真实 ID + 父链展示：

1. 从 `:extends` 计算只读 display path；cycle 时退回真实 ID。
2. CAPF 与共享 Tag reader 的候选值保持真实 ID，以 Emacs affixation 显示父链。
3. Schema 用 `:extends` 连接实际父子；只有无显式父级的旧路径 ID 才按 `/` 连接。
4. 删除独立路径子标签入口；`a n` 与兼容键 `a c` 调用同一个 Child Tag 命令。
5. 完整路径查询、后代聚合、分支重命名和 Store schema 保持兼容，不做数据迁移。

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

## User Confirmation

- 2026-07-29：确认采用“完整路径即 Tag ID、读取时推导层级、`:extends` 保持独立”的方案。
- 2026-08-01：实机确认初版 completion 的显示和输入仍是扁平的，task013
  前端验收不通过；进入 task015 修复。
- 2026-08-03：实机确认 `#diary` 仍无法显示可下钻的 nested namespace；进入 task017 修复。
- 2026-08-03：实机确认逐层 namespace 仍不符合预期，要求直接输入 `happy` 显示
  `diary/happy`，并把 Schema 中的 parent-child 合并为一棵树；进入 task018。
- 2026-08-03：实机捕获 task018 普通候选 suffix 为 `nil` 导致 Corfu 崩溃；进入 task019。
- 端到端实现结果验收：Pending
