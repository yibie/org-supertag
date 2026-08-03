# plan_smart_key_20260721

## Milestones

1. 用 focused ERT 锁定 context 优先级、已有交互原语、inline tag 过滤、heading 与 Assist 行为。
2. 新增一个小 Module，以 `supertag-smart-key` 为主入口，内部完成 target 识别和默认动作分派。
3. 接入主包与稳定测试入口，运行 focused/full batch tests 和静态检查。
4. 回写 task、change 与全局 CHANGE 索引。
5. 将前缀调用从全局菜单占位升级为对象级 Assist；保留无 target 时的全局菜单回落。
6. 保持 Node View 身份解析只读；无 ID heading 不进入任何隐式创建路径。
7. 修复显式 Node 退化：Store 删除成功后移除 Org ID，并保留其他 Org 属性。
8. 收紧共享 inline tag validator：Org link target 中的 `#fragment` 不参与 face/SVG 渲染或 point 识别。
9. 用“空白分隔的正文 token”统一渲染边界，覆盖 inline object、drawer、COMMENT subtree、block 与嵌入式 hash。
10. 将同步提取对齐同一边界，并限制到当前 headline 的标题和自身正文 section。
11. 保留最低/稳定 Emacs 矩阵，但将 CI 限制到 main、PR 的非文档变更及手动触发。
12. 排除 Emacs Lisp `#'function` 引用，过滤 completion 中的同类历史污染，并缩小 SVG tag 默认字号。
13. 保留完整路径 Tag ID，在查询 seam 增加显式后代匹配，不把 namespace 映射为 `:extends`。
14. 将路径 namespace 接入同步、Schema、completion、View 与 Table；分支重命名使用单次映射迁移，聚合表保持只读。
15. 延迟加载视图模块时补启用已经存在的 Org buffer，避免启动恢复的笔记失去 inline tag SVG。
16. 从原始 buffer 区间识别 inline Tag，避免 Org subscript 改写下划线；旧污染只通过显式、保守的孤立 Tag 清理命令处理。
17. 已有平面 Tag 精确匹配时提供只导航的子 namespace 候选，使用户无需先存在子 Tag 即可继续输入下一层。
18. 以真实 Tag ID 做叶子搜索，以 `:extends` 父链做补全展示和 Schema 缩进；`/` 路径树仅保留为旧数据兼容回退。

## Scope

- 代码：`supertag-smart-key.el`、`supertag-view-node.el`、`supertag-view-helper.el`、
  `supertag-ui-commands.el`、`supertag-ui-completion.el`、`supertag-core-transform.el`、
  `supertag-services-sync.el`、`supertag-view-svg-tag.el`、`supertag-core-scan.el`,
  `supertag-core-tag-path.el`、`supertag-view-api.el`、`supertag-view-framework.el`、
  `supertag-view-schema.el`、`supertag-view-table.el`、`supertag-ops-tag.el`、
  `supertag-ops-tag-merge.el`、`org-supertag.el`。
- 测试：`test/test-smart-key.el`、`test/test-inline-tag-filter.el`、`test/extractor-test.el`、`test/tag-merge-test.el`、`test/tag-path-test.el`、`test/run-tests.sh`。
- CI：`.github/workflows/test.yml`。
- 文档：本 phase 文档与 `.phrase/docs/CHANGE.md`。

## Priorities

- P0: recognizer 无持久化副作用，不覆盖既有按键。
- P0: 具体语义属性优先，Org link 不被 inline tag 遮蔽。
- P0: Org link target 的 `#fragment` 不获得 inline tag 的 face 或 SVG `display` 属性。
- P0: 只渲染行首/空白后的正文 `#token`；tag 名仍允许中文、emoji、`/` 与非空白标点。
- P0: 同步不得从 Org object、元数据、COMMENT subtree 或子 headline 提取 inline tag。
- P0: `#'function` 不得渲染、同步或出现在 tag completion；历史 Store 不做破坏性删除。
- P0: 视图模块晚于 Org buffer 加载时，现存与后续 Org buffer 都必须自动启用 inline tag 样式。
- P0: `_` 在 Tag token 中必须保持字面值；Org object 边界仍不得泄漏内部 `#token`。
- P0: cleanup 引用模型覆盖 Tag schema fields；整批 `after-operation-hook` 后按显式候选 ID 复检，事务回滚执行全部 invariant handler 后重抛首错。
- P1: SVG tag 默认字体小于正文行高，缓存键包含字号比例。
- P1: 嵌套 Tag 查询只在调用方显式请求时包含路径后代；精确查询保持兼容。
- P1: Schema View 以 `:extends` 为主父子树，旧完整路径 ID 在没有显式父级时才派生虚拟 namespace。
- P1: 所有 Tag 输入按真实 ID 搜索，父链只参与显示，不得改写插入值或 Store identity。
- P1: 从 namespace/branch 打开的 View 与 Table 保留 `include-descendants` scope；聚合 Table 不读写父 Tag 的自定义字段。
- P1: 单节点同步与全文件同步建立相同的 Tag entity/node-tag relation，并回收当前节点已失效的关系。
- P1: 分支 Tag 重命名同时迁移所有路径后代并预检冲突；精确删除不得破坏后代 token。
- P1: 复用既有命令，不复制 Ops 或 View 实现。
- P2: 插件注册、上下文 Assist 与 Hyperbole Adapter 留到真实调用方出现后再做。
- P2: Hyperbole Adapter 与第三方动作注册继续后置；对象级 Assist 只复用已有命令。

## Risks & Dependencies

- 现有 `supertag-context` 同名异形，归一化错误会导致 Node/Schema View 动作错配。
- 现有 inline tag point helper 未应用 font-lock validator；需在共享 helper 处修正，避免 Smart Key 复制第二套规则。
- 原生 `:tag:` 在非 ignore 策略下由增量/全量同步一致读取；当前节点关系可回收，
  但独立 Tag entity/字段 schema 不自动删除，`lazy-convert` 文件改写仍由 issue022 跟踪。
- 过滤 `#'function` 会隐藏此前误建的同名 tag；若用户确实需要以 `'` 开头的标签，需改用不冲突的名称。
- 路径后代查询沿用现有 O(N) node scan；10k-node 基准不满足交互延迟时再考虑前缀索引。
- 现有 Table 的字段列假设查询只有一个精确 Tag；后代聚合必须降级为通用只读列，避免把子路径字段写到父 namespace。
- 历史异常路径（前导/尾随 `/` 或空路径段）不可自动修复；新建入口拒绝异常路径，旧数据按普通未结构化 Tag 保留。
- 自动 Tag entity 与用户手工创建的空 schema 无法可靠区分；孤立清理必须保守扫描、显式选择、删除前复检，禁止随重扫自动执行。
- Sync 复用调用方已有的完整 parse tree；前端只解析当前行的 secondary Org text，二者共用同一个 range matcher，禁止回退到逐字符 `org-element-context`。
- 当前分支领先远端且包含既有提交；提交时只 stage 本 phase 文件，推送前按仓库协议 rebase。

## Rollback

- 删除 `supertag-smart-key.el`、主包 wiring 与 focused test。
- 恢复 inline tag point helper 的旧实现。
- 恢复同步提取的旧字符串正则；Store schema 无需回滚。
- 删除孤立 Tag 清理命令与 API；它没有迁移或自动修改 Store。
- 删除本 phase 文档与 CHANGE 索引。
