# task_smart_key_20260721

- task001 [x] 实现并验证最小语义 Smart Key Module
  - 产出：`supertag-smart-key.el`、共享 inline tag point 识别修正、主包 wiring、focused ERT、phase change 记录
  - 验证方式：focused ERT、`./test/run-tests.sh all`、主包 batch load、byte compile、`git diff --check`
  - 影响范围：Org buffer 与 org-supertag View 的显式命令调用；不设置默认按键、不改 Store

- task002 [x] 实现对象级 Assist
  - 产出：根据当前 target 生成相关动作列表；无 target 时回落到 `supertag-menu`
  - 验证方式：focused ERT 锁定对象菜单差异、动作参数与全局菜单回落；全量 ERT、byte compile、`git diff --check`
  - 影响范围：`supertag-smart-key` 的前缀调用；`supertag-menu` 保持完整且不改变

- task003 [x] 阻止 Node View 为无 ID heading 创建身份
  - 产出：Node View 与 Smart Key Node Action 只读取已有 ID；无 ID 时提示且不修改 Org buffer
  - 验证方式：focused 红/绿回归、全量 ERT、byte compile、`git diff --check`
  - 影响范围：仅 View 激活；显式创建、同步和 tag 编辑命令保持原有写入行为

- task004 [x] 修复 Node 退化后仍保留 Org ID
  - 产出：`supertag-back-to-heading` 删除 `ID`；仅含 ID 时由 Org 清理空属性抽屉，其他属性保持不变
  - 验证方式：focused 红/绿回归、全量 ERT、byte compile、`git diff --check`
  - 影响范围：仅显式 `supertag-back-to-heading` 命令

- task005 [x] 阻止 Org link fragment 被渲染为 inline tag
  - 产出：共享 validator 使用 Org 原生 link 语法排除 link target，删除只识别 `://` 的窄 URL 特判
  - 验证方式：`test/test-inline-tag-filter.el` 红/绿回归、focused/full ERT、byte compile、`git diff --check`
  - 影响范围：face/SVG font-lock 与 point tag 识别；正文 inline tag 行为保持不变

- task006 [x] 建立并实现 inline tag 渲染边界矩阵
  - 产出：以空白 token 边界和 Org 正文 context 替代 source/table/comment/link 等逐项特判
  - 验证方式：12 组正反边界红/绿自检、实际 font-lock property 检查、focused/full ERT、静态检查
  - 影响范围：face/SVG font-lock 与 point tag 识别；同步提取和 tag 名存储格式不变

- task007 [x] 恢复 inline tag validator 的 Emacs 29 兼容性
  - 产出：按 Org 9.6 API 约定以类型列表调用 `org-element-lineage`
  - 验证方式：focused Smart Key ERT、12 组边界自检、Emacs 29.1/29.4 CI
  - 影响范围：仅修复最低支持版本上的参数类型错误，不改变 tag 边界

- task008 [x] 将同步提取对齐 inline tag 正文边界
  - 产出：复用单一 token 正则，只从当前 headline 标题与自身 paragraph 的直接文本提取
  - 验证方式：字符串边界、Org 结构矩阵、标题清洗与 COMMENT subtree ERT；全量 ERT、Emacs 29 CI
  - 影响范围：下一次同步会修正 node `:tags`；Store schema、tag 名格式及历史 tag 定义保持不变

- task009 [ ] 明确原生 `:tag:` 的增量同步与 node-tag 关系清理策略
  - 产出：按 `supertag-sync-legacy-tags-policy` 定义增量/全量一致的读取与关系回收规则
  - 验证方式：read-only、lazy-convert、preserve、ignore 四种策略的增量/全量矩阵
  - 影响范围：task013 已统一非 ignore 策略的读取与当前节点关系回收；`lazy-convert` 文件改写契约仍由 issue022 跟踪

- task010 [x] 降低 GitHub Actions 的无效运行次数
  - 产出：仅在 main/PR 非文档变更或手动触发时运行；测试结果只在失败时上传
  - 验证方式：YAML 解析、`git diff --check`、推送后的 Emacs 29.1/29.4 矩阵
  - 影响范围：不改变测试内容；其他分支 push 与纯 Markdown/`.phrase` 变更不再自动运行

- task011 [x] 排除函数引用伪标签并缩小 SVG tag 字体
  - 产出：共享 tag-name 判定、completion 历史污染过滤、SVG 默认字号与缓存键修正
  - 验证方式：真实污染 fixture 红/绿自检、focused/full ERT、SVG 生成图视觉判定、byte compile、`git diff --check`
  - 影响范围：`#'function` 不再作为标签；不删除 Store 数据；SVG badge 外框尺寸不变

- task012 [x] 为完整路径 Tag ID 增加显式后代查询
  - 产出：共享路径段边界判断、scan query/View Data API 可选后代查询、真实 Tag 后代枚举
  - 验证方式：focused ERT 锁定完整路径保留、精确查询兼容、段边界与多级后代；10k-node 基准、全量 ERT、byte compile、`git diff --check`
  - 影响范围：默认查询行为不变；不新增 Store 字段、父 Tag entity 或 `:extends` 关系

- task013 [x] 完成嵌套标签从 Store 到交互视图的闭环
  - 产出：共享路径语义、增量同步关系一致性、Schema namespace 树、子路径创建、路径补全、后代聚合 View/Table，以及安全的分支重命名
  - 验证方式：focused ERT 锁定同步→Store→查询→Schema→View/Table→completion；真实 Store 只读验证、Schema 截图视觉判定、全量 ERT、byte compile、`git diff --check`
  - 影响范围：完整路径仍是唯一 Tag ID；namespace 不写入 `:extends`；后代聚合表只读且不暴露 tag-specific 字段编辑

- task014 [x] 修复延迟加载后现存 Org buffer 不显示 inline tag SVG
  - 产出：视图 helper 加载时补启用现存 Org buffer，并保留 `org-mode-hook` 对后续 buffer 的处理
  - 验证方式：late-load 回归 ERT、view/full ERT、byte compile、`git diff --check`
  - 影响范围：仅在 `supertag-view-style-auto-enable` 非 nil 时启用样式；非 Org buffer 与显式关闭配置不变

- task015 [x] 完成嵌套标签的逐层补全与统一输入
  - 产出：namespace direct-child 候选、正确的 completion-table 过滤、共享 Tag reader，并接入行内/Add/Change/Capture/Tag Field/Query/View/Table
  - 验证方式：focused ERT 19/19、相关 ERT 54/54、全量 ERT 335/335、completion/inline self-check、byte compile、`check-parens`、`git diff --check`
  - 影响范围：完整路径仍是唯一 Tag ID；namespace 候选只导航；不新增缓存、索引、Store 字段、实体或依赖

- task016 [x] 修复下划线 Tag 解析并提供安全的孤立 Tag 清理
  - 产出：基于原始 buffer 区间与已解析 object ranges 的 token 提取/标题清洗、保守孤立引用扫描、hook 后最终检查、rollback cache 修复、事务化删除 API、显式选择命令与源码优先测试入口
  - 验证方式：underscore/object 红绿回归、清理引用矩阵、真实笔记/Store 只读探测、全量 ERT、byte compile、`check-parens`、`git diff --check`
  - 影响范围：重扫会把错误 node tag 修正为完整下划线 ID；不会自动删除历史 Tag entity，也不会由清理命令修改 Org 文件
  - Reopened 2026-08-01：复核发现 Org object 跨界、schema field 引用漏扫、hook 后 TOCTOU 与事务回滚 cache 不一致；完成四类回归前禁止建议运行清理命令
  - Closed again 2026-08-01：上述四类回归及 underscore + nested object 组合边界均转绿；focused 42/42、full 344/344、真实笔记/Store 只读探测和双重复审通过
  - Reopened again 2026-08-01：`a18e6d8` 只覆盖 before-hook；after-hook、跨批次已删除候选、Smart Key matcher 与 rollback hook fail-fast 仍有可执行反例
  - Completed 2026-08-01：共享 range matcher、显式候选 post-hook 扫描与全量 rollback invariant runner 已实现；新增反例回归转绿，全量 ERT 349/349
  - Reopened 2026-08-02：`a9257518` 的 predicate 虽缩短 match data，真实 face/SVG font-lock extent 仍使用预先固定的宽 group 0
  - Completed 2026-08-02：face/SVG keyword 改用同一个 range-aware search matcher；真实 property extent 回归转绿，全量 ERT 351/351

- task017 [x] 允许从已有平面 Tag 继续补全子路径
  - 产出：精确匹配已有 Tag 时生成只导航的 `/` 子 namespace 候选；复用现有 CAPF、namespace property 与落库边界
  - 验证方式：真实 `#diary` CAPF basic + Corfu/orderless 枚举回归、focused ERT 20/20、全量 ERT 352/352、byte compile、`check-parens`、`git diff --check`
  - 影响范围：不创建父 namespace entity，不改 Store；用户选择 `diary/` 后继续输入 leaf，只有完整路径落库
