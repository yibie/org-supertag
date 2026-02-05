# task_view_dsl_20260204

- task001 [x] 建立 view-dsl phase 文档并登记 CHANGE 索引
  - 产出：`spec_view_dsl_20260204.md` / `plan_view_dsl_20260204.md` / `task_view_dsl_20260204.md` / `change_view_dsl_20260204.md`，并更新 `.phrase/docs/CHANGE.md`
  - 验证方式：上述文件存在且内容与“DSL 作为主入口”目标一致（手动检查）
  - 影响范围：phase 文档与 CHANGE 索引

- task002 [x] 修复 view framework 的 DSL 阻塞问题并补充 DSL 文档
  - 产出：
    - `supertag-view-framework.el` 修复 buffer 命名冻结、widget 类型归一化、配置导出 key 格式
    - `doc/VIEW_FRAMEWORK_DEV_GUIDE.md` 说明 `:type` 支持 keyword/symbol
  - 验证方式：代码检查 + DSL 示例仍可按文档方式加载（手动验证）
  - 影响范围：`supertag-view-framework.el`、`doc/VIEW_FRAMEWORK_DEV_GUIDE.md`

- task003 [x] 固化 DSL v2 规范（嵌套/绑定/刷新/组件清单）
  - 产出：`tech_refer_view_dsl_20260204.md` 或对 `spec_*` 增补 DSL 规范细节
  - 验证方式：文档中给出完整的组件模型与绑定语义（手动检查）
  - 影响范围：phase 文档

- task004 [x] 实现 DSL v2（嵌套渲染 + 容器组件 + 函数绑定）
  - 产出：`supertag-view-framework.el` 支持 `:children` 与 `(lambda (ctx) ...)` 绑定，新增最小容器组件
  - 验证方式：20 行 DSL 示例可渲染嵌套视图（手动验证）
  - 影响范围：`supertag-view-framework.el`

- task005 [x] 提供统一刷新命令与重渲染入口
  - 产出：刷新命令可在数据更新后重新渲染当前 view
  - 验证方式：修改数据后手动刷新，视图更新（手动验证）
  - 影响范围：`supertag-view-framework.el`（必要时扩展到 view schema 相关入口）

- task006 [x] 更新文档与示例，补齐最小验证步骤
  - 产出：`doc/VIEW_FRAMEWORK_DEV_GUIDE.md` 增加嵌套 + 绑定 + 刷新示例
  - 验证方式：示例可复制运行且结果可观察（手动验证）
  - 影响范围：`doc/VIEW_FRAMEWORK_DEV_GUIDE.md`、示例文件（如需）

- task007 [x] 更新 DSL v2 demo 示例文件
  - 产出：`doc/examples/supertag-view-demo-dashboard.el` 改为 DSL v2（嵌套 + 绑定 + 手动刷新）
  - 验证方式：加载示例并运行 `M-x supertag-view-demo-dashboard-open` 可渲染视图；执行 `M-x supertag-view-refresh` 能刷新（手动验证）
  - 影响范围：`doc/examples/supertag-view-demo-dashboard.el`

- task008 [x] 示例中使用虚拟列与全局字段能力
  - 产出：demo 通过 `:get-vc` 读取 virtual column，通过 `:get-global-field` 读取 global field
  - 验证方式：为节点配置对应 VC/字段后，示例显示进度与优先级（手动验证）
  - 影响范围：`doc/examples/supertag-view-demo-dashboard.el`

- task009 [x] Demo 示例改为使用内置样例数据
  - 产出：demo 使用内置 sample data，不读取用户真实数据
  - 验证方式：运行 demo 可渲染；数据不依赖用户 store（手动验证）
  - 影响范围：`doc/examples/supertag-view-demo-dashboard.el`

- task010 [x] 补齐基础组件库（网页视角）
  - 产出：新增 `card/panel/kv/badge/empty/toolbar` 组件
  - 验证方式：示例渲染无报错（手动验证）
  - 影响范围：`supertag-view-framework.el`、`doc/VIEW_FRAMEWORK_DEV_GUIDE.md`

- task011 [x] 将新组件应用到 demo
  - 产出：demo 使用 `card/kv/badge/toolbar` 组件
  - 验证方式：运行 demo 可渲染（手动验证）
  - 影响范围：`doc/examples/supertag-view-demo-dashboard.el`

- task012 [x] 对齐组件视觉风格（card/kv/toolbar/badge）
  - 产出：`card/panel` 使用 kanban 边框，`toolbar` 使用 Operations 样式，`badge` 使用 face + `[]`，`kv` 对齐列宽
  - 验证方式：demo 视觉输出符合预期（手动验证）
  - 影响范围：`supertag-view-framework.el`、`doc/VIEW_FRAMEWORK_DEV_GUIDE.md`、`spec_view_dsl_20260204.md`

- task013 [x] 将 kv 更名为 field 并改为表格样式
  - 产出：`field` 组件输出表格样式；`kv` 作为别名保留
  - 验证方式：示例渲染为表格样式（手动验证）
  - 影响范围：`supertag-view-framework.el`、`doc/VIEW_FRAMEWORK_DEV_GUIDE.md`、`doc/examples/supertag-view-demo-dashboard.el`

- task014 [x] 对齐 field 与 table 的表格样式
  - 产出：`field` 输出边框/标题/分隔线与 `supertag-view-table` 一致
  - 验证方式：demo 中 field 视觉与 table 相同风格（手动验证）
  - 影响范围：`supertag-view-framework.el`、`doc/examples/supertag-view-demo-dashboard.el`

- task015 [x] field 表格样式去掉标题栏
  - 产出：`field` 渲染不再显示表头，仅保留边框与分隔线
  - 验证方式：demo 中 field 无表头（手动验证）
  - 影响范围：`supertag-view-framework.el`

- task016 [x] demo 展示全部基础组件与能力
  - 产出：demo 覆盖 header/subheader/toolbar/section/columns/stack/card/panel/field/list/table/badge/empty
  - 验证方式：运行 demo 可看到组件展示区（手动验证）
  - 影响范围：`doc/examples/supertag-view-demo-dashboard.el`

- task017 [x] 修复 demo 配置括号错误
  - 产出：`supertag-view-demo-dashboard--config` 括号结构可读且可加载
  - 验证方式：文件可被 `require`，无语法错误（手动验证）
  - 影响范围：`doc/examples/supertag-view-demo-dashboard.el`

- task018 [x] 修复父标题变更后 `:olp` 不刷新导致 add-reference 候选过期（issue007）
  - 产出：`supertag-services-sync.el` 将 `:olp` 纳入 hash，合并时覆盖旧 `:olp`
  - 验证方式：重命名父标题 → `M-x supertag-sync-force-resync-current-file` → `supertag-add-reference` 候选显示新路径（手动验证）
  - 影响范围：`supertag-services-sync.el`、`issue007`

- task019 [x] 接入 `supertag-sync-hash-props` 参与节点 hash 计算
  - 产出：`supertag-services-sync.el` 使用 `supertag-sync-hash-props` 构建 hash 输入，默认包含 `:olp`
  - 验证方式：父标题变更后 resync 触发节点更新；自定义 `supertag-sync-hash-props` 生效（手动验证）
  - 影响范围：`supertag-services-sync.el`
