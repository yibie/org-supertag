# change_view_dsl_20260204

## 2026-02-04

### Added
- Phase initialized with spec/plan/task/change documents
- Phase registered in global CHANGE index

### Modified
- `supertag-view-framework.el`: fix buffer name evaluation timing, widget type normalization, config export key formatting
- `doc/VIEW_FRAMEWORK_DEV_GUIDE.md`: clarify DSL widget `:type` supports keyword/symbol

### task001 [x] 建立 view-dsl phase 文档并登记 CHANGE 索引
- Files:
  - `.phrase/phases/phase-view-dsl-20260204/spec_view_dsl_20260204.md`
  - `.phrase/phases/phase-view-dsl-20260204/plan_view_dsl_20260204.md`
  - `.phrase/phases/phase-view-dsl-20260204/task_view_dsl_20260204.md`
  - `.phrase/phases/phase-view-dsl-20260204/change_view_dsl_20260204.md`
  - `.phrase/docs/CHANGE.md`
- Verification: 手动检查文件存在与内容一致性

### task002 [x] 修复 view framework 的 DSL 阻塞问题并补充 DSL 文档
- Files:
  - `supertag-view-framework.el`
  - `doc/VIEW_FRAMEWORK_DEV_GUIDE.md`
- Change:
  - buffer 名称改为运行期计算
  - widget 类型归一化以兼容 keyword/symbol
  - 导出配置 key 格式修复
  - DSL 文档补充 `:type` 说明
- Verification: 代码检查（未运行自动化测试）

### task003 [x] 固化 DSL v2 规范（嵌套/绑定/刷新/组件清单）
- Files:
  - `.phrase/phases/phase-view-dsl-20260204/tech_refer_view_dsl_20260204.md`
- Change:
  - 明确 DSL v2 的结构、嵌套语义、函数绑定、context 契约与容器组件
  - 定义统一刷新与渲染内部 API 的建议接口
- Verification: 文档检查（未运行自动化测试）

### task004 [x] 实现 DSL v2（嵌套渲染 + 容器组件 + 函数绑定）
- Files:
  - `supertag-view-framework.el`
- Change:
  - 新增 DSL 渲染辅助函数（prop 绑定、递归渲染、列渲染）
  - 扩展 widget 渲染支持 context 参数
  - 新增容器组件 `section` / `stack` / `columns`
  - `supertag-view-define-from-config` 改为运行期渲染（移除 eval）
  - 构建 context 时补齐 nodes 与访问函数入口
- Verification: 未执行手动验证

### task005 [x] 提供统一刷新命令与重渲染入口
- Files:
  - `supertag-view-framework.el`
- Change:
  - 增加 view 渲染时的 buffer-local 元数据（view id/tag/context builder）
  - 新增 `supertag-view-refresh` 统一刷新命令
  - 渲染管线支持 context builder 回放
- Verification: 未执行手动验证

### task006 [x] 更新文档与示例，补齐最小验证步骤
- Files:
  - `doc/VIEW_FRAMEWORK_DEV_GUIDE.md`
  - `supertag-view-framework.el`
- Change:
  - DSL 文档更新为主入口描述，补充嵌套与函数绑定示例
  - 增加容器组件与刷新命令说明、最小验证步骤
  - 更新 `supertag-view-dsl-example` 展示嵌套与绑定
- Verification: 未执行手动验证

### task007 [x] 更新 DSL v2 demo 示例文件
- Files:
  - `doc/examples/supertag-view-demo-dashboard.el`
- Change:
  - 使用 DSL v2 重写 demo：嵌套容器 + 函数绑定 + 手动刷新入口
  - 移除自定义 mode/订阅/自动刷新逻辑，聚焦 DSL 主入口
- Verification: 未执行手动验证

### task008 [x] 示例中使用虚拟列与全局字段能力
- Files:
  - `doc/examples/supertag-view-demo-dashboard.el`
- Change:
  - 通过 `:get-vc` 读取虚拟列进度值（`progress-percent`）
  - 通过 `:get-global-field` 读取全局字段 `priority`
- Verification: 未执行手动验证

### task009 [x] Demo 示例改为使用内置样例数据
- Files:
  - `doc/examples/supertag-view-demo-dashboard.el`
- Change:
  - 使用内置 sample nodes/fields/virtual-column 数据，不读取用户真实数据
  - `M-x supertag-view-demo-dashboard-open` 仅用于展示标签文本
  - 节点列表改为逐条展示 progress bar 样式
- Verification: 未执行手动验证

### task010 [x] 补齐基础组件库（网页视角）
- Files:
  - `supertag-view-framework.el`
  - `doc/VIEW_FRAMEWORK_DEV_GUIDE.md`
- Change:
  - 新增 `card/panel/kv/badge/empty/toolbar` 组件
  - 文档补充组件示例
- Verification: 未执行手动验证

### task011 [x] 将新组件应用到 demo
- Files:
  - `doc/examples/supertag-view-demo-dashboard.el`
- Change:
  - Demo 使用 `card/kv/badge/toolbar` 组件展示结构与样式
- Verification: 未执行手动验证

### task012 [x] 对齐组件视觉风格（card/kv/toolbar/badge）
- Files:
  - `supertag-view-framework.el`
  - `doc/VIEW_FRAMEWORK_DEV_GUIDE.md`
  - `.phrase/phases/phase-view-dsl-20260204/spec_view_dsl_20260204.md`
- Change:
  - `card/panel` 使用 kanban 边框样式
  - `toolbar` 采用 “Operations” 风格 + 粗体标签
  - `badge` 使用 face + `[]`
  - `kv` 采用对齐列宽输出
- Verification: 未执行手动验证

### task013 [x] 将 kv 更名为 field 并改为表格样式
- Files:
  - `supertag-view-framework.el`
  - `doc/VIEW_FRAMEWORK_DEV_GUIDE.md`
  - `doc/examples/supertag-view-demo-dashboard.el`
- Change:
  - `field` 组件使用表格样式输出；`kv` 作为别名保留
  - demo 与文档示例改用 `field`
- Verification: 未执行手动验证

## 2026-02-05

### task014 [x] 对齐 field 与 table 的表格样式
- Files:
  - `supertag-view-framework.el`
- Change:
  - `field` 使用与 `supertag-view-table` 一致的边框/表头/分隔线样式
- Verification: 未执行手动验证

### task015 [x] field 表格样式去掉标题栏
- Files:
  - `supertag-view-framework.el`
- Change:
  - `field` 渲染移除表头，只保留边框与分隔线
- Verification: 未执行手动验证

### task016 [x] demo 展示全部基础组件与能力
- Files:
  - `doc/examples/supertag-view-demo-dashboard.el`
- Change:
  - demo 增加 subheader/section/panel/list/table/badge/empty 展示区
  - toolbar 文案调整为快捷键风格
- Verification: 未执行手动验证

### task017 [x] 修复 demo 配置括号错误
- Files:
  - `doc/examples/supertag-view-demo-dashboard.el`
- Change:
  - 修正 `supertag-view-demo-dashboard--config` 括号结构
  - 移除多余的顶层闭合括号
- Verification: 未执行手动验证

### task018 [x] 修复父标题变更后 `:olp` 不刷新导致候选过期
- Files:
  - `supertag-services-sync.el`
- Change:
  - `supertag-node-hash` 纳入 `:olp`，确保父路径变更触发更新
  - `supertag--merge-node-properties` 将 `:olp` 视为标准字段，避免旧值回写
- Verification: 未执行手动验证

### task019 [x] 接入 `supertag-sync-hash-props` 参与节点 hash 计算
- Files:
  - `supertag-services-sync.el`
- Change:
  - `supertag-node-hash` 使用 `supertag-sync-hash-props` 构建 hash 输入
  - 默认 `supertag-sync-hash-props` 补齐 `:olp/:content/:properties` 等字段
- Verification: 未执行手动验证
