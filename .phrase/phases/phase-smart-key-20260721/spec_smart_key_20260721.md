# spec_smart_key_20260721

## Summary

为 org-supertag 增加一个不依赖 Hyperbole 的语义激活入口：在 Org 与 org-supertag View 中，把光标下已有的 text property、Emacs button、Org link、inline tag 或 heading 解释为临时 target，再调用既有 UI/Ops 命令。

## Goals & Non-goals

### Goals

- 提供 `supertag-smart-key`：普通调用执行当前 target 的默认动作，前缀调用只显示与该 target 相关的 Assist 动作。
- 提供可独立绑定的 `supertag-assist` 命令，与前缀调用共享相同对象动作列表。
- 兼容 Node View 与 Schema View 两种现有 `supertag-context` 形状。
- 复用 concept/node/reference 属性、Emacs button、Org link、inline `#tag`、Table cell 与 Org heading 的既有动作。
- recognizer 只返回临时数据，不创建 ID、不写 Store、不执行动作。
- 不设置默认按键，避免覆盖 Org 与各 View 的既有局部行为。
- 同步提取与渲染/point 识别共享“行首或空白后的 Org 正文 token”边界。
- Emacs Lisp `#'function` 引用不进入渲染、同步提取或 tag completion。
- SVG tag 字体低于正文行高，保持 badge 尺寸与标签可读性。
- `a/b/c` 以完整路径作为唯一 Tag ID；Schema、completion、View 与 Table 从路径派生 namespace。
- 路径 namespace 与显式 `:extends` 字段继承分别展示、分别操作。

### Non-goals

- 不实现 Hyperbole 的 `defib`、`defact`、全局规则表或持久 Button。
- 不开放第三方 target/action 注册 Interface。
- 不把交互 action 与 Automation action 合并。
- 不改变 Store、Tag schema 或旧 Behavior 数据模型。
- 不自动删除历史 Tag entity 或字段 schema；当前节点的 node-tag relation 只按本次权威标签集合对齐。
- 不创建父 namespace Tag entity、前缀索引、独立侧边栏或第二套继承关系。
- 不实现动态 Transient、动作注册表或独立菜单框架；Assist 使用 Emacs 原生 completion UI。

## User Flows

1. 用户把光标放在 inline `#paper` 上并执行 `M-x supertag-smart-key`，打开该 tag 的 Table View。
2. 用户在 concept mention、node reference、Org link 或原生 Button 上执行同一命令，沿用既有跳转/激活动作。
3. 用户在 Node View 字段值或 Table cell 上执行同一命令，沿用既有编辑动作；Table 标题列打开源 node。
4. 用户在已有 ID 的 Org heading 上执行同一命令，打开既有 Node View；无 ID 时收到明确提示，原文不变。
5. 用户以前缀参数调用命令，获得当前 target 的相关动作；没有 target 时回落到完整的 `supertag-menu`。
6. 用户执行 `supertag-back-to-heading`，heading 和子树保持不变，Node 的 Store 数据与 Org ID 被移除；其他 Org 属性保持不变。
7. 用户输入 `#` 触发 tag completion；历史 Store 中由 `#'function` 误提取的条目不再显示，合法标签仍可选。
8. 用户输入 `#emacs/` 时可先选择 namespace 继续补全，再选择真实 leaf；只有完整合法路径落库。
9. 用户在 Schema View 中看到 `emacs/` → `package/` → `elpa`，并可在 namespace 下新建路径或打开后代聚合 Table。
10. 用户重命名一个分支时，根路径、全部后代、Store 引用与 Org token 一起迁移；冲突时零写入。
11. 用户启动 Emacs 时已经恢复的 Org buffer，在 org-supertag 延迟加载完成后自动恢复 inline tag SVG，无需重开文件。
12. 用户输入 `#a/` 时只看到 `a/` 的直接子级；Add/Change/Capture/Tag Field 等入口使用相同的逐层选择方式。
13. 用户写入 `#ai_suggestions` 后同步，Store 保留完整 ID；重扫后可通过 `M-x supertag-cleanup-orphaned-tags` 逐项选择旧孤立 Tag，确认前不修改数据。

## Edge Cases

- `supertag-context` 可能是布尔属性加平铺字段，也可能直接是 plist；两者必须归一化为同一临时 target。
- Org link 必须先于 inline tag；其 target 内的 `#fragment` 不得被解释或渲染为 tag。
- 可渲染 inline tag 的 `#` 必须位于行首或空白之后；`word#fragment`、HTML entity 与转义 hash 不是 tag。
- 可渲染 inline tag 只存在于 Org headline 或正文 paragraph；inline code/verbatim/link/macro/target、table、
  fixed-width、drawer、property、COMMENT subtree、source/example/verse block 均不参与渲染和 point 识别。
- `#` 后仍以空白或下一个 `#` 为 token 边界，保留中文、emoji、层级 `/`、`C++` 与标点型 tag 名。
- 同步只读取当前 headline 标题和自身 section 的直接 paragraph 文本；子 headline、Org inline object、
  drawer、block 与 COMMENT subtree 不贡献 inline tag。
- `#'name` 是 Emacs Lisp function quote，不是标签；即使旧 Store 中已有对应 tag entity，也不得出现在 completion。
- recognizer 与 Node View 激活都不得为无 ID heading 调用 `org-id-get-create`；创建身份只属于显式的数据修改命令。
- Node 退化必须删除 Org ID；若属性抽屉只含 ID，则不得留下空 drawer。
- 只有局部 RET keymap、没有语义属性的旧渲染文本只作为最后兼容回落，不宣称可解释 target。
- 缺失的路径父级只作为虚拟 namespace 展示，不写 Store；同名真实 Tag 可同时作为 branch。
- 从 namespace/branch 打开的 descendant Table 只显示 Title/Tags/File，字段与 schema 修改命令必须拒绝执行。
- 分支不得移动进自己的子 namespace；普通字符串字段即使等于旧 Tag ID 也不得被重命名。
- org-supertag 可以晚于 Org buffer 加载；自动样式启用必须同时覆盖现存 buffer 和以后进入 `org-mode` 的 buffer。
- namespace 下没有真实子路径时返回空候选，不回退显示无关根级 Tag；继续输入合法叶段仍可创建完整路径。
- Org 将 `_suffix` 解析为 subscript 时，若它属于同一个 `#token`，同步仍读取原始完整 token；独立 subscript/link/code 内的 `#` 仍不是 Tag。

## Acceptance Criteria

- Action 与 Assist 覆盖上述已有语义对象，并保持 first-match 顺序和对象动作集合可测试。
- `C-u M-x supertag-smart-key` 显示对象级动作；不同 target 的候选必须不同，并保留打开完整 `supertag-menu` 的出口。
- 没有默认全局/局部绑定，没有 Hyperbole 或新依赖。
- 非正文 `#...` 不打开 tag view。
- Org link target 内的 `#fragment` 不显示 inline tag face 或 SVG pill。
- 边界矩阵同时覆盖合法正文 token 与 Org 非正文对象，face/SVG/point 三条路径共享同一结论。
- 同步后的 node `:tags` 与同一文本的渲染/point 结论一致；不需要 Store schema 迁移。
- `#'function` 在渲染、同步提取和 completion 三条路径上结论一致；不自动删除历史 Store 数据。
- 20px frame character height 下，默认 SVG tag 字号为 14px；修改字号比例后不得命中旧尺寸缓存。
- `supertag-back-to-heading` 不得留下可被同步重新识别的 ID，也不得删除无关 Org 属性。
- 默认 Tag 查询保持精确；只有显式 `include-descendants` 命中路径后代且不命中 `emacs2/...`。
- Schema 缩进只由 `/` 路径决定，`:extends` 仅以箭头和 inherited fields 表达。
- 单节点同步后 Tag entity、node `:tags` 与 node-tag relation 一致；移除 token 后只回收当前节点的失效关系。
- descendant scope 在 View/Table 刷新后保持；聚合 Table 不提供 tag-specific 字段写入。
- 延迟加载完成后，现存 Org buffer 的 `supertag-view-style-mode` 自动开启；非 Org buffer 不受影响。
- completion 的 `action=t` 仍按当前输入过滤候选；普通 Tag/namespace 不显示冗余类型后缀，只有新路径显示 `[New]`。
- 原始 Tag token 必须在已解析的非透明 Org object 起点截断；sub/superscript 只保留 `_`/`^` 原文，不能隐藏其内部或紧邻的 link/code 边界。
- 孤立 Tag 候选不得包含被 node、relation、field/schema（含 `:tag` default/options）、inheritance、automation、saved query 或已加载 view config 引用的 ID。
- 旧预览必须在调用时整体复检；每个实际删除还必须在 `before-operation-hook` 之后、Store mutation 之前再次复检。
- 任何 Tag 批量事务回滚后，resolved schema cache 必须与已恢复的 Store 一致。
- focused ERT 与仓库稳定测试套件通过。
