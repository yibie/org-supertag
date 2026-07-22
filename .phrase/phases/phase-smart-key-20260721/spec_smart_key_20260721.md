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

### Non-goals

- 不实现 Hyperbole 的 `defib`、`defact`、全局规则表或持久 Button。
- 不开放第三方 target/action 注册 Interface。
- 不把交互 action 与 Automation action 合并。
- 不改变 Store、Tag schema 或旧 Behavior 数据模型。
- 不实现动态 Transient、动作注册表或独立菜单框架；Assist 使用 Emacs 原生 completion UI。

## User Flows

1. 用户把光标放在 inline `#paper` 上并执行 `M-x supertag-smart-key`，打开该 tag 的 Table View。
2. 用户在 concept mention、node reference、Org link 或原生 Button 上执行同一命令，沿用既有跳转/激活动作。
3. 用户在 Node View 字段值或 Table cell 上执行同一命令，沿用既有编辑动作；Table 标题列打开源 node。
4. 用户在已有 ID 的 Org heading 上执行同一命令，打开既有 Node View；无 ID 时收到明确提示，原文不变。
5. 用户以前缀参数调用命令，获得当前 target 的相关动作；没有 target 时回落到完整的 `supertag-menu`。

## Edge Cases

- `supertag-context` 可能是布尔属性加平铺字段，也可能直接是 plist；两者必须归一化为同一临时 target。
- Org link 必须先于 inline tag，避免 URL fragment 被解释为 tag。
- inline tag 必须排除 source block、表格、注释、Org priority 与 URL fragment，与现有 font-lock 规则一致。
- recognizer 与 Node View 激活都不得为无 ID heading 调用 `org-id-get-create`；创建身份只属于显式的数据修改命令。
- 只有局部 RET keymap、没有语义属性的旧渲染文本只作为最后兼容回落，不宣称可解释 target。

## Acceptance Criteria

- Action 与 Assist 覆盖上述已有语义对象，并保持 first-match 顺序和对象动作集合可测试。
- `C-u M-x supertag-smart-key` 显示对象级动作；不同 target 的候选必须不同，并保留打开完整 `supertag-menu` 的出口。
- 没有默认全局/局部绑定，没有 Hyperbole 或新依赖。
- 非正文 `#...` 不打开 tag view。
- focused ERT 与仓库稳定测试套件通过。
