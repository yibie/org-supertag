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
