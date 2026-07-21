# task_smart_key_20260721

- task001 [x] 实现并验证最小语义 Smart Key Module
  - 产出：`supertag-smart-key.el`、共享 inline tag point 识别修正、主包 wiring、focused ERT、phase change 记录
  - 验证方式：focused ERT、`./test/run-tests.sh all`、主包 batch load、byte compile、`git diff --check`
  - 影响范围：Org buffer 与 org-supertag View 的显式命令调用；不设置默认按键、不改 Store
