# change_smart_key_20260721

- 2026-07-21 Add/Modify
  - Files: `supertag-smart-key.el`, `supertag-view-helper.el`, `org-supertag.el`, `test/test-smart-key.el`, `test/run-tests.sh`, `README.md`, `README_CN.md`
  - Functions: `supertag-smart-key`, `supertag--target-at-point`, `supertag--activate-target`, `supertag-view-helper-get-tag-at-point`
  - Changes:
    - 增加唯一公开入口 `supertag-smart-key`；内部把既有 context/node/reference property、Table cell、Emacs button、Org link、inline tag、heading 与旧 RET keymap 归一化为临时 target。
    - 普通调用复用既有 View/UI 命令；前缀调用复用 `supertag-menu`；不设置默认按键、不写 Store、不新增依赖或注册表。
    - inline tag point 识别改为复用既有 validator，排除 source block、表格、注释、Org priority 与 URL fragment。
    - focused ERT 加入稳定测试 runner；README/README_CN 补充命令、Assist 与无默认绑键边界。
  - Risk: first-match 顺序是行为契约；新增 recognizer 时必须保持具体语义优先于 Org link、inline tag、heading 与兼容回落。
  - Verification:
    - `./test/run-tests.sh smart-key`: 7/7 passed
    - `./test/run-tests.sh all`: 277/277 passed
    - 主包 batch load、inline tag self-check、`checkdoc-file`、新模块 byte compile、`git diff --check` 通过
  - Related: `task001`

- 2026-07-21 Add
  - Files: `spec_smart_key_20260721.md`, `plan_smart_key_20260721.md`, `task_smart_key_20260721.md`, `change_smart_key_20260721.md`
  - Changes: 锁定无 Hyperbole 依赖的最小 Smart Key Interface、first-match 边界、无副作用约束与验证方式。
  - Related: `task001`
