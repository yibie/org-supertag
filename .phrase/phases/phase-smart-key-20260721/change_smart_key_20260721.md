# change_smart_key_20260721

- 2026-07-26 Align
  - Files: `supertag-core-transform.el`, `supertag-services-sync.el`, `test/extractor-test.el`, phase docs
  - Functions: `supertag-transform-extract-inline-tags`, `supertag--extract-inline-tags`,
    `supertag--strip-inline-tags`, `supertag-extractor--tags`
  - Changes:
    - String extraction now requires the same line-start/whitespace token boundary as rendering.
    - Sync reads direct prose from the current headline title and its own section paragraphs; Org inline objects,
      drawers, blocks, COMMENT subtrees and child headlines do not contribute tags.
    - Title cleanup removes only the accepted prose tokens and preserves link fragments, embedded hashes and code.
    - Removed the duplicate sync string extractor and reused the core transform helper.
  - Compatibility:
    - No Store schema or persisted tag-name migration is required.
    - A subsequent sync updates each node's `:tags`; historical tag definitions and relations are retained
      intentionally because incremental native `:tag:` reconciliation is outside this task.
  - Verification:
    - Three new boundary/structure extraction tests failed before the change and pass afterward.
    - Focused extractor ERT: 19/19 passed.
    - Full stable ERT: 300/300 passed.
    - Main package load, check-parens, byte compilation and `git diff --check` passed;
      byte compilation retains pre-existing warnings only.
    - Emacs 29.1/29.4 CI: passed in run 30165705638.
  - Related: `issue021`, `task008`

- 2026-07-26 Fix
  - Files: `supertag-view-helper.el`, phase docs
  - Function: `supertag-view-helper--valid-inline-tag-match-p`
  - Changes:
    - Passed headline types to `org-element-lineage` as a list, matching the Org 9.6 API bundled with the declared Emacs 29.1 floor.
    - Preserved the established rendering and point-recognition boundary without adding a version branch.
  - Verification:
    - Reproduced CI failure on Emacs 29.1 and 29.4: three Smart Key tests raised `(wrong-type-argument listp headline)`.
    - Local focused Smart Key ERT: 11/11 passed.
    - Focused 12-case inline-tag boundary self-check passed.
    - Emacs 29.1/29.4 CI: passed in run 30165131179.
  - Related: `issue021`, `task007`

- 2026-07-25 Harden
  - Files: `supertag-view-helper.el`, `test/test-inline-tag-filter.el`, phase docs
  - Function: `supertag-view-helper--valid-inline-tag-match-p`
  - Changes:
    - Replaced four context helpers and accumulated priority/link/face exceptions with one rule: a renderable tag is a whitespace-delimited token in an Org headline or prose paragraph.
    - Drawer/property content, COMMENT subtrees, links, inline code/verbatim, macro/target objects, tables, fixed-width text and source/example/verse blocks are excluded by Org structure.
    - Embedded hashes such as `word#fragment`, `&#169;` and `\#escaped` are rejected at the token boundary.
    - Unicode, emoji, hierarchy `/`, `C++` and punctuation-bearing tag names remain valid.
    - Expanded the focused self-check to a 12-case positive/negative boundary matrix.
  - Simplification: core context logic changed from 67 lines of special cases to one 10-line predicate.
  - Verification:
    - Expanded 12-case boundary self-check passed after reproducing the embedded-hash failure.
    - Actual font-lock properties: prose `#real` receives `supertag-inline-face`; inline code and embedded fragments receive neither face nor display.
    - Focused Smart Key ERT: 11/11 passed.
    - Full stable ERT suite: 297/297 passed.
    - Batch load, check-parens, byte compilation and `git diff --check` passed; byte compilation retains pre-existing warnings only.
    - 1000-line mixed-content font-lock smoke test: 0.177 seconds.
  - Related: `issue021`, `task006`

- 2026-07-25 Fix
  - Files: `supertag-view-helper.el`, `test/test-inline-tag-filter.el`, phase docs
  - Function: `supertag-view-helper--valid-inline-tag-match-p`
  - Changes:
    - Inline tag validation now uses Org's native link recognition, so `#fragment` text inside bracket and plain links never receives tag face/SVG display properties.
    - Removed the narrower `://` scan; face rendering, SVG rendering and point lookup continue to share one validator.
  - Risk: intentionally treats an inline `#tag` written inside an Org link description as part of the link, not as a second interactive object.
  - Verification:
    - Bracket-link self-check: passed after reproducing the pre-fix assertion failure.
    - Focused Smart Key ERT: 11/11 passed.
    - Full stable ERT suite: 297/297 passed.
    - Batch load, check-parens, byte compilation and `git diff --check` passed; byte compilation retains pre-existing warnings only.
  - Related: `issue021`, `task005`

- 2026-07-22 Fix
  - Files: `supertag-ui-commands.el`, `test/test-smart-key.el`, phase docs
  - Function: `supertag-back-to-heading`
  - Changes:
    - Node demotion now removes the Org `ID` after deleting the Store node.
    - Org's native property API removes an ID-only drawer and preserves unrelated properties.
    - Corrected the confirmation prompt typo.
  - Verification: focused 11/11; full suite 295/295; batch load, check-parens, byte compile and `git diff --check` passed.
  - Related: `issue019`, `task004`

- 2026-07-22 Fix
  - Files: `supertag-view-node.el`, `supertag-smart-key.el`, `test/test-smart-key.el`, phase docs
  - Functions: `supertag-view-node--current-entity-id`, `supertag-view-node`, `supertag--activate-target`
  - Changes:
    - Node View and its follow logic now resolve only existing Org IDs.
    - Smart Key reports an untracked heading instead of calling `org-id-get-create`.
    - Explicit node creation/sync commands retain their existing mutating helpers.
  - Verification: focused Smart Key 9/9; full suite 293/293.
  - Related: `issue018`, `task003`

- 2026-07-21 Add/Modify
  - Files: `supertag-smart-key.el`, `supertag-ui-commands.el`, `supertag-menu.el`, `test/test-smart-key.el`, `README.md`, `README_CN.md`, phase docs
  - Functions: `supertag-assist`, `supertag--assist-actions`, `supertag-smart-key`, `supertag-rename-tag`, `supertag-delete-tag-everywhere`
  - Changes:
    - 前缀调用与独立 `supertag-assist` 命令现在按 target 生成小型 completion 动作列表；默认动作排第一，并保留完整 `supertag-menu` 出口。
    - inline tag 可直接打开 schema、预选当前 tag 进行重命名或删除；heading、node reference、field value 与 Table cell 只显示已有且可安全复用的相关动作。
    - 无 target 时继续回落到完整 `supertag-menu`；全局菜单内容和按键不变。
    - `supertag-rename-tag` 与 `supertag-delete-tag-everywhere` 接受可选 tag 参数，避免 Assist 再次询问当前对象。
  - Risk: 对象动作表是显式 first-match 结果；新增 target kind 时必须提供明确默认标签，破坏性动作仍须由原命令确认。
  - Verification:
    - `./test/run-tests.sh smart-key`: 8/8 passed
    - `./test/run-tests.sh all`: 278/278 passed
    - changed modules byte compile、checkdoc、主包隔离 batch load、`git diff --check` 通过
  - Related: `task002`

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
