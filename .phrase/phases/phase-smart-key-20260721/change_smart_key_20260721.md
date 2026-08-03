# change_smart_key_20260721

- 2026-08-03 Fix
  - Files: `supertag-ui-completion.el`, `test/tag-path-test.el`, task020 phase/docs
  - Functions: CAPF candidate construction and post-completion normalization
  - Changes: parent-prefix input now adds read-only display aliases such as `diary/happy`; selection replaces the alias with real ID `happy` before sync or Store writes.
  - Simplification: aliases are derived only for the live parent prefix and reuse the existing `supertag-tag-id` property; no index, cache or second identity field.
  - Verification: focused ERT 21/21; full ERT 353/353; live Corfu enumerated the real `diary` children and formatted `diary/happy`; collision regression preserves a real full-path ID; byte compile, paren/diff checks passed with no repository `.elc`.
  - Risk: manually typing a display path without selecting completion remains a literal full-path Tag by design.
  - Related: `issue009`, `task020`, follow-up to `ba78c7f`

- 2026-08-03 Fix
  - Files: `supertag-ops-tag.el`, `test/tag-path-test.el`, task019 phase/docs
  - Functions: `supertag-tag-affixate-candidates`
  - Changes: ordinary candidates now return an empty suffix string instead of `nil`, satisfying Corfu's three-string affixation contract.
  - Simplification: one data-shape correction in the shared producer; no Corfu advice, UI-specific branch or new abstraction.
  - Verification: red/green focused ERT 19/19; full ERT 351/351; live `corfu--format-candidates` accepted both `("ta" "" "  [New]")` and `("task" "prj/" "")`; byte compile, paren/diff checks passed with no repository `.elc`.
  - Risk: none beyond the existing live visual acceptance gate.
  - Related: `issue009`, `task019`, follow-up to `aa74a24`

- 2026-08-03 Fix
  - Files: `supertag-core-tag-path.el`, `supertag-ops-tag.el`, `supertag-services-ui.el`, `supertag-ui-completion.el`, `supertag-view-schema.el`, `test/tag-path-test.el`, nested-tag phase/docs
  - Functions: Tag display path/affixation, shared Tag reader, CAPF candidates, Schema tree and Child command
  - Changes: typing `happy` now displays `diary/happy` while preserving the real ID; Schema indents `happy` under its `:extends` parent and uses `/` only as a legacy fallback; `a n` and compatibility key `a c` share one Child command.
  - Simplification: removed namespace-step CAPF navigation, the shared reader loop, the separate path-child creator and the `child -> parent` line suffix; no Store migration, cache, index or dependency.
  - Verification: focused ERT 19/19, full ERT 351/351, live Store CAPF/Schema probe, byte compilation, `check-parens`, `git diff --check`; no repository `.elc` remains.
  - Data cleanup: verified `diary/happy` had no source/node/schema references, backed up the DB to `/private/tmp/supertag-db.el.before-diary-slash-happy-cleanup-20260803`, then deleted the orphan; notes sync commit `b7bfdfe` is on `origin/main`.
  - Risk: existing full-path Tag IDs remain supported and are not automatically converted to `:extends`.
  - Related: `issue009`, `task018`; supersedes the task017 completion interaction.

- 2026-08-03 Fix
  - Files: `supertag-ui-completion.el`, `test/tag-path-test.el`, nested-tag phase/docs indexes
  - Functions: `supertag-completion--get-completion-table`
  - Changes: an exact existing flat Tag now offers a slash-terminated child namespace, so `#diary` can navigate to `diary/` before any child Tag exists.
  - Simplification: reused the existing CAPF candidate and namespace-property path; no Store write, entity, cache, index or dependency.
  - Verification: red/green real CAPF regression for basic and Corfu/orderless enumeration; focused ERT 20/20, full ERT 352/352, byte compilation, paren/diff checks; generated `.elc` removed.
  - Risk: live Corfu display remains the user acceptance gate; issue009 stays open.
  - Related: `issue009`, `task017`

- 2026-08-02 Fix
  - Files: `supertag-view-helper.el`, `supertag-view-svg-tag.el`, `test/view-framework-test.el`, `test/test-inline-tag-filter.el`, phase/docs indexes
  - Functions: `supertag-view-helper--font-lock-matcher`, face/SVG font-lock keywords, inline Tag point lookup
  - Changes: font-lock now receives the range-aware `#outer` span directly from its matcher, so adjacent Org link text receives neither the Tag face nor SVG display property; point lookup reuses the same matcher.
  - Simplification: removed the predicate-side match-data mutation and both duplicate wide regex keywords; no cleanup, Store or matcher-core changes.
  - Verification: two real `font-lock-ensure` property tests red/green; focused View 16/16, Smart Key 12/12, full ERT 351/351, inline self-check, byte compilation, paren/diff checks.
  - Risk: issue030 remains open for user review and real rescan/candidate preview; no database cleanup was run.
  - Related: `issue030`, `task016`, supersedes frontend extent claims in `a9257518`

- 2026-08-01 Harden
  - Files: `supertag-core-transform.el`, `supertag-services-sync.el`, `supertag-view-helper.el`, `supertag-ops-tag.el`, focused tests and phase/docs indexes
  - Functions: shared range-aware inline Tag matcher, explicit-candidate reference scan, post-hook batch validation, rollback hook runner
  - Changes: Sync/face/SVG/Smart Key now truncate adjacent Org objects to the same Tag ID; orphan cleanup validates the original candidate set after all after-hooks; rollback runs every invariant before rethrowing the first hook error.
  - Simplification: moved the existing Sync matcher to core and reused it; used native `run-hook-wrapped`; added no cache, Store field or dependency.
  - Verification: four latest user reproductions passed; focused ERT 77/77, full ERT 349/349, inline self-check, 1000-line font-lock smoke, byte compilation, package load, paren/diff checks and independent review.
  - Risk: no real database cleanup was run; full rescan and user review of candidates remain required before issue030 can close.
  - Related: `issue030`, `task016`, supersedes cleanup safety claims in `a18e6d8`

- 2026-08-01 Harden
  - Files: `supertag-services-sync.el`, `supertag-ops-tag.el`, `supertag-core-store.el`, `supertag-core-transform.el`, `supertag-ops-schema.el`, `supertag-query-library.el`, focused tests and phase/docs indexes
  - Functions: inline object range matching, Store/query/view public enumeration, guarded Tag deletion, transaction rollback hook
  - Changes: Tag matches stop at parsed Org objects without losing underscore subscript text; schema field definitions participate in orphan references; stale previews are checked both before the batch and after each operation hook; rollback rebuilds the resolved schema cache.
  - Simplification: replaced per-character Org context probing with one parsed range list per headline/paragraph; Tag Ops no longer reads the Store root or View registry private variable and no longer parses saved-query serialization itself.
  - Verification: four reported safety reproductions plus underscore+nested-link regression passed; focused ERT 42/42, full ERT 344/344, real note/Store read-only probes, 1000-Tag benchmark, byte compilation, paren/diff checks and two independent reviews passed.
  - Risk: cleanup remains deliberately conservative and still requires full rescan, explicit selection and confirmation; no user database deletion was performed.
  - Related: `issue030`, `task016`, supersedes cleanup safety claims in `188a106`

- 2026-08-01 Fix
  - Files: `supertag-services-sync.el`, `supertag-ops-tag.el`, `supertag-ui-commands.el`, `test/run-tests.sh`, focused tests and phase/docs indexes
  - Functions: `supertag--inline-tag-matches-in-region`, `supertag--extract-inline-tags`, `supertag--strip-inline-tags`, `supertag-tag-orphaned-ids`, `supertag-tag-delete-orphans`, `supertag-cleanup-orphaned-tags`
  - Changes: sync now reads Tag identity from raw Org buffer spans, so Org subscript parsing cannot rewrite underscores; the cleanup command loads saved-query configuration, lists only conservatively unreferenced Tag entities, requires explicit selection and confirmation, rechecks references, and never edits Org files; the test runner prefers newer source over stale ignored byte-code.
  - Simplification: removed the lossy AST-to-sentinel text conversion; reused the existing Tag reader, Store collections and transaction boundary without a migration, cache or new dependency.
  - Verification: red/green underscore fixture; focused ERT 35/35; full ERT 337/337; real-note parse preserved `ai_suggestions` and `smart_companion`; real Store preview was read-only; self-checks, byte compilation, paren/diff checks and independent review passed.
  - Risk: old polluted entities remain until a full rescan makes them orphaned and the user explicitly selects them for deletion.
  - Related: `issue030`, `task016`

- 2026-08-01 Modify
  - Files: nested Tag path/completion/UI readers, command/capture/query/view selectors, tests and phase/docs indexes
  - Functions: `supertag-tag-path-direct-candidates`, `supertag-ui-read-tag`, `supertag-ui-read-tags`, CAPF completion table
  - Changes: nested Tag candidates now advance one namespace level at a time; shared readers cover the primary Tag input surfaces; only new paths carry a type annotation.
  - Simplification: reused canonical full paths and the existing completion APIs; no persisted namespace state or index.
  - Verification: focused ERT 19/19, related ERT 54/54, full ERT 335/335, self-checks, byte compilation, paren/diff checks and independent review.
  - Risk: live Corfu theme/display confirmation remains the final user acceptance gate.
  - Related: `issue009`, `task015`

- 2026-07-30 Fix
  - Files: `supertag-view-helper.el`, `test/view-framework-test.el`, phase/docs indexes
  - Functions: `supertag-view-helper--enable-existing-org-buffers`
  - Changes:
    - Late loading now applies the existing auto-enable path to Org buffers that were already open.
    - After the SVG module finishes loading, active style buffers replace the temporary face keywords
      with the SVG keyword set.
    - `supertag-view-style-auto-enable=nil` and non-Org buffers remain untouched.
  - Simplification:
    - Reused the existing mode enable and SVG refresh functions; no timer, advice or configuration workaround.
  - Verification:
    - Focused red/green view ERT: 14/14 passed; full stable ERT: 331/331 passed.
    - Late-load smoke: `mode=t, face=nil, display-type=image`.
    - Changed files passed `check-parens`, temp-directory byte compilation and `git diff --check`;
      no repository `.elc` was generated.
  - Risk: live GUI startup/session-restore confirmation remains pending.
  - Related: `issue029`, `task014`

- 2026-07-29 Add/Modify
  - Files: `supertag-core-tag-path.el`, `supertag-core-scan.el`,
    `supertag-services-sync.el`, `supertag-ops-tag.el`,
    `supertag-ops-tag-merge.el`, `supertag-ui-completion.el`,
    `supertag-view-api.el`, `supertag-view-framework.el`,
    `supertag-view-helper.el`, `supertag-view-schema.el`,
    `supertag-view-table.el`, `test/tag-path-test.el`, phase/docs indexes
  - Functions: path validation/parent/leaf/rebase, authoritative node-tag
    reconciliation, Schema namespace tree, descendant View/Table queries,
    transactional namespace branch rename
  - Changes:
    - Complete slash paths remain canonical Tag IDs; missing parents are derived
      virtual namespaces and never become Tag entities or `:extends` links.
    - Single-node and full synchronization now create the same Tag entities and
      reconcile stale node-tag relations from the current authoritative tag set.
    - Schema, completion, custom View and Table preserve namespace scope; aggregate
      tables expose only common columns and reject schema/field edits.
    - Branch rename preflights collisions, migrates descendants and exact structured
      references, snapshots Org files, and rolls back Store/files on failure.
    - Exact text deletion/rename uses the shared full-token matcher, so `#a` cannot
      truncate `#a/b`; only `:tag` field values migrate during a branch rename.
  - Simplification:
    - No parent entities, Store schema migration, prefix index, duplicated raw path,
      or second inheritance system.
  - Verification:
    - Focused nested-path ERT: 15/15; full stable ERT: 330/330.
    - Completion self-check and Table ERT passed; 12 changed files passed
      `check-parens`, byte compilation and `git diff --check`.
    - Read-only copy of the current 101-tag/1554-node vault preserved its SHA-1;
      `coding` exact=0, descendants=1, and Schema derived `coding/` → `日志`.
    - Schema screenshot visual verdict: 92/100, pass.
  - Risk:
    - Descendant Table remains deliberately read-only until each row can carry an
      unambiguous tag-specific schema context.
    - Native `lazy-convert` file mutation remains tracked by issue022.
  - Related: `issue009`, `issue022`, `task013`

- 2026-07-29 Plan
  - Files: nested-tag issue/plan/task docs
  - Changes:
    - Reclassified task012 as the descendant-query foundation rather than complete nested-tag support.
    - Added task013 to cover sync consistency, namespace navigation, completion, aggregate views and safe branch rename.
    - Locked the boundary between path namespace and explicit `:extends` field inheritance.
  - Related: `issue009`, `task013`

- 2026-07-29 Add
  - Files: `supertag-core-scan.el`, `supertag-view-api.el`, `test/tag-path-test.el`,
    `test/run-tests.sh`, nested-tag decision/issue docs, phase docs
  - Functions: `supertag-find-tag-descendants`, `supertag-index-get-nodes-by-tag`,
    `supertag-find-nodes-by-tag`, `supertag-view-api-list-entity-ids`,
    `supertag-view-api-nodes-by-tag`
  - Changes:
    - Preserved complete path strings such as `emacs/package` as canonical Tag IDs.
    - Added opt-in descendant matching with `/` segment boundaries while keeping exact queries unchanged.
    - Kept namespace containment separate from explicit `:extends` schema inheritance.
  - Simplification:
    - Rejected leaf-only storage, `:raw-tag-paths`, parent entity writes and a prefix index.
  - Verification:
    - Focused red/green ERT: 4/4 passed; full stable ERT: 319/319 passed.
    - 10k-node benchmark: about 2.65ms/exact query and 24.86ms/descendant query.
  - Related: `issue009`, `task012`

- 2026-07-28 Fix
  - Files: `supertag-core-transform.el`, `supertag-view-helper.el`, `supertag-ui-completion.el`,
    `supertag-view-svg-tag.el`, `test/test-inline-tag-filter.el`, phase docs
  - Functions: `supertag-transform-inline-tag-name-p`,
    `supertag-transform-extract-inline-tags`, `supertag-view-helper--valid-inline-tag-match-p`,
    `supertag-completion--get-all-tags`, `supertag-svg-tag--get-cached`
  - Changes:
    - Added one shared tag-name rule that treats Emacs Lisp `#'function` as syntax rather than an inline tag.
    - Applied the rule to rendering, sync extraction and completion, so historical function-quote artifacts
      are hidden without deleting Store data.
    - Reduced the default SVG font scale from `0.78` to `0.68` and included the scale in the image cache key.
  - Compatibility:
    - Existing Unicode, emoji, hierarchy, `C++` and punctuation-bearing tags remain accepted.
    - A deliberately apostrophe-prefixed tag is now hidden because it conflicts with Emacs Lisp function-quote syntax.
  - Verification:
    - Focused red/green self-check reproduces and rejects the three reported `#'zettel-*` artifacts.
    - Focused extractor/Smart Key ERT: 30/30 passed; full stable ERT: 314/314 passed.
    - Main package batch load, changed-file `check-parens`, byte compilation and `git diff --check` passed.
    - Generated 16px-before/14px-after SVG comparison received a 93/100 visual verdict.
  - Related: `issue026`, `task011`

- 2026-07-26 Optimize
  - Files: `.github/workflows/test.yml`, phase docs
  - Changes:
    - Restricted push-triggered CI to `main`; pull requests keep the compatibility gate.
    - Ignored Markdown-only and `.phrase/**`-only changes for push and pull-request triggers.
    - Added `workflow_dispatch` for manual compatibility runs.
    - Uploads `test-results.txt` only after failure instead of on every successful matrix job.
  - Verification:
    - Local YAML parse and `git diff --check` passed.
    - Emacs 29.1/29.4 CI passed in run 30166407299.
    - Successful jobs skipped artifact upload as intended.
  - Related: `task010`

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
