# plan_smart_key_20260721

## Milestones

1. 用 focused ERT 锁定 context 优先级、已有交互原语、inline tag 过滤、heading 与 Assist 行为。
2. 新增一个小 Module，以 `supertag-smart-key` 为主入口，内部完成 target 识别和默认动作分派。
3. 接入主包与稳定测试入口，运行 focused/full batch tests 和静态检查。
4. 回写 task、change 与全局 CHANGE 索引。
5. 将前缀调用从全局菜单占位升级为对象级 Assist；保留无 target 时的全局菜单回落。
6. 保持 Node View 身份解析只读；无 ID heading 不进入任何隐式创建路径。
7. 修复显式 Node 退化：Store 删除成功后移除 Org ID，并保留其他 Org 属性。
8. 收紧共享 inline tag validator：Org link target 中的 `#fragment` 不参与 face/SVG 渲染或 point 识别。
9. 用“空白分隔的正文 token”统一渲染边界，覆盖 inline object、drawer、COMMENT subtree、block 与嵌入式 hash。
10. 将同步提取对齐同一边界，并限制到当前 headline 的标题和自身正文 section。
11. 保留最低/稳定 Emacs 矩阵，但将 CI 限制到 main、PR 的非文档变更及手动触发。
12. 排除 Emacs Lisp `#'function` 引用，过滤 completion 中的同类历史污染，并缩小 SVG tag 默认字号。

## Scope

- 代码：`supertag-smart-key.el`、`supertag-view-node.el`、`supertag-view-helper.el`、
  `supertag-ui-commands.el`、`supertag-ui-completion.el`、`supertag-core-transform.el`、
  `supertag-services-sync.el`、`supertag-view-svg-tag.el`、`org-supertag.el`。
- 测试：`test/test-smart-key.el`、`test/test-inline-tag-filter.el`、`test/extractor-test.el`、`test/run-tests.sh`。
- CI：`.github/workflows/test.yml`。
- 文档：本 phase 文档与 `.phrase/docs/CHANGE.md`。

## Priorities

- P0: recognizer 无持久化副作用，不覆盖既有按键。
- P0: 具体语义属性优先，Org link 不被 inline tag 遮蔽。
- P0: Org link target 的 `#fragment` 不获得 inline tag 的 face 或 SVG `display` 属性。
- P0: 只渲染行首/空白后的正文 `#token`；tag 名仍允许中文、emoji、`/` 与非空白标点。
- P0: 同步不得从 Org object、元数据、COMMENT subtree 或子 headline 提取 inline tag。
- P0: `#'function` 不得渲染、同步或出现在 tag completion；历史 Store 不做破坏性删除。
- P1: SVG tag 默认字体小于正文行高，缓存键包含字号比例。
- P1: 复用既有命令，不复制 Ops 或 View 实现。
- P2: 插件注册、上下文 Assist 与 Hyperbole Adapter 留到真实调用方出现后再做。
- P2: Hyperbole Adapter 与第三方动作注册继续后置；对象级 Assist 只复用已有命令。

## Risks & Dependencies

- 现有 `supertag-context` 同名异形，归一化错误会导致 Node/Schema View 动作错配。
- 现有 inline tag point helper 未应用 font-lock validator；需在共享 helper 处修正，避免 Smart Key 复制第二套规则。
- 原生 `:tag:` 只在全量重扫读取；本任务不得借机删除历史 tag 定义或关系，避免增量同步造成数据损失。
- 过滤 `#'function` 会隐藏此前误建的同名 tag；若用户确实需要以 `'` 开头的标签，需改用不冲突的名称。
- 当前分支领先远端且包含既有提交；提交时只 stage 本 phase 文件，推送前按仓库协议 rebase。

## Rollback

- 删除 `supertag-smart-key.el`、主包 wiring 与 focused test。
- 恢复 inline tag point helper 的旧实现。
- 恢复同步提取的旧字符串正则；Store schema 无需回滚。
- 删除本 phase 文档与 CHANGE 索引。
