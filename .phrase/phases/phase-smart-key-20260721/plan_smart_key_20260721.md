# plan_smart_key_20260721

## Milestones

1. 用 focused ERT 锁定 context 优先级、已有交互原语、inline tag 过滤、heading 与 Assist 行为。
2. 新增一个小 Module，以 `supertag-smart-key` 为主入口，内部完成 target 识别和默认动作分派。
3. 接入主包与稳定测试入口，运行 focused/full batch tests 和静态检查。
4. 回写 task、change 与全局 CHANGE 索引。
5. 将前缀调用从全局菜单占位升级为对象级 Assist；保留无 target 时的全局菜单回落。

## Scope

- 代码：`supertag-smart-key.el`、`supertag-view-helper.el`、`org-supertag.el`。
- 测试：`test/test-smart-key.el`、`test/run-tests.sh`。
- 文档：本 phase 文档与 `.phrase/docs/CHANGE.md`。

## Priorities

- P0: recognizer 无持久化副作用，不覆盖既有按键。
- P0: 具体语义属性优先，Org link 不被 inline tag 遮蔽。
- P1: 复用既有命令，不复制 Ops 或 View 实现。
- P2: 插件注册、上下文 Assist 与 Hyperbole Adapter 留到真实调用方出现后再做。
- P2: Hyperbole Adapter 与第三方动作注册继续后置；对象级 Assist 只复用已有命令。

## Risks & Dependencies

- 现有 `supertag-context` 同名异形，归一化错误会导致 Node/Schema View 动作错配。
- 现有 inline tag point helper 未应用 font-lock validator；需在共享 helper 处修正，避免 Smart Key 复制第二套规则。
- 当前分支领先远端且包含既有提交；提交时只 stage 本 phase 文件，推送前按仓库协议 rebase。

## Rollback

- 删除 `supertag-smart-key.el`、主包 wiring 与 focused test。
- 恢复 inline tag point helper 的旧实现。
- 删除本 phase 文档与 CHANGE 索引。
