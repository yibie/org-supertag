# Git 原生同步任务

- task001 [x] 修复 P0 复审发现的 merge shape/order/conflict-id、migration guard、
  unmerged Org 可见性和 Git index 所有权漏洞；验证：新增反例 ERT、git 组测试、
  `./test/run-tests.sh all` 245/245、`ACCEPTANCE.md` 三项真实 Git 场景全 PASS。
  关联：issue013、PLAN.md「P0 复审门槛」；提交：`20cb4b5`、`849206a`。

- task002 [x] 修复快照恢复的降级迁移、恢复点、锁与旧格式摘要安全缺陷，并把恢复
  回归接入默认 CI；验证：restore 13/13、persist 31/31、all 313/313 与 batch
  byte-compile（仅既有 core warning）。关联：issue023、PLAN.md「恢复安全复审补充」。
