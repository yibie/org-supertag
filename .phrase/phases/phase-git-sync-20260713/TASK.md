# Git 原生同步任务

- task001 [x] 修复 P0 复审发现的 merge shape/order/conflict-id、migration guard、
  unmerged Org 可见性和 Git index 所有权漏洞；验证：新增反例 ERT、git 组测试、
  `./test/run-tests.sh all` 245/245、`ACCEPTANCE.md` 三项真实 Git 场景全 PASS。
  关联：issue013、PLAN.md「P0 复审门槛」；提交：`20cb4b5`、`849206a`。
