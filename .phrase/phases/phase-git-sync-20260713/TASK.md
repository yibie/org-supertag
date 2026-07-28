# Git 原生同步任务

- task001 [x] 修复 P0 复审发现的 merge shape/order/conflict-id、migration guard、
  unmerged Org 可见性和 Git index 所有权漏洞；验证：新增反例 ERT、git 组测试、
  `./test/run-tests.sh all` 245/245、`ACCEPTANCE.md` 三项真实 Git 场景全 PASS。
  关联：issue013、PLAN.md「P0 复审门槛」；提交：`20cb4b5`、`849206a`。

- task002 [x] 修复快照恢复的降级迁移、恢复点、锁与旧格式摘要安全缺陷，并把恢复
  回归接入默认 CI；验证：restore 13/13、persist 31/31、all 313/313 与 batch
  byte-compile（仅既有 core warning）。关联：issue023、PLAN.md「恢复安全复审补充」。

- task003 [x] 让直接加载源码的安装也能从 `M-x` 发现 Git setup/clone/sync-mode
  三个公开命令；验证：隔离 `emacs -Q` 命令发现回归、Git 36/36、all 314/314
  与 batch byte-compile。关联：issue024、PLAN.md「S4 用户旅程」。

- task004 [x] 补齐 README 公开的 `supertag-doctor` runtime autoload，并让隔离
  命令发现测试强制加载新源码而非陈旧 `.elc`；验证：Git 36/36、all 314/314、
  无 `.elc` Git 36/36 与临时目录 byte-compile。关联：issue025。

- task005 [x] 消除 Git sync 启用提示中目录尾斜杠与句号拼成 `/.` 的歧义，并在
  Nova 的既有 idle-load 块中显式启用 `supertag-git-sync-mode`；验证：Git
  36/36、all 314/314、配置 `check-parens`。关联：issue027；提交：`051d985`。

- task006 [x] 增加公开的立即同步命令和正常退出前的同步查询保护；验证：真实临时
  Git remote 覆盖 working tree → commit/push、clean exit、保留本地退出与 mode
  hook 清理，Git 37/37、默认全量 315/315 与临时目录 byte-compile。关联：
  issue028；提交：`015db5a`。
