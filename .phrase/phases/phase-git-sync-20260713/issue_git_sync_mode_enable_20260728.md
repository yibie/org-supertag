# issue027 [x] Git sync 启用提示路径歧义且启动后未自动启用

## Environment

- Git vault：`org-directory/.supertag/`
- `supertag-git-sync-mode` 已完成人工 setup，可手动启用
- Nova 配置通过 idle timer 延迟加载 Org-Supertag

## Repro / Actual

手动启用后消息显示 `enabled for /.../notes/.`，看起来 repo root 多了一个点；
重新启动 Emacs 后还需再次手动启用模式。

## Expected

消息明确显示实际 repo root `/.../notes/`；完成 setup 的用户可在配置中用一次
显式启用，让每次加载插件时自动启动同步。

## Investigation / Root Cause

内部 root 已经是正确的尾斜杠目录。多出的点不是路径数据，而是
`"enabled for %s."` 的句号与目录尾斜杠相邻。模式本身是普通 global minor mode，
不会因为上一次会话开启而自动持久化。

## Fix / Verification

- 删除启用提示中路径后紧邻的句号，不改 repo root 数据。
- 在 Nova 既有 idle-load 块中调用 `(supertag-git-sync-mode 1)`；显式参数 `1`
  始终表示开启，重复求值不会反向关闭。
- Git 36/36、默认全量 314/314、Nova 配置 `check-parens`、`git diff --check`。

关联任务：task005。

- Resolved At: 2026-07-28
- Resolved By: 提示文本消歧与启动配置显式启用
- Resolved Commit: `051d985`
