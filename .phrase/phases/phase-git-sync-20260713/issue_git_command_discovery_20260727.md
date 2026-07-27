# issue024 [x] 直接加载源码时 M-x 找不到 Git 同步公开命令

## Environment

- 源码 checkout 直接加入 `load-path` 并加载 `org-supertag`
- 没有生成或加载 package autoload 文件
- 用户在 Git 同步人工演练中执行 `M-x supertag-git-setup`

## Repro / Actual

加载 `org-supertag` 后，`M-x` 搜不到 `supertag-git-setup`。命令虽在
`supertag-git.el` 中带有 `;;;###autoload`，入口文件没有实际的 runtime autoload；
只有安装流程生成并加载 autoload 文件时，cookie 才会生效。

## Expected

README 公开的 setup、clone、sync-mode 命令在直接加载源码和标准 package 安装中
都能由 `M-x` 发现；发现命令不应提前加载整个 Git 模块。

## Investigation / Root Cause

`org-supertag.el` 已为 graph/board 等可选模块显式注册 runtime autoload，却漏掉
`supertag-git.el`。菜单 wrapper 能延迟 `require` Git 模块，但不能让 README 中
直接公开的命令名进入 `M-x` 命令表。

## Fix / Verification

- `org-supertag.el` 为 `supertag-git-setup`、`supertag-git-clone`、
  `supertag-git-sync-mode` 增加显式 runtime autoload。
- 隔离 `emacs -Q` 回归验证三者都是可交互 autoload，并确认
  `supertag-git` 仍未加载。
- Git 子套件 36/36；默认全量 314/314；batch byte-compile 与
  `git diff --check` 通过。

关联任务：task003。

- User Confirmation: 2026-07-28，用户在真实笔记仓库中重新执行 setup，并确认首次
  push 成功。
- Resolved At: 2026-07-28
- Resolved By: runtime autoload 修复与真实 Git setup/push 演练
- Resolved Commit: `6fefbad`
