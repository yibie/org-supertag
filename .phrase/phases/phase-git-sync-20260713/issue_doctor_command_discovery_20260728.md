# issue025 [x] 直接加载源码时 M-x 找不到 supertag-doctor

## Environment

- 源码 checkout 直接加入 `load-path`
- package-generated autoload 未生成或未加载
- 仓库曾保留早于源码的本地 `org-supertag.elc`

## Repro / Actual

加载 `org-supertag` 后，`M-x` 搜不到 README 公开的 `supertag-doctor`。命令定义
带 `;;;###autoload`，但主入口没有 runtime autoload；而 Emacs 默认还会优先加载
陈旧 `.elc`，进一步隐藏源码修复。

## Expected

直接加载源码后即可从 `M-x` 发现 Doctor，同时保持 `supertag-doctor.el` 按需加载；
回归测试必须验证最新源码，不能被旧 byte-code 假通过或假失败。

## Investigation / Root Cause

这是 issue024 的同类遗漏：菜单 wrapper 会主动 `require` Doctor，但不能把真实
命令注册进 `M-x`。原隔离测试仅覆盖 Git 三命令，并且没有设置
`load-prefer-newer`，所以仓库内被忽略的旧 `.elc` 能覆盖新源码。

## Fix / Verification

- 主入口为 `supertag-doctor` 增加显式 runtime autoload。
- 隔离 `emacs -Q` 回归增加 Doctor，并确认 Doctor/Git feature 均未提前加载。
- 测试子进程设置 `load-prefer-newer`。
- Git 36/36、默认全量 314/314；删除仓库全部 6 个 `.elc` 后 Git 再次 36/36；
  临时目录 byte-compile 与 `git diff --check` 通过。

关联任务：task004。

- User Confirmation: 2026-07-28，用户在真实笔记 vault 中运行 Doctor，确认 Git
  仓库、DB 路径、merge driver、attributes、tracked remote、sync mode、pending
  push 与 text conflicts 全部正常。
- Resolved At: 2026-07-28
- Resolved By: runtime autoload 修复与真实 Doctor/Git Sync 健康检查
- Resolved Commit: `9114288`
