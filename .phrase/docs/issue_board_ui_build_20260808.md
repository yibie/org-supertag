# issue036: Board UI hooks 被根级 ignore 规则误伤

## Summary

仓库的 agent scratch ignore 使用未锚定的 `hooks/` 等目录名，导致
`ext/board-ui/hooks/useWebSocket.ts` 未被跟踪，clean clone 的 Board UI 构建失败。

## Environment

- GitHub PR #185
- Node.js 20
- Next.js 13.5.6

## Reproduction

1. 从 clean clone 在 `ext/board-ui` 执行 `npm ci`。
2. 执行 `npm run build`。
3. `Toolbar.tsx` 无法解析 `../hooks/useWebSocket`。

## Root Cause

`.gitignore` 中的 `hooks/`、`commands/` 等 agent scratch 目录规则没有以仓库根
锚定，因此 Git 会忽略任意深度的同名源码目录。例外规则只放行了目录本身，没有放行
目录内文件。

## Task

- task001 [x] 将 agent scratch 目录规则限制到仓库根，跟踪现有
  `useWebSocket.ts`，并在 CI 中锁定 ignore 边界与 clean Board UI 构建。

## Fix

- 将 agent scratch 目录统一改为根级规则，并删除重复规则与无效例外。
- 跟踪 Board UI 已使用的 `useWebSocket.ts`。
- CI 增加 ignore 边界检查、`npm ci` 和 `npm run build`。

## Verification

- 根级 scratch 目录仍被忽略，嵌套同名源码目录不再被忽略。
- `ext/board-ui/hooks/useWebSocket.ts` 可被 Git 跟踪。
- clean `npm ci && npm run build` 通过。
- 全量 ERT 400/400 通过。

## Change

- **Date**: 2026-08-08
- **Files**: `.gitignore`, `.github/workflows/test.yml`,
  `ext/board-ui/hooks/useWebSocket.ts`
- **Behavior**: clean clone 可以构建 Board UI；CI 会阻止同类 ignore 回归。
- **Scope**: 未加入无调用者的 `useShiftKey.ts`，未改动 WebSocket 端口策略。

## Resolution

- **Resolved At**: 2026-08-08
- **Resolved By**: Codex
- **Commit**: same commit as this record
