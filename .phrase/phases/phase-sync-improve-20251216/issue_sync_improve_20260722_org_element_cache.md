# issue016 [ ] embed 跨 buffer 写入使 Org element cache 失效

## Environment

- Emacs: 31.0.90
- Org: 9.8.6
- Trigger: 保存任意 Org 页面后继续编辑或移动光标

## Repro / Actual

`supertag-services-embed--render-node-to-file` 复用已打开的 Org buffer，在
`inhibit-modification-hooks=t` 下替换节点内容。缓存仍记录修改前的 buffer 大小与
元素位置；后续 `org-element-context`（常由 `org-appear--post-cmd` 调用）可能报告
`org-element--cache: Item parser` 或 `Emergency exit` 并重置缓存。

回归测试使用真实 render 函数，将含 1200 个 list item 的节点替换为一行。修复前：

```text
org-element--cache-last-buffer-size = 68453
buffer-size = 67
```

## Expected

embed 对已打开 Org buffer 的跨文件写入完成后，element cache 必须与当前文本一致；
不得依赖 Org 在后续命令中发现损坏并执行 emergency reset。

## Root Cause

为规避 Doom/`org-indent-mode` 修改 hook 冲突，embed 写入路径动态绑定了
`inhibit-modification-hooks`。这同时屏蔽了 Org cache 的 before/after change hooks，
但写入结束后没有显式调用 `org-element-cache-reset`。屏蔽范围还覆盖了
`save-buffer`，使全局 after-save 回调对其他 Org buffer 的刷新继承同一动态绑定。

两个 embed 保存回调又注册在全局 `after-save-hook`，所以单一写入缺陷表现为全局性：
保存一个页面可能破坏另一个已打开页面的 cache。

## Fix / Verification

- [x] 在必须屏蔽 modification hooks 的真实写入边界禁用旧 cache，并用
  `unwind-protect` 保证写入后执行 `org-element-cache-reset`。
- [x] 将屏蔽范围缩到 `delete-region`/`insert`，让 `save-buffer` 与全局保存回调在
  正常 modification hook 环境执行。
- [x] `./test/run-tests.sh embed`：2/2 通过。
- [x] 默认测试集 291/291、shell 语法检查与 touched-file byte compile 通过。

## User Confirmation

- [ ] 等待用户在真实编辑环境确认警告不再出现。

## Resolution Status

- Implementation completed: 2026-07-22
- Implemented By: Codex
- Commit: task019 Lore commit

## Related

- Task: task019
- Upstream precedent: Vulpea `4433949421f302edefb575896265f11a261275bf`
