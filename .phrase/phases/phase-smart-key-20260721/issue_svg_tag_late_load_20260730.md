# issue029 — 延迟加载后现存 Org buffer 不显示 SVG tag

## Environment

- Nova Emacs 在 idle timer 中延迟执行 `(require 'org-supertag)`
- 启动恢复的 Org buffer 早于 org-supertag 加载
- `supertag-view-style-auto-enable` 与 `supertag-svg-tag-enable` 均为非 nil

## Reproduction

1. 创建并进入一个 Org buffer。
2. 随后加载 `supertag-view-helper` / `org-supertag`。
3. 在正文输入合法 `#tag` 并触发 font-lock。

## Expected vs actual

- Expected: 已存在和以后创建的 Org buffer 都自动启用 `supertag-view-style-mode`。
- Actual: helper 只注册 `org-mode-hook`；已运行过 hook 的 buffer 保持 mode 关闭，没有 face 或 SVG `display` 属性。

## Investigation and root cause

批处理复现得到 `mode=nil, face=nil, display=nil`。同一 Emacs 报告 SVG image type 可用；
先加载 helper 再创建 Org buffer 时 mode 正常开启，强制图形显示路径也能产生 SVG `display`
属性。根因是 late-load 生命周期缺口，不是 SVG 生成、匹配边界或 Emacs 图像能力。

## Fix

- helper 注册 `org-mode-hook` 后，复用同一 auto-enable 函数扫描一次现存 buffer。
- 仅处理派生自 `org-mode` 的 buffer，并继续尊重 `supertag-view-style-auto-enable`。

## Verification

- 回归测试在修复前以 `void-function supertag-view-helper--enable-existing-org-buffers` 失败。
- Focused view ERT: 14/14 passed；full stable ERT: 331/331 passed。
- 真实 late-load batch smoke 得到 `mode=t, face=nil, display-type=image`。
- Changed files passed `check-parens`、临时目录 byte compile 与 `git diff --check`；
  仓库内没有生成 `.elc`。
- Live-buffer confirmation: pending.

## Tracking

- Task: `task014`
- User confirmation: pending
- Resolved At/By/Commit: pending
