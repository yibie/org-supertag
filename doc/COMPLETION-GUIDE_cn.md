# Org-Supertag 标签补全指南

## 功能概述

Org-Supertag 提供了强大的标签补全功能，让你可以通过输入 `#` 来快速添加标签到 org 节点。

## 使用方法

### 基本用法

1. 在 org-mode 文件中的任意位置输入 `#`
2. 补全界面会自动弹出，显示所有可用的标签
3. 选择一个标签或输入新标签名称
4. 按 `RET` 或 `TAB` 完成补全

### 特性

- **自动补全**：输入 `#` 后自动触发
- **智能过滤**：已添加到当前节点的标签不会再次显示
- **创建新标签**：如果输入的标签不存在，会显示 `[Create New Tag]` 选项
- **前缀匹配**：输入 `#proj` 会过滤出所有以 `proj` 开头的标签

### 支持的标签字符

标签名称可以包含：
- 字母（a-z, A-Z）
- 数字（0-9）
- 下划线（_）
- 连字符（-）
- @ 符号
- % 符号

注意：`#` 是触发符，不是标签名称的一部分。

## 启用和配置

### 自动启用

Org-Supertag 会在初始化时自动启用补全功能。如果你的配置中包含：

```elisp
(require 'org-supertag)
```

那么补全功能会自动在所有 org-mode 缓冲区中启用。

### 手动启用

如果需要在特定缓冲区中手动启用：

```elisp
M-x supertag-ui-completion-mode
```

### 全局启用/禁用

```elisp
;; 启用
M-x global-supertag-ui-completion-mode

;; 禁用
M-x global-supertag-ui-completion-mode (再次执行以切换)
```

### 配置选项

```elisp
;; 自动启用补全（默认：t）
(setq supertag-completion-auto-enable t)
```

## 故障排除

### 命令消失或无法找到

如果 `M-x supertag-ui-completion-mode` 命令不存在，可能是字节编译缓存问题：

1. **删除旧的编译文件**：
   ```bash
   cd /path/to/org-supertag
   rm -f *.elc
   ```

2. **或使用提供的脚本**：
   ```bash
   ./recompile.sh
   ```

3. **重启 Emacs** 或重新加载：
   ```elisp
   M-x load-file RET org-supertag.el RET
   ```

### 输入 # 后没有反应

1. **检查模式是否启用**：
   ```elisp
   M-x test-supertag-completion-debug
   ```

2. **手动启用补全模式**：
   ```elisp
   M-x supertag-ui-completion-mode
   ```

3. **重新加载模块**：
   ```elisp
   M-x load-file RET supertag-ui-completion.el RET
   ```

4. **检查补全框架**：
   - 确保你安装了 company-mode 或 corfu
   - 尝试手动触发：`M-x completion-at-point` 或 `M-TAB`

### 调试工具

使用内置的调试工具检查状态：

```elisp
;; 加载调试脚本
M-x load-file RET test-completion-debug.el RET

;; 运行诊断
M-x test-supertag-completion-debug

;; 强制启用（如果自动启用失败）
M-x test-supertag-completion-force-enable
```

## 与补全框架集成

### Company Mode

```elisp
(use-package company
  :hook (org-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))
```

### Corfu

```elisp
(use-package corfu
  :hook (org-mode . corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1))
```

## 技术细节

### 实现原理

补全功能基于 Emacs 的 `completion-at-point-functions` (CAPF) 机制：

1. 检测 `#` 触发符
2. 从 supertag 存储中获取所有标签
3. 过滤掉已添加的标签
4. 提供补全候选列表
5. 选择后自动添加标签到节点

### 依赖模块

- `supertag-core-store`：标签存储
- `supertag-ops-node`：节点操作
- `supertag-ops-tag`：标签操作

## 常见问题

**Q: 为什么有些标签不显示？**  
A: 已经添加到当前节点的标签会被自动过滤，避免重复添加。

**Q: 如何创建新标签？**  
A: 输入不存在的标签名称，选择 `[Create New Tag]` 选项即可。

**Q: 补全速度慢怎么办？**  
A: 如果标签数量很大（>1000），可以考虑调整补全框架的延迟设置。

**Q: 可以自定义触发符吗？**  
A: 目前固定为 `#`，如需修改需要编辑 `supertag-ui-completion.el`。

## 开发者注意事项

### 修改代码后

如果你修改了 org-supertag 的源代码：

1. **删除字节编译缓存**：
   ```bash
   rm -f *.elc
   ```

2. **重新加载**：
   ```elisp
   M-x load-file RET org-supertag.el RET
   ```

3. **或重新编译**（可选，用于性能）：
   ```bash
   ./recompile.sh
   ```

### 为什么需要删除 .elc 文件？

Emacs 会优先加载 `.elc`（字节编译）文件而不是 `.el`（源代码）文件。如果你修改了源代码但没有删除旧的 `.elc` 文件，Emacs 会继续使用旧的编译版本，导致你的修改不生效。

## 更新日志

### v5.0.0
- 重写补全系统，使用标准 CAPF 接口
- 支持所有主流补全框架
- 改进标签字符集识别
- 添加调试工具
- 修复自动启用问题
- 修复字节编译缓存导致的命令消失问题
- 添加 recompile.sh 脚本