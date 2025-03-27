# org-supertag-sim-epc

为 `org-supertag` 提供基于 EPC 的高性能语义功能。与标准版本相比，该实现使用 Python 常驻进程，大大提高了响应速度。

## 主要优势

- **显著提升性能**：使用 EPC 常驻 Python 进程，避免每次调用时启动新进程
- **降低资源消耗**：重用已加载的模型和向量
- **更快的响应速度**：适合频繁使用标签生成和实体提取的场景
- **更好的可靠性**：简化通信机制，减少错误和超时

## 安装要求

### Python 依赖

确保已安装以下 Python 包：

```bash
pip install epc sexpdata sentence-transformers
```

### Emacs 依赖

确保已安装 `epc` Emacs 包：

```elisp
(use-package epc
  :ensure t)
```

## 使用方法

### 启用 EPC 版本

在你的 Emacs 配置中加载并启用 EPC 版本：

```elisp
(require 'org-supertag-sim-epc)

;; 启用 EPC 版本（替换标准功能）
(org-supertag-sim-epc-enable)
```

### 禁用 EPC 版本

如果需要恢复为标准实现：

```elisp
(org-supertag-sim-epc-disable)
```

### 检查状态

查看 EPC 服务当前状态：

```elisp
M-x org-supertag-sim-epc-status
```

## 命令列表

| 命令                             | 功能                       |
|----------------------------------|----------------------------|
| `org-supertag-sim-epc-enable`    | 启用 EPC 版本              |
| `org-supertag-sim-epc-disable`   | 禁用 EPC 版本，恢复标准版  |
| `org-supertag-sim-epc-status`    | 显示 EPC 版本状态          |
| `org-supertag-sim-epc-start-server` | 手动启动 EPC 服务器     |
| `org-supertag-sim-epc-stop-server`  | 手动停止 EPC 服务器     |
| `org-supertag-sim-epc-restart-server` | 重启 EPC 服务器       |
| `org-supertag-sim-epc-test`      | 测试 EPC 功能是否正常      |

## 配置选项

```elisp
;; 设置 Python 解释器路径
(setq org-supertag-sim-epc-python-path "/path/to/python")

;; 设置超时时间（秒）
(setq org-supertag-sim-epc-request-timeout 60)
```

## 工作原理

`org-supertag-sim-epc` 使用 Emacs 的 EPC (Emacs RPC) 机制与 Python 进程通信。与标准版本中对每个请求启动新 Python 进程不同，EPC 版本保持一个常驻 Python 进程，保持模型和向量数据在内存中，显著提高了后续调用的速度。

通信使用 S 表达式，这是 Emacs 的原生数据格式，可以更高效地传输数据。

## 故障排除

### EPC 服务器无法启动

检查以下内容：

1. 确保已安装所需的 Python 包：`pip install epc sexpdata sentence-transformers`
2. 确保 `org-supertag-sim-epc-python-path` 设置正确
3. 检查日志文件 `simtag_epc.log` 获取详细错误信息

### 请求超时

如果遇到超时问题，可以增加超时时间：

```elisp
(setq org-supertag-sim-epc-request-timeout 120) ;; 设置为 120 秒
```

### 手动重启服务

如果服务表现异常，尝试重启：

```elisp
M-x org-supertag-sim-epc-restart-server
``` 