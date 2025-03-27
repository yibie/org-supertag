# Org-SuperTag SimTag EPC 系统

## 简介

Org-SuperTag SimTag EPC 是一个基于 EPC (Emacs RPC) 机制的标签相似度和实体提取服务，为 Org-SuperTag 提供智能标签建议、实体提取和相似标签查找等功能。通过 EPC 机制，系统将复杂的 NLP 处理逻辑放在 Python 侧执行，同时保持与 Emacs 的高效通信。

## 架构概述

![EPC架构](https://i.imgur.com/4sVcjnk.png)

**核心架构组件:**

1. **Emacs 侧**: 
   - `org-supertag-sim-epc.el`: 提供 EPC 通信基础设施，负责启动、停止服务器和基本请求处理
   - `org-supertag-sim-advanced.el`: 提供高级功能，包括实体提取、批处理和标签建议等用户交互

2. **Python 侧**: 
   - `simtag_epc.py`: EPC 服务器主程序，接收和处理来自 Emacs 的请求
   - `simtag/`: 包含 NLP 模型和处理逻辑的 Python 模块

3. **通信机制**:
   - 基于 TCP 的 EPC 协议，使用 S-表达式传输数据
   - 支持同步和异步请求模式
   - 结构化的错误处理和响应格式

## 核心功能

### 实体提取

从文本中提取命名实体（如人名、地点、组织等），帮助自动识别文档的关键元素。

```elisp
;; 从选中区域提取实体
M-x org-supertag-sim-extract-from-region

;; 从当前标题提取实体
M-x org-supertag-sim-extract-from-current-heading
```

### 标签建议

基于文本内容自动生成相关标签建议，提高标签质量和一致性。

```elisp
;; 为当前缓冲区获取标签建议
M-x org-supertag-sim-suggest-for-buffer

;; 开启/关闭实时标签建议
M-x org-supertag-sim-toggle-live-suggestions
```

### 相似标签查找

找到与特定标签相似的其他标签，有助于发现关联主题和内容。

```elisp
;; 查找相似标签
M-x org-supertag-sim-find-similar-to-tag
```

### 批量处理

批量处理多个 Org 文件，自动提取实体并生成标签建议。

```elisp
;; 批量处理 Org 文件
M-x org-supertag-sim-batch-process-org-files
```

## 安装与配置

### 前提条件

- Emacs 26.1 或更高版本
- Python 3.7 或更高版本
- 必要的 Python 包: `torch`, `sentence-transformers`, `epc`

### 安装步骤

1. **安装 EPC 包**:
   ```elisp
   M-x package-install RET epc RET
   ```

2. **下载 Org-SuperTag SimTag**:
   ```bash
   git clone https://github.com/your-repo/org-supertag.git
   ```

3. **安装 Python 依赖**:
   ```bash
   pip install -r requirements.txt
   ```

4. **配置 Emacs**:
   ```elisp
   (add-to-list 'load-path "/path/to/org-supertag")
   (require 'org-supertag-sim-epc)
   (require 'org-supertag-sim-advanced)
   ```

### 自定义配置

可以通过以下变量自定义系统行为:

```elisp
;; 设置向量文件路径
(setq org-supertag-sim-epc-vector-file "/path/to/vector/file")

;; 调整标签建议阈值
(setq org-supertag-sim-suggest-threshold 0.7)

;; 设置最大建议数量
(setq org-supertag-sim-max-suggestions 10)

;; 禁用实时标签建议
(setq org-supertag-sim-disable-live-suggestions t)
```

## 工作原理详解

### EPC 通信流程

1. **初始化阶段**:
   - Emacs 启动 Python EPC 服务器进程
   - Python 服务器初始化并开始监听指定端口
   - Emacs 连接到 Python EPC 服务器
   - 双方建立连接并完成初始握手

2. **请求-响应流程**:
   ```
   Emacs                                Python
     |                                    |
     |---- 调用函数 (如extract_entities)---->|
     |                                    |
     |<----- 处理请求 + 返回结果 ------------|
     |                                    |
   ```

3. **异步请求处理**:
   - Emacs 发送异步请求，不阻塞主线程
   - 请求完成后，通过回调函数处理结果
   - 支持多个并发请求

### 错误处理机制

系统实现了全面的错误处理:
- 通信层错误处理：连接超时、连接断开等
- 应用层错误处理：无效参数、处理异常等
- 自动重连机制：定期心跳检测，故障恢复

## 高级用法

### 回调函数定制

可以通过提供自定义回调函数来处理异步请求结果:

```elisp
(org-supertag-sim-epc-extract-entities-async 
 "分析这段文本"
 (lambda (entities)
   ;; 自定义处理逻辑
   (message "找到 %d 个实体" (length entities))
   (dolist (entity entities)
     ;; 处理每个实体
     )))
```

### 批量处理大型项目

对于包含大量文件的项目，可以通过批量处理功能提高效率:

```elisp
;; 处理指定目录下的所有 Org 文件
(org-supertag-sim-batch-process-org-files
 (directory-files-recursively "~/projects/documentation/" "\\.org$"))
```

### 超时设置

可以根据服务器性能和请求复杂度调整超时设置:

```elisp
;; 设置请求超时时间（秒）
(setq org-supertag-sim-epc-request-timeout 60)
```

## 故障排除

### 常见问题

1. **连接失败**
   - 检查 Python 环境是否配置正确
   - 检查端口是否被占用
   - 尝试重新启动 EPC 服务器: `M-x org-supertag-sim-epc-restart-server`

2. **处理缓慢**
   - 检查系统资源使用情况
   - 考虑降低批处理大小: `(setq org-supertag-sim-batch-size 5)`

3. **Python 错误**
   - 查看 `*SimTag-EPC-Process*` 缓冲区获取详细错误信息
   - 确保所有依赖库已正确安装

### 调试工具

```elisp
;; 显示 EPC 进程缓冲区
M-x org-supertag-sim-epc-show-process-buffer

;; 检查服务器状态
M-x org-supertag-sim-epc-server-status

;; 测试 EPC 连接
M-x org-supertag-sim-epc-ping

;; 紧急重启
M-x org-supertag-sim-epc-emergency-restart
```

## 开发者指南

### 扩展 Python 端功能

要添加新的 Python 函数供 Emacs 调用:

1. 在 `simtag_epc.py` 中添加新函数:
   ```python
   @safe_execution
   def my_new_function(param1, param2):
       # 处理逻辑
       return format_response(data=result)
   
   # 注册函数
   server.register_function(my_new_function)
   ```

2. 在 Emacs 端添加对应的调用函数:
   ```elisp
   (defun org-supertag-sim-my-new-function (param1 param2 &optional callback)
     (interactive)
     (org-supertag-sim-epc-ensure-server)
     (if callback
         ;; 异步版本
         (deferred:$
           (epc:call-deferred org-supertag-sim-epc-manager 
                             'my_new_function 
                             (list param1 param2))
           (deferred:nextc it callback))
       ;; 同步版本
       (epc:call-sync org-supertag-sim-epc-manager 
                     'my_new_function 
                     (list param1 param2))))
   ```

### 修改响应处理

自定义响应处理逻辑:

```elisp
(org-supertag-sim-epc-get-tag-suggestions-async 
 "标签建议测试"
 5
 (lambda (suggestions)
   ;; 自定义排序
   (setq suggestions 
         (sort suggestions 
               (lambda (a b) 
                 (> (cdr (assoc "score" a)) 
                    (cdr (assoc "score" b))))))
   ;; 处理排序后的结果
   ))
```

## 参考资料

- [EPC: RPC Stack for Emacs Lisp](https://github.com/kiwanami/emacs-epc)
- [Sentence-Transformers](https://www.sbert.net/)
- [Org Mode Documentation](https://orgmode.org/manual/)

## 许可证

本项目采用 GPL-3.0 许可证。 