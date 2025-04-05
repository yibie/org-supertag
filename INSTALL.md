# Org-SuperTag SimTag 

本文档提供了安装和配置 Org-SuperTag SimTag 组件的详细步骤。

## 系统要求

- Emacs 26.1 或更高版本
- Python 3.9 或更高版本
- [Ollama](https://ollama.ai/) 服务已安装并运行

## 安装步骤

### 1. 安装 Emacs 依赖

确保你已安装 `epc` 包:

```elisp
M-x package-install RET epc RET
```

### 2. 安装 Python 依赖

SimTag 需要一些 Python 依赖，包括 `torch`, `sentence-transformers`, `epc` 和 `ollama` 等。我们提供了一个自动化脚本来设置必要的虚拟环境和安装所有依赖。

#### 使用自动化脚本安装依赖

在 org-supertag 目录下运行：

```bash
# 进入 org-supertag 目录
cd /path/to/org-supertag

# 运行安装脚本
./setup_venv.sh
```

如果遇到问题需要重新安装，可以使用 `--clean` 选项：

```bash
./setup_venv.sh --clean
```

#### 手动安装依赖（如果自动化脚本失败）

如果自动化脚本不适用于你的环境，可以手动安装依赖：

```bash
# 创建虚拟环境
python3 -m venv venv
source venv/bin/activate

# 安装依赖
pip install torch sentence-transformers requests epc numpy urllib3 ollama
```

### 3. 配置 Emacs

在你的 Emacs 配置文件中添加：

```elisp
;; 加载 org-supertag
(add-to-list 'load-path "/path/to/org-supertag")
(require 'org-supertag)
(require 'org-supertag-sim)

;; 配置 Python 路径 (如果使用了虚拟环境)
(setq org-supertag-sim-python-command "/path/to/org-supertag/venv/bin/python")

;; 可选：设置默认模型
(setq org-supertag-sim-model-name "hf.co/unsloth/gemma-3-4b-it-GGUF:latest")
```

## 验证安装

重启 Emacs 后，运行以下命令验证安装：

```elisp
M-x org-supertag-sim-epc-test-safe-init
```

如果一切正常，你应该会看到服务初始化成功的消息。然后可以测试 Ollama 连接：

```elisp
M-x org-supertag-sim-epc-test-ollama-connection
```

## 故障排除

### 1. EPC 服务器无法启动

检查 Python 环境是否正确配置。可以使用以下命令查看 Python 环境信息：

```elisp
M-x org-supertag-sim-epc-debug-env
```

### 2. Ollama 连接问题

- 确保 Ollama 服务正在运行
- 检查是否已安装所需的模型：`ollama list`
- 如果模型未安装，运行：`ollama pull hf.co/unsloth/gemma-3-4b-it-GGUF:latest`

### 3. 依赖导入错误

如果遇到 `No module named 'ollama'` 等错误，说明某些依赖未正确安装。请尝试以下步骤：

1. 确认虚拟环境已激活
2. 手动安装缺失的依赖：`pip install ollama`
3. 重新运行 `setup_venv.sh --clean` 脚本

### 4. 模型不存在

如果 Ollama 报告模型不存在，请确保已安装相应模型：

```bash
ollama pull hf.co/unsloth/gemma-3-4b-it-GGUF:latest
```

## 其他资源

- 相关指令和用法请参考 README.md
- 完整 API 文档请参考源代码注释 