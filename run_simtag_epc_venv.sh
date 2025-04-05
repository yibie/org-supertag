#!/bin/bash

# 获取脚本所在目录的绝对路径
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
VENV_DIR="${SCRIPT_DIR}/venv"
PYTHON_SCRIPT="${SCRIPT_DIR}/simtag_epc.py"

# 输出错误信息到标准错误并退出
function error_exit {
    echo "错误: $1" >&2
    exit 1
}

echo "========================================"
echo "SimTag EPC 启动脚本"
echo "========================================"
echo "脚本目录: ${SCRIPT_DIR}"
echo "Python脚本: ${PYTHON_SCRIPT}"

# 检查Python脚本是否存在
if [ ! -f "$PYTHON_SCRIPT" ]; then
    error_exit "找不到Python脚本: ${PYTHON_SCRIPT}"
fi

# 首先清理任何可能残留的Python进程
echo "检查是否有运行中的SimTag进程..."
if pgrep -f "python.*simtag_epc.py" > /dev/null; then
    echo "找到正在运行的SimTag进程，尝试终止..."
    pkill -f "python.*simtag_epc.py" || true
    sleep 1
fi

# 检查虚拟环境是否存在
if [ -d "$VENV_DIR" ]; then
    echo "使用现有虚拟环境: ${VENV_DIR}"
    
    # 激活虚拟环境
    if [ -f "${VENV_DIR}/bin/activate" ]; then
        echo "激活虚拟环境..."
        source "${VENV_DIR}/bin/activate"
    else
        error_exit "虚拟环境目录存在但找不到激活脚本"
    fi
else
    echo "虚拟环境不存在，尝试使用系统Python"
    
    # 检查系统Python版本
    PYTHON_VERSION=$(python3 --version 2>&1)
    echo "系统Python版本: ${PYTHON_VERSION}"
    
    # 检查必要的依赖是否已安装
    echo "检查必要依赖..."
    
    if ! python3 -c "import sys; print(f'Python路径: {sys.executable}')" 2>/dev/null; then
        error_exit "无法执行Python"
    fi
    
    if ! python3 -c "import epc" 2>/dev/null; then
        echo "警告: 未检测到EPC库，尝试自动安装..."
        python3 -m pip install epc || error_exit "无法安装EPC库"
    fi
    
    if ! python3 -c "import sentence_transformers" 2>/dev/null; then
        echo "警告: 未检测到sentence_transformers库，尝试自动安装..."
        python3 -m pip install sentence_transformers || error_exit "无法安装sentence_transformers库"
    fi
    
    if ! python3 -c "import numpy" 2>/dev/null; then
        echo "警告: 未检测到numpy库，尝试自动安装..."
        python3 -m pip install numpy || error_exit "无法安装numpy库"
    fi
fi

# 设置环境变量
export SIMTAG_EPC_MODE="1"
export PYTHONPATH="${SCRIPT_DIR}:${PYTHONPATH}"

# 输出调试信息
echo ""
echo "运行环境信息:"
echo "- 脚本目录: ${SCRIPT_DIR}"
echo "- Python解释器: $(which python3)"
echo "- Python版本: $(python3 --version 2>&1)"
echo "- 命令行参数: $@"

# 检查是否存在simtag模块目录
if [ -d "${SCRIPT_DIR}/simtag" ]; then
    echo "- simtag模块目录: 存在"
else
    echo "- simtag模块目录: 不存在（警告）"
    # 尝试创建最小模块
    mkdir -p "${SCRIPT_DIR}/simtag"
    touch "${SCRIPT_DIR}/simtag/__init__.py"
    echo "  已创建最小模块目录"
fi

# 检查端口参数
PORT_ARG=""
for arg in "$@"; do
    if [[ $arg == --port* ]]; then
        PORT_ARG=$arg
        PORT_NUM=${arg#*=}
        PORT_NUM=${PORT_NUM#* }
        echo "- 使用端口: $PORT_NUM"
        break
    fi
done

# 如果没有指定端口，使用默认端口
if [ -z "$PORT_ARG" ]; then
    echo "- 未指定端口，使用默认端口"
fi

echo ""
echo "========================================"
echo "启动SimTag EPC服务器..."
echo "========================================"

# 确保目录存在
cd "$SCRIPT_DIR" || error_exit "无法切换到脚本目录"

# 检测是否有已有进程占用端口
if [[ ! -z "$PORT_NUM" ]]; then
    if nc -z 127.0.0.1 "$PORT_NUM" 2>/dev/null; then
        echo "警告: 端口 $PORT_NUM 已被占用"
        echo "尝试结束占用端口的进程..."
        if command -v lsof &> /dev/null; then
            lsof -i :"$PORT_NUM" -t | xargs kill -9 2>/dev/null || true
            echo "等待端口释放..."
            sleep 1
        fi
    fi
fi

# 执行Python脚本，确保传递所有参数
echo "执行命令: python3 $PYTHON_SCRIPT $@"
exec python3 "$PYTHON_SCRIPT" "$@"
