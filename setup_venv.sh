#!/bin/bash
set -e  # Exit immediately on error

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$SCRIPT_DIR"

VENV_DIR="${SCRIPT_DIR}/venv"
REQUIREMENTS_FILE="${SCRIPT_DIR}/requirements.txt"

# 显示系统信息
echo "====== 系统信息 ======"
python3 --version
echo "系统 Python 路径: $(which python3)"
echo "脚本目录: $SCRIPT_DIR"
echo "========================"
echo ""

# Create requirements.txt with versions suitable for Python 3.13
echo "Creating/Updating requirements.txt file..."
cat > "$REQUIREMENTS_FILE" << EOF
setuptools>=65.5.0
wheel>=0.40.0
numpy>=1.26.0
torch>=2.1.0
sentence-transformers>=2.2.2
requests>=2.31.0
epc>=0.0.5
huggingface-hub>=0.16.4
transformers>=4.34.0
urllib3<2.0.0
EOF

# 检查并清理存在的虚拟环境(如果指定)
if [ "$1" == "--clean" ]; then
    echo "Removing existing virtual environment..."
    rm -rf "$VENV_DIR"
fi

# Create virtual environment if it doesn't exist
if [ ! -d "$VENV_DIR" ]; then
    echo "Creating Python virtual environment..."
    python3 -m venv "$VENV_DIR" || {
        echo "Standard venv creation failed, trying alternative method..."
        # 尝试使用指定选项
        python3 -m venv --without-pip "$VENV_DIR" || {
            echo "Alternative venv creation also failed."
            exit 1
        }
    }
    
    # 检查Python解释器
    PYTHON_BINARY=""
    if [ -f "${VENV_DIR}/bin/python" ]; then
        PYTHON_BINARY="${VENV_DIR}/bin/python"
    elif [ -f "${VENV_DIR}/bin/python3" ]; then
        PYTHON_BINARY="${VENV_DIR}/bin/python3"
    else
        echo "Error: No Python interpreter found in virtual environment."
        echo "Creating symlink as a workaround..."
        # 创建符号链接作为解决方案
        ln -sf "$(which python3)" "${VENV_DIR}/bin/python"
        PYTHON_BINARY="${VENV_DIR}/bin/python"
    fi
    
    echo "Using Python interpreter: $PYTHON_BINARY"
fi

# Activate virtual environment and install dependencies
echo "Activating virtual environment and installing dependencies..."
source "${VENV_DIR}/bin/activate" || { echo "Failed to activate virtual environment"; exit 1; }

# 检查python命令
if command -v python &>/dev/null; then
    PYTHON_PATH=$(which python)
else
    if command -v python3 &>/dev/null; then
        PYTHON_PATH=$(which python3)
    else
        echo "Error: No python command found in activated environment"
        # 尝试通过直接路径访问
        if [ -f "${VENV_DIR}/bin/python" ]; then
            PYTHON_PATH="${VENV_DIR}/bin/python"
        elif [ -f "${VENV_DIR}/bin/python3" ]; then
            PYTHON_PATH="${VENV_DIR}/bin/python3"
        else
            echo "Fatal error: Cannot find Python interpreter"
            exit 1
        fi
    fi
fi
echo "Using Python: $PYTHON_PATH"

# 如果使用--without-pip创建的环境，需要安装pip
if ! command -v pip &>/dev/null; then
    echo "pip not found, installing it..."
    curl -sSL https://bootstrap.pypa.io/get-pip.py -o /tmp/get-pip.py
    "$PYTHON_PATH" /tmp/get-pip.py || { echo "Failed to install pip"; exit 1; }
    rm /tmp/get-pip.py
fi

# 更新pip
echo "Upgrading pip..."
"$PYTHON_PATH" -m pip install --upgrade pip || { echo "Failed to upgrade pip"; exit 1; }

# 先安装setuptools和wheel来解决构建问题
echo "Installing setuptools and wheel first..."
"$PYTHON_PATH" -m pip install --upgrade setuptools wheel || { echo "Failed to install setuptools and wheel"; exit 1; }

# 现在安装主要依赖项
echo "Installing dependencies from $REQUIREMENTS_FILE..."
"$PYTHON_PATH" -m pip install -r "$REQUIREMENTS_FILE" || { 
    echo "Failed to install dependencies with requirements.txt"
    echo "Trying to install packages one by one..."
    
    # 尝试逐个安装可能有兼容性问题的包
    "$PYTHON_PATH" -m pip install numpy || echo "Warning: Failed to install numpy"
    "$PYTHON_PATH" -m pip install torch || echo "Warning: Failed to install torch"
    "$PYTHON_PATH" -m pip install sentence-transformers || echo "Warning: Failed to install sentence-transformers"
    "$PYTHON_PATH" -m pip install requests || echo "Warning: Failed to install requests"
    "$PYTHON_PATH" -m pip install epc || echo "Warning: Failed to install epc"
    "$PYTHON_PATH" -m pip install "urllib3<2.0.0" || echo "Warning: Failed to install urllib3"
}

# 确保当前目录也加入到Python路径
echo "Installing current directory as package..."
"$PYTHON_PATH" -m pip install -e . 2>/dev/null || echo "Note: package install failed, but this is not critical"

# 测试导入关键模块 - 使用前面确定的PYTHON_PATH
echo "Testing imports..."
echo "Using Python: $PYTHON_PATH"

"$PYTHON_PATH" -c "import numpy; import torch; import sentence_transformers; import epc; print('All critical modules imported successfully')" || { 
    echo "Failed to import required modules. Trying minimal imports..."
    "$PYTHON_PATH" -c "import sys; print(f'Python version: {sys.version}')" || {
        echo "Basic Python functionality test failed!"
        exit 1
    }
    
    # 尝试逐个测试导入
    echo "Testing numpy..." 
    "$PYTHON_PATH" -c "import numpy; print('numpy ok')" || echo "numpy import failed"
    
    echo "Testing torch..."
    "$PYTHON_PATH" -c "import torch; print('torch ok')" || echo "torch import failed"
    
    echo "Testing sentence_transformers..."
    "$PYTHON_PATH" -c "import sentence_transformers; print('sentence_transformers ok')" || echo "sentence_transformers import failed"
    
    echo "Testing epc..."
    "$PYTHON_PATH" -c "import epc; print('epc ok')" || echo "epc import failed"
    
    echo "Some modules may have failed to import, but setup will continue."
}

# 验证simtag模块是否存在且可导入
if [ -d "${SCRIPT_DIR}/simtag" ]; then
    echo "SimTag module found, checking if it can be imported..."
    
    # 创建简单的测试脚本
    cat > "${SCRIPT_DIR}/test_simtag_import.py" << EOF
import sys
import os

# 添加当前目录到Python路径
sys.path.insert(0, "${SCRIPT_DIR}")

try:
    from simtag.config import Config
    print("Successfully imported simtag.config")
except ImportError as e:
    print(f"Failed to import simtag: {e}")
    sys.exit(1)
EOF
    
    # 运行测试脚本
    "$PYTHON_PATH" "${SCRIPT_DIR}/test_simtag_import.py" || { 
        echo "Warning: Failed to import simtag module using standard method"
        echo "Trying alternative import approach..."
        
        # 创建另一个测试脚本，使用不同的方法
        cat > "${SCRIPT_DIR}/test_simtag_import2.py" << EOF
import sys
import os

# 添加当前目录到Python路径
sys.path.insert(0, os.path.abspath("${SCRIPT_DIR}"))
print(f"Python path: {sys.path}")
print(f"Looking for simtag in: {os.path.abspath('${SCRIPT_DIR}/simtag')}")

try:
    # 尝试直接导入simtag模块
    import simtag
    print(f"Successfully imported simtag module: {simtag.__file__}")
except ImportError as e:
    print(f"Failed to import simtag: {e}")
    # 不退出，继续设置
    pass
EOF
        
        "$PYTHON_PATH" "${SCRIPT_DIR}/test_simtag_import2.py" || echo "Warning: Both import attempts failed, but setup will continue"
        rm "${SCRIPT_DIR}/test_simtag_import2.py"
    }
    rm "${SCRIPT_DIR}/test_simtag_import.py"
fi

echo ""
echo "====== 设置完成! ======"
echo "SimTag虚拟环境已就绪: ${VENV_DIR}"
echo ""
echo "使用提示:"
echo "1. 要在Emacs中使用此环境，请设置:"
echo "   (setq org-supertag-sim-epc-python-path \"${VENV_DIR}/bin/python\")"
echo "2. 如果遇到问题，请尝试使用 --clean 选项重新运行此脚本"
echo "   例如: ./setup_venv.sh --clean"
echo "========================"
