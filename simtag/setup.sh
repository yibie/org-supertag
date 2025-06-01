#!/bin/bash

# SimTag Automatic Installation Script
# Sets up Python virtual environment and installs all necessary dependencies

set -e  # Exit on error

echo "=== SimTag Automatic Installation Script ==="
echo

# Get script directory (simtag folder)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
VENV_DIR="$PROJECT_ROOT/.venv"
REQUIREMENTS_FILE="$SCRIPT_DIR/requirements.txt"

echo "Project root: $PROJECT_ROOT"
echo "Virtual environment directory: $VENV_DIR"
echo "Dependencies file: $REQUIREMENTS_FILE"
echo

# Check if Python is available
if ! command -v python3 &> /dev/null; then
    echo "❌ Error: Python3 not installed"
    echo "Please install Python3 first: https://www.python.org/downloads/"
    exit 1
fi

PYTHON_VERSION=$(python3 --version)
echo "✓ Found Python: $PYTHON_VERSION"

# Check if virtual environment exists
if [ -d "$VENV_DIR" ]; then
    echo "ℹ️  Virtual environment exists: $VENV_DIR"
    read -p "Recreate? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "🗑️  Removing existing virtual environment..."
        rm -rf "$VENV_DIR"
    else
        echo "Using existing virtual environment"
    fi
fi

# Create virtual environment
if [ ! -d "$VENV_DIR" ]; then
    echo "📦 Creating virtual environment..."
    python3 -m venv "$VENV_DIR"
    echo "✓ Virtual environment created"
fi

# Activate virtual environment
echo "🔄 Activating virtual environment..."
source "$VENV_DIR/bin/activate"

# Upgrade pip
echo "⬆️  Upgrading pip..."
pip install --upgrade pip

# Install base dependencies
echo "📥 Installing base dependencies..."
pip install wheel setuptools

# Check proxy settings
echo
echo "🌐 Checking network proxy settings..."
if [ -n "$HTTP_PROXY" ] || [ -n "$HTTPS_PROXY" ]; then
    echo "ℹ️  Detected proxy environment variables (HTTP_PROXY/HTTPS_PROXY set):"
    [ -n "$HTTP_PROXY" ] && echo "    HTTP_PROXY=$HTTP_PROXY"
    [ -n "$HTTPS_PROXY" ] && echo "    HTTPS_PROXY=$HTTPS_PROXY"
    echo "    Please ensure these settings allow access to external sites (like huggingface.co for model downloads)."
    echo "    If your network doesn't require a proxy, these variables may cause issues. Consider temporarily unsetting:"
    echo "    unset HTTP_PROXY HTTPS_PROXY"
else
    echo "ℹ️  No proxy environment variables detected (HTTP_PROXY/HTTPS_PROXY)."
    echo "    If you're in a network that requires a proxy (e.g., corporate network), you may need to set them to download dependencies and models."
    echo "    Example: export HTTPS_PROXY=http://your-proxy-address:port"
fi
echo "    To exclude local addresses, set NO_PROXY, e.g.: export NO_PROXY=localhost,127.0.0.1"
echo

# Install all dependencies
echo "📥 Installing project dependencies..."
if [ -f "$REQUIREMENTS_FILE" ]; then
    pip install -r "$REQUIREMENTS_FILE"
else
    echo "❌ Dependencies file not found: $REQUIREMENTS_FILE"
    exit 1
fi

# Special handling for sqlite-vec (may require special installation)
echo "🔧 Checking SQLite vector extension..."
if ! python -c "import sqlite_vec" 2>/dev/null; then
    echo "⚠️  sqlite-vec not available, attempting installation..."
    
    # Try different installation methods
    if command -v brew &> /dev/null; then
        echo "Installing sqlite-utils via Homebrew..."
        brew install sqlite-utils 2>/dev/null || echo "Homebrew installation failed, continuing..."
    fi
    
    # Try pip installation
    pip install sqlite-vec 2>/dev/null || echo "Note: sqlite-vec installation failed, will use fallback implementation"
fi

# Verify core dependencies
echo "✅ Verifying installation..."
python3 -c "
import sys
print(f'Python version: {sys.version}')

# Check core dependencies
modules = ['numpy', 'requests', 'epc', 'openai']
for module in modules:
    try:
        __import__(module)
        print(f'✓ {module}')
    except ImportError:
        print(f'❌ {module} - missing')

# Check optional dependencies
optional_modules = ['torch', 'sentence_transformers', 'sqlite_vec']
for module in optional_modules:
    try:
        __import__(module)
        print(f'✓ {module} (optional)')
    except ImportError:
        print(f'⚠️  {module} (optional) - not available')
"

echo
echo "🎉 Installation complete!"
echo
echo "Usage:"
echo "1. In Emacs, run: M-x org-supertag-sim-init"
echo "2. Or run quick test: M-x org-supertag-sim-epc-quick-test"
echo
echo "Virtual environment location: $VENV_DIR"
echo "To manually activate: source $VENV_DIR/bin/activate"
echo

# Return exit status
if python3 -c "import numpy, requests, epc, openai" 2>/dev/null; then
    echo "✅ Core dependencies verification successful"
    exit 0
else
    echo "❌ Core dependencies verification failed"
    exit 1
fi
