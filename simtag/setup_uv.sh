#!/bin/bash

# =============================================================================
# Org Supertag Python Environment Setup Script
# =============================================================================
# This script sets up a Python virtual environment using uv for the Org Supertag
# project. It installs all required dependencies and verifies the installation.
#
# Requirements:
# - uv (Python package manager) must be installed
# - bash shell
# - Internet connection for downloading dependencies
#
# Usage:
#   bash simtag/setup_uv.sh
# =============================================================================

set -e  # Exit on any error

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
VENV_DIR="$PROJECT_ROOT/.venv"
REQUIREMENTS_FILE="$SCRIPT_DIR/requirements.txt"
PYTHON_VERSION="3.11"  # Target Python version

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Check if uv is installed
check_uv() {
    if ! command -v uv &> /dev/null; then
        log_error "uv is not installed. Please install uv first:"
        echo "   Visit: https://docs.astral.sh/uv/getting-started/installation/"
        echo "   Or run: curl -LsSf https://astral.sh/uv/install.sh | sh"
        exit 1
    fi
    log_success "uv is installed: $(uv --version)"
}

# Check system requirements
check_system() {
    log_info "Checking system requirements..."
    
    # Check Python version
    if command -v python3 &> /dev/null; then
        PYTHON_VERSION_ACTUAL=$(python3 -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')")
        log_info "Python version: $PYTHON_VERSION_ACTUAL"
    else
        log_error "Python 3 is not installed"
        exit 1
    fi
    
    # Check available disk space (at least 1GB)
    DISK_SPACE=$(df "$PROJECT_ROOT" | awk 'NR==2 {print $4}')
    if [ "$DISK_SPACE" -lt 1048576 ]; then  # 1GB in KB
        log_warning "Low disk space available: ${DISK_SPACE}KB"
        log_warning "At least 1GB is recommended for installation"
    fi
}

# Create virtual environment
create_venv() {
    log_info "Creating virtual environment..."
    
    if [ -d "$VENV_DIR" ]; then
        log_warning "Virtual environment already exists at: $VENV_DIR"
        read -p "Do you want to recreate it? (y/N): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            log_info "Removing existing virtual environment..."
            rm -rf "$VENV_DIR"
        else
            log_info "Using existing virtual environment"
            return 0
        fi
    fi
    
    # Create virtual environment with uv
    log_info "Creating new virtual environment with uv..."
    uv venv --python "$PYTHON_VERSION" "$VENV_DIR"
    
    if [ ! -d "$VENV_DIR" ]; then
        log_error "Failed to create virtual environment"
        exit 1
    fi
    
    log_success "Virtual environment created at: $VENV_DIR"
}

# Activate virtual environment
activate_venv() {
    log_info "Activating virtual environment..."
    
    # Source the virtual environment
    if [ -f "$VENV_DIR/bin/activate" ]; then
        source "$VENV_DIR/bin/activate"
        log_success "Virtual environment activated"
    else
        log_error "Virtual environment activation script not found"
        exit 1
    fi
    
    # Verify activation
    if [ "$VIRTUAL_ENV" != "$VENV_DIR" ]; then
        log_error "Virtual environment activation failed"
        exit 1
    fi
}

# Check network connectivity
check_network() {
    log_info "Checking network connectivity..."
    
    # Test connection to PyPI
    if curl -s --connect-timeout 10 https://pypi.org/ > /dev/null; then
        log_success "Network connectivity to PyPI: OK"
    else
        log_warning "Cannot connect to PyPI. Check your internet connection."
        log_warning "If you're behind a proxy, make sure HTTP_PROXY/HTTPS_PROXY are set correctly."
    fi
}

# Check proxy settings
check_proxy() {
    log_info "Checking proxy configuration..."
    
    if [ -n "$HTTP_PROXY" ] || [ -n "$HTTPS_PROXY" ]; then
        log_info "Detected proxy environment variables (HTTP_PROXY/HTTPS_PROXY set):"
        [ -n "$HTTP_PROXY" ] && echo "    HTTP_PROXY=$HTTP_PROXY"
        [ -n "$HTTPS_PROXY" ] && echo "    HTTPS_PROXY=$HTTPS_PROXY"
        echo "    Please ensure these settings allow access to external sites (e.g., huggingface.co for model downloads)."
        echo "    If your network doesn't require a proxy, these variables may cause issues. Consider temporarily unsetting them:"
        echo "    unset HTTP_PROXY HTTPS_PROXY"
    else
        log_info "No proxy environment variables detected (HTTP_PROXY/HTTPS_PROXY)."
        echo "    If you're in a network that requires a proxy (e.g., corporate network), you may need to set them to download dependencies and models."
        echo "    Example: export HTTPS_PROXY=http://your-proxy-address:port"
    fi
    echo "    To exclude local addresses, set NO_PROXY, e.g.: export NO_PROXY=localhost,127.0.0.1"
    echo
}

# Install dependencies
install_dependencies() {
    log_info "Installing project dependencies..."
    
    if [ -f "$REQUIREMENTS_FILE" ]; then
        uv pip install -r "$REQUIREMENTS_FILE"
    else
        log_error "Dependencies file not found: $REQUIREMENTS_FILE"
        exit 1
    fi
}

# Handle special dependencies
handle_special_deps() {
    log_info "Checking SQLite vector extension..."
    
    # Note: uv should handle binary dependencies better, but this check remains as an extra safeguard.
    if ! python -c "import sqlite_vec" 2>/dev/null; then
        log_warning "sqlite-vec failed Python import check, attempting installation..."
        
        # Try uv pip install first, as uv's venv is already active
        uv pip install sqlite-vec 2>/dev/null || echo "Note: sqlite-vec installation failed, will use fallback implementation"
    fi
}

# Verify installation
verify_installation() {
    log_info "Verifying installation..."
    
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
}

# Main execution
main() {
    echo "============================================================================="
    echo "Org Supertag Python Environment Setup"
    echo "============================================================================="
    echo
    
    # Run all setup steps
    check_uv
    check_system
    create_venv
    activate_venv
    check_network
    check_proxy
    install_dependencies
    handle_special_deps
    verify_installation
    
    echo
    log_success "Installation complete!"
    echo
    echo "Virtual environment location: $VENV_DIR"
    echo "To manually activate: source $VENV_DIR/bin/activate"
    echo
    
    # Final verification
    if python3 -c "import numpy, requests, epc, openai" 2>/dev/null; then
        log_success "Core dependencies verification successful"
        exit 0
    else
        log_error "Core dependencies verification failed"
        exit 1
    fi
}

# Run main function
main "$@" 