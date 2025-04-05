#!/bin/bash

# Get the absolute path of the directory where the script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
VENV_DIR="${SCRIPT_DIR}/venv"
PYTHON_SCRIPT="${SCRIPT_DIR}/simtag_epc.py"

# Output error message to standard error and exit
function error_exit {
    echo "Error: $1" >&2
    exit 1
}

echo "========================================"
echo "SimTag EPC Startup Script"
echo "========================================"
echo "Script Directory: ${SCRIPT_DIR}"
echo "Python Script: ${PYTHON_SCRIPT}"

# Check if the Python script exists
if [ ! -f "$PYTHON_SCRIPT" ]; then
    error_exit "Python script not found: ${PYTHON_SCRIPT}"
fi

# First, clean up any possible residual Python processes
echo "Checking for running SimTag processes..."
if pgrep -f "python.*simtag_epc.py" > /dev/null; then
    echo "Found a running SimTag process, attempting to terminate..."
    pkill -f "python.*simtag_epc.py" || true
    sleep 1
fi

# Check if the virtual environment exists
if [ -d "$VENV_DIR" ]; then
    echo "Using existing virtual environment: ${VENV_DIR}"
    
    # Activate the virtual environment
    if [ -f "${VENV_DIR}/bin/activate" ]; then
        echo "Activating the virtual environment..."
        source "${VENV_DIR}/bin/activate"
    else
        error_exit "Virtual environment directory exists but the activation script was not found"
    fi
else
    echo "Virtual environment does not exist, attempting to use system Python"
    
    # Check the system Python version
    PYTHON_VERSION=$(python3 --version 2>&1)
    echo "System Python version: ${PYTHON_VERSION}"
    
    # Check if the necessary dependencies are installed
    echo "Checking necessary dependencies..."
    
    if ! python3 -c "import sys; print(f'Python Path: {sys.executable}')" 2>/dev/null; then
        error_exit "Unable to execute Python"
    fi
    
    if ! python3 -c "import epc" 2>/dev/null; then
        echo "Warning: EPC library not detected, attempting to install automatically..."
        python3 -m pip install epc || error_exit "Unable to install EPC library"
    fi
    
    if ! python3 -c "import sentence_transformers" 2>/dev/null; then
        echo "Warning: sentence_transformers library not detected, attempting to install automatically..."
        python3 -m pip install sentence_transformers || error_exit "Unable to install sentence_transformers library"
    fi
    
    if ! python3 -c "import numpy" 2>/dev/null; then
        echo "Warning: numpy library not detected, attempting to install automatically..."
        python3 -m pip install numpy || error_exit "Unable to install numpy library"
    fi
fi

# Set environment variables
export SIMTAG_EPC_MODE="1"
export PYTHONPATH="${SCRIPT_DIR}:${PYTHONPATH}"

# Output debug information
echo ""
echo "Runtime Environment Information:"
echo "- Script Directory: ${SCRIPT_DIR}"
echo "- Python Interpreter: $(which python3)"
echo "- Python Version: $(python3 --version 2>&1)"
echo "- Command Line Arguments: $@"

# Check if the simtag module directory exists
if [ -d "${SCRIPT_DIR}/simtag" ]; then
    echo "- simtag module directory: exists"
else
    echo "- simtag module directory: does not exist (warning)"
    # Attempt to create the minimal module
    mkdir -p "${SCRIPT_DIR}/simtag"
    touch "${SCRIPT_DIR}/simtag/__init__.py"
    echo "  Minimal module directory created"
fi

# Check the port parameter
PORT_ARG=""
for arg in "$@"; do
    if [[ $arg == --port* ]]; then
        PORT_ARG=$arg
        PORT_NUM=${arg#*=}
        PORT_NUM=${PORT_NUM#* }
        echo "- Using Port: $PORT_NUM"
        break
    fi
done

# If no port is specified, use the default port
if [ -z "$PORT_ARG" ]; then
    echo "- Port not specified, using default port"
fi

echo ""
echo "========================================"
echo "Starting SimTag EPC Server..."
echo "========================================"

# Ensure the directory exists
cd "$SCRIPT_DIR" || error_exit "Unable to switch to the script directory"

# Check if there is an existing process occupying the port
if [[ ! -z "$PORT_NUM" ]]; then
    if nc -z 127.0.0.1 "$PORT_NUM" 2>/dev/null; then
        echo "Warning: Port $PORT_NUM is already in use"
        echo "Attempting to terminate the process occupying the port..."
        if command -v lsof &> /dev/null; then
            lsof -i :"$PORT_NUM" -t | xargs kill -9 2>/dev/null || true
            echo "Waiting for the port to be released..."
            sleep 1
        fi
    fi
fi

# Execute the Python script, ensuring all parameters are passed
echo "Executing command: python3 $PYTHON_SCRIPT $@"
exec python3 "$PYTHON_SCRIPT" "$@"
