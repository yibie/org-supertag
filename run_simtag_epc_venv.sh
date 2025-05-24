#!/bin/bash

# Get the absolute path of the directory where the script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
VENV_DIR="${SCRIPT_DIR}/.venv"
PYTHON_SCRIPT="${SCRIPT_DIR}/simtag_epc.py"
PYTHON_EXE="python3"

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
echo "Virtual Env Dir: ${VENV_DIR}"

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

# Ensure the virtual environment exists
if [ ! -d "$VENV_DIR" ]; then
    echo "Virtual environment (${VENV_DIR}) does not exist, creating it..."
    if ! command -v $PYTHON_EXE &> /dev/null; then
        error_exit "'$PYTHON_EXE' command not found. Cannot create virtual environment."
    fi
    $PYTHON_EXE -m venv "$VENV_DIR" || error_exit "Failed to create virtual environment."
    echo "Virtual environment created."
else
    echo "Using existing virtual environment: ${VENV_DIR}"
fi

# Activate the virtual environment
if [ -f "${VENV_DIR}/bin/activate" ]; then
    echo "Activating the virtual environment..."
    source "${VENV_DIR}/bin/activate"
    PYTHON_IN_VENV="${VENV_DIR}/bin/python"
    PIP_IN_VENV="${VENV_DIR}/bin/pip"
else
    error_exit "Virtual environment activation script not found in ${VENV_DIR}/bin/"
fi

# Ensure all necessary dependencies are installed *within the virtual environment*
echo "Checking/Installing dependencies within virtual environment..."
REQUIRED_PACKAGES=(epc sentence-transformers numpy requests ollama)
INSTALL_CMD="${PIP_IN_VENV} install --upgrade"
PACKAGES_TO_INSTALL=""

for pkg in "${REQUIRED_PACKAGES[@]}"; do
    echo -n "Checking for ${pkg}... "
    if ! $PYTHON_IN_VENV -c "import importlib.util; exit(0) if importlib.util.find_spec('$pkg') else exit(1)" &> /dev/null; then
        echo "Not found."
        PACKAGES_TO_INSTALL+="${pkg} "
    else
        echo "Found."
    fi
done

if [ -n "$PACKAGES_TO_INSTALL" ]; then
    echo "Installing/Upgrading missing packages: ${PACKAGES_TO_INSTALL}"
    $INSTALL_CMD $PACKAGES_TO_INSTALL || error_exit "Failed to install dependencies in venv."
else
    echo "All required packages seem to be installed."
fi

# Set environment variables
export SIMTAG_EPC_MODE="1"
export PYTHONPATH="${SCRIPT_DIR}:${PYTHONPATH}"

# Output debug information
echo ""
echo "Runtime Environment Information:"
echo "- Script Directory: ${SCRIPT_DIR}"
echo "- Python Interpreter: $($PYTHON_IN_VENV --version 2>&1) (from ${PYTHON_IN_VENV})"
echo "- Command Line Arguments: $@"

# Check if the simtag module directory exists
if [ -d "${SCRIPT_DIR}/simtag" ]; then
    echo "- simtag module directory: exists"
else
    echo "- simtag module directory: does not exist (warning)"
    mkdir -p "${SCRIPT_DIR}/simtag"
    touch "${SCRIPT_DIR}/simtag/__init__.py"
    echo "  Minimal module directory created"
fi

# Check the port parameter
PORT_ARG=""
PORT_NUM=""
for i in "${!@}"; do
    arg="${@:$i:1}"
    if [[ "$arg" == "--port" ]]; then
        next_index=$((i + 1))
        next_arg="${@:$next_index:1}"
        if [[ "$next_arg" =~ ^[0-9]+$ ]]; then
            PORT_ARG="--port $next_arg"
            PORT_NUM="$next_arg"
            echo "- Using Port: $PORT_NUM"
            break
        fi
    elif [[ "$arg" == --port=* ]]; then
        PORT_ARG="$arg"
        PORT_NUM="${arg#*=}"
        echo "- Using Port: $PORT_NUM"
        break
    fi
done

if [ -z "$PORT_ARG" ]; then
    echo "- Port not specified, using default port (0)"
    PORT_NUM=0
fi

echo ""
echo "========================================"
echo "Starting SimTag EPC Server..."
echo "========================================"

cd "$SCRIPT_DIR" || error_exit "Unable to switch to the script directory"

if [[ ! -z "$PORT_NUM" ]] && [[ "$PORT_NUM" -ne 0 ]]; then
    if nc -z 127.0.0.1 "$PORT_NUM" &> /dev/null; then
        echo "Warning: Port $PORT_NUM is already in use"
        echo "Attempting to terminate the process occupying the port..."
        PID_ON_PORT=$(lsof -ti :"$PORT_NUM" -sTCP:LISTEN)
        if [[ ! -z "$PID_ON_PORT" ]]; then
            echo "Killing process(es) on port $PORT_NUM: $PID_ON_PORT"
            kill -9 $PID_ON_PORT || echo "Failed to kill process $PID_ON_PORT, maybe already terminated."
            sleep 1
        else
            echo "Warning: Could not find specific PID using lsof on port $PORT_NUM."
        fi
    fi
fi

echo "Executing command: $PYTHON_IN_VENV $PYTHON_SCRIPT $@"
exec "$PYTHON_IN_VENV" "$PYTHON_SCRIPT" "$@"
