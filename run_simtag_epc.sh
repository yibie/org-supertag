#!/bin/bash
# Wrapper script to activate venv and run simtag_epc.py

# Get the directory where the script resides
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
VENV_DIR="$SCRIPT_DIR/.venv"

# Enable debug mode
set -x

# Check if venv exists
if [ -d "$VENV_DIR" ]; then
  # Activate the virtual environment
  if [ -z "$VIRTUAL_ENV" ]; then
      source "$VENV_DIR/bin/activate"
      echo "Activated venv: $VENV_DIR" >&2
  else
      echo "Venv already active: $VIRTUAL_ENV" >&2
  fi
else
  echo "Warning: Virtual environment not found at $VENV_DIR. Using system Python." >&2
fi

# Execute the Python script with all passed arguments
PYTHON_EXEC="python"
if [ -n "$VIRTUAL_ENV" ] && [ -x "$VENV_DIR/bin/python" ]; then
    PYTHON_EXEC="$VENV_DIR/bin/python"
fi

# Add debug output
echo "Using Python: $PYTHON_EXEC" >&2
echo "Script arguments: $@" >&2
echo "Current directory: $(pwd)" >&2
echo "Python path: $(which $PYTHON_EXEC)" >&2

# Run Python with unbuffered output
exec "$PYTHON_EXEC" -u "$SCRIPT_DIR/simtag_epc.py" "$@" 