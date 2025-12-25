#!/bin/bash
# Launch TW Editor

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PYTHON_DIR="$REPO_ROOT/Platforms/Python"

# Check for venv
VENV_DIR="$REPO_ROOT/.venv"
if [ -d "$VENV_DIR" ]; then
    source "$VENV_DIR/bin/activate"
fi

export PYTHONPATH="$PYTHON_DIR:$PYTHONPATH"
python3 "$PYTHON_DIR/tw_editor.py" "$@"
