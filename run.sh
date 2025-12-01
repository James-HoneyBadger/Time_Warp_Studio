#!/usr/bin/env bash
# Time Warp IDE Launcher
# Launches the Python-based IDE

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_DIR="$DIR/Platforms/Python/.venv"

usage() {
  cat <<'EOF'
Time Warp IDE Launcher

Usage:
  ./run.sh [-- <extra args>]

Examples:
  ./run.sh
EOF
}

# Check if virtual environment exists, create if not
if [[ ! -d "$VENV_DIR" ]]; then
  echo "Creating virtual environment..."
  python3 -m venv "$VENV_DIR"
  "$VENV_DIR/bin/pip" install --upgrade pip
  "$VENV_DIR/bin/pip" install PySide6 websockets pillow
fi

# Use the virtual environment's Python
PYTHON_CMD="$VENV_DIR/bin/python"

# Parse command
if [[ $# -gt 0 ]]; then
  case "$1" in
    -h|--help)
      usage
      exit 0
      ;;
    *)
      # Pass all arguments to the IDE
      ;;
  esac
fi

# Launch the IDE
echo "Launching Time Warp IDE..."
export PYTHONPATH="$DIR/Platforms/Python"
exec "$PYTHON_CMD" "$DIR/Platforms/Python/time_warp_ide.py" "$@"
