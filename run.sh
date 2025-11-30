#!/usr/bin/env bash
# Time Warp IDE Launcher
# Launches the Python-based IDE

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  cat <<'EOF'
Time Warp IDE Launcher

Usage:
  ./run.sh [-- <extra args>]

Examples:
  ./run.sh
EOF
}

# Check if Python is available
if ! command -v python3 &> /dev/null && ! command -v python &> /dev/null; then
  echo "Error: Python 3 is required but not found."
  echo "Please install Python 3.8+ from https://python.org"
  exit 1
fi

# Use python3 if available, otherwise python
PYTHON_CMD="python3"
if ! command -v python3 &> /dev/null; then
  PYTHON_CMD="python"
fi

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
