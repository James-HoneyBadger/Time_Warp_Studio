#!/bin/bash
# Time Warp Python IDE Launcher
# Activates virtual environment and launches the PySide6 GUI

cd "$(dirname "$0")"

# Activate virtual environment if it exists
if [ -f "../.venv/bin/python" ]; then
    PYTHON="../.venv/bin/python"
elif [ -f ".venv/bin/python" ]; then
    PYTHON=".venv/bin/python"
else
    PYTHON="python3"
fi

# Launch GUI
echo "🚀 Launching Time Warp IDE (Python/PySide6)..."
exec "$PYTHON" time_warp_ide.py "$@"
