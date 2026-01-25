#!/bin/bash
# Time Warp Studio Desktop Launcher

cd "$(dirname "$0")"

# Check if PySide6 is installed
if ! python3 -c "import PySide6" 2>/dev/null; then
    echo "❌ PySide6 not found. Installing..."
    pip install PySide6
fi

# Launch IDE
python3 time_warp_ide.py "$@"
