#!/bin/bash
# Time Warp Studio Launcher (Bash Wrapper)
# ========================================
# Simple bash wrapper for the Python launcher
# Usage: ./run.sh [options]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Pass all arguments to Python launcher
python3 run.py "$@"
