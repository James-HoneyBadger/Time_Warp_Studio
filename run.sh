#!/usr/bin/env bash
# Time Warp IDE root launcher
# Usage:
#   ./run.sh python [-- <extra args>]
#   ./run.sh rust [--release] [-- <extra cargo args>]
# Launches the Python or Rust IDE from the repository root.

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  cat <<'EOF'
Usage:
  ./run.sh python [-- <extra args>]
  ./run.sh rust [--release] [-- <extra cargo args>]

Examples:
  ./run.sh python
  ./run.sh rust --release

Notes:
  - Python IDE requires: Python 3.8+, PySide6, pillow
  - Rust IDE requires: Rust toolchain (cargo)
EOF
}

if [[ $# -lt 1 ]]; then
  usage
  exit 1
fi

TARGET="$1"; shift || true

case "$TARGET" in
  python|py)
    # Pick python3 if available, else python
    if command -v python3 >/dev/null 2>&1; then
      PY=python3
    else
      PY=python
    fi

    if ! command -v "$PY" >/dev/null 2>&1; then
      echo "❌ Python not found. Please install Python 3.8+." >&2
      exit 1
    fi

  # Check for PySide6 and pillow
  if ! "$PY" -c "import PySide6, PIL" >/dev/null 2>&1; then
      echo "ℹ️ Missing dependencies for Python IDE. Install with:" >&2
      echo "    $PY -m pip install PySide6 pillow" >&2
    fi

    cd "$DIR/Time_Warp_Python"
    exec "$PY" time_warp_ide.py "$@"
    ;;

  rust|rs)
    if ! command -v cargo >/dev/null 2>&1; then
      echo "❌ cargo not found. Install Rust toolchain from https://rustup.rs/" >&2
      exit 1
    fi
    cd "$DIR/Time_Warp_Rust"
    # Pass through flags, default to debug run
    exec cargo run "$@"
    ;;

  *)
    echo "❌ Unknown target: $TARGET" >&2
    usage
    exit 1
    ;;
 esac
