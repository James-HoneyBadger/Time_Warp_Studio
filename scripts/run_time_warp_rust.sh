#!/usr/bin/env bash
set -euo pipefail

# Simple launcher for the Rust Time Warp IDE on Linux/macOS.
# - Builds release by default if the binary is missing
# - Optional: --debug to run debug build
# - Optional: --no-build to skip build if missing
# - Optional: --background to detach and log to /tmp/time-warp.log
# - Optional: --log <file> to set an explicit log file

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RUST_DIR="$REPO_ROOT/platforms/rust"

MODE=release
DO_BUILD=1
BACKGROUND=0
LOG_FILE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --debug)
      MODE=debug
      shift
      ;;
    --no-build)
      DO_BUILD=0
      shift
      ;;
    --background)
      BACKGROUND=1
      shift
      ;;
    --log)
      LOG_FILE="${2:-}"
      shift 2
      ;;
    -h|--help)
      echo "Usage: $0 [--debug] [--no-build] [--background] [--log <file>]";
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      exit 2
      ;;
  esac
done

cd "$RUST_DIR"

if [[ "$MODE" == "release" ]]; then
  BIN="target/release/time-warp"
else
  BIN="target/debug/time-warp"
fi

if [[ ! -x "$BIN" ]]; then
  if [[ "$DO_BUILD" == "1" ]]; then
    if [[ "$MODE" == "release" ]]; then
      echo "Building release binary..."; cargo build --release
    else
      echo "Building debug binary..."; cargo build
    fi
  else
    echo "Binary $BIN not found and --no-build specified." >&2
    exit 1
  fi
fi

if [[ -z "$LOG_FILE" ]]; then
  LOG_FILE="/tmp/time-warp.log"
fi

if [[ "$BACKGROUND" == "1" ]]; then
  echo "Launching in background: $BIN"
  nohup "$BIN" >"$LOG_FILE" 2>&1 &
  pid=$!
  echo "pid=$pid"
  echo "Logging to: $LOG_FILE"
else
  echo "Launching: $BIN"
  echo "Logs (if any) will appear here; press Ctrl+C to exit."
  exec "$BIN"
fi
