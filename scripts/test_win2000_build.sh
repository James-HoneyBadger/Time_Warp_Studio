#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="$ROOT/dist/win2000/TimeWarpIDE-win2000"
ZIP="$ROOT/dist/win2000/TimeWarpIDE-win2000.zip"

RED='\033[0;31m'; GREEN='\033[0;32m'; BLUE='\033[0;34m'; NC='\033[0m'
info(){ echo -e "${BLUE}ℹ️  $1${NC}"; }
ok(){ echo -e "${GREEN}✅ $1${NC}"; }
err(){ echo -e "${RED}❌ $1${NC}"; }

info "Checking zip exists…"
[ -f "$ZIP" ] || { err "Missing $ZIP"; exit 1; }
ok "Zip present"

info "Checking extracted folder contents…"
[ -d "$DIST_DIR" ] || { err "Missing extracted folder $DIST_DIR. Run build first."; exit 1; }
[ -f "$DIST_DIR/TimeWarpIDE.exe" ] || { err "Missing TimeWarpIDE.exe"; exit 1; }
[ -f "$DIST_DIR/README.txt" ] || { err "Missing README.txt"; exit 1; }
[ -d "$DIST_DIR/demos" ] || { err "Missing demos/"; exit 1; }
ok "Expected files present"

info "Checking exe size…"
SIZE=$(stat -c %s "$DIST_DIR/TimeWarpIDE.exe")
if [[ "$SIZE" -lt 500000 ]]; then
  err "Exe too small: $SIZE bytes"
  exit 1
fi
ok "Exe size is $SIZE bytes"

info "Scanning for expected strings…"
if command -v strings >/dev/null 2>&1; then
  if strings "$DIST_DIR/TimeWarpIDE.exe" | grep -i -E "TimeWarp(Editor|Canvas|MainWnd|MDIChild)|Windows 2000 Edition" >/dev/null; then
    ok "Found identifiers in binary"
  else
    err "Identifier missing"; exit 1
  fi
else
  info "strings not available; skipping binary scan"
fi

ok "Win2000 build sanity checks passed"
