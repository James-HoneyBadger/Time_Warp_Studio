#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="$ROOT/dist/win2000"
ZIP="$DIST_DIR/TimeWarpIDE-win2000.zip"
MANIFEST="$DIST_DIR/RELEASE_MANIFEST.txt"

RED='\033[0;31m'; GREEN='\033[0;32m'; BLUE='\033[0;34m'; NC='\033[0m'
info(){ echo -e "${BLUE}ℹ️  $1${NC}"; }
ok(){ echo -e "${GREEN}✅ $1${NC}"; }
err(){ echo -e "${RED}❌ $1${NC}"; }

[ -f "$ZIP" ] || { err "Missing $ZIP. Run build first."; exit 1; }

info "Generating release manifest…"

{
  echo "========================================"
  echo "Time Warp IDE - Windows 2000 Edition"
  echo "========================================"
  echo ""
  echo "Build Date: $(date -u '+%Y-%m-%d %H:%M:%S UTC')"
  echo "Platform: Windows 2000/XP/Vista/7/8/10/11 (i686)"
  echo "Toolchain: MinGW-w64 GCC $(i686-w64-mingw32-gcc --version | head -n1)"
  echo ""
  echo "Package Contents:"
  echo "----------------"
  unzip -l "$ZIP" | tail -n +4 | head -n -2
  echo ""
  echo "Checksums (SHA256):"
  echo "------------------"
  sha256sum "$ZIP" | awk '{print $1 "  " $2}'
  if [ -f "$DIST_DIR/TimeWarpIDE-win2000/TimeWarpIDE.exe" ]; then
    sha256sum "$DIST_DIR/TimeWarpIDE-win2000/TimeWarpIDE.exe" | awk '{print $1 "  TimeWarpIDE.exe"}'
  fi
  echo ""
  echo "Installation:"
  echo "------------"
  echo "1. Extract TimeWarpIDE-win2000.zip"
  echo "2. Run TimeWarpIDE.exe on Windows 2000 or later"
  echo "3. Load demo files from the demos/ folder"
  echo ""
  echo "Features:"
  echo "--------"
  echo "- BASIC interpreter with graphics commands"
  echo "- PILOT tutorial language support"
  echo "- Logo turtle graphics"
  echo "- MDI interface with syntax highlighting"
  echo "- GDI-based canvas (1024×768)"
  echo ""
  echo "Notes:"
  echo "-----"
  echo "- Requires Windows 2000 SP4 or later recommended"
  echo "- Uses RichEdit 2.0 and common controls"
  echo "- No external dependencies required"
  echo ""
  echo "========================================"
} > "$MANIFEST"

ok "Manifest saved to $MANIFEST"
cat "$MANIFEST"
