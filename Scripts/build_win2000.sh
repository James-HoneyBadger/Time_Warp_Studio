#!/usr/bin/env bash
set -euo pipefail

# Cross-build Time Warp IDE for Windows 2000 and package demos

RED='\033[0;31m'; GREEN='\033[0;32m'; BLUE='\033[0;34m'; NC='\033[0m'
info(){ echo -e "${BLUE}ℹ️  $1${NC}"; }
ok(){ echo -e "${GREEN}✅ $1${NC}"; }
err(){ echo -e "${RED}❌ $1${NC}"; }

ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
WIN2K_DIR="$ROOT/platforms/win2000"
DIST_DIR="$ROOT/dist/win2000"

mkdir -p "$DIST_DIR"

info "Checking toolchain (i686-w64-mingw32-gcc)…"
if ! command -v i686-w64-mingw32-gcc >/dev/null 2>&1; then
  err "Cross-compiler not found. On Debian/Ubuntu: sudo apt-get install mingw-w64"; exit 1;
fi

info "Generating icon (optional)…"
if command -v convert >/dev/null 2>&1; then
  bash "$ROOT/scripts/make_win2000_icon.sh" || true
else
  info "ImageMagick not installed; using existing icon if present."
fi

info "Building Windows 2000 executable…"
(cd "$WIN2K_DIR" && make clean && make)

PKG_DIR="$DIST_DIR/TimeWarpIDE-win2000"
rm -rf "$PKG_DIR" && mkdir -p "$PKG_DIR/demos"

cp "$WIN2K_DIR/TimeWarpIDE.exe" "$PKG_DIR/"
cp -R "$WIN2K_DIR/demos"/* "$PKG_DIR/demos/" 2>/dev/null || true

cat > "$PKG_DIR/README.txt" <<'EOF'
Time Warp IDE — Windows 2000 Edition
====================================

This build targets Windows 2000 (NT 5.0) using GDI and common controls.

Run:
  - Double-click TimeWarpIDE.exe on Windows 2000/XP/…
  - Use File → Open to load demos from the demos/ folder

Notes:
  - Requires Windows 2000 SP4 recommended
  - Classic theme and 800x600+ resolution suggested
EOF

(cd "$DIST_DIR" && zip -r "TimeWarpIDE-win2000.zip" "TimeWarpIDE-win2000" >/dev/null)
ok "Created $DIST_DIR/TimeWarpIDE-win2000.zip"
