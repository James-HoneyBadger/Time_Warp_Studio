#!/usr/bin/env bash
set -euo pipefail

# Package a portable Rust release tarball with installer
# Produces: dist/time-warp-ide-rust-linux-<arch>.tar.gz

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

info(){ echo -e "${BLUE}ℹ️  $1${NC}"; }
ok(){ echo -e "${GREEN}✅ $1${NC}"; }
err(){ echo -e "${RED}❌ $1${NC}"; }

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RUST_DIR="$REPO_ROOT/platforms/rust"
DIST_DIR="$REPO_ROOT/dist"

mkdir -p "$DIST_DIR"

info "Building Rust release…"
(cd "$RUST_DIR" && cargo build --release)

BIN_SRC="$RUST_DIR/target/release/time-warp"
if [[ ! -x "$BIN_SRC" ]]; then
  err "Built binary missing: $BIN_SRC"; exit 1
fi

ARCH="$(uname -m)"
PKG_NAME="time-warp-ide-rust-linux-$ARCH"
PKG_DIR="$DIST_DIR/$PKG_NAME"

rm -rf "$PKG_DIR" && mkdir -p "$PKG_DIR/bin" "$PKG_DIR/docs" "$PKG_DIR/examples"

install -m 755 "$BIN_SRC" "$PKG_DIR/bin/time-warp"

# Include installer
install -m 755 "$REPO_ROOT/scripts/install-linux-desktop-rust.sh" "$PKG_DIR/"

# Minimal docs
cp "$REPO_ROOT/README.md" "$PKG_DIR/docs/README.md" 2>/dev/null || true
cp "$REPO_ROOT/LICENSE" "$PKG_DIR/docs/" 2>/dev/null || true

# Include examples (optional, lightweight)
if [[ -d "$REPO_ROOT/examples" ]]; then
  cp -R "$REPO_ROOT/examples"/* "$PKG_DIR/examples/" 2>/dev/null || true
fi

cat > "$PKG_DIR/INSTALL.txt" <<'EOF'
Time Warp IDE (Rust) — Portable Package
======================================

Quick Install (User Scope)
--------------------------

1) Extract this archive:
   tar -xzf time-warp-ide-rust-linux-*.tar.gz

2) Install desktop entry + launcher:
   cd time-warp-ide-rust-linux-*/
   ./install-linux-desktop-rust.sh --no-build

3) Launch from your applications menu (Development/Education) or run:
   timewarp-ide-rust

Uninstall
---------
./install-linux-desktop-rust.sh --uninstall
EOF

(cd "$DIST_DIR" && tar -czf "$PKG_NAME.tar.gz" "$PKG_NAME")
ok "Created $DIST_DIR/$PKG_NAME.tar.gz"

echo "Contents:" && ls -1 "$DIST_DIR/$PKG_NAME" | sed 's/^/  - /'
