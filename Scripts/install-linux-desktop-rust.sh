#!/usr/bin/env bash
set -euo pipefail

# Time Warp IDE (Rust) - Linux desktop installer (user scope)
# - Builds and installs the Rust binary to ~/.local/share/timewarp-rust/bin
# - Creates launcher ~/.local/bin/timewarp-ide-rust
# - Installs .desktop entry and hicolor PNG icons (if ImageMagick available)
# - Uninstall with --uninstall

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

info() { echo -e "${BLUE}ℹ️  $1${NC}"; }
ok() { echo -e "${GREEN}✅ $1${NC}"; }
warn() { echo -e "${YELLOW}⚠️  $1${NC}"; }
err() { echo -e "${RED}❌ $1${NC}"; }

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RUST_DIR="$REPO_ROOT/platforms/rust"

INSTALL_BASE="$HOME/.local/share/timewarp-rust"
BIN_DIR_USER="$HOME/.local/bin"
BIN_INSTALL="$INSTALL_BASE/bin"
DESKTOP_DIR="$HOME/.local/share/applications"
ICON_DIR="$HOME/.local/share/icons/hicolor"

UNINSTALL=0
SKIP_BUILD=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --uninstall) UNINSTALL=1; shift ;;
    --no-build) SKIP_BUILD=1; shift ;;
    -h|--help)
      cat <<EOF
Usage: $0 [--no-build] [--uninstall]

Installs the Rust Time Warp IDE desktop entry for the current user.
- Builds and installs the binary to:   $BIN_INSTALL/time-warp
- Creates launcher in PATH:            $BIN_DIR_USER/timewarp-ide-rust
- Installs .desktop to:                $DESKTOP_DIR/timewarp-ide-rust.desktop
- Installs icons under:                $ICON_DIR/*/apps/timewarp.png

Options:
  --no-build   Use existing release binary (platforms/rust/target/release/time-warp)
  --uninstall  Remove installed files
EOF
      exit 0 ;;
    *) err "Unknown option: $1"; exit 2 ;;
  esac
done

uninstall() {
  echo -e "${BLUE}Removing Time Warp IDE (Rust) desktop install${NC}"
  rm -f "$BIN_DIR_USER/timewarp-ide-rust" || true
  rm -f "$DESKTOP_DIR/timewarp-ide-rust.desktop" || true
  rm -rf "$INSTALL_BASE" || true
  rm -f "$ICON_DIR"/*/apps/timewarp.png || true
  command -v update-desktop-database &>/dev/null && update-desktop-database "$DESKTOP_DIR" || true
  command -v gtk-update-icon-cache &>/dev/null && gtk-update-icon-cache -f -t "$ICON_DIR" || true
  ok "Uninstalled Time Warp IDE (Rust)"
}

create_icons() {
  info "Installing icons (hicolor)…"
  mkdir -p "$ICON_DIR/48x48/apps" "$ICON_DIR/64x64/apps" "$ICON_DIR/128x128/apps" "$ICON_DIR/256x256/apps"
  if command -v convert &>/dev/null; then
    cat > /tmp/timewarp-rust-$$.svg << 'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<svg width="256" height="256" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <linearGradient id="grad" x1="0%" y1="0%" x2="100%" y2="100%">
      <stop offset="0%" style="stop-color:#4A90E2;stop-opacity:1" />
      <stop offset="100%" style="stop-color:#7B68EE;stop-opacity:1" />
    </linearGradient>
  </defs>
  <rect width="256" height="256" rx="32" fill="url(#grad)"/>
  <text x="128" y="140" font-family="monospace" font-size="120" font-weight="bold" text-anchor="middle" fill="white">TW</text>
  <path d="M 40 200 Q 128 180 216 200" stroke="white" stroke-width="8" fill="none" stroke-linecap="round"/>
  <circle cx="216" cy="200" r="6" fill="white"/>
  <circle cx="40" cy="200" r="6" fill="white"/>
  <title>Time Warp IDE</title>
  </svg>
EOF
    convert /tmp/timewarp-rust-$$.svg -resize 48x48  "$ICON_DIR/48x48/apps/timewarp.png" 2>/dev/null || true
    convert /tmp/timewarp-rust-$$.svg -resize 64x64  "$ICON_DIR/64x64/apps/timewarp.png" 2>/dev/null || true
    convert /tmp/timewarp-rust-$$.svg -resize 128x128 "$ICON_DIR/128x128/apps/timewarp.png" 2>/dev/null || true
    convert /tmp/timewarp-rust-$$.svg -resize 256x256 "$ICON_DIR/256x256/apps/timewarp.png" 2>/dev/null || true
    rm -f /tmp/timewarp-rust-$$.svg
    ok "Icons installed"
  else
    warn "ImageMagick not found; skipping icon creation"
  fi
}

create_desktop() {
  info "Writing desktop entry…"
  mkdir -p "$DESKTOP_DIR"
  cat > "$DESKTOP_DIR/timewarp-ide-rust.desktop" <<EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=Time Warp IDE (Rust)
GenericName=Educational Programming Environment
Comment=Time Warp IDE with BASIC, PILOT, and Logo (Rust)
Exec=$BIN_DIR_USER/timewarp-ide-rust %F
Icon=timewarp
Terminal=false
Categories=Development;Education;IDE;
MimeType=text/x-pilot;text/x-basic;text/x-logo;
Keywords=programming;education;basic;pilot;logo;turtle;
StartupNotify=true
EOF
  chmod 644 "$DESKTOP_DIR/timewarp-ide-rust.desktop"
  ok "Desktop entry created"
  command -v update-desktop-database &>/dev/null && update-desktop-database "$DESKTOP_DIR" || true
}

create_launcher() {
  info "Creating launcher script in PATH…"
  mkdir -p "$BIN_DIR_USER" "$BIN_INSTALL"
  cat > "$BIN_DIR_USER/timewarp-ide-rust" <<EOF
#!/usr/bin/env bash
exec "$BIN_INSTALL/time-warp" "${1:-}" "$@"
EOF
  chmod 755 "$BIN_DIR_USER/timewarp-ide-rust"
  ok "Launcher created: $BIN_DIR_USER/timewarp-ide-rust"
}

install_main() {
  echo -e "${BLUE}╔════════════════════════════════════════════════╗${NC}"
  echo -e "${BLUE}║${NC}  Installing Time Warp IDE (Rust) — user scope"
  echo -e "${BLUE}╚════════════════════════════════════════════════╝${NC}"

  if [[ "$SKIP_BUILD" -ne 1 ]]; then
    info "Building release binary (cargo)…"
    (cd "$RUST_DIR" && cargo build --release)
  else
    info "Skipping build; using existing release binary"
  fi

  local BIN_SRC="$RUST_DIR/target/release/time-warp"
  if [[ ! -x "$BIN_SRC" ]]; then
    err "Release binary not found at $BIN_SRC"
    err "Run: (cd platforms/rust && cargo build --release) or omit --no-build"
    exit 1
  fi

  mkdir -p "$BIN_INSTALL"
  install -m 755 "$BIN_SRC" "$BIN_INSTALL/time-warp"
  ok "Installed binary to $BIN_INSTALL/time-warp"

  create_launcher
  create_icons
  create_desktop

  command -v gtk-update-icon-cache &>/dev/null && gtk-update-icon-cache -f -t "$ICON_DIR" || true

  echo
  ok "Installation complete"
  echo "Launch methods:"
  echo "  • Command: timewarp-ide-rust"
  echo "  • Menu:    Applications → Development/Education → Time Warp IDE (Rust)"
}

if [[ "$UNINSTALL" -eq 1 ]]; then
  uninstall
else
  install_main
fi
