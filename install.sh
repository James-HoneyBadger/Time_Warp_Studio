#!/usr/bin/env bash
# =============================================================================
#  Time Warp Studio — Standalone Installation Script
#  Version: auto-detected from VERSION file
#
#  Creates a fully self-contained installation with:
#    • A dedicated Python virtual environment
#    • Command-line launcher:  time-warp-studio
#    • Desktop menu entry:     Applications ▸ Development ▸ Time Warp Studio
#
#  Re-running this script UPGRADES / REPLACES any existing installation.
#
#  Usage:
#    sudo ./install.sh              System-wide install to /opt (recommended)
#    sudo ./install.sh --upgrade    Force reinstall of all dependencies too
#    ./install.sh --user            Per-user install to ~/.local (no sudo)
#    ./install.sh --user --upgrade  Per-user upgrade/force-reinstall
#    sudo ./install.sh --uninstall  Remove a system-wide installation
#    ./install.sh --user --uninstall  Remove a per-user installation
# =============================================================================

set -euo pipefail

# ── Version ──────────────────────────────────────────────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VERSION="$(cat "$SCRIPT_DIR/VERSION" 2>/dev/null || echo "9.0.0")"
APP_NAME="time-warp-studio"
APP_DISPLAY_NAME="Time Warp Studio"

# ── Colour helpers ────────────────────────────────────────────────────────────
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
BLUE='\033[0;34m'; BOLD='\033[1m'; RESET='\033[0m'

info()    { echo -e "${BLUE}ℹ️  $*${RESET}"; }
success() { echo -e "${GREEN}✅ $*${RESET}"; }
warn()    { echo -e "${YELLOW}⚠️  $*${RESET}"; }
error()   { echo -e "${RED}❌ $*${RESET}" >&2; }
header()  { echo -e "\n${BOLD}${BLUE}$*${RESET}\n"; }

# ── Argument parsing ──────────────────────────────────────────────────────────
USER_INSTALL=false
UNINSTALL=false
FORCE_REINSTALL=false   # --upgrade: wipe and recreate the venv
for arg in "$@"; do
    case "$arg" in
        --user)          USER_INSTALL=true ;;
        --uninstall)     UNINSTALL=true ;;
        --upgrade)       FORCE_REINSTALL=true ;;
        --help|-h)
            cat << 'HELP'
Usage: ./install.sh [OPTIONS]

Options:
  (none)       System-wide install into /opt  (requires sudo)
  --user       Per-user install into ~/.local  (no sudo needed)
  --upgrade    Force wipe and reinstall of the Python venv
  --uninstall  Remove Time Warp Studio completely
  --help       Show this help message

Examples:
  sudo ./install.sh              # system install
  sudo ./install.sh --upgrade    # upgrade existing system install
  ./install.sh --user            # per-user install (no sudo)
  ./install.sh --user --upgrade  # upgrade per-user install
  sudo ./install.sh --uninstall  # remove system install
HELP
            exit 0 ;;
        *) error "Unknown option: $arg"; exit 1 ;;
    esac
done

# ── Determine install paths ───────────────────────────────────────────────────
if $USER_INSTALL; then
    APP_DIR="$HOME/.local/share/$APP_NAME"
    BIN_DIR="$HOME/.local/bin"
    DESKTOP_DIR="$HOME/.local/share/applications"
    ICON_DIR="$HOME/.local/share/icons/hicolor/256x256/apps"
    NEED_SUDO=false
else
    APP_DIR="/opt/$APP_NAME"
    BIN_DIR="/usr/local/bin"
    DESKTOP_DIR="/usr/share/applications"
    ICON_DIR="/usr/share/icons/hicolor/256x256/apps"
    NEED_SUDO=true
fi

LAUNCHER="$BIN_DIR/$APP_NAME"
DESKTOP_FILE="$DESKTOP_DIR/$APP_NAME.desktop"
ICON_FILE="$ICON_DIR/$APP_NAME.png"
VENV_DIR="$APP_DIR/.venv"

# ── Permission check ──────────────────────────────────────────────────────────
if $NEED_SUDO && [[ $EUID -ne 0 ]]; then
    error "System-wide installation requires root privileges."
    error "Run:  sudo ./install.sh"
    error "Or for a per-user install:  ./install.sh --user"
    exit 1
fi

# ════════════════════════════════════════════════════════════════════════════════
#  UNINSTALL
# ════════════════════════════════════════════════════════════════════════════════
if $UNINSTALL; then
    header "Uninstalling $APP_DISPLAY_NAME $VERSION"

    # Remove both system and user locations in one pass
    PATHS_TO_REMOVE=(
        "/opt/$APP_NAME"
        "/usr/local/bin/$APP_NAME"
        "/usr/share/applications/$APP_NAME.desktop"
        "/usr/share/icons/hicolor/256x256/apps/$APP_NAME.png"
        "$HOME/.local/share/$APP_NAME"
        "$HOME/.local/bin/$APP_NAME"
        "$HOME/.local/share/applications/$APP_NAME.desktop"
        "$HOME/.local/share/icons/hicolor/256x256/apps/$APP_NAME.png"
        # Legacy name from packaging/linux original
        "/usr/local/bin/time-warp-ide"
        "/usr/share/applications/time-warp-ide.desktop"
    )
    for path in "${PATHS_TO_REMOVE[@]}"; do
        if [[ -e "$path" ]]; then
            rm -rf "$path"
            info "Removed $path"
        fi
    done

    # Refresh desktop and icon caches
    for dir in "/usr/share/applications" "$HOME/.local/share/applications"; do
        update-desktop-database "$dir" 2>/dev/null || true
    done
    for icon_root in "/usr/share/icons/hicolor" "$HOME/.local/share/icons/hicolor"; do
        gtk-update-icon-cache -f -t "$icon_root" 2>/dev/null || true
    done

    success "$APP_DISPLAY_NAME has been completely removed."
    exit 0
fi

# ════════════════════════════════════════════════════════════════════════════════
#  INSTALL / UPGRADE
# ════════════════════════════════════════════════════════════════════════════════
IS_UPGRADE=false
if [[ -d "$APP_DIR" ]]; then
    IS_UPGRADE=true
fi

if $IS_UPGRADE; then
    header "Upgrading $APP_DISPLAY_NAME to $VERSION"
else
    header "Installing $APP_DISPLAY_NAME $VERSION"
fi

if $USER_INSTALL; then
    info "Mode: per-user install  →  $APP_DIR"
else
    info "Mode: system-wide install  →  $APP_DIR  (running as root)"
fi

# ── 1. Check Python ───────────────────────────────────────────────────────────
info "Checking Python version…"
PYTHON_BIN=""
for candidate in python3 python3.14 python3.13 python3.12 python3.11 python3.10; do
    if command -v "$candidate" &>/dev/null; then
        ver=$("$candidate" -c "import sys; print(sys.version_info[0]*100+sys.version_info[1])")
        if [[ "$ver" -ge 310 ]]; then
            PYTHON_BIN="$candidate"
            break
        fi
    fi
done

if [[ -z "$PYTHON_BIN" ]]; then
    error "Python 3.10 or newer is required but was not found."
    error "Install it:  sudo dnf install python3   (Fedora)"
    error "             sudo apt install python3    (Ubuntu/Debian)"
    exit 1
fi
success "Found $($PYTHON_BIN --version)"

# ── 2. Check pip and venv availability ───────────────────────────────────────
info "Checking pip and venv modules…"
if ! "$PYTHON_BIN" -m pip --version &>/dev/null; then
    error "pip is not available for $PYTHON_BIN"
    error "Install:  sudo dnf install python3-pip   (Fedora)"
    error "          sudo apt install python3-pip    (Ubuntu/Debian)"
    exit 1
fi
if ! "$PYTHON_BIN" -m venv --help &>/dev/null; then
    error "venv module not available for $PYTHON_BIN"
    error "Install:  sudo dnf install python3-venv  (Fedora)"
    error "          sudo apt install python3-venv   (Ubuntu/Debian)"
    exit 1
fi
success "pip and venv are available"

# ── 3. Copy / update application files ────────────────────────────────────────
info "Syncing application files to $APP_DIR…"
mkdir -p "$APP_DIR"

# rsync is ideal for upgrades: only changed files are transferred
if command -v rsync &>/dev/null; then
    rsync -a --delete \
        --exclude='.venv' \
        --exclude='__pycache__' \
        --exclude='*.pyc' \
        --exclude='.git' \
        --exclude='htmlcov' \
        --exclude='test_reports' \
        --exclude='.pytest_cache' \
        --exclude='.ruff_cache' \
        --exclude='.mypy_cache' \
        "$SCRIPT_DIR/" "$APP_DIR/"
else
    cp -r "$SCRIPT_DIR/." "$APP_DIR/"
    for d in .venv __pycache__ .git htmlcov test_reports .pytest_cache .ruff_cache .mypy_cache; do
        find "$APP_DIR" -name "$d" -type d -exec rm -rf {} + 2>/dev/null || true
    done
    find "$APP_DIR" -name '*.pyc' -delete 2>/dev/null || true
fi
success "Application files synced"

# ── 4. Create or upgrade virtual environment ──────────────────────────────────
if $FORCE_REINSTALL && [[ -d "$VENV_DIR" ]]; then
    info "Removing existing virtual environment (--upgrade)…"
    rm -rf "$VENV_DIR"
fi

if [[ ! -d "$VENV_DIR" ]]; then
    info "Creating virtual environment at $VENV_DIR…"
    "$PYTHON_BIN" -m venv "$VENV_DIR"
    success "Virtual environment created"
else
    info "Reusing existing virtual environment at $VENV_DIR"
fi

VENV_PYTHON="$VENV_DIR/bin/python"

# ── 5. Install / upgrade dependencies ─────────────────────────────────────────
info "Upgrading pip inside venv…"
"$VENV_PYTHON" -m pip install --quiet --upgrade pip

info "Installing core dependencies (PySide6, Pillow)…"
"$VENV_PYTHON" -m pip install --quiet --upgrade "PySide6>=6.5.0" "Pillow>=10.0.0"
success "Core dependencies installed"

info "Installing optional dependencies…"
"$VENV_PYTHON" -m pip install --quiet --upgrade \
    "requests>=2.31.0" "websockets>=11.0.3" 2>/dev/null || \
    warn "Some optional dependencies not installed — app will still work"

# ── 6. Fix permissions (system-wide installs only) ────────────────────────────
if ! $USER_INSTALL; then
    find "$APP_DIR" -type d -exec chmod 755 {} +
    find "$APP_DIR" -type f -exec chmod 644 {} +
    chmod 755 "$APP_DIR/install.sh" "$APP_DIR/run.py" 2>/dev/null || true
    find "$VENV_DIR/bin" -type f -exec chmod 755 {} + 2>/dev/null || true
fi

# ── 7. Install icon ───────────────────────────────────────────────────────────
info "Installing icon…"
mkdir -p "$ICON_DIR"
SOURCE_ICON="$APP_DIR/packaging/linux/icon.png"
if [[ -f "$SOURCE_ICON" ]]; then
    cp "$SOURCE_ICON" "$ICON_FILE"
    success "Icon installed"
else
    warn "Icon not found at $SOURCE_ICON — the menu entry will use a generic icon"
fi

# ── 8. Write command-line launcher ────────────────────────────────────────────
info "Writing command-line launcher at $LAUNCHER…"
mkdir -p "$BIN_DIR"
cat > "$LAUNCHER" << LAUNCHER_SCRIPT
#!/usr/bin/env bash
# Time Warp Studio launcher — written by install.sh $VERSION
APP_DIR="$APP_DIR"
VENV_PYTHON="\$APP_DIR/.venv/bin/python"

if [[ ! -x "\$VENV_PYTHON" ]]; then
    echo "\u274c Time Warp Studio installation is broken or incomplete." >&2
    echo "   Re-run the installer from: $APP_DIR/install.sh" >&2
    exit 1
fi

exec "\$VENV_PYTHON" "\$APP_DIR/Platforms/Python/time_warp_ide.py" "\$@"
LAUNCHER_SCRIPT
chmod 755 "$LAUNCHER"
success "Launcher written: $LAUNCHER"

# ── 9. Warn if ~/.local/bin is not on PATH ────────────────────────────────────
if $USER_INSTALL && [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    warn "\$HOME/.local/bin is not in your PATH."
    warn "Add this line to ~/.bashrc (or ~/.profile on non-bash shells):"
    warn "    export PATH=\"\$HOME/.local/bin:\$PATH\""
    warn "Then run:  source ~/.bashrc"
fi

# ── 10. Write .desktop menu entry ─────────────────────────────────────────────
info "Writing desktop menu entry…"
mkdir -p "$DESKTOP_DIR"
cat > "$DESKTOP_FILE" << DESKTOP_ENTRY
[Desktop Entry]
Version=1.1
Type=Application
Name=$APP_DISPLAY_NAME
GenericName=Multi-Language Programming IDE
Comment=Educational programming environment — 24 languages, turtle graphics
Exec=$LAUNCHER %f
Icon=$APP_NAME
Terminal=false
Categories=Development;Education;IDE;
Keywords=programming;education;IDE;BASIC;Logo;Pascal;Prolog;PILOT;Forth;Python;Lua;Scheme;Haskell;Smalltalk;
StartupNotify=true
StartupWMClass=TimeWarpStudio
MimeType=text/x-basic;text/x-pascal;text/x-prolog;application/x-logo;
DESKTOP_ENTRY

if ! $USER_INSTALL; then
    chmod 644 "$DESKTOP_FILE"
fi
success "Desktop entry written: $DESKTOP_FILE"

# ── 11. Refresh desktop and icon caches ───────────────────────────────────────
info "Refreshing desktop database and icon cache…"
update-desktop-database "$DESKTOP_DIR" 2>/dev/null || true
ICON_THEME_DIR="$(dirname "$(dirname "$(dirname "$ICON_DIR")")")"
gtk-update-icon-cache -f -t "$ICON_THEME_DIR" 2>/dev/null || true

# ── 12. Verify ────────────────────────────────────────────────────────────────
info "Verifying installation…"
if "$VENV_PYTHON" -c "import PySide6; import PIL" &>/dev/null; then
    success "Verification passed — dependencies are importable"
else
    warn "Could not fully verify (PySide6 may need a display — normal in headless/SSH sessions)"
fi

# ══════════════════════════════════════════════════════════════════════════════
#  SUMMARY
# ══════════════════════════════════════════════════════════════════════════════
ACTION_WORD="Installed"
$IS_UPGRADE && ACTION_WORD="Upgraded"

echo ""
echo -e "${BOLD}${GREEN}════════════════════════════════════════════════════${RESET}"
echo -e "${BOLD}${GREEN}  $APP_DISPLAY_NAME $VERSION — $ACTION_WORD!${RESET}"
echo -e "${BOLD}${GREEN}════════════════════════════════════════════════════${RESET}"
echo ""
echo -e "  ${BOLD}Start from terminal:${RESET}   $APP_NAME"
echo -e "  ${BOLD}Start from menu:${RESET}       Applications \u25b8 Development \u25b8 $APP_DISPLAY_NAME"
echo -e "  ${BOLD}Installed to:${RESET}          $APP_DIR"
echo -e "  ${BOLD}Python runtime:${RESET}        $VENV_PYTHON"
echo ""
if $USER_INSTALL; then
    echo -e "  ${BOLD}Upgrade later:${RESET}         $APP_DIR/install.sh --user --upgrade"
    echo -e "  ${BOLD}Uninstall:${RESET}             $APP_DIR/install.sh --user --uninstall"
else
    echo -e "  ${BOLD}Upgrade later:${RESET}         sudo $APP_DIR/install.sh --upgrade"
    echo -e "  ${BOLD}Uninstall:${RESET}             sudo $APP_DIR/install.sh --uninstall"
fi
