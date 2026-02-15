#!/bin/bash
set -e

# Time Warp Studio - Native Build Script
# Builds a standalone executable for the current architecture using PyInstaller
# and packages it into a .deb file.

# 1. Detect Architecture
ARCH=$(uname -m)
if [ "$ARCH" == "x86_64" ]; then
    DEB_ARCH="amd64"
elif [ "$ARCH" == "aarch64" ]; then
    DEB_ARCH="arm64"
elif [ "$ARCH" == "armv7l" ]; then
    DEB_ARCH="armhf"
else
    echo "Warning: Unknown architecture $ARCH. Defaulting to $ARCH."
    DEB_ARCH=$ARCH
fi

echo "=========================================="
echo "Building Time Warp Studio for $ARCH ($DEB_ARCH)"
echo "=========================================="

# 2. Setup Paths
REPO_ROOT=$(pwd)
PYTHON_SOURCE="$REPO_ROOT/Platforms/Python"
ENTRY_POINT="$PYTHON_SOURCE/time_warp_ide.py"
DIST_DIR="$REPO_ROOT/dist/$ARCH"
BUILD_DIR="$REPO_ROOT/build/$ARCH"
VERSION="7.0.0"

# 3. Check Dependencies and Setup Venv
# In CI environments, skip venv creation since dependencies are pre-installed
if [ -z "$CI" ]; then
    VENV_DIR="$REPO_ROOT/.build_venv"
    if [ ! -d "$VENV_DIR" ]; then
        echo "Creating virtual environment..."
        python3 -m venv "$VENV_DIR"
    fi

    source "$VENV_DIR/bin/activate"

    echo "Installing dependencies..."
    pip install --upgrade pip
    pip install pyinstaller
    # Install the project in editable mode to ensure PyInstaller can find all modules
    pip install -e "$PYTHON_SOURCE"
else
    echo "CI environment detected, skipping venv setup..."
fi

# 4. Clean previous builds
rm -rf "$DIST_DIR" "$BUILD_DIR"
mkdir -p "$DIST_DIR"

# 5. Run PyInstaller
# We set PYTHONPATH so PyInstaller can find the 'time_warp' package
export PYTHONPATH="$PYTHON_SOURCE:$PYTHONPATH"

ICON_PATH="$REPO_ROOT/packaging/linux/icon.png"
ICON_ARG=""
if [ -f "$ICON_PATH" ]; then
    ICON_ARG="--icon=$ICON_PATH"
fi

echo "Compiling to binary..."
pyinstaller --clean --noconfirm --onefile --windowed \
    --name TimeWarpIDE \
    --distpath "$DIST_DIR" \
    --workpath "$BUILD_DIR" \
    --paths "$PYTHON_SOURCE" \
    --hidden-import="PySide6" \
    --hidden-import="PIL" \
    --hidden-import="logging.handlers" \
    --hidden-import="getpass" \
    --hidden-import="asyncio" \
    --collect-all="time_warp" \
    $ICON_ARG \
    "$ENTRY_POINT"

EXECUTABLE="$DIST_DIR/TimeWarpIDE"

if [ ! -f "$EXECUTABLE" ]; then
    echo "Error: Build failed. Executable not found."
    exit 1
fi

echo "Build successful: $EXECUTABLE"

# 6. Create Distribution Packages
echo "Creating distribution packages..."

# Create Tarball
TARBALL_NAME="time-warp-ide_${VERSION}_${DEB_ARCH}.tar.gz"
TARBALL_DIR="$DIST_DIR/time-warp-ide_${VERSION}_${DEB_ARCH}"
mkdir -p "$TARBALL_DIR"
cp "$EXECUTABLE" "$TARBALL_DIR/time-warp-ide"
cp "$REPO_ROOT/README.md" "$TARBALL_DIR/"
cp "$REPO_ROOT/LICENSE" "$TARBALL_DIR/" 2>/dev/null || touch "$TARBALL_DIR/LICENSE"
cp "$REPO_ROOT/packaging/linux/time-warp-ide.desktop" "$TARBALL_DIR/" 2>/dev/null || true
cp "$REPO_ROOT/packaging/linux/icon.png" "$TARBALL_DIR/" 2>/dev/null || true

# Create simple install script for tarball
cat > "$TARBALL_DIR/install.sh" <<EOF
#!/bin/bash
# Simple install script
if [ "\$(id -u)" -ne 0 ]; then
    echo "Please run as root"
    exit 1
fi

echo "Installing Time Warp Studio..."

# Install executable
cp time-warp-ide /usr/local/bin/
chmod +x /usr/local/bin/time-warp-ide

# Install icon
mkdir -p /usr/share/icons/hicolor/256x256/apps
cp icon.png /usr/share/icons/hicolor/256x256/apps/time-warp-ide.png

# Install desktop file
mkdir -p /usr/share/applications
cp time-warp-ide.desktop /usr/share/applications/

# Update icon cache if possible
if command -v gtk-update-icon-cache &> /dev/null; then
    gtk-update-icon-cache /usr/share/icons/hicolor
fi

echo "Success! Installed to /usr/local/bin/time-warp-ide"
EOF
chmod +x "$TARBALL_DIR/install.sh"

tar -czf "$DIST_DIR/$TARBALL_NAME" -C "$DIST_DIR" "time-warp-ide_${VERSION}_${DEB_ARCH}"
echo "Tarball created: $DIST_DIR/$TARBALL_NAME"

# Create Debian Package (if dpkg-deb is available)
if command -v dpkg-deb &> /dev/null; then
    echo "Creating Debian package..."
    DEB_BUILD_DIR="$BUILD_DIR/deb/time-warp-ide_${VERSION}_${DEB_ARCH}"
    mkdir -p "$DEB_BUILD_DIR/usr/local/bin"
    mkdir -p "$DEB_BUILD_DIR/usr/share/applications"
    mkdir -p "$DEB_BUILD_DIR/usr/share/icons/hicolor/256x256/apps"
    mkdir -p "$DEB_BUILD_DIR/DEBIAN"

    # Copy executable
    cp "$EXECUTABLE" "$DEB_BUILD_DIR/usr/local/bin/time-warp-ide"
    chmod 755 "$DEB_BUILD_DIR/usr/local/bin/time-warp-ide"

    # Create Control file
    cat > "$DEB_BUILD_DIR/DEBIAN/control" <<EOF
Package: time-warp-ide
Version: $VERSION
Section: devel
Priority: optional
Architecture: $DEB_ARCH
Maintainer: James Temple <james@honey-badger.org>
Description: Time Warp Studio
 Educational multi-language programming environment.
 Unifies BASIC, PILOT, Logo, Pascal, and Prolog.
 Native standalone build.
EOF

    # Create Desktop entry
    cat > "$DEB_BUILD_DIR/usr/share/applications/time-warp-ide.desktop" <<EOF
[Desktop Entry]
Name=Time Warp Studio
Comment=Educational Programming Environment
Exec=/usr/local/bin/time-warp-ide
Icon=time-warp-ide
Terminal=false
Type=Application
Categories=Development;Education;
EOF

    # Build .deb
    dpkg-deb --build "$DEB_BUILD_DIR" "$DIST_DIR/time-warp-ide_${VERSION}_${DEB_ARCH}.deb"
    echo "Debian Package: $DIST_DIR/time-warp-ide_${VERSION}_${DEB_ARCH}.deb"
else
    echo "Warning: dpkg-deb not found. Skipping Debian package creation."
    echo "To build .deb, install dpkg (e.g., sudo apt install dpkg or sudo pacman -S dpkg)"
fi

echo "=========================================="
echo "Packaging Complete!"
echo "Executable: $EXECUTABLE"
echo "Tarball: $DIST_DIR/$TARBALL_NAME"
echo "=========================================="
