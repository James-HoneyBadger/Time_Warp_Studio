#!/usr/bin/env bash
set -euo pipefail

# Build and package a macOS .app and signed .pkg suitable for App Store upload.
# Usage: ./scripts/build_macos_app.sh --app-name "Time Warp" --bundle-id "org.example.timewarp" \
#        --version "1.2.3" --build-version "42" --sign-identity "Apple Distribution: Name (TEAMID)" \
#        --installer-identity "3rd Party Mac Developer Installer: Name (TEAMID)" \
#        --output dist
#
# Notes:
# - This script builds Rust targets for x86_64-apple-darwin and aarch64-apple-darwin.
#   Ensure you have the targets installed (rustup target add aarch64-apple-darwin x86_64-apple-darwin)
# - You must run this on a macOS machine with Xcode command-line tools installed.
# - For the Mac App Store you will sign with an Apple Distribution certificate and upload via Xcode/Transporter.

APP_NAME="${APP_NAME:-__APP_NAME__}"
BUNDLE_ID="${BUNDLE_ID:-org.example.timewarp}"
VERSION="${VERSION:-0.0.1}"
BUILD_VERSION="${BUILD_VERSION:-1}"
EXECUTABLE_NAME="${EXECUTABLE_NAME:-time-warp}"
SIGN_IDENTITY="${SIGN_IDENTITY:-}"  # Apple Distribution identity
INSTALLER_IDENTITY="${INSTALLER_IDENTITY:-}"  # 3rd Party Mac Developer Installer
OUT_DIR="${OUT_DIR:-dist}"
RESOURCES_DIR="${RESOURCES_DIR:-packaging/macos/resources}"

# Helper: print usage
usage() {
  grep '^#' "$0" | sed -e 's/^#//'
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

# Create output dirs
mkdir -p "$OUT_DIR"
BUILD_DIR="$OUT_DIR/build"
APP_DIR="$OUT_DIR/${APP_NAME}.app"
CONTENTS_DIR="$APP_DIR/Contents"
MACOS_DIR="$CONTENTS_DIR/MacOS"
RES_DIR="$CONTENTS_DIR/Resources"
rm -rf "$BUILD_DIR" "$APP_DIR"
mkdir -p "$MACOS_DIR" "$RES_DIR"

# Build Rust targets
echo "Building Rust release for x86_64 and arm64 (aarch64)"
if ! command -v cargo >/dev/null 2>&1; then
  echo "cargo not found. Install Rust toolchain first."
  exit 1
fi

TARGET_X86="x86_64-apple-darwin"
TARGET_ARM="aarch64-apple-darwin"

# Ensure targets installed
if ! rustup target list --installed | grep -q "$TARGET_X86"; then
  echo "Installing target $TARGET_X86"
  rustup target add "$TARGET_X86"
fi
if ! rustup target list --installed | grep -q "$TARGET_ARM"; then
  echo "Installing target $TARGET_ARM"
  rustup target add "$TARGET_ARM"
fi

# Build both
cargo build --release --target "$TARGET_X86"
cargo build --release --target "$TARGET_ARM"

BIN_X86="target/$TARGET_X86/release/$EXECUTABLE_NAME"
BIN_ARM="target/$TARGET_ARM/release/$EXECUTABLE_NAME"
BIN_UNIVERSAL="$MACOS_DIR/$EXECUTABLE_NAME"

if [[ ! -f "$BIN_X86" ]]; then
  echo "Missing build: $BIN_X86"
  exit 1
fi
if [[ ! -f "$BIN_ARM" ]]; then
  echo "Missing build: $BIN_ARM"
  exit 1
fi

# Create universal binary
if command -v lipo >/dev/null 2>&1; then
  echo "Creating universal binary"
  lipo -create -output "$BIN_UNIVERSAL" "$BIN_X86" "$BIN_ARM"
else
  echo "lipo not found; copying x86 binary as fallback"
  cp "$BIN_X86" "$BIN_UNIVERSAL"
fi
chmod +x "$BIN_UNIVERSAL"

# Prepare Info.plist from template
INFOTPL="packaging/macos/Info.plist"
if [[ ! -f "$INFOTPL" ]]; then
  echo "Missing Info.plist template at $INFOTPL"
  exit 1
fi

TMP_INFO="$BUILD_DIR/Info.plist"
mkdir -p "$BUILD_DIR"
sed \
  -e "s|__APP_NAME__|${APP_NAME}|g" \
  -e "s|__BUNDLE_ID__|${BUNDLE_ID}|g" \
  -e "s|__BUILD_VERSION__|${BUILD_VERSION}|g" \
  -e "s|__VERSION__|${VERSION}|g" \
  -e "s|__EXECUTABLE__|${EXECUTABLE_NAME}|g" \
  "$INFOTPL" > "$TMP_INFO"

cp "$TMP_INFO" "$CONTENTS_DIR/Info.plist"

# Copy resources (icns, other assets)
if [[ -d "$RESOURCES_DIR" ]]; then
  cp -R "$RESOURCES_DIR/"* "$RES_DIR/" || true
else
  echo "No resources found at $RESOURCES_DIR (app icon: app.icns recommended)"
fi

# Sign the app (required for App Store)
if [[ -z "$SIGN_IDENTITY" || -z "$INSTALLER_IDENTITY" ]]; then
  echo "WARNING: SIGN_IDENTITY or INSTALLER_IDENTITY not set. Skipping codesign/productbuild steps."
  echo "Set SIGN_IDENTITY and INSTALLER_IDENTITY environment variables to sign and build the pkg."
  echo "App bundle is at: $APP_DIR"
  exit 0
fi

ENTITLEMENTS="packaging/macos/entitlements.plist"
if [[ ! -f "$ENTITLEMENTS" ]]; then
  echo "Missing entitlements at $ENTITLEMENTS"
  exit 1
fi

echo "Codesigning app with identity: $SIGN_IDENTITY"
# Deep sign and enable hardened runtime
codesign --verbose --deep --force --options runtime \
  --entitlements "$ENTITLEMENTS" -s "$SIGN_IDENTITY" "$APP_DIR"

# Verify codesign
codesign --verify --deep --strict --verbose=2 "$APP_DIR"

# Build signed installer pkg suitable for upload via Xcode/Transporter
PKG_PATH="$OUT_DIR/${APP_NAME}-${VERSION}.pkg"
productbuild --component "$APP_DIR" /Applications --sign "$INSTALLER_IDENTITY" "$PKG_PATH"

echo "Built signed pkg: $PKG_PATH"

echo "Next steps:"
echo "  - Open Xcode and use Organizer -> Distribute App to upload to the Mac App Store"
echo "  - Or use Transporter to upload the pkg to App Store Connect"

echo "Done."
