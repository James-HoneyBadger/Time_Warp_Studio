#!/bin/bash
# Bundle Time Warp as a native macOS .app application
set -e
APP_NAME="Time Warp"
BINARY="time_warp_apple"
BUNDLE="${APP_NAME}.app"

# Build release binary
cargo build --release

# Create .app bundle structure
mkdir -p "$BUNDLE/Contents/MacOS"
mkdir -p "$BUNDLE/Contents/Resources"

# Copy binary
cp "target/release/$BINARY" "$BUNDLE/Contents/MacOS/$BINARY"

# Copy Info.plist and icon
cp Info.plist "$BUNDLE/Contents/Info.plist"
cp TimeWarp.icns "$BUNDLE/Contents/Resources/TimeWarp.icns"

echo "App bundle created: $BUNDLE"
