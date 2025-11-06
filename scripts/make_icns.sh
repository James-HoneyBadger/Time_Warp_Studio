#!/usr/bin/env bash
set -euo pipefail

# Convert a set of PNGs (in a folder) into an .icns file using iconutil
# Usage: ./scripts/make_icns.sh path/to/icon.iconset output/AppIcon.icns

ICONSET_DIR="${1:-packaging/macos/icon.iconset}"
OUT_ICNS="${2:-packaging/macos/resources/app.icns}"

if [[ ! -d "$ICONSET_DIR" ]]; then
  echo "Iconset directory $ICONSET_DIR does not exist. Create it with PNGs named like icon_16x16.png, icon_32x32@2x.png, etc."
  exit 1
fi

TMP_DIR="/tmp/iconset_$$"
rm -rf "$TMP_DIR"
cp -R "$ICONSET_DIR" "$TMP_DIR"

if ! command -v iconutil >/dev/null 2>&1; then
  echo "iconutil not found. Run this on macOS with Xcode command-line tools installed."
  rm -rf "$TMP_DIR"
  exit 1
fi

mkdir -p "$(dirname "$OUT_ICNS")"
iconutil -c icns "$TMP_DIR" -o "$OUT_ICNS"
rm -rf "$TMP_DIR"

echo "Created $OUT_ICNS"
