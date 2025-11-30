#!/usr/bin/env bash
set -euo pipefail

# Generate a Windows .ico for the Win2000 build using ImageMagick

ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
RES_DIR="$ROOT/platforms/win2000/resources"
SVG="/tmp/timewarp-win2k-$$.svg"

if ! command -v convert >/dev/null 2>&1; then
  echo "ImageMagick 'convert' not found. Install it and retry." >&2; exit 1
fi

mkdir -p "$RES_DIR"

cat > "$SVG" << 'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<svg width="256" height="256" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <linearGradient id="grad" x1="0%" y1="0%" x2="100%" y2="100%">
      <stop offset="0%" style="stop-color:#0050ef;stop-opacity:1" />
      <stop offset="100%" style="stop-color:#0078d7;stop-opacity:1" />
    </linearGradient>
  </defs>
  <rect width="256" height="256" rx="32" fill="url(#grad)"/>
  <text x="128" y="150" font-family="Tahoma, Verdana, sans-serif" font-size="110" font-weight="bold" text-anchor="middle" fill="white">TW</text>
  <rect x="28" y="28" width="60" height="60" fill="#e61e26"/>
  <rect x="168" y="28" width="60" height="60" fill="#f2c100"/>
  <rect x="28" y="168" width="60" height="60" fill="#14a44d"/>
  <rect x="168" y="168" width="60" height="60" fill="#0078d7"/>
</svg>
EOF

convert "$SVG" -define icon:auto-resize=256,128,64,48,32,16 "$RES_DIR/icon.ico"
rm -f "$SVG"
echo "Wrote $RES_DIR/icon.ico"
