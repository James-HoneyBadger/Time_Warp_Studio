#!/usr/bin/env bash
set -euo pipefail

# Build using a deps-only Docker image while mounting a host-cloned amiga-gcc toolchain.
# This avoids network access inside the container.
# Usage:
#   TOOLCHAIN_DIR="$HOME/amiga-gcc" ./scripts/amiga_docker_build_with_host_toolchain.sh [Time_Warp_Amiga path]

DIR="${1:-$(cd "$(dirname "$0")/.." && pwd)/Time_Warp_Amiga}"
TOOLCHAIN_DIR="${TOOLCHAIN_DIR:-}"

if ! command -v docker >/dev/null 2>&1; then
  echo "❌ Docker is required to run this script." >&2
  exit 1
fi

if [[ -z "$TOOLCHAIN_DIR" ]]; then
  echo "❌ Set TOOLCHAIN_DIR to your host amiga-gcc path (e.g., \$HOME/amiga-gcc)" >&2
  exit 2
fi

if [[ ! -d "$DIR" || ! -f "$DIR/Makefile" ]]; then
  echo "❌ Could not find Time_Warp_Amiga directory at: $DIR" >&2
  exit 2
fi

if [[ ! -d "$TOOLCHAIN_DIR" ]]; then
  echo "❌ TOOLCHAIN_DIR does not exist: $TOOLCHAIN_DIR" >&2
  exit 2
fi

cd "$DIR"

echo "🧱 Building deps-only image..."
docker build -t timewarp/amiga-deps -f Dockerfile.local-toolchain --target deps-only .

echo "🔧 Building toolchain (using host clone) and compiling app..."
docker run --rm \
  -v "$TOOLCHAIN_DIR":/opt/amiga-gcc \
  -v "$DIR":/src -w /src \
  timewarp/amiga-deps \
  bash -lc 'make -C /opt/amiga-gcc -j"$(nproc)" all && make amiga'

echo "✅ Done. Artifact: $DIR/timewarp_amiga"
