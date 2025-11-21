#!/usr/bin/env bash
set -euo pipefail

# Build a local Docker image with the Amiga toolchain from source and compile the project.
# Useful when GHCR pull is denied or unavailable.
# Usage: ./scripts/amiga_docker_build_local.sh [platforms/amiga path]

DIR="${1:-$(cd "$(dirname "$0")/.." && pwd)/platforms/amiga}"
IMG="${LOCAL_IMAGE:-timewarp/amiga-toolchain:local}"

if ! command -v docker >/dev/null 2>&1; then
  echo "❌ Docker is required to run this script." >&2
  exit 1
fi

if [[ ! -d "$DIR" || ! -f "$DIR/Makefile" ]]; then
  echo "❌ Could not find platforms/amiga directory at: $DIR" >&2
  exit 2
fi

cd "$DIR"
echo "🛠️  Building local Amiga toolchain image: $IMG (this can take a while on first run)"
docker build -t "$IMG" -f Dockerfile.local-toolchain .

echo "🚀 Building Amiga binary inside the image..."
docker run --rm -v "$DIR":/src -w /src "$IMG" make amiga

echo "✅ Done. Artifact: $DIR/timewarp_amiga"
