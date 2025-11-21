#!/usr/bin/env bash
set -euo pipefail

# Build the Amiga m68k binary using the bebbo Amiga GCC Docker image.
# This does not require a local cross toolchain.
# Usage: ./scripts/amiga_docker_build.sh [platforms/amiga path]

IMG="${DOCKER_IMAGE:-ghcr.io/bebbo/amiga-gcc:latest}"
DIR="${1:-$(cd "$(dirname "$0")/.." && pwd)/platforms/amiga}"

if ! command -v docker >/dev/null 2>&1; then
  echo "❌ Docker is required to run this script." >&2
  exit 1
fi

if [[ ! -d "$DIR" || ! -f "$DIR/Makefile" ]]; then
  echo "❌ Could not find platforms/amiga directory at: $DIR" >&2
  exit 2
fi

echo "🚀 Building Amiga binary using Docker image: $IMG"

docker run --rm -v "$DIR":/src -w /src "$IMG" make amiga

echo "✅ Done. Artifact: $DIR/timewarp_amiga"
