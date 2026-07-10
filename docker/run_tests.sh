#!/usr/bin/env bash
set -euo pipefail

IMAGE_NAME=time-warp-tests:latest

DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
PROJECT_ROOT="$DIR/.."

echo "Building Docker image ${IMAGE_NAME}..."
docker build -f "$DIR/test-runner/Dockerfile" -t "$IMAGE_NAME" "$PROJECT_ROOT"

echo "Running tests inside container..."
docker run --rm "$IMAGE_NAME"

echo "Done."
