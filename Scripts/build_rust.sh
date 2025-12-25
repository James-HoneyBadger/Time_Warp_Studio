#!/bin/bash
set -e

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUST_DIR="$REPO_ROOT/Platforms/Rust"

# Check for cargo
if ! command -v cargo &> /dev/null; then
    echo "Error: cargo not found. Please install Rust."
    exit 1
fi

echo "Building Time Warp IDE (Rust)..."
cd "$RUST_DIR"
cargo build --release

echo "Copying executable..."
cp target/release/tw "$REPO_ROOT/tw"

echo "Done! Executable is at $REPO_ROOT/tw"
