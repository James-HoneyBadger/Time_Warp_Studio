#!/bin/bash
# Build script for Time Warp Apple/macOS IDE
set -e
if ! command -v cargo >/dev/null 2>&1; then
  echo "Error: cargo not found. Please install Rust (https://rustup.rs) first."
  exit 1
fi

echo "Building Time Warp Apple/macOS IDE..."
cargo build --release

echo "Build complete. Binary: target/release/time_warp_apple"
