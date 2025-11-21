#!/usr/bin/env bash
# Build Time Warp Windows IDE (cross-compile from Linux/Mac or build on Windows)
set -e

echo "========================================"
echo "Time Warp (Windows) - Build Script"
echo "========================================"
echo ""

# Check if cargo is available
if ! command -v cargo &> /dev/null; then
    echo "ERROR: Cargo not found. Please install Rust from https://rustup.rs/"
    exit 1
fi

echo "Rust toolchain detected:"
cargo --version
echo ""

# Check if we're cross-compiling or building natively
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
    echo "Building natively on Windows..."
    TARGET=""
else
    echo "Cross-compiling for Windows from $OSTYPE..."
    # Check if Windows target is installed
    if ! rustup target list --installed | grep -q "x86_64-pc-windows-gnu"; then
        echo "Installing Windows target..."
        rustup target add x86_64-pc-windows-gnu
    fi
    TARGET="--target x86_64-pc-windows-gnu"
fi

# Build release version
echo "Building release version..."
cargo build --release $TARGET

if [ $? -ne 0 ]; then
    echo ""
    echo "Build FAILED. Check errors above."
    exit 1
fi

echo ""
echo "========================================"
echo "Build SUCCESS!"
echo "========================================"
echo ""

if [ -z "$TARGET" ]; then
    echo "Executable: target/release/time_warp_windows.exe"
else
    echo "Executable: target/x86_64-pc-windows-gnu/release/time_warp_windows.exe"
fi

echo ""
echo "To run (on Windows):"
echo "  cargo run --release"
echo ""
