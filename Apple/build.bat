@echo off
REM Build script for Temple Land Apple/macOS IDE (for Windows users with cross-compilation)
if not exist Cargo.toml (
  echo Error: Cargo.toml not found. Run from the Apple folder.
  exit /b 1
)
where cargo >nul 2>nul
if errorlevel 1 (
  echo Error: cargo not found. Please install Rust (https://rustup.rs) first.
  exit /b 1
)

echo Building Temple Land Apple/macOS IDE...
cargo build --release

echo Build complete. Binary: target\release\temple_land_apple.exe
