@echo off
rem Build Time Warp Windows IDE
rem Prerequisites: Rust toolchain from https://rustup.rs/

echo ========================================
echo Time Warp (Windows) - Build Script
echo ========================================
echo.

rem Check if cargo is available
where cargo >nul 2>&1
if errorlevel 1 (
    echo ERROR: Cargo not found. Please install Rust from https://rustup.rs/
    exit /b 1
)

echo Rust toolchain detected.
cargo --version
echo.

rem Build release version
echo Building release version...
cargo build --release

if errorlevel 1 (
    echo.
    echo Build FAILED. Check errors above.
    exit /b 1
)

echo.
echo ========================================
echo Build SUCCESS!
echo ========================================
echo.
echo Executable: target\release\time_warp_windows.exe
echo.
echo To run:
echo   target\release\time_warp_windows.exe
echo.
echo Or from the Windows folder:
echo   cargo run --release
echo.

exit /b 0
