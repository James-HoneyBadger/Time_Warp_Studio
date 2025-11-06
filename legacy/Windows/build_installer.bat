@echo off
REM Build and package Time Warp Windows IDE as a native .exe with icon and manifest
REM Requires Rust, windres (from MinGW), and Inno Setup (for installer)

REM Compile resources
windres time_warp_windows.rc -O coff -o time_warp_windows.res

REM Build release executable with embedded resources
cargo build --release --features "embed-resource"

REM Link resources (if using embed-resource crate, otherwise use linker)
REM See https://github.com/EmbarkStudios/embed-resource for details

REM Optionally, create installer using Inno Setup
REM iscc installer.iss

ECHO Build complete. Executable: target\release\time_warp_windows.exe
ECHO Installer: TempleLandSetup.exe (if Inno Setup used)
