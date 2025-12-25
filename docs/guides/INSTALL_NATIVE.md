# Time Warp IDE - Native Installation Guide

This guide explains how to build and install the native standalone version of Time Warp IDE for Linux (ARM64 and AMD64).

## 1. Building from Source

The project includes a build script that automatically detects your architecture and builds a standalone executable using PyInstaller.

### Prerequisites
- Python 3.8 or higher
- `pip` (Python package manager)
- `dpkg` (Optional, for creating .deb packages)

### Build Instructions

1.  Open a terminal in the project root.
2.  Run the build script:
    ```bash
    ./Scripts/build_native.sh
    ```
    *Note: The script will create a temporary virtual environment and install necessary dependencies.*

3.  The build artifacts will be located in `dist/<arch>/`:
    -   **Executable:** `dist/<arch>/TimeWarpIDE`
    -   **Tarball:** `dist/<arch>/time-warp-ide_1.0.0_<arch>.tar.gz`
    -   **Debian Package:** `dist/<arch>/time-warp-ide_1.0.0_<arch>.deb` (if `dpkg-deb` is available)

### Supported Architectures
-   **Linux ARM64 (aarch64):** Run the script on a Raspberry Pi 4/5, NVIDIA Jetson, or other ARM64 Linux devices.
-   **Linux AMD64 (x86_64):** Run the script on a standard PC or laptop running Linux.

## 2. Installation

### Option A: Debian/Ubuntu (.deb)
If the `.deb` package was generated:
```bash
sudo dpkg -i dist/$(uname -m)/time-warp-ide_1.0.0_*.deb
sudo apt-get install -f  # Fix any missing dependencies
```
This will install Time Warp IDE to `/usr/local/bin` and add a desktop shortcut.

### Option B: Tarball (Universal)
1.  Extract the tarball:
    ```bash
    tar -xzf dist/$(uname -m)/time-warp-ide_1.0.0_*.tar.gz
    ```
2.  Run the install script (requires root):
    ```bash
    cd time-warp-ide_1.0.0_*
    sudo ./install.sh
    ```
    Or simply run the executable directly:
    ```bash
    ./time-warp-ide
    ```

### Option C: Arch Linux (PKGBUILD)
For Arch Linux users, a `PKGBUILD` is provided in `packaging/arch/`.
1.  Navigate to the directory:
    ```bash
    cd packaging/arch
    ```
2.  Build and install:
    ```bash
    makepkg -si
    ```

## 3. Automated Builds (GitHub Actions)

A GitHub Actions workflow (`.github/workflows/build_release.yml`) is included to automatically build the **Linux AMD64** version on every release tag (e.g., `v1.0.0`).

-   Push a tag to trigger the build:
    ```bash
    git tag v1.0.0
    git push origin v1.0.0
    ```
-   Download the artifact `time-warp-linux-amd64` from the Actions tab in GitHub.

## 4. Running the Application

After installation, you can launch Time Warp IDE from your application menu or by running:
```bash
time-warp-ide
```

## Troubleshooting

-   **"PyInstaller not found"**: The script attempts to install it, but ensure you have internet access.
-   **"dpkg-deb not found"**: Install `dpkg` package if you want a .deb file (`sudo apt install dpkg` or `sudo pacman -S dpkg`).
-   **Graphics Issues**: Ensure your system supports OpenGL/Vulkan as required by PySide6 (Qt6).
