# Experimental Platform Prototypes

This directory contains experimental prototype implementations of Time Warp for various platforms. These are **not actively maintained** and are provided for historical reference and experimentation only.

**Note:** The primary and actively maintained implementation is now the Python version at the repository root. See the main README.md for current usage.

## Experimental Prototypes

- `amiga/` – AmigaOS cross-compiled port with Intuition GUI option
- `apple/` – SwiftUI-based macOS and iOS implementation  
- `dos/` – MS-DOS / DOSBox build written in portable C
- `go/` – Go command-line interpreter and GUI experiments
- `haiku/` – Native Haiku OS prototype using the Be API
- `os2/` – OS/2 Presentation Manager port
- `rust/` – Legacy egui desktop IDE and interpreter (superseded by Python)
- `web/` – Browser-based implementation (HTML5/JS)
- `win2000/` – Windows 2000 compatibility branch (Win32/C)
- `windows/` – Modern Windows launcher and WPF shell

## Status

These implementations are **experimental prototypes** and may not work correctly. They are kept for historical reference but are not recommended for actual use. The Python implementation at the repository root is the only actively maintained and supported version.

## Historical Notes

These directories were previously located at the repository root and named using the "Time Warp <Platform>" convention. The reorganization completed in November 2025 moved them into `experimental/` to clarify their status and promote the Python implementation as the primary platform.
