# Time Warp IDE for Windows

This is a fully native Windows application, packaged as a standalone `.exe` with embedded icon and manifest. It can be distributed as a Windows installer for true OS integration.

## Features

- Native Windows GUI (eframe/egui)
- Turtle graphics canvas (500x400)
- Live console output
- File operations (Open, Save, Quick Save PNG)
- Built-in examples
- Keyboard shortcuts (Ctrl+R, Ctrl+S, Ctrl+Q)
- Embedded icon and manifest for Windows OS integration
- Optional installer for easy distribution

## Prerequisites

- Windows 10/11
- [Rust toolchain](https://rustup.rs) installed
- [MinGW](http://www.mingw.org/) for windres (resource compiler)
- [Inno Setup](https://jrsoftware.org/isinfo.php) for installer (optional)

## Build and Package

```bat
cd Windows
build_installer.bat
```

This produces `target\release\time_warp_windows.exe` with icon and manifest. To create an installer:

```bat
iscc installer.iss
```

## Distribution

- Distribute the `.exe` directly, or use the installer for a professional setup experience.
- Replace `TimeWarp.ico` with your own icon for branding.

## Project Structure

- `src/main.rs` — Main application source
- `Cargo.toml` — Build configuration
- `time_warp_windows.rc` — Resource script
- `time_warp_windows.manifest` — Windows app manifest
- `TimeWarp.ico` — App icon
- `build_installer.bat` — Build and package script
- `installer.iss` — Inno Setup installer script
- `examples/` — Example Time Warp scripts

## License

MIT

# Time Warp (Windows)

A native Windows desktop IDE for Time Warp, built with Rust and the eframe/egui cross-platform GUI framework. Features include a code editor, live turtle graphics canvas, console output, file operations, and PNG export.

## Features

- **Native Windows Application**: Built with eframe (egui) for smooth, native Windows UI
- **Code Editor**: Syntax-aware editing with line numbers and run shortcuts
- **Live Turtle Graphics**: 500×400 canvas with anti-aliased rendering
- **Console Output**: Real-time script output and error messages
- **File Operations**:
  - Open/Save TempleCode files (.tc)
  - Quick Save PNG with auto-timestamped filenames
  - Remember last save directory
  - Open exports folder in Windows Explorer
- **Examples**: Built-in example scripts (spiral, koch, tree, etc.)
- **Keyboard Shortcuts**:
  - `Ctrl+R`: Run script
  - `Ctrl+S`: Save file
  - `Ctrl+Q`: Quick Save PNG

## Prerequisites

- **Rust toolchain** (1.70+ recommended)
  - Install from: <https://rustup.rs/>
  - Verify: `rustc --version` and `cargo --version`

## Build

### Debug Build (Faster compilation)

```cmd
cd Windows
cargo build
```

Executable: `target\debug\time_warp_windows.exe`

### Release Build (Optimized)

```cmd
cd Windows
cargo build --release
```

Executable: `target\release\time_warp_windows.exe`

## Run

### From Source

```cmd
cd Windows
cargo run --release
```

### Run the Executable

```cmd
cd Windows
target\release\time_warp_windows.exe
```

Or double-click `time_warp_windows.exe` in Windows Explorer.

## Usage

1. **Write or Load Code**:
   - Type TempleCode in the editor pane
   - Or load an example from the dropdown
   - Or use File → Open to load a `.tc` file

2. **Run**:
   - Click **Run** or press `Ctrl+R`
   - Graphics appear on the canvas, console shows output

3. **Save Work**:
   - **File → Save** or `Ctrl+S`: Save code to a `.tc` file
   - **File → Save PNG...**: Export canvas as PNG with custom name
   - **File → Quick Save PNG** or `Ctrl+Q`: Auto-timestamped PNG in last folder

4. **Open Folders**:
   - **File → Open last save folder**: Opens the directory in Windows Explorer

## Building a Standalone Executable

To create a portable `.exe` (no Rust toolchain needed to run):

```cmd
cd Windows
cargo build --release --target x86_64-pc-windows-msvc
```

Copy `target\release\time_warp_windows.exe` to any Windows machine. The executable is self-contained (includes all dependencies except system DLLs).

### Optional: Strip and Compress

For a smaller binary:

```cmd
cd Windows
cargo build --release
strip target\release\time_warp_windows.exe
upx --best target\release\time_warp_windows.exe
```

(Requires `strip` from binutils and [UPX](https://upx.github.io/))

## Project Structure

```text
Windows/
├── Cargo.toml          # Dependencies and build config
├── src/
│   └── main.rs         # Windows IDE implementation
├── README.md           # This file
└── target/             # Build artifacts (generated)
```

## Supported Commands

The interpreter supports:

- **Graphics**: `FD n`, `BK n`, `LT n`, `RT n`, `PU`, `PD`, `SETXY x y`, `COLOR r g b`, `PEN RGB(r,g,b)`
- **Control**: `REPEAT n ... ENDREPEAT`, `IF condition ... ENDIF`
- **Output**: `PRINT "text"`, `PRINT expression`
- **Canvas**: `CLS`, `EXPORTPNG "file.png"`
- **Expressions**: Math (`+`, `-`, `*`, `/`, `^`, `%`), comparisons, variables

See the main project README or examples for full syntax.

## Troubleshooting

### Build Errors

- **Missing Rust**: Install from <https://rustup.rs/>
- **Linker errors**: Ensure Visual Studio C++ Build Tools are installed
  - Download from: <https://visualstudio.microsoft.com/downloads/>
  - Select "Desktop development with C++"

### Runtime Issues

- **Examples not loading**: Ensure you run from the `Windows` directory or the project root
- **Blank canvas**: Check console for script errors; ensure valid TempleCode syntax
- **File dialog crashes**: Update to latest Rust stable (`rustup update stable`)

## Differences from Other Targets

- **vs. Rust (main)**: Identical functionality; this is the Windows-specific packaging
- **vs. Browser**: Native performance, file system access, no WASM limitations
- **vs. DOS**: Full GUI, expression evaluator, nested REPEAT, PNG export

## Development

To modify the Windows IDE:

1. Edit `src/main.rs`
2. Rebuild: `cargo build --release`
3. Test: `cargo run --release`

The Windows version shares the interpreter engine with the main Rust implementation (`../Rust/src/engine.rs`), so interpreter changes are automatically inherited.

## Distribution

To distribute the Windows IDE:

1. Build release: `cargo build --release`
2. Copy `target\release\time_warp_windows.exe`
3. Optionally include:
   - `README.md` (this file)
   - `../examples/` folder (sample scripts)
4. Zip and distribute

Users can run the `.exe` directly without installing Rust or any dependencies.

## License

See the main project LICENSE file.
