# Time Warp Windows - Quick Reference

## Build & Run

### Quick Start (Windows)

```cmd
cd Windows
build.bat
target\release\time_warp_windows.exe
```

### Using Cargo Directly

```cmd
cd Windows
cargo run --release
```

## File Structure

```text
Windows/
├── Cargo.toml       # Rust dependencies and build config
├── Cargo.lock       # Dependency lock file (auto-generated)
├── build.bat        # Windows build script
├── build.sh         # Unix/cross-compile build script
├── README.md        # Full documentation
├── QUICK_START.md   # This file
├── src/
│   └── main.rs      # Windows IDE implementation
└── target/          # Build artifacts (auto-generated)
```

## Features

✅ Native Windows desktop application  
✅ Code editor with syntax highlighting  
✅ Live turtle graphics (500×400 canvas)  
✅ Console output window  
✅ File operations (Open, Save, Quick Save PNG)  
✅ Built-in examples  
✅ Keyboard shortcuts (Ctrl+R, Ctrl+S, Ctrl+Q)  
✅ Windows file dialogs (native)  
✅ Open export folder in Windows Explorer  

## Requirements

- Windows 7 or later
- (For building) Rust toolchain from <https://rustup.rs/>

## Distribution

The compiled `.exe` is standalone and can be distributed without Rust:

1. Build: `cargo build --release`
2. Copy: `target\release\time_warp_windows.exe`
3. Distribute the `.exe` file

Users can run it without installing any dependencies (Rust is only needed to build).

## Differences from Other Versions

| Feature | Windows | Rust (Linux/Mac) | Browser | DOS |
|---------|---------|------------------|---------|-----|
| Native GUI | ✅ | ✅ | ❌ (Web) | ❌ (Text VGA) |
| File System | ✅ | ✅ | ❌ | Limited |
| Expression Evaluator | ✅ | ✅ | ✅ | ❌ |
| PNG Export | ✅ | ✅ | ✅ (Download) | ✅ (BMP) |
| Nested REPEAT | ✅ | ✅ | ✅ | Limited |
| Performance | Native | Native | Good (WASM) | Slow (DOS) |

## Example Commands

### Draw a Square

```turtle
COLOR 0 255 255
REPEAT 4
  FD 80
  RT 90
ENDREPEAT
```

### Spiral

```turtle
COLOR 255 100 200
REPEAT 36
  FD 100
  RT 170
ENDREPEAT
```

### Export to PNG

```turtle
; Draw something
FD 100
RT 90
FD 100
; Save it
EXPORTPNG "my_drawing.png"
```

## Troubleshooting

**Build fails**: Install Visual Studio C++ Build Tools  
**Can't find examples**: Run from Windows folder or project root  
**Graphics not showing**: Check console for script errors  

See README.md for full documentation.
