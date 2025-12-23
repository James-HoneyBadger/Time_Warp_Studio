# Time Warp IDE for Windows 2000

Complete native Windows 2000 implementation of Time Warp IDE - an educational programming environment supporting BASIC, PILOT, and Logo languages.

## Features

### Complete IDE

- **Multi-Document Interface (MDI)** with full window management
- **Syntax-Highlighting Editor** supporting BASIC, PILOT, and Logo
- **Turtle Graphics Canvas** with GDI rendering, zoom, pan, and export
- **Multi-Tab Console** with ANSI color support
- **Full Debugger** with breakpoints, stepping, watch windows, call stack, and memory view

### Language Support

- **BASIC**: Complete interpreter with variables, arrays, loops, conditionals, functions, string operations, math functions, and file I/O
- **PILOT**: Full implementation of all commands (T:, A:, M:, C:, etc.) with pattern matching and branching
- **Logo**: Complete turtle graphics with procedures, recursion, variables, and list operations
- **Pascal**: Added support for standard Pascal syntax and execution
- **Forth**: Added support for Forth stack-based operations

## Installation

An installer has been generated for Windows 2000.

- **Installer**: `Release/Setup.exe`
- **Standalone Executable**: `Release/TimeWarpIDE.exe`

To install on Windows 2000:
1. Copy `Setup.exe` to your Windows 2000 machine (or VM).
2. Run `Setup.exe`.
3. Follow the prompts to install to `C:\Program Files\TimeWarp` (or custom location).

## Virtual Machine Setup

Scripts are provided in the `vm/` directory to set up and run a Windows 2000 VM using QEMU.

### Prerequisites
- QEMU (installed)
- Windows 2000 ISO image (user must provide)

### Setup
1. Run the setup script:
   ```bash
   ./vm/setup_vm.sh
   ```
2. Provide the path to your Windows 2000 ISO when prompted.
3. Follow the Windows 2000 installation process in the QEMU window.

### Running
After installation, run:
```bash
./vm/run_vm.sh
```

### Development Features

- Line numbers and bookmarks
- Find and replace
- Recent files list
- Project management
- File operations (New, Open, Save, Save As)
- Export graphics to BMP/PNG
- Comprehensive keyboard shortcuts

## System Requirements

- Windows 2000, XP, or later
- Minimum 64 MB RAM
- 10 MB disk space
- VGA or better graphics (800x600 minimum)

## Building from Source

### Prerequisites

Install MinGW cross-compiler targeting Windows 2000:

```bash
# On Debian/Ubuntu
sudo apt-get install mingw-w64

# On Fedora
sudo dnf install mingw32-gcc mingw32-binutils
```

### Compilation

```bash
cd Win2K
make
```

This produces `TimeWarpIDE.exe` compatible with Windows 2000.

Or use the convenience script from the repo root to build and package with demos:

```bash
./scripts/build_win2000.sh
```

This script will:
- Generate a Windows 2000–styled `icon.ico` (if ImageMagick is available)
- Build the executable with MinGW (NT 5.0 API level)
- Package `TimeWarpIDE.exe` and `demos/` into `dist/win2000/TimeWarpIDE-win2000.zip`

### Cross-Compilation Flags

The Makefile uses these flags for Windows 2000 compatibility:

- `WINVER=0x0500` - Target Windows 2000
- `_WIN32_WINNT=0x0500` - NT 5.0 API level
- `_WIN32_IE=0x0500` - IE 5.0 common controls

## Installation

### Method 1: Manual Installation

1. Copy `TimeWarpIDE.exe` to `C:\Program Files\TimeWarp\`
2. Create desktop shortcut if desired
3. Launch the executable

### Method 2: Using Installer

Run the NSIS installer (when built):

```bash
makensis installer/timewarp.nsi
```

This creates an installer with:

- Start Menu shortcuts
- File associations for `.twb`, `.twp`, `.twl` files
- Registry entries
- Uninstaller

### Demos (Showcase)

The Windows 2000 Edition includes a `demos/` folder with OS-themed samples:

- `win2000_intro.pilot` — PILOT intro flow referencing Windows 2000
- `win2000_classic.bas` — BASIC graphics drawing concentric circles (GDI)
- `win2000_logo.logo` — Logo turtle draws Windows 2000-style colored panes

Open them via File → Open to showcase the retro environment.

## Usage

### Keyboard Shortcuts

#### File Operations

- `Ctrl+N` - New file
- `Ctrl+O` - Open file
- `Ctrl+S` - Save file
- `Ctrl+W` - Close file
- `Alt+F4` - Exit application

#### Editing

- `Ctrl+Z` - Undo
- `Ctrl+Y` - Redo
- `Ctrl+X` - Cut
- `Ctrl+C` - Copy
- `Ctrl+V` - Paste
- `Ctrl+F` - Find
- `Ctrl+H` - Replace
- `Ctrl+G` - Go to line

#### Running Code

- `F5` - Execute code
- `Shift+F5` - Stop execution
- `F9` - Toggle debug mode
- `F8` - Toggle breakpoint
- `F11` - Step into

### Example Programs

#### BASIC Example

```basic
10 PRINT "Hello from BASIC!"
20 FOR I = 1 TO 10
30   CIRCLE 320, 240, I * 10
40 NEXT I
```

#### PILOT Example

```pilot
T: Welcome to PILOT
A: name
T: Hello, $name!
```

#### Logo Example

```logo
REPEAT 4 [FORWARD 100 RIGHT 90]
```

## Architecture

### Component Structure

```
Win2K/
├── src/
│   ├── main.c              # WinMain, MDI framework, menu handling
│   ├── editor.c/.h         # Syntax-highlighting editor
│   ├── canvas.c/.h         # GDI turtle graphics canvas
│   ├── console.c/.h        # Multi-tab output console
│   ├── basic_interpreter.c/.h    # Complete BASIC interpreter
│   ├── pilot_interpreter.c/.h    # Complete PILOT interpreter
│   ├── logo_interpreter.c/.h     # Complete Logo interpreter
│   ├── debugger.c/.h       # Full debugger implementation
│   └── file_ops.c/.h       # File operation handlers
├── resources/
│   ├── timewarp.rc         # Resource definitions
│   ├── icon.ico            # Application icon
│   └── toolbar.bmp         # Toolbar images
├── installer/
│   └── timewarp.nsi        # NSIS installer script
├── docs/
│   ├── user_guide.md       # User documentation
│   ├── basic_reference.md  # BASIC language reference
│   ├── pilot_reference.md  # PILOT language reference
│   └── logo_reference.md   # Logo language reference
├── tests/
│   └── test_suite.c        # Comprehensive test suite
└── Makefile                # Build system
```

### API Design

All interpreters follow a consistent interface:

```c
BOOL <Language>Interpreter_Init(void);
void <Language>Interpreter_Cleanup(void);
BOOL <Language>Interpreter_Execute(const TCHAR *code, HWND hwndConsole,
                                   HWND hwndCanvas, BOOL debugMode);
void <Language>Interpreter_Stop(void);
```

### Graphics System

The canvas uses GDI for Windows 2000 compatibility:

- Double-buffered rendering for flicker-free graphics
- Zoom and pan with smooth scrolling
- Export to BMP format (PNG requires GDI+)
- Coordinate system: 1024x768 logical units

### Debugger Integration

The debugger hooks into all interpreters providing:

- Line-by-line execution tracking
- Breakpoint management
- Variable inspection
- Call stack visualization
- Memory view for advanced debugging

## Testing

Comprehensive test suite covers:

- All BASIC commands and functions
- All PILOT commands and pattern matching
- All Logo turtle commands and procedures
- Editor operations (syntax highlighting, find/replace, bookmarks)
- Canvas operations (drawing, zoom, pan, export)
- Debugger functionality (breakpoints, stepping, watches)
- File operations (open, save, recent files)
- MDI window management

Run tests:

```bash
make test
```

## Troubleshooting

### Common Issues

**Problem**: Application doesn't start on Windows 2000  
**Solution**: Ensure you have the latest Windows 2000 service pack (SP4) and common controls update

**Problem**: Build fails due to missing `icon.ico`  
**Solution**: Run `./scripts/make_win2000_icon.sh` to generate a placeholder icon, or place an `icon.ico` under `platforms/win2000/resources/`

**Problem**: Graphics don't display correctly  
**Solution**: Check display settings - minimum 800x600 resolution required

**Problem**: Syntax highlighting is slow  
**Solution**: This is normal for very large files (>10,000 lines); consider splitting into smaller modules

**Problem**: Can't open .twb/.twp/.twl files by double-clicking  
**Solution**: Run the installer to register file associations, or manually associate in Windows Explorer

## License

Part of the Time Warp IDE project - Educational Programming Environment  
© 2025 James Temple

## Support

For issues, questions, or contributions, please refer to the main Time Warp IDE repository.

## Version History

### 3.0.0 (2025-01-XX)

- Initial Windows 2000 native implementation
- Complete BASIC, PILOT, and Logo interpreters
- Full MDI IDE with syntax highlighting
- GDI-based turtle graphics
- Comprehensive debugger
- NSIS installer package
