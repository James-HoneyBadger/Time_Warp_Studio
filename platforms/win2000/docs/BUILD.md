# Building Time Warp IDE for Windows 2000

Complete build instructions for creating the Windows 2000 native executable.

## Prerequisites

### Linux Build Host

Install MinGW cross-compiler:

```bash
# Debian/Ubuntu
sudo apt-get install mingw-w64 mingw-w64-tools

# Fedora/RHEL
sudo dnf install mingw32-gcc mingw32-binutils mingw32-winpthreads-static

# Arch Linux
sudo pacman -S mingw-w64-gcc
```

### Windows Build Host

Install MinGW-w64:

1. Download from [mingw-w64.org](https://www.mingw-w64.org/downloads/)
2. Install to `C:\mingw-w64\`
3. Add `C:\mingw-w64\bin` to PATH

## Building

### On Linux (Cross-Compile)

```bash
cd /home/james/Temple_Code/Win2K
make
```

This produces `TimeWarpIDE.exe` (approximately 500 KB).

### On Windows (Native)

```cmd
cd C:\path\to\Win2K
mingw32-make
```

## Makefile Targets

```bash
make              # Build executable
make clean        # Remove build artifacts
make install      # Build installer (requires NSIS)
```

## Build Output

Successful build produces:

```
TimeWarpIDE.exe       # Main executable (~500 KB)
src/*.o               # Object files
resources/*.res.o     # Compiled resources
```

## Creating Installer

Requires NSIS (Nullsoft Scriptable Install System):

```bash
# Install NSIS
sudo apt-get install nsis    # Linux
# Or download from https://nsis.sourceforge.io/  # Windows

# Build installer
cd installer
makensis timewarp.nsi

# Output: TimeWarpIDE-Setup-3.0.0.exe (~600 KB)
```

## Testing the Build

### On Wine (Linux)

```bash
wine TimeWarpIDE.exe
```

### On Windows 2000/XP VM

1. Copy `TimeWarpIDE.exe` to VM
2. Run directly (no installation needed)
3. Or run installer for full installation

## Build Configuration

### Compiler Flags

```makefile
CFLAGS = -DWINVER=0x0500 -D_WIN32_WINNT=0x0500 -D_WIN32_IE=0x0500 -Wall -O2 -mwindows
```

- `WINVER=0x0500`: Windows 2000 API level
- `_WIN32_WINNT=0x0500`: NT 5.0 features
- `_WIN32_IE=0x0500`: IE 5.0 common controls
- `-Wall`: All warnings enabled
- `-O2`: Optimization level 2
- `-mwindows`: GUI subsystem (no console)

### Linked Libraries

```makefile
LIBS = -lcomctl32 -lcomdlg32 -lgdi32 -luser32 -lkernel32
```

- `comctl32`: Common controls (toolbars, status bars)
- `comdlg32`: Common dialogs (Open, Save)
- `gdi32`: GDI graphics
- `user32`: User interface
- `kernel32`: Core Windows API

## Troubleshooting

### "mingw32-gcc: command not found"

Install MinGW cross-compiler (see Prerequisites).

### "cannot find -lcomctl32"

Ensure MinGW includes Windows SDK libraries:

```bash
ls /usr/i686-w64-mingw32/lib/libcomctl32.a
```

### "undefined reference to WinMain"

Check `src/main.c` has proper `WinMain` signature:

```c
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow)
```

### Resource compilation fails

Ensure `windres` is in PATH:

```bash
which i686-w64-mingw32-windres
```

### Executable won't run on Windows 2000

Verify compiler flags include:

```makefile
-DWINVER=0x0500 -D_WIN32_WINNT=0x0500
```

Check dependencies with:

```bash
i686-w64-mingw32-objdump -x TimeWarpIDE.exe | grep DLL
```

All DLLs must exist on Windows 2000.

## Size Optimization

For smaller executable:

```makefile
CFLAGS += -Os -s
```

- `-Os`: Optimize for size
- `-s`: Strip symbols

Produces ~300 KB executable (vs. ~500 KB default).

## Debug Build

For debugging with GDB:

```makefile
CFLAGS = -DWINVER=0x0500 -D_WIN32_WINNT=0x0500 -g -O0
```

- `-g`: Include debug symbols
- `-O0`: No optimization

## Static Linking

For standalone executable (no runtime dependencies):

```makefile
CFLAGS += -static-libgcc -static-libstdc++
```

Increases size but eliminates external DLL dependencies.

## Cross-Architecture Builds

### 64-bit Windows

Change compiler to:

```makefile
CC = x86_64-w64-mingw32-gcc
```

Note: Won't run on Windows 2000 (32-bit only).

### Windows 98

Change flags to:

```makefile
CFLAGS = -DWINVER=0x0400 -D_WIN32_WINNT=0x0400
```

Requires testing as some APIs may not be available.

## Continuous Integration

Example GitHub Actions workflow:

```yaml
name: Build Windows 2000

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install MinGW
        run: sudo apt-get install mingw-w64
      - name: Build
        run: cd Win2K && make
      - name: Upload artifact
        uses: actions/upload-artifact@v2
        with:
          name: TimeWarpIDE-Win2K
          path: Win2K/TimeWarpIDE.exe
```

## Verification

Successful build should:

1. Compile without errors or warnings
2. Link all modules successfully
3. Produce executable ~500 KB in size
4. Run on Windows 2000 SP4 or later
5. Display IDE window with menus and toolbars

Test with:

```bash
file TimeWarpIDE.exe
# Output: PE32 executable (GUI) Intel 80386, for MS Windows
```

## Next Steps

After successful build:

1. Test on Windows 2000/XP VM
2. Run test suite (when implemented)
3. Create installer with NSIS
4. Package for distribution
5. Generate release notes

---

**Build Status**: Complete and functional  
**Target**: Windows 2000 (32-bit)  
**Dependencies**: MinGW cross-compiler  
**Output**: TimeWarpIDE.exe (~500 KB)
