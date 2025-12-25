# Launching Time Warp IDE

Quick start scripts are provided for both Linux/macOS and Windows.

## Linux / macOS

### Basic Launch

```bash
./launch_ide.sh
```

This will:
1. Create a Python virtual environment (if not already created)
2. Install all required dependencies
3. Activate the virtual environment
4. Launch the Time Warp IDE

### Advanced Options

**Reinstall dependencies:**
```bash
./launch_ide.sh --reinstall
```

**Install development dependencies too:**
```bash
./launch_ide.sh --dev
```

**Combine options:**
```bash
./launch_ide.sh --reinstall --dev
```

### Requirements

- Python 3.8 or later
- Bash shell
- Internet connection (for first-time installation)

### What Gets Installed

Core dependencies:
- `PySide6>=6.5.0` - GUI framework
- `Pillow>=10.0.0` - Image processing
- `requests>=2.31.0` - HTTP requests

Development dependencies (with `--dev`):
- `pytest>=7.4.0` - Testing framework
- `black>=23.7.0` - Code formatter
- `mypy>=1.5.0` - Type checker
- And more...

---

## Windows

### Basic Launch

```cmd
launch_ide.bat
```

This will:
1. Create a Python virtual environment (if not already created)
2. Install all required dependencies
3. Activate the virtual environment
4. Launch the Time Warp IDE

### Advanced Options

**Reinstall dependencies:**
```cmd
launch_ide.bat --reinstall
```

**Install development dependencies too:**
```cmd
launch_ide.bat --dev
```

**Combine options:**
```cmd
launch_ide.bat --reinstall --dev
```

### Requirements

- Python 3.8 or later (installed and in PATH)
- Windows Command Prompt or PowerShell
- Internet connection (for first-time installation)

### What Gets Installed

Same as Linux/macOS (see above).

---

## Manual Setup (Advanced)

If you prefer to set up manually:

```bash
# Navigate to Python directory
cd Platforms/Python

# Create virtual environment
python3 -m venv .venv

# Activate virtual environment
# Linux/macOS:
source .venv/bin/activate
# Windows:
.venv\Scripts\activate.bat

# Install dependencies
pip install -r requirements.txt

# Launch IDE
python3 time_warp_ide.py
```

---

## Troubleshooting

### Python not found
**Error:** "Python is not installed or not in PATH"

**Solution:** 
1. Install Python 3.8+ from https://python.org
2. On Windows, ensure "Add Python to PATH" is checked during installation
3. Restart your terminal/cmd

### Permission denied (Linux/macOS)
**Error:** "Permission denied" when running `./launch_ide.sh`

**Solution:**
```bash
chmod +x launch_ide.sh
./launch_ide.sh
```

### PySide6 installation fails
**Error:** "Failed to install PySide6" or "Illegal instruction"

**Cause:** 
- Older CPU without required instructions (SSE4.2, POPCNT, etc.)
- Running on VM/QEMU without hardware acceleration
- Incompatible system library

**Solution:**
1. Try on physical hardware or modern cloud instance
2. Or build PySide6 from source (advanced)

### Virtual environment already exists
**Error:** "venv already exists"

**Solution:**
```bash
./launch_ide.sh --reinstall
```

This will remove and recreate the virtual environment.

---

## Scripts Overview

### `launch_ide.sh` (Linux/macOS)
- 200+ lines of bash script
- Full color output for status
- Error handling and validation
- Support for command-line flags

### `launch_ide.bat` (Windows)
- Equivalent Windows batch script
- Same features as Linux version
- CMD/PowerShell compatible
- Automatic path handling

---

## What Happens Next

Once the IDE launches, you can:

1. **Load examples** - File → Open → Examples/
2. **Write code** - Type in the editor
3. **Run programs** - Click Run or press Ctrl+R
4. **See graphics** - Logo draws to canvas
5. **Use REPL** - Quick commands in Immediate Mode

For more information, see:
- [User Guide](docs/user-guide/README.md) - IDE usage
- [Programming Tutorials](docs/tutorials/README.md) - Learn languages
- [Technical Reference](docs/technical/README.md) - For developers

---

## Environment Variables

The scripts automatically set these for you:

- `VIRTUAL_ENV` - Points to `.venv` directory
- `PATH` - Includes virtual environment's bin/ directory
- `PYTHONPATH` - Includes project root

---

## Performance Notes

**First launch:** May take 1-2 minutes for dependency installation.

**Subsequent launches:** Should start in 10-30 seconds (just venv activation).

**GPU/VRAM:**
- Editor is lightweight (50MB RAM typical)
- Graphics require more VRAM for complex scenes
- 4GB RAM is recommended minimum

---

## For Developers

To work on the IDE itself:

```bash
./launch_ide.sh --dev
cd Platforms/Python
pytest  # Run tests
black .  # Format code
mypy .   # Type check
```

---

Need help? Check the [User Guide](docs/user-guide/README.md) or [Technical Reference](docs/technical/README.md).
