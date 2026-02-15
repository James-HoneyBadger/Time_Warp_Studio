# Installation Guide for Time Warp Studio

Complete step-by-step instructions for installing Time Warp Studio on Windows, macOS, and Linux.

---

## System Requirements

### Minimum Requirements

- **OS**: Windows 10+, macOS 10.13+, or Linux (Ubuntu 20.04+, Fedora 33+, etc.)
- **Python**: 3.10 or higher
- **RAM**: 2GB minimum (4GB recommended)
- **Disk**: 500MB free space
- **CPU**: 64-bit processor with SSE3/SSE4.1/SSE4.2 support

### Optional Hardware

- **GPU**: Recommended for smooth graphics (integrated GPU sufficient)
- **Display**: 1920x1080 minimum (1920x1080+ recommended)

### CPU Feature Check

If you get "Illegal instruction" errors, your CPU lacks required features:

```bash
# Linux: Check CPU flags
grep -E 'ssse3|sse4_1|sse4_2|popcnt' /proc/cpuinfo

# macOS: Most Macs have required features
sysctl -a | grep machdep.cpu.features
```

Virtual machines (QEMU with TCG) may not support these features. Use KVM or nested virtualization.

---

## Installation Methods

### Method 1: Quick Install (Recommended)

Fastest way to get started:

#### Linux/macOS

```bash
# 1. Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio

# 2. Create virtual environment
python3 -m venv venv
source venv/bin/activate

# 3. Install dependencies
pip install -e Platforms/Python

# 4. Run IDE
python Platforms/Python/time_warp_ide.py
```

#### Windows

```cmd
# 1. Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio

# 2. Create virtual environment
python -m venv venv
venv\Scripts\activate

# 3. Install dependencies
pip install -e Platforms\Python

# 4. Run IDE
python Platforms\Python\time_warp_ide.py
```

### Method 2: Traditional Setup

More control over installation:

#### Step 1: Install Python

**Windows:**
1. Download Python 3.10+ from [python.org](https://www.python.org/downloads/)
2. Run installer
3. **IMPORTANT**: Check "Add Python to PATH"
4. Click "Install Now"

**macOS:**
```bash
# Using Homebrew (recommended)
brew install python@3.11

# Or using official installer from python.org
```

**Linux:**
```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install python3.10 python3.10-venv python3-pip

# Fedora
sudo dnf install python3.10 python3-pip

# Arch
sudo pacman -S python
```

#### Step 2: Clone Repository

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
```

#### Step 3: Create Virtual Environment

```bash
# Linux/macOS
python3 -m venv venv
source venv/bin/activate

# Windows
python -m venv venv
venv\Scripts\activate
```

Your prompt should now show `(venv)` prefix.

#### Step 4: Upgrade pip

```bash
pip install --upgrade pip
```

#### Step 5: Install Core Dependencies

```bash
pip install PySide6>=6.4.0 Pillow>=10.0.0
```

#### Step 6: Install Time Warp

```bash
pip install -e Platforms/Python
```

#### Step 7: Verify Installation

```bash
# Test import
python -c "from time_warp.core.interpreter import TimeWarpInterpreter; print('✅ Installation successful')"

# Run IDE
python Platforms/Python/time_warp_ide.py
```

---

## Installation Troubleshooting

### Python Not Found

**Problem**: `python: command not found` or `'python' is not recognized`

**Solution**:

```bash
# Linux/macOS - Use python3
python3 -m venv venv
source venv/bin/activate
python3 --version

# Windows - Install from python.org
# Make sure to check "Add Python to PATH" during installation
# Restart terminal after installation
python --version
```

### Virtual Environment Not Activating

**Problem**: `(venv)` prefix doesn't appear in prompt

**Solution**:

```bash
# Linux/macOS - Check command
source venv/bin/activate  # Not: source venv/bin/activate.sh

# Windows - Check command
venv\Scripts\activate  # Not: activate.bat

# If still not working, check venv exists
ls venv         # Linux/macOS
dir venv        # Windows
```

### pip Install Fails

**Problem**: `error: externally-managed-environment`

**Solution**:
```bash
# This means you're not in a virtual environment
# Make sure (venv) appears in your prompt

# Linux/macOS
source venv/bin/activate

# Windows
venv\Scripts\activate

# Then retry
pip install -e Platforms/Python
```

### PySide6 Installation Issues

**Problem**: PySide6 fails to install

**Solution**:

```bash
# Update pip, setuptools, wheel
pip install --upgrade pip setuptools wheel

# Install specific PySide6 version
pip install PySide6==6.4.2

# If still fails, check Python version
python --version  # Should be 3.10+
```

**Linux-specific**:
```bash
# Install Qt development libraries
sudo apt-get install qt6-base-dev qt6-qml-dev  # Ubuntu
sudo dnf install qt6-devel                      # Fedora
```

### "Illegal Instruction" When Running IDE

**Problem**: Program crashes with "Illegal instruction"

**Solution**: Your CPU lacks required features (SSSE3/SSE4.1/SSE4.2)

```bash
# Check CPU features
grep -E 'ssse3|sse4_1|sse4_2|popcnt' /proc/cpuinfo

# If missing, you need:
# - Different hardware, OR
# - Use Docker container (see Method 3), OR
# - Compile custom PySide6 build without SIMD
```

### Git Not Found

**Problem**: `git: command not found`

**Solution**:

```bash
# Windows: Download from git-scm.com
# Or use GitHub Desktop

# Linux
sudo apt-get install git      # Ubuntu
sudo dnf install git          # Fedora

# macOS
brew install git
```

### Permission Denied (Linux)

**Problem**: `Permission denied` when running script

**Solution**:

```bash
chmod +x Platforms/Python/time_warp_ide.py
python Platforms/Python/time_warp_ide.py
```

---

### Method 3: Docker Installation

Run Time Warp Studio in a container (best compatibility):

#### Prerequisites

- Install Docker: [docker.com/get-docker](https://www.docker.com/get-docker/)
- ~2GB disk space for container image

#### Steps

```bash
# 1. Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio

# 2. Build image
docker build -t time-warp-studio .

# 3. Run container (with display support)

# Linux
docker run -it --rm \
  -e DISPLAY=$DISPLAY \
  -v /tmp/.X11-unix:/tmp/.X11-unix \
  -v $(pwd)/Examples:/app/Examples \
  time-warp-studio

# macOS
docker run -it --rm \
  -e DISPLAY=host.docker.internal:0 \
  -v $(pwd)/Examples:/app/Examples \
  time-warp-studio

# Windows (PowerShell)
docker run -it --rm `
  -v ${PWD}\Examples:C:\app\Examples `
  time-warp-studio
```

#### Docker Issues

**No Display**:
```bash
# Allow Docker to display windows
xhost +local:docker  # Linux

# Or use VNC for remote display
```

---

### Method 4: Development Installation

For contributors modifying source code:

```bash
# 1. Fork and clone
git clone https://github.com/YOUR_USERNAME/Time_Warp_Studio.git
cd Time_Warp_Studio

# 2. Create environment
python3 -m venv venv
source venv/bin/activate  # Linux/macOS or: venv\Scripts\activate (Windows)

# 3. Install editable + dev dependencies
pip install -e Platforms/Python[dev]

# 4. Install development tools
pip install black flake8 pylint mypy pytest pytest-cov

# 5. Run tests to verify
python test_runner.py --basic
```

This installs the project in "editable" mode so changes to source files immediately affect the running IDE.

---

## Verification

### Verify Successful Installation

```bash
# 1. Check Python version
python --version
# Output: Python 3.10.x or higher

# 2. Check virtual environment active
echo $VIRTUAL_ENV  # Linux/macOS
echo %VIRTUAL_ENV%  # Windows

# 3. Check imports work
python -c "from time_warp.core.interpreter import TimeWarpInterpreter; print('✅ TimeWarpInterpreter imports!')"

# 4. Check PySide6 installed
python -c "from PySide6.QtWidgets import QApplication; print('✅ PySide6 imports!')"

# 5. Run IDE
python Platforms/Python/time_warp_ide.py
```

Expected output:
```
✅ TimeWarpInterpreter imports!
✅ PySide6 imports!
```

Then IDE window should appear.

---

## First Launch

### Launch IDE

```bash
cd Time_Warp_Studio
source venv/bin/activate  # Linux/macOS: venv\Scripts\activate on Windows
python Platforms/Python/time_warp_ide.py
```

### Initial Setup

1. **Language Selection**: Choose from BASIC, Logo, PILOT, C, Pascal, Prolog, Forth
2. **Load Example**: File > Example > Choose program
3. **Run Program**: Click Run or press Ctrl+Enter
4. **Explore UI**: Check menus and feature panels

### Connection Test

In editor, try BASIC:
```basic
PRINT "Hello from Time Warp!"
```

Press **Ctrl+Enter** to run. Should see output in console.

---

## Next Steps

### After Installation

1. **Read [USER_GUIDE.md](docs/USER_GUIDE.md)** - Learn IDE features
2. **Explore [Examples/](Examples/)** - Study example programs
3. **Try [LANGUAGE_GUIDE.md](docs/LANGUAGE_GUIDE.md)** - Learn language syntax
4. **Read [TURTLE_GRAPHICS.md](docs/TURTLE_GRAPHICS.md)** - Learn graphics

### Getting Help

- **Questions**: Check [FAQ.md](docs/FAQ.md)
- **Problems**: See [TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md)
- **Issues**: Report on GitHub Issues
- **Email**: james@honey-badger.org

---

## Uninstallation

### Remove Time Warp Studio

```bash
# 1. Remove virtual environment
rm -rf venv              # Linux/macOS
rmdir /s venv            # Windows

# 2. Remove cloned repo (optional)
cd ..
rm -rf Time_Warp_Studio  # Linux/macOS
rmdir /s Time_Warp_Studio  # Windows

# 3. Remove config (optional)
rm -rf ~/.Time_Warp      # Linux/macOS
rmdir %USERPROFILE%\.Time_Warp  # Windows
```

---

## Quick Reference

### Common Commands

```bash
# Activate virtual environment
source venv/bin/activate    # Linux/macOS
venv\Scripts\activate       # Windows

# Deactivate virtual environment
deactivate

# Run IDE
python Platforms/Python/time_warp_ide.py

# Run tests
python test_runner.py --basic       # Quick tests
python test_runner.py --comprehensive  # Full suite

# Check Python version
python --version

# List installed packages
pip list
```

### Useful Paths

```
Time_Warp_Studio/
├── Platforms/Python/time_warp_ide.py  # Main entry point
├── Examples/                          # Demo programs
├── docs/                              # Documentation
├── ~/.Time_Warp/                      # User config (after first run)
└── venv/                              # Virtual environment
```

---

## Hardware Notes

### Recommended Specifications

For smooth experience:

- **CPU**: Intel i5/i7 or AMD Ryzen 5/7+ (2020 or newer)
- **RAM**: 8GB+ (4GB minimum works)
- **GPU**: Dedicated GPU for graphics (integrated OK)
- **Display**: 1920x1080+ at 60Hz+
- **Storage**: SSD preferred (500MB minimum)

### Performance Tips

1. **Close other apps** - Frees system RAM
2. **Use native window manager** - Avoid Wayland if possible
3. **Check CPU features** - Run `grep SSSE3 /proc/cpuinfo` on Linux
4. **Update GPU drivers** - Latest drivers improve performance
5. **Use wired connection** - For remote/Docker setups

---

## Getting Latest Updates

### Check for Updates

```bash
cd Time_Warp_Studio
git status
```

### Update to Latest

```bash
git pull origin main
pip install --upgrade -e Platforms/Python
```

### Stay on Specific Version

```bash
git checkout v7.0.0  # Release tag
git checkout main     # Latest development
```

---

## Installation Complete!

You're ready to use Time Warp Studio. 

👉 **Next**: Read [USER_GUIDE.md](docs/USER_GUIDE.md) to learn the IDE
👉 **Examples**: Check [Examples/](Examples/) for sample programs
👉 **Help**: See [FAQ.md](docs/FAQ.md) for common questions

**Happy coding!** 🚀
