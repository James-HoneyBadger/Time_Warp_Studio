# Installation Quick Start

Get Time Warp IDE running on your system in minutes!

---

## Choose Your Operating System

- **[Linux](#linux-quick-install)** - Debian, Ubuntu, Arch, and more
- **[macOS](#macos-quick-install)** - Apple computers
- **[Windows](#windows-quick-install)** - Modern Windows systems

---

## Linux Quick Install

### Debian/Ubuntu

```bash
# Download the latest release
wget https://github.com/honey-badger-org/Time_Warp/releases/latest/download/time-warp-ide_amd64.deb

# Install
sudo dpkg -i time-warp-ide_amd64.deb
sudo apt-get install -f  # Install dependencies if needed

# Run
time-warp-ide
```

**Or use Python version:**
```bash
# Install Python 3.10+ if needed
sudo apt update
sudo apt install python3 python3-pip python3-venv

# Clone repository
git clone https://github.com/honey-badger-org/Time_Warp.git
cd Time_Warp

# Create virtual environment
python3 -m venv .venv
source .venv/bin/activate

# Install
pip install -e ./platforms/python

# Run
python platforms/python/time_warp_ide.py
```

### Arch Linux

```bash
# Using AUR
yay -S time-warp-ide

# Or install from source
git clone https://github.com/honey-badger-org/Time_Warp.git
cd Time_Warp
makepkg -si
```

**Detailed guide:** [Linux Installation](01-linux.md)

---

## macOS Quick Install

### Using Homebrew

```bash
# Install Homebrew if you don't have it
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install Time Warp IDE
brew install honey-badger-org/tap/time-warp-ide

# Run
time-warp-ide
```

### Downloadable App

1. Download `TimeWarpIDE.dmg` from [Releases](https://github.com/honey-badger-org/Time_Warp/releases)
2. Open the DMG file
3. Drag Time Warp IDE to Applications folder
4. Launch from Applications

**First time:**
- Right-click → Open (to bypass Gatekeeper)
- Or: System Preferences → Security → Allow

**Detailed guide:** [macOS Installation](02-macos.md)

---

## Windows Quick Install

### Installer

1. Download `TimeWarpIDE-Setup.exe` from [Releases](https://github.com/honey-badger-org/Time_Warp/releases)
2. Run the installer
3. Follow the setup wizard
4. Launch from Start Menu

### Portable Version

1. Download `TimeWarpIDE-Portable.zip`
2. Extract to any folder
3. Run `TimeWarpIDE.exe`
4. No installation required!

**Detailed guide:** [Windows Installation](03-windows.md)

---

## System Requirements

### Minimum
- **OS**: Linux (kernel 3.10+), macOS 10.13+, Windows 7+
- **CPU**: 1 GHz processor with SSSE3/SSE4.1 support
- **RAM**: 256 MB
- **Disk**: 50 MB free space
- **Display**: 800×600 resolution

### Recommended
- **CPU**: Modern multi-core processor
- **RAM**: 512 MB or more
- **Disk**: 100 MB for examples and documentation
- **Display**: 1024×768 or higher

### Known Issues
- **Older CPUs**: Virtual machines may lack required CPU extensions (SSSE3, SSE4.1, SSE4.2, POPCNT)
  - **Solution**: Run on physical hardware or use Python version
- **Windows 2000/XP**: Use special retro edition in `platforms/win2000/`

---

## Verify Installation

After installing, verify everything works:

### Quick Test

1. Launch Time Warp IDE
2. Select **BASIC** from Language menu
3. Type:
   ```basic
   10 PRINT "Hello, World!"
   20 END
   ```
4. Press **F5** to run
5. See "Hello, World!" in output

✅ **Success!** You're ready to program.

### Turtle Graphics Test

1. Select **Logo** from Language menu
2. Type:
   ```logo
   REPEAT 4 [FORWARD 100 RIGHT 90]
   ```
3. Press **F5**
4. See a square in the graphics canvas

✅ **Graphics working!**

---

## Troubleshooting

### "Command not found" (Linux/macOS)

**Problem**: Terminal doesn't recognize `time-warp-ide`

**Solutions**:
```bash
# Add to PATH (add to ~/.bashrc or ~/.zshrc)
export PATH="$PATH:$HOME/.local/bin"

# Or use full path
/usr/local/bin/time-warp-ide

# Or run from install directory
cd /opt/time-warp-ide
./time-warp-ide
```

### "Illegal instruction" Error

**Problem**: CPU lacks required extensions

**Solutions**:
1. Update your system (may include CPU microcode updates)
2. Run on physical hardware (not old VM)
3. Use Python version instead of compiled binary

### "Python not found" (Windows)

**Problem**: Python not installed or not in PATH

**Solutions**:
1. Download Python from [python.org](https://www.python.org/downloads/)
2. During installation, check "Add Python to PATH"
3. Reinstall if needed

### Permission Denied (Linux/macOS)

**Problem**: File not executable

**Solution**:
```bash
chmod +x time-warp-ide
```

### Missing Dependencies

**Linux**:
```bash
# Debian/Ubuntu
sudo apt install python3-pyside6 python3-pillow

# Arch
sudo pacman -S python-pyside6 python-pillow

# Fedora
sudo dnf install python3-pyside6 python3-pillow
```

**macOS**:
```bash
pip3 install PySide6 Pillow
```

---

## Next Steps

- **New users**: Read the [User Manual](../user/00-user-manual.md)
- **Students**: Start with the [Student Workbook](../student/00-workbook.md)
- **Teachers**: Check the [Teacher's Guide](../teacher/00-overview.md)
- **Developers**: See the [Developer Guide](../developer/00-developer-guide.md)

---

## Getting Help

**Installation Issues?**
- Check [Troubleshooting Guide](04-troubleshooting.md)
- Search [existing issues](https://github.com/honey-badger-org/Time_Warp/issues)
- Ask in [Discussions](https://github.com/honey-badger-org/Time_Warp/discussions)

**Found a bug?**
- Report it: [New Issue](https://github.com/honey-badger-org/Time_Warp/issues/new)

---

*Happy coding!*
