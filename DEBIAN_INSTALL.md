# Time Warp IDE - Debian Installation Guide

Complete guide for installing Time Warp IDE on Debian-based systems (Debian, Ubuntu, Linux Mint, etc.)

## Table of Contents

1. [Quick Start](#quick-start)
2. [System Requirements](#system-requirements)
3. [Installation Methods](#installation-methods)
4. [Post-Installation](#post-installation)
5. [Troubleshooting](#troubleshooting)
6. [Uninstallation](#uninstallation)

---

## Quick Start

### For Most Users (Recommended)

```bash
# Clone or download Time Warp IDE
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp

# Install for current user only (no root required)
./install-user.sh

# Or install system-wide (requires sudo)
sudo ./install.sh
```

After installation, launch from:
- **Applications menu**: Development → Time Warp IDE
- **Command line**: `timewarp` or `timewarp-python` or `timewarp-rust`

---

## System Requirements

### Minimum Requirements

- **Operating System**: Debian 10+ / Ubuntu 20.04+ / Linux Mint 20+
- **CPU**: Any modern x86_64 processor
- **RAM**: 512 MB minimum, 1 GB recommended
- **Disk Space**: 500 MB for installation

### For Python Implementation

- Python 3.8 or higher
- pip (Python package manager)
- python3-venv (virtual environment support)

### For Rust Implementation

- Rust toolchain 1.70+ (install from https://rustup.rs/)
- Build essentials (gcc, make)

### Optional Dependencies

- **ImageMagick** - For application icon generation
- **Git** - For cloning the repository

---

## Installation Methods

### Method 1: User Installation (Recommended)

Installs to `~/.local/share/timewarp` - **no root access required**.

```bash
cd Time_Warp
./install-user.sh
```

**Installation locations:**
- Binaries: `~/.local/bin/`
- Application data: `~/.local/share/timewarp/`
- Desktop entries: `~/.local/share/applications/`
- Icons: `~/.local/share/icons/`
- Documentation: `~/.local/share/doc/timewarp/`

**Advantages:**
- ✅ No sudo required
- ✅ Safe for multi-user systems
- ✅ Easy to modify and update
- ✅ Doesn't affect other users

**Note**: Make sure `~/.local/bin` is in your PATH. If not, add to `~/.bashrc`:

```bash
export PATH="$HOME/.local/bin:$PATH"
```

Then reload: `source ~/.bashrc`

### Method 2: System-Wide Installation

Installs to `/opt/timewarp` - available to all users.

```bash
cd Time_Warp
sudo ./install.sh
```

**Installation locations:**
- Binaries: `/usr/local/bin/`
- Application data: `/opt/timewarp/`
- Desktop entries: `/usr/share/applications/`
- Icons: `/usr/share/icons/`
- Documentation: `/usr/share/doc/timewarp/`

**Advantages:**
- ✅ Available to all users
- ✅ Central management
- ✅ Professional deployment
- ✅ Standard Linux locations

**Requires:** sudo/root access

### Method 3: Python Only

Install only the Python implementation (lighter, faster install):

```bash
# User installation
./install-user.sh --python-only

# System-wide installation
sudo ./install.sh --python-only
```

**Use when:**
- You don't have Rust installed
- You prefer Python for educational purposes
- You want the fastest installation
- You're on older or limited hardware

### Method 4: Rust Only

Install only the Rust implementation (best performance):

```bash
# User installation
./install-user.sh --rust-only

# System-wide installation
sudo ./install.sh --rust-only
```

**Use when:**
- You need maximum performance
- You want a single compiled binary
- You have Rust toolchain installed
- You prefer native applications

### Method 5: Manual Installation

For development or custom setups:

#### Python Version

```bash
cd Time_Warp_Python

# Create virtual environment
python3 -m venv venv
source venv/bin/activate

# Install dependencies
pip install PySide6 pillow

# Run directly
python time_warp_ide.py
```

#### Rust Version

```bash
cd Time_Warp_Rust

# Build release version
cargo build --release

# Run directly
./target/release/time-warp
```

---

## Post-Installation

### Verify Installation

Test that everything is working:

```bash
# Test Python version
timewarp-python --help

# Test Rust version
timewarp-rust --help

# Run example program
timewarp examples/logo_star.logo
```

### Desktop Integration

After installation, Time Warp IDE should appear in your application menu under:
- **Development** category
- **Education** category
- Search for "Time Warp"

If icons don't appear immediately:
1. Log out and back in, or
2. Run: `update-desktop-database ~/.local/share/applications/` (user install)
3. Run: `sudo update-desktop-database /usr/share/applications/` (system install)

### File Associations

Time Warp IDE registers handlers for:
- `.pilot` - PILOT programs
- `.bas` - BASIC programs
- `.logo` - Logo programs
- `.tc` - TempleCode programs

Right-click any of these files and select "Open With → Time Warp IDE"

### Command-Line Usage

```bash
# Launch IDE with no file
timewarp

# Open specific file
timewarp myprogram.pilot

# Use specific implementation
timewarp-python myprogram.bas
timewarp-rust myprogram.logo

# Run from anywhere (if in PATH)
timewarp ~/Documents/mycode.tc
```

---

## Troubleshooting

### Python Version Issues

**Problem**: "Python 3 not found"

```bash
# Install Python 3
sudo apt-get update
sudo apt-get install python3 python3-pip python3-venv
```

**Problem**: "PySide6 not found" or Qt errors

```bash
# Install PySide6 dependencies
sudo apt-get install python3-pyqt6 libqt6widgets6

# Or reinstall in virtual environment
cd ~/.local/share/timewarp
./venv/bin/pip install --upgrade --force-reinstall PySide6
```

### Rust Version Issues

**Problem**: "cargo not found"

```bash
# Install Rust toolchain
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env

# Rebuild Time Warp
cd ~/.local/share/timewarp/Time_Warp_Rust
cargo build --release
```

**Problem**: Build fails with linking errors

```bash
# Install build dependencies
sudo apt-get install build-essential pkg-config libfontconfig1-dev
```

### Desktop Integration Issues

**Problem**: Application doesn't appear in menu

```bash
# Update desktop database
update-desktop-database ~/.local/share/applications/
gtk-update-icon-cache -f -t ~/.local/share/icons/hicolor/

# Or log out and back in
```

**Problem**: Icon not showing

```bash
# Install ImageMagick and regenerate
sudo apt-get install imagemagick

# Re-run installation
./install-user.sh
```

### PATH Issues

**Problem**: `timewarp` command not found

```bash
# Check if binary exists
ls -la ~/.local/bin/timewarp*

# Add to PATH (add to ~/.bashrc)
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

### Permission Issues

**Problem**: Permission denied errors during installation

```bash
# For user installation - no sudo needed
./install-user.sh

# For system installation - use sudo
sudo ./install.sh

# Fix permissions on user install
chmod -R u+rwX ~/.local/share/timewarp
```

### Display Issues

**Problem**: GUI doesn't start or crashes

```bash
# Check display environment
echo $DISPLAY

# Install missing GUI libraries
sudo apt-get install libxcb-xinerama0 libxcb-cursor0

# Try Python version (more compatible)
timewarp-python
```

### Example Programs Not Found

```bash
# Check installation
ls -la ~/.local/share/timewarp/examples/

# Or use full path
timewarp ~/.local/share/timewarp/examples/logo_star.logo
```

---

## Uninstallation

### Remove User Installation

```bash
cd Time_Warp
./install-user.sh --uninstall
```

Or manually:

```bash
rm -rf ~/.local/share/timewarp
rm -f ~/.local/bin/timewarp*
rm -f ~/.local/share/applications/timewarp*.desktop
rm -rf ~/.local/share/icons/hicolor/*/apps/timewarp.png
rm -rf ~/.local/share/doc/timewarp
```

### Remove System Installation

```bash
cd Time_Warp
sudo ./install.sh --uninstall
```

Or manually:

```bash
sudo rm -rf /opt/timewarp
sudo rm -f /usr/local/bin/timewarp*
sudo rm -f /usr/share/applications/timewarp*.desktop
sudo rm -rf /usr/share/icons/hicolor/*/apps/timewarp.png
sudo rm -rf /usr/share/doc/timewarp
```

### Clean Build Artifacts

```bash
# Remove Rust build cache
cd Time_Warp_Rust
cargo clean

# Remove Python cache
cd Time_Warp_Python
find . -type d -name __pycache__ -exec rm -rf {} +
find . -type f -name "*.pyc" -delete
```

---

## Additional Configuration

### Set Default Implementation

If both Python and Rust are installed:

```bash
# Make Python the default
ln -sf ~/.local/bin/timewarp-python ~/.local/bin/timewarp

# Make Rust the default
ln -sf ~/.local/bin/timewarp-rust ~/.local/bin/timewarp
```

### Create Desktop Shortcuts

Create a launcher on your desktop:

```bash
# Copy desktop file to Desktop
cp ~/.local/share/applications/timewarp-python.desktop ~/Desktop/
chmod +x ~/Desktop/timewarp-python.desktop
```

### Integration with Other IDEs

Time Warp can be launched from other editors:

**VS Code** - Add to tasks.json:
```json
{
  "label": "Open in Time Warp",
  "type": "shell",
  "command": "timewarp ${file}"
}
```

**Vim/Neovim** - Add to config:
```vim
nnoremap <leader>tw :!timewarp %<CR>
```

---

## Getting Help

### Documentation

- **Main README**: `/home/james/Time_Warp/README.md`
- **User Guide**: `~/.local/share/doc/timewarp/docs/USER_GUIDE.md`
- **Teacher Guide**: `~/.local/share/doc/timewarp/docs/TEACHER_GUIDE.md`
- **Technical Reference**: `~/.local/share/doc/timewarp/docs/TECHNICAL_REFERENCE.md`

### Online Resources

- **GitHub**: https://github.com/James-HoneyBadger/Time_Warp
- **Issues**: https://github.com/James-HoneyBadger/Time_Warp/issues
- **Email**: james@honey-badger.org

### Quick Examples

After installation, try these examples:

```bash
# Simple turtle graphics
timewarp ~/.local/share/timewarp/examples/logo_star.logo

# Interactive quiz
timewarp ~/.local/share/timewarp/examples/pilot_quiz.pilot

# BASIC game
timewarp ~/.local/share/timewarp/examples/basic_guess.bas

# Advanced graphics
timewarp ~/.local/share/timewarp/examples/logo_koch_snowflake.logo
```

---

## Comparison: User vs System Installation

| Feature | User Install | System Install |
|---------|--------------|----------------|
| Root required | ❌ No | ✅ Yes |
| Available to all users | ❌ No | ✅ Yes |
| Easy to update | ✅ Yes | ⚠️ Requires sudo |
| Safe on shared systems | ✅ Yes | ⚠️ Affects everyone |
| Standard Linux paths | ⚠️ User paths | ✅ System paths |
| Isolation | ✅ Per user | ❌ Shared |

**Recommendation**: Use **user installation** unless you're an administrator deploying to multiple users.

---

## Next Steps

After successful installation:

1. **Try the examples**: Start with `logo_star.logo` or `pilot_quiz.pilot`
2. **Read the User Guide**: Learn all the features and commands
3. **Explore TempleCode**: Mix BASIC, PILOT, and Logo in one program
4. **Join the community**: Share your creations and get help
5. **Contribute**: Help improve Time Warp IDE

---

**Happy Coding with Time Warp IDE!** 🚀🐢

*Last updated: November 1, 2025*
