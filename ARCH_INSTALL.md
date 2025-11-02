# Time Warp IDE - Arch Linux Installation Guide

Complete guide for installing Time Warp IDE on Arch Linux and derivatives (Manjaro, EndeavourOS, etc.)

## Quick Start

```bash
# Navigate to Time Warp directory
cd ~/Time_Warp

# Install for current user (recommended)
./install-user.sh

# Launch Time Warp IDE
timewarp
```

---

## System Requirements

### Minimum Requirements

- **Operating System**: Arch Linux, Manjaro, EndeavourOS, etc.
- **CPU**: Any modern ARM64 or x86_64 processor
- **RAM**: 512 MB minimum, 1 GB recommended
- **Disk Space**: 500 MB for installation

### Required Packages

Install with pacman:

```bash
# For Python implementation
sudo pacman -S python python-pip

# For Rust implementation  
sudo pacman -S rust cargo

# Optional but recommended
sudo pacman -S imagemagick git
```

---

## Installation

### Recommended: User Installation

Installs to `~/.local/share/timewarp` - **no root access required**.

```bash
cd ~/Time_Warp
./install-user.sh
```

**Features:**
- ✅ No sudo required
- ✅ Easy to modify and update
- ✅ Doesn't affect other users
- ✅ Follows XDG Base Directory specification

**Installation locations:**
- Binaries: `~/.local/bin/`
- Application: `~/.local/share/timewarp/`
- Desktop entries: `~/.local/share/applications/`
- Icons: `~/.local/share/icons/hicolor/`
- Documentation: `~/.local/share/doc/timewarp/`

### Ensure PATH is Set

The installer checks automatically, but if `timewarp` command isn't found:

```bash
# Add to ~/.bashrc or ~/.zshrc
export PATH="$HOME/.local/bin:$PATH"

# Reload shell config
source ~/.bashrc  # or source ~/.zshrc
```

### Optional: System-Wide Installation

For multi-user systems or system-wide deployment:

```bash
cd ~/Time_Warp
sudo ./install.sh
```

Installs to:
- `/opt/timewarp/` - Application files
- `/usr/local/bin/` - Launcher scripts
- `/usr/share/applications/` - Desktop entries
- `/usr/share/icons/` - Application icons

---

## Installation Options

### Python Only (Lighter Install)

```bash
./install-user.sh --python-only
```

**Benefits:**
- Faster installation
- Smaller disk footprint
- No Rust toolchain needed
- Better for educational use

### Rust Only (Best Performance)

```bash
./install-user.sh --rust-only
```

**Benefits:**
- Native compiled binary
- Maximum performance
- Single executable
- Lower memory usage

### Both (Full Experience)

```bash
./install-user.sh
```

Get both implementations and choose which to use.

---

## Post-Installation

### Verify Installation

```bash
# Check installation
which timewarp
timewarp --version

# List available commands
ls -la ~/.local/bin/timewarp*
```

### Desktop Integration

Time Warp IDE should appear in your application menu:
- **Development** category
- **Education** category
- Search for "Time Warp"

If applications don't appear:
1. Log out and back in
2. Or run: `update-desktop-database ~/.local/share/applications/`

### Test with Example Programs

```bash
# Simple star
timewarp ~/.local/share/timewarp/examples/logo_star.logo

# Interactive quiz
timewarp ~/.local/share/timewarp/examples/pilot_quiz.pilot

# BASIC game
timewarp ~/.local/share/timewarp/examples/basic_guess.bas

# Advanced fractals
timewarp ~/.local/share/timewarp/examples/logo_koch_snowflake.logo
```

### File Associations

Right-click on `.pilot`, `.bas`, `.logo`, or `.tc` files and select:
**Open With → Time Warp IDE**

---

## Using Time Warp IDE

### Launch Methods

**From command line:**
```bash
# Default version (Python or Rust, whichever is set as default)
timewarp

# Specific Python version
timewarp-python

# Specific Rust version
timewarp-rust

# Open file directly
timewarp myprogram.pilot
```

**From desktop:**
- Open application menu
- Find "Time Warp IDE" under Development or Education
- Or search for "Time Warp"

### Choose Your Implementation

Both implementations support the same TempleCode language:

| Feature | Python | Rust |
|---------|--------|------|
| TempleCode support | ✅ Full | ✅ Full |
| Turtle graphics | ✅ Yes | ✅ Yes |
| UI framework | PySide6 | egui |
| Performance | Good | Excellent |
| Memory usage | Higher | Lower |
| Startup time | Slower | Faster |
| Best for | Education | Production |

**Switch default:**
```bash
# Make Python default
ln -sf ~/.local/bin/timewarp-python ~/.local/bin/timewarp

# Make Rust default
ln -sf ~/.local/bin/timewarp-rust ~/.local/bin/timewarp
```

---

## Troubleshooting

### Python Issues

**Problem:** PySide6 not found or Qt errors

```bash
# Install Qt dependencies
sudo pacman -S qt6-base python-pyqt6

# Or reinstall in venv
cd ~/.local/share/timewarp
./venv/bin/pip install --upgrade --force-reinstall PySide6
```

**Problem:** Missing Python modules

```bash
# Reinstall dependencies
~/.local/share/timewarp/venv/bin/pip install PySide6 pillow
```

### Rust Issues

**Problem:** Rust compilation fails

```bash
# Update Rust toolchain
rustup update stable

# Install build dependencies
sudo pacman -S base-devel fontconfig

# Rebuild
cd ~/.local/share/timewarp/Time_Warp_Rust
cargo clean
cargo build --release
```

### Display Issues

**Problem:** GUI doesn't start

```bash
# Check display
echo $DISPLAY
echo $WAYLAND_DISPLAY

# Install missing libraries
sudo pacman -S libxcb xcb-util-wm xcb-util-image xcb-util-keysyms

# Try Wayland compatibility (if on Wayland)
export QT_QPA_PLATFORM=wayland
timewarp
```

**Problem:** Icon not showing

```bash
# Install ImageMagick
sudo pacman -S imagemagick

# Regenerate installation
./install-user.sh

# Update icon cache
gtk-update-icon-cache -f -t ~/.local/share/icons/hicolor/
```

### Permission Issues

```bash
# Fix permissions
chmod -R u+rwX ~/.local/share/timewarp
chmod +x ~/.local/bin/timewarp*
```

### PATH Issues

```bash
# Verify PATH contains ~/.local/bin
echo $PATH | grep -o "$HOME/.local/bin"

# If not, add to shell config
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

### Desktop Entry Issues

```bash
# Update desktop database
update-desktop-database ~/.local/share/applications/

# Refresh icon cache
gtk-update-icon-cache -f -t ~/.local/share/icons/hicolor/

# Or log out and back in
```

---

## Uninstallation

### Remove User Installation

```bash
cd ~/Time_Warp
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
cd ~/Time_Warp
sudo ./install.sh --uninstall
```

### Clean Build Artifacts

```bash
# Rust
cd ~/Time_Warp/Time_Warp_Rust
cargo clean

# Python cache
cd ~/Time_Warp/Time_Warp_Python
find . -type d -name __pycache__ -exec rm -rf {} +
find . -type f -name "*.pyc" -delete
```

---

## Advanced Configuration

### AUR Package (Future)

Time Warp IDE will be available in the AUR:

```bash
# Install from AUR (when available)
yay -S timewarp-ide
# or
paru -S timewarp-ide
```

### Desktop Shortcut

Create launcher on desktop:

```bash
cp ~/.local/share/applications/timewarp-python.desktop ~/Desktop/
chmod +x ~/Desktop/timewarp-python.desktop
```

### Integration with Editors

**VS Code:**
Add to `.vscode/tasks.json`:
```json
{
  "label": "Open in Time Warp",
  "type": "shell",
  "command": "timewarp ${file}"
}
```

**Vim/Neovim:**
Add to config:
```vim
nnoremap <leader>tw :!timewarp %<CR>
```

### Custom File Associations

```bash
# Associate .tc files with Time Warp
xdg-mime default timewarp-python.desktop text/x-templecode

# For all supported types
for type in text/x-pilot text/x-basic text/x-logo text/x-templecode; do
    xdg-mime default timewarp-python.desktop $type
done
```

---

## Documentation

After installation, comprehensive documentation is available at:

- **User Guide**: `~/.local/share/doc/timewarp/docs/USER_GUIDE.md`
- **Teacher Guide**: `~/.local/share/doc/timewarp/docs/TEACHER_GUIDE.md`
- **Student Lessons**: `~/.local/share/doc/timewarp/docs/STUDENT_LESSON_BOOK.md`
- **Technical Reference**: `~/.local/share/doc/timewarp/docs/TECHNICAL_REFERENCE.md`
- **Main README**: `~/.local/share/doc/timewarp/README.md`

View with:
```bash
less ~/.local/share/doc/timewarp/docs/USER_GUIDE.md
```

Or open in editor:
```bash
code ~/.local/share/doc/timewarp/docs/
```

---

## Getting Help

### Online Resources

- **GitHub**: https://github.com/James-HoneyBadger/Time_Warp
- **Issues**: https://github.com/James-HoneyBadger/Time_Warp/issues
- **Email**: james@honey-badger.org

### Local Examples

Try these to get started:

```bash
# Browse all examples
ls -la ~/.local/share/timewarp/examples/

# Categories
ls ~/.local/share/timewarp/examples/basic_*    # BASIC programs
ls ~/.local/share/timewarp/examples/pilot_*    # PILOT programs
ls ~/.local/share/timewarp/examples/logo_*     # Logo programs
```

### Quick Reference

**Launch IDE:**
```bash
timewarp                # Default version
timewarp-python         # Python explicitly
timewarp-rust           # Rust explicitly
```

**Run example:**
```bash
timewarp ~/.local/share/timewarp/examples/logo_star.logo
```

**Get help:**
```bash
timewarp --help
```

---

## What's Next?

1. **Try examples**: Start with `logo_star.logo` or `pilot_quiz.pilot`
2. **Read guides**: Check out the User Guide and Student Lesson Book
3. **Write code**: Create your first TempleCode program
4. **Explore**: Mix BASIC, PILOT, and Logo in one program!
5. **Share**: Join the community and share your creations

---

## Arch Linux Specific Notes

### Wayland Compatibility

Time Warp IDE works on both X11 and Wayland:

```bash
# Force Wayland (if auto-detection fails)
export QT_QPA_PLATFORM=wayland
timewarp

# Force X11
export QT_QPA_PLATFORM=xcb
timewarp
```

### System Integration

Time Warp follows Arch Linux packaging standards:
- Uses standard paths (`~/.local/`, `/opt/`)
- No systemd services required
- No special permissions needed
- Clean uninstall with no leftover files

### Performance

On Arch Linux:
- Python version: Good performance with PySide6
- Rust version: Excellent native performance
- Both work great on ARM64 (like your system)

---

**Enjoy programming with Time Warp IDE!** 🚀🐢

*Installation completed on: November 1, 2025*
*System: Arch Linux ARM64*
