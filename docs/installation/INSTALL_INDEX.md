# Time Warp IDE - Installation & Setup Index

**Quick navigation for all installation documentation**

---

## 🚀 Start Here

New to Time Warp? Start with these:

1. **[INSTALLATION_SUCCESS.md](INSTALLATION_SUCCESS.md)** - Read this first! Complete setup guide
2. **[YOUR_INSTALLATION.md](YOUR_INSTALLATION.md)** - Quick reference for your specific installation

---

## 📖 Installation Guides

### For Your System (Arch Linux)

- **[ARCH_INSTALL.md](ARCH_INSTALL.md)** - Complete guide for Arch Linux, Manjaro, EndeavourOS

### For Other Systems

- **[DEBIAN_INSTALL.md](DEBIAN_INSTALL.md)** - Debian, Ubuntu, Linux Mint, and derivatives

---

## 🛠️ Installation Scripts

All scripts are executable and ready to use:

### User Installation (Recommended)
```bash
./install-user.sh              # Install for current user only
./install-user.sh --python-only    # Python implementation only
./install-user.sh --rust-only      # Rust implementation only
./install-user.sh --uninstall      # Remove installation
```

**Installs to:** `~/.local/share/timewarp/`  
**No root required** ✓

### System-Wide Installation
```bash
sudo ./install.sh              # Install for all users
sudo ./install.sh --python-only    # Python only
sudo ./install.sh --rust-only      # Rust only
sudo ./install.sh --uninstall      # Remove installation
```

**Installs to:** `/opt/timewarp/`  
**Requires sudo**

### Testing
```bash
./test-install.sh              # Verify installation
```

---

## 📚 Documentation Structure

```
Time_Warp/
├── README.md                      Main project overview
├── INSTALLATION_SUCCESS.md        Your installation summary ⭐
├── YOUR_INSTALLATION.md           Quick reference for your setup ⭐
├── ARCH_INSTALL.md                Arch Linux installation guide ⭐
├── DEBIAN_INSTALL.md              Debian/Ubuntu installation guide
├── install.sh                     System-wide installer script
├── install-user.sh                User installer script ⭐
├── test-install.sh                Installation test suite
├── run.sh                         Direct launch script
│
├── docs/                          Main documentation
│   ├── USER_GUIDE.md              Complete user manual
│   ├── STUDENT_LESSON_BOOK.md     24 programming lessons
│   ├── TEACHER_GUIDE.md           Teaching curriculum
│   ├── TECHNICAL_REFERENCE.md     Language specification
│   └── ...                        (more guides)
│
├── examples/                      33 example programs
│   ├── logo_*.logo                Logo turtle graphics
│   ├── pilot_*.pilot              PILOT interactive programs
│   ├── basic_*.bas                BASIC programs
│   └── demo.tc                    TempleCode mixed program
│
├── platforms/
│   ├── python/                    Python implementation
│   │   ├── time_warp_ide.py       GUI launcher
│   │   ├── time_warp/             Source code
│   │   └── README.md              Python-specific docs
│   └── rust/                      Rust implementation
│       ├── src/                   Source code
│       ├── Cargo.toml             Rust configuration
│       └── README.md              Rust-specific docs
```

---

## 🎯 Quick Access

### Launch Commands

```bash
timewarp                           # Default (Python)
timewarp-python                    # Python version
timewarp-rust                      # Rust version
```

### Example Programs

```bash
# List all examples
ls ~/.local/share/timewarp/examples/

# Run an example
timewarp ~/.local/share/timewarp/examples/logo_star.logo
```

### Documentation

```bash
# User guides (installed)
ls ~/.local/share/doc/timewarp/docs/

# Installation guides (source)
ls ~/Time_Warp/*.md
```

---

## 🔍 Find What You Need

### I want to...

**Learn to use Time Warp**
→ Read [YOUR_INSTALLATION.md](YOUR_INSTALLATION.md) then [docs/USER_GUIDE.md](docs/USER_GUIDE.md)

**Teach with Time Warp**
→ See [docs/TEACHER_GUIDE.md](docs/TEACHER_GUIDE.md)

**Learn programming**
→ Start with [docs/STUDENT_LESSON_BOOK.md](docs/STUDENT_LESSON_BOOK.md)

**Reinstall or update**
→ Run `./install-user.sh` again

**Uninstall**
→ Run `./install-user.sh --uninstall`

**Install on another system**
→ Follow [ARCH_INSTALL.md](ARCH_INSTALL.md) or [DEBIAN_INSTALL.md](DEBIAN_INSTALL.md)

**Understand the language**
→ Read [docs/TECHNICAL_REFERENCE.md](docs/TECHNICAL_REFERENCE.md)

**Troubleshoot issues**
→ Check [ARCH_INSTALL.md#troubleshooting](ARCH_INSTALL.md#troubleshooting)

**Try examples**
→ Browse `~/.local/share/timewarp/examples/`

**Switch versions**
→ Use `timewarp-python` or `timewarp-rust` directly

---

## ✅ Installation Status

**Your current installation:**
- Type: User installation
- Location: `~/.local/share/timewarp/`
- Python: ✅ Installed with PySide6
- Rust: ✅ Built and ready (16MB)
- Desktop: ✅ Menu entries created
- Examples: ✅ 33 programs available
- Status: ✅ All tests passed

Run `./test-install.sh` to verify anytime.

---

## 🆘 Need Help?

### Documentation
1. **[INSTALLATION_SUCCESS.md](INSTALLATION_SUCCESS.md)** - Complete setup guide
2. **[YOUR_INSTALLATION.md](YOUR_INSTALLATION.md)** - Quick reference
3. **[ARCH_INSTALL.md](ARCH_INSTALL.md)** - Detailed Arch Linux guide

### Online
- GitHub: https://github.com/James-HoneyBadger/Time_Warp
- Issues: https://github.com/James-HoneyBadger/Time_Warp/issues
- Email: james@honey-badger.org

### Commands
```bash
# Verify installation
./test-install.sh

# Check command availability
which timewarp

# View installed files
ls -la ~/.local/share/timewarp/
```

---

## 📝 Summary

**What was installed:**
- ✅ Time Warp IDE (Python + Rust)
- ✅ Desktop integration
- ✅ 33 example programs
- ✅ Complete documentation
- ✅ Command-line tools

**Where to find things:**
- **Binaries:** `~/.local/bin/timewarp*`
- **Application:** `~/.local/share/timewarp/`
- **Docs:** `~/.local/share/doc/timewarp/`
- **Examples:** `~/.local/share/timewarp/examples/`

**How to use:**
```bash
timewarp                           # Launch IDE
timewarp file.logo                 # Open file
timewarp-python                    # Python version
timewarp-rust                      # Rust version
```

---

## 🎉 Ready to Start!

Time Warp IDE is fully installed and ready for use. Choose your path:

**New Users:**
1. Read [INSTALLATION_SUCCESS.md](INSTALLATION_SUCCESS.md)
2. Try: `timewarp ~/.local/share/timewarp/examples/logo_star.logo`
3. Explore [YOUR_INSTALLATION.md](YOUR_INSTALLATION.md)

**Students:**
1. Open [docs/STUDENT_LESSON_BOOK.md](docs/STUDENT_LESSON_BOOK.md)
2. Start with Lesson 1
3. Try examples as you learn

**Teachers:**
1. Review [docs/TEACHER_GUIDE.md](docs/TEACHER_GUIDE.md)
2. Explore curriculum materials
3. Customize for your class

**Developers:**
1. Read [docs/TECHNICAL_REFERENCE.md](docs/TECHNICAL_REFERENCE.md)
2. Study example programs
3. Explore the source code

---

**Welcome to Time Warp IDE!** 🚀🐢

*For complete information, start with [INSTALLATION_SUCCESS.md](INSTALLATION_SUCCESS.md)*
