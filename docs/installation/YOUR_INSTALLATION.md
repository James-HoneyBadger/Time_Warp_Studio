# Time Warp IDE - Your Installation

**Installation completed successfully on November 1, 2025**

---

## ✅ What's Installed

Time Warp IDE has been installed on your system with both Python and Rust implementations.

### Installation Locations

- **Application**: `~/.local/share/timewarp/`
- **Launchers**: `~/.local/bin/timewarp*`
- **Desktop entries**: `~/.local/share/applications/`
- **Icons**: `~/.local/share/icons/hicolor/`
- **Documentation**: `~/.local/share/doc/timewarp/`
- **Examples**: `~/.local/share/timewarp/examples/`

### Available Commands

```bash
timewarp              # Default launcher (→ Python)
timewarp-python       # Python implementation
timewarp-rust         # Rust implementation (native, fast)
```

---

## 🚀 Quick Start

### Launch from Command Line

```bash
# Open IDE
timewarp

# Open with example file
timewarp ~/.local/share/timewarp/examples/logo_star.logo

# Try an interactive quiz
timewarp ~/.local/share/timewarp/examples/pilot_quiz.pilot

# Play a BASIC game
timewarp ~/.local/share/timewarp/examples/basic_guess.bas
```

### Launch from Desktop

Look in your application menu:
- **Development** → Time Warp IDE
- **Education** → Time Warp IDE
- Or search: "Time Warp"

---

## 📚 Learn & Explore

### Example Programs (33 total)

Located at: `~/.local/share/timewarp/examples/`

**Beginner:**
- `logo_star.logo` - Draw a colorful star
- `pilot_quiz.pilot` - Interactive quiz
- `basic_guess.bas` - Number guessing game

**Intermediate:**
- `logo_flower.logo` - Flower pattern
- `pilot_adventure.pilot` - Text adventure
- `basic_hangman.bas` - Word game

**Advanced:**
- `logo_koch_snowflake.logo` - Fractal generation
- `pilot_dragon_adventure.pilot` - Complex game
- `basic_rock_paper_scissors.bas` - Interactive game

### Browse All Examples

```bash
ls ~/.local/share/timewarp/examples/

# By category
ls ~/.local/share/timewarp/examples/basic_*
ls ~/.local/share/timewarp/examples/pilot_*
ls ~/.local/share/timewarp/examples/logo_*
```

### Documentation

```bash
# Main guides
less ~/.local/share/doc/timewarp/docs/USER_GUIDE.md
less ~/.local/share/doc/timewarp/docs/STUDENT_LESSON_BOOK.md
less ~/.local/share/doc/timewarp/docs/TEACHER_GUIDE.md

# Open in editor
code ~/.local/share/doc/timewarp/docs/
```

---

## 🎨 Features

### TempleCode Language

Time Warp IDE implements **TempleCode** - a unified language combining:

- **BASIC**: Traditional PRINT, LET, IF, FOR, GOTO
- **PILOT**: Interactive T:, A:, M: pattern matching
- **Logo**: Turtle graphics with procedures

### Example Program

```templecode
10 PRINT "Welcome to TempleCode!"
20 LET SIZE = 100

T:What color? (red/blue/green)
A:COLOR

SETCOLOR *COLOR*
PENWIDTH 3

TO SQUARE :S
  REPEAT 4 [
    FORWARD :S
    RIGHT 90
  ]
END

SQUARE SIZE
```

---

## 🎯 Choose Your Implementation

Both versions support the same TempleCode language:

### Python Version (Default)

```bash
timewarp-python
```

**Best for:**
- Educational use
- Easy modification
- Cross-platform compatibility
- Learning Python

**Pros:** Easy to customize, readable code
**Cons:** Slower startup

### Rust Version

```bash
timewarp-rust
```

**Best for:**
- Production use
- Performance-critical tasks
- Native applications
- Low resource usage

**Pros:** Very fast, low memory, single binary
**Cons:** Requires Rust toolchain for building

### Switch Default

```bash
# Make Rust the default
ln -sf ~/.local/bin/timewarp-rust ~/.local/bin/timewarp

# Make Python the default (current)
ln -sf ~/.local/bin/timewarp-python ~/.local/bin/timewarp
```

---

## 🛠 System Information

Your Time Warp installation:

- **OS**: Arch Linux ARM64
- **Python**: 3.13.7
- **Rust**: Installed and working
- **Qt**: PySide6 (for Python version)
- **GUI**: egui (for Rust version)

---

## 🔧 Maintenance

### Update Time Warp

```bash
cd ~/Time_Warp
git pull
./install-user.sh  # Reinstall with updates
```

### Uninstall

```bash
cd ~/Time_Warp
./install-user.sh --uninstall
```

### Reinstall Specific Version

```bash
# Python only
./install-user.sh --python-only

# Rust only
./install-user.sh --rust-only

# Both (current setup)
./install-user.sh
```

### Clear Cache

```bash
# Python cache
find ~/.local/share/timewarp/Time_Warp_Python -name __pycache__ -exec rm -rf {} +

# Rust rebuild
cd ~/.local/share/timewarp/Time_Warp_Rust
cargo clean
cargo build --release
```

---

## 📖 Help & Support

### Documentation Files

- **Arch Linux Guide**: `~/Time_Warp/ARCH_INSTALL.md`
- **General Install**: `~/Time_Warp/DEBIAN_INSTALL.md` (also helpful)
- **Main README**: `~/Time_Warp/README.md`
- **User Docs**: `~/.local/share/doc/timewarp/docs/`

### Online

- **GitHub**: https://github.com/James-HoneyBadger/Time_Warp
- **Issues**: https://github.com/James-HoneyBadger/Time_Warp/issues
- **Email**: james@honey-badger.org

### Quick Help

```bash
# Check installation
which timewarp
timewarp --version

# List launchers
ls -la ~/.local/bin/timewarp*

# Check desktop entries
ls -la ~/.local/share/applications/timewarp*.desktop

# View logs (if issues)
journalctl --user -b | grep -i timewarp
```

---

## 🎓 Learning Path

1. **Start with basics**: Try `logo_star.logo`
2. **Interactive learning**: Run `pilot_quiz.pilot`
3. **Write first program**: Create a simple PRINT statement
4. **Explore turtle graphics**: Draw shapes with Logo
5. **Mix languages**: Combine BASIC, PILOT, and Logo
6. **Advanced projects**: Fractals, games, interactive stories

---

## ✨ Quick Tips

### File Associations

Right-click `.pilot`, `.bas`, `.logo`, or `.tc` files → **Open With** → Time Warp IDE

### Desktop Shortcut

```bash
cp ~/.local/share/applications/timewarp-python.desktop ~/Desktop/
chmod +x ~/Desktop/timewarp-python.desktop
```

### Run from Any Directory

The `timewarp` command works from anywhere:

```bash
cd ~/Documents
timewarp myprogram.tc
```

### Open Multiple Files

```bash
timewarp file1.pilot file2.bas file3.logo
```

### Theme Selection

In the IDE:
- View → Themes
- Choose from 8 beautiful themes
- Settings persist between sessions

---

## 🎉 You're Ready!

Time Warp IDE is now installed and ready to use. Start with a simple example:

```bash
timewarp ~/.local/share/timewarp/examples/logo_star.logo
```

**Happy coding!** 🚀🐢

---

*Installation Date: November 1, 2025*  
*System: Arch Linux ARM64*  
*Python: 3.13.7*  
*Implementations: Python + Rust*
