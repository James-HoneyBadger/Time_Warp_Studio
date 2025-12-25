# Quick Start Guide

Get Time Warp IDE running in seconds!

## 🚀 Fastest Way to Launch

### Linux / macOS
```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
./launch_ide.sh
```

### Windows
```cmd
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
launch_ide.bat
```

That's it! The script will:
- ✅ Create a virtual environment
- ✅ Install all dependencies
- ✅ Launch the IDE

**First run takes 1-2 minutes. Subsequent runs are instant.**

---

## 🎯 What to Do After Launch

### 1. Load an Example (File → Open)
```
Examples/logo/01_hello_world.logo
Examples/basic/hello_world.bas
Examples/pilot/hello.pilot
```

### 2. Click Run (Ctrl+R)
See immediate visual feedback!

### 3. Try the REPL
Type commands in Immediate Mode panel (bottom left):
```
FORWARD 50
RIGHT 90
FORWARD 50
```

### 4. Read the Tutorials
- **User Guide:** [docs/user-guide/README.md](docs/user-guide/README.md)
- **Language Tutorials:** [docs/tutorials/README.md](docs/tutorials/README.md)

---

## 📋 Requirements

- **Python 3.8+** (check: `python3 --version`)
- **Internet** (for first-time dependency installation)
- **4GB RAM** (minimum)
- **Linux, macOS, or Windows**

---

## 🆘 Troubleshooting

**Python not found?**
- Install Python 3.8+ from https://python.org
- On Windows, check "Add Python to PATH" during installation

**Permission denied? (Linux/macOS)**
```bash
chmod +x launch_ide.sh
./launch_ide.sh
```

**Dependencies failed?**
```bash
# Force reinstall
./launch_ide.sh --reinstall
```

**Need more help?**
- See [LAUNCHING.md](LAUNCHING.md) for detailed instructions
- See [Technical Reference](docs/technical/README.md) for advanced setup

---

## 📚 Next Steps

1. **Complete User Guide:** [docs/user-guide/README.md](docs/user-guide/README.md) (10 min read)
2. **Pick a Language:** [docs/tutorials/README.md](docs/tutorials/README.md) (30-60 min tutorials)
3. **Build Projects:** Use multiple languages together

---

## 🎮 Try These Quick Examples

**Hello World in BASIC:**
```basic
PRINT "Hello, World!"
```

**Draw a Circle in Logo:**
```logo
REPEAT 360 [FORWARD 1 RIGHT 1]
```

**Pattern Matching in PILOT:**
```pilot
ACCEPT X(N)
MATCH: ACCEPT Y(N)
  C: Y = X → PRINT "Correct!"
  C: ELSE → JUMP ACCEPT
```

---

## 💡 Pro Tips

- **Keyboard Shortcuts:**
  - `Ctrl+R` - Run program
  - `Ctrl+S` - Save
  - `Ctrl+O` - Open
  - `Ctrl+N` - New

- **Themes:** View → Theme (8 themes available)

- **Variables Panel:** Watch variables update in real-time

- **Canvas Zoom:** Mouse wheel to zoom graphics

---

## 🔗 Important Links

| Resource | URL |
|----------|-----|
| **User Guide** | [docs/user-guide/README.md](docs/user-guide/README.md) |
| **Launch Instructions** | [LAUNCHING.md](LAUNCHING.md) |
| **Programming Tutorials** | [docs/tutorials/README.md](docs/tutorials/README.md) |
| **API Reference** | [docs/technical/api.md](docs/technical/api.md) |
| **GitHub Repository** | https://github.com/James-HoneyBadger/Time_Warp_Studio |

---

## ✨ Features at a Glance

✅ **7 Programming Languages** - BASIC, PILOT, Logo, Pascal, Prolog, Forth, C  
✅ **Turtle Graphics** - Real-time drawing and animation  
✅ **Code Editor** - Syntax highlighting, themes, snippets  
✅ **Immediate Mode** - REPL for quick testing  
✅ **50+ Examples** - Ready-to-run programs  
✅ **Educational** - Designed for learning  

---

Ready? Start with:
```bash
./launch_ide.sh
```

Happy coding! 🎉
