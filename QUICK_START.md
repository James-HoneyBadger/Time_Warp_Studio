# Quick Start Guide

Get Time Warp Studio running in less than 2 minutes.

---

## ⚡ Fastest Way (Recommended)

### macOS/Linux
```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
./run.sh
```

### Windows (PowerShell)
```powershell
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
python run.py
```

**That's it!** The launcher will:
- Check your Python version
- Create a virtual environment
- Install all dependencies
- Launch the IDE

---

## 📋 Requirements

- **Python 3.10+** - Download from [python.org](https://www.python.org/)
- **Git** - Download from [git-scm.com](https://git-scm.com/)
- **Internet connection** - For downloading dependencies
- **~500MB free disk space**

---

## 🚀 First Launch

When you run the launcher for the first time:

```
============================================================
⏰ Time Warp Studio Launcher
============================================================

ℹ️  Checking Python version...
✅ Python 3.11.5

============================================================
📦 Setting up dependencies
============================================================

ℹ️  Upgrading pip...
✅ pip upgraded
ℹ️  Installing dependencies from requirements.txt...
✅ Dependencies installed

============================================================
🔍 Verifying installation
============================================================

✅ IDE script found: time_warp_ide.py

============================================================
🚀 Launching Time Warp Studio
============================================================
```

The IDE window will then appear.

---

## 🎯 Your First Program

1. **Choose Language**: Select **BASIC** from the language dropdown
2. **Write Code**: Type
   ```basic
   PRINT "Hello, World!"
   ```
3. **Run**: Click **Run** or press `Ctrl+Enter`
4. **See Output**: Check the output console below

---

## 💡 Try More Features

### Logo Graphics
Create a new file `square.logo`:
```logo
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
```
Click Run to see a square drawn on the canvas.

### Debug a Program
1. Write a BASIC program with variables
2. Click on any line number to set a breakpoint
3. Press **Run** to start debugging
4. Use **F10** to step over lines, **F11** to step into functions
5. Watch variables update in the Variables panel

---

## 📂 Project Layout

What you just cloned:

```
Time_Warp_Studio/
├── run.py              👈 Smart launcher (just click this)
├── run.sh              👈 Or this on Linux/Mac
├── Platforms/
│   └── Python/         📝 The actual IDE code
├── Examples/           📚 86+ example programs
├── docs/               📖 Complete documentation
└── README.md           📄 Full project info
```

---

## 🔧 Launcher Options

The smart launcher has several options:

```bash
# Standard run (creates venv if needed)
python run.py

# Skip setup (if you know venv is ready)
python run.py --skip-setup

# Force fresh setup (delete and recreate venv)
python run.py --fresh

# Use system Python (not recommended)
python run.py --no-venv

# Show help
python run.py --help
```

---

## 📂 File Organization

Since the last cleanup:

- **Root directory** is tidy with just essential files
- **Demo programs** moved to `Examples/demo/`
- **All documentation** in `docs/` folder
- **Virtual environment** in `.venv/` (auto-created)
- **IDE code** nicely organized in `Platforms/Python/`

---

## 🆘 Troubleshooting

### "Python not found"
```bash
# Install Python 3.10+
# https://www.python.org/downloads/

# Verify installation
python --version  # Should be 3.10+
```

### "Permission denied" on run.sh
```bash
chmod +x run.sh
./run.sh
```

### "Illegal instruction" error
Your CPU doesn't support required features. Solutions:
- Use a different computer
- Update to latest OS
- Try in cloud VM (AWS, Google Cloud, etc.)

### Still stuck?
See [TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md) for detailed solutions.

---

## 📚 Next Steps

1. **Explore Examples**: Check `Examples/` for working programs
2. **Read Guides**:
   - [USER_GUIDE.md](docs/USER_GUIDE.md) - How to use the IDE
   - [LANGUAGE_GUIDE.md](docs/LANGUAGE_GUIDE.md) - Language syntax
   - [TURTLE_GRAPHICS.md](docs/TURTLE_GRAPHICS.md) - Graphics programming
3. **Try Debugging**: Set breakpoints and step through code
4. **Get Help**: See [FAQ.md](docs/FAQ.md) for common questions

---

## 🎓 Learning Resources

### Built-in Examples
```
File → Examples → [Choose language]
```

### Official Documentation
- User Guide: [docs/USER_GUIDE.md](docs/USER_GUIDE.md)
- All Languages: [docs/LANGUAGE_GUIDE.md](docs/LANGUAGE_GUIDE.md)
- Graphics: [docs/TURTLE_GRAPHICS.md](docs/TURTLE_GRAPHICS.md)
- Debugging: [docs/DEBUGGER_GUIDE.md](docs/DEBUGGER_GUIDE.md)

### Keyboard Shortcuts
| Shortcut | Action |
|----------|--------|
| Ctrl+N | New file |
| Ctrl+O | Open file |
| Ctrl+S | Save file |
| Ctrl+R / F5 | Run program |
| Ctrl+B | Toggle breakpoint |
| F10 | Step over |
| F11 | Step into |
| Ctrl+E | Load example |

---

## ✨ Happy Coding!

You now have a fully functional multi-language programming environment. Enjoy exploring!

**Questions?** Check [FAQ.md](docs/FAQ.md)  
**Problems?** See [TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md)  
**Contributing?** Read [CONTRIBUTING.md](CONTRIBUTING.md)

---

**Version:** 7.0.0  
**License:** MIT  
**Author:** James Temple  
**Repository:** https://github.com/James-HoneyBadger/Time_Warp_Studio
