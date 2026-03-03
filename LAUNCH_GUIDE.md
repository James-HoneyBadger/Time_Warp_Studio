# Time Warp Studio - Launch Guide

## Quick Start (2 Minutes)

### Prerequisites
- Python 3.10 or higher
- Modern operating system (Windows, macOS, Linux)
- 4GB RAM recommended
- Modern CPU with SSSE3/SSE4 support

### Setup & Launch

**Linux/macOS:**
```bash
cd Time_Warp_Studio
python3 -m venv .venv
source .venv/bin/activate
pip install -r Platforms/Python/requirements.txt
python Platforms/Python/time_warp_ide.py
```

**Windows:**
```cmd
cd Time_Warp_Studio
python -m venv .venv
.venv\Scripts\activate
pip install -r Platforms/Python/requirements.txt
python Platforms/Python/time_warp_ide.py
```

The IDE will start in ~5 seconds. If successful, you'll see the Time Warp Studio main window with the code editor, turtle graphics canvas, and output panel.

---

## What You Can Do Right Now

### 1. Run Your First Program
1. Select **BASIC** from Language dropdown (top right)
2. Type: `PRINT "Hello, World!"`
3. Press **Ctrl+R** or click **Run**

### 2. Explore Examples
- Press **Ctrl+E** or go to **File → Examples**
- Browse 220+ programs across 24 languages
- Load any example and press **Ctrl+R** to run

### 3. Try Turtle Graphics
- Select **Logo** from Language dropdown
- Paste this code:
  ```logo
  REPEAT 4 [FORWARD 100 RIGHT 90]
  ```
- Press **Ctrl+R** to draw a square

### 4. Work Through a Lesson
- Go to **File → Lessons**
- Select a lesson (e.g., "BASIC Fundamentals")
- Follow step-by-step instructions with auto-verification

---

## Project Structure

```
Time_Warp_Studio/
├── Platforms/Python/
│   ├── time_warp_ide.py           ← Main IDE executable
│   ├── time_warp/                 ← Core application
│   │   ├── core/                  ← Interpreter and language executors
│   │   ├── features/              ← Lesson, autosave, theme systems
│   │   ├── ui/                    ← PySide6 GUI components
│   │   └── tests/                 ← Test suite (28 tests)
│   ├── requirements.txt           ← Python dependencies
│   └── test_runner.py             ← Test orchestration
├── Examples/                      ← 220+ example programs
├── docs/                          ← Complete documentation
│   ├── guides/                    ← How-to guides
│   ├── tutorials/                 ← Language tutorials
│   └── reference/                 ← API reference and FAQ
└── config/                        ← Configuration files
```

---

## Features Overview

### Core Features
- ✅ **7 Languages**: BASIC, PILOT, Logo, C, Pascal, Prolog, Forth
- ✅ **Turtle Graphics**: Full turtle graphics with interactive canvas
- ✅ **Syntax Highlighting**: Code editing with language support
- ✅ **Real-time Output**: See program output instantly

### Learning Features
- ✅ **Lesson System**: Structured lessons with checkpoints and hints
- ✅ **Examples Browser**: Searchable catalog of 90+ programs
- ✅ **Turtle Preview**: Live visualization while coding
- ✅ **Auto-verification**: Lessons check your solutions automatically

### Productivity Features
- ✅ **Autosave**: Background saving with version history
- ✅ **Themes**: 8 built-in themes + custom theme editor
- ✅ **Classroom Mode**: Presentation mode and assignment support
- ✅ **Settings**: Persistent configuration across sessions

---

## Troubleshooting

### Issue: "ModuleNotFoundError: No module named 'PySide6'"
**Solution:** The venv hasn't been activated or dependencies weren't installed.
```bash
source .venv/bin/activate  # or .venv\Scripts\activate on Windows
pip install -r Platforms/Python/requirements.txt
```

### Issue: "Illegal instruction" Error
**Cause:** Your CPU lacks required CPU features (SSSE3, SSE4.1, SSE4.2, POPCNT)  
**Solution:** Run on modern hardware or upgrade to a VM with full CPU features

### Issue: Qt xcb platform plugin fails to load
**Symptoms:**
- `qt.qpa.plugin: From 6.5.0, xcb-cursor0 or libxcb-cursor0 is needed`
- `Could not load the Qt platform plugin "xcb"`

**Cause:** Missing Linux X11 runtime libraries required by Qt.

**Solution:** Install platform packages, then relaunch:
```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install -y libxcb-cursor0 libxkbcommon-x11-0 libxcb-icccm4 libxcb-keysyms1 libxcb-render-util0

# Fedora
sudo dnf install xcb-util-cursor libxkbcommon-x11 xcb-util-wm xcb-util-keysyms xcb-util-renderutil

# Arch/Arch Linux ARM
sudo pacman -S xcb-util-cursor libxkbcommon-x11 xcb-util-wm xcb-util-keysyms xcb-util-renderutil
```

### Issue: IDE Won't Start
**Solution:** Check Python version and dependencies:
```bash
python --version  # Should be 3.10 or higher
pip list | grep -E "PySide6|Pillow"
```

### Issue: Examples Don't Load
**Solution:** Ensure Examples/ directory exists with all files:
```bash
ls Examples/basic/  # Should show *.bas files
```

---

## Testing

Run the comprehensive test suite:
```bash
cd Platforms/Python
python test_runner.py --comprehensive
```

Run quick smoke tests:
```bash
python test_runner.py --basic
```

Run specific test file:
```bash
pytest tests/test_core_interpreter.py -v
```

---

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| **Ctrl+R** or **F5** | Run code |
| **Ctrl+E** | Open Examples browser |
| **Ctrl+S** | Save file |
| **Ctrl+O** | Open file |
| **Ctrl+N** | New file |
| **Ctrl+/** | Toggle comment |
| **Ctrl+Z** | Undo |
| **Ctrl+Y** | Redo |

---

## Documentation & Help

- 📖 **Full Documentation**: [docs/INDEX.md](docs/INDEX.md)
- 🚀 **Getting Started**: [docs/guides/01-getting-started.md](docs/guides/01-getting-started.md)
- 🐢 **Turtle Graphics**: [docs/guides/04-turtle-graphics.md](docs/guides/04-turtle-graphics.md)
- 📚 **Language Tutorials**: [docs/tutorials/](docs/tutorials/)
- ❓ **FAQ**: [docs/reference/faq.md](docs/reference/faq.md)
- 🏗️ **Architecture**: [ARCHITECTURE.md](ARCHITECTURE.md)

---

## Next Steps

1. ✅ IDE is running - explore the interface
2. 📝 **Run examples** - Open File → Examples to browse 90+ programs
3. 📚 **Try lessons** - Open File → Lessons to follow structured learning paths
4. 🐢 **Learn Logo** - Draw with turtle graphics using simple commands
5. 💾 **Create your own** - Start a new file and write your first program

---

## Support

- **Issues & Bugs**: [GitHub Issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)
- **Discussions**: [GitHub Discussions](https://github.com/James-HoneyBadger/Time_Warp_Studio/discussions)
- **Email**: james@honey-badger.org

Enjoy learning and programming with Time Warp Studio! 🚀

---

**Next Steps:** Follow the [Getting Started Guide](docs/guides/01-getting-started.md) for your first program!
