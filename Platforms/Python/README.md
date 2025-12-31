# 🐍 Time Warp IDE - Python Implementation

**🎓 Educational Programming Platform - Accessibility & Portability Focus**

[![Python 3.8+](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://www.python.org)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](../../LICENSE)
[![Tests: Passing](https://img.shields.io/badge/Tests-Passing-success.svg)](../../Docs/developer/00-developer-guide.md#testing)

> **🎯 Part of the Time Warp Educational Platform** — See [main documentation](../../Docs/INDEX.md) for complete guides and curriculum materials.

The **Python implementation** of Time Warp IDE prioritizes **accessibility and educational value**. With its pure-Python codebase, this version runs on any system with Python 3.8+, making it perfect for schools, coding camps, and educational environments where easy installation and cross-platform compatibility are essential.

## 🎯 Why Choose the Python Version?

- **📱 Universal Compatibility**: Runs on any device with Python (including Raspberry Pi)
- **🔧 Easy Installation**: Simple `pip install` or run directly from source
- **👨‍🏫 Education-First**: Designed specifically for classroom environments
- **🔍 Readable Code**: Students can explore the implementation to learn Python
- **📦 Lightweight**: Minimal dependencies make it perfect for restricted networks
- **🎨 Full Feature Set**: Complete multi-language support with turtle graphics

---

## ✨ Features

### Language Features

- ✅ **Unified Environment**: Mix BASIC, PILOT, and Logo seamlessly
- ✅ **50+ Commands**: Complete verified command set
- ✅ **Turtle Graphics**: Full Logo compatibility with procedures
- ✅ **Color Support**: Named colors, hex codes, and RGB values
- ✅ **User Procedures**: TO/END with parameters and local variables
- ✅ **Multi-line Loops**: REPEAT blocks with proper nesting
- ✅ **Expression Evaluation**: Safe math with operator precedence
- ✅ **Pattern Matching**: PILOT-style wildcards and conditions
- ✅ **Error Recovery**: Continues on non-fatal errors with hints

### IDE Features

- 🎨 **Modern PySide6 UI**: Professional desktop interface
- 🐢 **Interactive Canvas**: Zoom/pan turtle graphics with coordinate axes
- 📝 **Code Editor**: Syntax highlighting and line numbers
- 🎨 **23 Themes**: Dark, Light, Dracula, Monokai, VS Code Dark/Light, GitHub Dark/Light, Nord, Solarized Dark/Light, Ocean, Spring, retro (CGA, C64, Apple II, DOS Blue, ZX Spectrum), and more
- 📁 **File Management**: Open/save with recent files history
- ▶️ **Execution Controls**: Run (F5), Stop (Shift+F5), Clear
- 📊 **Output Panel**: Colored text with emoji indicators
- 🔍 **Error Detection**: Syntax checking with helpful suggestions

### Educational Features

- 📚 **34 Example Programs**: All language styles and difficulty levels
- 📖 **Comprehensive Docs**: Turtle graphics reference and guides
- 💡 **Safe Execution**: Timeout protection and iteration limits
- 🧪 **Full Test Suite**: Verified correctness of all commands

---

## 🚀 Installation

---

## 🚀 Installation

### Prerequisites

- **Python 3.8 or higher**
- **PySide6** for GUI (or tkinter as fallback)
- **Pillow** for image processing (optional)

### Quick Install

```bash
# Clone the repository
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/Platforms/Python

# Install dependencies
pip install PySide6 pillow

# Launch the IDE
python time_warp_ide.py
```

### From Package

```bash
# Install from source (development mode)
pip install -e .

# Or install directly
pip install .
```

### Virtual Environment (Recommended)

```bash
# Create virtual environment
python -m venv venv

# Activate it
source venv/bin/activate  # Linux/Mac
venv\Scripts\activate     # Windows

# Install dependencies
pip install PySide6 pillow

# Run IDE
python time_warp_ide.py
```

---

## 📖 Quick Start

### Desktop IDE (Recommended)

Launch the full graphical IDE:

```bash
cd Platforms/Python
python time_warp_ide.py
```

**IDE Features:**
- 🎨 Syntax-aware code editor with auto-indent
- 🐢 Interactive turtle graphics canvas with zoom/pan
- 📊 Multi-tab output panel (Text + Graphics)
- 💾 File operations with recent files menu
- 🌈 23 beautiful color themes (dark, light, retro, modern with syntax highlighting for all)
- ⚡ Run/Stop execution with real-time feedback
- 🎯 Auto-switch to Graphics tab when drawing

### Try the Examples

```bash
# Logo turtle graphics
python time_warp_ide.py ../../Examples/logo/spiral.logo
python time_warp_ide.py ../../Examples/logo/square.logo

# BASIC programs
python time_warp_ide.py ../../Examples/basic/guessing_game.bas
python time_warp_ide.py ../../Examples/basic/loops.bas

# PILOT interactive tutorials
python time_warp_ide.py ../../Examples/pilot/hello.pilot
python time_warp_ide.py ../../Examples/pilot/quiz.pilot
```

### Language Examples

**PILOT-style (Interactive):**
```pilot
T:What is your name?
A:NAME
T:Hello *NAME*! Welcome to Time Warp.
M:yes
JY:CONTINUE
J:START
L:CONTINUE
T:Let's learn some programming!
E:
```

**BASIC-style (Imperative):**
```basic
10 CLS
20 PRINT "Countdown"
30 FOR I = 10 TO 1 STEP -1
40   LOCATE I, 10
45   PRINT I
50 NEXT I
60 PRINT "Blastoff!"
```

**Logo-style (Turtle Graphics):**
```logo
TO SQUARE :SIZE
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
END

SETCOLOR blue
PENWIDTH 3
SQUARE 100
```

---

## 🏗️ Architecture

```
time_warp/
├── core/                      # Core interpreter engine
│   └── interpreter.py         # Main execution engine
├── languages/                 # Language executors
│   ├── pilot.py               # PILOT language executor
│   ├── basic.py               # BASIC language executor
│   └── logo.py                # Logo language executor
├── graphics/                  # Turtle graphics system
│   └── turtle_state.py        # Turtle position, angle, pen state
├── utils/                     # Utilities
│   ├── expression_evaluator.py  # Safe math expression parser
│   └── error_hints.py           # Typo detection & suggestions
└── ui/                        # GUI components
    ├── main_window.py         # PySide6 main window
    ├── canvas.py              # Turtle graphics canvas
    ├── code_editor.py         # Syntax-aware editor
    └── theme_manager.py       # Theme system (23 themes)
```

**Key Design:** Time Warp is a **unified environment** - BASIC, PILOT, and Logo commands work together in a single program. The `interpreter.py` engine handles all three syntaxes seamlessly.

---

## 🧪 Testing

```bash
# Run comprehensive turtle graphics tests
python test_all_turtle_commands.py

# Test basic functionality
python test_basic_functionality.py

# Test IDE components
python test_ide.py

# Test graphics rendering
python test_graphics.py

# Verify all commands
python verify_commands.py
```

**Test Coverage:**
- ✅ 50+ turtle graphics commands verified
- ✅ Logo procedures (TO/END) with parameters
- ✅ Multi-line REPEAT blocks
- ✅ Expression evaluation in commands
- ✅ Named colors, hex colors, RGB colors
- ✅ All pen width and movement aliases
- ⏳ Full unit test suite in progress

---

## 📚 API Reference

### Interpreter

```python
from time_warp.core.interpreter import Interpreter

interp = Interpreter()

# Execute program
output = interp.execute(source_code, turtle_state)

# Legacy methods (compatibility)
output = interp.execute_pilot(pilot_code, turtle_state)
output = interp.execute_basic(basic_code, turtle_state)
output = interp.execute_logo(logo_code, turtle_state)
```

### Turtle Graphics

```python
from time_warp.graphics.turtle_state import TurtleState

turtle = TurtleState(width=800, height=600)

# Movement
turtle.forward(100)
turtle.backward(50)
turtle.left(90)
turtle.right(45)

# Position
turtle.setxy(100, 200)
turtle.setx(150)
turtle.sety(250)
turtle.home()  # Return to (0, 0)

# Pen control
turtle.penup()
turtle.pendown()
turtle.penwidth(5)

# Colors
turtle.setcolor("#FF5733")       # Hex
turtle.setcolor("255,100,50")    # RGB
turtle.setcolor("blue")          # Named color
turtle.setbgcolor("black")

# State queries
x, y = turtle.position()
angle = turtle.heading()
visible = turtle.isvisible()
```

### Expression Evaluator

```python
from time_warp.utils.expression_evaluator import ExpressionEvaluator

evaluator = ExpressionEvaluator({'X': 5, 'Y': 3})
result = evaluator.evaluate('X * 2 + Y')      # 13.0
result = evaluator.evaluate('SIN(45) + COS(30)')  # Supports trig
result = evaluator.evaluate('360 / 6')        # 60.0
```

### Error Hints

```python
from time_warp.utils.error_hints import ErrorHints

hints = ErrorHints()
suggestion = hints.get_suggestion('FORWRD', ['FORWARD', 'BACKWARD'])
# Returns: "Did you mean 'FORWARD'?"
```

---

## 🔒 Security Features

- **Iteration Limit**: 100,000 max iterations prevents infinite loops
- **Timeout Protection**: 10-second execution limit per command
- **Safe Evaluation**: Expression evaluator uses manual parsing - no `eval()` or `exec()`
- **Token Limit**: Complexity limits on mathematical expressions
- **Input Validation**: All user inputs sanitized before execution

---

## 📊 Development Status

**Current Version**: 5.1.0

| Component | Status | Completion |
|-----------|--------|------------|
| Core Interpreter | ✅ Complete | 100% |
| Language Executors | ✅ Complete | 100% |
| Turtle Graphics | ✅ Complete | 100% |
| Logo Procedures | ✅ Complete | 100% |
| Expression Evaluator | ✅ Complete | 100% |
| Error Hints | ✅ Complete | 100% |
| PySide6 GUI | ✅ Complete | 100% |
| Theme System | ✅ Complete | 100% |
| Example Programs | ✅ Complete | 34 programs |
| Test Suite | ⏳ In Progress | 60% |
| Documentation | ⏳ In Progress | 70% |

**Recent Updates:**
- ✅ Logo procedures (TO/END) with parameters
- ✅ Multi-line REPEAT blocks (top-level and nested)
- ✅ SETCOLOR with 14 named colors + hex/RGB
- ✅ All turtle command aliases (PENWIDTH, BACKWARD, CLEAR, etc.)
- ✅ Expression evaluation in rotation commands (e.g., RIGHT 360 / :SIDES)
- ✅ 50+ turtle graphics commands verified working

---

## 🖥 Platform Snapshot

| Platform | Location | Status | Notes |
|----------|----------|--------|-------|
| Python (PySide6) | `Platforms/Python/` | ✅ Official | Primary desktop IDE with full BASIC, PILOT, and Logo support |
| Browser (HTML/JS) | `Platforms/Browser/` | 🧪 Experimental | Prototype for future web deployment |
| DOS (C89) | `Platforms/DOS/` | 🧪 Experimental | Retro text-mode interpreter retained for history labs |

Legacy implementations such as Go, Haiku, and others were removed during the v5.1.0 cleanup to keep maintenance focused on the supported stack.

---

## 🤝 Contributing

Contributions welcome! Areas needing work:

1. **Test Coverage** - Expand comprehensive test suite
2. **Documentation** - Complete API docs and tutorials
3. **Performance** - Optimize interpreter hot paths
4. **BASIC Extensions** - Add DIM, DATA, READ commands
5. **Examples** - More tutorial programs for beginners

**Development Setup:**
```bash
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/Platforms/Python
python -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python time_warp_ide.py
```

See the [Developer Guide](../../Docs/developer/00-developer-guide.md) for contribution guidelines.

---

## 📜 License

MIT License - see [LICENSE](../../LICENSE) file in repository root.

---

## 🙏 Credits

**Author**: James Temple ([@James-HoneyBadger](https://github.com/James-HoneyBadger))  
**Email**: james@honey-badger.org  
**Inspiration**: Classic educational computing (Apple II, Commodore 64, Logo, PILOT)

## 📚 Learning & Documentation

### 🎓 For Students & Beginners
- **[Student Workbook](../../Docs/student/00-workbook.md)** — Progressive lessons that reinforce BASIC, PILOT, and Logo fundamentals.
- **[User Manual](../../Docs/user/00-user-manual.md)** — Guided tour of the IDE interface and workspace.
- **[Programming Guide](../../Docs/user/01-programming-guide.md)** — Language syntax deep dive with runnable samples.
- **[Quick Reference](../../Docs/user/02-quick-reference.md)** — Emoji legend and command cheat sheets for classroom use.

### 👨‍🏫 For Educators
- **[Teacher Overview](../../Docs/teacher/00-overview.md)** — Curriculum framing, assessments, and classroom management tips.
- **[FAQ](../../Docs/user/03-faq.md)** — Answers to common setup and pedagogy questions you can share with students.

### 🔧 For Developers
- **[Developer Guide](../../Docs/developer/00-developer-guide.md)** — Contribution workflow, testing expectations, and release process.
- **[Installation Quickstart](../../Docs/installation/00-quickstart.md)** — Steps to configure the Python IDE on supported platforms.
- **[Release Notes](../../Docs/misc/RELEASE_NOTES.md)** — Historical highlights and version-specific changes.

### 📦 Assets & Examples
- **Example Programs**: [`Examples/`](../../Examples/) — A curated set of BASIC, PILOT, and Logo samples for lessons and demos.
- **Browser Prototype**: [`Platforms/Browser/`](../Browser/) — Experimental HTML/JS build for future deployment.
- **DOS Interpreter**: [`Platforms/DOS/`](../DOS/) — C89 text-mode interpreter for historical computing labs.

---

## 🔗 Links

- **Main Repository**: [Time_Warp](https://github.com/James-HoneyBadger/Time_Warp)
- **Issue Tracker**: <https://github.com/James-HoneyBadger/Time_Warp/issues>
- **Discussions**: <https://github.com/James-HoneyBadger/Time_Warp/discussions>

---

<div align="center">

**🐍 Time Warp IDE - Python Implementation** 

*Educational programming made accessible everywhere* 

🎓 **Perfect for Schools** • 📱 **Runs Anywhere** • � **Easy Setup**

Made with ❤️ for educators, students, and lifelong learners

</div>
