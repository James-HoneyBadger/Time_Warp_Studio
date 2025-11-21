# 🐍 Time Warp IDE - Python Implementation

**🎓 Educational Programming Platform - Accessibility & Portability Focus**

[![Python 3.8+](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://www.python.org)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](../LICENSE)
[![Tests: Passing](https://img.shields.io/badge/Tests-Passing-success.svg)](../docs/TECHNICAL_REFERENCE.md)

> **🎯 Part of the Time Warp Educational Platform** — See [main documentation](../docs/) for complete guides and curriculum materials.

The **Python implementation** of Time Warp IDE prioritizes **accessibility and educational value**. With its pure-Python codebase, this version runs on any system with Python 3.8+, making it perfect for schools, coding camps, and educational environments where easy installation and cross-platform compatibility are essential.

## 🎯 Why Choose the Python Version?

- **📱 Universal Compatibility**: Runs on any device with Python (including Raspberry Pi)
- **🔧 Easy Installation**: Simple `pip install` or run directly from source
- **👨‍🏫 Education-First**: Designed specifically for classroom environments
- **🔍 Readable Code**: Students can explore the implementation to learn Python
- **📦 Lightweight**: Minimal dependencies make it perfect for restricted networks
- **🎨 Full Feature Set**: Complete TempleCode language with turtle graphics

---

## ✨ Features

### Language Features

- ✅ **Unified TempleCode**: Mix BASIC, PILOT, and Logo seamlessly
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
- 🎨 **8 Themes**: Dracula, Monokai, Solarized Dark, Ocean, Spring, Sunset, Candy, Forest
- 📁 **File Management**: Open/save with recent files history
- ▶️ **Execution Controls**: Run (F5), Stop (Shift+F5), Clear
- 📊 **Output Panel**: Colored text with emoji indicators
- 🔍 **Error Detection**: Syntax checking with helpful suggestions

### Educational Features

- 📚 **34 Example Programs**: All language styles and difficulty levels
- 📖 **Comprehensive Docs**: Turtle graphics reference and guides
- 💡 **Safe Execution**: Timeout protection and iteration limits
- 🎓 **CLI Mode**: Command-line REPL for quick testing
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
cd Time_Warp/Time_Warp_Python

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
cd Time_Warp_Python
python time_warp_ide.py
```

**IDE Features:**
- 🎨 Syntax-aware code editor with auto-indent
- 🐢 Interactive turtle graphics canvas with zoom/pan
- 📊 Multi-tab output panel (Text + Graphics)
- 💾 File operations with recent files menu
- 🌈 8 beautiful color themes (Dracula, Monokai, Solarized Dark, Ocean, Spring, Sunset, Candy, Forest)
- ⚡ Run/Stop execution with real-time feedback
- 🎯 Auto-switch to Graphics tab when drawing

### Try the Examples

```bash
# Logo turtle graphics
python time_warp_ide.py examples/logo_spiral_walk.logo
python time_warp_ide.py examples/logo_koch_snowflake.logo

# BASIC programs
python time_warp_ide.py examples/basic_hangman.bas
python time_warp_ide.py examples/basic_graphics.bas

# PILOT interactive tutorials
python time_warp_ide.py examples/pilot_adventure.pilot
python time_warp_ide.py examples/pilot_quiz_competition.pilot
```

### CLI Usage

Run programs from command line:

```python
from time_warp.core.interpreter import Interpreter
from time_warp.graphics.turtle_state import TurtleState

# Create interpreter and turtle
interp = Interpreter()
turtle = TurtleState()

# Execute TempleCode
code = """
FORWARD 100
RIGHT 90
FORWARD 100
"""

output = interp.execute_templecode(code, turtle)
print(output)
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
│   ├── templecode.py          # 🌟 Unified TempleCode executor (BASIC+PILOT+Logo)
│   ├── pilot.py               # Legacy compatibility wrapper
│   ├── basic.py               # Legacy compatibility wrapper
│   └── logo.py                # Legacy compatibility wrapper
├── graphics/                  # Turtle graphics system
│   └── turtle_state.py        # Turtle position, angle, pen state
├── utils/                     # Utilities
│   ├── expression_evaluator.py  # Safe math expression parser
│   └── error_hints.py           # Typo detection & suggestions
└── ui/                        # GUI components
    ├── main_window.py         # PySide6 main window
    ├── canvas.py              # Turtle graphics canvas
    ├── code_editor.py         # Syntax-aware editor
    └── theme_manager.py       # Theme system (8 themes)
```

**Key Design:** TempleCode is a **unified language** - all BASIC, PILOT, and Logo commands work together in a single program. The `templecode.py` executor handles all three syntaxes seamlessly.

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

# Execute TempleCode (unified BASIC+PILOT+Logo)
output = interp.execute_templecode(source_code, turtle_state)

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

**Current Version**: 2.0.0

| Component | Status | Completion |
|-----------|--------|------------|
| Core Interpreter | ✅ Complete | 100% |
| TempleCode Executor | ✅ Complete | 100% |
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

## 🆚 Comparison with Rust Version

| Feature | Python | Rust | Notes |
|---------|--------|------|-------|
| PILOT interpreter | ✅ | ✅ | Feature parity |
| BASIC interpreter | ✅ | ✅ | Feature parity |
| Logo interpreter | ✅ | ✅ | Feature parity |
| Logo procedures | ✅ | ✅ | TO/END with parameters |
| Turtle graphics | ✅ | ✅ | 50+ commands |
| Expression eval | ✅ | ✅ | Math + trig functions |
| Error hints | ✅ | ✅ | Typo suggestions |
| GUI | PySide6 | egui | Both functional |
| Themes | 8 themes | 2 themes | Python has more |
| Tests | 5 scripts | 72 tests | Rust has comprehensive suite |
| Performance | Good | Excellent | Rust is faster |

**Recommendation**: 
- **Python version**: Better for education, rapid prototyping, easier to modify
- **Rust version**: Better for production, performance-critical applications

---

## 🤝 Contributing

Contributions welcome! Areas needing work:

1. **Test Coverage** - Port comprehensive test suite from Rust version
2. **Documentation** - Complete API docs and tutorials
3. **Performance** - Optimize interpreter hot paths
4. **BASIC Extensions** - Add DIM, DATA, READ commands
5. **Examples** - More tutorial programs for beginners

**Development Setup:**
```bash
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/Time_Warp_Python
python -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python time_warp_ide.py
```

See [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines.

---

## 📜 License

MIT License - see [LICENSE](../LICENSE) file in repository root.

---

## 🙏 Credits

**Author**: James Temple ([@James-HoneyBadger](https://github.com/James-HoneyBadger))  
**Email**: james@honey-badger.org  
**Inspiration**: Classic educational computing (Apple II, Commodore 64, Logo, PILOT)

---

## 🔗 Links

- **Main Repository**: [Time_Warp](https://github.com/James-HoneyBadger/Time_Warp)
- **Rust Version**: [Time_Warp_Rust/](../Time_Warp_Rust/)
- **Examples**: [examples/](examples/)
## 📚 Learning & Documentation

### 🎓 **For Students & Beginners**
- **[📖 Student Lesson Book](../docs/STUDENT_LESSON_BOOK.md)** — Progressive 24-lesson curriculum with hands-on projects
- **[🎯 User Guide](../docs/USER_GUIDE.md)** — Complete installation and usage guide for all platforms  
- **[⚡ Quick Start Examples](examples/)** — Ready-to-run programs for immediate learning

### 👨‍🏫 **For Educators**  
- **[🍎 Teacher Guide & Curriculum](../docs/TEACHER_GUIDE.md)** — Complete educational framework with lesson plans
- **[📋 Assessment Tools](../docs/TEACHER_GUIDE.md#assessment-rubrics)** — Rubrics and evaluation strategies
- **[🎮 Interactive Projects](../docs/STUDENT_LESSON_BOOK.md#level-4-loops-and-patterns)** — Engaging programming challenges

### 🔧 **For Developers**
- **[⚙️ Technical Reference](../docs/TECHNICAL_REFERENCE.md)** — Architecture, APIs, and implementation details
- **[🏗️ Contributing Guide](../docs/CONTRIBUTING.md)** — How to extend and improve Time Warp
- **[📊 Project Status](../docs/TECHNICAL_REFERENCE.md#implementation-status)** — Development progress and roadmap

---

## 🔗 Links

- **Main Repository**: [Time_Warp](https://github.com/James-HoneyBadger/Time_Warp)
- **Rust Version**: [Time_Warp_Rust/](../Time_Warp_Rust/)
- **Web Version**: [Time_Warp_Web/](../Time_Warp_Web/)
- **Issues**: [GitHub Issues](https://github.com/James-HoneyBadger/Time_Warp/issues)

---

<div align="center">

**🐍 Time Warp IDE - Python Implementation** 

*Educational programming made accessible everywhere* 

🎓 **Perfect for Schools** • 📱 **Runs Anywhere** • � **Easy Setup**

Made with ❤️ for educators, students, and lifelong learners

</div>
