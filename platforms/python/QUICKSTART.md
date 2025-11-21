# Time Warp IDE - Python Version - Quick Start

## 🚀 Getting Started

The Python version is **fully functional** with a complete PySide6 GUI and unified TempleCode language support (BASIC + PILOT + Logo).

### Installation

**Option 1: Direct Run (Recommended)**
```bash
cd Time_Warp_Python
pip install PySide6 pillow
python time_warp_ide.py
```

**Option 2: Development Install**
```bash
cd Time_Warp_Python
pip install -e .
python time_warp_ide.py
```

---

## 🎮 Running the IDE

### Graphical IDE (Full Featured)

```bash
python time_warp_ide.py
```

**Open a specific file:**
```bash
python time_warp_ide.py examples/logo_spiral_walk.logo
python time_warp_ide.py examples/basic_hangman.bas
python time_warp_ide.py examples/pilot_adventure.pilot
```

**IDE Features:**
- Syntax-aware code editor with auto-indent
- Interactive turtle graphics canvas (zoom/pan)
- Multi-tab output (Text + Graphics)
- Recent files menu
- 8 color themes (Dracula, Monokai, Solarized, Ocean, Spring, Sunset, Candy, Forest)
- Run/Stop controls with status feedback

---

## 💻 CLI Usage

**Run programs from command line:**

```bash
# Using the CLI runner
python run_time_warp.py examples/logo_square.logo

# Interactive REPL
python run_time_warp.py --interactive

# Show turtle info
python run_time_warp.py examples/logo_square.logo --turtle
```

**Using Python API:**

```python
from time_warp.core.interpreter import Interpreter
from time_warp.graphics.turtle_state import TurtleState

interp = Interpreter()
turtle = TurtleState()

# TempleCode program
code = """
TO SQUARE :SIZE
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
END

SETCOLOR blue
PENWIDTH 3
SQUARE 100
"""

output = interp.execute_templecode(code, turtle)
print(output)
```

---

## 📝 Demo Script

Run comprehensive demonstration:

```bash
./demo.sh
```

**Demonstrates:**
- Logo turtle graphics with procedures
- PILOT interactive text processing
- BASIC arithmetic and control flow
- Error detection with suggestions

---

## ✅ What Works

**TempleCode Language Support:**
- ✅ **PILOT**: 11 commands (T:, A:, M:, Y:, N:, C:, U:, J:, L:, E:, R:)
- ✅ **BASIC**: 20+ commands (PRINT, LET, INPUT, GOTO, FOR/NEXT, IF/THEN, etc.)
- ✅ **Logo**: 50+ turtle commands + procedures (TO/END)

**Core Systems:**
- ✅ **Expression Evaluator**: Safe math with 15+ functions (SIN, COS, SQRT, etc.)
- ✅ **Error Hints**: 100+ command suggestions for typos
- ✅ **Turtle Graphics**: Full support with colors, pen width, procedures
- ✅ **GUI**: Complete PySide6 interface with themes
- ✅ **34 Example Programs**: Ready to run

**Logo Features:**
- ✅ User-defined procedures: `TO name :param1 :param2 ... END`
- ✅ Multi-line REPEAT blocks: `REPEAT n [ ... ]`
- ✅ Named colors: `SETCOLOR blue` (14 colors)
- ✅ Hex colors: `SETCOLOR #FF5733`
- ✅ RGB colors: `SETCOLOR 255,100,50`
- ✅ Expression evaluation: `RIGHT 360 / :SIDES`
- ✅ All aliases: PENWIDTH, BACKWARD, CLEAR, etc.

---

## 🧪 Testing

```bash
# Test all turtle graphics commands
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

**Test Results:**
```
Testing PILOT... ✅
Testing BASIC... ✅
Testing Logo... ✅
Testing Turtle Graphics (50+ commands)... ✅
Testing Expression Evaluator... ✅
Testing Error Hints... ✅

ALL TESTS PASSED!
```

---

## 🔒 Security

- **Iteration Limit**: 100,000 max (prevents infinite loops)
- **Timeout**: 10 seconds per execution (prevents DoS)
- **Safe Evaluation**: Manual expression parsing (no `eval()` or `exec()`)
- **Input Validation**: All user input sanitized
---

## 📚 Example Programs

**34 programs included - organized by language style:**

### Logo Turtle Graphics (15 programs)
- `logo_square.logo` - Simple square ✅
- `logo_spiral_walk.logo` - Colorful spiral ✅
- `logo_starburst.logo` - Radial starburst ✅
- `logo_flower.logo` - Petal flower with procedures ✅
- `logo_koch_snowflake.logo` - Fractal Koch curve ✅
- `logo_spirograph.logo` - Mathematical spirograph ✅
- `logo_fractal_tree.logo` - Recursive tree ✅
- `logo_polygonal_rose.logo` - Rose curve ✅
- ...and more!

### BASIC Programs (10 programs)
- `basic_hangman.bas` - Word guessing game ✅
- `basic_graphics.bas` - Graphics demo ✅
- `basic_countdown.bas` - Animated countdown ✅
- `basic_multiplication_table.bas` - Math practice ✅
- `basic_rock_paper_scissors.bas` - Classic game ✅
- `basic_inkey_demo.bas` - Keyboard input ✅
- ...and more!

### PILOT Interactive (7 programs)
- `pilot_adventure.pilot` - Text adventure ✅
- `pilot_quiz_competition.pilot` - Quiz game ✅
- `pilot_dragon_adventure.pilot` - Story game ✅
- `pilot_simple_calculator.pilot` - Calculator ✅
- `pilot_story_builder.pilot` - Story creator ✅
- ...and more!

**All programs work in the GUI!** Programs requiring INPUT use dialog prompts.

---

## 🎯 Quick Examples

### Simple Logo Program
```logo
REPEAT 6 [
  FORWARD 100
  RIGHT 60
]
```

### Logo with Procedures
```logo
TO STAR :SIZE
  REPEAT 5 [
    FORWARD :SIZE
    RIGHT 144
  ]
END

SETCOLOR red
STAR 100
```

### BASIC Program
```basic
10 CLS
20 FOR I = 1 TO 10
30   PRINT I * I
40 NEXT I
50 PRINT "Done!"
```

### PILOT Program
```pilot
T:Welcome! What's your name?
A:NAME
T:Hello *NAME*!
T:Ready to learn?
M:yes,no
JY:LEARN
T:Come back when ready!
E:
L:LEARN
T:Great! Let's start...
E:
```

---

## 🚧 Known Limitations

1. **Compiler**: TempleCode → C transpilation is experimental (not in GUI)
2. **Test Coverage**: Comprehensive unit tests in progress (currently have integration tests)
3. **Some BASIC commands**: DIM, DATA, READ not yet implemented

---

## 🆚 Python vs Rust

| Aspect | Python | Rust |
|--------|--------|------|
| **Speed** | Fast enough | 10-50x faster |
| **GUI** | PySide6 (Qt) | egui (native) |
| **Themes** | 8 themes | 2 themes |
| **Tests** | 5 test scripts | 72 unit tests |
| **Ease of Use** | Very easy | Requires compilation |
| **Best For** | Education, prototyping | Production, performance |

**Recommendation**: Start with Python, switch to Rust if you need performance.

---

## 📖 More Resources

- **Full Documentation**: See [README.md](README.md)
- **Turtle Reference**: See [docs/TURTLE_GRAPHICS_REFERENCE.md](docs/TURTLE_GRAPHICS_REFERENCE.md)
- **Examples**: Browse [examples/](examples/) directory
- **Main Project**: See [../README.md](../README.md)

---

## 🐛 Troubleshooting

**IDE won't start:**
```bash
# Check PySide6 installation
pip install --upgrade PySide6 pillow
```

**Programs not running:**
- Check syntax (unified TempleCode allows mixing styles)
- Look at example programs for reference
- Check error messages for hints

**Turtle graphics not showing:**
- Click "Graphics" tab after running
- Check that commands like FORWARD/RIGHT are being called
- Verify pen is down: use PENDOWN if needed

---

<div align="center">

**Need help?** Check the full [README.md](README.md) or open an [issue](https://github.com/James-HoneyBadger/Time_Warp/issues)

</div>
