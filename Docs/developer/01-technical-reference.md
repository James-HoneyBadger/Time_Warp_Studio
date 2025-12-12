# Technical Reference — Time Warp IDE

This reference documents the internal architecture, public interfaces, and implementation details for Time Warp IDE.

Use this document if you're extending the core, writing a new language executor, or working on UI features.

---

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [Language Executor API](#2-language-executor-api)
3. [Safe Expression Evaluator](#3-safe-expression-evaluator)
4. [Turtle Graphics System](#4-turtle-graphics-system)
5. [UI Framework & Theming](#5-ui-framework--theming)
6. [Screen Modes](#6-screen-modes)
7. [Testing & QA](#7-testing--qa)
8. [Adding New Languages](#8-adding-new-languages)
9. [Adding UI Features](#9-adding-ui-features)

---

## 1. Architecture Overview

Time Warp IDE is a **Python (PySide6) application** that coordinates stateless language executors with a rich Qt6-based user interface.

### System Design

```
PySide6 Main Window (qt_ui.py)
  • Menu bar, toolbar, tabs
  • Code editor (syntax highlighting, snippets)
  • Graphics canvas (turtle rendering)
  • Output panel (text console)
         ↓
TimeWarpInterpreter (core/interpreter.py)
  • Routes code to executors
  • Manages turtle state
  • Coordinates I/O
         ↓
Language Executors (languages/)
  ├─ BASIC (basic.py)
  ├─ PILOT (pilot.py)
  ├─ Logo (logo.py)
  ├─ Pascal (pascal.py)
  ├─ Prolog (prolog.py)
  └─ C (c_lang_fixed.py)
```

### Key Principles

1. **Stateless Executors** – Executors process commands and return strings; they don't modify external state
2. **Emoji Protocol** – All output prefixed with standardized emoji (❌, ✅, ℹ️, 🐢, 🚀)
3. **Safe Evaluation** – Use `safe_eval()` only; `eval()` is forbidden
4. **Turtle via Polling** – UI reads executor state (position, heading, pen) to render, not message-driven

---

## 2. Language Executor API

All language executors implement the same interface.

### Python Base Class

```python
class LanguageExecutor:
    """Base interface for language executors."""
    
    def __init__(self, interpreter):
        """Initialize executor with reference to parent interpreter."""
        self.interpreter = interpreter
    
    def execute_command(self, command: str) -> str:
        """Execute a single command or line of code.
        
        Returns:
            Emoji-prefixed output string:
            - "❌ Error message" for errors
            - "✅ Success" for success confirmations
            - "ℹ️ Info" for informational messages
            - "🐢 Lines drawn" for turtle actions
            - "🚀 event" for execution events
        """
        raise NotImplementedError
    
    def reset(self):
        """Reset executor to initial state."""
        raise NotImplementedError
```

### Executor Responsibilities

✅ **DO:**
- Parse input syntax for your language
- Validate expressions and statements
- Return appropriate emoji-prefixed strings
- Maintain internal state (variables, loop counters)

❌ **DON'T:**
- Directly call UI methods
- Modify TurtleState (UI reads it instead)
- Use Python's `eval()` or `exec()`
- Throw exceptions (return error strings instead)

---

## 3. Safe Expression Evaluator

**CRITICAL**: Never use `eval()`. Use the safe evaluator instead.

```python
from time_warp.core.safe_expression_evaluator import safe_eval

# Use it
result = safe_eval("2 + 3 * X", {"X": 5})  # Returns 17.0
```

### Supported Operations

- **Arithmetic**: `+`, `-`, `*`, `/`, `%`, `**` (power)
- **Numbers**: Integers and floats
- **Variables**: Alphanumeric names
- **Functions**: `ABS()`, `INT()`, `SQR()`, `SQRT()`, `SIN()`, `COS()`, `TAN()`

### Unsupported (Intentionally)

- Variable assignment, loops, imports, function definitions
- `eval()`, `exec()`, `__import__`

---

## 4. Turtle Graphics System

### TurtleState Class

Located in `graphics/turtle_state.py`:

```python
class TurtleState:
    """Represents turtle position, heading, pen state, and drawing lines."""
    
    # Position and heading
    x: float                 # X coordinate (0 = center)
    y: float                 # Y coordinate (0 = center)
    heading: float           # Direction in degrees (0=right, 90=up)
    
    # Pen state
    pen_down: bool           # Is pen drawing?
    pen_color: tuple[int, int, int]  # RGB color (0-255)
    pen_width: int           # Pen width in pixels
    visible: bool            # Is turtle visible?
    
    # Graphics output
    lines: list[Line]        # All lines drawn
```

### Logo Executor Integration

```python
class LogoExecutor:
    def __init__(self, interpreter):
        self.interpreter = interpreter
        self.turtle = TurtleState()  # Own turtle instance
        
    def execute_command(self, command: str) -> str:
        # Parse and execute Logo commands
        if cmd == "FORWARD":
            self.turtle.forward(distance)
        return "🐢 Lines drawn\n"
```

### UI Reading Turtle State

The canvas widget reads turtle state after execution:

```python
def update_display(self, executor):
    """Read turtle state and render."""
    turtle = executor.turtle  # Direct read
    
    # Render all lines
    for line in turtle.lines:
        self.draw_line(line.start_x, line.start_y, 
                      line.end_x, line.end_y,
                      line.color, line.width)
    
    # Render turtle if visible
    if turtle.visible:
        self.draw_turtle(turtle.x, turtle.y, turtle.heading)
```

---

## 5. UI Framework & Theming

### Main Window Structure

File: `ui/main_window.py`

```python
class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        
        # Menu bar (File, Edit, Run, Language, View, Help)
        self.create_menus()
        
        # Central widget with tabs
        self.tabs = QTabWidget()
        self.editor_tab = CodeEditor()
        self.canvas_tab = Canvas()
        
        # Status bar
        self.statusbar = QStatusBar()
        
        # Tool windows
        self.variable_inspector = VariableInspectorDialog()
        self.debug_panel = DebugPanel()
```

### Styling System

Global stylesheet in `main_window.py`:

```python
self.setStyleSheet("""
    QTabBar::tab {
        background-color: palette(window);
        padding: 8px 16px;
    }
    QTabBar::tab:selected {
        background-color: palette(highlight);
        font-weight: bold;
    }
    
    QPushButton {
        background-color: palette(button);
        border-radius: 6px;
        padding: 6px 14px;
        font-weight: bold;
    }
    QPushButton:hover {
        background-color: palette(highlight);
    }
""")
```

### Theme System

File: `ui/themes.py` – 8 built-in themes:
- Dracula, Monokai, Solarized, Ocean, Spring, Sunset, Candy, Forest

Use palette roles for compatibility:
- `palette(window)` – Background
- `palette(base)` – Content
- `palette(button)` – Buttons
- `palette(highlight)` – Selection/hover
- `palette(text)` – Text color

---

## 6. Screen Modes

File: `ui/screen_modes.py`

### Available Modes

- **Text** – Editor full, canvas hidden
- **Graphics** – Canvas full, editor hidden
- **Single** – Editor left, canvas right
- **Combined** – Editor top, canvas bottom

---

## 7. Testing & QA

### Running Tests

```bash
# All tests with coverage
python Tests/run_tests.py --comprehensive

# Quick smoke tests
python Tests/run_tests.py --quick

# Specific test
pytest Tests/test_core_interpreter.py -v
```

### Writing Executor Tests

```python
from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState

def test_logo_forward():
    """Logo FORWARD moves turtle."""
    turtle = TurtleState()
    interp = Interpreter()
    interp.load_program("FORWARD 50\n", Language.LOGO)
    output = interp.execute(turtle)
    
    assert "❌" not in output  # No errors
    assert turtle.y == 50      # Moved forward
```

---

## 8. Adding New Languages

### Step 1: Create Executor

File: `languages/my_lang.py`

```python
class MyLangExecutor:
    def __init__(self, interpreter):
        self.interpreter = interpreter
        self.variables = {}
    
    def execute_command(self, command: str) -> str:
        tokens = command.strip().split()
        if not tokens:
            return ""
        
        verb = tokens[0].upper()
        if verb == "PRINT":
            return " ".join(tokens[1:]) + "\n"
        elif verb == "SET":
            self.variables[tokens[1]] = tokens[2]
            return f"ℹ️ {tokens[1]} = {tokens[2]}\n"
        else:
            return f"❌ Unknown: {verb}\n"
    
    def reset(self):
        self.variables.clear()
```

### Step 2: Register in Interpreter

File: `core/interpreter.py`

```python
from languages.my_lang import MyLangExecutor

class Interpreter:
    def __init__(self):
        self.my_lang = MyLangExecutor(self)
```

### Step 3: Add to Language Enum

```python
class Language(Enum):
    MY_LANG = "MY_LANG"
```

### Step 4: Add Syntax Highlighting

File: `ui/editor.py`

### Step 5: Write Tests

File: `Tests/test_my_lang.py`

---

## 9. Adding UI Features

### Adding a Dialog

```python
from PySide6.QtWidgets import QDialog, QPushButton, QVBoxLayout

class MyDialog(QDialog):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("My Feature")
        
        layout = QVBoxLayout()
        button = QPushButton("Action")
        button.clicked.connect(self.do_action)
        layout.addWidget(button)
        self.setLayout(layout)
    
    def do_action(self):
        pass
```

### Registering in Menu

```python
# In main_window.py
tools_menu = self.menuBar().addMenu("Tools")
my_action = tools_menu.addAction("My Feature")
my_action.triggered.connect(self.show_my_dialog)
```

---

For complete examples, see the [Developer Guide](00-developer-guide.md).
