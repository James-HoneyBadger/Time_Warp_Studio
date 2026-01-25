# Time Warp Studio Architecture Guide

**Complete architectural overview for developers and contributors**

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Core Components](#core-components)
3. [Language Executors](#language-executors)
4. [Data Flow](#data-flow)
5. [Feature Modules](#feature-modules)
6. [UI Architecture](#ui-architecture)
7. [Extending the IDE](#extending-the-ide)
8. [Development Guidelines](#development-guidelines)

---

## System Overview

### High-Level Architecture

Time Warp Studio is a **single-process desktop application** with a clear separation between the **core interpreter** and the **UI layer**.

```
┌─────────────────────────────────────────┐
│      PySide6 (Qt6) User Interface       │
│  ┌─────────────────────────────────────┐│
│  │ Editor | Canvas | Output | Panels   ││
│  └─────────────────────────────────────┘│
└────────────────┬────────────────────────┘
                 │
         Command Dispatch
                 │
┌────────────────▼────────────────────────┐
│    TimeWarpInterpreter (Core)           │
│  ┌──────────────────────────────────────┤
│  │ Language Executors:                  │
│  │ • BASIC → BasicExecutor              │
│  │ • PILOT → PilotExecutor              │
│  │ • Logo  → LogoExecutor               │
│  │ • Python, C, Pascal, Prolog, Forth   │
│  └──────────────────────────────────────┘
│  ┌──────────────────────────────────────┤
│  │ Support Modules:                     │
│  │ • safe_expression_evaluator.py       │
│  │ • analytics.py                       │
│  │ • debugger.py                        │
│  │ • [40+ other modules]                │
│  └──────────────────────────────────────┘
└────────────────┬────────────────────────┘
                 │
         Text Output with Emoji Prefixes
                 │
                 ▼
        Display in UI (Output Panel)
```

### Key Design Principles

1. **Stateless Executors**: Language executors don't own UI state; they return text output only
2. **Single Responsibility**: Each component has one clear purpose
3. **Text-Based Protocol**: Executors communicate via strings with emoji prefixes (❌, ✅, ℹ️, 🐢, 🎨, 🚀, 📝)
4. **Safe Evaluation**: Math expressions use protected evaluator, never raw `eval()`
5. **Async Support**: Non-blocking execution via AsyncInterpreterRunner
6. **Cross-Platform**: Works on Windows, macOS, Linux with identical behavior

---

## Core Components

### 1. TimeWarpInterpreter (`core/interpreter.py`)

The **central command dispatcher** and state manager.

**Responsibilities:**
- Detect programming language from code
- Dispatch commands to appropriate executor
- Maintain global interpreter state (variables, functions)
- Handle special commands (help, clear, etc.)
- Manage plugin system

**Key Methods:**
```python
class TimeWarpInterpreter:
    def execute(self, code: str) -> str
        """Execute code in appropriate language"""
    
    def get_language(self, code: str) -> str
        """Detect programming language"""
    
    def reset(self) -> str
        """Clear all state and reset"""
    
    def set_plugin(self, plugin) -> None
        """Register a plugin"""
```

**State Management:**
- Variables dictionary
- Function registry
- Output buffer
- Turtle graphics state (turtle_x, turtle_y, turtle_angle, pen_state)

### 2. Language Executors (`languages/`)

Each language has its own executor implementing the `LanguageExecutor` interface.

**Base Interface:**
```python
class LanguageExecutor:
    def __init__(self, interpreter: TimeWarpInterpreter):
        self.interpreter = interpreter
    
    def execute_command(self, command: str) -> str:
        """Parse and execute command, return string output"""
        pass
```

**Available Executors:**
- `basic.py` - BASIC interpreter (core)
- `pilot.py` - PILOT interpreter (core)
- `logo.py` - Logo with turtle graphics (core)
- `python.py` - Python support
- `c.py` - C language (experimental)
- `pascal.py` - Pascal language (experimental)
- `prolog.py` - Prolog language (experimental)
- `forth.py` - Forth language (experimental)

**Output Protocol - Emoji Prefixes:**
```
❌ - Error message (error, undefined variable, etc.)
✅ - Success confirmation ("Program executed successfully")
ℹ️ - Informational message
🎨 - Theme/UI change notification
🚀 - Execution event (program start, end)
🐢 - Turtle graphics action (move, turn, pen up/down)
📝 - Input prompt
```

### 3. Safe Expression Evaluator (`core/safe_expression_evaluator.py`)

Protected mathematical expression evaluation without using `eval()`.

**Usage:**
```python
from core.safe_expression_evaluator import safe_eval

result = safe_eval("2 + 3 * X", {"X": 5})  # Returns 17

# Allowed operations:
# - Arithmetic: +, -, *, /, //, %, **
# - Comparison: ==, !=, <, >, <=, >=
# - Logical: and, or, not
# - Functions: abs, round, int, float, min, max, sum, len
```

**Security Features:**
- No access to builtins
- No attribute access
- No function definitions
- Safe operators only

---

## Language Executors

### BASIC Executor (`languages/basic.py`)

**Supported Features:**
- Variable declaration and assignment
- Arrays (DIM statement)
- Arithmetic and string operations
- Control flow (IF/THEN/ELSE, FOR, WHILE, DO/LOOP)
- Subroutines (SUB/FUNCTION)
- String functions (LEN, LEFT, RIGHT, MID, UPPER, LOWER)
- Input/Output (INPUT, PRINT)
- Comments (REM)

**Example:**
```basic
DIM A(10)
FOR I = 1 TO 10
  A(I) = I * 2
NEXT I
PRINT "Array sum:", SUM(A)
```

### PILOT Executor (`languages/pilot.py`)

**Supported Features:**
- Interactive instruction language
- COMPUTE statements (arithmetic)
- MATCH statements (conditional)
- JUMP/JUMPT (conditionals branching)
- LOOP statements
- Type (output)
- Accept (input)
- Remarks (comments)

**Example:**
```pilot
*START
T: What is 2+2?
A: 4
M: Correct!
J: *DONE
M: Try again.
J: *START
*DONE
```

### Logo Executor (`languages/logo.py`)

**Supported Features:**
- Turtle graphics commands
- Procedures with parameters
- Recursion
- Pen control (PENUP, PENDOWN, PENWIDTH, PENCOLOR)
- Movement (FORWARD, BACKWARD, RIGHT, LEFT)
- Colors (SETCOLOR or direct color names)
- Loops (REPEAT)

**Example:**
```logo
TO SQUARE :SIZE
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
END

TO TREE :SIZE :DEPTH
  IF :DEPTH = 0 [STOP]
  FORWARD :SIZE
  RIGHT 30
  TREE :SIZE * 0.8 :DEPTH - 1
  LEFT 60
  TREE :SIZE * 0.8 :DEPTH - 1
  RIGHT 30
  BACKWARD :SIZE
END
```

**Turtle State:**
```python
turtle_x = 0           # X coordinate
turtle_y = 0           # Y coordinate
turtle_angle = 0       # Heading (0 = up, 90 = right)
pen_down = True        # Is pen drawing?
pen_color = (0,0,0)    # RGB color
pen_width = 1          # Line width
```

---

## Data Flow

### Execution Pipeline

```
1. User writes code in Editor
            ↓
2. User presses Ctrl+R (Run)
            ↓
3. UI sends code to TimeWarpInterpreter.execute()
            ↓
4. Interpreter detects language (BASIC, Logo, etc.)
            ↓
5. Appropriate executor processes the code
            ↓
6. Executor returns string output with emoji prefixes
            ↓
7. UI displays output in Output Panel
            ↓
8. If turtle graphics: UI reads turtle state and renders canvas
```

### State Management

**Interpreter Holds State:**
- Variable dictionary
- Function registry
- Turtle position/angle/pen state
- Output buffer
- Error state

**UI Holds State:**
- Window geometry
- Editor content
- Canvas rendering
- Theme preference
- Recent files

**Never:**
- Store UI references in executor
- Store window/canvas refs in interpreter
- Mix UI concerns with language execution

---

## Feature Modules

### 1. Lesson System (`features/lesson_system.py`)

Provides structured learning with step-by-step guidance.

**Classes:**
- `Lesson` - A complete lesson with description, code examples, objectives
- `Checkpoint` - A verification point within a lesson with expected output
- `LessonManager` - Manages lesson lifecycle, progress tracking
- `LessonStatus` - Tracks completion state and hints used

**Usage:**
```python
from features.lesson_system import LessonManager, Lesson

manager = LessonManager()
lesson = manager.load_lesson("basic_fundamentals")
lesson.start()

# User solves checkpoint
checkpoint = lesson.current_checkpoint()
is_correct = checkpoint.verify(user_solution)
lesson.hint()  # Get a hint
lesson.next_checkpoint()  # Move to next
```

### 2. Examples Browser (`features/examples_browser.py`)

Searchable catalog of 100+ example programs.

**Classes:**
- `Example` - Single example program with metadata
- `ExamplesBrowser` - Load, search, filter examples
- `Language` and `Difficulty` enums - Organization metadata

**Usage:**
```python
from features.examples_browser import ExamplesBrowser

browser = ExamplesBrowser()
examples = browser.search("graphics", language="logo", difficulty="beginner")
for example in examples:
    print(f"{example.title}: {example.description}")
    code = browser.load_example(example.id)
```

### 3. Turtle Preview (`features/turtle_preview.py`)

Live visualization of Logo code as you type.

**Classes:**
- `TurtlePreview` - Real-time preview engine
- `TurtleState` - Turtle position, angle, pen state
- `TurtleStroke` - Individual line segment from turtle movement

**Usage:**
```python
from features.turtle_preview import TurtlePreview

preview = TurtlePreview()
preview.execute("FORWARD 100")
preview.execute("RIGHT 90")
strokes = preview.get_strokes()  # For rendering
```

### 4. Theme Editor (`features/theme_editor.py`)

Create and manage editor themes.

**Classes:**
- `Theme` - Complete theme definition
- `ThemeColors` - Color palette (background, text, syntax)
- `ThemeManager` - Load, save, apply themes

**Usage:**
```python
from features.theme_editor import ThemeManager, Theme

manager = ThemeManager()
theme = manager.get_theme("dracula")
manager.apply_theme(theme)  # Apply to UI

# Create custom theme
custom = Theme(
    name="My Theme",
    background=(40, 40, 40),
    text=(255, 255, 255),
    keywords=(86, 156, 214)
)
manager.save_custom_theme(custom)
```

### 5. Autosave Manager (`features/autosave_manager.py`)

Background file saving with version history.

**Classes:**
- `FileVersion` - Single saved version with timestamp
- `FileHistory` - Collection of versions for one file
- `AutosaveManager` - Automatic and manual save operations

**Usage:**
```python
from features.autosave_manager import AutosaveManager

autosave = AutosaveManager(interval_seconds=30)
autosave.save_file("program.bas", "PRINT 'Hello'")
autosave.enable()  # Start background autosaving

# Restore previous version
versions = autosave.get_versions("program.bas")
content = autosave.restore_version("program.bas", versions[0])
```

### 6. Classroom Mode (`features/classroom_mode.py`)

Teaching tools for presentations and assignments.

**Classes:**
- `PresentationMode` - Full-screen code display
- `WorkspaceBundle` - Package code and examples for distribution
- `ClassroomMode` - Manage presentations and assignments

**Usage:**
```python
from features.classroom_mode import ClassroomMode, WorkspaceBundle

classroom = ClassroomMode()
classroom.start_presentation("advanced_logo.py")

bundle = WorkspaceBundle()
bundle.add_file("solution.bas")
bundle.add_example("guessing_game.bas")
bundle.export_zip("assignment.zip")
```

---

## UI Architecture

### PySide6 (Qt6) UI Framework

**Main Components:**

```
MainWindow (time_warp_ide.py)
├── MenuBar
│   ├── File (New, Open, Save, Save As, Lessons, Examples, Settings)
│   ├── Edit (Undo, Redo, Cut, Copy, Paste, Find, Replace)
│   ├── View (Zoom, Theme, Panels)
│   └── Help (Documentation, About, Report Issue)
├── ToolBar
│   ├── Language Selector dropdown
│   ├── Run/Stop buttons
│   ├── Clear/Reset buttons
│   └── Settings button
├── Central Widget
│   ├── EditorPanel (Code editor with syntax highlighting)
│   ├── CanvasPanel (Turtle graphics canvas with zoom/pan)
│   ├── OutputPanel (Program output and error messages)
│   ├── InputPanel (For INPUT statements)
│   └── StatusBar (Current line/col, mode, status)
└── Dialogs
    ├── Settings/Preferences
    ├── Theme Editor
    ├── Find/Replace
    └── About
```

### Editor Component (`ui/editor.py`)

- Syntax highlighting for all supported languages
- Line numbers
- Code folding
- Bracket matching
- Search/replace
- Themes support

### Canvas Component (`ui/canvas.py`)

- Turtle graphics rendering with QPainter
- Zoom and pan controls
- Stroke rendering
- Color support
- Grid overlay (optional)

### Theme System (`tools/theme.py`)

8 built-in themes:
1. **Dracula** - Popular dark theme
2. **Monokai** - Classic code editor colors
3. **Solarized Dark** - Low contrast dark
4. **Ocean** - Blue palette
5. **Spring** - Green pastels
6. **Sunset** - Warm orange/red
7. **Candy** - Bright pastels
8. **Forest** - Deep greens/browns

Each theme defines:
- Background colors
- Text colors
- Keyword colors
- Comment colors
- Error colors
- Canvas background

---

## Extending the IDE

### Adding a New Language

**Step 1: Create Executor**
```python
# time_warp/languages/mylang.py
from . import LanguageExecutor

class MyLangExecutor(LanguageExecutor):
    def __init__(self, interpreter):
        self.interpreter = interpreter
    
    def execute_command(self, command: str) -> str:
        try:
            # Parse and execute
            result = self._execute(command)
            return f"✅ {result}\n"
        except Exception as e:
            return f"❌ Error: {str(e)}\n"
    
    def _execute(self, command: str):
        # Your implementation here
        pass
```

**Step 2: Register in Interpreter**
```python
# time_warp/core/interpreter.py
from .languages.mylang import MyLangExecutor

class TimeWarpInterpreter:
    def __init__(self):
        # ... other initializations
        self.mylang = MyLangExecutor(self)
    
    def execute(self, code: str) -> str:
        if code.startswith("mylang:"):
            return self.mylang.execute_command(code)
```

**Step 3: Add Language Detection**
```python
def get_language(self, code: str) -> str:
    if "mylang:" in code.lower():
        return "mylang"
    # ... other detections
```

**Step 4: Create Examples**
```
Examples/
└── mylang/
    ├── 01_hello_world.ml
    ├── 02_variables.ml
    └── ...
```

**Step 5: Write Tests**
```python
# tests/test_mylang.py
import pytest
from time_warp.languages.mylang import MyLangExecutor

def test_hello():
    executor = MyLangExecutor(mock_interpreter)
    result = executor.execute_command("print 'hello'")
    assert "✅" in result
```

### Adding a Feature Module

**Create Feature File:**
```python
# time_warp/features/my_feature.py
class MyFeature:
    def __init__(self):
        self.config = {}
    
    def initialize(self, interpreter, ui):
        """Called when feature is loaded"""
        self.interpreter = interpreter
        self.ui = ui
    
    def execute(self, command):
        """Handle feature-specific commands"""
        pass
```

**Register in UI:**
```python
# time_warp_ide.py
from features.my_feature import MyFeature

class MainWindow:
    def __init__(self):
        # ... UI setup
        self.my_feature = MyFeature()
        self.my_feature.initialize(self.interpreter, self)
```

### Adding a Theme

**Create Theme File:**
```python
# In tools/theme.py or custom location
from dataclasses import dataclass

@dataclass
class MyTheme:
    name: str = "My Theme"
    background: tuple = (30, 30, 30)
    text: tuple = (255, 255, 255)
    keywords: tuple = (0, 150, 255)
    # ... more colors
```

**Register in ThemeManager:**
```python
manager = ThemeManager()
manager.register_theme(MyTheme())
manager.apply_theme("My Theme")
```

---

## Development Guidelines

### Code Style

- **Language:** Python 3.10+
- **Formatter:** black (line length 100)
- **Linter:** flake8
- **Type Checker:** mypy
- **Import Sorter:** isort

**Configuration:**
```toml
# pyproject.toml
[tool.black]
line-length = 100

[tool.mypy]
strict = true
```

### Testing

- **Framework:** pytest
- **Coverage:** pytest-cov
- **Mock:** pytest-mock
- **Fixtures:** conftest.py

**Test Structure:**
```
tests/
├── test_*.py          # Unit tests
├── *_test.py          # Integration tests
└── conftest.py        # Shared fixtures
```

**Example Test:**
```python
def test_basic_print(interpreter):
    result = interpreter.execute('PRINT "Hello"')
    assert "Hello" in result
    assert "✅" in result
```

### Commit Guidelines

- **Branch naming:** feature/name, bugfix/name, docs/name
- **Commit messages:** "Add feature", "Fix issue", "Docs: update guides"
- **PR description:** What, why, how, testing steps

### Pull Request Process

1. Fork repository
2. Create feature branch
3. Make changes with tests
4. Run: `pytest tests/ -v --cov`
5. Run: `flake8`
6. Commit and push
7. Create PR with description

---

## Performance Considerations

- **Interpreter:** Single-threaded, ~ms per command
- **Canvas:** Direct QPainter drawing, ~60 FPS
- **Autosave:** Background thread, configurable interval
- **Memory:** ~100MB base, scales with file count

**Optimization Tips:**
- Use async execution for long-running code
- Batch turtle drawing commands
- Limit undo/redo history
- Clear canvas between runs

---

## Troubleshooting for Developers

### "Illegal instruction" error
- **Cause:** Missing CPU features (SSSE3, SSE4.1, SSE4.2, POPCNT)
- **Solution:** Run on modern hardware or VM with full CPU features

### Tests failing
- **Check:** Python version (3.10+)
- **Check:** Dependencies installed (`pip install -r requirements-dev.txt`)
- **Check:** Working directory (should be `Platforms/Python/`)

### UI not rendering
- **Check:** PySide6 version (`pip show PySide6`)
- **Check:** Qt platform plugin availability
- **Try:** Set environment variable `QT_QPA_PLATFORM_PLUGIN_PATH`

### Language not detected
- **Check:** Detection logic in `get_language()`
- **Check:** Command format matches expected pattern
- **Check:** Executor registered in `__init__()`

---

**Last Updated:** January 2026  
**Version:** 6.0.0  
**Maintainer:** James Temple <james@honey-badger.org>
