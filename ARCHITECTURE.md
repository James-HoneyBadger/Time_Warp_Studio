# Architecture Guide

**Time Warp Studio v7.0.0 — System Design and Implementation**

---

## Table of Contents

1. [Overview](#overview)
2. [Core Interpreter Architecture](#core-interpreter-architecture)
3. [Language Executors](#language-executors)
4. [UI Architecture](#ui-architecture)
5. [Graphics System](#graphics-system)
6. [Debugger System](#debugger-system)
7. [Data Flow](#data-flow)
8. [Extension Points](#extension-points)
9. [Threading Model](#threading-model)
10. [Configuration & Persistence](#configuration--persistence)

---

## Overview

Time Warp Studio is a desktop IDE built with:
- **Backend**: Python 3.10+ with PySide6 (Qt6)
- **Architecture**: Single-process, multi-threaded desktop application
- **Core Pattern**: Central interpreter with stateless language executors
- **UI Framework**: PySide6/Qt6 for cross-platform GUI

### Design Philosophy

**State Management**:
- UI owns all state (editor content, canvas, variables)
- Language executors are stateless command processors
- Results returned as text strings with emoji prefixes

**Safety**:
- No use of `eval()` for expression evaluation
- Protected math expression evaluator
- Thread-safe queues for execution communication

**Modularity**:
- 7 independent language modules (easily extensible)
- Feature panels loaded dynamically
- Theme system completely separate from core logic

---

## Core Interpreter Architecture

### TimeWarpInterpreter (Main Class)

Location: `Platforms/Python/time_warp/core/interpreter.py`

Core dispatcher managing all 7 language executors:

```python
class TimeWarpInterpreter:
    def __init__(self):
        # Initialize all 7 language executors
        self.basic = BasicExecutor(self)
        self.logo = LogoExecutor(self)
        self.pilot = PilotExecutor(self)
        self.c_lang = CExecutor(self)
        self.pascal = PascalExecutor(self)
        self.prolog = PrologExecutor(self)
        self.forth = ForthExecutor(self)
```

### Core Methods

**execute(code: str, language: Language) -> ExecutionResult**
- Main entry point for code execution
- Routes to appropriate executor based on language
- Captures output and errors
- Returns structured result with emoji prefix

Output Prefixes:
- `❌` Execution error
- `✅` Success
- `ℹ️` Information message
- `🐢` Turtle graphics command
- `🎨` Theme change notification

### Execution Flow

```
User Code
    ↓
UI calls: interpreter.execute(code, language)
    ↓
Route to language executor
    ↓
Executor parses and executes
    ↓
Returns: "✅ Output\n" or "❌ Error\n"
    ↓
UI updates console and canvas
```

---

## Language Executors

### Architecture Pattern

Each language follows stateless pattern:

```python
class BasicExecutor(LanguageExecutor):
    def __init__(self, interpreter):
        self.interpreter = interpreter
    
    def execute_command(self, command: str) -> str:
        # Parse and execute
        return output_with_emoji_prefix
```

### Key Design Rule

**Language executors MUST NOT:**
- ❌ Store UI widget references
- ❌ Update canvas directly
- ❌ Modify main window

**Language executors CAN:**
- ✅ Track internal state (variables, turtle position)
- ✅ Return strings as output
- ✅ Reference the interpreter

### The 7 Supported Languages

#### 1. BASIC

Features:
- Variables and arrays (DIM statement)
- Control flow (IF/ELSE, FOR, WHILE, DO...LOOP)
- Subroutines (GOSUB/RETURN)
- Turbo BASIC graphics
  - SCREEN, COLOR, LINE, CIRCLE, PSET, PAINT

#### 2. LOGO

Features:  
- Turtle graphics (FORWARD, BACKWARD, RIGHT, LEFT)
- Pen control (PENUP, PENDOWN)
- Shapes (CIRCLE, RECTANGLE, POLYGON)
- Procedures (TO/END definitions)
- Recursion support

Turtle State Tracking:
- Position: `x`, `y` (-400 to 400 logical coords)
- Heading: `angle` (0-360 degrees)
- Pen: `pen_down`, `pen_color`, `pen_width`

#### 3. PILOT

Features:
- T-units: Text output
- A-units: Answer input validation
- J-units: Conditional jumps
- Used for CAI (Computer-Aided Instruction) lessons

#### 4. C

Features:
- Basic C syntax (variables, functions)
- stdio (printf, scanf)
- Standard math library
- Limited to safe subset

Status: Experimental

#### 5. Pascal

Features:
- Structured programming
- Type declarations, procedures, functions
- Control structures (IF, FOR, WHILE, REPEAT)

Status: Experimental

#### 6. Prolog

Features:
- Facts and rules
- Unification and backtracking
- List operations
- Query processing

Status: Experimental

#### 7. Forth

Features:
- Stack operations (DUP, SWAP, DROP)
- Word definitions (: WORD ... ;)
- Control structures (IF...THEN, DO...LOOP)
- Integer arithmetic

Status: Experimental

---

## UI Architecture

### Main Components

```
MainWindow
├── MenuBar (File, Edit, Run, Debug, View, Help)
├── Central Widget
│   ├── CodeEditor
│   ├── TabbedPanels
│   │   ├── OutputPanel
│   │   ├── GraphicsCanvas
│   │   ├── VariablesInspector
│   │   └── ...
│   └── 14 Feature Panels
├── Toolbar
└── StatusBar
```

### Editor & Canvas

**CodeEditor** (`ui/editor.py`):
- Syntax highlighting per language
- Real-time validation
- Line numbers, code folding
- Find/Replace functionality

**Canvas** (`ui/canvas.py`):
- Qt-based graphics rendering
- Zoom (0.1x to 5.0x) and pan support
- Renders turtle graphics in real-time
- 800x600 logical coordinates

### 14 Feature Panels

Specialized development tools accessible via tabs:
1. Lesson Mode - Step-by-step guided instruction
2. AI Assistant - Code suggestions
3. Error Explainer - Understand errors
4. Reference Search - Quick documentation
5. Examples Browser - Browse code examples
6. Turtle Inspector - Visualize turtle state
7. Debugger - Step through execution
8. Variables Inspector - View current variables
9. Achievements - Progress gamification
10. Project Runner - Multi-file management
11. Classroom Mode - Teaching features
12. Performance Monitor - Execution profiling
13. Settings - IDE configuration
14. Help - Integrated documentation

### Menu System (53+ Items)

**File**: New, Open, Save, Export, Recent, Examples, Lessons
**Edit**: Undo, Redo, Cut, Copy, Paste, Find, Replace
**Run**: Execute, Continue, Stop, Clear output
**Debug**: Step Into, Step Over, Breakpoints, Timeline
**View**: Show/Hide panels, Themes, Zoom
**Help**: Documentation, Shortcuts, About, Settings

---

## Graphics System

### TurtleShape Primitives

Core drawing system with 50+ commands:

```python
class TurtleState:
    x: float              # Position X
    y: float              # Position Y
    angle: float          # Heading 0-360
    pen_down: bool        # Drawing enabled
    pen_color: tuple      # RGB color
    pen_width: int        # Pixel width
```

### Canvas Coordinate System

```
     -400    0    400
  300 ┌─────┬──────┐
      │     │      │
    0 ├─────•(0,0) ┤
      │     │      │
-300 └─────┴──────┘
```

Canvas: 800x600 logical coordinates
Turtle: Center at (0, 0)
Angle: 0°=East, 90°=North, 180°=West, 270°=South

### Drawing Operations

Movement:
- FORWARD, BACKWARD - Move and optionally draw
- RIGHT, LEFT - Rotate turtle
- SETPOSITION - Absolute move

Drawing:
- PENUP, PENDOWN - Control drawing
- LINE - Draw line
- CIRCLE - Draw circle
- POINT - Draw single pixel

Style:
- SETCOLOR - Change pen color
- SETWIDTH - Change pen width
- CLEAR - Erase canvas
- HOME - Reset to origin

---

## Debugger System

### Features

- **Breakpoints**: Stop at specific lines
- **Stepping**: Step into/over statements
- **Timeline**: Full execution history
- **Variables**: Inspect values at each step
- **Rewind**: Navigate backwards through execution

### Timeline Recording

Each execution step stored with:
- Step number
- Line number
- Command executed
- Variable snapshot
- Turtle state snapshot
- Output generated
- Any errors

### Debug Commands

- Step Into - Execute next, enter functions
- Step Over - Execute next, skip functions  
- Continue - Run until next breakpoint
- Pause - Pause execution
- Stop - Halt program
- Toggle Breakpoint - Add/remove breakpoint
- View Timeline - Browse execution history

---

## Data Flow

### Execution Pipeline

```
User Code in Editor
        ↓
[User clicks Run]
        ↓
MainWindow.on_run_clicked()
        ↓
Create ExecutionThread
        ↓
ExecutionThread.run()
        ↓
interpreter.execute(code, language)
        ↓
Language Executor processes code
        ↓
Emit signals with results
        ↓
Main thread receives signals
        ↓
Update UI (console, canvas, variables)
```

### Signal/Slot Communication

Qt signal/slot pattern for thread-safe communication:

```python
# Signal definitions
state_changed = pyqtSignal(dict)  # Full state update
output_received = pyqtSignal(str)
error_received = pyqtSignal(str)

# Slots (received in main thread)
@pyqtSlot(dict)
def on_state_changed(self, state):
    # Update all UI components
    pass
```

---

## Extension Points

### Adding a New Language

1. Create `languages/newlang.py`:

```python
from . import LanguageExecutor

class NewLangExecutor(LanguageExecutor):
    def __init__(self, interpreter):
        self.interpreter = interpreter
    
    def execute_command(self, command: str) -> str:
        try:
            # Parse and execute
            return "✅ Result\n"
        except Exception as e:
            return f"❌ {e}\n"
```

2. Register in `core/interpreter.py`:

```python
from .languages.newlang import NewLangExecutor
self.newlang = NewLangExecutor(self)
```

3. Add detection in `execute()` method

4. Create examples in `Examples/newlang/`

5. Write tests in `tests/test_newlang.py`

### Adding a Feature Panel

1. Create in `ui/feature_panels/new_panel.py`

2. Register in `ui/main_window.py`:

```python
self.new_panel = NewPanel()
self.tabs.addTab(self.new_panel, "New Panel")
```

3. Add menu toggle in View menu:

```python
self.action_show_new = self.menu_view.addAction("New Panel")
self.action_show_new.setCheckable(True)
self.action_show_new.triggered.connect(self.new_panel.show)
```

### Adding a Theme

1. Define in `tools/theme.py`:

```python
THEME_CUSTOM = {
    "name": "Custom",
    "colors": {
        "background": "#ffffff",
        "foreground": "#000000",
        # ... colors for all UI elements
    }
}
```

2. Add to THEMES list - automatically available in menu

---

## Threading Model

### Single-Threaded UI, Multi-Threaded Execution

```
┌──────────────────────┐
│ Qt Main Event Loop   │
│ - User input         │
│ - UI rendering       │
│ - Signal processing  │
└──────────────────────┘
        ↕ (signals/slots)
┌──────────────────────┐
│ Execution Thread     │
│ - Run interpreter    │
│ - Emit results       │
└──────────────────────┘
```

All UI updates happen in main thread via slots.
All code execution happens in worker thread.

---

## Configuration & Persistence

### Config File

Location: `~/.Time_Warp/config.json`

Contains:
- Theme preference
- Editor settings (font, size)
- Canvas settings
- Recent files
- Feature panel visibility
- Color schemes
- Keyboard shortcuts
- Learning progress

### Automatic Saving

- Config auto-saved on changes
- Windows geometry saved on close
- Recent files list maintained
- Theme choice persisted

### Theme System

8 built-in themes:
- Dracula - Purple dark theme
- Monokai - Classic editor
- Solarized Dark - Low contrast
- Ocean - Blue palette
- Spring - Green/pastels
- Sunset - Warm oranges
- Candy - Bright pastels
- Forest - Deep greens

Custom themes can be created and saved.

---

## Module Overview

### Core (`core/`)
- `interpreter.py` - Main dispatcher (1100+ lines)
- `game_support.py` - Game state and timers
- `accessibility.py` - Accessibility features
- `ai_assistant.py` - AI code suggestions
- `chat_service.py` - Chat integration
- Support modules (analytics, collaboration, etc.)

### Languages (`languages/`)
- `basic.py`, `logo.py`, `pilot.py`, `c_lang_fixed.py`, `pascal.py`, `prolog.py`, `forth.py`

### UI (`ui/`)
- `main_window.py` - Main IDE window (3300+ lines)
- `editor.py` - Code editor
- `canvas.py` - Graphics canvas
- `output.py` - Console output
- `feature_panels.py` - 14 specialized panels
- `themes.py` - Theme application

### Tools (`tools/`)
- `theme.py` - Theme definitions (8 themes)
- `startup.py` - Startup sequence manager

---

## Performance Characteristics

### Startup
- Cold start: 2-5 seconds
- Theme loading: 0.5 seconds
- Interpreter init: 1 second
- UI rendering: 0.5 seconds

### Execution
- BASIC: ~1000 statements/second
- Logo: 100+ turtle commands/second
- Debugger overhead: ~10% slower

### Memory
- Base: 200-300 MB
- Per file: +5-10 MB
- Large graphics: +50-100 MB per 1000 commands

---

## Contributing

### Code Style

- Python: PEP 8, 100 char line limit
- Formatter: black
- Linter: flake8, pylint
- Types: mypy strict mode
- Docstrings: All public functions

### Testing

```bash
pytest tests/ -v                           # Run tests
python test_runner.py --comprehensive     # Full suite
black --check .                            # Check formatting
flake8 time_warp                          # Linting
mypy time_warp --strict                   # Type check
```

### PR Process

1. Fork repository
2. Create feature branch
3. Make changes following style guide
4. Write/update tests
5. Run test suite (all pass)
6. Submit PR with description

---

**For more details:**
- [README.md](README.md) - Project overview
- [CONTRIBUTING.md](CONTRIBUTING.md) - Contributing
- Source: `Platforms/Python/time_warp/`
