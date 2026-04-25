# Architecture Guide

## Time Warp Studio v10.0.0 — System Design and Implementation

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

- 20 independent language modules (easily extensible)
- Feature panels loaded dynamically
- Theme system completely separate from core logic

---

## Core Interpreter Architecture

### TimeWarpInterpreter (Main Class)

Location: `Platforms/Python/time_warp/core/interpreter.py`

Core dispatcher managing all 20 language executors:

```python
class Interpreter:
    def __init__(self):
        # Language executors are functions, not class instances.
        # Line-by-line executors (7 languages):
        #   execute_basic, execute_pilot, execute_logo,
        #   execute_c, execute_pascal, execute_prolog, execute_forth
        #
        # Whole-program executors (13 languages) registered in:
        #   _WHOLE_PROGRAM_EXECUTORS dict
        #   (Python, Lua, Scheme, Brainfuck, JavaScript,
        #    REXX, Smalltalk, HyperTalk, Haskell,
        #    Ruby, Erlang, Rust, Perl)
        pass
```

### Core Methods

#### execute(code: str, language: Language) -> ExecutionResult

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

```text
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

Each language follows a stateless **function** pattern:

```python
def execute_basic(interpreter: Interpreter, command: str, turtle: TurtleState) -> str:
    """Execute a single BASIC command; return output text."""
    # Parse and execute
    return output_with_emoji_prefix
```

### Key Design Rule

**Language executors MUST NOT:**

- ❌ Store UI widget references
- ❌ Update canvas directly
- ❌ Modify main window

**Language executors CAN:**

- ✅ Read/write interpreter state (variables, turtle position)
- ✅ Return strings as output
- ✅ Access the interpreter and turtle state objects

### The 20 Supported Languages

**Line-by-line executors** (parsed per statement):

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

**Whole-program executors** (13 languages):

Python, Lua, Scheme, Brainfuck, JavaScript,
REXX, Smalltalk, HyperTalk, Haskell, Ruby, Erlang, Rust, Perl.

Each receives the full source text and returns output. Registered in
`_WHOLE_PROGRAM_EXECUTORS` in `core/interpreter.py`.

---

## UI Architecture

### Main Components

```text
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

```text
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

```text
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
# Signal definitions (PySide6)
from PySide6.QtCore import Signal, Slot

state_changed = Signal(dict)  # Full state update
output_received = Signal(str)
error_received = Signal(str)

# Slots (received in main thread)
@Slot(dict)
def on_state_changed(self, state):
    # Update all UI components
    pass
```

---

## Extension Points

### Adding a New Language

1. Create `languages/newlang.py`:

```python
from __future__ import annotations
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

def execute_newlang(interpreter: Interpreter, source: str, turtle: TurtleState) -> str:
    """Execute NewLang source code."""
    output_lines = []
    try:
        # Parse and execute source
        output_lines.append("✅ Result")
    except Exception as e:
        output_lines.append(f"❌ {e}")
    return "\n".join(output_lines) + "\n"
```

1. Register in `core/interpreter.py`:

```python
from ..languages.newlang import execute_newlang
# Add Language.NEWLANG enum member
# Add entry in _init_whole_program_executors()
```

1. Add detection in `execute()` method

1. Create examples in `Examples/newlang/`

1. Write tests in `tests/test_newlang.py`

### Adding a Feature Panel

1. Create in `ui/feature_panels/new_panel.py`

2. Register in `ui/main_window.py`:

```python
self.new_panel = NewPanel()
self.tabs.addTab(self.new_panel, "New Panel")
```

1. Add menu toggle in View menu:

```python
self.action_show_new = self.menu_view.addAction("New Panel")
self.action_show_new.setCheckable(True)
self.action_show_new.triggered.connect(self.new_panel.show)
```

### Adding a Theme

1. Define in `ui/themes.py` by adding a new `Theme(...)` entry to the themes dict:

```python
"My Custom": Theme(
    bg="#1e1e2e",
    fg="#cdd6f4",
    accent="#89b4fa",
    # ... colors for all UI elements
),
```

1. The theme is automatically available in the theme menu

---

## Threading Model

Time Warp Studio employs a multi-threaded architecture to ensure responsive UI interactions while executing potentially long-running code. Below is an overview of the threading model:

### Key Threads

1. **Main Thread (UI)**:

   - Handles all user interactions, including editor updates, canvas rendering, and menu actions.
   - Ensures that the application remains responsive during execution.

2. **Interpreter Thread**:

   - Executes user code in a separate thread to prevent blocking the UI.
   - Communicates with the main thread via thread-safe queues.

3. **Worker Threads**:

   - Used for auxiliary tasks such as file I/O, syntax highlighting, and background processing.

### Communication

- **Thread-Safe Queues**:
  - The `queue.Queue` module is used for passing messages between threads.
  - Ensures safe and efficient communication without race conditions.

- **Signals and Slots**:
  - PySide6's signal-slot mechanism is used for notifying the UI of execution results or errors.

### Safeguards

- **Deadlock Prevention**:
  - All long-running tasks are offloaded to worker threads.
  - The main thread never waits indefinitely for a response.

- **Error Handling**:
  - Exceptions in worker threads are captured and reported back to the main thread.

---

## Configuration & Persistence

### Config File

Location: `~/.time_warp/config.json`

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

28 built-in themes organized by category:

**Dark (11):** Dracula, Monokai, VS Code Dark, GitHub Dark, Nord, One Dark Pro, Solarized Dark, Ocean, Catppuccin Mocha, Gruvbox Dark, Tokyo Night

**Light (6):** Gruvbox Light, Catppuccin Latte, VS Code Light, GitHub Light, Solarized Light, Spring

**Specialty (2):** High Contrast Dark, High Contrast Light

**Accessibility (2):** Dyslexia Friendly, Accessible Dark Blue

**Retro CRT (7):** Amber Monochrome, Green Monochrome, IBM PC CGA, Commodore 64, Apple II, DOS Blue, ZX Spectrum

---

## Module Overview

### Core (`core/`)

- `interpreter.py` — Main dispatcher (~1,500 lines, 20 language executors)
- `debugger.py` — Step-through debugger with execution timeline and rewind
- `sql_engine.py` — SQLite-backed T-SQL compatibility layer
- `orchestrator.py` — System integration and component registry
- `config.py` — Canonical paths (`~/.time_warp/`) and settings

### Features (`features/`)

- `hardware_simulator.py` — Simulation-first IoT hardware support
- `plugin_system.py` — Plugin architecture (future extensibility)
- `game_support.py` — Game state, timers, and input handling
- `accessibility.py` — Accessibility features and screen reader support
- `ai_assistant.py` — AI-powered code suggestions
- `ai_suggestions.py` — Contextual suggestion engine
- `cloud_storage.py` — Cloud file storage (optional)
- `hardware_integration.py` — Physical hardware bridging (optional)
- Lessons, autosave, achievements, classroom mode, examples browser, etc.

### Languages (`languages/`)

- `base.py` — Executor protocol definition
- **Line-by-line executors (7):** `basic.py`, `pilot.py`, `logo.py`, `c_lang_fixed.py`, `pascal.py`, `prolog.py`, `forth.py`
- **Whole-program executors (17):** `python.py`, `lua.py`, `scheme.py`, `cobol.py`, `brainfuck.py`, `assembly.py`, `javascript.py`, `fortran.py`, `rexx.py`, `smalltalk.py`, `hypertalk.py`, `haskell.py`, `apl.py`, `sql.py`, `jcl.py`, `cics.py`, `sqr.py`
- `lang_utils.py`, `parser_patterns.py` — Shared parsing utilities

### UI (`ui/`)

- `main_window.py` — Main IDE window (6 mixins, 3,300+ lines)
- `editor.py` — Code editor with syntax highlighting and minimap
- `canvas.py` — Turtle graphics canvas with zoom/pan
- `output.py` — Console output with interpreter threads
- `themes.py` — 28-theme manager (dark, light, retro CRT, accessibility)
- `debug_panel.py` — Debugger controls, watch, and call-stack
- `command_palette.py` — Ctrl+Shift+P command palette
- `feature_panels.py` — 14 dynamic feature panels
- `mixins/` — Collaboration, classroom, debug, export, file ops, help

### Graphics (`graphics/`)

- `turtle_state.py` — Turtle position, heading, pen state (~600 lines)
- `art_toolkit.py` — Drawing primitives and shapes
- `pixel_canvas.py` — Pixel-level canvas operations
- `turtle_gallery.py` — Pre-built turtle art examples

### Features (`features/`)

- `lesson_system.py` — Step-by-step lessons with checkpoints
- `examples_browser.py` — Searchable 97-program catalog
- `turtle_preview.py` — Live Logo code preview
- `theme_editor.py` — Custom theme creation
- `autosave_manager.py` — Background autosave with versioning
- `achievements.py` — Gamified progress tracking
- `classroom_mode.py` — Teaching and presentation tools
- `reference_search.py` — Quick documentation lookup

### Utils (`utils/`)

- `expression_evaluator.py` — Safe math eval (hand-written parser, no `eval()`)
- `string_evaluator.py` — String expression evaluation
- `error_hints.py` — Syntax error suggestions
- `validators.py` — Input validation helpers
- `code_formatter.py` — Code formatting utilities
- `logging_config.py` — Structured logging setup

### Tests (`tests/`)

- 41+ test files covering all 24 language executors, graphics, GUI, and interpreter
- `conftest.py`, `conftest_lang.py` — Shared fixtures and helpers
- `test_runner.py` — Orchestration with HTML coverage reports

### Scripts (`Scripts/`)

Build, launch, deploy, and utility scripts — including `deploy.sh`, `startup.py`,
`build_native.sh`, `generate_icon.py`, and IDE launchers.

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
python Platforms/Python/test_runner.py --comprehensive     # Full suite with coverage reports
python Platforms/Python/test_runner.py --basic             # Quick smoke tests
python Platforms/Python/test_runner.py --parallel          # Parallel execution (pytest-xdist)
black --check .                            # Check formatting
ruff check time_warp                       # Fast linting
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

- [README.md](README.md) — Project overview and quick start
- [CONTRIBUTING.md](CONTRIBUTING.md) — Contribution guidelines
- [CHANGELOG.md](CHANGELOG.md) — Version history
- [ROADMAP.md](ROADMAP.md) — Development roadmap
- Source: `Platforms/Python/time_warp/`
