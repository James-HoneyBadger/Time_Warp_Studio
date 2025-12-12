# Developer Guide

Welcome to Time Warp IDE development! This guide will help you contribute to the project.

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Architecture](#architecture)
3. [Development Setup](#development-setup)
4. [Code Organization](#code-organization)
5. [Contributing](#contributing)
6. [Testing](#testing)
7. [Release Process](#release-process)

---

## Project Overview

### What is Time Warp IDE?

Time Warp IDE is an educational programming environment with:
- **Three core languages**: BASIC, PILOT, and Logo (full feature parity)
- **Experimental languages**: Pascal, Prolog, and C (incomplete implementations)
- **Advanced UI**: Multiple screen modes, debug tools, variable inspector, accessibility features
- **Rich graphics**: Real-time turtle canvas with zoom/pan, colors, sprites
- **Extended features**: Music, particles, fractals, gamepad support, collaborative editing, speech synthesis

### Design Philosophy

1. **Educational First** – Every design choice prioritizes learning
2. **Stateless Executors** – Language processors return text; UI manages all state
3. **Clear Feedback** – Error messages explain what went wrong, not just "Error"
4. **Visual Learning** – Built-in graphics make abstract concepts tangible
5. **Safe Code** – Never use `eval()`; use `safe_eval()` for expressions
6. **Accessibility** – Support keyboards, high contrast, and screen readers

### Technology Stack

**Python Implementation** (official):
- **GUI Framework**: PySide6 (Qt6 bindings)
- **Graphics Engine**: QPainter with custom canvas state management
- **Testing**: pytest with coverage
- **Package Management**: pip with requirements.txt

---

## Architecture

### Current Implementation (Python)

Time Warp IDE is a **Python (PySide6) desktop application**. All other platform implementations (Rust, Go, Amiga, Haiku, Apple, OS/2, DOS) have been removed. Historical directories (`Browser/`, `Windows2000/`) are retained for reference only.

**High-Level Structure**:
```
Time_Warp_Studio/
├── Platforms/
│   └── Python/                         # OFFICIAL IMPLEMENTATION
│       ├── time_warp_ide.py           # Entry point
│       └── time_warp/
│           ├── core/                  # Language interpreter & support
│           │   ├── interpreter.py     # Main command dispatcher
│           │   ├── game_support.py
│           │   ├── gamepad.py
│           │   ├── music.py
│           │   ├── particles.py
│           │   ├── shapes.py
│           │   ├── speech.py
│           │   └── fractals.py
│           ├── languages/             # Language executors
│           │   ├── basic.py           # BASIC interpreter ✅
│           │   ├── pilot.py           # PILOT interpreter ✅
│           │   ├── logo.py            # Logo interpreter ✅
│           │   ├── pascal.py          # Pascal (experimental)
│           │   ├── prolog.py          # Prolog (experimental)
│           │   └── c_lang_fixed.py    # C (experimental)
│           ├── ui/                    # Qt6 UI components
│           │   ├── main_window.py     # Main application window & styling
│           │   ├── editor.py          # Code editor widget
│           │   ├── canvas.py          # Graphics canvas
│           │   ├── output.py          # Output panel
│           │   ├── debug_panel.py     # Debugger UI
│           │   ├── variable_inspector.py
│           │   ├── error_explorer.py
│           │   ├── screen_modes.py
│           │   ├── themes.py
│           │   ├── focus_mode.py
│           │   ├── crt_effect.py
│           │   ├── cassette_animation.py
│           │   ├── accessibility.py
│           │   ├── collaboration_client.py
│           │   ├── snippets.py
│           │   ├── snippet_dialog.py
│           │   └── onboarding.py
│           ├── graphics/              # Turtle state management
│           │   └── turtle_state.py
│           ├── cloud/                 # Cloud features
│           ├── compiler/              # Compilation support
│           └── playground.py          # Playground environment
├── Docs/                              # Documentation library
├── Examples/                          # Sample programs
├── Tests/                             # Pytest suite
├── Core_Spec/                         # Language specifications
└── .github/                           # GitHub Actions workflows
```

### Design Patterns

#### 1. Stateless Executors
Language interpreters **do not** maintain UI state. They:
- Accept a command/line of code
- Return emoji-prefixed output string
- Do NOT call UI methods, draw graphics, or modify external state

```python
class LanguageExecutor:
    def execute_command(self, command: str) -> str:
        """
        Execute a single line/command.
        Returns emoji-prefixed string:
        - ❌ for errors
        - ✅ for success
        - ℹ️ for info
        - 🐢 for turtle actions
        - 🚀 for execution events
        """
        pass
```

#### 2. Command Dispatch
The interpreter (in `core/interpreter.py`) routes commands to the appropriate executor:
```python
def execute(self, code: str, language: Language) -> str:
    if language == Language.BASIC:
        return self.basic_executor.execute_command(code)
    elif language == Language.PILOT:
        return self.pilot_executor.execute_command(code)
    # etc.
```

#### 3. Safe Expression Evaluation
**Never use Python's `eval()`**. Always use safe evaluation:
```python
from time_warp.core.safe_expression_evaluator import safe_eval

# Safe: Limited grammar, audited operations
result = safe_eval("2 + 3 * X", {"X": 5})  # Returns 17

# FORBIDDEN
result = eval("2 + 3 * X")  # Security risk!
```

#### 4. Turtle Graphics via UI
- Executors update turtle state and emit `🐢` messages
- **UI reads executor state** to render (doesn't receive drawing commands)
- Canvas state lives in `TurtleState` and the UI components

---

## Development Setup

### Prerequisites

```bash
# Python 3.10+ required
python3 --version

# Verify you're on a system with SSE4 support
# (Some VMs lack this and will get "Illegal instruction" errors)
```

### Clone Repository

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp_Studio
```

### Python Development Environment

```bash
cd Platforms/Python

# Create virtual environment
python3 -m venv .venv
source .venv/bin/activate  # On Windows: .venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt

# Verify installation
python time_warp_ide.py  # Should launch the IDE
```

### IDE Configuration (VS Code Recommended)

```json
// .vscode/settings.json
{
    "python.defaultInterpreterPath": "${workspaceFolder}/Platforms/Python/.venv/bin/python",
    "python.linting.enabled": true,
    "python.linting.pylintEnabled": true,
    "python.testing.pytestEnabled": true,
    "python.testing.pytestArgs": [
        "Tests"
    ]
}
```

---

## Code Organization

### Python Directory Structure

```
Platforms/Python/
├── time_warp_ide.py                 # Entry point (PySide6 application)
├── time_warp/
│   ├── __init__.py
│   ├── core/
│   │   ├── interpreter.py           # Main dispatch logic (1500+ lines)
│   │   ├── game_support.py          # Game development helpers
│   │   ├── gamepad.py               # Gamepad input handling
│   │   ├── music.py                 # Music playback
│   │   ├── particles.py             # Particle system
│   │   ├── shapes.py                # Geometric shapes
│   │   ├── speech.py                # Text-to-speech
│   │   └── fractals.py              # Fractal generation
│   ├── languages/
│   │   ├── basic.py                 # BASIC executor (~700 lines)
│   │   ├── pilot.py                 # PILOT executor (~400 lines)
│   │   ├── logo.py                  # Logo executor (~800 lines)
│   │   ├── pascal.py                # Pascal executor (experimental)
│   │   ├── prolog.py                # Prolog executor (experimental)
│   │   └── c_lang_fixed.py          # C executor (experimental)
│   ├── ui/
│   │   ├── main_window.py           # Application main window
│   │   ├── editor.py                # Code editor with syntax highlighting
│   │   ├── canvas.py                # Graphics canvas
│   │   ├── output.py                # Output/console panel
│   │   ├── debug_panel.py           # Debug controls
│   │   ├── variable_inspector.py    # Variable inspector dialog
│   │   ├── error_explorer.py        # Error details view
│   │   ├── screen_modes.py          # Screen mode switching
│   │   ├── themes.py                # Theme management (8 built-in themes)
│   │   ├── focus_mode.py            # Distraction-free mode
│   │   ├── crt_effect.py            # Retro monitor effect
│   │   ├── cassette_animation.py    # Cassette loading animation
│   │   ├── accessibility.py         # Accessibility features
│   │   ├── collaboration_client.py  # Multi-user editing
│   │   ├── snippets.py              # Code snippets
│   │   ├── snippet_dialog.py        # Snippet selector dialog
│   │   └── onboarding.py            # First-run wizard
│   ├── graphics/
│   │   └── turtle_state.py          # Turtle state & graphics data
│   ├── cloud/                       # Cloud integration
│   ├── compiler/                    # Code compilation support
│   └── playground.py                # Interactive playground
├── requirements.txt                 # Python dependencies
├── pyproject.toml                   # Package configuration
└── setup.py                         # Installation script
```

### Key Files Explained

**`core/interpreter.py`** (1500+ lines)
- Central command dispatcher routing to language executors
- Manages hardware simulation (Arduino, Raspberry Pi)
- Handles I/O operations
- Coordinates turtle graphics state

**`languages/logo.py`** (800+ lines)
- Complete Logo interpreter with turtle graphics
- Commands: FORWARD, BACK, RIGHT, LEFT, PENDOWN, PENUP, SETCOLOR, PENWIDTH, CLEARSCREEN, etc.
- Sprite support and graphical output

**`languages/basic.py`** (700+ lines)
- BASIC interpreter with variables, loops, conditionals
- GOTO/GOSUB support for subroutines
- INPUT/PRINT for I/O
- Mathematical expressions via safe_eval()

**`ui/main_window.py`**
- Main application window
- Menu bar, status bar layout
- Tab widget coordination
- Theme application and button styling

**`ui/canvas.py`**
- Graphics rendering using QPainter
- Zoom/pan controls
- Turtle rendering and animation

---

## Contributing

### Before You Start

1. **Read the philosophy** – Understand "stateless executors" and "safe evaluation"
2. **Check existing issues** – Avoid duplicate work
3. **Discuss big changes** – Open an issue first if you're changing core behavior

### Workflow

1. **Fork the repository**
2. **Create a feature branch**:
   ```bash
   git checkout -b feature/my-feature
   ```
3. **Make changes** following the guidelines below
4. **Write/update tests** (see Testing section)
5. **Submit PR** with clear description

### Code Style Guidelines

**Python**:
- Follow PEP 8 (use `black` for auto-formatting)
- Type hints for public functions
- Docstrings for classes and public methods
- Max line length: 100 characters

```python
def execute_command(self, command: str) -> str:
    """Execute a single command and return emoji-prefixed output.
    
    Args:
        command: The command to execute
        
    Returns:
        Emoji-prefixed output string (❌, ✅, ℹ️, 🐢, or 🚀)
    """
    pass
```

**Emoji Conventions**:
- `❌` – Error messages
- `✅` – Success confirmations
- `ℹ️` – Informational messages
- `🐢` – Turtle graphics actions
- `🚀` – Execution/runtime events
- `📝` – Input prompts

### Adding a New Language

1. Create `languages/my_language.py`:
```python
class MyLanguageExecutor:
    def __init__(self, interpreter):
        self.interpreter = interpreter
        self.variables = {}
        
    def execute_command(self, command: str) -> str:
        # Parse and execute
        # Return emoji-prefixed output
        return "✅ MyLanguage executed\n"
```

2. Register in `core/interpreter.py`:
```python
from languages.my_language import MyLanguageExecutor
# In __init__:
self.my_language = MyLanguageExecutor(self)
```

3. Add syntax highlighting in `ui/editor.py`

4. Add tests in `Tests/test_my_language.py`

### Adding a UI Feature

1. Create component in `ui/new_component.py`
2. Integrate into `ui/main_window.py`
3. Apply theme colors using `QPalette` where possible
4. Add to menu bar if user-facing
5. Write tests in `Tests/test_ui_new_component.py`

### Example: Adding a Button

```python
# In ui/main_window.py
self.my_button = QPushButton("My Button")
self.my_button.setToolTip("Helpful tooltip")
self.my_button.clicked.connect(self.on_my_button_clicked)

# Styling is applied via main stylesheet in setStyleSheet()
```

---

## Testing

### Test Organization

```
Tests/
├── run_tests.py                            # Test orchestrator
├── test_conformance_basic_pilot_logo.py   # Language conformance tests
├── test_core_interpreter.py               # Interpreter tests
├── test_gui_debug_integration.py          # UI integration tests
└── golden_snapshots/                      # Expected output files
```

### Running Tests

```bash
cd Platforms/Python

# Activate virtual environment
source .venv/bin/activate

# From repo root (run all tests)
cd ../../
python Tests/run_tests.py --comprehensive

# Quick smoke test
python Tests/run_tests.py --quick

# Specific test file
pytest Tests/test_core_interpreter.py -v

# Specific test function
pytest Tests/test_core_interpreter.py::test_basic_math -v

# With coverage
pytest Tests/ --cov=Platforms/Python/time_warp --cov-report=html
```

### Writing Tests

```python
import pytest
from time_warp.core.interpreter import Interpreter, Language

def test_basic_hello_world():
    """Test BASIC PRINT statement."""
    interp = Interpreter()
    interp.load_program('PRINT "Hello"', Language.BASIC)
    output = interp.execute()
    assert "Hello" in output
    assert "❌" not in output  # No errors

def test_logo_forward():
    """Test Logo FORWARD command."""
    from time_warp.graphics.turtle_state import TurtleState
    
    turtle = TurtleState()
    interp = Interpreter()
    interp.load_program("FORWARD 50", Language.LOGO)
    interp.execute(turtle)
    
    # Verify turtle moved
    assert turtle.y == 50  # Moved forward 50 units

def test_error_handling():
    """Test that errors are properly reported."""
    interp = Interpreter()
    interp.load_program("INVALID COMMAND", Language.BASIC)
    output = interp.execute()
    assert output.startswith("❌")
```

### Test Best Practices

1. **Use fixtures** for shared state
2. **Keep tests small** – one assertion per test when possible
3. **Test both success and failure** cases
4. **Use descriptive names** – `test_basic_for_loop_iterations_correctly()` not `test_loop()`
5. **Document complex assertions** with comments

---

## Release Process

### Version Numbers

We use semantic versioning: `MAJOR.MINOR.PATCH`
- `MAJOR` – Breaking changes or major features
- `MINOR` – New features, backward compatible
- `PATCH` – Bug fixes

### Release Steps

1. **Update version**:
   ```bash
   # In Platforms/Python/setup.py
   version="5.1.0"
   ```

2. **Update changelog**:
   - Add entry to `Docs/misc/RELEASE_NOTES.md`
   - List major features and bug fixes

3. **Run tests**:
   ```bash
   python Tests/run_tests.py --comprehensive
   ```

4. **Create git tag**:
   ```bash
   git tag -a v5.1.0 -m "Release version 5.1.0"
   git push origin v5.1.0
   ```

5. **GitHub Release**:
   - Go to Releases → Create New Release
   - Use tag name: `v5.1.0`
   - Add release notes (copy from RELEASE_NOTES.md)
   - Mark as "Latest" if appropriate

---

## Troubleshooting

### "Illegal instruction" on Startup

**Problem**: PySide6 requires SSSE3/SSE4 CPU instructions, not available in some VMs.

**Solution**: Run on physical hardware or modern cloud instance. Check:
```bash
grep -o 'sse4_1\|ssse3' /proc/cpuinfo
```

### ImportError: No module named 'PySide6'

**Problem**: Dependencies not installed.

**Solution**:
```bash
pip install -r requirements.txt
```

### Tests Fail with "AttributeError: 'module' object has no attribute"

**Problem**: Stale `.pyc` files or imports from wrong version.

**Solution**:
```bash
find . -type d -name __pycache__ -exec rm -r {} +
pip install -e .  # Reinstall in development mode
```

---

## Resources

### Documentation
- [Python Official Docs](https://docs.python.org/3/)
- [PySide6 Documentation](https://doc.qt.io/qtforpython-6/)
- [pytest Documentation](https://docs.pytest.org/)

### Community
- [GitHub Issues](https://github.com/James-HoneyBadger/Time_Warp/issues)
- [GitHub Discussions](https://github.com/James-HoneyBadger/Time_Warp/discussions)

### Tools
- [Python REPL](https://www.python.org/)
- [Qt Designer](https://doc.qt.io/qt-6/qtdesigner-manual.html) (optional, for UI work)

---

## Getting Help

**Questions?**
- Search [existing issues](https://github.com/James-HoneyBadger/Time_Warp/issues)
- Ask in [Discussions](https://github.com/James-HoneyBadger/Time_Warp/discussions)
- Read the [FAQ](../user/03-faq.md)

**Found a bug?**
- Check if already reported
- Create issue with steps to reproduce, expected/actual behavior, system info

**Want a feature?**
- Describe the use case and educational value
- Discuss in Discussions before starting major work

---

Thank you for contributing to Time Warp IDE!
