# Time Warp Studio - AI Coding Agent Instructions

**Project:** Time Warp Studio - Educational multi-language programming environment  
**Maintainer:** James Temple <james@honey-badger.org>  
**Last Updated:** April 2026

---

## Project Overview

Time Warp Studio is an educational desktop programming environment built with Python and PySide6 (Qt6) that provides a unified IDE for learning **20 programming languages** with integrated turtle graphics.

**Supported Languages:** BASIC, PILOT, Logo, C, Pascal, Prolog, Forth, Python, Lua, Scheme, Brainfuck, JavaScript, REXX, Smalltalk, HyperTalk, Haskell, Erlang, Ruby, Rust, Perl.

**Current State:** Native desktop application (Python/PySide6) - single actively maintained version.

## Architecture: The Big Picture

### Implementation

- **Desktop Application (Python/PySide6)** — primary and only maintained version
    - Entry point: `Platforms/Python/time_warp_ide.py`
    - Core: `Platforms/Python/time_warp/core/interpreter.py`
    - Languages: 20 executors in `Platforms/Python/time_warp/languages/`
    - UI: PySide6 (Qt6) with modern desktop interface (30+ UI modules)
    - All UI state (editor, canvas, themes) managed by main application

**Critical Design Decision:** Language executors are stateless command processors returning text output. All UI state (turtle canvas, output display, themes) lives in the main application, not the interpreter.

## Language Executor Pattern

Each language executor is a **function** (not a class) conforming to the Protocol in `languages/base.py`:

```python
def execute_my_lang(interpreter: Interpreter, source: str, turtle: TurtleState) -> str:
    """Execute source code, return output text with emoji prefixes."""
    ...
```

**Two execution modes:**
- **Whole-program executors** (13 languages): Receive the entire source as a string. Registered in `_WHOLE_PROGRAM_EXECUTORS` dict in `core/interpreter.py`. These are: Python, Lua, Scheme, Brainfuck, JavaScript, REXX, Smalltalk, HyperTalk, Haskell, Erlang, Ruby, Rust, Perl.
- **Line-by-line executors** (7 languages: BASIC, PILOT, Logo, C, Pascal, Prolog, Forth): The interpreter iterates lines and calls the executor per statement.

When adding a new whole-program language, only one dict needs updating: `_WHOLE_PROGRAM_EXECUTORS` in `core/interpreter.py`.

## Critical Workflows

### Running the Desktop IDE

```bash
# Primary method
python Platforms/Python/time_warp_ide.py

# Or use the smart launcher (handles venv + deps):
python run.py

# System Requirements
# - Python 3.10+
# - PySide6 (auto-installed if needed)
# - CPU with SSSE3/SSE4.1/SSE4.2/POPCNT support
# Note: Older VMs/QEMU may lack required CPU features
```

### Testing Strategy

```bash
# Comprehensive suite with coverage
python Platforms/Python/test_runner.py --comprehensive

# Quick smoke tests
python Platforms/Python/test_runner.py --basic

# Component-specific
pytest Platforms/Python/time_warp/tests/test_basic_executor.py -v
pytest Platforms/Python/time_warp/tests/test_logo_graphics.py -v

# All demo programs (standalone, 15s timeout per file)
python tests/test_all_demos.py
```

**Test Organization:**
- `test_*.py` = unit tests for components (41 test files in `time_warp/tests/`)
- `Platforms/Python/test_runner.py` = orchestrator with HTML reports -> `test_reports/`
- `tests/test_all_demos.py` = standalone demo verifier (subprocess per file)
- `conftest_lang.py` = shared `run()`, `ok()`, `has()`, `no_errors()` test helpers

### Adding a New Language

1. Create `time_warp/languages/my_lang.py`:
```python
from __future__ import annotations
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

def execute_my_lang(interpreter: Interpreter, source: str, turtle: TurtleState) -> str:
    """Execute MyLang source code."""
    output_lines = []
    # Parse and execute source
    output_lines.append("✅ MyLang output")
    return "\n".join(output_lines) + "\n"
```

2. Add to `core/interpreter.py`:
   - Import: `from ..languages.my_lang import execute_my_lang`
   - Add `Language.MY_LANG` enum member
   - Add entry in `_init_whole_program_executors()` (for whole-program langs)
   - Add `Language.MY_LANG` in `Language.from_extension()` mapping

3. Add syntax highlighting and file extensions in `ui/editor.py`

## Project-Specific Conventions

### Emoji Prefixes (Universal Pattern)
- `❌` = Errors/exceptions
- `✅` = Success confirmations
- `ℹ️` = Informational messages
- `🎨` = Theme/UI changes
- `🚀` = Execution/run events
- `🐢` = Turtle graphics actions
- `📝` = Input prompts

### Safe Expression Evaluation
**Never use `eval()` directly.** Use the `ExpressionEvaluator` class for math expressions:
```python
from time_warp.utils.expression_evaluator import ExpressionEvaluator
evaluator = ExpressionEvaluator(variables={"X": 5})
result = evaluator.evaluate("2 + 3 * X")  # Returns 17
```

### Hardware/IoT Integration
Simulation-first design in `core/hardware_simulator.py` (not yet wired into main interpreter). Future feature.

## Integration Points to Watch

### Turtle Graphics State
- Turtle state managed via `graphics/turtle_state.py` (593 lines)
- Graphics rendered using Qt painter with zoom/pan support in `ui/canvas.py`
- Canvas clearing happens in main UI, not executor

### Theme System
`ui/themes.py` defines 28 themes (Dracula, Monokai, Catppuccin Mocha, Gruvbox Dark, VS Code Dark/Light, GitHub Dark/Light, Nord, Solarized, plus retro CRT themes). Persisted via QSettings. Apply via `ThemeManager.apply_theme()`.

## Common Pitfalls

1. **PySide6 CPU requirements** - "Illegal instruction" errors = missing CPU features, not code bugs.
2. **Executor statelessness** - Don't store UI refs in executors; return strings only.
3. **Test isolation** - Use `conftest.py` fixtures for interpreter instances; don't share state.
4. **Whole-program language registration** - Only update `_WHOLE_PROGRAM_EXECUTORS` in `core/interpreter.py`. No other dispatch tables need updating.

## File Structure at a Glance

```
Platforms/Python/
  time_warp_ide.py          - Primary entry point (PySide6)
  test_runner.py            - Test orchestration with HTML reports
  time_warp/
    core/
      interpreter.py        - Central interpreter (~1400 lines)
      orchestrator.py       - System integration, component registry
      debugger.py           - Execution timeline / step-through debugger
      sql_engine.py         - SQLite-backed T-SQL compatibility layer
      config.py             - Canonical paths (~/.time_warp/)
    features/               - Feature modules (AI, collaboration, games, etc.)
    languages/              - 24 language executors
      base.py               - Protocol definition
      basic.py, pilot.py, logo.py, c_lang_fixed.py, pascal.py,
      prolog.py, forth.py, python.py, lua.py, scheme.py, cobol.py,
      brainfuck.py, assembly.py, javascript.py, fortran.py, rexx.py,
      smalltalk.py, hypertalk.py, haskell.py, apl.py, sql.py, jcl.py,
      cics.py, sqr.py
    ui/                     - 30+ UI modules
      main_window.py        - PySide6 main window (uses 6 mixins)
      editor.py             - Code editor + minimap + line numbers
      themes.py             - Theme manager (28 themes)
      canvas.py             - Turtle graphics canvas
      output.py             - Output panel + interpreter threads
      debug_panel.py        - Debug controls/watch/call-stack
      command_palette.py    - Ctrl+Shift+P palette
      mixins/               - Collaboration, classroom, debug, export, file ops, help
    graphics/
      turtle_state.py       - Turtle state management
    utils/
      expression_evaluator.py - Hand-written tokenizer + recursive-descent parser
      error_hints.py        - Syntax error suggestions
      validators.py         - Input validation helpers
    tests/                  - 41 test files
docker/                     - Dockerfiles, nginx, supervisord configs
tests/                      - Root-level integration tests
Examples/                   - ~93 demo programs across all languages
```

## Dependencies

Core: `pillow>=10.0.0`, `PySide6` (Qt6)
Dev: `pytest`, `pytest-cov`, `pytest-mock`, `ruff`
Optional: `pyfirmata` (Arduino), `RPi.GPIO` (Raspberry Pi), `openai`, `librosa`

---

**When in doubt:** Read `core/interpreter.py` (main dispatch logic, look for `_WHOLE_PROGRAM_EXECUTORS`) and check `test_runner.py --help` for test workflows.

### Key Components

- **Interpreter**: Main interpreter class handling command dispatch and execution
- **Language Executors**: 24 executor functions in `time_warp/languages/`
- **UI Components**: Qt-based UI (`ui/main_window.py`) with editor, canvas, and turtle controls
- **Theme System**: `ui/themes.py` with 28 themes and persistent configuration
- **Graphics Canvas**: Unified drawing surface in `ui/canvas.py` for all turtle graphics output
- **Debugger**: Step-through debugger with execution timeline in `core/debugger.py`

### File Naming Conventions

- **Test files**: `test_*.py` for unit tests
- **Language demos**: `*.bas`, `*.logo`, `*.pilot`, `*.c`, `*.py`, etc. in `Examples/`
- **Language executors**: Named after the language in `time_warp/languages/`

### Configuration Management

- User settings stored in `~/.time_warp/config.json`
- Theme preferences persist between sessions via QSettings
- Virtual environment used for Python dependencies

### Error Handling Patterns

All interpreter errors are returned as strings with emoji prefixes:
- `❌` for errors
- `ℹ️` for info messages
- `🎨` for theme changes

### Execution Guidelines

PROGRESS TRACKING:
- If any tools are available to manage the above todo list, use it to track progress through this checklist.
- After completing each step, mark it complete and add a summary.
- Read current todo list status before starting each new step.

## Development Workflows

COMMUNICATION RULES:
- Avoid verbose explanations or printing full command outputs.
- If a step is skipped, state that briefly (e.g. "No extensions needed").
- Do not explain project structure unless asked.
- Keep explanations concise and focused.

### Running Time_Warp

```bash
# Primary method
python Platforms/Python/time_warp_ide.py
```

DEVELOPMENT RULES:
- Use '.' as the working directory unless user specifies otherwise.
- Avoid adding media or external links unless explicitly requested.
- Use placeholders only with a note that they should be replaced.
- Use VS Code API tool only for VS Code extension projects.

## Editing Guidelines

- Use `replace_string_in_file` for precise edits, providing 3-5 lines of context before and after.
- For `apply_patch`, ensure minimal diffs, preserve indentation, and do not reformat unrelated code.
- Best practices: keep changes minimal, avoid adding external links unless requested.

### Testing

See `Platforms/Python/test_runner.py --help` for testing options:

```bash
# Run comprehensive test suite
python Platforms/Python/test_runner.py --comprehensive

# Run quick smoke tests
python Platforms/Python/test_runner.py --basic
```

FOLDER CREATION RULES:
- Always use the current directory as the project root.
- Do not create a new folder unless the user explicitly requests it besides a .vscode folder for a tasks.json file.

EXTENSION INSTALLATION RULES:
- Only install extension specified by the get_project_setup_info tool. DO NOT INSTALL any other extensions.

### Adding New Languages

1. Create executor function in `time_warp/languages/new_language.py`
2. Implement `execute_<lang>(interpreter, source, turtle) -> str` following existing patterns
3. Register in `core/interpreter.py`: add import, Language enum member, and `_WHOLE_PROGRAM_EXECUTORS` entry
4. Add file extension mapping in `Language.from_extension()`
5. Add syntax highlighting and file extensions to `ui/editor.py`

PROJECT CONTENT RULES:
- If the user has not specified project details, assume they want a "Hello World" project as a starting point.
- Avoid adding links of any type (URLs, files, folders, etc.) or integrations that are not explicitly required.
- Avoid generating images, videos, or any other media files unless explicitly requested.
- Ensure all generated components serve a clear purpose within the user's requested workflow.
- If a feature is assumed but not confirmed, prompt the user for clarification before including it.

### Theme Development

Themes defined in `ui/themes.py` with color schemes applied uniformly across:
- Main window backgrounds
- Editor components
- Menu systems
- Button styles
- Output panels
- Retro CRT effects (Amber, Green, C64, Apple II, etc.)

TASK COMPLETION RULES:
- Your task is complete when:
  - Code runs without errors (`python Platforms/Python/time_warp_ide.py`)
  - copilot-instructions.md file in the .github directory exists in the project
  - README.md file exists and is up to date
  - User is provided with clear instructions to debug/launch the project

### Plugin Development

Not yet implemented - future extensible architecture.

Before starting a new task in the above plan, update progress in the plan.

- Work through each checklist item systematically.

## Critical Integration Points

- Keep communication concise and focused.
- Follow development best practices.

### Interpreter-UI Communication
- Commands executed through `Interpreter.execute()` method
- Results displayed via Qt widgets and canvas
- Error handling centralized through status messages
- Input prompts handled through Qt input widgets

### Turtle Graphics Integration
- Turtle state managed in `graphics/turtle_state.py`
- Graphics rendered using Qt painter with zoom/pan support in `ui/canvas.py`
- Canvas clearing and setup handled automatically per execution
- Compatible with existing turtle graphics commands

### Screen Mode Management
- **Text**: Text mode with configurable cols/rows via `ScreenConfig`
- **Graphics**: Full canvas with 2D drawing using Qt painter
- **Turtle Graphics**: Integrated with canvas

### Hardware/IoT Extensions
Future features (stubs exist in `core/hardware_simulator.py`):
- Raspberry Pi GPIO control
- Sensor data visualization
- Arduino integration

## Code Style and Conventions

- Use descriptive docstrings for all classes and functions
- Error messages prefixed with emoji indicators (`❌`, `ℹ️`, `🎨`, `🚀`)
- Graceful degradation for optional dependencies
- Consistent Python formatting (PEP 8)
