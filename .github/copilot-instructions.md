# Time Warp IDE - AI Coding Agent Instructions

**Project:** Time Warp IDE - Educational multi-language programming environment  
**Maintainer:** James Temple <james@honey-badger.org>  
**Last Updated:** October 27, 2025

---

## Project Overview

Time Warp IDE is an educational programming environment that unifies BASIC, PILOT, and Logo with integrated turtle graphics, IoT/robotics capabilities, and game development features.

**Current State:** Python implementation (PySide6) is the sole actively maintained version.

## Architecture: The Big Picture

### Implementation

- **Python (PySide6)** — primary and official version
    - Entry point: `time_warp_ide.py` → Core: `Platforms/Python/time_warp/core/interpreter.py`
    - Languages: BASIC, PILOT, Logo (+ Pascal, Prolog, C experimental)
    - All UI state (editor, canvas, themes) lives outside language executors

**Critical Design Decision:** Language executors are stateless command processors returning text output. All UI state (turtle canvas, output display, themes) lives in the main application, not the interpreter.

## Language Executor Pattern

Each language executor in Python handles parsing and updates interpreter state, emitting text output with emoji prefixes (❌ error, ℹ️ info, 🎨 theme, 🚀 run, 🐢 turtle).

## Critical Workflows

### Running the IDE

```bash
# Primary method (auto-installs PySide6 if needed)
python Time_Warp_IDE.py

# System Requirements Check
# IDE exits with clear error if CPU lacks SSSE3/SSE4.1/SSE4.2/POPCNT
# Common on older VMs/QEMU instances - must run on physical hardware or modern cloud
```

### Testing Strategy

```bash
# Comprehensive suite with coverage
python test_runner.py --comprehensive

# Quick smoke tests
python test_runner.py --basic

# Component-specific
pytest tests/test_core_interpreter.py -v
pytest tests/test_logo_graphics.py -v
```

**Test Organization:**
- `test_*.py` = unit tests for components
- `*_test.py` = integration/workflow tests  
- `test_runner.py` = orchestrator with HTML reports → `test_reports/`

### Adding a New Language

1. Create `core/interpreters/my_lang.py`:
```python
from . import LanguageExecutor

class MyLangExecutor(LanguageExecutor):
    def __init__(self, interpreter):
        self.interpreter = interpreter
        self.variables = {}
    
    def execute_command(self, command: str) -> str:
        # Parse and execute
        return "✅ MyLang output\n"
```

2. Register in `core/interpreter.py`:
```python
from core.interpreters.my_lang import MyLangExecutor
# In __init__: self.my_lang = MyLangExecutor(self)
```

3. Add detection logic in `TimeWarpInterpreter.execute()`

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
**Never use `eval()` directly.** Use `core/safe_expression_evaluator.py::safe_eval()` for math expressions:
```python
from core.safe_expression_evaluator import safe_eval
result = safe_eval("2 + 3 * X", {"X": 5})  # Returns 17
```

### Hardware/IoT Integration
Simulation-first design in `core/interpreter.py`:
```python
self.arduino = ArduinoController(simulation_mode=True)
self.rpi = RPiController(simulation_mode=True)
```
Real hardware requires `pyfirmata`/`RPi.GPIO` (optional deps).

### Plugin System
Plugins in `plugins/sample_plugin/`:
```python
# __init__.py metadata
PLUGIN_NAME = "Sample Plugin"
PLUGIN_VERSION = "1.0.0"

# plugin.py
class SamplePlugin:
    def initialize(self, ide_instance):
        """Hook into IDE after startup"""
        self.ide = ide_instance
```

Load via `core/plugin_system.py::PluginManager.discover_plugins()`

## Integration Points to Watch

### Turtle Graphics State
- Executors track position/angle internally (e.g., `LogoExecutor.turtle_x/y/angle`)
- UI reads turtle state from executor for rendering: `self.logo_executor.turtle_x`
- Canvas clearing happens in main UI, not executor

### Async Support
`core/async_support.py` provides `AsyncInterpreterRunner` for non-blocking execution:
```python
from core.async_support import get_async_runner
runner = get_async_runner()
runner.run_code(code_string, callback=on_complete)
```

### Theme System
`tools/theme.py` defines 8 themes (Dracula, Monokai, Solarized Dark, Ocean, Spring, Sunset, Candy, Forest). Persisted to `~/.Time_Warp/config.json`. Apply via `QtUIFactory.apply_theme()`.

## Common Pitfalls

1. **Don't modify `Time_Warp.py`** - It's archived. Work in `Time_Warp_IDE.py` + `ui/` + `core/`.
2. **PySide6 CPU requirements** - "Illegal instruction" errors = missing CPU features, not code bugs.
3. **Executor statelessness** - Don't store UI refs in executors; return strings only.
4. **Test isolation** - Use `conftest.py` fixtures for interpreter instances; don't share state.

## File Structure at a Glance

- `Time_Warp_IDE.py` - Primary entry point (PySide6)
- `core/interpreter.py` - Central interpreter (1521 lines, dispatches to executors)
- `core/interpreters/` - Language executors (pilot, basic, logo)
- `ui/qt_ui.py` - PySide6 UI factory
- `tools/theme.py` - Theme manager (8 themes)
- `test_runner.py` - Test orchestration with reporting
- `tests/` - Pytest suite (30+ test files)
- `plugins/` - Plugin samples and docs
- `examples/` - Demo programs in all languages

## Dependencies

Core: `pillow>=10.0.0`, `PySide6` (Qt6)  
Dev: `pytest`, `pytest-cov`, `pytest-mock`, `black`, `mypy`  
Optional: `pyfirmata` (Arduino), `RPi.GPIO` (Raspberry Pi), `scikit-learn` (ML demos)

---

**When in doubt:** Read `core/interpreter.py` (main dispatch logic) and check `test_runner.py --help` for test workflows.

### Key Components

- **TimeWarpInterpreter**: Main interpreter class handling command dispatch and execution
- **Language Executors**: Individual BASIC, PILOT, Logo, etc. modules in `core/interpreters/`
- **UI Components**: Qt-based UI (main_window.py) with editor, canvas, and turtle controls
- **Theme System**: Theme manager in `tools/theme.py` with persistent configuration
- **Graphics Canvas**: Unified drawing surface for all turtle graphics output

### File Naming Conventions

- **Test files**: `test_*.py` for unit tests, `*_test.py` for integration tests
- **Language demos**: `*.pilot`, `*.bas`, `*.logo` for example programs
- **Compiled output**: `*_compiled` files for interpreter execution results

### Configuration Management

- User settings stored in `~/.Time_Warp/config.json`
- Theme preferences persist between sessions
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
python Time_Warp_IDE.py
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

See `test_runner.py --help` for testing options:

```bash
# Run comprehensive test suite
python test_runner.py --comprehensive

# Run quick smoke tests  
python test_runner.py --basic
```

FOLDER CREATION RULES:
- Always use the current directory as the project root.
- Do not create a new folder unless the user explicitly requests it besides a .vscode folder for a tasks.json file.
- If any of the scaffolding commands mention that the folder name is not correct, let the user know to create a new folder with the correct name and then reopen it again in vscode.

EXTENSION INSTALLATION RULES:
- Only install extension specified by the get_project_setup_info tool. DO NOT INSTALL any other extensions.

### Adding New Languages

1. Create executor module in `src/languages/new_language.rs`
2. Implement `execute_command()` method following existing patterns
3. Register in `src/main.rs` import and language mapping
4. Add syntax highlighting and file extensions to main UI

PROJECT CONTENT RULES:
- If the user has not specified project details, assume they want a "Hello World" project as a starting point.
- Avoid adding links of any type (URLs, files, folders, etc.) or integrations that are not explicitly required.
- Avoid generating images, videos, or any other media files unless explicitly requested.
- If you need to use any media assets as placeholders, let the user know that these are placeholders and should be replaced with the actual assets later.
- Ensure all generated components serve a clear purpose within the user's requested workflow.
- If a feature is assumed but not confirmed, prompt the user for clarification before including it.

### Theme Development

Themes defined in `tools/theme.py` with color schemes applied uniformly across:
- Main window backgrounds
- Editor components
- Menu systems
- Button styles
- Output panels

TASK COMPLETION RULES:
- Your task is complete when:
  - Code runs without errors (`python Time_Warp_IDE.py`)
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
- Commands executed through `TimeWarpInterpreter.execute()` method
- Results displayed via Qt widgets and canvas
- Error handling centralized through status messages
- Input prompts handled through Qt input widgets

### Turtle Graphics Integration
- Turtle state managed in Language Executors
- Graphics rendered using Qt painter with zoom/pan support
- Canvas clearing and setup handled automatically per execution
- Compatible with existing turtle graphics commands

### Screen Mode Management
- **Single Mode**: Graphics mode with text overlay
- **Text Grid**: Text input/output with Qt text widgets
- **Graphics**: Full canvas with 2D drawing using Qt painter
- **Turtle Graphics**: Integrated with canvas

### Hardware/IoT Extensions
Future features for:
- Raspberry Pi GPIO control
- Sensor data visualization
- Arduino integration
- Smart home device management

## Code Style and Conventions

- Use descriptive docstrings for all classes and functions
- Error messages prefixed with emoji indicators (`❌`, `ℹ️`, `🎨`, `🚀`)
- Graceful degradation for optional dependencies
- Consistent Python formatting (PEP 8)


