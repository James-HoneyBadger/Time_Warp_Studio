# Time Warp IDE - AI Coding Agent Instructions

**Project:** Time Warp IDE - Educational multi-language programming environment  
**Maintainer:** James Temple <james@honey-badger.org>  
**Last Updated:** October 27, 2025

---

## Project Overview

Time Warp IDE is an educational programming environment for TempleCode — a unified language combining BASIC, PILOT, and Logo — with integrated turtle graphics, IoT/robotics capabilities, and game development features.

**Current State:** Rust implementation is primary. Legacy Python variants have been removed.

## Architecture: The Big Picture

### Implementation

- **Rust (egui)** — primary and official version
  - Entry point: `src/main.rs` → App: `src/app.rs` → Interpreter: `src/interpreter/`

**Critical Design Decision:** Language executors are stateless command processors returning text output. All UI state (turtle canvas, output display, themes) lives in the main application, not the interpreter.

## Language Executor Pattern

Each language executor in Rust handles parsing and updates interpreter state, emitting text output with emoji prefixes (❌ error, ℹ️ info, 🎨 theme, 🚀 run, 🐢 turtle).

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

- **TimeWarpApp**: Main egui app struct containing all state and UI logic
- **TurtleState**: Struct managing turtle graphics position, angle, and drawing state
- **UI Components**: Menu bar, status bar, code editor, canvas, and turtle controls
- **Theme Support**: egui theming for light/dark modes
- **File I/O**: rfd crate for cross-platform file dialogs

### File Naming Conventions

- **Source files**: `src/*.rs` for Rust source code
- **Test files**: `tests/*.rs` for unit tests
- **Compiled output**: `target/debug/time-warp-ide` for executable
- **Examples**: `examples/*.twb`, `examples/*.twp`, `examples/*.tpr` for sample programs

### Configuration Management

- User settings stored in application state (future: config file)
- Theme preferences persist between sessions
- Virtual environment not needed (compiled binary)

### Error Handling Patterns

```rust
// Standard Time_Warp error pattern
match self.execute_command(command) {
    Ok(result) => {
        // Handle success
    }
    Err(e) => {
        self.status_message = format!("Error: {}", e);
    }
}
```

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
# Build and run
cargo run

# Build release
cargo build --release
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

```bash
# Run tests
cargo test

# Run specific test
cargo test test_name
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

Themes defined using egui's theming system:
- Main window backgrounds
- Editor components
- Menu systems
- Button styles
- Output panels

TASK COMPLETION RULES:
- Your task is complete when:
  - Project compiles without errors (`cargo build`)
  - Executable runs successfully (`cargo run`)
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
- Commands executed through `TimeWarpApp::execute_command()` method
- Results displayed via egui widgets and canvas
- Error handling centralized through status messages
- Input prompts handled through egui text input widgets

### Turtle Graphics Integration
- Turtle state managed in `TurtleState` struct
- Graphics rendered using egui painter with zoom/pan support
- Canvas clearing and setup handled automatically per execution
- Compatible with existing turtle graphics commands

### Screen Mode Management
- **Single Mode**: Graphics mode with text overlay
- **Text Grid**: Text input/output with egui text widgets
- **Graphics**: Full canvas with 2D drawing using egui painter
- **Turtle Graphics**: Integrated with canvas

### Hardware/IoT Extensions
Future features for:
- Raspberry Pi GPIO control
- Sensor data visualization
- Arduino integration
- Smart home device management

## Code Style and Conventions

- Use descriptive docstrings for all structs and functions
- Error messages prefixed with emoji indicators (`❌`, `ℹ️`, `🎨`, `🚀`)
- Graceful degradation for optional dependencies
- Consistent Rust formatting (`cargo fmt`)
- Type-safe variable management

## Rust Testing Strategy

Tests focus on:
- Individual language executor functionality
- UI component interactions
- File loading/saving operations
- Turtle graphics rendering
- Multi-language integration scenarios

When adding features, ensure compatibility across all supported languages and maintain the educational focus of the platform.

### Rust File Naming Conventions

- **Test files**: `test_*.py` for unit tests, `*_test.py` for integration tests
- **Language demos**: `*.pilot`, `*.bas`, `*.logo` for example programs
- **Compiled output**: `*_compiled` files for interpreter execution results

### Rust Configuration Management

- User settings stored in application state (future: config file)
- Theme preferences persist between sessions
- Virtual environment not needed (compiled binary)

### Rust Error Handling Patterns

```rust
// Standard Time_Warp error pattern
match self.execute_command(command) {
    Ok(result) => {
        // Handle success
    }
    Err(e) => {
        self.status_message = format!("Error: {}", e);
    }
}
```

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
# Build and run
cargo run

# Build release
cargo build --release
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

```bash
# Run tests
cargo test

# Run specific test
cargo test test_name
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
  - Project is successfully scaffolded and compiled without errors
  - copilot-instructions.md file in the .github directory exists in the project
  - README.md file exists and is up to date
  - User is provided with clear instructions to debug/launch the project

### Plugin Development

See `plugins/sample_plugin/` for complete plugin template including:
- `__init__.py` with plugin metadata
- Main plugin class with `initialize()` method
- Integration hooks for UI and interpreter

Before starting a new task in the above plan, update progress in the plan.

- Work through each checklist item systematically.

## Critical Integration Points

- Keep communication concise and focused.
- Follow development best practices.

### Interpreter-UI Communication
- Commands executed through `Time_WarpInterpreter.execute()` method
- Results displayed via unified canvas text/graphics rendering methods
- Error handling centralized through interpreter's error display system
- Input prompts handled through `UnifiedCanvas.prompt_input()` with callback system

### Turtle Graphics Integration
- Each language executor can access `self.interpreter.ide_unified_canvas`
- Graphics state managed in unified canvas with screen mode awareness
- Canvas clearing and setup handled automatically per execution
- Compatible with existing turtle graphics commands

### Screen Mode Management
- **Single Mode**: Mode 11 only - Unified Canvas (1024×768, 256 colors)
- **Text Grid**: 25 rows × 80 columns for input/output
- **Graphics**: Full 1024×768 pixel canvas with 256 colors
- **Turtle Graphics**: Integrated with unified canvas
>>>>>>> 066f2538e86bb3d8413c1ab261082a1d003dc877

### Hardware/IoT Extensions
Advanced features in `core/hardware/` and `core/iot/` for:
- Raspberry Pi GPIO control
- Sensor data visualization
- Arduino integration
- Smart home device management

## Code Style and Conventions

- Use descriptive docstrings for all structs and functions
- Error messages prefixed with emoji indicators (`❌`, `ℹ️`, `🎨`, `🚀`)
- Graceful degradation for optional dependencies
- Consistent Rust formatting (`cargo fmt`)
- Type-safe variable management

## Testing Strategy

Tests focus on:
- Individual language executor functionality
- Theme system persistence
- File loading/saving operations
- Multi-language integration scenarios
- UI component interactions

When adding features, ensure compatibility across all supported languages and maintain the educational focus of the platform.