# Developer Reference

This document describes the architecture and extension points of the Time Warp IDE.

## Architecture Overview

The Time Warp IDE is a multi-language educational programming environment built with Python and PySide6 (Qt). It supports multiple languages (BASIC, Logo, PILOT, TempleCode) through a unified interpreter architecture.

### Directory Structure

- `platforms/python/` — Main Python implementation.
  - `time_warp_ide.py` — Main entry point for the IDE.
  - `time_warp/` — Core package.
    - `core/` — Interpreter core, state management, and plugin system.
      - `interpreter.py` — Main `TimeWarpInterpreter` class.
    - `languages/` — Language-specific executors.
      - `basic.py` — BASIC language implementation.
      - `logo.py` — Logo language implementation.
      - `pilot.py` — PILOT language implementation.
      - `temple_code.py` — TempleCode language implementation.
    - `ui/` — PySide6 UI components.
      - `qt_ui.py` — Main window and UI factory.
      - `widgets/` — Custom widgets (Canvas, Terminal, Editor).
    - `utils/` — Utility modules (Expression evaluator, file I/O).

## Key Components

### Interpreter (`core/interpreter.py`)
The `TimeWarpInterpreter` is the central engine. It:
- Manages execution state (variables, arrays, call stack).
- Dispatches commands to language executors.
- Handles control flow (loops, subroutines).
- Manages the unified canvas and turtle graphics state.

### Language Executors (`languages/`)
Each language is implemented as a `LanguageExecutor` subclass. They:
- Parse language-specific syntax.
- Execute commands.
- Interact with the interpreter state.
- Return output strings with emoji prefixes for the UI.

### User Interface (`ui/`)
The UI is built with PySide6.
- **Unified Canvas**: A custom widget that handles both turtle graphics and pixel-based drawing.
- **Terminal**: A rich-text console for input/output.
- **Editor**: A syntax-highlighting code editor.

## Extending the Language

To add a new command or feature:

1.  **Identify the Language**: Determine which language (BASIC, Logo, etc.) the command belongs to.
2.  **Implement in Executor**: Add the logic to the corresponding file in `languages/`.
    -   Example: To add a `BEEP` command to BASIC, modify `languages/basic.py`.
3.  **Register Command**: Ensure the command is recognized by the parser in the executor's `execute_command` method.
4.  **Update UI (if needed)**: If the command requires UI interaction (e.g., a dialog), use the `interpreter.ui_interface`.

## Debugging

-   **Console Output**: The IDE prints detailed logs to the terminal if run from a command line.
-   **Emoji Prefixes**:
    -   ❌ Error
    -   ℹ️ Info
    -   🎨 Theme Change
    -   🚀 Execution Start
    -   🐢 Turtle Action

## Configuration

Preferences are stored in `~/.time_warp/config.json`:
-   `theme`: UI theme name.
-   `font_size`: Editor font size.
-   `window_geometry`: Saved window position and size.

## Testing

Run tests using the `test_runner.py` script in the `platforms/python` directory:
```bash
cd platforms/python
python test_runner.py --basic
```

- Use `run_templecode.py` for fast iteration; it reports interpreter errors without starting the GUI.
- For graphical tests, run the IDE and use the Examples menu. Add tiny scripts for new commands.
