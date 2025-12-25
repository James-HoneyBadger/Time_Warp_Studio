# TW Editor

TW Editor is a standalone code editor designed for Time Warp languages (BASIC, PILOT, Logo, Pascal, Prolog, Forth, C). It provides syntax highlighting, code formatting, and basic editing features without the overhead of the full IDE.

## Features

*   **Syntax Highlighting**: Supports all Time Warp languages.
*   **Code Formatting**: Auto-format code for BASIC, Logo, and PILOT.
*   **Line Numbers**: With breakpoint toggling (visual only in editor).
*   **Auto-completion**: Basic keyword completion.
*   **Lightweight**: Fast startup and low memory usage.

## How to Run

### From Source

```bash
./Scripts/launch_editor.sh
```

### From Python

```bash
cd Platforms/Python
python3 tw_editor.py
```

## Key Shortcuts

*   **New**: `Ctrl+N`
*   **Open**: `Ctrl+O`
*   **Save**: `Ctrl+S`
*   **Format Code**: `Ctrl+Shift+F`
*   **Undo**: `Ctrl+Z`
*   **Redo**: `Ctrl+Y` or `Ctrl+Shift+Z`
