# Developer Reference

This document describes the architecture and extension points of TempleCode and Time Warp.

## Architecture

- `templecode/interpreter.py` — TempleCode interpreter, safe expression evaluator, program model, and command execution.
- `time.warp/app.py` — Tkinter IDE: editor, console, canvas (turtle), menus, status bar, examples, preferences.
- `examples/` — sample TempleCode programs.
- `run_templecode.py` — minimal CLI runner (no graphics).

## Key components

- SafeEval: wraps Python `ast` with a whitelist for expressions.
- TempleInterpreter: maintains program lines, labels, procedure table, loop stacks, and call stack.
- IOBase: pluggable I/O for console input/output.
- TurtleAPI: pluggable turtle drawing backend; IDE provides a CanvasTurtle.

## Extending the language

Add a new statement:

1. Define any needed turtle or I/O methods in `TurtleAPI` (optional) and implement them in the IDE's `CanvasTurtle`.
2. Handle the new keyword in `TempleInterpreter._exec_line` (parse args with `_split_args` or `_split_on_comma`, evaluate with `_eval`).
3. Add the keyword to the IDE highlighter in `TimeWarpApp._setup_highlighting`.
4. Add examples and documentation.

Expression functions:

- Add safe helpers to `_ALLOWED_FUNCS` in `interpreter.py`.

## Debugging/tracing

- `TRACE ON/OFF`: Echoes each non-comment line before execution.
- `DUMPVARS`: Prints merged variable scopes.
- `PAUSE [prompt]`: Blocks for user input.
- `ASSERT expr[, message]`: Raises error if expr is false.

## IDE preferences

Stored at `~/.time.warp/config.json`:

- theme: light|dark
- editor_font_size: int
- console_font_size: int
- geometry: last window geometry string

Apply/update via `TimeWarpApp._apply_settings`, save via `_save_settings`.

## Testing tips

- Use `run_templecode.py` for fast iteration; it reports interpreter errors without starting the GUI.
- For graphical tests, run the IDE and use the Examples menu. Add tiny scripts for new commands.
