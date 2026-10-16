# Time Warp Studio - AI Coding Agent Instructions

**Project:** Time Warp Studio - Educational multi-language programming environment  
**Maintainer:** James Temple <james@honey-badger.org>  
**Last Updated:** October 2026

---

## Project Overview

Time Warp Studio is an educational desktop programming environment built with **C++20 and Qt6** that provides a unified IDE for learning **10 programming languages** with integrated turtle graphics.

**Supported Languages:** BASIC, PILOT, Logo, COMAL, REXX, and Scheme.

**Current State:** The Qt desktop application in `native/` is the product. The
PySide6 implementation that came before it has been **removed**; what remains of
Python is a temporary engine the Qt app spawns (see "The language bridge").

## Architecture: The Big Picture

### Implementation

- **Qt application (C++20)** — the product, and the only maintained UI.
    - Entry point: `native/tools/time_warp_ide.cpp`
    - Build: `cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release && cmake --build native/build -j`
    - Shell: `native/src/ui/main_window.cpp`; canvas `native/src/ui/turtle_canvas.cpp`;
      editor `native/src/ui/code_editor.cpp`; languages `native/src/ui/language.cpp`
    - Engines that are already native: the expression evaluator (`timewarp_expr`)
      and Scheme (`timewarp_scheme`, an explicit-stack CEK machine).
- **The language bridge (Python, temporary)** — `native/bridge/`.
    - Five languages (BASIC, PILOT, Logo, COMAL, REXX) are *not* ported yet, so the
      IDE runs them through `native/bridge/worker.py`, which drives the retained
      Python engine and streams JSON lines back (drawing, output, input, errors).
    - Protocol: `run`/`in-reply`/`stop` in; `ready`/`out`/`canvas`/`pose`/`err`/`in`/`done` out.
    - It exists to be deleted: each language ported to C++ removes its bridge path.

**Critical Design Decision:** Language executors are stateless command processors returning text output. All UI state (turtle canvas, output display, themes) lives in the Qt application, not in an interpreter. Drawing crosses the bridge as commands on a `CanvasCommandQueue`, which the GUI thread drains into `TurtleCanvas`.

## Language Executor Pattern

Each language executor is a **function** (not a class) conforming to the Protocol in `languages/base.py`:

```python
def execute_my_lang(interpreter: Interpreter, source: str, turtle: TurtleState) -> str:
    """Execute source code, return output text with emoji prefixes."""
    ...
```

**Two execution modes:**
- **Whole-program executors** (1 active language): Receive the entire source as a string. Registered in `_WHOLE_PROGRAM_EXECUTORS` dict in `core/interpreter.py`. This is: Scheme.
- **Line-by-line executors** (5 active languages: BASIC, PILOT, Logo, COMAL, and REXX): The interpreter iterates lines and calls the executor per statement.

Whole-program executors are dispatched through `_WHOLE_PROGRAM_EXECUTORS` in `core/interpreter.py`; adding a language also requires the corresponding enum, extension, editor, and test updates.

## Critical Workflows

### Running the Desktop IDE

```bash
# Build once
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j

# Run it
./native/build/time_warp_ide                 # empty workspace
./native/build/time_warp_ide Examples/logo/mandala.logo
./native/build/time_warp_ide --run Examples/logo/mandala.logo
./native/build/time_warp_ide --help          # all options

# Or through the Makefile
make -C native run
```

**System Requirements**
- Qt6 (Widgets, Core, Gui), CMake 3.20+, a C++20 compiler
- Python 3.10+ **only** for the five bridged languages; Scheme and expressions
  run in-process with no Python at all

### Testing Strategy

```bash
# The Qt suites (11 of them)
ctest --test-dir native/build --output-on-failure
make -C native test-debug     # Debug tree
make -C native asan           # AddressSanitizer; ASAN=undefined for UBSan

# The whole example corpus, through the real bridge
python3 native/tools/example_smoke.py

# The bridge engine's own tests
PYTHONPATH=native/bridge python3 -m pytest -q

# Parity gates against the Python reference (0 unexpected mismatches)
python3 native/tools/differential_check.py native/build/expr_cli
cd native/tools && python3 scheme_differential_check.py
```

**Test Organization:**
- `native/tests/*_tests.cpp` = the 11 C++ suites (lexer, evaluator, scheme, canvas queue,
  canvas bridge, debug, debugger runner, ui layout, ui document, turtle canvas, python bridge)
- `native/tools/example_smoke.py` = every `Examples/**` program through the bridge; the 14
  that must draw are listed in `DRAWING_EXAMPLES`
- `native/bridge/time_warp/tests/` = the bridge engine's tests (the 6 language executors,
  the expression evaluator, validators)

### Adding a New Language

1. Create the executor. A language with no C++ implementation runs in the bridge:
   `native/bridge/time_warp/languages/my_lang.py`:
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

2. Register it in the bridge:
   - Import: `from ..languages.my_lang import execute_my_lang` in `native/bridge/time_warp/languages/__init__.py`
   - Add `Language.MY_LANG` in `native/bridge/time_warp/core/interpreter.py`
   - Add the extension in `Language.from_extension()` and in `core/language_registry.py`

3. Register it in the Qt side so the IDE knows the name and the extensions:
   `native/src/ui/language.cpp` (display name, settings key, file extensions)

   The display name is what the lesson documents and panels show, and it comes from
   `core/language_registry.py` on the Python side and `kLanguages` on the C++ side.
   They are two copies of one fact and must agree: the enum member spells CHIP-8 as
   `CHIP8`, the display name is `CHIP-8`, and using one where the other belongs is a
   wrong name in the one place it is written down.

4. Add an example under `Examples/my_lang/` and a row to
   `native/bridge/time_warp/tests/command_table.py` so every new command is covered
   by `native/tools/command_matrix.py`. A row's `language` string is the protocol key
   (`Language[key.upper()]`), so CHIP-8's rows read `CHIP8`; a row labelled with prose
   needs `also=(...)` naming the command the inventory knows, or it is reported as
   untested forever.

5. Add at least one lesson to `native/bridge/time_warp/features/lessons.json`
   (`native/tools/teach.py new <id> --language NAME` prints a skeleton the loader
   accepts) and run `teach.py validate` until it is silent. The lessons panel and
   `learn.py` both read that one file, so nothing else needs registering.

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

## Integration Points to Watch

### Turtle Graphics State
- Turtle state managed via `graphics/turtle_state.py` in the bridge, and by
  `TurtleCanvas` in the Qt application, which owns the display list
- Graphics rendered with the Qt painter with zoom/pan support in `native/src/ui/turtle_canvas.cpp`
- Canvas clearing happens in the Qt UI, not in an executor
- The reference's conventions are unusual and asserted in `turtle_canvas_tests`:
  heading 0 is **north** and increases clockwise; `forward` is
  `x += d*sin(h), y += d*cos(h)`; `circle` is a polygon walk

### Theme System
`native/src/ui/ui_style.cpp` owns the Qt styling. It does not yet carry the 33 themes
of the removed PySide6 implementation: that is tracked as pending work in
`docs/ROADMAP.md`. Do not describe themes as ported until they are.

## Common Pitfalls

1. **Executor statelessness** - Don't store UI refs in executors; return strings only.
2. **Test isolation** - Use fixtures for interpreter instances; don't share state.
3. **A program that runs and draws nothing is the bug users report.** Never assume a
   silent success is a success: `example_smoke.py` exists because 5 languages once ran
   "successfully" and produced no ink at all.
4. **Bridge path discovery is explicit, not clever.** `locateWorker()` tries the paths
   in `native/src/bridge/python_worker.cpp` in order; a moved worker that is not added
   there fails only at run time. `TIME_WARP_WORKER` overrides it for debugging.
5. **Deleting Python needs a dependency check that an import trace cannot give you.**
   The engine reaches `any(Features)` through imports *inside functions* (BASIC's
   SOUND/SAY/GAMEPAD, and the evaluator's TIMER), so a module can be invisible to a
   real run yet still load-bearing. Scan for imports below module level before cutting.

## File Structure at a Glance

```text
native/                        - the Qt application (the product)
  CMakeLists.txt
  include/ src/ tests/ tools/
  bridge/                      - TEMPORARY Python engine (delete as languages are ported)
    worker.py                  - the JSON-lines worker the IDE spawns
    pyproject.toml             - ruff/mypy config for the engine
    time_warp/
      core/                    - interpreter.py, language_registry.py, config.py, sandbox.py
      languages/               - the 10 executors (base.py documents the Protocol)
      features/                - lesson_system.py + lessons.json, classroom_mode.py
      graphics/turtle_state.py - turtle state
      features/                - language-level modules reached by name from a program
      utils/                   - expression_evaluator.py, error_hints.py, validators.py
      tests/                   - the engine's own tests
Examples/                      - 48 example programs across the 10 active languages
docs/                          - language guide + per-language tutorials
Scripts/                       - audit_interpreters.py, version_sync.py, build_native.sh
```

---

**When in doubt:** Read `core/interpreter.py` (main dispatch logic, look for `_WHOLE_PROGRAM_EXECUTORS`) and check `pytest --help` for test workflows.

### Key Components

- **Qt shell**: `native/src/ui/main_window.cpp` — documents, tabs, menus, status bar
- **TurtleCanvas**: `native/src/ui/turtle_canvas.cpp` — the display list and the painter
- **CodeEditor**: `native/src/ui/code_editor.cpp` — gutter, current line, tab stops
- **Language table**: `native/src/ui/language.cpp` — names, settings keys, extensions
- **The bridge**: `native/bridge/worker.py` + `native/src/bridge/python_worker.cpp` —
  runs programs *and* checks lessons, so a learner's verdict and a teacher's mark are
  one function's answer
- **Lessons**: `native/bridge/time_warp/features/lesson_system.py` (the curriculum and
  the verifier), `classroom_mode.py` (rosters, handouts, marking, bundles),
  `native/src/ui/panels.cpp` (`LessonPanel`, the one real page in the right column),
  `native/tools/learn.py` and `native/tools/teach.py` (the two CLIs)
- **Engine (bridge)**: `time_warp/core/interpreter.py` dispatches; the 6 executors in
  `time_warp/languages/` are stateless functions
- **Expression evaluator**: native C++ (`timewarp_expr`) and, in the bridge, the
  reference in `time_warp/utils/expression_evaluator.py` that gates it
- **Scheme**: native C++ explicit-stack machine (`timewarp_scheme`)

## Dependencies

The Qt application: Qt6 (Widgets, Core, Gui), CMake 3.20+, a C++20 compiler.

The bridge (only the five unported languages need it): Python 3.10+, standard
library only. Optional extras -- `pyttsx3` (BASIC `SAY`), `pygame` and `inputs`
(BASIC `SOUND`/`GAMEPAD`) -- are imported defensively and are not required.

The removed PySide6 application's dependencies are no longer installed or referenced.

### File Naming Conventions

- **Test files**: `test_*.py` for unit tests
- **Language demos**: `*.bas`, `*.logo`, `*.pilot`, `*.c`, `*.py`, etc. in `Examples/`
- **Language executors**: Named after the language in `time_warp/languages/`

### Configuration Management

- User settings stored via Qt QSettings; lesson progress in `~/.time_warp/progress.json`
  and a class in `~/.time_warp/classroom`. `TIME_WARP_DATA_DIR` moves all of it, which is
  how a test avoids writing a learner's real file
- Theme preferences persist between sessions via QSettings
- Virtual environment used for Python dependencies

### Error Handling Patterns

All interpreter errors are returned as strings with emoji prefixes:
- `❌` for errors
- `ℹ️` for info messages
- `🎨` for theme changes

### Theme Development

Themes are not ported yet. The removed implementation's 33 themes (including the
retro CRT sets: Amber, Green, C64, Apple II) are the target, to be carried as data
and applied through QSS plus a palette in `native/src/ui/ui_style.cpp`. See
`docs/ROADMAP.md`; do not describe a theme as available until it is.

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

## Code Style and Conventions

- Use descriptive docstrings for all classes and functions
- Error messages prefixed with emoji indicators (`❌`, `ℹ️`, `🎨`, `🚀`)
- Graceful degradation for optional dependencies
- Consistent Python formatting (PEP 8)
