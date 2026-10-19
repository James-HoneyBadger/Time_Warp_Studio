# Time Warp Studio - AI Coding Agent Instructions

**Project:** Time Warp Studio - Educational multi-language programming environment  
**Maintainer:** James Temple <james@honey-badger.org>  
**Last Updated:** October 2026

---

## Project Overview

Time Warp Studio is an educational desktop programming environment built with **C++20 and Qt6** that provides a unified IDE for learning **10 programming languages** with integrated turtle graphics.

**Supported Languages:** BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX, and Smalltalk.

**Current State:** The Qt desktop application in `native/` is the product, and all
ten languages run in C++ inside it. The PySide6 implementation that came before
it has been **removed**; the Python engine that remains (`native/bridge/`) is the
reference the parity gates, the command matrix and the lesson check run in (see
"The language bridge").

## Architecture: The Big Picture

### Implementation

- **Qt application (C++20)** — the product, and the only maintained UI.
  - Entry point: `native/tools/time_warp_ide.cpp`
  - Build: `cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release && cmake --build native/build -j`
  - Shell: `native/src/ui/main_window.cpp`; canvas `native/src/ui/turtle_canvas.cpp`;
    editor `native/src/ui/code_editor.cpp`; languages `native/src/ui/language.cpp`
  - Every language is native: Scheme (`timewarp_scheme`, an explicit-stack CEK
    machine, dispatched in-process) and CHIP-8, Logo and Smalltalk
    (whole-program) plus BASIC, FOCAL, PILOT, COMAL, APL and REXX (line-by-line)
    on `timewarp_lang`, the shared interpreter loop plus one executor each, and
    the expression evaluator (`timewarp_expr`, linked into `timewarp_lang` and
    used by every arithmetic evaluator the executors share).
- **The Python reference engine** — `native/bridge/`.
  - No program is routed to it at run time: `executor_for()` answers for all ten.
    It is kept because the differential gates compare each port against it, the
    command matrix and example smoke drive it, and the Lessons panel asks
    `native/bridge/worker.py` to check a learner's program, so the panel's
    verdict and `teach.py`'s are one function's answer.
  - Protocol: `run`/`in-reply`/`stop` in; `ready`/`out`/`canvas`/`pose`/`err`/`in`/`done` out.
  - It is the part that goes away once lesson checking runs natively.

**Critical Design Decision:** Language executors are stateless command processors returning text output. All UI state (turtle canvas, output display, themes) lives in the Qt application, not in an interpreter. Drawing crosses the thread boundary as commands on a `CanvasCommandQueue`, which the GUI thread drains into `TurtleCanvas`.

## Language Executor Pattern

Each language executor is a **function** (not a class) conforming to the Protocol in `languages/base.py`:

```python
def execute_my_lang(interpreter: Interpreter, source: str, turtle: TurtleState) -> str:
    """Execute source code, return output text with emoji prefixes."""
    ...
```

**Two execution modes:**

- **Whole-program executors** (3 active languages: Scheme, CHIP-8 and Smalltalk): Receive the entire source as a string. Registered in `_WHOLE_PROGRAM_EXECUTORS` dict in `core/interpreter.py`.
- **Line-by-line executors** (7 active languages: BASIC, APL, Logo, PILOT, FOCAL, COMAL and REXX): The interpreter iterates lines and calls the executor per statement.

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
./native/build/time_warp_ide --run Examples/logo/mandala.logo --export mandala.png
./native/build/time_warp_ide --help          # all options

# Or through the Makefile
make -C native run
```

**System Requirements**

- Qt6 (Widgets, Core, Gui), CMake 3.20+, a C++20 compiler
- Python 3.10+ **only** for the reference engine the gates and the lesson check
  use; every language runs in-process with no Python at all

### Testing Strategy

```bash
# The Qt suites (17 of them)
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
python3 native/tools/lang_differential_check.py   # all nine bridged-once languages, 472 programs

# The documentation gates, which .github/workflows/docs.yml runs on every docs
# change; the teach.py write-plans output is then checked with git diff --exit-code
python3 native/tools/teach.py validate
make lint-md
```

**Test Organization:**

- `native/tests/*_tests.cpp` = the 17 C++ suites (lexer, evaluator, scheme, native
  languages, canvas queue, canvas bridge, debug, debugger runner, ui layout, ui
  document, turtle canvas, syntax highlighter, python bridge, media, editor
  services, help browser, chip8 emulator)
- `native/tools/example_smoke.py` = every `Examples/**` program through the reference
  engine; the 19 that must draw are listed in `DRAWING_EXAMPLES`
- `native/bridge/time_warp/tests/` = the bridge engine's tests (the 10 language executors,
  the expression evaluator, validators)

### Adding a New Language

1. Create the reference executor first, because the parity gate compares against
   it: `native/bridge/time_warp/languages/my_lang.py`:

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
  bridge/                      - the Python reference engine (gates, lesson check)
    worker.py                  - the JSON-lines worker the IDE spawns for a lesson check
    pyproject.toml             - ruff/mypy config for the engine
    time_warp/
      core/                    - interpreter.py, language_registry.py, config.py, sandbox.py
      languages/               - the 10 executors (base.py documents the Protocol)
      features/                - lesson_system.py + lessons.json, classroom_mode.py,
                                 and the language-level modules a program reaches
                                 by name (music, speech, shapes, particles,
                                 fractals, gamepad, game_support)
      graphics/turtle_state.py - turtle state
      utils/                   - expression_evaluator.py, error_hints.py, validators.py
      tests/                   - the engine's own tests
Examples/                      - 49 example programs across the 10 active languages
docs/                          - language guide + per-language tutorials
Scripts/                       - audit_interpreters.py, generate_icon.py,
                                 version_sync.py
```

---

**When in doubt:** Read `core/interpreter.py` (main dispatch logic, look for `_WHOLE_PROGRAM_EXECUTORS`) and check `pytest --help` for test workflows.

### Key Components

- **Qt shell**: `native/src/ui/main_window.cpp` — documents, tabs, menus, status bar
- **TurtleCanvas**: `native/src/ui/turtle_canvas.cpp` — the display list and the painter
- **CodeEditor**: `native/src/ui/code_editor.cpp` — gutter, current line, tab stops,
  syntax colouring and the search that `FindBar` drives
- **Language table**: `native/src/ui/language.cpp` — names, settings keys, extensions
- **The bridge**: `native/bridge/worker.py` + `native/src/bridge/python_worker.cpp` —
  runs programs *and* checks lessons, so a learner's verdict and a teacher's mark are
  one function's answer
- **Lessons**: `native/bridge/time_warp/features/lesson_system.py` (the curriculum and
  the verifier), `classroom_mode.py` (rosters, handouts, marking, bundles),
  `native/src/ui/panels.cpp` (`LessonPanel`), `native/tools/learn.py` and
  `native/tools/teach.py` (the two CLIs)
- **Inspection panels**: `native/src/ui/panels.cpp` — `VariablesPanel`,
  `TelemetryPanel`, `DebugPanel`. The bridge sends both tables in its `done` message
  (`variables` and `telemetry_rows`, both built by `time_warp/utils/inspection.py`);
  `PythonWorker` re-emits them as `variablesReported`/`telemetryReported` *before*
  `finished`. Scheme has no bridge payload, so its tables come from
  `DebuggerRunner::snapshot()` — including the final snapshot taken when a run ends
  without pausing. `DebugPanel`'s buttons all forward to the current runner and
  are disabled for every language but Scheme, because no other engine has a pause
  protocol. Only the terminal tab and the outline dock are still
  `PlaceholderPanel`s
- **Engine (reference)**: `time_warp/core/interpreter.py` dispatches; the 10
  executors in `time_warp/languages/` are stateless functions
- **Expression evaluator**: native C++ (`timewarp_expr`) and, in the bridge, the
  reference in `time_warp/utils/expression_evaluator.py` that gates it
- **Scheme**: native C++ explicit-stack machine (`timewarp_scheme`)

## Dependencies

The Qt application: Qt6 (Widgets, Core, Gui), CMake 3.20+, a C++20 compiler.

The Python reference engine (needed by the parity gates, the command matrix and
the lesson check, not by a Run): Python 3.10+, standard library only. Optional
extras -- `pyttsx3` (the reference's BASIC `SAY` backend), `pygame` and `inputs`
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

## GitHub surfaces

`.github/` is documentation as much as configuration, because GitHub renders it as
UI: `ISSUE_TEMPLATE/{config,bug_report,feature_request,language_request}.yml`,
`pull_request_template.md`, `SUPPORT.md`, `CODEOWNERS`, `DEPLOYMENT.md` (what each
workflow checks, and why) and `RELEASE_CHECKLIST.md`. A change to what the project
promises belongs in those files as much as in `docs/`.
