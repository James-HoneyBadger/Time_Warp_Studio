# Time Warp Studio - AI Coding Agent Instructions

**Project:** Time Warp Studio - an old-school programming environment: eleven languages, an IDE, and two complete machines  
**Maintainer:** James Temple <james@honey-badger.org>  
**Last Updated:** October 2026

---

## Project Overview

Time Warp Studio is an old-school desktop programming environment built with **C++20 and Qt6** (Qt 5.15 for the Windows 7 build): a unified IDE for eleven interpreters from 1964 to 1980, with turtle graphics, a debugger in every language, and two complete machines — the HB-1 home computer and a live CHIP-8. A checked lesson curriculum ships for teaching, but the product is framed for retro-computing hobbyists first.

**Supported Languages:** BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX, Smalltalk and Forth.

**Current State:** The Qt desktop application in `native/` is the product, and all
eleven languages run in C++ inside it. The PySide6 implementation that came before it
has been **removed**, and so has the Python engine the ports were judged against:
the parity gate compares with a recorded corpus, and the harnesses drive
`time_warp_check` (see "The gates").

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
- **The workspace libraries** — `timewarp_project` (`.twproject` groups),
  `timewarp_sql` (one SQLite connection) and `timewarp_api` (JSON-RPC with no
  transport of its own). The IDE's Projects dock, Database tab and `./api` are
  views over these; `./twc` links the language engines into a standalone
  executable.
- **The frozen parity corpus** — `native/tests/golden/lang_corpus.json.gz`.
  - The Python reference engine was deleted at 18.0. The corpus is what remains of
    it: 470 programs, each with the program text, what it printed, every drawing
    call and the variables it left. `make golden-check` compares the ports with
    those answers and imports no Python to do it.
  - It is a weaker gate than the live differential it replaced and says so: a
    recording cannot notice a mistake in the recording, so a deliberate behaviour
    change edits the corpus entry in the same commit that changes the behaviour.
  - `time_warp_check` (`native/tools/check.cpp`) is what answers "what did this
    program do?" now, from the engines the product ships.

**Critical Design Decision:** Language executors are stateless command processors returning text output. All UI state (turtle canvas, output display, themes) lives in the Qt application, not in an interpreter. Drawing crosses the thread boundary as commands on a `CanvasCommandQueue`, which the GUI thread drains into `TurtleCanvas`.

## Language Executor Pattern

Each language executor is a **function**, not a class, and its contract is the one
`TurtleHost` documents in `native/include/timewarp/lang/turtle_host.hpp`:

```cpp
std::string execute_my_lang(Interpreter& interpreter, std::string_view source,
                            TurtleHost& turtle);
// Returns the text to display, with the emoji prefixes; never throws out of the
// executor, because every caller reads the returned text.
```

**Two execution modes:**

- **Whole-program executors** (five languages: Scheme, CHIP-8, Logo, Smalltalk and
  Forth): receive the entire source at once. `is_whole_program` in
  `native/src/lang/language.cpp` is the one list.
- **Line-by-line executors** (six languages: BASIC, APL, PILOT, FOCAL, COMAL and
  REXX): the interpreter iterates lines and calls the executor per statement.

`executor_for` in the same file answers with the executor for every language; adding
one also requires the enum, the extension, the IDE table and the tests.

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
./native/build/time_warp_ide --computer      # the HB-1 screen
./native/build/time_warp_ide --help          # all options

# Or through the Makefile
make -C native run
```

**System Requirements**

- Qt6 (Widgets, Core, Gui), CMake 3.20+, a C++20 compiler
- Qt 5.15 as well if you are building the Windows 7 artifact, which is the only
  reason the second Qt exists: `-DTIME_WARP_QT5=ON` selects it and
  `-DTIME_WARP_WIN7=ON` states the Windows 7 API level. Never `#if` on the Qt
  version at a call site — the two compatibility headers below are the seam.
- No interpreter of any kind. Every language runs in process, and the gates and
  documentation tools are C++ built from this tree; `make tree-check` fails if a
  `.py` comes back, or the config file of a Python tool (`pytest.ini` outlived the
  removal because that gate only matched the extension).

### Testing Strategy

```bash
# The Qt suites (24 of them)
ctest --test-dir native/build --output-on-failure
make -C native test-debug     # Debug tree
make -C native asan           # AddressSanitizer; ASAN=undefined for UBSan

# The whole example corpus, through the native engines
./native/build/time_warp_gates smoke
./native/build/time_warp_gates commands  # every command row as a program
./native/build/time_warp_gates repl      # every command row through the REPL
./run.sh --help                       # build if needed, then launch the IDE
./twc Examples/basic/hello.bas -o hello
./api                                 # JSON-RPC on stdin/stdout; docs/API.md

# The parity gate: the ports against the corpus recorded from the reference engine
# before it was deleted. 0 differences is the verdict.
make golden-check     # both corpora: 470 language programs and 192 Scheme ones

# The curriculum and the documentation gates
./native/build/time_warp_teach validate
make doc-links doc-claims
```

**Test Organization:**

- `native/tests/*_tests.cpp` = the C++ suites (lexer, evaluator, scheme, native
  languages, audio, canvas queue, canvas bridge, debug, debugger runner, lessons,
  classroom, ui layout, ui document, turtle canvas, syntax highlighter, media, editor
  services, help browser, chip8 emulator, retro computer, project, sql, api)
- `native/tools/gates/` = `time_warp_gates`, one binary with a subcommand per
  harness: `smoke` (every `Examples/**` program through the native engines, with the
  programs that must draw listed in `smoke.cpp`), `commands` (one row per command,
  from `native/share/command_rows.json`, plus the waivers for what cannot be
  exercised automatically), `repl` (the same rows through the API) and `golden` (the
  frozen corpora)

### Adding a New Language

The order is the native one first, because that is the only implementation there is:

1. Add the `Language` member (`native/include/timewarp/lang/language.hpp`), its key,
   its display name and its extensions in `native/src/lang/language.cpp`.

2. Implement the executor in `native/src/lang/` and register it in `executor_for`.
   A line-by-line language gets one `execute_x` per statement; a whole-program one
   gets the source and runs to completion. Either way it returns text with an emoji
   prefix and keeps no UI state.

3. Record what it does for the parity gate: add rows to
   `native/share/command_rows.json` and an example under `Examples/`, then run
   `make golden-check` — the corpus is
   recorded answers, so a new language's entries are added to
   `native/tests/golden/lang_corpus.json.gz` by hand, with the behaviour in the same
   commit.

4. Register it in the Qt side so the IDE knows its settings key and extensions:
   `native/src/ui/language.cpp` (`kLanguages`). The display name is what the lesson
   documents and panels show: the enum member spells CHIP-8 as `CHIP8`, the display
   name is `CHIP-8`, and using one where the other belongs is a wrong name in the one
   place it is written down.

5. Add an example under `Examples/my_lang/`<!-- doc-claims: ignore --> and a row to
   `native/share/command_rows.json` so every new command is covered by
   `time_warp_gates commands`. A row's `language` string is the protocol key
   (`Language[key.upper()]`), so CHIP-8's rows read `CHIP8`; a row labelled with prose
   needs `also=(...)` naming the command the inventory knows, or it is reported as
   untested forever.

6. Add at least one lesson to `native/share/lessons.json`
   (`./native/build/time_warp_teach new <id> --language NAME` prints a skeleton the loader
   accepts) and run `time_warp_teach validate` until it is silent. The lessons panel and
   `time_warp_learn` both read that one file, so nothing else needs registering.

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

There is no `eval()` anywhere in the tree. Arithmetic a language needs goes through
`timewarp_expr` (`native/src/token.cpp`, `lexer.cpp`, `evaluator.cpp`), reached from
an executor as `interpreter.evaluate_expression(...)` with the program's variables
supplied — the same evaluator every language shares, and the one the corpus pins.

## Integration Points to Watch

### Turtle Graphics State

- Turtle state is managed by
  `TurtleCanvas` in the Qt application, which owns the display list
- Graphics rendered with the Qt painter with zoom/pan support in `native/src/ui/turtle_canvas.cpp`
- Canvas clearing happens in the Qt UI, not in an executor
- The reference's conventions are unusual and asserted in `turtle_canvas_tests`:
  heading 0 is **north** and increases clockwise; `forward` is
  `x += d*sin(h), y += d*cos(h)`; `circle` is a polygon walk

### Theme System

`native/src/ui/ui_style.cpp` owns the Qt styling and `native/src/ui/themes.cpp` applies
the 35 themes carried as data in `native/src/ui/theme_table.inc` — the same schemes the
removed PySide6 implementation had. View > Theme selects one, View > Next Theme
(`Shift+F10`) cycles, and the choice persists in QSettings. High Contrast derives a
maximum-contrast variant of the active theme; it does not replace it.

## Common Pitfalls

1. **Executor statelessness** - Don't store UI refs in executors; return strings only.
2. **Test isolation** - Use fixtures for interpreter instances; don't share state.
3. **A program that runs and draws nothing is the bug users report.** Never assume a
   silent success is a success: `time_warp_gates smoke` exists because 5 languages
   once ran "successfully" and produced no ink at all.
4. **A path search that guesses fails at run time, not at build time.** That is why the
   launcher this build used to need was an *ordered* list rather than a clever search,
   and why `native/tools/gates/locate.cpp` is the single place the harnesses ask where
   `time_warp_check` is.
5. **Deleting Python needs a dependency check that an import trace cannot give you.**
   The engine reaches `any(Features)` through imports *inside functions* (BASIC's
   SOUND/SAY/GAMEPAD, and the evaluator's TIMER), so a module can be invisible to a
   real run yet still load-bearing. Scan for imports below module level before cutting.

## File Structure at a Glance

```text
native/                        - the Qt application (the product)
  CMakeLists.txt
  include/ src/ tests/ tools/
    lang/                      - the eleven executors and the shared loop
    scheme/                    - the explicit-stack Scheme machine
    lessons/                   - curriculum, verifier, progress, classroom
    debugger/ project/ sql/ api/ canvas/
  share/                       - lessons.json, snippets.json, command_rows.json
  tools/
    gates/                     - time_warp_gates: smoke, commands, repl, golden, tree
    docs/                      - time_warp_docs: links, claims, version (no Qt)
    icon/                      - time_warp_icon: the packaged icons (zlib only)
Examples/                      - 63 example programs across the 11 active languages
docs/                          - USER_GUIDE, API, language guide and tutorials
```

---

**When in doubt:** Read `native/src/lang/interpreter.cpp` (the dispatch, and
`is_whole_program`), and run `make help` for the checks this tree has. Nothing here
needs an interpreter: if a guide names a `python3` command it is out of date, and
`make tree-check` is the gate that says so.

### Key Components

- **Qt shell**: `native/src/ui/main_window.cpp` — documents, tabs, menus, status bar
- **TurtleCanvas**: `native/src/ui/turtle_canvas.cpp` — the display list and the painter
- **CodeEditor**: `native/src/ui/code_editor.cpp` — gutter, current line, tab stops,
  syntax colouring and the search that `FindBar` drives
- **Language table**: `native/src/ui/language.cpp` — names, settings keys, extensions
- **HB-1**: `native/src/ui/retro_computer.cpp` — a self-contained home
  computer whose ROM socket is the `Language` enum. It owns its own canvas queue,
  bridge, `ReplRunner` and `NativeRunner`, so nothing started there shares state with
  the editor's REPL bar; per-ROM programs, the ROM and the screen mode persist in
  QSettings. Its configuration screen (`retro_settings.cpp`) owns the boot ROM and
  boot screen, the start-up message, the prompt-history budget, program memory, the
  phosphor palette (`retroPalette`), the two text sizes and the CRT strengths, stored
  under `retroComputer/config` and written only when the dialog is accepted. Its
  disk is `hb_dos.{hpp,cpp}` (`HbDos`): one volume of named programs with a lock
  bit and 256-byte sector accounting, persisted as a JSON image under
  `~/.time_warp/disks`; the window's `CATALOG`/`SAVE`/`LOAD`/`RUN "name"`/`DELETE`/
  `RENAME`/`LOCK`/`UNLOCK`/`VERIFY`/`TYPE`/`STATUS`/`INIT` words are thin calls
  into it, and `INIT` must be said twice. Its manuals are `docs/hb1/`.
- **CHIP-8 machine**: `native/src/lang/chip8.cpp` (assembler + `Chip8Machine`,
  also the editor's whole-program executor) and `native/src/ui/chip8_*.cpp`
  (`Chip8Window`, the 60 Hz `Chip8Emulator`, the screen, the ROM library). Manuals:
  `docs/chip8/`. Both machines are introduced, and the design premise stated, in
  `docs/MACHINES.md` — the two machines are a centrepiece, not a demo.
- **Lessons**: `native/src/lessons/` (`timewarp_lessons`: the curriculum, the verifier,
  progress, and the classroom with rosters, handouts, marking and bundles),
  `native/src/ui/panels.cpp` (`LessonPanel`), and the two commands
  `native/tools/{learn,teach}.cpp`
- **The run report**: `native/tools/check.cpp` (`time_warp_check`) — one program through
  the native engines, with what it printed and what it drew. This is what the example
  and command harnesses drive, and what a person at a terminal can ask.
- **Inspection panels**: `native/src/ui/panels.cpp` — `VariablesPanel`,
  `TelemetryPanel`, `DebugPanel`. The native runners report both tables to the window
  before `finished`. Scheme has no executor in that path, so its tables come from
  `DebuggerRunner::snapshot()` — including the final snapshot taken when a run ends
  without pausing. `DebugPanel`'s buttons forward to whichever runner is paused:
  Scheme through `DebuggerRunner`, every other language through `NativeRunner` and the
  shared `DebugSession` (a statement, a Logo top-level statement, a Smalltalk message
  send, a CHIP-8 instruction). The terminal is a real opt-in `TerminalWidget`, and the
  outline dock is a real `OutlinePanel`; nothing in the window is a `PlaceholderPanel`.
- **Engine**: `timewarp_lang` dispatches through `executor_for`; the eleven executors are
  stateless functions returning text with an emoji prefix
- **Expression evaluator**: native C++ (`timewarp_expr`), gated by the recorded corpus
- **Scheme**: native C++ explicit-stack machine (`timewarp_scheme`)

## Dependencies

The Qt application: Qt6 (Widgets, Core, Gui), CMake 3.20+, a C++20 compiler.
`Svg` and `PrintSupport` are optional components: they enable SVG drawing export and
the two Print entries, and the build hides those features when they are absent.

Qt 5.15 is the second Qt, and it is not optional in the sense the others are: it
builds the Windows 7 artifact, because Qt 6 requires Windows 10 and 5.15 is the last
Qt that runs on 7 SP1. One tree serves both — every `Qt6::` reference in
`native/CMakeLists.txt` is `${TIME_WARP_QT}::` — and the names that moved between the
versions live in `native/include/timewarp/core/qt_compat.hpp` (`intLength`,
`fromUtf8`) and `qt_gui_compat.hpp` (`eventPosition`, `keySequence`). `native.yml`
builds and tests that configuration on every push.

The harness scripts in `native/tools/` (needed by the gates, not by a Run): Python
3.10+, standard library only. `make lint` wants ruff. Neither is needed to build or
run the application.

The removed PySide6 application's dependencies are no longer installed or referenced.

### File Naming Conventions

- **Test files**: `native/tests/*_tests.cpp`, one suite per area
- **Language demos**: `*.bas`, `*.logo`, `*.pilot`, `*.fs`, etc. in `Examples/`
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

The 35 themes — including the retro CRT sets (Amber, Green, C64, Apple II) — are
carried as data in `native/src/ui/theme_table.inc` and applied through
`native/src/ui/themes.cpp` plus the QSS in `native/src/ui/ui_style.cpp`. Add a theme
by adding its record to the table and, if it needs one, a high-contrast variant;
`ui_layout_tests` pins the count and the required colour roles.

## Critical Integration Points

- Keep communication concise and focused.
- Follow development best practices.

### Interpreter-UI Communication

- Commands executed through `Interpreter.execute()` method
- Results displayed via Qt widgets and canvas
- Error handling centralized through status messages
- Input prompts handled through Qt input widgets

### Turtle Graphics Integration

- Turtle state is `TurtlePose`/`TurtleHost` in `native/include/timewarp/lang/turtle_host.hpp`;
  the reference's 16-colour palette is `reference_palette_colour()` there, the one table
  every host and BASIC use
- Graphics rendered using Qt painter with zoom/pan support in `native/src/ui/turtle_canvas.cpp`
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

## Documentation map

Each fact has one home; the other guides link to it rather than restating it.

| Book | Owns |
| --- | --- |
| `docs/MACHINES.md` | The two machines as a set, and the parallel-universe premise item by item |
| `docs/hb1/` | The HB-1: owner's manual, ROM library, HB DOS reference, CONFIG reference, engineering companion |
| `docs/chip8/` | The CHIP-8: machine manual, assembler, instruction set |
| `docs/USER_GUIDE.md` | The IDE: projects, the REPL bar, debugging, SQL, terminal, automation, appearance |
| `docs/LANGUAGE_GUIDE.md`, `docs/tutorials/` | Every command, and how to learn each language |
| `docs/lessons/` | The curriculum, and the student and teacher guides |
| `docs/ARCHITECTURE.md`, `native/README.md` | How it is built and why |
| `docs/API.md` | The JSON-RPC automation surface |

`README.md` is the front door and carries the capability inventory that links into
all of the above. `.github/workflows/docs.yml` gates the set: every promised file
must exist, every lesson plan must match the curriculum, and (with
`time_warp_docs links`) every relative link and anchor must resolve.

## GitHub surfaces

`.github/` is documentation as much as configuration, because GitHub renders it as
UI: `ISSUE_TEMPLATE/{config,bug_report,feature_request,language_request}.yml`,
`pull_request_template.md`, `SUPPORT.md`, `CODEOWNERS`, `DEPLOYMENT.md` (what each
workflow checks, and why) and `RELEASE_CHECKLIST.md`. A change to what the project
promises belongs in those files as much as in `docs/`.
