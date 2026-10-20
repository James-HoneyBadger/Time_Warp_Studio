# Time Warp Studio

## Educational Multi-Language Programming Environment with Integrated IDE, Debugger, and Turtle Graphics

[![C++20](https://img.shields.io/badge/C%2B%2B-20-blue)](https://en.cppreference.com/)
[![Qt6](https://img.shields.io/badge/Qt-6-green)](https://www.qt.io/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-16.2.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a unified desktop programming environment designed for educators and students. It brings together **10 active programming languages** into a single, modern IDE. Built as a **native C++20 / Qt6 application**, it provides an integrated platform for learning programming concepts, exploring turtle graphics, and working through a **checked lesson curriculum** with tools for both learners and teachers.

And it is a time machine. Inside it are two complete machines — the **HB-1**, a 1982 home computer from a parallel universe, and the **CHIP-8**, the 1977 virtual machine that made a computer out of four kilobytes. They are not themes or demonstrations: you can boot them, type at them, debug them, save programs to their disks and meet them again tomorrow.

---

## Two machines, one Studio

| | **HB-1** | **CHIP-8** |
| --- | --- | --- |
| What it is | A complete home computer: CRT, prompt, ten-language ROM socket, real editor, turtle canvas, disk | A complete virtual machine: 4 KB of memory, sixteen registers, a 64×32 screen, sixteen keys |
| Its universe | 1982, where one machine shipped ten language ROMs | 1977, where the machine was written in software and ran everywhere |
| Storage | **HB DOS 1.0** — 512 sectors of 256 bytes, a catalogue, lock bits, a two-step `INIT` | Your own folder of listings and ROMs, browsed by a library dialog |
| Run it | Tools > **HB-1 Computer…** (`Ctrl+Alt+W`) or `./run.sh --computer` | Tools > **CHIP-8 Emulator…** (`Ctrl+Shift+M`) |
| Read it | [Owner's Manual](docs/hb1/README.md) · [ROMs](docs/hb1/roms.md) · [HB DOS](docs/hb1/hb-dos.md) · [CONFIG](docs/hb1/config.md) · [Inside](docs/hb1/inside.md) | [Machine Manual](docs/chip8/README.md) · [Assembler](docs/chip8/assembly.md) · [Instruction set](docs/chip8/instruction-set.md) |

The premise is a single sentence: a system from a parallel universe, where the
design sense of the next forty years arrived in time for the 8-bit boom. The
constraints are period — a prompt, a sector-counted disk, 64K, a CRT with
phosphor — and the engines behind them are modern, in-process and kind. Nothing
about the machines is decorative: every switch, word and opcode in their manuals
does something you can watch.

Start with [the machines](docs/MACHINES.md): why two, what they share, and the
timey-wimey parts listed one by one.

---

## Install it

You do not need a compiler. Every [release](https://github.com/James-HoneyBadger/Time_Warp_Studio/releases/latest)
carries a ready-built application for **x86_64** (Intel/AMD) and **arm64**
(Raspberry Pi 4/5, ARM laptops).

| You have | Take | Then |
| --- | --- | --- |
| no wish to install anything | `Time_Warp_Studio-<version>-x86_64.AppImage` | Make it executable and double-click it. No root, no package manager, no Qt to fetch. |
| Ubuntu, Debian, Mint or Pop!_OS | `time-warp-studio_<version>_amd64.deb`, or `_arm64.deb` | Double-click it, or `sudo apt install ./time-warp-studio_<version>_amd64.deb` |
| Fedora, RHEL or openSUSE | `time-warp-studio-<version>-1.x86_64.rpm`, or `.aarch64.rpm` | `sudo dnf install ./time-warp-studio-<version>-1.x86_64.rpm` |
| any other Linux, or arm64 | `time-warp-studio-<version>-linux-<arch>.tar.gz` | `tar -xzf … -C / --strip-components=1` |

`uname -m` says which architecture yours is: `x86_64` or `aarch64`. Omit the
architecture from the file name above and the release assets fill it in. The full
detail, including requirements and the container image, is in
[docs/INSTALLATION.md](docs/INSTALLATION.md); building from source is still
supported and is what `native/README.md` describes.

---

## Run from a checkout

```bash
./run.sh                                          # build if needed, then launch
./run.sh Examples/logo/mandala.logo
./run.sh --compile Examples/basic/hello.bas --output hello
./run.sh --computer                              # HB-1 home computer
./twc Examples/scheme/hello.scm -o hello-scheme   # small standalone executable
./api                                             # JSON-RPC on stdin/stdout
```

`run.sh` builds incrementally and forwards every argument to the IDE, including
`--run`, `--screenshot`, `--export`, `--compile` and `--computer`. `twc` and `api` are the
terminal compiler and the automation service; see [docs/USER_GUIDE.md](docs/USER_GUIDE.md).

---

## What the Studio can do

Everything in this section is implemented, tested and documented; the links are
manuals, not promises. The full inventory, feature by feature:

| Capability | What it is | Read |
| --- | --- | --- |
| **Ten languages, one editor** | BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX and Smalltalk, each running on a native engine in process | [LANGUAGE_GUIDE](docs/LANGUAGE_GUIDE.md), [tutorials](docs/tutorials/) |
| **Editor and console** | Gutter, current line, syntax colouring, find and replace, **find in files**, **go to line**, **snippets**, **outline**, **format code**, **live lint**, **autosave**, **version history**, two panes | [USER_GUIDE](docs/USER_GUIDE.md#1-start-and-stop) |
| **Turtle canvas** | Live drawing while a program runs, zoom, pan, grid, auto-fit, turtle marker, **scrubber playback**, **animated GIF recording**, PNG/JPEG/BMP/SVG export | [USER_GUIDE](docs/USER_GUIDE.md#1-start-and-stop) |
| **Debugging, in all ten** | Gutter breakpoints, Continue/Pause/Step In/Over/Out, **watch expressions**, **data breakpoints**, a Variables panel and a Telemetry HUD | [USER_GUIDE](docs/USER_GUIDE.md#5-debugging) |
| **The HB-1** | A 1982 home computer in a parallel universe: ten-language ROM socket, CRT, prompt, HB DOS disk, CONFIG with four phosphors | [HB-1 manuals](docs/hb1/README.md) |
| **The CHIP-8** | The 1977 virtual machine: live 60 Hz machine, assembler, keypad, register inspector, ROM library | [CHIP-8 manuals](docs/chip8/README.md) |
| **Projects** | `.twproject` groups that organise programs without moving them on disk | [USER_GUIDE](docs/USER_GUIDE.md#2-projects-and-groups) |
| **SQL** | A SQLite Database tab, plus `SQL` statements in nine languages and through the API | [USER_GUIDE](docs/USER_GUIDE.md#6-sql-and-the-database-tab) |
| **Lessons and classroom** | 20 lessons, 60 checkpoints, checked by running the program; rosters, handouts, marking, reports | [lessons/](docs/lessons/README.md) |
| **Automation** | JSON-RPC over stdin/stdout, a standalone `twc` compiler, and scriptable screenshots and exports | [API](docs/API.md), [USER_GUIDE](docs/USER_GUIDE.md#7-terminal-tools) |
| **Presentation and access** | 33 themes, high contrast, 100/125/150% text, focus mode, CRT settings and four monitor presets, presentation mode | [USER_GUIDE](docs/USER_GUIDE.md#9-lessons-classroom-and-appearance) |
| **Help that covers the whole thing** | `F1` context help, `Ctrl+F1` keyword inspector, the documentation browser, a guided tour and a shortcut reference | [Help](#help-from-inside-the-studio) |

### Multi-Language Support

10 active languages in the current studio build:

| Language | Paradigm | Era | Execution Mode |
| --- | --- | --- | --- |
| **BASIC** | Imperative / Educational | 1964 | Line-by-line |
| **APL** | Array / Notation as tool | 1966 | Line-by-line |
| **Logo** | Turtle Graphics / Educational | 1967 | Whole-program |
| **PILOT** | Computer-Aided Instruction | 1969 | Line-by-line |
| **FOCAL** | Formula calculator / Teaching | 1969 | Line-by-line |
| **Scheme** | Functional / Lisp dialect | 1975 | Whole-program |
| **CHIP-8** | Bytecode machine / Assembly | 1977 | Whole-program |
| **COMAL** | Structured BASIC / Educational | 1979 | Line-by-line |
| **REXX** | Scripting / String processing | 1979 | Line-by-line |
| **Smalltalk** | Object-oriented / Messages all the way down | 1980 | Whole-program |

### Integrated IDE

- **Code editor** — line-number gutter, current-line highlight, 4-space tab stops, per-language file handling, per-language syntax colouring (comments, strings, numbers, keywords), and find/replace with go-to-line (F3 for the next match, Ctrl+L for a line number)
- **Output console** — program output as it is produced, with interpreter errors in their own tab
- **Turtle canvas** — the drawing appears as it is made, with zoom, pan, grid, auto-fit and a turtle marker; export it to PNG/JPEG/BMP (or SVG when the build has Qt6 Svg) with Ctrl+E, or copy it to the clipboard, with or without the grid on screen. A **scrubber** replays the drawing stroke by stroke, and **Record Canvas as GIF…** captures it while it happens (still frames merge into one, so an idle minute is a pause, not a hundred pictures)
- **Printing and export** — File > Print Code (Ctrl+P) sends the program to a printer as the same A4 page the PDF export writes, and File > Print Drawing sends the drawing scaled to fit the page; both entries appear only in a build with Qt6 PrintSupport. The program also exports as PDF, and the program, output and drawing as one HTML page; a lesson session exports as Markdown or PDF
- **Profiling** — View > Profile Run times every statement of a line-by-line program and ends with a report of line, hits, time and share, longest first. It is off at every launch because timing costs
- **Terminal (opt-in)** — Settings > Workspace adds a 💻 Terminal tab that runs commands through your own shell, starting in the current document's folder. It is off by default, stopped and removed in presentation and player modes, and is a command line rather than a terminal emulator: no prompts, no job control, no full-screen programs
- **Documents** — tabs, per-file language detection, unsaved-change prompts, new/open/save/save-as, a recent-files menu, and the files from the last session reopened at launch
- **Find and navigate** — find, find and replace (`Ctrl+F`, `Ctrl+H`), next/previous (`F3`, `Shift+F3`), go to line (`Ctrl+L`), and **Find in Files** (`Ctrl+Shift+F`) across a folder or project
- **Command palette** — `Ctrl+Shift+P` or `F10` lists every action in the window, so a feature you cannot find in a menu is three letters away
- **Snippets** — `Ctrl+Shift+I` inserts a language-aware example from a catalogue the build installs (`share/time-warp-studio/snippets.json`; `TIME_WARP_SNIPPETS` points at your own)
- **Outline** — a dock listing every named place in the current program — procedures, labels, functions, methods — as a jump list
- **Format and lint** — `Ctrl+I` re-indents the document for its language, preserving line endings and blank lines, and the editor's lint runs as you type (Settings > Editor turns either off)
- **Autosave and version history** — snapshots on a timer you set, and **File > Version History…** (`Ctrl+Alt+V`) to read or restore any of them, including buffers that were never saved
- **Two panes** — **Split Editor Right** (`Ctrl+\`) shows two documents side by side; **Run Both Panes** (`Ctrl+Alt+B`) runs both headless and reports time, turtle calls and each program's own output in one table — a comparison, not a race
- **Run controls** — Run (F9) and Stop, with a status bar showing language, cursor position and execution state
- **Presentation and access** — 33 themes including the retro CRT sets, a High Contrast variant of whichever is active, 100/125/150% text sizes, focus mode, CRT settings with four monitor presets, and **Presentation Mode** (`Shift+F11`, `Esc` to leave) for teaching from the front
- **Inspection** — a 🔍 **Variables** panel listing every binding with its type and value, filtered by name, and a ⚡ **Telemetry** HUD showing what the run cost and did: steps used, engine time, steps per second, last line, recursion depth, call stack, output lines, drawing commands, BASIC's memory window, and — beside the engine's own counters — the IDE's wall clock and the ink that actually reached the canvas
- **Debugging** — gutter breakpoints and a 🐞 **Debug** panel (Continue, Pause, Step In/Over/Out, Stop) plus Debug/Continue/Trace on the toolbar, working in all ten languages: at a statement, a Logo top-level statement, a Smalltalk message send or a CHIP-8 instruction. Scheme additionally shows the call stack and the paused frame's bindings; for a native run the Variables panel shows the bindings the pause stopped in. **Watch expressions** evaluate arithmetic over the paused bindings, and **data breakpoints** stop when a named variable changes value
- **Projects** — a 🗂 **Projects** dock groups programs into named groups without moving them on disk. Create or open a `.twproject`, add files, a whole folder (its programs are grouped by subfolder), and groups, drag a file between groups, and the project reopens with the session; missing files are marked rather than forgotten
- **HB-1 computer** — Tools > **HB-1 Computer…** opens a self-contained home computer: a boot intro and prompt, a selectable language ROM for all ten languages, TEXT/SPLIT/GRAPHICS modes sharing one CRT, and a PROGRAM editor beside the turtle canvas. Each ROM keeps its own program, and the machine persists its ROM, mode and memory between sessions. **CONFIG** changes its boot ROM and screen, start-up message, phosphor (green, amber, white or ice), text sizes and CRT glass, and **HB DOS** gives it a disk of named programs with `CATALOG`, `SAVE`, `LOAD`, `RUN "name"`, `DELETE`, `RENAME`, `LOCK`, `VERIFY` and `INIT`, catalogued in the manner of Apple DOS 3.3. It is also `./run.sh --computer`
- **CHIP-8 machine** — Tools > **CHIP-8 Emulator…** (`Ctrl+Shift+M`) is the other machine: a live 60 Hz CHIP-8 with a 64×32 screen (128×64 in SuperChip hi-res), a clickable keypad mirrored on the PC keyboard, a register/memory/listing inspector, single-step, adjustable speed, sound and a ROM library that browses a folder you own. It assembles listings as well as running binary ROMs, and the same programs run in the editor's whole-program mode — see [the CHIP-8 manuals](docs/chip8/README.md)
- **REPL bar** — the line under the editor runs a command in the current document's language. BASIC, FOCAL, PILOT, COMAL, REXX, APL, Logo and Scheme keep bindings between commands (`LET X = 5` then `PRINT X`); Scheme also keeps definitions and echoes expression values. In BASIC, `?` is shorthand for `PRINT`, a numbered line is stored in the editor as a program line, and `RUN`, `LIST` and `NEW` work as direct-mode commands. Smalltalk and CHIP-8 run each submission as a whole program, so their state resets
- **SQL database** — a 🗄 **Database** tab opens or creates a SQLite file, lists its tables, runs scripts and shows the result grid. A program reaches the same connection with `SQL SELECT ...` (BASIC, FOCAL, PILOT, COMAL, REXX and APL), `SQL ...` in Logo, `(sql "...")` in Scheme, and `SQL run: '...'` in Smalltalk
- **Automation** — `./api` speaks the JSON-RPC API on stdin/stdout: run a program, keep a REPL session, start and step a debug session, query SQL, edit a project including folder import, and evaluate watches. See [docs/API.md](docs/API.md)
- **Terminal tools** — `./run.sh` builds if needed and launches the IDE; `./twc program.bas -o program` compiles a program into a standalone executable that embeds the interpreter, writes its drawing as SVG, and takes `--db FILE` when the program uses SQL
- **Native engines** — every language runs in-process in C++ with no Python at
  all. Scheme (`timewarp_scheme`) and CHIP-8, Logo and Smalltalk are
  whole-program; BASIC, FOCAL, PILOT, COMAL, APL and REXX are line-by-line on
  the shared `timewarp_lang` engine. The arithmetic each one evaluates goes
  through the native expression evaluator (`timewarp_expr`), and the drawing
  through the same canvas command queue the IDE drains (see
  [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md))

### What is still being built

The Qt IDE is the product; the PySide6 application that preceded it has been removed,
and the port is complete: every phase of
[PORTING_PLAN.md](docs/PORTING_PLAN.md) is implemented and has a test. What remains
unbuilt is a shorter list — a dedicated player target, an automatic update check,
and Flatpak/Snap or Windows/macOS packaging. It is tracked in
[docs/ROADMAP.md](docs/ROADMAP.md); please read that list before reporting a missing
feature as a regression.

### Turtle Graphics

- Native turtle support with position, heading, and pen state tracking
- 50+ turtle and drawing commands across the active languages
- Real-time rendering with the Qt painter, zoom/pan controls and a content-fitted view
- Shapes, text labels, thick strokes, pen up/down and filled polygons
- Cross-language graphics support (Logo, BASIC, COMAL, REXX, Scheme, FOCAL, APL
  and Smalltalk; PILOT draws with its `G:` commands and CHIP-8 paints rectangles)
- The reference's conventions are unusual and are asserted in tests: heading 0 is north and increases clockwise, and `circle` is a polygon walk

### Debugging

Scheme runs on a native explicit-stack machine with a background runner, and the 🐞
Debug panel drives it: Continue, Pause, Step In/Over/Out, Stop and breakpoints, with the
call stack and that frame's bindings shown while paused. Values are typed by the machine
rather than guessed from their printed form. Every language can stop: the line-loop
languages at a statement, Logo at a top-level statement, Smalltalk at a message send
and CHIP-8 before an instruction, with gutter breakpoints and Continue/Trace on the
toolbar. The call stack and frame bindings are Scheme's own; for a native run the
Variables panel shows the bindings the pause stopped in.

### Guided Lessons

- **20 lessons, 60 checkpoints** — two lessons for each of the ten languages
- **Checked by running the program**, never by comparing it with an answer, so `PRINT 6 * 7` and `PRINT 42` are the same answer and a drawing checkpoint cannot be passed by printing about drawing
- **In the IDE** — the 🎓 **Lessons** panel: the curriculum, the current checkpoint, hints, and a Check button that runs the program in your document
- **At a terminal** — `python3 native/tools/learn.py walk basic_first_steps`
- **For teachers** — `python3 native/tools/teach.py`: lesson plans with objectives and teacher notes, answer keys, a class roster, handout distribution, marking, class progress reports, bundles a class can be moved in, and a presentation size for teaching from the front
- **The curriculum checks itself** — `teach.py validate` runs all sixty solutions through the engine and refuses a starter that already works, so a lesson nobody can finish cannot sit in the tree looking correct
- Progress is saved per lesson and hints are counted; neither a reset nor a marking pass can lose a learner's work

See [docs/lessons/](docs/lessons/README.md) for the guides.

### Help from inside the Studio

The application documents itself, from the thing you are looking at:

| Key / menu | What it does |
| --- | --- |
| `F1` | **Context Help** — the manual section for the panel, language or control with focus |
| `Ctrl+F1` | **Keyword Inspector** — the command reference entry for a word under the cursor |
| Help > **Documentation** | The documentation browser: every guide in `docs/`, indexed from [INDEX.md](docs/INDEX.md), including the [HB-1](docs/hb1/README.md) and [CHIP-8](docs/chip8/README.md) manuals |
| Help > **Getting Started** | The first-run path through the Studio |
| `F12` | The **Guided Tour**, a one-time walk through the window with a card on each part |
| Help > **Keyboard Shortcuts** | Every shortcut, in one dialog, generated from the actions themselves |
| `Ctrl+Shift+P` or `F10` | The **Command Palette** — every action by name, whether or not it is in a menu |

The browser reads the same Markdown files this repository ships, so a teacher's
copy of the manuals and the application's own help are one set of documents.

### Educational Content

- **60 curated example programs** across all 10 active languages, under
  `Examples/`, including a **showcase** for each language that walks its whole
  surface and ends in a drawing
- **Error hints** — syntax suggestions alongside interpreter error messages
- **Ten language tutorials** — one per language, under `docs/tutorials/`
- **Lesson plans** — one document per language, generated from the curriculum, under `docs/lessons/plans/`

---

## About this repository

The description and topics below are the canonical ones for this project. GitHub's
**About** box is set in the repository settings rather than in a file, so this is the
source to copy from when it changes — and the place to check when it looks wrong.

### Description

```text
Educational multi-language programming environment: a native C++20/Qt6 IDE with ten
languages (BASIC, Logo, PILOT, COMAL, REXX, Scheme, FOCAL, APL, CHIP-8, Smalltalk),
turtle graphics, projects, an HB-1 home-computer screen, a SQLite-backed SQL workspace, a checked lesson
curriculum with student and teacher tools, a debugger, and a JSON-RPC automation
API.
```

### Topics

```text
education
programming-education
computer-science-education
ide
qt6
cpp20
interpreters
turtle-graphics
sqlite
json-rpc
projects
debugger
basic
logo
pilot
comal
rexx
scheme
focal
apl
chip8
smalltalk
lesson-plans
teaching-tools
```

### Where to start

| If you are | Start here |
| ---------- | ---------- |
| Meeting the machines | [the HB-1](docs/hb1/README.md) and [the CHIP-8](docs/chip8/README.md), introduced in [MACHINES.md](docs/MACHINES.md) |
| Learning one of the languages | [`docs/tutorials/`](docs/tutorials/) |
| Use projects, the REPL, SQL, debugging or the API | [`docs/USER_GUIDE.md`](docs/USER_GUIDE.md) |
| Working through the lessons | [`docs/lessons/README.md`](docs/lessons/README.md) |
| Teaching a class | [`docs/lessons/teacher-guide.md`](docs/lessons/teacher-guide.md) |
| Building or porting | [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) and [`native/README.md`](native/README.md) |
| Seeing what is not done yet | [`docs/ROADMAP.md`](docs/ROADMAP.md) |
| Reporting a bug or asking for a feature | The issue forms in [`.github/ISSUE_TEMPLATE/`](.github/ISSUE_TEMPLATE/) |

---

## Working with the project

- **[CONTRIBUTING.md](CONTRIBUTING.md)** — setup, the gates, and recipes for a
  language, a command and a lesson
- **[.github/SUPPORT.md](.github/SUPPORT.md)** — where to ask, and what is
  supported on which platform
- **[.github/pull_request_template.md](.github/pull_request_template.md)** — what
  a pull request is expected to answer
- **[SECURITY.md](SECURITY.md)** — how to report a vulnerability privately
- **[CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)** — the standard expected here
- **[CHANGELOG.md](CHANGELOG.md)** — what changed, release by release

Releases carry `.deb`, `.rpm` and tarball packages for x86_64 and arm64, and an
AppImage for x86_64 (see **Install it** above). Building from source is still the way to work on the
application, and [docs/INSTALLATION.md](docs/INSTALLATION.md) is the whole
procedure for both.
