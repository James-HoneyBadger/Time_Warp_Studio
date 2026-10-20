# Time Warp Studio

## Educational Multi-Language Programming Environment with Integrated IDE, Debugger, and Turtle Graphics

[![C++20](https://img.shields.io/badge/C%2B%2B-20-blue)](https://en.cppreference.com/)
[![Qt6](https://img.shields.io/badge/Qt-6-green)](https://www.qt.io/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-16.0.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a unified desktop programming environment designed for educators and students. It brings together **10 active programming languages** into a single, modern IDE. Built as a **native C++20 / Qt6 application**, it provides an integrated platform for learning programming concepts, exploring turtle graphics, and working through a **checked lesson curriculum** with tools for both learners and teachers.

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

## 🎯 Core Features

### Multi-Language Support

10 active languages in the current studio build:

| Language | Paradigm | Era | Execution Mode |
| --- | --- | --- | --- |
| **BASIC** | Imperative / Educational | 1964 | Line-by-line |
| **APL** | Array / Notation as tool | 1966 | Line-by-line |
| **Logo** | Turtle Graphics / Educational | 1967 | Line-by-line |
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
- **Turtle canvas** — the drawing appears as it is made, with zoom, pan, grid, auto-fit and a turtle marker; export it to PNG/JPEG/BMP (or SVG when the build has Qt6 Svg) with Ctrl+E, or copy it to the clipboard, with or without the grid on screen
- **Printing and export** — File > Print Code (Ctrl+P) sends the program to a printer as the same A4 page the PDF export writes, and File > Print Drawing sends the drawing scaled to fit the page; both entries appear only in a build with Qt6 PrintSupport. The program also exports as PDF, and the program, output and drawing as one HTML page
- **Profiling** — View > Profile Run times every statement of a line-by-line program and ends with a report of line, hits, time and share, longest first. It is off at every launch because timing costs
- **Terminal (opt-in)** — Settings > Workspace adds a 💻 Terminal tab that runs commands through your own shell, starting in the current document's folder. It is off by default, stopped and removed in presentation and player modes, and is a command line rather than a terminal emulator: no prompts, no job control, no full-screen programs
- **Documents** — tabs, per-file language detection, unsaved-change prompts, new/open/save/save-as, a recent-files menu, and the files from the last session reopened at launch
- **Run controls** — Run (F9) and Stop, with a status bar showing language, cursor position and execution state
- **Inspection** — a 🔍 **Variables** panel listing every binding with its type and value, filtered by name, and a ⚡ **Telemetry** HUD showing what the run cost and did: steps used, engine time, steps per second, last line, recursion depth, call stack, output lines, drawing commands, BASIC's memory window, and — beside the engine's own counters — the IDE's wall clock and the ink that actually reached the canvas
- **Debugging (Scheme)** — a 🐞 **Debug** panel driving the native runner: Continue, Pause, Step In/Over/Out, Stop and breakpoints, with the call stack and the frame's bindings shown while paused. Every other language runs to completion without a pause protocol, so the panel says so instead of offering dead buttons
- **Native engines** — every language runs in-process in C++ with no Python at
  all. Scheme (`timewarp_scheme`) and CHIP-8, Logo and Smalltalk are
  whole-program; BASIC, FOCAL, PILOT, COMAL, APL and REXX are line-by-line on
  the shared `timewarp_lang` engine. The arithmetic each one evaluates goes
  through the native expression evaluator (`timewarp_expr`), and the drawing
  through the same canvas command queue the IDE drains (see
  [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md))

### What is still being built

The Qt IDE is the product; the PySide6 application that preceded it has been removed.
Features of that application that have **not** been ported yet — themes and the CRT
effects, the examples browser, find-in-files, auto-completion, the console's clickable
line links, the toolbar's Debug/Continue/Trace actions, and the function-key bar's Shift
and Ctrl layers — are tracked in [docs/ROADMAP.md](docs/ROADMAP.md).
Please read that list before reporting one of them as a regression.

### Turtle Graphics

- Native turtle support with position, heading, and pen state tracking
- 50+ drawing commands shared across every active language
- Real-time rendering with the Qt painter, zoom/pan controls and a content-fitted view
- Shapes, text labels, dotted and thick strokes, pen up/down, filled polygons
- Cross-language graphics support (Logo, BASIC, COMAL, REXX, Scheme, FOCAL, APL
  and Smalltalk; PILOT draws with its `G:` commands and CHIP-8 paints rectangles)
- The reference's conventions are unusual and are asserted in tests: heading 0 is north and increases clockwise, and `circle` is a polygon walk

### Debugging

Scheme runs on a native explicit-stack machine with a background runner, and the 🐞
Debug panel drives it: Continue, Pause, Step In/Over/Out, Stop and breakpoints, with the
call stack and that frame's bindings shown while paused. Values are typed by the machine
rather than guessed from their printed form. Only Scheme can be stepped — every
other language runs to completion and has no pause protocol — so
the panel disables itself and says why, and the toolbar's Debug/Continue/Trace actions
are still inert (tracked in the roadmap).

### Guided Lessons

- **20 lessons, 60 checkpoints** — two lessons for each of the ten languages
- **Checked by running the program**, never by comparing it with an answer, so `PRINT 6 * 7` and `PRINT 42` are the same answer and a drawing checkpoint cannot be passed by printing about drawing
- **In the IDE** — the 🎓 **Lessons** panel: the curriculum, the current checkpoint, hints, and a Check button that runs the program in your document
- **At a terminal** — `python3 native/tools/learn.py walk basic_first_steps`
- **For teachers** — `python3 native/tools/teach.py`: lesson plans with objectives and teacher notes, answer keys, a class roster, handout distribution, marking, class progress reports, bundles a class can be moved in, and a presentation size for teaching from the front
- **The curriculum checks itself** — `teach.py validate` runs all sixty solutions through the engine and refuses a starter that already works, so a lesson nobody can finish cannot sit in the tree looking correct
- Progress is saved per lesson and hints are counted; neither a reset nor a marking pass can lose a learner's work

See [docs/lessons/](docs/lessons/README.md) for the guides.

### Educational Content

- **48 curated example programs** across all 10 active languages, under `Examples/`
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
turtle graphics, a checked lesson curriculum with student and teacher tools, and a
debugger.
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
| Learning one of the languages | [`docs/tutorials/`](docs/tutorials/) |
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

Releases carry packages, an AppImage and a tarball for x86_64 and arm64 (see
**Install it** above). Building from source is still the way to work on the
application, and [docs/INSTALLATION.md](docs/INSTALLATION.md) is the whole
procedure for both.
