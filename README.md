# Time Warp Studio

## Educational Multi-Language Programming Environment with Integrated IDE, Debugger, and Turtle Graphics

[![C++20](https://img.shields.io/badge/C%2B%2B-20-blue)](https://en.cppreference.com/)
[![Qt6](https://img.shields.io/badge/Qt-6-green)](https://www.qt.io/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-15.0.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a unified desktop programming environment designed for educators and students. It brings together **10 active programming languages** into a single, modern IDE. Built as a **native C++20 / Qt6 application**, it provides an integrated platform for learning programming concepts, exploring turtle graphics, and working through a **checked lesson curriculum** with tools for both learners and teachers.

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

- **Code editor** — line-number gutter, current-line highlight, 4-space tab stops, per-language file handling, and per-language syntax colouring (comments, strings, numbers, keywords)
- **Output console** — program output as it is produced, with interpreter errors in their own tab
- **Turtle canvas** — the drawing appears as it is made, with zoom, pan, grid, auto-fit and a turtle marker
- **Documents** — tabs, per-file language detection, unsaved-change prompts, new/open/save/save-as
- **Run controls** — Run (F9) and Stop, with a status bar showing language, cursor position and execution state
- **Native engines** — Scheme runs in-process with no Python at all; the expression
  evaluator (`timewarp_expr`) is native C++ too, but nothing calls it yet, so
  expressions inside a Run are still evaluated by the bridge (see
  [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md))

### What is still being built

The Qt IDE is the product; the PySide6 application that preceded it has been removed.
Features of that application that have **not** been ported yet — themes and the CRT
effects, the examples browser, find/replace, auto-completion, the debugger panels, and
the function-key bar's Shift and Ctrl layers — are tracked in
[docs/ROADMAP.md](docs/ROADMAP.md).
Please read that list before reporting one of them as a regression.

### Turtle Graphics

- Native turtle support with position, heading, and pen state tracking
- 50+ drawing commands shared across every active language
- Real-time rendering with the Qt painter, zoom/pan controls and a content-fitted view
- Shapes, text labels, dotted and thick strokes, pen up/down, filled polygons
- Cross-language graphics support (Logo, BASIC, COMAL, REXX, and Scheme)
- The reference's conventions are unusual and are asserted in tests: heading 0 is north and increases clockwise, and `circle` is a polygon walk

### Debugging

Scheme runs on a native explicit-stack machine with a background runner that supports Stop today; the stepping and inspection panels are being wired up (tracked in the roadmap). Breakpoints and stepping remain available in the language runtimes themselves.

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

There are no installers or packages yet. Building from source is the only way to
run it, and [docs/INSTALLATION.md](docs/INSTALLATION.md) is the whole procedure.
