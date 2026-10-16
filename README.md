# Time Warp Studio

## Educational Multi-Language Programming Environment with Integrated IDE, Debugger, and Turtle Graphics

[![C++20](https://img.shields.io/badge/C%2B%2B-20-blue)](https://en.cppreference.com/)
[![Qt6](https://img.shields.io/badge/Qt-6-green)](https://www.qt.io/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-15.0.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a unified desktop programming environment designed for educators and students. It brings together **6 active programming languages** into a single, modern IDE. Built as a **native C++20 / Qt6 application**, it provides an integrated platform for learning programming concepts, exploring turtle graphics, and working through structured lessons and examples.

---

## 🎯 Core Features

### Multi-Language Support

6 active languages in the current studio build:

| Language | Paradigm | Era | Execution Mode |
| --- | --- | --- | --- |
| **BASIC** | Imperative / Educational | 1964 | Line-by-line |
| **Logo** | Turtle Graphics / Educational | 1967 | Line-by-line |
| **PILOT** | Computer-Aided Instruction | 1969 | Line-by-line |
| **COMAL** | Structured BASIC / Educational | 1979 | Line-by-line |
| **REXX** | Scripting / String processing | 1979 | Line-by-line |
| **Scheme** | Functional / Lisp dialect | 1975 | Whole-program |

### Integrated IDE

- **Code editor** — line-number gutter, current-line highlight, 4-space tab stops, per-language file handling (syntax colouring is not implemented yet: see `docs/ROADMAP.md`)
- **Output console** — program output as it is produced, with interpreter errors in their own tab
- **Turtle canvas** — the drawing appears as it is made, with zoom, pan, grid, auto-fit and a turtle marker
- **Documents** — tabs, per-file language detection, unsaved-change prompts, new/open/save/save-as
- **Run controls** — Run (F9) and Stop, with a status bar showing language, cursor position and execution state
- **Native engines** — Scheme runs in-process with no Python at all; the expression
  evaluator (`timewarp_expr`) is native C++ too, but nothing calls it yet, so
  expressions inside a Run are still evaluated by the bridge (see
  [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md))

### What is still being built

The Qt IDE is the product; the PySide6 application that preceded it has been removed. Features of that application that have **not** been ported yet — themes, lessons, the examples browser, find/replace, auto-completion, the debugger panels and the function-key bar — are tracked in [docs/ROADMAP.md](docs/ROADMAP.md).
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

### Educational Content

- **44 curated example programs** across all 6 active languages, under `Examples/`
- **Error hints** — syntax suggestions alongside interpreter error messages
- **Six language tutorials** — one per language, under `docs/tutorials/`
