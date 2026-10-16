# Time Warp Studio

## Educational Multi-Language Programming Environment with Integrated IDE, Debugger, and Turtle Graphics

[![C++20](https://img.shields.io/badge/C%2B%2B-20-blue)](https://en.cppreference.com/)
[![Qt6](https://img.shields.io/badge/Qt-6-green)](https://www.qt.io/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-14.1.2-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a unified desktop programming environment designed for educators and students. It brings together **6 active programming languages** into a single, modern IDE. Built as a **native C++20 / Qt6 application**, it provides an integrated platform for learning programming concepts, exploring vector turtle graphics, and working through structured lessons.

---

## Table of Contents

- [Core Features](#-core-features)
- [Requirements](#-requirements)
- [Quick Start](#-quick-start)
- [Installation](#-installation)
- [Your First Program](#-your-first-program)
- [Example Code](#-example-code)
- [Documentation](#-documentation)
- [Project Structure](#-project-structure)
- [Running the IDE](#-running-the-ide)
- [Testing](#-testing)
- [Contributing](#-contributing)
- [License](#-license)

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
- **Native engines** — the shared expression evaluator and Scheme run in-process with no Python at all

### What is still being built

The Qt IDE is the product; the PySide6 application that preceded it has been
removed. Features of that application that have **not** been ported yet — themes,
lessons, the examples browser, find/replace, auto-completion, the debugger panels
and the function-key bar — are tracked in [docs/ROADMAP.md](docs/ROADMAP.md).
Please read that list before reporting one of them as a regression.

### Turtle Graphics

- Native turtle support with position, heading, and pen state tracking
- 50+ drawing commands shared across every active language
- Real-time rendering with the Qt painter, zoom/pan controls and a content-fitted view
- Shapes, text labels, dotted and thick strokes, pen up/down, filled polygons
- Cross-language graphics support (Logo, BASIC, COMAL, REXX, and Scheme)
- The reference's conventions are unusual and are asserted in tests: heading 0 is
  north and increases clockwise, and `circle` is a polygon walk

### Debugging

Scheme runs on a native explicit-stack machine with a background runner that
supports Stop today; the stepping and inspection panels are being wired up
(tracked in the roadmap). Breakpoints and stepping remain available in the
language runtimes themselves.

### Educational Content

- **44 curated example programs** across all 6 active languages, under `Examples/`
- **Error hints** — syntax suggestions alongside interpreter error messages
- **Six language tutorials** — one per language, under `docs/tutorials/`

---

## 📋 Requirements

| Requirement | Minimum | Notes |
| --- | --- | --- |
| **C++ compiler** | C++20 | GCC 13+, Clang 16+, or MSVC 19.3+ |
| **CMake** | 3.20 | |
| **Qt6** | 6.2 | Widgets, Core and Gui modules |
| **Python** | 3.10 | Optional: only for the five languages that are not yet native |
| **RAM** | 4 GB | 8 GB recommended |

Scheme and the shared expression engine run natively with **no Python at all**.
BASIC, PILOT, Logo, COMAL and REXX currently run through the bundled engine
process; each one is being ported to C++, after which the bridge path is deleted.

---

## 🚀 Quick Start

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio

# Build the IDE
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j

# Run it
./native/build/time_warp_ide
./native/build/time_warp_ide Examples/logo/mandala.logo   # open a program
./native/build/time_warp_ide --run Examples/logo/mandala.logo
./native/build/time_warp_ide --help
```

The build produces no output on success; the first build takes a few minutes,
after that it is incremental. `make -C native run` builds and launches in one step.

---

## 🔧 Installation

### Linux / macOS

```bash
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j
./native/build/time_warp_ide
```

Add `-DCMAKE_BUILD_TYPE=Debug` for a debuggable build, and
`-DTIME_WARP_EXPR_WERROR=OFF` to build without `-Werror`.

### Windows

```powershell
cmake -S native -B native/build -G "Ninja" -DCMAKE_PREFIX_PATH=C:\Qt\6.6.0\msvc2019_64
cmake --build native/build -j
.\native\build\time_warp_ide.exe
```

Adjust `CMAKE_PREFIX_PATH` to your Qt installation.

### The language bridge (already bundled)

Nothing to install for the five bridged languages beyond Python 3.10+ on `PATH`.
The worker script ships in `native/bridge/` and the IDE finds it automatically; set
`TIME_WARP_WORKER` or `TIME_WARP_PYTHON` to override either half while debugging.

### Installing it

```bash
make -C native install PREFIX=$HOME/.local   # or /usr/local with sudo
```

That installs the binary, the language bridge, a desktop entry and an icon;
`make -C native install DESTDIR=/tmp/stage PREFIX=/usr` stages a package instead.
Installing is also the easiest way to check that a relocated build still finds the
bridge. Distro packages and installers are not built yet — see
[docs/ROADMAP.md](docs/ROADMAP.md).

---

## ✏️ Your First Program

1. Open Time Warp Studio
2. Select **BASIC** from the language dropdown (top-right)
3. Type this in the editor:

```basic
PRINT "Hello, World!"
FOR I = 1 TO 5
  PRINT "Count: "; I
NEXT I
```

1. Press **F9** or **Ctrl+R** (or click **Run**)

You'll see the output in the console panel below the editor.

---

## 📝 Example Code

### BASIC — Turbo Graphics

```basic
SCREEN 1
COLOR 14, 1
LINE (10, 10)-(200, 100)
CIRCLE (150, 150), 50
PSET (300, 200)
PRINT "Graphics demo complete!"
```

### Logo — Turtle Drawing

```logo
; Draw a colorful square
SETPENCOLOR "RED
REPEAT 4 [FORWARD 100 RIGHT 90]

; Draw a star
PENUP SETPOSITION 150 0 PENDOWN
SETPENCOLOR "BLUE
REPEAT 5 [FORWARD 80 RIGHT 144]
```

### PILOT — Interactive Lesson

```pilot
T: Welcome to the math quiz!
T: What is 2 + 2?
A: 4
TY: Correct! Well done!
TN: Not quite. The answer is 4.
```

### COMAL — Structured Programming

```comal
10 FOR I := 1 TO 5
20   PRINT "Count: "; I
30 NEXT I
40 PROC BANNER(TITLE$)
50   PRINT "=== "; TITLE$; " ==="
60 ENDPROC
70 CALL BANNER("COMAL")
```

### REXX — String Scripting

```rexx
SAY 'Hello from REXX!'
name = 'Time Warp'
SAY 'Hello' || ' ' || name
DO i = 1 TO 5
  SAY 'Count:' i
END
```

### Scheme — Functional Programming

```scheme
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))

(display "5! = ")
(display (fact 5))
(newline)
```

Browse more examples in the [Examples/](Examples/) directory or through **Help → Browse Examples** (Shift+F3) in the IDE.

---

## 📖 Documentation

### Quick References

| Document | Description |
| --- | --- |
| [Architecture](docs/ARCHITECTURE.md) | System design and technical details |
| [INSTALLATION.md](docs/INSTALLATION.md) | Detailed installation instructions |
| [CONTRIBUTING.md](CONTRIBUTING.md) | How to contribute |

### Reference

| Document | Description |
| --- | --- |
| [Language Guide](docs/LANGUAGE_GUIDE.md) | Command reference for all 6 languages |
| [Architecture](docs/ARCHITECTURE.md) | System design and technical details |
| [FAQ](docs/FAQ.md) | Frequently asked questions |
| [Documentation Index](docs/INDEX.md) | Full documentation listing |
| [Roadmap](docs/ROADMAP.md) | What is built, and what is still to come |
| [Examples Catalog](Examples/CATALOG.md) | Example programs catalog |

| Tutorial | Description |
| --- | --- |
| [BASIC](docs/tutorials/basic.md) | BASIC programming with graphics |
| [Logo](docs/tutorials/logo.md) | Turtle graphics programming |
| [PILOT](docs/tutorials/pilot.md) | Interactive lesson creation |
| [COMAL](docs/tutorials/comal.md) | Structured COMAL programming |
| [REXX](docs/tutorials/rexx.md) | String processing and scripting |
| [Scheme](docs/tutorials/scheme.md) | Functional programming with Lisp |

### Reference

| Document | Description |
| --- | --- |
| [FAQ](docs/FAQ.md) | Frequently asked questions |
| [Documentation Index](docs/INDEX.md) | Full documentation listing |
| [Examples Catalog](Examples/CATALOG.md) | Example programs catalog |

---

## 🗂️ Project Structure

```text
Time_Warp_Studio/
├── Makefile                       # Convenience targets (delegate to native/)
│
├── README.md                      # This file
├── CHANGELOG.md                   # Version history and release notes
├── CONTRIBUTING.md                # Contributor guide
├── SECURITY.md                    # Vulnerability reporting policy
├── LICENSE                        # MIT License
│
├── native/                        # THE APPLICATION (C++20 / Qt6)
│   ├── CMakeLists.txt
│   ├── include/ src/              # headers and sources
│   │   ├── ui/                    # IDE shell, editor, canvas, styling
│   │   ├── canvas/                # SPSC command queue + Qt bridge
│   │   ├── scheme/                # native Scheme machine
│   │   ├── debugger/              # background runner
│   │   └── bridge/                # QProcess layer for the Python worker
│   ├── tests/                     # 11 C++ suites (ctest)
│   ├── tools/                     # time_warp_ide.cpp + Python harnesses
│   └── bridge/                    # TEMPORARY engine for 5 unported languages
│       ├── worker.py              #   JSON-lines worker the IDE spawns
│       └── time_warp/             #   interpreter, 6 executors, turtle state
│
├── Examples/                      # 44 example programs across 6 languages
│   ├── CATALOG.md                 # Searchable example index
│   └── basic/ logo/ pilot/ comal/ rexx/ scheme/
│
├── docs/                          # Documentation
│   ├── ARCHITECTURE.md            # System design document
│   ├── INSTALLATION.md            # Build and install notes
│   ├── LANGUAGE_GUIDE.md          # Command reference for all 6 languages
│   ├── FAQ.md                     # Frequently asked questions
│   ├── README_TESTING.md          # Test setup and commands
│   ├── ROADMAP.md                 # Development roadmap
│   ├── INDEX.md                   # Documentation hub
│   └── tutorials/                 # One tutorial per language
│
├── Scripts/                       # audits, version sync, native build helper
└── .github/workflows/             # CI: native build + test + gates, docs, markdown
```

---

## 🏃 Running the IDE

### Launch commands

```bash
# Build and run
make -C native run

# Or directly
./native/build/time_warp_ide
./native/build/time_warp_ide Examples/logo/mandala.logo
./native/build/time_warp_ide --run Examples/logo/mandala.logo
./native/build/time_warp_ide --screenshot /tmp/shot.png --size 1400x900
```

### What happens on startup

1. Settings are restored via Qt QSettings (window layout, language per file)
2. The turtle canvas takes the colours the program asks for; the reference pen is
   white on a dark surface, so a blank canvas is not a blank program
3. Files named on the command line open as tabs; `--run` starts the program once
   the window is shown

### Performance

| Metric | Typical value |
| --- | --- |
| Cold configure + build | a few minutes (Release) |
| Incremental build | seconds |
| Startup | well under a second on a modern machine |
| Codebase | C++20 application plus a small, temporary Python engine |

---

## 🧪 Testing

Run from the repository root:

```bash
# The 11 C++ suites
make test

# The same suites in a Debug build, and under the sanitizers
make test-debug
make asan
make asan ASAN=undefined SAN_DIR=build-ubsan \
  ASAN_FLAGS='-fsanitize=undefined -fno-omit-frame-pointer -g'

# The bridge engine's own pytest suite
make engine-test

# Every example program, through the real bridge (non-zero exit on failure)
make smoke

# Everything
make test-all && make lint
```

**What the gates protect.** The suites cover the lexer, the expression evaluator,
the Scheme machine, the canvas queue and its Qt bridge, the debug and debugger
runner, the UI layout and documents, the turtle canvas, and the Python bridge.
Two differential harnesses additionally compare the C++ engines against the
Python reference on a fixed corpus and must report **0 unexpected mismatches**.
CI runs all of this plus the 44-example gate.

**Current status:** green in Release, Debug, AddressSanitizer and
UndefinedBehaviorSanitizer builds; all 44 example programs run and the 14 that
draw, draw.

---

## 🤝 Contributing

We welcome contributions! Here's how to get started:

1. **Fork** the repository
2. **Clone** your fork and create a feature branch:

   ```bash
   git checkout -b feature/my-improvement
   ```

3. **Set up** the development environment:

   ```bash
   # Build (Release) and run the suites
   cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
   cmake --build native/build -j
   ctest --test-dir native/build --output-on-failure

   # Only if you are touching the bridged languages:
   python -m venv .venv && . .venv/bin/activate
   pip install pytest ruff
   ```

4. **Make** your changes. Match the surrounding C++ style; the build is
   `-Wall -Wextra -Wpedantic -Wshadow -Wcast-qual -Wuseless-cast -Wlogical-op
   -Wnull-dereference` with `-Werror`, so warnings are failures
5. **Test** that everything passes:

   ```bash
   make test-all && make lint
   ```

6. **Commit** with a clear message and submit a pull request

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

---

## 📄 License

Licensed under the **[MIT License](LICENSE)**.

---

## 🔗 Resources

- **Repository:** [github.com/James-HoneyBadger/Time_Warp_Studio](https://github.com/James-HoneyBadger/Time_Warp_Studio)
- **Maintainer:** James Temple — <james@honey-badger.org>
- **Version:** 14.1.2

---

## 🙏 Acknowledgments

Time Warp Studio honors the educational legacy of BASIC, PILOT, Logo, COMAL, REXX, and Scheme, and other pioneering programming languages spanning six decades of computing history, while providing a modern IDE experience for today's learners. Special thanks to the open-source communities behind Qt and Python, and the many educators who continue to champion accessible programming education.

---

**Happy Programming!** 🚀
