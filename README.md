# Time Warp Studio

**Educational Multi-Language Programming Environment with Integrated IDE, Debugger, and Turtle Graphics**

[![Python 3.10+](https://img.shields.io/badge/Python-3.10%2B-blue)](https://www.python.org/)
[![PySide6](https://img.shields.io/badge/PySide6-Qt6-green)](https://wiki.qt.io/Qt_for_Python)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-8.1.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a unified desktop programming environment designed for educators and students. It brings together **24 programming languages** spanning six decades of computing history into a single, modern IDE. Built with Python and PySide6 (Qt6), it provides an integrated platform for learning programming concepts, exploring turtle graphics, and working through structured lessons.

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

24 languages spanning six decades of computing history:

| Language | Paradigm | Era | Status |
|----------|----------|-----|--------|
| **BASIC** | Imperative / Educational | 1964 | Complete |
| **Logo** | Turtle Graphics / Educational | 1967 | Complete |
| **PILOT** | Computer-Aided Instruction | 1969 | Complete |
| **C** | Systems / Procedural | 1972 | Complete |
| **Forth** | Stack-based / Concatenative | 1970 | Complete |
| **Pascal** | Structured / Educational | 1970 | Complete |
| **Prolog** | Logic / Declarative | 1972 | Complete |
| **SQL** | Relational / Query | 1974 | Complete |
| **Smalltalk** | Object-Oriented | 1980 | Complete |
| **APL** | Array / Mathematical | 1966 | Complete |
| **Assembly** | Low-level / x86 | 1950s | Complete |
| **Brainfuck** | Esoteric / Turing | 1993 | Complete |
| **CICS** | Mainframe / Transaction | 1969 | Complete |
| **COBOL** | Business / Data | 1959 | Complete |
| **Fortran** | Scientific / Numeric | 1957 | Complete |
| **Haskell** | Functional / Typed | 1990 | Complete |
| **HyperTalk** | Event-driven / English | 1987 | Complete |
| **JavaScript** | Scripting / Web | 1995 | Complete |
| **JCL** | Job Control / Mainframe | 1964 | Complete |
| **Lua** | Scripting / Embedded | 1993 | Complete |
| **Python** | Multi-paradigm / Modern | 1991 | Complete |
| **REXX** | Scripting / Mainframe | 1979 | Complete |
| **Scheme** | Functional / Lisp | 1975 | Complete |
| **SQR** | Report / Database | 1980s | Complete |

### Integrated IDE

- **Code Editor** — Syntax highlighting, line numbers, and auto-indentation per language
- **Output Console** — Program results, error messages, and interactive input
- **Graphics Canvas** — Real-time turtle graphics rendering with zoom and pan
- **Theme System** — 25 built-in themes (Dracula, Monokai, Catppuccin Mocha, Gruvbox Dark, VS Code Dark/Light, GitHub Dark/Light, Nord, Solarized, retro CRT, and more)
- **Find & Replace** — Advanced search with regex, case sensitivity, whole word matching, and live match highlighting
- **Auto-Completion** — Context-aware completions from language keywords and document identifiers
- **14 Feature Panels** — Lessons, AI Assistant, Error Explainer, Examples Browser, Turtle Inspector, Debugger, and more

### Turtle Graphics

- Native turtle support with position, heading, and pen state tracking
- 50+ drawing commands across Logo and BASIC
- Real-time rendering with Qt painter, zoom/pan controls
- Cross-language graphics support (Logo and Turbo BASIC)

### Advanced Debugger

- Statement-level stepping with breakpoint support
- Timeline recording of program execution with state snapshots
- Variable inspector with real-time value tracking
- Rewind capability to navigate backwards through execution history

### Educational Tools

- **Lesson System** — Step-by-step guided instruction with auto-verification
- **AI Assistant** — Intelligent code suggestions and explanations
- **Error Explainer** — Human-readable explanations of programming errors
- **Examples Browser** — 93 example programs across all 24 languages
- **Achievements** — Gamified progress tracking

---

## 📋 Requirements

| Requirement | Minimum | Recommended |
|-------------|---------|-------------|
| **Python** | 3.10 | 3.12+ |
| **PySide6** | Any recent | Latest |
| **Pillow** | 10.0.0 | Latest |
| **OS** | Windows 10, macOS 10.14, Ubuntu 20.04 | Any modern OS |
| **RAM** | 4 GB | 8 GB |
| **CPU** | SSSE3/SSE4 support | Any modern CPU |

---

## 🚀 Quick Start

### Linux / macOS

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
python run.py
```

### Windows (PowerShell)

```powershell
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
python run.py
```

The `run.py` launcher will automatically:

1. Check your Python version (3.10+ required)
2. Create a virtual environment if one doesn't exist
3. Install all dependencies
4. Launch the IDE

That's it — you should see the Time Warp Studio window in a few seconds.

---

## 🔧 Installation

### Option 1: Smart Launcher (Recommended)

```bash
python run.py              # Auto-setup and launch
python run.py --fresh      # Force-recreate virtual environment
python run.py --skip-setup # Skip dependency checks (faster startup)
python run.py --no-venv    # Use system Python (not recommended)
python run.py --help       # Show all options
```

### Option 2: Manual Setup

```bash
# Create and activate virtual environment
python3 -m venv .venv
source .venv/bin/activate        # Linux/macOS
# .venv\Scripts\activate         # Windows

# Install dependencies
pip install -r Platforms/Python/requirements.txt

# Launch the IDE
python Platforms/Python/time_warp_ide.py
```

### Option 3: Shell Script (Linux/macOS)

```bash
./run.sh
```

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

4. Press **Ctrl+R** (or click **Run**)

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

### Pascal — Structured Programming

```pascal
program Hello;
var
  i: integer;
begin
  writeln('Hello from Pascal!');
  for i := 1 to 5 do
    writeln('Count: ', i);
end.
```

### Prolog — Logic Programming

```prolog
parent(tom, bob).
parent(bob, ann).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
?- grandparent(tom, ann).
```

### Forth — Stack Operations

```forth
: GREET ." Hello from Forth!" CR ;
GREET
5 3 + . CR
```

Browse 93 more examples in the [Examples/](Examples/) directory or through **File → Examples** in the IDE.

---

## 📖 Documentation

### Quick References

| Document | Description |
|----------|-------------|
| [ARCHITECTURE.md](ARCHITECTURE.md) | System design and technical details |
| [INSTALLATION.md](docs/INSTALLATION.md) | Detailed installation instructions |
| [CONTRIBUTING.md](CONTRIBUTING.md) | How to contribute |

### Guides (in `docs/guides/`)

| Guide | Description |
|-------|-------------|
| [Getting Started](docs/guides/01-getting-started.md) | First steps with the IDE |
| [IDE Basics](docs/guides/02-ide-basics.md) | Editor, menus, and panels |
| [Lessons](docs/guides/03-lessons.md) | Working through guided lessons |
| [Turtle Graphics](docs/guides/04-turtle-graphics.md) | Drawing with the turtle |
| [Settings](docs/guides/06-settings.md) | Customizing the IDE |
| [Keyboard Shortcuts](docs/guides/07-shortcuts.md) | Complete shortcut reference |
| [Troubleshooting](docs/guides/08-troubleshooting.md) | Fixing common problems |

### Language Tutorials (in `docs/tutorials/`)

| Tutorial | Description |
|----------|-------------|
| [BASIC](docs/tutorials/basic.md) | BASIC programming with graphics |
| [Logo](docs/tutorials/logo.md) | Turtle graphics programming |
| [PILOT](docs/tutorials/pilot.md) | Interactive lesson creation |
| [C](docs/tutorials/c.md) | C language basics |
| [Pascal](docs/tutorials/pascal.md) | Structured programming |
| [Prolog](docs/tutorials/prolog.md) | Logic programming |
| [Python](docs/tutorials/python.md) | Modern scripting & sandboxed execution |
| [Lua](docs/tutorials/lua.md) | Lightweight scripting language |
| [JavaScript](docs/tutorials/javascript.md) | Scripting in the browser era |
| [Haskell](docs/tutorials/haskell.md) | Pure functional programming |
| [Scheme](docs/tutorials/scheme.md) | Lisp-family functional programming |
| [Smalltalk](docs/tutorials/smalltalk.md) | Object-oriented programming |
| [REXX](docs/tutorials/rexx.md) | Mainframe scripting language |
| [Forth](docs/tutorials/forth.md) | Stack-based programming |
| [Brainfuck](docs/tutorials/brainfuck.md) | Esoteric Turing-complete language |
| [COBOL](docs/tutorials/cobol.md) | Business data processing |
| [Fortran](docs/tutorials/fortran.md) | Scientific computing |
| [Assembly](docs/tutorials/assembly.md) | x86 low-level programming |
| [APL](docs/tutorials/apl.md) | Array programming language |
| [HyperTalk](docs/tutorials/hypertalk.md) | Event-driven scripting |
| [JCL](docs/tutorials/jcl.md) | IBM Job Control Language |
| [CICS](docs/tutorials/cics.md) | IBM Transaction processing |
| [SQL](docs/tutorials/sql.md) | Relational database queries |
| [SQR](docs/tutorials/sqr.md) | Structured Query reporting |

### Reference

| Document | Description |
|----------|-------------|
| [FAQ](docs/reference/faq.md) | Frequently asked questions |
| [Documentation Index](docs/INDEX.md) | Full documentation listing |
| [Examples README](Examples/README.md) | Example programs guide |

---

## 🗂️ Project Structure

```
Time_Warp_Studio/
├── run.py                         # Smart launcher (auto-setup + launch)
├── run.sh                         # Shell wrapper for Linux/macOS
├── README.md                      # This file
├── ARCHITECTURE.md                # System design document
├── CONTRIBUTING.md                # Contributor guide
├── LICENSE                        # MIT License
│
├── Platforms/Python/              # Main application source
│   ├── time_warp_ide.py           # IDE entry point
│   └── time_warp/
│       ├── core/                  # Interpreter engine and services
│       │   └── interpreter.py     # Central command dispatcher
│       ├── languages/             # 24 language executors
│       │   ├── basic.py           # BASIC with Turbo graphics
│       │   ├── logo.py            # Logo turtle graphics
│       │   ├── pilot.py           # PILOT CAI system
│       │   ├── c_lang_fixed.py    # C language subset
│       │   ├── pascal.py          # Pascal programming
│       │   ├── prolog.py          # Prolog logic engine
│       │   ├── forth.py           # Forth stack machine
│       │   ├── python.py          # Python sandbox executor
│       │   ├── lua.py             # Lua scripting
│       │   ├── javascript.py      # JavaScript interpreter
│       │   ├── haskell.py         # Haskell functional
│       │   ├── scheme.py          # Scheme/Lisp dialect
│       │   ├── smalltalk.py       # Smalltalk OO
│       │   ├── rexx.py            # REXX scripting
│       │   ├── brainfuck.py       # Brainfuck esoteric
│       │   ├── cobol.py           # COBOL business
│       │   ├── fortran.py         # Fortran scientific
│       │   ├── assembly.py        # x86 Assembly
│       │   ├── apl.py             # APL array language
│       │   ├── hypertalk.py       # HyperTalk
│       │   ├── jcl.py             # JCL mainframe
│       │   ├── cics.py            # CICS transactions
│       │   ├── sql.py             # SQL queries
│       │   └── sqr.py             # SQR reporting
│       ├── ui/                    # PySide6 UI components
│       │   ├── main_window.py     # Main IDE window
│       │   ├── editor.py          # Code editor widget
│       │   ├── canvas.py          # Graphics canvas
│       │   └── feature_panels.py  # 14 feature panels
│       ├── graphics/              # Turtle graphics engine
│       ├── features/              # Lessons, autosave, etc.
│       ├── cloud/                 # Cloud sync services
│       ├── debugging/             # Integrated debugger
│       └── tests/                 # Test suite (34 tests)
│
├── Examples/                      # 93 example programs across 24 languages
│   ├── basic/       (5)  ├── logo/        (5)  ├── pilot/       (3)
│   ├── c/           (5)  ├── pascal/      (4)  ├── prolog/      (4)
│   ├── cobol/       (5)  ├── sqr/         (4)  ├── fortran/     (3)
│   ├── haskell/     (3)  ├── javascript/  (4)  ├── assembly/    (3)
│   ├── apl/         (3)  ├── hypertalk/   (3)  ├── brainfuck/   (3)
│   ├── forth/       (3)  ├── lua/         (4)  ├── rexx/        (3)
│   ├── scheme/      (4)  ├── smalltalk/   (3)  ├── python/      (5)
│   ├── sql/         (4)  ├── cics/        (4)  ├── jcl/         (5)
│   └── demo/        (1)  # cross-language showcases
│
├── docs/                          # Documentation
│   ├── INDEX.md                   # Documentation index
│   ├── guides/                    # How-to guides
│   ├── tutorials/                 # Language tutorials
│   └── reference/                 # FAQ and reference
│
├── Scripts/                       # Build and utility scripts
├── tools/                         # Development tools
└── .github/                       # GitHub CI/CD configuration
```

---

## 🏃 Running the IDE

### Launch Commands

```bash
# Recommended: Smart launcher
python run.py

# Direct launch (requires venv activated)
python Platforms/Python/time_warp_ide.py

# Shell script (Linux/macOS)
./run.sh
```

### What Happens on Startup

1. All 24 language interpreters are initialized
2. Configuration is loaded from `~/.time_warp/config.json`
3. The main IDE window opens with editor, canvas, and output panels
4. Your last theme and settings are restored

### Performance

| Metric | Typical Value |
|--------|---------------|
| Startup time | 2–5 seconds |
| Memory usage | 200–300 MB |
| Codebase | 56,000+ lines across 126 Python modules |

---

## 🧪 Testing

Run from the repository root:

```bash
# Quick root-level test run
pytest -q

# Full package suite with integrated reporting
python Platforms/Python/test_runner.py --comprehensive

# Basic smoke suite
python Platforms/Python/test_runner.py --basic

# Full package tests directly via pytest
PYTHONPATH=Platforms/Python pytest Platforms/Python/time_warp/tests -q

# Optional backend load/security suites (require backend services)
RUN_BACKEND_INTEGRATION=1 PYTHONPATH=Platforms/Python pytest Platforms/backend/tests -q
```

**Current status:** 500+ tests passing across 36+ test modules covering all 24 language executors, graphics, GUI, and interpreter tests.

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
   python -m venv .venv
   source .venv/bin/activate
   pip install -r Platforms/Python/requirements.txt
   ```
4. **Make** your changes following [PEP 8](https://peps.python.org/pep-0008/) style
5. **Test** that all tests pass:
   ```bash
  pytest -q
  python Platforms/Python/test_runner.py --basic
   ```
6. **Commit** with a clear message and submit a pull request

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

---

## 📄 License

Licensed under the **[MIT License](LICENSE)**.

---

## 🔗 Resources

- **Repository:** [github.com/James-HoneyBadger/Time_Warp_Studio](https://github.com/James-HoneyBadger/Time_Warp_Studio)
- **Maintainer:** James Temple — james@honey-badger.org
- **Version:** 8.1.0

---

## 🙏 Acknowledgments

Time Warp Studio honors the educational legacy of BASIC, Logo, PILOT, COBOL, Fortran, APL, and other pioneering programming languages spanning six decades of computing history, while providing a modern IDE experience for today's learners. Special thanks to the open-source communities behind Python, PySide6/Qt, and the many educators who continue to champion accessible programming education.

---

**Happy Programming!** 🚀
