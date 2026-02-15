# Time Warp Studio

**Educational Multi-Language Programming Environment with Integrated IDE, Debugger, and Turtle Graphics**

[![Python 3.10+](https://img.shields.io/badge/Python-3.10%2B-blue)](https://www.python.org/)
[![PySide6](https://img.shields.io/badge/PySide6-Qt6-green)](https://wiki.qt.io/Qt_for_Python)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Version](https://img.shields.io/badge/Version-7.0.0-orange)](https://github.com/James-HoneyBadger/Time_Warp_Studio)

Time Warp Studio is a unified desktop programming environment designed for educators and students. It brings together **7 programming languages** — BASIC, PILOT, Logo, C, Pascal, Prolog, and Forth — into a single, modern IDE. Built with Python and PySide6 (Qt6), it provides an integrated platform for learning programming concepts, exploring turtle graphics, and working through structured lessons.

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

| Language | Description | Status |
|----------|-------------|--------|
| **BASIC** | Turbo BASIC with graphics extensions (SCREEN, LINE, CIRCLE, PSET, COLOR) | Complete |
| **Logo** | Full turtle graphics with procedures, recursion, and 50+ commands | Complete |
| **PILOT** | Computer-Aided Instruction for interactive lessons | Complete |
| **C** | Subset of C with stdio, math, conditionals, loops, and functions | Experimental |
| **Pascal** | Structured programming with procedures, functions, and types | Experimental |
| **Prolog** | Logic programming with unification and backtracking | Experimental |
| **Forth** | Stack-based programming with word definitions | Experimental |

### Integrated IDE

- **Code Editor** — Syntax highlighting, line numbers, and auto-indentation per language
- **Output Console** — Program results, error messages, and interactive input
- **Graphics Canvas** — Real-time turtle graphics rendering with zoom and pan
- **Theme System** — 8 built-in themes (Dracula, Monokai, Solarized Dark, Ocean, Spring, Sunset, Candy, Forest)
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
- **Examples Browser** — 90+ example programs across all 7 languages
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

Browse 90+ more examples in the [Examples/](Examples/) directory or through **File → Examples** in the IDE.

---

## 📖 Documentation

### Quick References

| Document | Description |
|----------|-------------|
| [LAUNCH_GUIDE.md](LAUNCH_GUIDE.md) | Quick-start walkthrough |
| [ARCHITECTURE.md](ARCHITECTURE.md) | System design and technical details |
| [INSTALLATION.md](INSTALLATION.md) | Detailed installation instructions |
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
├── LAUNCH_GUIDE.md                # Quick-start guide
├── INSTALLATION.md                # Installation details
├── CONTRIBUTING.md                # Contributor guide
├── LICENSE                        # MIT License
│
├── Platforms/Python/              # Main application source
│   ├── time_warp_ide.py           # IDE entry point
│   └── time_warp/
│       ├── core/                  # Interpreter engine and services
│       │   └── interpreter.py     # Central command dispatcher
│       ├── languages/             # 7 language executors
│       │   ├── basic.py           # BASIC with Turbo graphics
│       │   ├── logo.py            # Logo turtle graphics
│       │   ├── pilot.py           # PILOT CAI system
│       │   ├── c_lang_fixed.py    # C language subset
│       │   ├── pascal.py          # Pascal programming
│       │   ├── prolog.py          # Prolog logic engine
│       │   └── forth.py           # Forth stack machine
│       ├── ui/                    # PySide6 UI components
│       │   ├── main_window.py     # Main IDE window
│       │   ├── editor.py          # Code editor widget
│       │   ├── canvas.py          # Graphics canvas
│       │   └── feature_panels.py  # 14 feature panels
│       ├── graphics/              # Turtle graphics engine
│       ├── features/              # Lessons, autosave, etc.
│       ├── cloud/                 # Cloud sync services
│       ├── debugging/             # Integrated debugger
│       └── tests/                 # Test suite (55 tests)
│
├── Examples/                      # 90+ example programs
│   ├── basic/                     # 18 BASIC examples
│   ├── logo/                      # 17 Logo examples
│   ├── pilot/                     # 14 PILOT examples
│   ├── c/                         # 12 C examples
│   ├── pascal/                    # 10 Pascal examples
│   ├── prolog/                    # 10 Prolog examples
│   ├── forth/                     # 5 Forth examples
│   └── demo/                      # 8 cross-language demos
│
├── docs/                          # Documentation
│   ├── INDEX.md                   # Documentation index
│   ├── guides/                    # How-to guides
│   ├── tutorials/                 # Language tutorials
│   └── reference/                 # FAQ and reference
│
├── Scripts/                       # Build and utility scripts
├── tools/                         # Development tools
├── config/                        # Configuration files
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

1. Language interpreters are initialized (BASIC, Logo, PILOT, C, Pascal, Prolog, Forth)
2. Configuration is loaded from `~/.Time_Warp/config.json`
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

Run from the `Platforms/Python/` directory:

```bash
# Run full test suite with coverage
python -m pytest time_warp/tests/ -v

# Comprehensive suite with HTML reporting
python test_runner.py --comprehensive

# Quick smoke tests
python test_runner.py --basic
```

**Current status:** 55 tests passing across 4 test modules, covering API integration, Logo graphics, multiplayer collaboration, and WebSocket integration.

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
   cd Platforms/Python
   python -m pytest time_warp/tests/ -v
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
- **Version:** 7.0.0

---

## 🙏 Acknowledgments

Time Warp Studio honors the educational legacy of BASIC, Logo, PILOT, and other pioneering programming languages from the 1960s–1980s, while providing a modern IDE experience for today's learners. Special thanks to the open-source communities behind Python, PySide6/Qt, and the many educators who continue to champion accessible programming education.

---

**Happy Programming!** 🚀
