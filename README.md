# Time Warp Studio

A unified educational programming environment supporting BASIC, PILOT, Logo, Python, C, Pascal, Prolog, and Forth with integrated turtle graphics, modern IDE features, and comprehensive lesson/example system.

**[Quick Start](#quick-start) • [Features](#key-features) • [Documentation](#documentation) • [Examples](#examples) • [Contributing](#contributing)**

---

## Overview

Time Warp Studio is a native desktop educational programming environment that brings classic and modern programming languages into a single application designed for learning and experimentation. Built with Python and PySide6 (Qt6), it provides an integrated IDE for teaching programming concepts, exploring graphics with turtle graphics, and managing lessons with built-in checkpoints.

### Key Features

- 🎨 **Unified Editor** - Syntax highlighting and code editing for all supported languages with real-time validation
- 🐢 **Turtle Graphics** - Full turtle graphics support with interactive canvas, stroke tracking, and live preview
- 📚 **8 Languages** - BASIC, PILOT, Logo, Python, C, Pascal, Prolog, Forth
- ⚡ **Fast Execution** - Instant code execution with real-time output and error highlighting
- 🎓 **Lesson System** - Step-by-step guided lessons with checkpoints, hints, and auto-verification
- 📖 **Examples Browser** - Searchable catalog of 100+ example programs by language, difficulty, and category
- 🎨 **Theme Editor** - Create and manage custom themes with built-in presets (Dracula, Solarized, Light, and more)
- 💾 **Autosave System** - Automatic file saving with version history (up to 20 versions per file)
- 🏫 **Classroom Mode** - Presentation mode, workspace bundles, and assignment distribution
- 🔧 **Extensible** - Plugin system for custom features and integrations
- 🔄 **Multiplayer** - Real-time code sharing and collaborative development
- 📊 **Analytics** - Learning progress tracking and performance insights

---

## Quick Start

### Requirements

- **Python** 3.10 or higher
- **PySide6** (Qt6 bindings) - automatically installed with pip
- **Pillow** 10.0.0+ for graphics support
- **Modern operating system** (Windows, macOS, Linux)
- **4GB RAM** minimum recommended
- **CPU with SSSE3/SSE4 support** (required for PySide6; most modern CPUs have this)

### Installation

1. **Clone the repository:**
   ```bash
   git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
   cd Time_Warp_Studio
   ```

2. **Create and activate a virtual environment:**
   ```bash
   # Linux/macOS
   python3 -m venv .venv
   source .venv/bin/activate
   
   # Windows
   python -m venv .venv
   .venv\Scripts\activate
   ```

3. **Install dependencies:**
   ```bash
   pip install -r Platforms/Python/requirements.txt
   ```

4. **Run the IDE:**
   ```bash
   python Platforms/Python/time_warp_ide.py
   ```

### First Program

1. Select **BASIC** from the Language dropdown
2. Type:
   ```basic
   PRINT "Hello, World!"
   ```
3. Click **Run** (or press Ctrl+R)

For interactive lessons, open **File → Lessons** to browse the lesson system.

---

## Key Features Explained

### Lesson System
- **Structured Learning**: Step-by-step guided lessons with checkpoints
- **Verification**: Automatic solution checking against expected output
- **Hints**: Context-sensitive hints for stuck learners
- **Progress Tracking**: Complete lessons and track learning progress
- Access via: **File → Lessons**

### Examples Browser
- **Organized Catalog**: 100+ examples sorted by language, difficulty, and category
- **Searchable**: Find examples by keyword or feature
- **Copy & Modify**: Load examples and modify to experiment
- **Learning Path**: Follow progression from basic to advanced examples
- Access via: **File → Examples** or **Ctrl+E**

### Turtle Preview
- **Live Visualization**: See Logo graphics as you type
- **Interactive Canvas**: Zoom, pan, and inspect drawings
- **Stroke Tracking**: View all turtle movements and pen operations
- **Color Support**: Full color palette for artistic drawings
- Integrated into editor canvas

### Theme Editor
- **8 Built-in Themes**: Dracula, Monokai, Solarized Dark, Ocean, Spring, Sunset, Candy, Forest
- **Custom Themes**: Create personalized color schemes
- **Live Preview**: See theme changes immediately
- **Persistent Storage**: Theme preferences saved across sessions
- Access via: **Settings → Themes**

### Autosave System
- **Background Saving**: Automatically save work without interruption
- **Version History**: Keep up to 20 versions of each file
- **Restore Previous**: Recover earlier versions of your code
- **Configurable Intervals**: Set autosave frequency (default: 30 seconds)
- Disabled for read-only files

### Classroom Mode
- **Presentation Mode**: Full-screen code display for teaching
- **Workspace Bundles**: Package code, examples, and lessons for distribution
- **Assignment Support**: Create and distribute assignments with rubrics
- **Student Tracking**: Monitor student progress and submissions
- Access via: **View → Classroom Mode**

---

## Documentation

### Getting Started
- [Installation & Setup](docs/guides/01-getting-started.md) - Complete setup instructions
- [IDE Basics](docs/guides/02-ide-basics.md) - Navigate the interface and features
- [Turtle Graphics Guide](docs/guides/04-turtle-graphics.md) - Master turtle graphics and drawing

### Language Tutorials
- [BASIC Tutorial](docs/tutorials/basic.md) - Learn classic BASIC programming
- [PILOT Tutorial](docs/tutorials/pilot.md) - Computer-based instruction language
- [Logo Tutorial](docs/tutorials/logo.md) - Turtle graphics and visual programming
- [Python Guide](docs/tutorials/python.md) - Modern Python in Time Warp
- [C Reference](docs/tutorials/c.md) - Systems programming with C
- [Pascal Guide](docs/tutorials/pascal.md) - Structured programming
- [Prolog Guide](docs/tutorials/prolog.md) - Logic programming
- [Forth Guide](docs/tutorials/forth.md) - Stack-based programming

### IDE Features
- [Settings & Themes](docs/guides/06-settings.md) - Customize appearance and behavior
- [Keyboard Shortcuts](docs/guides/07-shortcuts.md) - Speed up your workflow
- [Lesson System](docs/guides/03-lessons.md) - Use structured learning paths

### Reference & Support
- [Troubleshooting](docs/guides/08-troubleshooting.md) - Solutions for common issues
- [FAQ](docs/reference/faq.md) - Frequently asked questions
- [Architecture Guide](ARCHITECTURE.md) - Project design and structure

---

## Examples

Explore 100+ example programs in the `Examples/` directory, organized by language and difficulty:

```
Examples/
├── basic/          - BASIC samples (11 programs)
├── pilot/          - PILOT interactive lessons (8 programs)
├── logo/           - Logo turtle graphics (10 programs)
├── python/         - Python demonstrations
├── c/              - C language programs (8 programs)
├── pascal/         - Pascal structured programs
├── prolog/         - Logic programming examples
├── forth/          - Stack-based Forth examples
└── fixtures/       - Test fixtures for examples
```

**How to run examples:**
1. **File → Examples** or press **Ctrl+E**
2. Browse and select an example
3. Click **Load** to open in editor
4. Press **Ctrl+R** or click **Run** to execute

### Featured Examples

- **basic/hello_world.bas** - Simple PRINT statement
- **basic/guessing_game.bas** - Interactive game with loops and conditionals
- **logo/01_hello_world.logo** - First Logo program with turtle
- **logo/05_trees.logo** - Recursive tree drawing using procedures
- **logo/showcase.logo** - Complete turtle graphics demonstration
- **pilot/09_showcase.pilot** - All PILOT features combined

---

## Architecture

### Desktop Application (Python/PySide6)

Time Warp Studio is a native desktop application built with Python 3.10+ and PySide6 (Qt6 for Python).

**Core Components:**

```
Platforms/Python/time_warp/
├── core/
│   ├── interpreter.py              - Central command dispatcher
│   ├── safe_expression_evaluator.py - Protected math expression evaluation
│   └── [50+ support modules]        - Analytics, AI, debugging, etc.
├── languages/
│   ├── basic.py                    - BASIC interpreter
│   ├── pilot.py                    - PILOT interpreter
│   ├── logo.py                     - Logo with turtle graphics
│   ├── python.py                   - Python support
│   ├── c.py                        - C language (experimental)
│   ├── pascal.py                   - Pascal language (experimental)
│   ├── prolog.py                   - Prolog language (experimental)
│   └── forth.py                    - Forth language (experimental)
├── features/
│   ├── lesson_system.py            - Structured learning with checkpoints
│   ├── examples_browser.py         - Searchable example catalog
│   ├── turtle_preview.py           - Live Logo code preview
│   ├── theme_editor.py             - Custom theme creation
│   ├── autosave_manager.py         - Background autosave with versioning
│   └── classroom_mode.py           - Presentation and assignment features
├── ui/
│   ├── qt_ui.py                    - PySide6 UI factory
│   ├── main_window.py              - Main application window
│   ├── editor.py                   - Code editor with syntax highlighting
│   └── [more UI components]        - Canvas, panels, dialogs
└── tests/
    └── [30+ test files]            - Comprehensive pytest suite
```

**Key Design Decisions:**
- **Single Process**: All code execution in one application instance
- **Stateless Executors**: Language executors return text output, UI owns state
- **Safe Evaluation**: Math expressions use protected evaluator, not `eval()`
- **Threading**: Async support for non-blocking execution
- **Persistence**: Config and themes in `~/.Time_Warp/config.json`

### Supported Languages

| Language | Status | Features |
|----------|--------|----------|
| **BASIC** | Core | Variables, arrays, loops, subroutines, string operations |
| **PILOT** | Core | Instruction language, conditional branching, lesson support |
| **Logo** | Core | Turtle graphics, procedures, recursion, color support |
| **Python** | Supported | Modern Python with libraries |
| **C** | Experimental | Basic C programs, limited library support |
| **Pascal** | Experimental | Structured programming |
| **Prolog** | Experimental | Facts, rules, unification |
| **Forth** | Experimental | Stack-based programming |

### Theme System

8 built-in themes + custom theme support:
- **Dracula** - Dark with purple accent
- **Monokai** - Classic code editor theme
- **Solarized Dark** - Low-contrast dark theme
- **Ocean** - Blue water-inspired palette
- **Spring** - Fresh green and pastel colors
- **Sunset** - Warm orange and red tones
- **Candy** - Bright pastel colors
- **Forest** - Deep green and brown palette
- **Custom** - User-defined color schemes

---

## Configuration

User settings are stored in `~/.Time_Warp/config.json`:

```json
{
  "theme": "dracula",
  "font_size": 12,
  "font_family": "Monaco",
  "auto_save": true,
  "auto_save_interval": 30,
  "recent_files": [
    "/path/to/file1.bas",
    "/path/to/file2.logo"
  ],
  "lesson_progress": {
    "lesson_id_1": {
      "completed": true,
      "checkpoint_3": true
    }
  }
}
```

Settings are automatically saved when changed. All paths are cross-platform compatible.

---

## Development

### Project Structure Overview

```
Time_Warp_Studio/
├── Platforms/Python/
│   ├── time_warp_ide.py             - Main entry point
│   ├── time_warp/                   - Core application (50+ modules)
│   ├── requirements.txt             - Python dependencies
│   └── test_runner.py               - Test orchestration
├── Examples/                        - 100+ example programs
├── docs/                            - Complete documentation
│   ├── guides/                      - How-to guides and tutorials
│   ├── tutorials/                   - Language tutorials
│   └── reference/                   - API and reference material
├── config/                          - Configuration files
├── Scripts/                         - Build and launch scripts
└── tests/                           - Test fixtures and utilities
```

### Running Tests

```bash
# Set up test environment
cd Platforms/Python
pip install -r requirements-dev.txt

# Run comprehensive test suite
python test_runner.py --comprehensive

# Quick smoke tests
python test_runner.py --basic

# Run specific test file
pytest tests/test_core_interpreter.py -v

# With coverage report
pytest tests/ --cov=time_warp --cov-report=html
```

**Current Test Status:**
- ✅ 55+ unit tests passing
- ✅ 30+ test files covering all major components
- ✅ Coverage reporting available
- ✅ Linting: black, flake8, mypy, pylint, ruff configured

### Adding a New Language

1. **Create executor module** in `time_warp/languages/my_lang.py`:
   ```python
   from . import LanguageExecutor
   
   class MyLangExecutor(LanguageExecutor):
       def __init__(self, interpreter):
           self.interpreter = interpreter
       
       def execute_command(self, command: str) -> str:
           # Parse and execute
           return "✅ Output\n"
   ```

2. **Register in** `time_warp/core/interpreter.py`:
   ```python
   from .languages.my_lang import MyLangExecutor
   self.my_lang = MyLangExecutor(self)
   ```

3. **Add detection logic** in `TimeWarpInterpreter.execute()`

4. **Create examples** in `Examples/my_lang/`

5. **Write tests** in `tests/test_my_lang.py`

See [ARCHITECTURE.md](ARCHITECTURE.md) for more details on extending the IDE.

### Code Style

- **Python**: PEP 8 with line length 100
- **Formatter**: black
- **Linter**: flake8, pylint
- **Type Checking**: mypy with strict mode
- **Import Sorting**: isort
- **Code Quality**: ruff

Configuration in `pyproject.toml` and `.flake8`

### Making a Contribution

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests: `pytest tests/ -v`
5. Verify linting: `flake8` (should pass)
6. Commit your changes (`git commit -m 'Add amazing feature'`)
7. Push to your branch (`git push origin feature/amazing-feature`)
8. Open a Pull Request

---

## Troubleshooting

### "Illegal instruction" Error
**Cause:** Your CPU lacks required features (SSSE3, SSE4.1, SSE4.2, POPCNT)  
**Solution:** Run on modern hardware or cloud instance

### IDE won't start
See [Troubleshooting Guide](docs/guides/08-troubleshooting.md)

### Performance Issues
Check [Performance Optimization](docs/guides/08-troubleshooting.md#performance)

---

## License

This project is licensed under the Apache License 2.0 - see [LICENSE](LICENSE) file for details.

---

## Support

- 📖 **Documentation:** See [docs/](docs/) directory
- 🐛 **Issues:** [GitHub Issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)
- 💬 **Discussions:** [GitHub Discussions](https://github.com/James-HoneyBadger/Time_Warp_Studio/discussions)
- 📧 **Email:** james@honey-badger.org

---

## Acknowledgments

Time Warp Studio builds on the legacy of educational programming languages while incorporating modern development practices. Special thanks to the BASIC, Logo, PILOT, and open-source communities for their inspiration.

---

**Made with ❤️ for educators and learners everywhere**
