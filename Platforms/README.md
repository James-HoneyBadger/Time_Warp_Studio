# Time Warp Studio - Platform Implementations

This directory contains implementations of Time Warp Studio for different platforms.

---

## 🟢 Active Platform: Python/PySide6

**Status:** Primary maintained implementation

The **Python/** subdirectory contains a native desktop application built with:
- **Python** 3.10+
- **PySide6** (Qt6 bindings for Python)
- **Pillow** for graphics support
- **pytest** for testing

### Quick Start

```bash
cd Python
pip install -r requirements.txt
python time_warp_ide.py
```

### Project Structure

```
Python/
├── time_warp_ide.py              # Main IDE entry point
├── time_warp/                    # Core application (~50+ modules)
│   ├── core/
│   │   ├── interpreter.py        # Central command dispatcher
│   │   ├── safe_expression_evaluator.py
│   │   └── [40+ support modules] # Analytics, AI, debugging, etc.
│   ├── languages/
│   │   ├── basic.py              # BASIC interpreter (core)
│   │   ├── pilot.py              # PILOT interpreter (core)
│   │   ├── logo.py               # Logo with turtle graphics (core)
│   │   ├── python.py             # Python support
│   │   ├── c.py                  # C language (experimental)
│   │   ├── pascal.py             # Pascal language (experimental)
│   │   ├── prolog.py             # Prolog language (experimental)
│   │   └── forth.py              # Forth language (experimental)
│   ├── features/
│   │   ├── lesson_system.py      # Structured lessons with checkpoints
│   │   ├── examples_browser.py   # Searchable example catalog (86+)
│   │   ├── turtle_preview.py     # Live Logo code preview
│   │   ├── theme_editor.py       # Custom theme creation (8 built-in)
│   │   ├── autosave_manager.py   # Background autosave with versioning
│   │   └── classroom_mode.py     # Presentation and assignment tools
│   ├── ui/
│   │   ├── qt_ui.py              # PySide6 UI factory
│   │   ├── main_window.py        # Main application window
│   │   ├── editor.py             # Code editor with syntax highlighting
│   │   └── canvas.py             # Turtle graphics rendering canvas
│   └── tests/
│       └── [4 test files]        # Comprehensive pytest suite (55 tests)
├── requirements.txt              # Production dependencies
├── requirements-dev.txt          # Development dependencies
├── test_runner.py                # Test orchestration with reporting
├── pyproject.toml                # Tool configuration (black, mypy, etc.)
└── README.md                     # Python platform README
```

### Supported Languages

| Language | Level | Features |
|----------|-------|----------|
| **BASIC** | Core | Variables, arrays, loops, subroutines, strings |
| **PILOT** | Core | Interactive instruction, conditional branching, lessons |
| **Logo** | Core | Turtle graphics, procedures, recursion, colors |
| **Python** | Supported | Modern Python with standard library |
| **C** | Experimental | Basic C programs, limited library |
| **Pascal** | Experimental | Structured programming, procedures |
| **Prolog** | Experimental | Facts, rules, unification, recursion |
| **Forth** | Experimental | Stack-based programming |

### Key Features

**Learning Tools:**
- 📚 Lesson System - Step-by-step guided lessons with checkpoints
- 📖 Examples Browser - Searchable catalog of 86+ example programs
- 🐢 Turtle Preview - Live visualization while coding

**Productivity:**
- 💾 Autosave - Background saving with version history (20 versions/file)
- 🎨 Theme Editor - 8 built-in themes + custom theme creation
- 🏫 Classroom Mode - Presentation mode and assignment distribution

**Development:**
- 🔍 Debugger - Step through code with breakpoints
- 📊 Analytics - Learning progress tracking and insights
- 🔄 Multiplayer - Real-time code sharing (beta)

### Dependencies

**Core:**
```
PySide6 >= 6.5.0      # Qt6 Python bindings
Pillow >= 10.0.0      # Image and graphics support
requests >= 2.28.0    # HTTP client for cloud features
```

**Development:**
```
pytest >= 7.4.0       # Testing framework
pytest-cov >= 4.0.0   # Coverage reporting
pytest-mock >= 3.11.0 # Mocking for tests
black >= 23.0.0       # Code formatter
flake8 >= 6.0.0       # Style linter
mypy >= 1.0.0         # Type checker
pylint >= 2.17.0      # Static analyzer
ruff >= 0.1.0         # Fast linter
isort >= 5.11.0       # Import sorter
```

### Testing

```bash
# Comprehensive test suite
python test_runner.py --comprehensive

# Quick smoke tests
python test_runner.py --basic

# Specific test file
pytest tests/test_core_interpreter.py -v

# With coverage
pytest tests/ --cov=time_warp --cov-report=html
```

**Current Status:**
- ✅ 55+ unit tests passing
- ✅ All major components tested
- ✅ Coverage reporting available
- ✅ CI/CD integration ready

### Running the IDE

```bash
# Basic launch
python time_warp_ide.py

# With debug output
python time_warp_ide.py --debug

# Specify working directory
python time_warp_ide.py --dir /path/to/projects
```

### Configuration

User settings stored in `~/.Time_Warp/config.json`:

```json
{
  "theme": "dracula",
  "font_size": 12,
  "font_family": "Monaco",
  "auto_save": true,
  "auto_save_interval": 30,
  "recent_files": [],
  "lesson_progress": {}
}
```

### Development

For detailed development information, see:
- [Architecture Guide](../../ARCHITECTURE.md) - System design and extending the IDE
- [Launch Guide](../../LAUNCH_GUIDE.md) - Setup and first use
- [Full Documentation](../../docs/) - Complete documentation

---

## 🔵 Archived Platforms

The following platforms are archived but remain in the repository for reference:

### web/ - WebAssembly/Web Version (Archived)
- Original plan for browser-based Time Warp Studio
- Status: Experimental, not maintained
- See [WASM_IMPLEMENTATION.md](WASM_IMPLEMENTATION.md) for details

### mobile/ - Mobile Implementation (Archived)
- Original plan for iOS/Android versions
- Status: Not implemented
- Would require React Native or Flutter

### backend/ - Cloud Backend (Archived)
- Cloud sync and collaboration backend
- Status: Experimental
- Not required for desktop version

---

## 🚀 Recommended Workflow

1. **Development:**
   ```bash
   cd Python
   python -m venv .venv
   source .venv/bin/activate
   pip install -r requirements-dev.txt
   python test_runner.py --comprehensive  # Verify setup
   python time_warp_ide.py                # Run IDE
   ```

2. **Making Changes:**
   - Edit code in `time_warp/` directory
   - Run tests: `pytest tests/ -v`
   - Check style: `flake8`
   - Format code: `black .`

3. **Contributing:**
   - Create feature branch
   - Make changes with tests
   - Run full test suite
   - Submit pull request

---

## 📊 Platform Comparison

| Aspect | Python (Active) | Web (Archived) | Mobile (Archived) |
|--------|:---------------:|:--------------:|:-----------------:|
| Status | ✅ Maintained | ⚠️ Archived | ❌ Not implemented |
| Native GUI | Yes (PySide6) | Browser | N/A |
| Offline | Yes | No | N/A |
| All Languages | Yes | Planned | N/A |
| Turtle Graphics | Yes | Planned | N/A |
| Performance | Excellent | Good | N/A |
| Deployment | Standalone binary | Web server | N/A |

---

## 🔗 Related Documentation

- **Main README:** [../../README.md](../../README.md)
- **Architecture Guide:** [../../ARCHITECTURE.md](../../ARCHITECTURE.md)
- **Launch Guide:** [../../LAUNCH_GUIDE.md](../../LAUNCH_GUIDE.md)
- **Documentation Index:** [../../docs/INDEX.md](../../docs/INDEX.md)
- **Examples:** [../../Examples/README.md](../../Examples/README.md)

---

## 🤝 Contributing

Contributions are welcome! See main [README.md](../../README.md#development) for guidelines.

---

**Last Updated:** January 2026  
**Primary Implementation:** Python with PySide6 (Qt6)  
**Version:** 7.0.0+  
**Maintainer:** James Temple <james@honey-badger.org>
