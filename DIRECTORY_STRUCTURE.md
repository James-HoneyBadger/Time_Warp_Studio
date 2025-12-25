# Time Warp Studio - Directory Structure

**Last Updated:** December 25, 2025  
**Version:** 5.1.0

---

## Root Level

```
Time_Warp_Studio/
├── Code Files
│   ├── Code_of_Conduct.md          Core project governance
│   ├── README.md                   Main project overview
│   └── LICENSE                     Apache 2.0 License
│
├── Platforms/                      Language implementations
│   ├── Python/                     PySide6 IDE (primary, maintained)
│   ├── Rust/                       Rust reference implementation
│   └── Windows2000/                Legacy experimental
│
├── Examples/                       Sample programs for all languages
│   ├── basic/                      BASIC language examples
│   ├── c/                          C language examples
│   ├── forth/                      Forth language examples
│   ├── logo/                       Logo language examples
│   ├── pascal/                     Pascal language examples
│   ├── pilot/                      PILOT language examples
│   └── prolog/                     Prolog language examples
│
├── Config/                         Configuration files & assets
│
├── Scripts/                        Build, deployment, launch scripts
│   ├── build_native.sh
│   ├── build_rust.sh
│   ├── install.sh
│   ├── install-user.sh
│   ├── launch_ide.bat              Launch on Windows
│   ├── launch_ide_root.sh          Launch IDE (root level)
│   ├── launch_editor.sh            Launch standalone editor
│   ├── launch_gui.sh               Launch GUI directly
│   ├── generate_icon.py            Icon generation utility
│   └── gh_set_secrets.sh           GitHub Actions secrets
│
├── docs/                           **NEW: Organized Documentation**
│   ├── guides/                     Installation & quick-start guides
│   │   ├── QUICKSTART.md           Getting started (5 minutes)
│   │   ├── LAUNCHING.md            How to launch all IDEs
│   │   └── INSTALL_NATIVE.md       Native installation instructions
│   │
│   ├── updates/                    Version updates & changelogs
│   │   ├── VERSION_5.1.0_UPDATE.md Complete v5.1.0 changes
│   │   └── VERSION_DISPLAY_UPDATE.md UI/window title updates
│   │
│   ├── reference/                  Technical reference & structure
│   │   ├── STRUCTURE.md            System architecture overview
│   │   └── DOCUMENTATION_COMPLETE.md Doc verification report
│   │
│   ├── technical/                  Deep technical documentation
│   │   ├── api.md
│   │   └── README.md
│   │
│   ├── tutorials/                  Language tutorials & examples
│   │   ├── basic.md
│   │   ├── pilot.md
│   │   ├── logo.md
│   │   ├── pascal.md
│   │   ├── c.md
│   │   ├── forth.md
│   │   ├── prolog.md
│   │   └── README.md
│   │
│   ├── user-guide/                 User-facing documentation
│   │   └── README.md
│   │
│   └── README.md                   Documentation hub
│
└── .github/                        GitHub Actions & templates
    ├── copilot-instructions.md     AI development guidelines
    └── workflows/                  CI/CD pipelines
```

---

## Key Directories Explained

### `Platforms/Python/`
**Status:** ✅ ACTIVELY MAINTAINED  
The primary Time Warp IDE implementation using PySide6 (Qt for Python).

Structure:
```
Platforms/Python/
├── time_warp/
│   ├── core/                       Core interpreter & language executors
│   │   ├── interpreter.py          Main dispatch logic
│   │   ├── interpreters/           Language implementations
│   │   │   ├── basic.py
│   │   │   ├── pilot.py
│   │   │   ├── logo.py
│   │   │   ├── pascal.py
│   │   │   ├── prolog.py
│   │   │   ├── c.py
│   │   │   └── forth.py
│   │   ├── safe_expression_evaluator.py
│   │   └── async_support.py
│   │
│   ├── ui/                        User interface (PySide6)
│   │   ├── main_window.py          Main IDE window (2631 lines)
│   │   ├── editor.py               Code editor widget
│   │   ├── canvas.py               Graphics/turtle canvas
│   │   ├── qt_ui.py                Qt factory & theming
│   │   └── dialogs/                Dialog windows
│   │
│   ├── tools/                     Utility tools
│   │   ├── theme.py               Theme manager (23 themes)
│   │   └── code_formatter.py
│   │
│   ├── utils/                     Helper utilities
│   ├── iot/                       IoT/hardware integration
│   ├── hardware/                  Hardware abstraction
│   └── logging_config.py
│
├── tw_editor.py                   Standalone editor (379 lines)
├── Time_Warp_IDE.py              Main entry point
└── tests/                         Test suite (30+ test files)
```

### `Examples/`
Language-specific sample programs organized by type:
- **basic/** - 11 BASIC examples (hello world → guessing game)
- **pilot/** - 9 PILOT examples (interactive language)
- **logo/** - 15 Logo examples (turtle graphics)
- **pascal/** - 10 Pascal examples (procedural)
- **prolog/** - 5 Prolog examples (logic programming)
- **c/** - 8 C examples (systems programming)
- **forth/** - 5 Forth examples (stack-based)

### `Scripts/`
Utility scripts for development, building, and deployment:
- **Build scripts:** `build_native.sh`, `build_rust.sh`
- **Installation:** `install.sh`, `install-user.sh`
- **Launch scripts:** `launch_ide.bat`, `launch_ide_root.sh`, `launch_editor.sh`, `launch_gui.sh`
- **Utilities:** `generate_icon.py`, `gh_set_secrets.sh`

### `docs/` (NEW ORGANIZATION)

**Purpose:** Centralized, categorized documentation

- **guides/** - Quick-start and installation guides (new users)
- **updates/** - Version history and upgrade notes
- **reference/** - Architecture and structural documentation
- **technical/** - Deep API and implementation details
- **tutorials/** - Language-specific tutorials
- **user-guide/** - End-user documentation

---

## Configuration Files

- `Config/` - Configuration assets and settings
- `.github/` - GitHub Actions workflows and templates
- `.gitignore` - Git ignore patterns
- `.mypy_cache/` - Mypy type checking cache
- `.venv/` - Python virtual environment (local dev)

---

## Version Information

**Current Version:** 5.1.0  
**Release Date:** December 25, 2025  
**Status:** Production Ready ✅

**Key Files:**
- `docs/updates/VERSION_5.1.0_UPDATE.md` - Complete changelog
- `docs/updates/VERSION_DISPLAY_UPDATE.md` - UI version display updates

---

## Quick Navigation

**Want to...**
- 📚 **Get Started?** → `docs/guides/QUICKSTART.md`
- 🚀 **Launch the IDE?** → `docs/guides/LAUNCHING.md`
- 💻 **Install natively?** → `docs/guides/INSTALL_NATIVE.md`
- ��️ **Understand architecture?** → `docs/reference/STRUCTURE.md`
- 📖 **Learn a language?** → `docs/tutorials/`
- 🔍 **View what's new?** → `docs/updates/VERSION_5.1.0_UPDATE.md`

---

## Statistics

- **Languages Supported:** 7 (BASIC, PILOT, Logo, Pascal, Prolog, C, Forth)
- **Themes Available:** 23 color schemes
- **Example Programs:** 70+ samples across all languages
- **Main IDE (Python):** 2631 lines (main_window.py) + supporting modules
- **Test Coverage:** 30+ test files
- **Documentation:** 20+ markdown files organized by category

