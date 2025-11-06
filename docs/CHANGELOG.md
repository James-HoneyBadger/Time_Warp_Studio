# Changelog

All notable changes to Time Warp IDE will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.0.0] - 2025-10-28

### Major Release - Feature Complete Implementation

This release represents the complete, production-ready Time Warp IDE with full feature parity between Python and Rust implementations.

### Added

#### Core Language Features
- **TempleCode Unified Language**: Seamlessly mix BASIC, PILOT, and Logo in single programs
- **Logo Procedures**: User-defined procedures with parameters (`TO name :param ... END`)
- **Multi-line REPEAT Blocks**: Support for nested and top-level multi-line loops
- **Expression Evaluation**: Safe math parser with 15+ functions (SIN, COS, SQRT, etc.)
- **Named Colors**: 14 built-in color names (red, blue, green, yellow, cyan, magenta, orange, purple, pink, brown, gray, white, black, lime)
- **Color Formats**: Hex colors (#RRGGBB) and RGB colors (r, g, b)
- **50+ Turtle Commands**: Complete turtle graphics system verified and tested

#### Python Implementation
- **PySide6 Desktop IDE**: Modern Qt6-based GUI with full feature set
- **8 Color Themes**: Dracula, Monokai, Solarized Dark/Light, Ocean, Spring, Sunset, Candy, Forest
- **Recent Files Menu**: Track last 10 opened files
- **Multi-tab Output**: Separate Text and Graphics tabs with auto-switch
- **Syntax Highlighting**: Keyword coloring for all three languages
- **Line Numbers**: Dynamic line number gutter in code editor
- **Zoom Controls**: Editor zoom in/out (Ctrl+Plus/Minus)
- **Find/Replace**: Search and replace functionality (F3)
- **Canvas Controls**: Pan, zoom, and clear turtle graphics
- **Auto-clear**: Output and graphics clear on each run
- **Error Hints**: Typo suggestions with Levenshtein distance matching (100+ suggestions)
- **Expression Caching**: Performance optimization for repeated expressions
- **Security Limits**: 100K iteration limit, 10-second timeout

#### Rust Implementation
- **egui Native GUI**: High-performance native interface
- **Async Execution**: tokio-based non-blocking program execution
- **PNG Export**: Save turtle graphics canvas as PNG images
- **Experimental Compiler**: TempleCode → C transpiler (text-mode subset)
- **Step Debugger**: Line-by-line execution with breakpoints
- **Unified Screen**: Combined text and graphics rendering
- **Input Dialogs**: Modal prompts for INPUT and A: commands
- **INKEY$ Support**: Real-time keyboard detection for games
- **Theme System**: 8 color themes matching Python version

#### Documentation
- **Comprehensive Docs**: 76+ markdown files covering all aspects
- **User Guide**: Unified guide for both Python and Rust implementations
- **Quick Reference**: Complete command reference for all languages
- **Turtle Graphics Reference**: Full documentation of 50+ turtle commands
- **Student Guide**: Language cheatsheets and learning challenges
- **Teacher Guide**: 8-week curriculum with session outlines
- **Lesson Plans**: Structured learning path for middle school students
- **Programming Challenges**: 12 challenges with solutions
- **Developer Reference**: API documentation and extension guide
- **Architecture Docs**: System design and implementation details

#### Examples
- **33 Example Programs**: Organized by language and difficulty
  - 15 Logo programs (graphics, fractals, patterns)
  - 10 BASIC programs (games, utilities, demos)
  - 7 PILOT programs (quizzes, adventures, calculators)
  - 1 TempleCode mixed-language demo
- **Categorized by Difficulty**: Beginner, Intermediate, Advanced

#### Developer Experience
- **Root Launcher**: `./run.sh python` or `./run.sh rust` to launch from repo root
- **CLI Support**: Run programs from command line (Python)
- **Test Suite**: 5 Python test scripts, 22 Rust integration tests
- **Code Documentation**: Comprehensive inline comments and docstrings
- **Type Safety**: Full type annotations in Python, strict Rust types

### Changed

- **Python Version**: Updated from 2.0.0-alpha to 2.0.0 (stable release)
- **Documentation Structure**: Consolidated and organized all docs
- **Folder Organization**: Cleaned up duplicate files and outdated status docs
- **Output Behavior**: IDEs now start with blank output (no welcome banner)
- **Clear on Run**: Output and graphics auto-clear before each execution
- **Example Count**: Corrected documentation to reflect 33 examples (not 32)
- **Theme Count**: Updated to show 9 themes in Python (added Forest theme)

### Fixed

- **Logo Procedures**: TO/END procedure definitions now work correctly
- **SETCOLOR**: Named colors (blue, red, etc.) now recognized
- **PENWIDTH Aliases**: All aliases work (PENWIDTH, SETPENWIDTH, SETPW, SETPENSIZE)
- **BACKWARD/CLEAR**: Missing command aliases added
- **Multi-line REPEAT**: Now works at top level and nested contexts
- **Expression in Commands**: RIGHT 360 / :SIDES now evaluates correctly
- **Parameter Binding**: Logo procedure parameters (:param) work in all contexts
- **Documentation Errors**: Fixed outdated status claims about GUI and features

### Security

- **Safe Expression Evaluation**: No eval() or exec() in Python implementation
- **Iteration Limits**: 100,000 iterations maximum per execution
- **Execution Timeout**: 10-second hard limit prevents infinite loops
- **Token Limits**: 1,000 token limit in expression evaluator
- **Input Validation**: All user input sanitized and validated

### Performance

- **Regex Optimization**: Lazy compilation patterns (5-10x speedup)
- **Expression Caching**: Repeated expressions evaluated once
- **O(n) Interpolation**: Efficient variable substitution
- **Native Compilation**: Rust version compiled for maximum speed

## [1.0.0] - 2024-12-01

### Initial Release

- Basic PILOT language support
- BASIC interpreter foundation
- Logo turtle graphics prototype
- CLI execution mode
- Initial documentation

---

## Release Notes

### Python vs Rust Feature Parity

Both implementations now have complete feature parity:
- ✅ All 50+ turtle graphics commands
- ✅ Logo procedures with parameters
- ✅ Multi-line REPEAT blocks
- ✅ Named colors and hex/RGB support
- ✅ Expression evaluation with functions
- ✅ Error hints and typo suggestions
- ✅ 8 color themes
- ✅ Recent files management
- ✅ Syntax highlighting
- ✅ Find/replace functionality

### Known Limitations

- Rust compiler (experimental) only supports text-mode subset
- Python implementation lacks PNG export (Rust only)
- No network I/O support in either version
- File I/O limited to program loading/saving

### Upgrade Guide

**From 1.x to 2.0:**
1. No breaking changes - all 1.x programs run unchanged
2. New features available immediately
3. Update imports if using Python API directly
4. Rust users: recompile with `cargo build --release`

### Installation

**Python:**
```bash
pip install PySide6 pillow
python Time_Warp_Python/time_warp_ide.py
```

**Rust:**
```bash
cd Time_Warp_Rust
cargo run --release
```

**Quick Launch (from root):**
```bash
./run.sh python    # Launch Python IDE
./run.sh rust      # Launch Rust IDE
```

### Contributors

- James Temple - Project Lead, Primary Developer

### Links

- [GitHub Repository](https://github.com/James-HoneyBadger/Time_Warp)
- [User Guide](USER_GUIDE.md)
- [Python README](Time_Warp_Python/README.md)
- [Rust README](Time_Warp_Rust/README.md)
- [Documentation](Time_Warp_Rust/docs/)

---

**Full Changelog**: https://github.com/James-HoneyBadger/Time_Warp/compare/v1.0.0...v2.0.0
