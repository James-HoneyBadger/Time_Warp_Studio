# Developer Guide

Welcome to Time Warp IDE development! This guide will help you contribute to the project.

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Architecture](#architecture)
3. [Development Setup](#development-setup)
4. [Code Organization](#code-organization)
5. [Contributing](#contributing)
6. [Testing](#testing)
7. [Building](#building)
8. [Release Process](#release-process)

---

## Project Overview

### What is Time Warp IDE?

Time Warp IDE is an educational programming environment supporting six classic languages:
- BASIC (1964)
- PILOT (1968)
- Logo (1967)
- Pascal (1970)
- Prolog (1972)
- C (1972)

### Design Philosophy

1. **Educational First**: Every decision prioritizes learning
2. **Multi-Paradigm**: Expose students to different programming styles
3. **Clear Feedback**: Error messages explain, don't confuse
4. **Visual Learning**: Built-in turtle graphics make concepts tangible
5. **Cross-Platform**: Same experience on Linux, macOS, Windows

### Technology Stack

**Primary Implementation (Rust)**:
- GUI: egui (immediate-mode GUI)
- Graphics: egui painter with custom turtle rendering
- File I/O: rfd (cross-platform file dialogs)
- Build: Cargo

**Legacy Implementation (Python)**:
- GUI: PySide6 (Qt6 bindings)
- Graphics: QPainter with custom canvas
- Testing: pytest with coverage
- Package: setuptools with pyproject.toml

---

## Architecture

### High-Level Structure

```
Time_Warp/
├── platforms/
│   ├── rust/          # Primary Rust implementation
│   ├── python/        # Legacy Python implementation
│   ├── web/           # WebAssembly version (future)
│   ├── amiga/         # Retro Amiga port
│   └── ...
├── core-spec/         # Language specifications
├── examples/          # Sample programs
├── docs-new/          # Documentation (this folder)
└── tests/             # Cross-platform integration tests
```

### Rust Implementation

**Core Components**:

```rust
// src/main.rs - Entry point
fn main() {
    let options = eframe::NativeOptions::default();
    eframe::run_native(
        "Time Warp IDE",
        options,
        Box::new(|_cc| Box::new(TimeWarpApp::default())),
    );
}

// src/app.rs - Main application state
struct TimeWarpApp {
    code: String,
    output: String,
    current_language: Language,
    turtle_state: TurtleState,
    // ...
}

// src/interpreter/ - Language executors
mod basic;
mod pilot;
mod logo;
mod pascal;
mod prolog;
mod c_lang;
```

**Key Design Patterns**:

1. **Stateless Executors**: Language interpreters don't maintain UI state
2. **Command Pattern**: Each statement is parsed and executed independently
3. **Visitor Pattern**: AST traversal for complex languages (Pascal, C)
4. **Observable State**: Turtle state updated via callbacks

### Python Implementation

**Core Components**:

```python
# platforms/python/time_warp/core/interpreter.py
class TimeWarpInterpreter:
    def __init__(self):
        self.basic = BasicExecutor(self)
        self.pilot = PilotExecutor(self)
        self.logo = LogoExecutor(self)
        # ...
    
    def execute(self, code: str, language: str) -> str:
        # Route to appropriate executor
```

**Language Executors**:
```
platforms/python/time_warp/
├── languages/
│   ├── basic.py          # BASIC interpreter
│   ├── pilot.py          # PILOT interpreter
│   ├── logo.py           # Logo interpreter
│   ├── pascal.py         # Pascal interpreter
│   ├── prolog.py         # Prolog interpreter
│   └── c_lang_fixed.py   # C interpreter
```

**UI Layer**:
```
platforms/python/time_warp/
├── ui/
│   ├── qt_ui.py       # Main Qt window
│   ├── canvas.py      # Turtle graphics widget
│   └── output.py      # Output panel with threading
```

### Language Executor Pattern

All executors follow this interface:

```rust
pub trait LanguageExecutor {
    fn execute_command(&mut self, command: &str) -> Result<String, String>;
    fn reset(&mut self);
}
```

Or in Python:
```python
class LanguageExecutor:
    def execute_command(self, command: str) -> str:
        """
        Execute a single command or line of code.
        Returns output string with emoji prefixes:
        - ❌ for errors
        - ✅ for success confirmations
        - ℹ️ for informational messages
        - 🐢 for turtle graphics actions
        """
        pass
```

**Critical Rule**: Executors return text output only. All UI updates (turtle position, canvas changes) happen in the main application, not the executor.

---

## Development Setup

### Prerequisites

**Rust Development**:
```bash
# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Verify
rustc --version
cargo --version
```

**Python Development**:
```bash
# Python 3.10+ required
python3 --version

# Create virtual environment
python3 -m venv .venv
source .venv/bin/activate  # Linux/macOS
# or
.venv\Scripts\activate  # Windows
```

### Clone Repository

```bash
git clone https://github.com/honey-badger-org/Time_Warp.git
cd Time_Warp
```

### Rust Setup

```bash
cd platforms/rust

# Build
cargo build

# Run
cargo run

# Run with release optimizations
cargo run --release
```

### Python Setup

```bash
# Install development dependencies
pip install -e "./platforms/python[dev]"

# This installs:
# - time-warp-ide (editable)
# - pytest, pytest-cov, pytest-mock
# - black, flake8, mypy, pylint
# - All runtime dependencies
```

### IDE Configuration

**VS Code** (recommended):
```bash
# Install extensions
code --install-extension rust-lang.rust-analyzer
code --install-extension ms-python.python

# Open workspace
code Time_Warp.code-workspace
```

**PyCharm**:
1. Open `Time_Warp/` as project
2. Configure Python interpreter → Use .venv
3. Mark `platforms/python` as Sources Root

---

## Code Organization

### Rust Structure

```
platforms/rust/
├── src/
│   ├── main.rs              # Entry point
│   ├── app.rs               # Main application
│   ├── interpreter/
│   │   ├── mod.rs           # Executor trait
│   │   ├── basic.rs         # BASIC language
│   │   ├── pilot.rs         # PILOT language
│   │   ├── logo.rs          # Logo language
│   │   ├── pascal.rs        # Pascal language
│   │   ├── prolog.rs        # Prolog language
│   │   └── c_lang.rs        # C language
│   ├── turtle.rs            # Turtle graphics state
│   └── ui/
│       ├── mod.rs
│       ├── editor.rs        # Code editor widget
│       ├── canvas.rs        # Graphics canvas
│       └── menu.rs          # Menu bar
├── Cargo.toml               # Dependencies
└── Cargo.lock
```

### Python Structure

```
platforms/python/
├── time_warp/
│   ├── __init__.py
│   ├── __main__.py          # Entry point
│   ├── core/
│   │   ├── interpreter.py   # Main interpreter
│   │   ├── safe_expression_evaluator.py
│   │   └── async_support.py
│   ├── languages/
│   │   ├── basic.py
│   │   ├── pilot.py
│   │   ├── logo.py
│   │   ├── pascal.py
│   │   ├── prolog.py
│   │   └── c_lang_fixed.py
│   ├── ui/
│   │   ├── qt_ui.py
│   │   ├── canvas.py
│   │   └── output.py
│   └── tools/
│       └── theme.py
├── tests/                   # Unit tests
├── pyproject.toml          # Package config
└── setup.py                # Setup script
```

### Configuration Files

**Rust**:
- `Cargo.toml` - Dependencies and project metadata
- `Cargo.lock` - Locked dependency versions

**Python**:
- `pyproject.toml` - Package configuration, tool settings
- `setup.py` - Installation script
- `config/.flake8` - Flake8 linter configuration
- `config/.pylintrc` - Pylint configuration

**VS Code**:
- `.vscode/settings.json` - Workspace settings
- `.vscode/extensions.json` - Recommended extensions

---

## Contributing

### Getting Started

1. **Fork** the repository
2. **Clone** your fork
3. **Create** a feature branch
4. **Make** your changes
5. **Test** thoroughly
6. **Submit** a pull request

### Branch Naming

- `feature/description` - New features
- `fix/description` - Bug fixes
- `docs/description` - Documentation
- `refactor/description` - Code improvements
- `test/description` - Test additions

Examples:
- `feature/add-forth-language`
- `fix/turtle-coordinate-bug`
- `docs/update-installation-guide`

### Commit Messages

Follow conventional commits:

```
type(scope): description

[optional body]

[optional footer]
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation
- `style`: Formatting
- `refactor`: Code restructuring
- `test`: Test additions
- `chore`: Maintenance

Examples:
```
feat(logo): add SETPENSIZE command

Implements variable pen thickness for turtle graphics.
Closes #123

fix(basic): handle empty INPUT correctly

Previously crashed on empty input. Now treats as empty string.

docs(readme): update installation instructions

Add macOS Homebrew installation method.
```

### Code Style

**Rust**:
```bash
# Format code
cargo fmt

# Lint
cargo clippy

# Both required before PR
```

**Python**:
```bash
# Format code
black platforms/python

# Lint
flake8 platforms/python

# Type check
mypy platforms/python

# All required before PR
```

**Consistent Style**:
- Indentation: 4 spaces (Python), 4 spaces (Rust)
- Line length: 100 characters max
- Comments: Clear, explain why not what
- Docstrings: All public functions/classes

### Adding a New Language

1. **Create executor file**:
```rust
// platforms/rust/src/interpreter/new_language.rs
pub struct NewLanguageExecutor {
    variables: HashMap<String, Value>,
}

impl LanguageExecutor for NewLanguageExecutor {
    fn execute_command(&mut self, command: &str) -> Result<String, String> {
        // Parse and execute
        Ok("✅ Output\n".to_string())
    }
    
    fn reset(&mut self) {
        self.variables.clear();
    }
}
```

2. **Register in interpreter**:
```rust
// src/interpreter/mod.rs
pub mod new_language;
use new_language::NewLanguageExecutor;
```

3. **Add to main app**:
```rust
// src/app.rs
enum Language {
    // existing...
    NewLanguage,
}

impl TimeWarpApp {
    fn execute(&mut self) {
        match self.current_language {
            // existing...
            Language::NewLanguage => {
                let result = self.new_language_executor.execute_command(&self.code);
                // handle result
            }
        }
    }
}
```

4. **Add tests**:
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_basic_functionality() {
        let mut exec = NewLanguageExecutor::new();
        let result = exec.execute_command("TEST COMMAND");
        assert!(result.is_ok());
    }
}
```

5. **Add examples**:
```
examples/new_language/
├── hello_world.nlang
├── loops.nlang
└── procedures.nlang
```

6. **Document**:
- Add language section to `docs-new/user/01-programming-guide.md`
- Create reference in `docs-new/reference/00-languages.md`
- Update main README

---

## Testing

### Rust Tests

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_basic_print

# With output
cargo test -- --nocapture

# With coverage (requires tarpaulin)
cargo install cargo-tarpaulin
cargo tarpaulin --out Html
```

### Python Tests

```bash
# Run all tests
pytest

# Run specific file
pytest tests/test_basic.py

# With coverage
pytest --cov=time_warp --cov-report=html

# Verbose
pytest -v

# Stop on first failure
pytest -x
```

### Test Organization

**Rust**:
- Unit tests: In same file as code (bottom of file)
- Integration tests: `platforms/rust/tests/`

**Python**:
- Unit tests: `platforms/python/tests/test_*.py`
- Integration tests: `platforms/python/tests/*_test.py`

### Writing Good Tests

```rust
#[test]
fn test_descriptive_name() {
    // Arrange
    let mut executor = BasicExecutor::new();
    let command = "PRINT \"Hello\"";
    
    // Act
    let result = executor.execute_command(command);
    
    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "Hello\n");
}
```

```python
def test_descriptive_name():
    """Test that PRINT outputs correct string"""
    # Arrange
    executor = BasicExecutor()
    
    # Act
    result = executor.execute_command('PRINT "Hello"')
    
    # Assert
    assert result == "Hello\n"
```

**Test Coverage Goals**:
- Core interpreter: 90%+
- Language executors: 85%+
- UI code: 60%+ (harder to test)
- Overall: 80%+

---

## Building

### Rust Build

```bash
# Debug build (fast compilation, slower execution)
cargo build

# Release build (optimized)
cargo build --release

# Executable location:
# target/debug/time-warp-ide
# target/release/time-warp-ide
```

### Python Distribution

```bash
# Build wheel
cd platforms/python
python -m build

# Creates:
# dist/time_warp_ide-2.1.0-py3-none-any.whl
# dist/time_warp_ide-2.1.0.tar.gz

# Install wheel
pip install dist/time_warp_ide-2.1.0-py3-none-any.whl
```

### Platform-Specific Builds

**macOS App Bundle**:
```bash
cd scripts
./build_macos_app.sh
```

**Windows Installer**:
```bash
# Requires NSIS
cd packaging/windows
makensis installer.nsi
```

**Linux Packages**:
```bash
# Debian/Ubuntu
cd packaging/debian
dpkg-buildpackage -us -uc

# Arch Linux
cd packaging/arch
makepkg
```

---

## Release Process

### Version Numbering

Follow Semantic Versioning (semver):
- MAJOR.MINOR.PATCH (e.g., 2.1.0)
- MAJOR: Breaking changes
- MINOR: New features (backward compatible)
- PATCH: Bug fixes

### Creating a Release

1. **Update version** numbers:
   - `platforms/rust/Cargo.toml`
   - `platforms/python/pyproject.toml`
   - `README.md`

2. **Update changelog**:
   ```markdown
   ## [2.2.0] - 2025-02-01
   
   ### Added
   - Forth language support
   - Syntax highlighting improvements
   
   ### Fixed
   - Turtle coordinate calculation bug
   - Input dialog focus issue
   ```

3. **Run full test suite**:
   ```bash
   cargo test --release
   pytest platforms/python
   ```

4. **Build all platforms**:
   ```bash
   # Rust
   cargo build --release
   
   # Python
   cd platforms/python
   python -m build
   
   # Platform packages
   ./scripts/build_macos_app.sh
   # etc.
   ```

5. **Create git tag**:
   ```bash
   git tag -a v2.2.0 -m "Release version 2.2.0"
   git push origin v2.2.0
   ```

6. **GitHub Release**:
   - Go to GitHub Releases
   - Create new release from tag
   - Upload build artifacts
   - Copy changelog content

7. **Announce**:
   - Update website
   - Post in discussions
   - Social media
   - Email newsletter (if applicable)

---

## Resources

### Documentation
- [Rust Book](https://doc.rust-lang.org/book/)
- [egui Documentation](https://docs.rs/egui/)
- [PySide6 Documentation](https://doc.qt.io/qtforpython/)
- [pytest Documentation](https://docs.pytest.org/)

### Community
- [GitHub Discussions](https://github.com/honey-badger-org/Time_Warp/discussions)
- [Issue Tracker](https://github.com/honey-badger-org/Time_Warp/issues)

### Tools
- [Rust Playground](https://play.rust-lang.org/)
- [Python REPL](https://www.python.org/shell/)

---

## Getting Help

**Questions?**
- Search [existing issues](https://github.com/honey-badger-org/Time_Warp/issues)
- Ask in [Discussions](https://github.com/honey-badger-org/Time_Warp/discussions)
- Read the [FAQ](../user/03-faq.md)

**Found a bug?**
- Check if already reported
- Create new issue with:
  - Steps to reproduce
  - Expected behavior
  - Actual behavior
  - System information
  - Code samples

**Want a feature?**
- Search existing feature requests
- Create new issue describing:
  - Use case
  - Proposed solution
  - Alternatives considered

---

Thank you for contributing to Time Warp IDE!
