# Developer Guide# TempleCode & Time Warp v3.0.0 - Developer Guide



**Time Warp IDE - Development Documentation**This guide is for contributors who want to understand, extend, and maintain TempleCode (the interpreter) and Time Warp v3.0.0 (the multi-platform GUI IDE).



**Document Type:** Developer Guide  ## Version 3.0.0 - Multi-Platform Architecture

**Audience:** Contributors and Developers  

**Last Updated:** November 18, 2025Time Warp v3.0.0 is available on 6 platforms with distinct implementations:



---- **Python** (`Python/`) - Original Tkinter GUI

- **Rust** (`Rust/`) - Cross-platform eframe/egui with shared engine

## Table of Contents- **Windows** (`Windows/`) - Native Windows `.exe` with installer

- **Apple** (`Apple/`) - Native macOS `.app` bundle

- [Overview](#overview)- **DOS** (`DOS/`) - DJGPP/Mode 13h VGA retro implementation

- [Architecture](#architecture)- **Browser** (`Browser/`) - WebAssembly/JavaScript web version

- [Development Setup](#development-setup)

- [Platform Implementations](#platform-implementations)For complete documentation, see:

- [Code Structure](#code-structure)- User Guide: `docs/user_guide.md`

- [Contributing](#contributing)- Technical Reference: `docs/technical_reference.md`

- [Testing](#testing)- Student Handbook: `docs/student_handbook.md`

- [Release Process](#release-process)- Teacher Guide: `docs/teacher_guide.md`



---## Repository Structure



## OverviewSee README.md for platform-specific build instructions.



This guide is for contributors who want to understand, extend, and maintain TempleCode (the language interpreter) and Time Warp IDE (the multi-platform development environment).## Architecture



**Project Goals:**### Python Implementation

- Educational programming environment for all skill levels- Original Tkinter GUI

- Cross-platform consistency with platform-specific optimizations- AST-based safe expression evaluator

- Clean, maintainable codebase- Pluggable I/O and Turtle abstractions

- Comprehensive test coverage

- Active community contributions### Rust Implementation

- High-performance with evalexpr

**Key Technologies:**- Shared by Rust, Windows, Apple via Cargo path dependency

- **Rust:** egui/eframe for primary GUI- Thread-safe with Arc<Mutex<T>>

- **Python:** PySide6 for educational variant

- **Go:** Fyne for CLI and alternative GUI### DOS Implementation

- **WebAssembly:** Browser-based execution- Standalone C for DJGPP

- **C:** DOS/retro implementations- Direct VGA Mode 13h graphics

- DPMI BIOS calls, nearptr VRAM

---

## Contributing

## Architecture

1. Fork repository

### Design Principles2. Create feature branch  

3. Add tests

Time Warp IDE follows these core architectural principles:4. Update docs

5. Submit PR

1. **Stateless Executors** - Language interpreters are stateless command processors

2. **UI-Centric State** - All display state (turtle canvas, themes) lives in the application, not the interpreterAll platforms should remain functional.

3. **Emoji Prefixes** - Standardized output markers (❌ error, ✅ success, ℹ️ info, 🎨 theme, 🚀 run, 🐢 turtle)

4. **Safe Evaluation** - No `eval()` or code injection risks## License

5. **Educational Focus** - Clear error messages, beginner-friendly design

MIT

### Language Executor Pattern

Each language executor follows this pattern:

```rust
pub trait LanguageExecutor {
    fn execute_command(&mut self, command: &str) -> Result<String, String>;
    fn reset(&mut self);
}
```

**Key Points:**
- Executors parse commands and update internal state
- Return text output with emoji prefixes
- Never store UI components or graphics contexts
- UI reads executor state for rendering

### Project Structure

```
Time_Warp/
├── Time_Warp_Rust/          # Primary implementation
│   ├── src/
│   │   ├── main.rs          # Entry point
│   │   ├── app.rs           # Main application state
│   │   ├── interpreter/     # Language executors
│   │   ├── graphics/        # Turtle graphics engine
│   │   ├── ui/              # UI components
│   │   └── utils/           # Utilities
│   ├── Cargo.toml           # Dependencies
│   └── tests/               # Unit tests
│
├── Time_Warp_Python/        # Python implementation
│   ├── core/
│   │   ├── interpreter.py   # Main interpreter
│   │   └── interpreters/    # Language modules
│   ├── ui/                  # PySide6 UI
│   └── tests/               # Python tests
│
├── Time_Warp_Go/            # Go implementation
├── Time_Warp_Web/           # WebAssembly version
├── Time_Warp_DOS/           # DOS/retro version
├── docs/                    # Documentation
├── examples/                # Sample programs
├── core-spec/               # Language specification
└── tests/                   # Integration tests
```

---

## Development Setup

### Rust Development

**Prerequisites:**
- Rust 1.70 or newer
- Cargo (comes with Rust)
- Git

**Setup:**

```bash
# Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/Time_Warp_Rust

# Build debug version
cargo build

# Run tests
cargo test

# Run IDE
cargo run

# Build release version
cargo build --release
```

**Recommended Tools:**
- rust-analyzer (LSP)
- clippy (linting)
- rustfmt (formatting)

### Python Development

**Prerequisites:**
- Python 3.10 or newer
- pip

**Setup:**

```bash
cd Time_Warp/Time_Warp_Python

# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements-dev.txt

# Run tests
pytest

# Run IDE
python run_time_warp.py
```

### Go Development

**Prerequisites:**
- Go 1.20 or newer

**Setup:**

```bash
cd Time_Warp/Time_Warp_Go

# Build
go build ./cmd/timewarp

# Run tests
go test ./...

# Run CLI
./timewarp --help
```

---

## Platform Implementations

### Rust Implementation (Primary)

**Location:** `Time_Warp_Rust/`

**Architecture:**
- **UI Framework:** egui/eframe
- **Graphics:** egui painter with custom turtle renderer
- **Interpreter:** Modular executor system
- **Themes:** 8 built-in color schemes

**Key Files:**
- `src/main.rs` - Application entry point
- `src/app.rs` - Main app struct and UI logic
- `src/interpreter/mod.rs` - Interpreter core
- `src/languages/` - BASIC, PILOT, Logo executors
- `src/graphics/turtle.rs` - Turtle graphics engine

**Building:**

```bash
cargo build --release
./target/release/time-warp
```

### Python Implementation

**Location:** `Time_Warp_Python/`

**Architecture:**
- **UI Framework:** PySide6
- **Graphics:** QPainter for turtle graphics
- **Interpreter:** Python-based executor system

**Key Files:**
- `run_time_warp.py` - Entry point
- `core/interpreter.py` - Main interpreter (1500+ lines)
- `core/interpreters/` - Language modules
- `ui/qt_ui.py` - Qt UI factory

**Running:**

```bash
python run_time_warp.py
```

### Go Implementation

**Location:** `Time_Warp_Go/`

**Features:**
- Fast CLI interpreter
- Optional Fyne GUI
- Batch processing support

**Usage:**

```bash
# Interactive mode
./timewarp

# Batch mode
echo "PRINT 'Hello'" | ./timewarp --batch BASIC
```

### Web Implementation

**Location:** `Time_Warp_Web/`

**Technologies:**
- WebAssembly (compiled from Rust)
- JavaScript UI
- HTML5 Canvas for graphics

**Building:**

```bash
cd Time_Warp_Web
# See Time_Warp_Web/README.md for details
```

### DOS Implementation

**Location:** `Time_Warp_DOS/`

**Features:**
- DJGPP C implementation
- VGA Mode 13h graphics (320x200, 256 colors)
- Runs on FreeDOS and MS-DOS

**Building:**

```bash
# Requires DJGPP toolchain
cd Time_Warp_DOS
# See BUILD.md for details
```

---

## Code Structure

### Interpreter Core

The interpreter follows a command dispatch pattern:

```rust
pub struct TimeWarpInterpreter {
    // Language executors
    basic_executor: BasicExecutor,
    pilot_executor: PilotExecutor,
    logo_executor: LogoExecutor,
    
    // State
    variables: HashMap<String, Value>,
    output_buffer: String,
}

impl TimeWarpInterpreter {
    pub fn execute(&mut self, command: &str) -> Result<String, String> {
        // Detect language and route to executor
        let lang = self.detect_language(command);
        match lang {
            Language::BASIC => self.basic_executor.execute_command(command),
            Language::PILOT => self.pilot_executor.execute_command(command),
            Language::Logo => self.logo_executor.execute_command(command),
        }
    }
}
```

### Adding New Language Features

**Step 1:** Define command in appropriate executor:

```rust
// In src/languages/basic.rs
impl BasicExecutor {
    fn execute_new_command(&mut self, args: &str) -> Result<String, String> {
        // Parse args
        // Execute logic
        // Return output with emoji prefix
        Ok("✅ Command executed\n".to_string())
    }
}
```

**Step 2:** Update command parser:

```rust
fn parse_command(&self, line: &str) -> Command {
    let upper = line.to_uppercase();
    if upper.starts_with("NEWCMD") {
        Command::NewCommand(line[6..].trim().to_string())
    } else {
        // ... other commands
    }
}
```

**Step 3:** Add tests:

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_new_command() {
        let mut executor = BasicExecutor::new();
        let result = executor.execute_command("NEWCMD args");
        assert!(result.is_ok());
    }
}
```

**Step 4:** Update documentation:

- Add to `docs/PROGRAMMING_GUIDE.md`
- Update examples in `examples/`
- Document in `core-spec/language.md`

### Turtle Graphics Integration

Turtle graphics state is managed separately from executors:

```rust
pub struct TurtleState {
    pub x: f32,
    pub y: f32,
    pub angle: f32,
    pub pen_down: bool,
    pub color: Color32,
    pub lines: Vec<Line>,
}
```

**Executors update turtle state:**

```rust
// In executor
self.turtle_x += distance * self.turtle_angle.cos();
self.turtle_y += distance * self.turtle_angle.sin();
return Ok("🐢 Moved forward\n".to_string());
```

**UI reads and renders:**

```rust
// In app.rs
ui.painter().line_segment(
    [pos2(line.x1, line.y1), pos2(line.x2, line.y2)],
    Stroke::new(line.width, line.color),
);
```

---

## Contributing

### Contribution Workflow

1. **Fork** the repository on GitHub
2. **Create** a feature branch: `git checkout -b feature/my-feature`
3. **Make** your changes with clear commit messages
4. **Test** thoroughly (run all tests)
5. **Document** your changes (update docs, add examples)
6. **Submit** a pull request with description

### Code Standards

**Rust:**
- Follow Rust conventions (rustfmt)
- Use `clippy` for linting
- Document public APIs
- Add unit tests for new features

```bash
# Format code
cargo fmt

# Check linting
cargo clippy

# Run tests
cargo test
```

**Python:**
- Follow PEP 8 style guide
- Use type hints where possible
- Add docstrings to functions
- Use pytest for testing

```bash
# Format code
black .

# Type check
mypy .

# Run tests
pytest
```

### Documentation Requirements

Every contribution should include:

- **Code comments** explaining complex logic
- **API documentation** for public functions
- **User documentation** if adding commands
- **Examples** demonstrating usage
- **Tests** covering new functionality

### Commit Messages

Follow conventional commit format:

```
type(scope): brief description

Longer explanation if needed.

Fixes #123
```

**Types:** `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

**Examples:**
```
feat(basic): add INKEY$ command for keyboard input
fix(turtle): correct angle calculation in RT command
docs(guide): update installation instructions
test(logo): add tests for REPEAT command
```

---

## Testing

### Test Organization

```
tests/
├── unit/              # Unit tests for individual components
├── integration/       # Integration tests across systems
├── examples/          # Example program tests
└── regression/        # Regression test suite
```

### Running Tests

**Rust:**

```bash
# All tests
cargo test

# Specific test
cargo test test_name

# With output
cargo test -- --nocapture

# Integration tests only
cargo test --test integration_tests
```

**Python:**

```bash
# All tests
pytest

# Specific file
pytest tests/test_interpreter.py

# With coverage
pytest --cov=core tests/

# Verbose
pytest -v
```

### Writing Tests

**Rust Example:**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_print() {
        let mut executor = BasicExecutor::new();
        let result = executor.execute_command("PRINT \"Hello\"");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("Hello"));
    }
}
```

**Python Example:**

```python
import pytest
from core.interpreter import TimeWarpInterpreter

def test_pilot_type():
    interpreter = TimeWarpInterpreter()
    result = interpreter.execute("T:Hello")
    assert "Hello" in result
    assert result.startswith("✅")
```

### Test Coverage

Maintain minimum 80% code coverage:

```bash
# Rust coverage (requires tarpaulin)
cargo tarpaulin --out Html

# Python coverage
pytest --cov=core --cov-report=html tests/
```

---

## Release Process

### Version Numbering

We follow [Semantic Versioning](https://semver.org/):

- **MAJOR:** Incompatible API changes
- **MINOR:** New functionality (backward compatible)
- **PATCH:** Bug fixes (backward compatible)

**Current Version:** 3.0.0

### Release Checklist

1. **Update Version Numbers:**
   - `Time_Warp_Rust/Cargo.toml`
   - `Time_Warp_Python/version.py`
   - `README.md` badge
   - Documentation references

2. **Update Documentation:**
   - `RELEASE_NOTES.md`
   - `CHANGELOG.md`
   - Platform-specific READMEs

3. **Run Full Test Suite:**
   ```bash
   # Rust
   cd Time_Warp_Rust && cargo test --release
   
   # Python
   cd Time_Warp_Python && pytest --cov=core
   ```

4. **Build Release Artifacts:**
   ```bash
   # Rust binaries
   cargo build --release
   
   # Python package
   python setup.py sdist bdist_wheel
   ```

5. **Tag Release:**
   ```bash
   git tag -a v3.0.0 -m "Release version 3.0.0"
   git push origin v3.0.0
   ```

6. **Create GitHub Release:**
   - Upload binaries
   - Include release notes
   - Link to documentation

### Platform-Specific Releases

**Rust:**
- Windows: `.exe` binary
- macOS: `.app` bundle
- Linux: AppImage or `.deb` package

**Python:**
- PyPI package
- Standalone executables (PyInstaller)

**Web:**
- Static site deployment
- CDN distribution

---

## Best Practices

### Performance

- **Minimize allocations** in hot loops
- **Cache** frequently used values
- **Lazy load** resources when possible
- **Profile** before optimizing

### Security

- **Never use eval()** or equivalent
- **Validate** all user input
- **Sanitize** file paths
- **Limit** execution time and memory

### Error Handling

- Use **Result types** (Rust) or try/except (Python)
- Provide **clear error messages**
- Include **context** in errors
- Log errors appropriately

### Code Organization

- **Single Responsibility Principle**
- **Keep functions small** (<50 lines)
- **DRY** (Don't Repeat Yourself)
- **YAGNI** (You Aren't Gonna Need It)

---

## Resources

### Internal Documentation

- [Technical Reference](docs/TECHNICAL_REFERENCE.md)
- [Architecture Overview](ARCHITECTURE_OVERVIEW.md)
- [Programming Guide](docs/PROGRAMMING_GUIDE.md)
- [User Guide](docs/USER_GUIDE.md)

### External Resources

- [Rust Book](https://doc.rust-lang.org/book/)
- [egui Documentation](https://docs.rs/egui/)
- [PySide6 Documentation](https://doc.qt.io/qtforpython/)
- [WebAssembly Guide](https://webassembly.org/)

### Community

- **GitHub:** Issues and Pull Requests
- **Email:** James Temple <james@honey-badger.org>
- **Documentation:** [docs/](docs/)

---

## Questions?

If you have questions about development:

1. **Check documentation** in `docs/`
2. **Review examples** in `examples/`
3. **Search issues** on GitHub
4. **Ask maintainer** via email or GitHub

We appreciate your contributions to Time Warp IDE!

---

**Last Updated:** November 18, 2025  
**Maintainer:** James Temple <james@honey-badger.org>  
**License:** MIT
