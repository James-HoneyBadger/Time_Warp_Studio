# Time Warp IDE - Rust Implementation

**Native Performance Educational Programming Platform**

[![Rust](https://img.shields.io/badge/rust-1.70+-orange.svg)](https://www.rust-lang.org)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](../LICENSE)
[![Version](https://img.shields.io/badge/version-3.0.0-green.svg)](CHANGELOG.md)

> **⚡ Primary Implementation** - The Rust version of Time Warp IDE delivers native performance for intensive computational tasks while maintaining the full educational experience. Built with modern Rust and egui, this version excels at real-time graphics, complex simulations, and performance-critical educational projects.

---

## Overview

The Rust implementation of Time Warp IDE is the **primary recommended version** for most users. It provides:

- **Native Speed:** Compiled Rust code for maximum performance
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/platforms/rust
- **Modern UI:** Beautiful egui interface with 8 professional themes
- **Single Binary:** No runtime dependencies or complex installations
- **Developer-Friendly:** Easy to extend and customize for advanced users

---

## Quick Start

### Installation

```bash
# Clone the repository (if not already)
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/platforms/rust

# Build and run
cargo run

# Or build release version
cargo build --release
./target/release/time-warp
```

### First Program

1. Launch Time Warp IDE
2. Type this in the editor:
   ```templecode
   PRINT "Hello, TempleCode!"
   FOR i = 1 TO 5 STEP 1
       PRINT "Count: " + i
   NEXT i
   ```
3. Click **Run** (or press Ctrl+R)
4. See output in the panel below!

---

## Features

### Language Support

**TempleCode** - All three classic languages in one unified syntax:

**BASIC Commands:**
- `PRINT`, `INPUT`, `LET`, `IF...THEN`
- `FOR...NEXT`, `WHILE...ENDWHILE`
- `GOTO`, `GOSUB`, `RETURN`
- `INKEY$` for keyboard input

**PILOT Commands:**
- `T:` (TYPE), `A:` (ACCEPT)
- `J:` (JUMP), `U:` (USE)
- `*` variables and `#` wildcards

**Logo Commands:**
- `FORWARD`/`FD`, `BACKWARD`/`BK`
- `LEFT`/`LT`, `RIGHT`/`RT`
- `PENUP`/`PU`, `PENDOWN`/`PD`
- `REPEAT`, `TO...END` procedures
- `COLOR`, `PENWIDTH`
- PNG export

### IDE Features

**Code Editor:**
- Syntax highlighting (optional)
- Line numbers
- Undo/redo support
- Find and replace
- Code templates

**Output Display:**
- Real-time execution output
- Emoji-coded messages
- Color-coded errors
- Scrollable history
- Copy to clipboard

**Turtle Graphics:**
- 800×600 canvas
- Zoom and pan
- Vector drawing
- Color support (RGB)
- PNG export
- Canvas clearing

**Themes:**
Eight built-in professional themes:
1. **Dracula** - Dark purple
2. **Monokai** - Classic dark
3. **Solarized Dark** - Low-contrast dark
4. **Ocean** - Blue dark theme
5. **Spring** - Fresh green
6. **Sunset** - Warm orange
7. **Candy** - Bright pink
8. **Forest** - Natural green

### Safety Features

- **No eval():** Safe expression parsing
- **Resource Limits:** Execution timeouts
- **Memory Safety:** Rust ownership prevents leaks
- **Input Validation:** All user input sanitized
- **Error Handling:** Clear, beginner-friendly messages

---

## Example Programs

### Hello World

```templecode
REM My first TempleCode program
PRINT "Hello, World!"
INPUT name "What's your name?"
PRINT "Nice to meet you, " + name + "!"
```

### Turtle Square

```templecode
REM Draw a colorful square
CLS
COLOR 255 0 0
REPEAT 4
    FD 100
    RT 90
ENDREPEAT
```

### Counting Game

```templecode
REM Guess the number
LET secret = 42
INPUT guess "Guess a number (1-100):"
IF guess < secret THEN PRINT "Too low!"
IF guess > secret THEN PRINT "Too high!"
IF guess = secret THEN PRINT "You got it!"
```

**33 Complete Examples** in `examples/` directory:
- **Beginner:** Simple programs, basic commands
- **Intermediate:** Loops, conditionals, turtle graphics
- **Advanced:** Procedures, complex graphics, games

---

## Documentation

### Quick References
- **[Getting Started](docs/GETTING_STARTED.md)** - Installation and first steps
- **[User Guide](USER_GUIDE.md)** - Complete IDE reference
- **[API Reference](docs/API_REFERENCE.md)** - Command documentation
- **[Quick Reference](docs/QUICK_REFERENCE.md)** - Command cheat sheet

### Learning Resources
- **[Student Guide](docs/STUDENT_GUIDE.md)** - Learn programming from scratch
- **[Teacher Guide](docs/TEACHER_GUIDE.md)** - Classroom curriculum and lessons
- **[Programming Challenges](docs/PROGRAMMING_CHALLENGES.md)** - Practice projects
- **[Lesson Plans](docs/LESSON_PLANS.md)** - Structured learning path

### Development Resources
- **[Developer Reference](docs/DEVELOPER_REFERENCE.md)** - Internals and architecture
- **[Contributing Guide](CONTRIBUTING.md)** - How to contribute
- **[Architecture](ARCHITECTURE.md)** - System design overview
- **[Test Results](TEST_RESULTS.md)** - Testing and quality metrics

### Project Documentation
- **[Main Documentation](../docs/)** - Full project documentation
- **[Installation Guide](../docs/INSTALLATION_GUIDE.md)** - Deployment guide
- **[Technical Reference](../docs/TECHNICAL_REFERENCE.md)** - Detailed specifications

---

## Building from Source

### Prerequisites

- **Rust:** 1.70 or newer ([rustup.rs](https://rustup.rs/))
- **Git:** For cloning the repository

**Platform-Specific:**
- **Linux:** `build-essential` (gcc, make)
- **Windows:** Visual Studio Build Tools
- **macOS:** Xcode Command Line Tools

### Build Steps

```bash
# Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/platforms/rust

# Debug build (faster compilation, slower execution)
cargo build
./target/debug/time-warp

# Release build (slower compilation, fast execution)
cargo build --release
./target/release/time-warp

# Run without building binary
cargo run

# Run release version directly
cargo run --release
```

### Testing

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_basic_print

# Run with output
cargo test -- --nocapture

# Run integration tests
cargo test --test integration_tests
```

### Linting and Formatting

```bash
# Format code
cargo fmt

# Check formatting
cargo fmt --check

# Run clippy (linter)
cargo clippy

# Fix clippy warnings automatically
cargo clippy --fix
```

---

## Platform-Specific Notes

### Windows

**Build Requirements:**
- Visual Studio 2019 or newer, OR
- Visual Studio Build Tools

**Running:**
```cmd
time-warp.exe
```

**Known Issues:**
- First run may be slow (Windows Defender scanning)
- Use `--release` for best performance

### macOS

**Build Requirements:**
- Xcode Command Line Tools: `xcode-select --install`

**Running:**
```bash
./time-warp
```

**App Bundle:**
See `packaging/macos/` for creating `.app` bundle.

### Linux

**Build Requirements:**
```bash
# Debian/Ubuntu
sudo apt install build-essential libgtk-3-dev

# Fedora
sudo dnf install gcc gtk3-devel

# Arch
sudo pacman -S base-devel gtk3
```

**Running:**
```bash
./time-warp
```

**Installation:**
```bash
# System-wide install
sudo cp target/release/time-warp /usr/local/bin/

# Create desktop entry
# See scripts/install-linux.sh
```

---

## Compilation Features

The Rust implementation supports optional features:

```bash
# Default features (minimal)
cargo build

# With audio support
cargo build --features audio

# With plugin system
cargo build --features plugins

# All features
cargo build --all-features
```

**Available Features:**
- `audio` - Sound effects and music (experimental)
- `ml` - Machine learning integration (planned)
- `plugins` - Dynamic plugin loading (experimental)

---

## Configuration

User settings are stored in:
- **Linux:** `~/.config/time-warp/config.toml`
- **macOS:** `~/Library/Application Support/time-warp/config.toml`
- **Windows:** `%APPDATA%\time-warp\config.toml`

**Example config.toml:**

```toml
[ui]
theme = "dracula"
font_size = 14
show_line_numbers = true

[editor]
tab_size = 4
auto_indent = true

[graphics]
canvas_width = 800
canvas_height = 600
default_color = [0, 0, 0]

[execution]
timeout_seconds = 30
max_turtle_lines = 10000
```

---

## Performance

The Rust implementation offers excellent performance:

| Operation | Time | Comparison |
|-----------|------|------------|
| Startup | ~0.3s | Fastest |
| Simple command | ~10μs | Native speed |
| 1000 PRINT loops | ~50ms | 20× faster than Python |
| Turtle graphics (100 lines) | ~5ms | Real-time |
| PNG export (800×600) | ~100ms | Fast |

**Memory Usage:**
- Baseline: ~30 MB
- With program loaded: ~50 MB
- Maximum (10K turtle lines): ~200 MB

---

## Troubleshooting

### Build Errors

**Problem:** "linker error" on Linux  
**Solution:** Install `build-essential` and `libgtk-3-dev`

**Problem:** Slow compilation  
**Solution:** Use `cargo build --release` only for final builds

**Problem:** "cannot find -lgtk-3"  
**Solution:** Install GTK3 development libraries

### Runtime Issues

**Problem:** Window doesn't appear  
**Solution:** Update graphics drivers, try different desktop environment

**Problem:** Slow performance  
**Solution:** Use release build: `cargo run --release`

**Problem:** Turtle graphics flickering  
**Solution:** Enable VSync in system settings

### Platform Issues

**Problem:** macOS "unidentified developer" warning  
**Solution:** Right-click → Open, or: `xattr -d com.apple.quarantine time-warp`

**Problem:** Windows Defender false positive  
**Solution:** Add exception for `time-warp.exe`

---

## Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

**Ways to Contribute:**
- Report bugs and issues
- Suggest features and improvements
- Submit pull requests
- Improve documentation
- Create example programs
- Write tests

**Development Workflow:**
1. Fork the repository
2. Create feature branch: `git checkout -b feature/my-feature`
3. Make changes and add tests
4. Run tests: `cargo test`
5. Format code: `cargo fmt`
6. Lint: `cargo clippy`
7. Commit: `git commit -m "feat: add my feature"`
8. Push: `git push origin feature/my-feature`
9. Create pull request

---

## Release Notes

**Version 3.0.0** (Current)

**Added:**
- Unified TempleCode language
- Enhanced turtle graphics with PNG export
- Eight professional themes
- INKEY$ keyboard input
- Safe expression evaluation
- Comprehensive test suite

**Changed:**
- Migrated from Tkinter to egui
- Improved error messages
- Better documentation

**Fixed:**
- Turtle angle calculations
- Memory leaks in graphics
- Theme persistence

See [CHANGELOG.md](CHANGELOG.md) for complete history.

---

## License

Time Warp IDE is licensed under the **MIT License**.

```
Copyright (c) 2025 James Temple <james@honey-badger.org>
```

See [LICENSE](../LICENSE) for full details.

**SPDX-License-Identifier:** MIT

---

## Support

**Documentation:** [Main Docs](../docs/) | [User Guide](USER_GUIDE.md)  
**Examples:** [examples/](examples/)  
**Issues:** [GitHub Issues](https://github.com/James-HoneyBadger/Time_Warp/issues)  
**Email:** James Temple <james@honey-badger.org>

---

## Acknowledgments

Built with these excellent libraries:
- **egui/eframe** - Immediate mode GUI
- **image** - Image encoding/decoding
- **serde** - Serialization
- **tokio** - Async runtime

Thanks to all contributors and the Rust community!

---

**Happy Coding! 🚀**
