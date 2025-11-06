# 🚀 Time Warp IDE - Rust Implementation

**🦀 Native Performance Educational Programming Platform**

[![Rust](https://img.shields.io/badge/rust-1.70+-orange.svg)](https://www.rust-lang.org)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Tests](https://img.shields.io/badge/Tests-22%20passing-green.svg)](TEST_RESULTS.md)

> **🎯 Part of the Time Warp Educational Platform** — See [main documentation](../docs/) for complete guides and curriculum materials.

The **Rust implementation** of Time Warp IDE delivers **native performance** for intensive computational tasks while maintaining the full educational experience. Built with modern Rust and egui, this version excels at real-time graphics, complex simulations, and performance-critical educational projects.

## ⚡ Why Choose the Rust Version?

- **🚀 Native Speed**: Perfect for computational mathematics and real-time graphics
- **🔒 Memory Safety**: Rust's ownership system prevents crashes and memory leaks  
- **⚙️ Cross-Platform**: Single binary runs on Windows, macOS, and Linux
- **🎨 Modern UI**: Beautiful egui interface with 8 professional themes
- **🔧 Developer-Friendly**: Easy to extend and customize for advanced users

## Quick Start

```bash
# Build and run
cargo run

# Run tests
cargo test

# Build release
cargo build --release
./target/release/time-warp
```

## Features

- TempleCode language: All BASIC, PILOT, and Logo commands in one language
  - Text commands: PRINT, LET, INPUT, INKEY$, IF...THEN, FOR/NEXT, GOTO, GOSUB, RETURN
  - PILOT-style: T:, A:, labels (L:), jumps (J:)
  - Logo turtle graphics: FORWARD/FD, LEFT/LT, RIGHT/RT, REPEAT, TO/END procedures, PENWIDTH, colors, PNG export
- Safe expression evaluator (no eval())
- Async execution with tokio
- Expression caching (10-50x speedup)
- Security limits and timeouts
- Modern egui UI with 8 themes
- Unified Screen: single canvas for text and graphics output (text/graphics modes)
- Text screen controls: CLS, LOCATE; GW-BASIC–style SCREEN command
- Input prompts: interactive 📝 dialog for BASIC INPUT and PILOT A:
- Real-time keyboard detection: INKEY$ for game loops and interactive programs

## Example Programs

**PILOT:**

```pilot
T:What is your name?
A:NAME
T:Hello *NAME*!
```

**TempleCode (BASIC-style):**

```basic
10 LET A = 5
20 PRINT "Value:", A
30 IF A > 3 THEN PRINT "Greater"
```

**33 example programs** organized by difficulty in `examples/`:

- **Beginner**: `pilot_quiz.pilot`, `basic_guess.bas`, `logo_star.logo`
- **Intermediate**: `basic_rock_paper_scissors.bas`, `logo_flower.logo`
- **Advanced**: `pilot_dragon_adventure.pilot`, `logo_koch_snowflake.logo`

PNG export: use View → "Save Canvas as PNG…" in the UI.

## 📚 Learning & Documentation

### 🎓 **For Students & Beginners**
- **[📖 Student Lesson Book](../docs/STUDENT_LESSON_BOOK.md)** — Progressive 24-lesson curriculum with hands-on projects
- **[🎯 User Guide](../docs/USER_GUIDE.md)** — Complete installation and usage guide for all platforms  
- **[⚡ Quick Start Examples](examples/)** — 33 ready-to-run programs organized by difficulty

### 👨‍🏫 **For Educators**  
- **[🍎 Teacher Guide & Curriculum](../docs/TEACHER_GUIDE.md)** — Complete educational framework with lesson plans
- **[📋 Assessment Tools](../docs/TEACHER_GUIDE.md#assessment-rubrics)** — Rubrics and evaluation strategies
- **[🎮 Interactive Projects](../docs/STUDENT_LESSON_BOOK.md#level-4-loops-and-patterns)** — Engaging programming challenges

### 🔧 **For Developers**
- **[⚙️ Technical Reference](../docs/TECHNICAL_REFERENCE.md)** — Architecture, APIs, and implementation details
- **[🏗️ Contributing Guide](../docs/CONTRIBUTING.md)** — How to extend and improve Time Warp
- **[🧪 Test Results](TEST_RESULTS.md)** — Comprehensive testing and quality metrics

### TempleCode Compiler (experimental)

You can transpile TempleCode to C and build a native Linux executable.

Scope (v0):
 
- Text-mode subset: PRINT, LET, INPUT, IF ... THEN (GOTO | PRINT), GOTO, END
- PILOT: T:, A:
- Logo: currently ignored at compile-time (runtime support remains via interpreter)

Usage:

```bash
# Build and run the GUI normally
cargo run

# Compile a TempleCode source file to an executable
cargo run -- --compile my_program.tc -o my_program

# Then run it
./my_program
```

Notes:
 
- Requires a system C compiler (cc/gcc/clang) on PATH.
- Output C file is written to target/tmp internally before linking.

**See also**: [Examples README](examples/README.md) for learning paths and all 33 programs.

## Testing

- 22 integration tests (100% pass)
- 5 doc tests
- Zero warnings

See [TEST_RESULTS.md](TEST_RESULTS.md) for details.

## License

See [LICENSE](LICENSE) file for details.
