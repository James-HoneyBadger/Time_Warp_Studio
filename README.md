# Time Warp IDE# Time Warp IDE



**Educational Programming Environment for TempleCode****Educational Programming Environment for TempleCode**



[![Rust](https://img.shields.io/badge/rust-1.70+-orange.svg)](https://www.rust-lang.org)[![Rust](https://img.shields.io/badge/rust-1.70+-orange.svg)](https://www.rust-lang.org)

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

[![Version](https://img.shields.io/badge/version-3.0.0-green.svg)](RELEASE_NOTES.md)[![Version](https://img.shields.io/badge/version-3.0.0-green.svg)](RELEASE_NOTES.md)



> **TempleCode** is a unified educational programming language that blends approachable ideas from BASIC (friendly I/O, loops), PILOT (concise teaching commands), and Logo (turtle graphics). **Time Warp IDE** is a modern, multi-platform development environment for writing and running TempleCode programs.> **TempleCode** is a unified educational programming language that blends approachable ideas from BASIC (friendly I/O, loops), PILOT (concise teaching commands), and Logo (turtle graphics). **Time Warp IDE** is a modern, multi-platform development environment for writing and running TempleCode programs.



------



## Overview## Overview



Time Warp IDE provides students, educators, and hobbyists with an accessible yet powerful programming environment. Whether you're teaching fundamentals, exploring retro computing, or building educational projects, Time Warp makes programming engaging and fun.Time Warp IDE provides students, educators, and hobbyists with an accessible yet powerful programming environment. Whether you're teaching fundamentals, exploring retro computing, or building educational projects, Time Warp makes programming engaging and fun.



**Key Features:****Key Features:**

- Three classic languages in one: BASIC, PILOT, and Logo syntax- Three classic languages in one: BASIC, PILOT, and Logo syntax

- Integrated turtle graphics for visual learning- Integrated turtle graphics for visual learning

- Modern IDE with syntax highlighting and themes- Modern IDE with syntax highlighting and themes

- Cross-platform: Rust, Python, Web, DOS, and more- Cross-platform: Rust, Python, Web, DOS, and more

- Educational resources and lesson plans included- Educational resources and lesson plans included



------



## Platform Implementations## Platform Implementations



Time Warp IDE is available on multiple platforms to suit different needs:Time Warp IDE is available on multiple platforms to suit different needs:



| Platform | Technology | Best For | Location || Platform | Technology | Best For | Location |

|----------|-----------|----------|----------||----------|-----------|----------|----------|

| **Rust** ⚡ | egui/eframe | Native performance, production use | `Time_Warp_Rust/` || **Rust** ⚡ | egui/eframe | Native performance, production use | `Time_Warp_Rust/` |

| **Python** 🐍 | PySide6 | Educational environments, prototyping | `Time_Warp_Python/` || **Python** 🐍 | PySide6 | Educational environments, prototyping | `Time_Warp_Python/` |

| **Web** 🌐 | WebAssembly | Browser access, no installation | `Time_Warp_Web/` || **Web** 🌐 | WebAssembly | Browser access, no installation | `Time_Warp_Web/` |

| **DOS** 💾 | DJGPP/VGA | Retro computing, historical education | `Time_Warp_DOS/` || **DOS** 💾 | DJGPP/VGA | Retro computing, historical education | `Time_Warp_DOS/` |

| **Windows** 🪟 | Native Win32 | Enterprise deployment | `Time_Warp_Windows/` || **Windows** 🪟 | Native Win32 | Enterprise deployment | `Time_Warp_Windows/` |

| **Apple** 🍎 | macOS/iOS | Apple ecosystem integration | `Time_Warp_Apple/` || **Apple** 🍎 | macOS/iOS | Apple ecosystem integration | `Time_Warp_Apple/` |

| **Go** 🚀 | Fyne GUI | Fast CLI, alternative GUI | `Time_Warp_Go/` || **Go** 🚀 | Fyne GUI | Fast CLI, alternative GUI | `Time_Warp_Go/` |



**Primary Implementation:** Rust (recommended for most users)**Primary Implementation:** Rust (recommended for most users)



---## Highlights



## Quick Start- Simple statements: PRINT/TYPE, INPUT/ACCEPT, LET and assignment, IF/THEN, GOTO/JUMP, labels

- Loops: FOR/NEXT, WHILE/ENDWHILE, REPEAT/ENDREPEAT

### Rust (Recommended)- Turtle graphics: FORWARD/FD, LEFT/LT, RIGHT/RT, PENUP/PU, PENDOWN/PD, CLEAR/CLS, SETXY, COLOR

- Comments with `REM` or `#`

```bash- Case-insensitive keywords (variables are case-sensitive)

cd Time_Warp_Rust

cargo run --release## Quick start

```

- Run from CLI (Go - fastest):

### Python

```bash

```bashcd Time_Warp_Go

cd Time_Warp_Pythongo build ./cmd/timewarp

python run_time_warp.pyecho "10 PRINT \"Hello World\"" | ./timewarp --batch BASIC

``````



### Go (CLI)- Run from CLI (Python):



```bash```bash

cd Time_Warp_Gopython scripts/run_templecode.py examples/hello.tc

go build ./cmd/timewarp```

echo "PRINT \"Hello, TempleCode!\"" | ./timewarp --batch BASIC

```- Launch Time Warp GUI (Python):



### Web```bash

python Time_Warp_Python/run_time_warp.py

```bash```

cd Time_Warp_Web

# See Time_Warp_Web/README.md for build instructions- Launch Time Warp GUI (Rust):

```

```bash

**First Time?** See the [Installation Guide](docs/INSTALLATION_GUIDE.md) for detailed setup instructions.cd Time_Warp_Rust

cargo run --release

---```



## TempleCode Language FeaturesIf PySide6 isn’t available on your system, install system Qt deps or use the bundled virtual environment.



TempleCode combines three classic educational languages into one unified syntax:## Language cheatsheet



### Core Commands```text

REM A comment

**Text I/O:**# Also a comment

- `PRINT "text"` / `TYPE "text"` - Display output

- `INPUT variable "prompt"` / `ACCEPT variable` - Get user inputPRINT "Hello"            ' alias: TYPE "Hello"

- `LET x = 10` / `x = 10` - Assign variablesINPUT name "Your name?"  ' alias: ACCEPT name "Your name?"

LET x = 10               ' or: x = 10

**Control Flow:**IF x > 5 THEN PRINT "big"

- `IF condition THEN command` - Conditional executionIF x < 3 THEN GOTO end

- `GOTO label` / `JUMP label` - Jump to label

- `GOSUB label` / `RETURN` - Subroutinesstart:

- Labels: `start:` or `*label`FOR i = 1 TO 4 STEP 1

  PRINT i

**Loops:**NEXT i

- `FOR i = 1 TO 10 STEP 1` ... `NEXT i` - Counted loops

- `WHILE condition` ... `ENDWHILE` - Conditional loopsWHILE x > 0

- `REPEAT n` ... `ENDREPEAT` - Fixed repetition  PRINT x

  x = x - 1

**Turtle Graphics:**ENDWHILE

- `FORWARD n` / `FD n` - Move turtle forward

- `LEFT angle` / `LT angle` - Turn leftREPEAT 3

- `RIGHT angle` / `RT angle` - Turn right  PRINT "loop"

- `PENUP` / `PU`, `PENDOWN` / `PD` - Control drawingENDREPEAT

- `COLOR r g b` - Set pen color

- `CLEAR` / `CLS` - Clear canvasJUMP start

- `SETXY x y` - Position turtleend:

PRINT "Done"

**Comments:**

- `REM This is a comment`' Turtle graphics

- `# This is also a comment`CLS

- `' Single quote comment`FD 100

LT 90

**Case Sensitivity:**FD 100

- Keywords are case-insensitive (`PRINT` = `print`)RT 45

- Variables are case-sensitive (`name` ≠ `Name`)COLOR 255 0 0

PU

---SETXY 0 0

PD

## Example Programs```



### Hello World## Licensing



```templecodePrimary license: MIT (see `LICENSE`).

REM My first programHistorical/secondary: Apache 2.0 retained in `LICENSES/Apache-2.0.txt`.

PRINT "Hello, TempleCode!"

INPUT name "What's your name?"SPDX headers: `// SPDX-License-Identifier: MIT` in active source files.

PRINT "Welcome, " + name

```## Examples



### Turtle Graphics- `examples/hello.tc` – basics

- `examples/turtle_square.tc` – simple turtle drawing

```templecode

REM Draw a square## Project layout

CLS

REPEAT 4- `Time_Warp_Python/` – Python IDE and interpreter

    FD 100- `Time_Warp_Rust/` – Rust IDE (eframe/egui)

    RT 90- `Time_Warp_Web/` – WebAssembly demo and HTML test pages

ENDREPEAT- `Time_Warp_Windows/` – Windows launch scripts and docs

```- `Time_Warp_Apple/` – macOS launch scripts and docs

- `Time_Warp_DOS/` – DOS DJGPP implementation and samples

### Loop Example- `examples/` – Cross-language sample programs

- `docs/` – Guides, references, and installation docs

```templecode- `scripts/` – Utility scripts (CLI runner, debug helpers)

REM Count to 10- `legacy/` – Archived older platform folders and experimental code

FOR i = 1 TO 10 STEP 1

    PRINT "Count: " + i## Minimum requirements

NEXT i

PRINT "Done!"- Python 3.10+

```- Tkinter for the GUI (usually packaged with your Python distro; on Debian/Ubuntu `python3-tk`)

- Optional: `mysql-connector-python` for MySQL features

**More Examples:** See `examples/` directory for 30+ complete programs organized by difficulty level.

### Rust GUI (optional)

---

If you want to try the experimental Rust version of Time Warp:

## Documentation

- Rust toolchain (stable)

### For Students and Beginners- The Rust app shells out to the Python interpreter, so ensure `python` or `python3` is on your PATH.

- **[User Guide](docs/USER_GUIDE.md)** - Complete guide to using Time Warp IDE

- **[Student Lesson Book](docs/STUDENT_LESSON_BOOK.md)** - 24 progressive programming lessonsBuild and run:

- **[Programming Guide](docs/PROGRAMMING_GUIDE.md)** - TempleCode language reference

```bash

### For Educatorscd Rust

- **[Teacher Guide](docs/TEACHER_GUIDE.md)** - Curriculum, lesson plans, and assessmentscargo run

- **[Installation Guide](docs/INSTALLATION_GUIDE.md)** - Classroom deployment instructions```



### For DevelopersNotes:

- **[Developer Reference](docs/DEVELOPER_REFERENCE.md)** - Architecture and implementation details

- **[Technical Reference](docs/TECHNICAL_REFERENCE.md)** - API documentation and specifications- The Rust app loads `.tc` examples from `../examples` into its editor.

- **[Contributing Guide](docs/CONTRIBUTING.md)** - How to contribute to the project- Press Run (or Ctrl+R) to execute the current buffer via `scripts/run_templecode.py` and see output in the console.



**Full Documentation:** See [Documentation Index](docs/DOCUMENTATION_INDEX.md)Install optional MySQL support:



---```bash

pip install mysql-connector-python

## Project Structure```



```## Makefile and tests

Time_Warp/

├── Time_Warp_Rust/       # Primary Rust implementation (recommended)Convenience targets:

├── Time_Warp_Python/     # Python implementation with PySide6

├── Time_Warp_Go/         # Go CLI and Fyne GUI```bash

├── Time_Warp_Web/        # WebAssembly browser version# Launch the GUI IDE

├── Time_Warp_DOS/        # DOS/DJGPP retro implementationmake run

├── Time_Warp_Windows/    # Windows-specific builds

├── Time_Warp_Apple/      # macOS/iOS applications# Run a .tc program (provide FILE)

├── examples/             # Sample TempleCode programsmake run-example FILE=examples/hello.tc

├── docs/                 # Complete documentation

├── core-spec/            # Language specification# Run Python tests (requires pytest)

├── scripts/              # Build and utility scriptspip install -r requirements-dev.txt

└── tests/                # Test suitesmake test

```

# Clean caches/artifacts

---make clean

```

## System Requirements

## Placeholder assets generator

### Rust Implementation

- **Operating System:** Windows 10+, macOS 10.15+, Linux (any modern distro)Utility to generate placeholder icons (.iconset) and screenshots for packaging.

- **RAM:** 512 MB minimum, 1 GB recommended

- **Disk Space:** 100 MB- Script: `scripts/generate_placeholders.py`

- **Rust:** 1.70 or newer- Requires: Pillow (`pip install Pillow`)



### Python ImplementationQuick usage:

- **Operating System:** Windows 10+, macOS 10.15+, Linux

- **Python:** 3.10 or newer```bash

- **RAM:** 512 MB minimum, 1 GB recommendedpython scripts/generate_placeholders.py --help

- **Dependencies:** PySide6 (automatically installed)```



### Web ImplementationCommon examples:

- **Browser:** Modern browser with WebAssembly support

- **Internet:** Optional (can run offline after initial load)```bash

# Icons only, default sizes, preview only (no files created)

---python scripts/generate_placeholders.py --icons-only --dry-run --out-dir ./build/placeholders



## License# Icons only with custom glyph, background and sizes

python scripts/generate_placeholders.py --icons-only \

This project is licensed under the **MIT License** - see the [LICENSE](LICENSE) file for details.  --text TW --bg-color "#1e90ff" --fg-color "#ffffff" --sizes 16,32,128,256,512 \

  --out-dir ./build/placeholders

**Historical Note:** Some components retain Apache 2.0 licensing for compatibility. See `LICENSES/` directory for details.

# Screenshots only, 1600x1000, custom title prefix

**SPDX Identifier:** `MIT`python scripts/generate_placeholders.py --screenshots-only \

  --screenshot-size 1600x1000 --text "Temple Code — Shot" \

---  --out-dir ./build/placeholders

```

## Contributing

Flags of interest:

We welcome contributions from everyone! Whether you're fixing bugs, adding features, improving documentation, or creating educational content, your help is appreciated.

- `--icons-only` | `--screenshots-only` – choose what to generate

**Getting Started:**- `--out-dir PATH` – base output (icons → OUT/icon.iconset, screenshots → OUT/screenshots)

1. Read the [Contributing Guide](docs/CONTRIBUTING.md)- `--text TEXT` – shared text; icon glyph uses the first character

2. Check the [Developer Reference](docs/DEVELOPER_REFERENCE.md) for architecture details- `--font PATH` – custom `.ttf`/`.ttc` font

3. Browse open issues on GitHub- `--bg-color`, `--fg-color` – `#RRGGBB` or `R,G,B[,A]`

4. Join the discussion in pull requests- `--sizes` – comma-separated base sizes; generates @1x and @2x icon files

- `--screenshot-size WxH` – dimensions for screenshots (default 1280x800)

**Code of Conduct:** See [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)- `--verbose` – extra logging

- `--dry-run` – print planned outputs without writing files

---

## License

## Support and Community

This example project is provided for learning and experimentation. Use at your own discretion.

- **Documentation:** [docs/](docs/)
- **Examples:** [examples/](examples/)
- **Issues:** GitHub Issues
- **Maintainer:** James Temple <james@honey-badger.org>

---

## Release Notes

**Current Version:** 3.0.0

For detailed release information, see [RELEASE_NOTES.md](RELEASE_NOTES.md)

**What's New in 3.0.0:**
- Unified TempleCode language across all platforms
- Enhanced Rust implementation with egui interface
- Improved turtle graphics with PNG export
- Comprehensive educational documentation
- Cross-platform consistency improvements

---

## Acknowledgments

Time Warp IDE builds upon decades of educational programming tradition:
- **BASIC** - Dartmouth College (1964)
- **PILOT** - John A. Starkweather (1968)
- **Logo** - Seymour Papert, MIT (1967)

Special thanks to all contributors and educators who have supported this project.

---

**Happy Coding! 🚀**
