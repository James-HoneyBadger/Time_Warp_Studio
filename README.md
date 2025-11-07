# TempleCode and Time Warp v3.0.0

TempleCode is a small, interpreted, general‑purpose language that blends approachable ideas from BASIC (friendly I/O, line/label jumps, loops), PILOT (concise teaching commands like TYPE/ACCEPT/JUMP), and Logo (turtle graphics). Time Warp v3.0.0 is a multi-platform GUI IDE for writing and running TempleCode programs.

## Version 3.0.0 - Platforms

- **Go** - Fast CLI interpreter with batch mode (`Time_Warp_Go/`)
- **Python** - Tkinter GUI IDE (`Time_Warp_Python/`)
- **Rust** - Cross-platform eframe/egui IDE (`Time_Warp_Rust/`)
- **Windows** - Windows launchers and helpers (`Time_Warp_Windows/`)
- **Apple** - macOS launchers and docs (`Time_Warp_Apple/`)
- **DOS** - DJGPP/Mode 13h VGA implementation (`Time_Warp_DOS/`)
- **Browser** - WebAssembly/JavaScript version (`Time_Warp_Web/`)

## Highlights

- Simple statements: PRINT/TYPE, INPUT/ACCEPT, LET and assignment, IF/THEN, GOTO/JUMP, labels
- Loops: FOR/NEXT, WHILE/ENDWHILE, REPEAT/ENDREPEAT
- Turtle graphics: FORWARD/FD, LEFT/LT, RIGHT/RT, PENUP/PU, PENDOWN/PD, CLEAR/CLS, SETXY, COLOR
- Comments with `REM` or `#`
- Case-insensitive keywords (variables are case-sensitive)

## Quick start

- Run from CLI (Go - fastest):

```bash
cd Time_Warp_Go
go build ./cmd/timewarp
echo "10 PRINT \"Hello World\"" | ./timewarp --batch BASIC
```

- Run from CLI (Python):

```bash
python scripts/run_templecode.py examples/hello.tc
```

- Launch Time Warp GUI (Python):

```bash
python Time_Warp_Python/run_time_warp.py
```

- Launch Time Warp GUI (Rust):

```bash
cd Time_Warp_Rust
cargo run --release
```

If Tkinter isn’t available on your system, install `python3-tk` via your OS package manager (e.g., `sudo apt-get install python3-tk`).

## Language cheatsheet

```text
REM A comment
# Also a comment

PRINT "Hello"            ' alias: TYPE "Hello"
INPUT name "Your name?"  ' alias: ACCEPT name "Your name?"
LET x = 10               ' or: x = 10
IF x > 5 THEN PRINT "big"
IF x < 3 THEN GOTO end

start:
FOR i = 1 TO 4 STEP 1
  PRINT i
NEXT i

WHILE x > 0
  PRINT x
  x = x - 1
ENDWHILE

REPEAT 3
  PRINT "loop"
ENDREPEAT

JUMP start
end:
PRINT "Done"

' Turtle graphics
CLS
FD 100
LT 90
FD 100
RT 45
COLOR 255 0 0
PU
SETXY 0 0
PD
```

## Examples

- `examples/hello.tc` – basics
- `examples/turtle_square.tc` – simple turtle drawing

## Project layout

- `Time_Warp_Python/` – Python IDE and interpreter
- `Time_Warp_Rust/` – Rust IDE (eframe/egui)
- `Time_Warp_Web/` – WebAssembly demo and HTML test pages
- `Time_Warp_Windows/` – Windows launch scripts and docs
- `Time_Warp_Apple/` – macOS launch scripts and docs
- `Time_Warp_DOS/` – DOS DJGPP implementation and samples
- `examples/` – Cross-language sample programs
- `docs/` – Guides, references, and installation docs
- `scripts/` – Utility scripts (CLI runner, debug helpers)
- `legacy/` – Archived older platform folders and experimental code

## Minimum requirements

- Python 3.10+
- Tkinter for the GUI (usually packaged with your Python distro; on Debian/Ubuntu `python3-tk`)
- Optional: `mysql-connector-python` for MySQL features

### Rust GUI (optional)

If you want to try the experimental Rust version of Time Warp:

- Rust toolchain (stable)
- The Rust app shells out to the Python interpreter, so ensure `python` or `python3` is on your PATH.

Build and run:

```bash
cd Rust
cargo run
```

Notes:

- The Rust app loads `.tc` examples from `../examples` into its editor.
- Press Run (or Ctrl+R) to execute the current buffer via `scripts/run_templecode.py` and see output in the console.

Install optional MySQL support:

```bash
pip install mysql-connector-python
```

## Makefile and tests

Convenience targets:

```bash
# Launch the GUI IDE
make run

# Run a .tc program (provide FILE)
make run-example FILE=examples/hello.tc

# Run Python tests (requires pytest)
pip install -r requirements-dev.txt
make test

# Clean caches/artifacts
make clean
```

## Placeholder assets generator

Utility to generate placeholder icons (.iconset) and screenshots for packaging.

- Script: `scripts/generate_placeholders.py`
- Requires: Pillow (`pip install Pillow`)

Quick usage:

```bash
python scripts/generate_placeholders.py --help
```

Common examples:

```bash
# Icons only, default sizes, preview only (no files created)
python scripts/generate_placeholders.py --icons-only --dry-run --out-dir ./build/placeholders

# Icons only with custom glyph, background and sizes
python scripts/generate_placeholders.py --icons-only \
  --text TW --bg-color "#1e90ff" --fg-color "#ffffff" --sizes 16,32,128,256,512 \
  --out-dir ./build/placeholders

# Screenshots only, 1600x1000, custom title prefix
python scripts/generate_placeholders.py --screenshots-only \
  --screenshot-size 1600x1000 --text "Temple Code — Shot" \
  --out-dir ./build/placeholders
```

Flags of interest:

- `--icons-only` | `--screenshots-only` – choose what to generate
- `--out-dir PATH` – base output (icons → OUT/icon.iconset, screenshots → OUT/screenshots)
- `--text TEXT` – shared text; icon glyph uses the first character
- `--font PATH` – custom `.ttf`/`.ttc` font
- `--bg-color`, `--fg-color` – `#RRGGBB` or `R,G,B[,A]`
- `--sizes` – comma-separated base sizes; generates @1x and @2x icon files
- `--screenshot-size WxH` – dimensions for screenshots (default 1280x800)
- `--verbose` – extra logging
- `--dry-run` – print planned outputs without writing files

## License

This example project is provided for learning and experimentation. Use at your own discretion.
