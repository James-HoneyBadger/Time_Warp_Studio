# TempleCode and Time Warp v3.0.0

TempleCode is a small, interpreted, general‑purpose language that blends approachable ideas from BASIC (friendly I/O, line/label jumps, loops), PILOT (concise teaching commands like TYPE/ACCEPT/JUMP), and Logo (turtle graphics). Time Warp v3.0.0 is a multi-platform GUI IDE for writing and running TempleCode programs.

## Version 3.0.0 - Platforms

- **Python** - Tkinter GUI IDE (`Python/`)
- **Rust** - Cross-platform eframe/egui IDE (`Rust/`)
- **Windows** - Native Windows desktop app (`Windows/`)
- **Apple** - Native macOS `.app` bundle (`Apple/`)
- **DOS** - DJGPP/Mode 13h VGA implementation (`DOS/`)
- **Browser** - WebAssembly/JavaScript version (`Browser/`)

## Highlights

- Simple statements: PRINT/TYPE, INPUT/ACCEPT, LET and assignment, IF/THEN, GOTO/JUMP, labels
- Loops: FOR/NEXT, WHILE/ENDWHILE, REPEAT/ENDREPEAT
- Turtle graphics: FORWARD/FD, LEFT/LT, RIGHT/RT, PENUP/PU, PENDOWN/PD, CLEAR/CLS, SETXY, COLOR
- Comments with `REM` or `#`
- Case-insensitive keywords (variables are case-sensitive)

## Quick start

- Run from CLI:

```bash
python scripts/run_templecode.py examples/hello.tc
```

- Launch Time Warp GUI (Python):

```bash
cd Python
make run
```

- Launch Time Warp GUI (Rust):

```bash
cd Rust
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

- `templecode/` – TempleCode interpreter package
- `time.warp/` – Time Warp GUI package (launch with `python -m time.warp.app`)
- `examples/` – Sample TempleCode programs (.tc)
- `tests/` – Additional TempleCode scripts used as simple tests (moved from project root)
- `scripts/` – Utility scripts (CLI runner, debug helpers)
- `Rust/` – Experimental Rust GUI (eframe/egui) for Time Warp

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

## License

This example project is provided for learning and experimentation. Use at your own discretion.
