# Time Warp IDE — User Guide

Welcome to Time Warp IDE, a retromodern learning environment for TempleCode — a unified language combining BASIC, PILOT, and Logo with turtle graphics. This guide covers daily use of the Rust-based IDE.

For help: <james@honey-badger.org>

---

## Getting Started

Run the IDE:

```bash
cargo run
```

## Core Workflow

1. Open or create files from File → Open or File → New.
2. Edit code in the Editor tab. Multiple files can be open in tabs.
3. Run the program (▶️) to view output and graphics in the unified Screen.
4. When prompted for input (📝), type your response and press Enter.
5. Save often via File → Save or Ctrl+S. Unsaved files show a modified indicator.

Notes:

- Examples are in `examples/` (TempleCode supports `.tc`, `.bas`, `.pilot`, `.logo`).
- Turtle graphics render on the unified canvas.

## Language Reference

### PILOT Commands

- `T:text` - Output text (supports *VARIABLE* interpolation)
- `A:variable` - Accept input into variable
- `Y:condition` - Set match flag if condition is true
- `N:condition` - Alternative conditional test
- `J:label` - Jump to label (conditional if follows Y:/N:)
- `M:label` - Jump to label if match flag is set
- `R:label` - Gosub to label (subroutine call)
- `C:` - Return from subroutine
- `L:label` - Label definition
- `U:var=expr` - Update/assign variable
- `END` - End program

### BASIC Commands

- `LET var = expr` - Variable assignment
- `PRINT expr` - Output expression or string
- `INPUT var` - Get user input (blocking, waits for Enter)
- `LET var$ = INKEY$` - Get key press (non-blocking, for game loops)
- `SCREEN mode[, w, h]` - Switch between text/graphics modes (0=text, 1=640x480, 2=1024x768)
- `CLS` - Clear text screen and reset cursor
- `LOCATE row, col` - Move text cursor (1-based)
- `GOTO line` - Jump to line number
- `IF condition THEN command` - Conditional execution
- `FOR var = start TO end [STEP step]` - Loop construct
- `NEXT [var]` - End of FOR loop
- `GOSUB line` - Call subroutine
- `RETURN` - Return from subroutine
- `REM comment` - Comment line
- `END` - End program

### Logo Commands

- `FORWARD n` (or `FD n`) - Move turtle forward
- `BACK n` (or `BK n`) - Move turtle backward
- `LEFT n` (or `LT n`) - Turn turtle left
- `RIGHT n` (or `RT n`) - Turn turtle right
- `PENUP` (or `PU`) - Lift pen up
- `PENDOWN` (or `PD`) - Put pen down
- `CLEARSCREEN` (or `CS`) - Clear screen
- `HOME` - Return turtle to center
- `SETXY x y` - Set turtle position

### Built-in Functions

- `RND()` - Random number (0-1)
- `INT(expr)` - Convert to integer
- `VAL(string)` - Convert string to number
- `UPPER(string)` - Convert to uppercase
- `LOWER(string)` - Convert to lowercase
- `MID(string,start,length)` - Extract substring

## Tips & Good Practices

- Keep programs small and focused; split into multiple files when helpful.
- Save before running to avoid losing changes.
- Explore example programs to learn language features quickly.

## Example Programs

### How to run PILOT examples

1. Open any `.pilot` file from the `examples/` folder (e.g., `pilot_quiz.pilot`).
2. Click Run (▶️). Output appears on the unified screen (text mode).
3. When you see the 📝 prompt (A:), type your answer and press Enter.

Tips:

- Accept input with `A:NAME` and print it with `T:Hello *NAME*!` (use `*VAR*` for interpolation).
- Use labels `L:START` and jumps `J:START` for simple control flow.
- You can issue `SCREEN mode` (BASIC-style) to switch screen modes; Logo will follow the selected mode.

### How to run BASIC examples

1. Open any `.bas` file from the `examples/` folder (e.g., `basic_guess.bas`).
2. Click Run (▶️). Text/graphics appear on the unified screen.
3. For `INPUT`, the 📝 prompt appears—type a value and press Enter.

Tips:

- Real-time keys: `INKEY$` (e.g., `LET K$ = INKEY$` or `PRINT INKEY$`).
- Screen control: `SCREEN mode[, w, h]`, `CLS`, and `LOCATE row, col`.
- Graphics: LINE/CIRCLE (where supported) draw on the same canvas; use View → “Save Canvas as PNG…”.

### How to run Logo examples

1. Open any `.logo` file from the `examples/` folder (e.g., `logo_star.logo`).
2. Click Run (▶️). The turtle will draw on the unified graphics canvas.
3. To save your art, use View → “Save Canvas as PNG…”.

Tips:

- Colors: `SETCOLOR RED`, `SETCOLOR 255 128 0`, or `SETCOLOR #FF8000` all work.
- Procedures: Define with `TO NAME ... END`, then call like `NAME 100 45`.
- Clear and center: `CLEARSCREEN` resets drawings and returns the turtle HOME.

### Hello World (PILOT)

```pilot
L:START
T:Hello, World!
T:This is Time Warp!
END
```

### Math Demo (Mixed Languages)

```basic
REM This is a BASIC comment
LET A = 15
LET B = 25
PRINT "A = "; A; ", B = "; B

L:PILOT_SECTION
U:SUM=*A*+*B*
T:Sum using PILOT: *SUM*
END
```

### Simple Drawing (Logo)

```logo
CLEARSCREEN
PENDOWN
FORWARD 50
RIGHT 90
FORWARD 50
```

## Keyboard Shortcuts

- `Ctrl+N` — New file
- `Ctrl+O` — Open file
- `Ctrl+S` — Save file
- `Ctrl+Z` / `Ctrl+Y` — Undo/Redo
- `Ctrl+F` — Find
- `F5` — Run

## File Formats

Time Warp IDE supports:

- `.spt` - Time Warp program files
- `.txt` - Plain text files
- `.pil` - PILOT program files

## Troubleshooting

Common issues:

1. Turtle graphics not showing: ensure your system supports GUI apps (X11/Wayland on Linux).
2. Build fails: run `rustup update` and `cargo clean && cargo build`.

Getting help:

- See examples and `docs/`
- Contact support: <james@honey-badger.org>

## System Requirements

- Rust stable toolchain (rustup, cargo)
- Linux desktop environment (X11/Wayland)
- Optional: C compiler (cc/gcc/clang) to build TempleCode to native executables
- At least 50MB disk space

---

Empowering educational programming — © 2025 Honey Badger Universe
