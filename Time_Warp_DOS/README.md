# 💾 Time Warp IDE - DOS Implementation

**🕹️ Authentic Retro Computing Experience - MS-DOS & DOSBox Compatible**

[![MS-DOS](https://img.shields.io/badge/MS--DOS-Compatible-blue.svg)](https://en.wikipedia.org/wiki/MS-DOS)
[![DOSBox](https://img.shields.io/badge/DOSBox-Tested-green.svg)](https://www.dosbox.com/)
[![C89](https://img.shields.io/badge/C89-Standard-orange.svg)](https://en.wikipedia.org/wiki/ANSI_C)

> **🎯 Part of the Time Warp Educational Platform** — See [main documentation](../docs/) for complete guides and curriculum materials.

The **DOS implementation** of Time Warp IDE delivers an authentic **retro computing experience** using pure C89. Perfect for **vintage computer labs**, **computer history courses**, and **nostalgic programming adventures** on original hardware or modern DOSBox emulation.

## 🎯 Why Choose the DOS Version?

- **🕹️ Authentic Experience**: True-to-era MS-DOS computing on original hardware
- **📚 Computer History**: Perfect for teaching computing evolution and constraints  
- **⚡ Minimal Requirements**: Runs on 8086 processors with 64KB RAM
- **🏛️ Educational Value**: Understand programming without modern abstractions
- **💾 Single File**: Complete interpreter in one portable executable
- **🔧 No Dependencies**: Pure C89 with no external libraries required

## Features (v1.0)

- Single-file C89 interpreter (no external deps)
- Text-mode; runs in MS-DOS, DOSBox, Windows 95/98/ME DOS prompt
- **Full BASIC command set**: `PRINT`, `LET`, `INPUT`, `GOTO`, `GOSUB`, `RETURN`, `IF...THEN`, `FOR...NEXT`, `END`, `STOP`, `CLS`, `REM`
- **PILOT subset**: `L:`, `T:`, `A:`, `U:`, `J:`, `Y:`, `N:` with label jumps, simple `*VAR*` interpolation, and conditional branching
- **Logo (text-mode turtle)**: `FORWARD/FD`, `RIGHT/RT`, `LEFT/LT`, `PENUP/PU`, `PENDOWN/PD`, `HOME`, `CLEAR`, `SHOW`
- **Variables**: Integer variables A..Z, String variables A$..Z$
- **Expressions**: Full arithmetic with parentheses (+, -, *, /)
- **Comparison operators**: =, <, >, <=, >=, <>
- **Interactive mode**: LIST, NEW, RUN, SAVE, LOAD for direct program entry
- Line-numbered programs with automatic sorting

## Build

You can build with OpenWatcom (DOS target) or DJGPP (32-bit DOS). Choose one of the options below.

### Option A: OpenWatcom (recommended on modern Windows)

Prereqs:

- Install OpenWatcom (v2) and ensure `%WATCOM%` is set
- Open a "OpenWatcom Build Environment" command prompt

Build:

```bat
cd Time_Warp_DOS\build
build_watcom.bat
```

Output: `..\bin\TIMEWARP.EXE`

### Option B: DJGPP (inside DOSBox or native)

Prereqs:

- DJGPP installed and `%DJGPP%` set (in DOS/DOSBox)

Build with Makefile:

```bat
cd Time_Warp_DOS\build
make -f Makefile.djgpp
```

Or build script:

```bat
cd Time_Warp_DOS\build
build_djgpp.bat
```

Output: `..\bin\TIMEWARP.EXE`

## Run

### From Command Line (DOS/DOSBox)

Run a program file:

```bat
TIMEWARP.EXE ..\examples\hello.spt
```

### Interactive Mode

Launch without arguments to start interactive mode:

```bat
TIMEWARP.EXE
```

In interactive mode, you can:

- Type line numbers to add/edit/delete lines (e.g., `10 PRINT "Hello"`)
- `LIST` - display the current program
- `RUN` - execute the program
- `NEW` - clear the program
- `SAVE filename.spt` - save program to disk
- `LOAD filename.spt` - load program from disk
- `HELP` - show available commands
- `BYE` / `EXIT` / `QUIT` - exit interactive mode

### Quick-run with DOSBox (Windows)

You can launch a pre-configured DOSBox session from this folder:

```bat
run_dosbox.bat
```

This mounts `Time_Warp_DOS` as `C:` inside DOSBox and tries to run `bin\TIMEWARP.EXE` against `examples\hello.spt`. If the binary is missing, it will prompt you to build first.

## Language Reference

### Commands

- `PRINT <expr|string|var$>` — Display text, number, or string variable
  - Use semicolon `;` at end to suppress newline
  - Example: `PRINT "Count: "; N`
- `LET <var> = <expr>` — Assign integer to variable A..Z
- `LET <var$> = "string"` — Assign string to variable A$..Z$
- `INPUT <var>` — Read integer from keyboard
- `INPUT <var$>` — Read string from keyboard
- `GOTO <line>` — Jump to a line number
- `GOSUB <line>` — Call subroutine at line number
- `RETURN` — Return from subroutine
- `IF <comparison> THEN <line>` — Conditional jump
  - Comparisons: =, <, >, <=, >=, <>
- `FOR <var> = <start> TO <end> [STEP <step>]` — Begin loop
- `NEXT <var>` — End loop, increment and test
- `CLS` — Clear screen
- `REM <comment>` — Comment (ignored)
- `END` — Terminate program
- `STOP` — Halt execution

### PILOT subset

- `L:LABEL` — Define label
- `T: text ...` — Print literal text with variable interpolation using `*A*` (integer) or `*A$*` (string)
- `A: A` or `A: A$` — Input to integer or string variable; also records the last answer for `Y:`/`N:`
- `U: <assignment>` — Update (same as BASIC LET), e.g., `U: A = 5` or `U: A$ = "HI"`
- `J: LABEL` or `J: 100` — Jump to label or line number
- `Y: LABEL|100` — If the last PILOT `A:` answer was “yes”, jump to label or line
- `N: LABEL|100` — If the last PILOT `A:` answer was “no” (or not yes), jump to label or line

Notes:

- Labels are case-insensitive, up to 31 characters, no spaces.
- Interpolation supports single-letter variables only (A..Z, A$..Z$).
- "Yes" detection: We accept common variants. If the last `A:` input (ignoring spaces and case) is `Y`/`YES`, `OK`/`OKAY`, `SURE`, `YEP`, `YEAH`, `AYE`, `AFFIRMATIVE`, `T`/`TRUE`, `ON`, or any non-zero number, then `Y:` will branch. If it is `N`/`NO`, `NOPE`, `NAH`, `NAY`, `NEGATIVE`, `F`/`FALSE`, `OFF`, or `0` (or empty/other), then `N:` will branch. BASIC `INPUT` does not affect this state—only PILOT `A:` updates the last answer.

### Logo (text-mode turtle)

Movement and state

- `FORWARD n` or `FD n` — Move n steps forward (8 directions, 45° quantized)
- `BACK n` or `BK n` — Move n steps backward
- `RIGHT deg` or `RT deg` — Turn right by degrees (rounded to nearest 45°)
- `LEFT deg` or `LT deg` — Turn left (rounded to nearest 45°)
- `HEADING deg` or `HD deg` — Point to absolute heading
- `PENUP` or `PU` — Raise pen (movement won’t draw)
- `PENDOWN` or `PD` — Lower pen (movement draws)
- `HOME` — Center the turtle (optional start mark with `MARKSTART ON`)
- `SETXY x,y` — Jump cursor to position (clamped to canvas)

Drawing primitives

- `LINE x0,y0,x1,y1` — Draw a line between two points
- `RECT x,y,w,h` — Outline rectangle
- `BOX x,y,w,h` — Filled rectangle
- `CIRCLE x,y,r` — Outline circle (midpoint algorithm)
- `DISC x,y,r` — Filled circle
- `TEXT x,y message` — Text label at x,y (quoted or raw)

Canvas and display

- `CANVAS w,h` — Resize visible canvas (max 60x20)
- `PENCHAR c` — Set drawing character (default `*`)
- `FILL [c]` — Fill entire canvas with character (default space)
- `CLEAR` — Clear canvas to spaces and reset turtle state
- `SHOW` — Render canvas (with optional border/turtle overlay) and a status line
- `BORDER ON|OFF` — Toggle border rendering
- `TURTLE ON|OFF` — Toggle turtle marker `O`
- `MARKSTART ON|OFF` — When ON and pen is down, mark start spot or HOME position
- `DUMP filename` — Save the canvas (without border) to a text file

Notes:

- Movement is clamped to the canvas edges; when a step would leave the canvas, drawing stops at the boundary.
- RIGHT/LEFT turn arguments are rounded to the nearest multiple of 45°.
- MARKSTART is OFF by default; enable it to leave a dot where you start a stroke or after moving HOME with the pen down.
- SHOW prints a one-line status (position, direction, pen state) beneath the canvas.

### Variables

- **Integer**: A..Z (single letter, stores integers)
- **String**: A$..Z$ (single letter + $, stores text up to 127 chars)

### Expressions

- Operators: `+ - * /` with standard precedence
- Parentheses supported: `(A + B) * C`
- Integer math only (no floating point)

### Comparison Operators

Use in IF statements:

- `=` equal
- `<` less than
- `>` greater than
- `<=` less than or equal
- `>=` greater than or equal
- `<>` not equal

### Interactive Commands

- `LIST` — Show program
- `NEW` — Clear program
- `RUN` — Execute program
- `SAVE <filename>` — Save to file
- `LOAD <filename>` — Load from file
- `HELP` — Show commands
- `BYE` / `EXIT` / `QUIT` — Exit
- `<number> <statement>` — Add/replace line
- `<number>` (alone) — Delete line

## Examples

See `examples/` directory:

- `hello.spt` — Hello world
- `loop.spt` — Simple loop with input
- `expr.spt` — Expression parsing with parentheses
- `forloop.spt` — FOR...NEXT loops with STEP
- `strings.spt` — String variables and INPUT
- `compare.spt` — Comparison operators
- `gosub.spt` — Subroutines with GOSUB/RETURN
- `game.spt` — Number guessing game
- `pilot_demo.spt` — Simple PILOT program with T:/A:/U:/J:/L:
- `pilot_branching.spt` — PILOT branching with `Y:`/`N:` based on last `A:` answer
- `logo_demo.spt` — Draws using Logo turtle and SHOW
- `logo_diagonal_demo.spt` — 45° turns and clamped movement
- `logo_border_toggle.spt` — Toggle BORDER/TURTLE overlays
- `logo_canvas_resize_demo.spt` — Resize canvas and observe status line
- `logo_commands_demo.spt` — BACK/HEADING/SETXY/PENCHAR/FILL showcase
- `logo_line_text_demo.spt` — LINE + TEXT + DUMP demo
- `logo_shapes_demo.spt` — RECT/BOX/CIRCLE/DISC shapes

## 📚 Learning & Documentation

### 🎓 **For Students & Beginners**
- **[📖 Student Lesson Book](../docs/STUDENT_LESSON_BOOK.md)** — Progressive curriculum adapted for text-mode constraints
- **[🎯 User Guide](../docs/USER_GUIDE.md)** — Complete DOS setup and DOSBox configuration  
- **[💾 DOS-Specific Guide](../docs/USER_GUIDE.md#dos-version)** — Vintage hardware setup and limitations

### 👨‍🏫 **For Educators**  
- **[🍎 Teacher Guide & Curriculum](../docs/TEACHER_GUIDE.md)** — Educational framework with retro computing context
- **[🏛️ Computer History Lessons](../docs/TEACHER_GUIDE.md#historical-computing)** — Teaching computing evolution
- **[⚡ Constraint-Based Learning](../docs/TEACHER_GUIDE.md#minimal-resources)** — Working within DOS limitations

### 🔧 **For Developers & Historians**
- **[⚙️ Technical Reference](../docs/TECHNICAL_REFERENCE.md)** — C89 implementation details and constraints
- **[💾 Historical Context](../docs/TECHNICAL_REFERENCE.md#dos-era-computing)** — Understanding 16-bit programming
- **[🏗️ Contributing Guide](../docs/CONTRIBUTING.md)** — Maintaining C89 compatibility

## 🕹️ Educational Value

### **Understanding Computing Constraints**
- **Memory Limits**: Work within 64KB program space like original computers
- **Text-Only Interface**: Focus on logic without visual distractions  
- **Integer Math**: Understand precision limitations and optimization
- **Command-Line Mastery**: Build essential terminal skills

### **Historical Programming Experience**
- **Authentic Environment**: Experience programming as it was in the 1980s-90s
- **Essential Skills**: Learn core concepts without modern conveniences
- **Problem-Solving**: Develop creativity within strict limitations
- **Appreciation**: Understand how far computing has advanced

## Quick Start Examples

### Example 1: Interactive Mode Session

```text
> TIMEWARP.EXE
Time Warp DOS v1.0 - Interactive Mode
Type HELP for commands, BYE to exit

> 10 PRINT "Hello from DOS!"
> 20 FOR I = 1 TO 5
> 30 PRINT I
> 40 NEXT I
> 50 END
> LIST
10 PRINT "Hello from DOS!"
20 FOR I = 1 TO 5
30 PRINT I
40 NEXT I
50 END
> RUN
Hello from DOS!
1
2
3
4
5
> SAVE myprog.spt
Saved to myprog.spt
> BYE
```

### Example 2: Running from File

```bat
TIMEWARP.EXE examples\game.spt
```

Runs the number guessing game directly.
