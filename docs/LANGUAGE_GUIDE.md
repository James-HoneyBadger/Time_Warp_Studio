# Language Guide - Time Warp Studio

Time Warp Studio actively implements and ships **11 programming languages**:
BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX, Smalltalk and Forth.

> **All eleven have reference sections below.** Each language also has a suite in
> `native/tests/`, a worked example and a showcase under `Examples/`, and rows in
> `native/share/command_rows.json`; running `make command-matrix` exercises every
> command through the native engines, so the tables here are checked against the
> code rather than written from memory.
>
> This page is the *reference*. To learn a language, start with the
> [tutorial](tutorials/) for it; to be taught it, the
> [lessons](lessons/README.md) have two per language, each with a plan, hints and
> a worked answer.

**Tools > HB-1 Computer** (`Ctrl+Alt+W`) boots all eleven languages in one
home-computer screen: a ROM selector, a prompt, and the same turtle canvas. It is
the shortest way to try a command from these tables without opening a file — see
[the HB-1 Owner's Manual](hb1/README.md) and, for each language's prompt and
direct mode, [the ROM library](hb1/roms.md). The CHIP-8 section below is joined by
[the CHIP-8 machine manual](chip8/README.md),
[the assembler](chip8/assembly.md) and
[the instruction set](chip8/instruction-set.md).

---

## Table of Contents

### ✅ Active Languages (supported in this release)

1. [BASIC](#basic)
2. [PILOT](#pilot)
3. [Logo](#logo)
4. [COMAL](#comal)
5. [REXX](#rexx)
6. [Scheme](#scheme)
7. [APL](#apl)
8. [FOCAL](#focal)
9. [CHIP-8](#chip-8)
10. [Smalltalk](#smalltalk)
11. [Forth](#forth)

[The REPL in each language](#the-repl-in-each-language) explains which bindings
survive between commands, and [SQL in nine languages](#sql-in-nine-languages) is a
Studio extension available from every language except CHIP-8 and Forth.

Each language also has a worked example and a tutorial:

| Language | Extensions | Example | Showcase | Tutorial |
| --- | --- | --- | --- | --- |
| **BASIC** | `.bas`, `.bsc` | [`hello.bas`](../Examples/basic/hello.bas) | [`showcase.bas`](../Examples/basic/showcase.bas) | [`docs/tutorials/basic.md`](tutorials/basic.md) |
| **PILOT** | `.pilot`, `.plt` | [`hello.pilot`](../Examples/pilot/hello.pilot) | [`showcase.pilot`](../Examples/pilot/showcase.pilot) | [`docs/tutorials/pilot.md`](tutorials/pilot.md) |
| **Logo** | `.logo`, `.lg` | [`hello.logo`](../Examples/logo/hello.logo) | [`showcase.logo`](../Examples/logo/showcase.logo) | [`docs/tutorials/logo.md`](tutorials/logo.md) |
| **COMAL** | `.comal`, `.cml` | [`hello.comal`](../Examples/comal/hello.comal) | [`showcase.comal`](../Examples/comal/showcase.comal) | [`docs/tutorials/comal.md`](tutorials/comal.md) |
| **REXX** | `.rexx`, `.rex` | [`hello.rexx`](../Examples/rexx/hello.rexx) | [`showcase.rexx`](../Examples/rexx/showcase.rexx) | [`docs/tutorials/rexx.md`](tutorials/rexx.md) |
| **Scheme** | `.scm`, `.ss`, `.scheme` | [`hello.scm`](../Examples/scheme/hello.scm) | [`showcase.scm`](../Examples/scheme/showcase.scm) | [`docs/tutorials/scheme.md`](tutorials/scheme.md) |
| **APL** | `.apl`, `.dyalog` | [`arrays_and_turtle.apl`](../Examples/apl/arrays_and_turtle.apl) | [`showcase.apl`](../Examples/apl/showcase.apl) | [`docs/tutorials/apl.md`](tutorials/apl.md) |
| **FOCAL** | `.foc`, `.focal` | [`square.foc`](../Examples/focal/square.foc) | [`showcase.foc`](../Examples/focal/showcase.foc) | [`docs/tutorials/focal.md`](tutorials/focal.md) |
| **CHIP-8** | `.ch8`, `.c8` | [`font_and_frames.ch8`](../Examples/chip8/font_and_frames.ch8) | [`showcase.ch8`](../Examples/chip8/showcase.ch8) | [`docs/tutorials/chip8.md`](tutorials/chip8.md) |
| **Smalltalk** | `.st`, `.smalltalk` | [`shapes.st`](../Examples/smalltalk/shapes.st) | [`showcase.st`](../Examples/smalltalk/showcase.st) | [`docs/tutorials/smalltalk.md`](tutorials/smalltalk.md) |
| **Forth** | `.fs`, `.fth` | [`hello.fs`](../Examples/forth/hello.fs) | [`showcase.fs`](../Examples/forth/showcase.fs) | [`docs/tutorials/forth.md`](tutorials/forth.md) |

---

## The REPL in each language

The REPL line under the editor submits one entry to the current document's
language. It runs the same native engine as **Run**, and its drawing goes through
the same canvas queue.

| Language | What survives between entries |
| --- | --- |
| BASIC, FOCAL, PILOT, COMAL, REXX | Variables and evaluator state. `LET X = 5` then `PRINT X` gives `5`. |
| APL | Variables and `∇` function definitions. |
| Logo | `MAKE` variables; `TO ... END` definitions do not survive because each entry is re-run as a whole program. |
| Scheme | The persistent machine: `define`/`set!` and closures; expression values are echoed. |
| Smalltalk | Nothing from the previous entry; each entry is a complete program. |
| CHIP-8 | Nothing; each entry is an assembly listing that runs and resets the machine. |
| Forth | Nothing; a definition spans as many lines as it likes, so each entry is run as a whole program and the dictionary it built goes with it. |

In BASIC, `NEW` clears the session's bindings and prints `✅ BASIC workspace
cleared`. Running a document stops and replaces the REPL session. The API's
`repl.reset` forgets every binding in a session; `repl.close` releases it. See the
[user guide](USER_GUIDE.md#3-the-repl-bar) for the controls.

---

## SQL in nine languages

The Studio embeds SQLite. Open or create a database in the 🗄 **Database** tab,
then a program can use the same connection with a `SQL` statement:

```basic
10 SQL CREATE TABLE scores (name TEXT, score INTEGER)
20 SQL INSERT INTO scores VALUES ('Ada', 42)
30 SQL SELECT name, score FROM scores
```

The line-by-line languages — BASIC, FOCAL, PILOT, COMAL, REXX and APL — get this
from the shared interpreter loop. Logo writes the same statement as a command:

```logo
SQL SELECT * FROM scores
```

Scheme uses a primitive:

```scheme
(sql "SELECT * FROM scores")
```

and Smalltalk asks its `SQL` class:

```smalltalk
Transcript show: (SQL run: 'SELECT * FROM scores').
```

A query prints its columns, rows and row count; an insert or update prints how
many rows changed; an error prints the database's own message. The database
manager and the running program share one connection, so a table created at the
prompt is visible to the next `SQL` statement without reopening anything.
CHIP-8 is the one language without a `SQL` form: a bytecode program has no
strings to carry a statement.

---

## BASIC

Classic BASIC language with Turbo BASIC graphics extensions.

In the REPL, a line beginning with a number is stored in the editor as a
numbered program line rather than executed; `RUN`, `LIST` and `NEW` are the
classic direct-mode commands. `?` is accepted as shorthand for `PRINT`.

### Variables and Types

```basic
' Numbers
X = 42
PI = 3.14159

' Strings
NAME$ = "Hello"
MESSAGE$ = "World"

' Arrays (one dimension only)
DIM NUMBERS(10)
NUMBERS(1) = 100
```

### Input and Output

```basic
' PRINT outputs to console
PRINT "Hello, World!"
PRINT "Value:", X

' ? is the classic shorthand for PRINT
? "Hello again"

' INPUT reads from user
INPUT "Enter name: ", NAME$
INPUT "Enter number: ", X

' Sandboxed Sequential File I/O (confined to active workspace)
OPEN "scores.txt" FOR OUTPUT AS #1
PRINT #1, NAME$
PRINT #1, X
CLOSE #1

OPEN "scores.txt" FOR INPUT AS #1
LINE INPUT #1, SAVED_NAME$
INPUT #1, SAVED_SCORE
CLOSE #1
```

### Control Flow

```basic
X = 1

' IF statement
IF X > 10 THEN
    PRINT "X is large"
ELSE
    PRINT "X is small"
END IF

' FOR loop
FOR I = 1 TO 10
    PRINT I
NEXT I

' WHILE loop
WHILE X < 100
    X = X + 1
    PRINT X
WEND

' DO...LOOP
DO
    PRINT X
    X = X + 1
LOOP UNTIL X > 100
```

### Subroutines

```basic
10 X = 4
20 GOSUB 100
30 PRINT RESULT
40 END
100 RESULT = X * 2
110 RETURN
```

Subroutines are called by line number. Named labels are not supported.

```basic
' Single-line function
DEF FNA(N) = N * N
PRINT FNA(5)
```

A `FUNCTION ... END FUNCTION` block is the other spelling, for a body that needs more
than one line. Its answer is whatever the variable named after it holds when `END
FUNCTION` is reached, and a `$` on the name makes it a text function:

```basic
FUNCTION TWICE(N)
  TWICE = N * 2
END FUNCTION

FUNCTION GRADE$(MARK)
  IF MARK >= 50 THEN GRADE$ = "pass"
  IF MARK < 50 THEN GRADE$ = "fail"
END FUNCTION

PRINT TWICE(21); " "; GRADE$(64)
```

Subroutines can also be named blocks, called with `CALL`:

```basic
CALL GREET("Ann")
END

SUB GREET(NAME$)
  PRINT "Hello "; NAME$
END SUB
```

`EXIT FUNCTION`, `EXIT SUB`, `EXIT FOR`, `EXIT DO` and `EXIT WHILE` leave that block
immediately — how a function returns early, and how a loop stops without a flag
variable:

```basic
FOR I = 1 TO 100
  IF I * I > 50 THEN EXIT FOR
NEXT I
PRINT "the first square over 50 is "; I
```

### String Operations

```basic
NAME$ = "Ann"
MESSAGE$ = "Hello World"

' String concatenation
GREETING$ = "Hello " + NAME$

' String length
MSG_LEN = LEN(MESSAGE$)

' Substring
PART$ = MID$(MESSAGE$, 1, 5)
```

Use `UCASE(s)` or `UCASE$(s)` to convert text to uppercase, and `LCASE(s)` or `LCASE$(s)` to convert it to lowercase.

### Graphics

```basic
' Switch to graphics mode
SCREEN 1

' White foreground, black background
COLOR 15, 0

' Draw a line, circle, pixel, and fill
LINE (10, 10) - (100, 50)
CIRCLE (160, 100), 40
PSET (5, 5)
PAINT (160, 100), 4

' Combined example
SCREEN 1
FOR I = 0 TO 360 STEP 10
    X = 150 + 100 * COS(I * PI / 180)
    Y = 100 + 100 * SIN(I * PI / 180)
    CIRCLE (INT(X), INT(Y)), 5
NEXT I
```

`SCREEN 1` also gives the program a screen it can read back: `POINT` answers the
colour at a pixel, and `GET`/`PUT` move a rectangle of pixels around.

Which screen a program asks for is the mode's number, and the numbers mean what
they meant:

| `SCREEN` | Size | Colours | What it was |
| --- | --- | --- | --- |
| `0` | text (80x25 by default) | — | text mode; `SCREEN 0, cols, rows` resizes within 40-120 by 20-50 |
| `1` | 800x600 | 16 | the studio's graphics mode; `SCREEN 1, w, h` picks another size |
| `2` | 640x200 | 16 | CGA graphics |
| `7` | 320x200 | 16 | EGA 320x200 |
| `9` | 640x350 | 16 | EGA 640x350 |
| `12` | 640x480 | 16 | VGA 640x480 |
| `13` | 320x200 | 256 | VGA 320x200, 256 colours |

A number that was never a mode is refused with the list of the ones that were, so
a typo says what it could have said instead. The numbered modes above 1 are a
native addition: the reference engine only ever had `SCREEN 1`, and the parity
gate therefore cannot compare them — `lang_tests` and the shipped game are what
hold them up.

The size is not decoration. When a program names a screen, the drawing surface
stops fitting itself to the ink and becomes that screen: (0, 0) is the top-left
pixel, x grows right and y grows down, and the picture is shown at a whole-number
scale so a pixel stays a square. `SCREEN 0` — or the end of a run — gives the
fit-to-content drawing back. This is what a game needs and a drawing does not.

```basic
SCREEN 13
PSET (10, 10), #FF0000
PRINT POINT(10, 10) / 65536      ' 255: the red channel of &H00FF0000

' A 16x16 tile captured once and stamped five times
LINE (92, 92)-(108, 108), 12, BF
GET (92, 92)-(108, 108), TILE
FOR I = 1 TO 5
    PUT (I * 40, 150), TILE
NEXT I

SCREEN 0                          ' text mode: POINT now answers -1
```

`POINT` answers `-1` when there is no graphics screen, which is how a program
asks whether it has one. `GET` stores the width in element 0 of the array and the
height in element 1, then the pixels row by row from the top-left corner; it sizes
the array itself, so a sprite needs no `DIM` first. `GET #n` and `PUT #n` are
still the file channel — the `#` is what tells the two apart.

The screen is the program's own record of what it drew: the canvas keeps the
primitives `LINE`, `CIRCLE`, `PSET` and `PAINT` always emitted — the log, the
scrubber and the vector export read them — and the pixels are what `POINT`, `GET`
and `PUT` answer from, and what is shown. A restored sprite is drawn too, so a
captured tile is not only remembered. `PAINT` also floods the region that contains
its point — so a filled area can be captured with `GET`. `CLS` clears both, and
`SCREEN 0` discards the screen along with the mode.

`SPRITE` is the same tile with a name and a memory: it captures a rectangle of the
screen once and then moves without being re-sent.

```basic
SCREEN 1
LINE (10, 10)-(40, 50), 4, BF
SPRITE SHIP, 10, 10, 30, 40      ' capture: the top-left corner is the hotspot
SPRITE SHIP, 100, 100            ' place it there
SPRITEMOVE SHIP, 5, -5           ' and move it by a delta
SPRITEHIDE SHIP                  ' put back what it covered
SPRITESHOW SHIP                  ' and draw it again
SPRITE SHIP, OFF                 ' forget it altogether
```

A placement is a bitmap on a bitmap screen and a shape on a drawing surface, so
hiding one puts back the pixels it covered rather than leaving a hole. Placing a
name that was never captured is an error naming the statement that would capture
one, and moving one that was never placed says so — the point of the statement is
the sprite, not the acknowledgement.

Two statements paint a shape that changes colour across itself:

```basic
GRADIENT 0, 0, 199, 99, 1, 4, 0          ' rectangle; two corners; ink 1 to ink 4
GRADIENT 0, 0, 199, 99, 1, 4, 90         ' the same, shaded upwards
ELLIPSEGRADIENT 20, 120, 180, 250, 4, 1  ' the ellipse such a rectangle contains
```

`GRADIENT` takes the two opposite corners, the colour it starts with and the one it
ends with, then an optional angle in degrees (0 runs left to right, 90 runs upwards).
Both are drawn into the screen a program asked `SCREEN` for, and both are native
additions: the reference had no gradient, so `lang_tests` holds them up rather than
the parity corpus.

[`Examples/basic/star_patrol.bas`](../Examples/basic/star_patrol.bas) is a whole
program built on this: a sprite ship, a star field, a shot whose hits are found
with `POINT`, and a score bar.

### Waiting, and the timer

`SLEEP n` waits n seconds (clamped to sixty), and a **stop request breaks the wait**
rather than being ignored until it ends: the sleep is taken in two-millisecond
slices. `ON TIMER(n) GOSUB line` arranges for `line` to be called every n seconds
while the program runs, and `TIMER ON` / `TIMER OFF` arm and disarm it:

```basic
10 ON TIMER(0.5) GOSUB 100
20 TIMER ON
30 FOR J = 1 TO 10
40 SLEEP 0.5
50 NEXT J
60 TIMER OFF
70 END
100 TICKS = TICKS + 1 : RETURN
```

The handler runs at a **statement boundary**, so it never interrupts a statement
half-way, and it cannot interrupt itself: a handler that takes longer than the
interval simply runs late. `TIMER` returns the seconds this run has been going.

Both statements used to be acknowledgements — `SLEEP` printed "SLEEP 0.1s" and
returned, and `TIMER ON` set a flag nothing read.

### Sound

The synth is the machine's own kind: square waves and note lists, not sampled
audio. `SOUND` takes a frequency in hertz and a duration in ticks (1/18.2 of a
second, the IBM PC's timer); `PLAY` takes the note notation the dialects shared.

```basic
BEEP                             ' the system bell, about 880Hz
SOUND 440, 18                    ' A above middle C for about a second
PLAY "T120 O4 L8 CDEFGAB"        ' a scale
PLAY "T120 L4 C E G > C"         ' a chord's own arpeggio, an octave up
SOUND OFF                        ' silence
```

`PLAY` reads `T` (tempo, quarter notes per minute), `L` (default note length),
`O` (octave), `>` and `<` (octave up and down), note letters `A`–`G` with
`#`/`+`/`-`/`b` accidentals, an optional octave digit per note, `.` for dotted,
`P` or `R` for a rest, and the numeric form `N1`–`N84` counting from C0.

Sound needs a player on the machine (`paplay`, `aplay`, `ffplay` or `play`).
Without one the statements still run and still acknowledge — they are silent,
not broken — because a program should not refuse to draw just because a laptop
has no speaker.

`SAY` is the exception: the build has no speech synthesiser, so it answers with
the reference's own refusal, `Speech synthesis not available`, rather than
acknowledging something that did not happen.

### Keyboard

```basic
K$ = INKEY$
IF K$ = "q" THEN END
```

`INKEY$` answers the next key typed at the drawing canvas, or the empty string
when nothing has been typed. Click the canvas first: that is where a game is
played, so that is where the keys are read. A headless run has no keyboard and
answers empty, which is what the parity gate records. It is therefore not a
useful REPL command: the REPL has no running program for the buffer to feed.

### Gamepad

`JOYINIT` opens the first joystick the machine has; `STICK` reads an axis and
`STRIG` a button.

```basic
JOYINIT
X = 160
FOR FRAME = 1 TO 3600                     ' about a minute at 16ms a frame
    IF STICK(0) < 100 THEN X = X - 4      ' axis 0, left
    IF STICK(0) > 156 THEN X = X + 4      ' axis 0, right
    IF STRIG(0) = 1 THEN FIRE = 1         ' button 0 is held
    PSET (X, 100), #FFFFFF
    WAIT 16
    IF INKEY$ = "q" THEN END
NEXT FRAME
```

`STICK(n)` answers 0-255 with a resting stick at 128, and `STRIG(n)` answers 1
while a button is held. Both answer 0 until `JOYINIT` has found a pad, so a machine
with a joystick plugged in does not change a program that never asked to read one.
`JOYINIT` reports the device it opened or that there is none: a gamepad is
optional hardware, not an error in the program.

On Linux the pad is `/dev/input/js0`, or the first of `js0` to `js3` that opens;
`TIME_WARP_GAMEPAD=/dev/input/js1 ./time_warp_ide` names one instead. Windows and
macOS builds report no pad rather than a backend that cannot read one.

### Drawing without the turtle: `DRAW`

`DRAW` takes GW-BASIC's path string. The turtle executes it, so it draws and
leaves the turtle where the path ended.

```basic
DRAW "U20 R20 D20 L20"           ' a square, from the turtle's current spot
DRAW "BM50,50 C4 R20 D20 L20 U20" ' move without drawing, colour 4, square
```

| Letter | Meaning |
| --- | --- |
| `U n`, `D n` | up (forward) and down (back) |
| `R n`, `L n` | turn right and left |
| `E n`, `H n` | the same, as the dialects' diagonal steps |
| `F n`, `G n` | forward and back, the turtle spelling |
| `B`, `N` | pen up, pen down |
| `M x,y` | move to a point, drawing if the pen is down |
| `C n` | pen colour, as a palette index |

### Sound, pauses and frames

A game loop needs pacing, and `WAIT` is the pause in milliseconds:

```basic
SCREEN 1
FOR FRAME = 1 TO 100
    CLS
    CIRCLE (FRAME * 3, 100), 10
    WAIT 16                     ' about sixty frames a second
NEXT FRAME
```

### Ports: serial and parallel

A program can talk to a board — an Arduino, a Raspberry Pi's GPIO over a USB
adapter, a Micro:bit, or the parallel-port projects these machines were built
around. Ports are addressed by channel, with the `#n` spelling a file already
uses.

```basic
' A serial device at 9600 baud
SERIAL OPEN #1, "/dev/ttyUSB0", 9600
SERIAL WRITE #1, "PING"
SERIAL READ #1, ANSWER$, 2000    ' wait up to two seconds for a line
SERIAL CLOSE #1
PRINT ANSWER$

' The parallel port: eight LEDs on the data lines
PARALLEL OPEN #1, "/dev/lp0"
PARALLEL BYTE #1, 255
PARALLEL WRITE #1, "hello"
PARALLEL CLOSE #1
```

`SERIAL READ` reads one line (without its terminator) and puts it in the
variable; the timeout is optional and defaults to 1000ms. A timeout with nothing
arrived is not an error — a program polling a quiet sensor keeps going.

**Which devices a program may open.** Only the serial and parallel device
families: `/dev/ttyS*`, `/dev/ttyUSB*`, `/dev/ttyACM*`, `/dev/ttyAMA*`,
`/dev/serial/…`, `/dev/pts/*`, `/dev/lp*`, `/dev/parport*` and `/dev/usb/lp*`.
Anything else is refused with a sentence rather than attempted, because a
program that can open any device node can also open a disk. A teacher whose
board appears under another name can allow it:

```bash
TIME_WARP_PORTS=/dev/mydevice:/dev/ttyOTHER0 ./time_warp_ide
```

Serial ports are opened raw (no echo, no line translation) and read with
timeouts; writes wait rather than dropping bytes when the board is slower than
the program.

> **Note** — `DRAW`, `WAIT`, `SERIAL`, `PARALLEL`, `POINT`, sprite `GET`/`PUT` and
> the gamepad are native extensions: the retired reference engine implemented
> none of the first four nor the screen readback, and answered `STICK`/`STRIG`
> with 0 because it shipped no backend. The parity gates do not compare them —
> a gamepad's answer depends on the machine, not on the program — and
> `BEEP`/`SOUND`/`PLAY` keep the reference's acknowledgement text exactly.

### Math Functions

```basic
' Basic math: absolute value 5, square root 4, integer part 3
PRINT ABS(-5)
PRINT SQR(16)
PRINT INT(3.7)

' Trigonometry (angles in radians)
PRINT SIN(0)
PRINT COS(0)
PRINT TAN(0)
PRINT ATAN(1)

' Other: e^x, natural log, random 0 to 1
PRINT EXP(0)
PRINT LOG(1)
PRINT RND(1)
```

### Complete Example

```basic
SCREEN 1
COLOR 15, 0

' Draw boxes
FOR I = 0 TO 9
    X1 = 10 + I * 30
    Y1 = 10 + I * 20
    X2 = X1 + 20
    Y2 = Y1 + 15
    LINE (X1, Y1) - (X2, Y2)
NEXT I

' Draw circles
FOR I = 1 TO 10
    CIRCLE (RND * 320, RND * 200), RND * 50
NEXT I

PRINT "Graphics complete"
```

### Turtle Movement (Logo words)

A BASIC program may also drive the turtle with Logo's words. A statement BASIC
does not recognise whose first word is a Logo command is handed to the Logo
executor — the same blend the reference makes — which is how a program can mix
`PRINT` with direction-and-distance drawing:

```basic
CLEARSCREEN
FORWARD 15
RIGHT 90
LEFT 90
PENUP
PENDOWN
SETPENCOLOR "YELLOW"
SETPENWIDTH 2
HOME
```

A word that is neither BASIC nor Logo is still reported as
`❌ Unknown BASIC command`, so a typo cannot quietly do nothing.

---

## Logo

Logo language with turtle graphics primitives.

### Turtle Movement

```logo
; Move forward 100 units (FORWARD and FD are the same)
FORWARD 100
FD 100

; Move back 50 units (BACK and BK are the same)
BACK 50
BK 50

; Turn right and left, in degrees
RIGHT 90
RT 90
LEFT 45
LT 45

; Return to center, facing up
HOME

; Move to an absolute position, then set the heading
SETXY 100 50
SETHEADING 90
```

### Pen Control

```logo
; Enable drawing (default)
PENDOWN
PD

; Disable drawing
PENUP
PU

; Pen color by name (quote the name)
SETPENCOLOR "red

; Or by palette slot: a bare number is a slot, not a brightness
SETPC 1

; Pen width
SETPENWIDTH 2

; Pen style: 0 solid, 1 dashed, 2 dotted, 3 dash-dot, 4 invisible.
; The second number scales the dash length; leaving it out means 1.
PENSTYLE 1
FD 50
PENSTYLE 2, 2
FD 50
PENSTYLE 0
```

`PENSTYLE` is a **native extension**, not a reference command: the canvas learned to
draw a dashed pen in 19.0, and a pen style no program can ask for is a feature nobody
has. Style 4 is an invisible pen — the turtle moves, nothing is inked, and the stroke
does not even appear in the scrubber, because a frame that draws nothing is not a
frame.

### Shapes

```logo
; Logo has no CIRCLE, RECTANGLE, or POLYGON commands.
; Build shapes with REPEAT:
REPEAT 4 [FD 100 RT 90]
REPEAT 6 [FD 50 RT 60]
```

`BEZIER` draws a curve the turtle cannot walk to, and then leaves the turtle on its
end facing along it — so two of them joined make a smooth curve rather than a corner:

```logo
; BEZIER cx1 cy1 cx2 cy2 x y -- a cubic from where the turtle stands
BEZIER 0 60 60 60 60 0
BEZIER 120 0 120 -60 60 -60
```

Also a **native extension**: the canvas has drawn a bezier since 19.0 and no
language could ask for one until this word existed. The two control points pull the
curve towards themselves without the turtle ever visiting them, and the heading the
turtle ends with is the curve's own end tangent.

### Procedures (Subprograms)

```logo
TO SQUARE
    FORWARD 100
    RIGHT 90
    FORWARD 100
    RIGHT 90
    FORWARD 100
    RIGHT 90
    FORWARD 100
    RIGHT 90
END

; Call the procedure
SQUARE
```

### Recursion

```logo
TO TREE :SIZE
  IF :SIZE < 5 [STOP]
  FD :SIZE
  RT 30
  TREE (:SIZE / 2)
  LT 60
  TREE (:SIZE / 2)
  RT 30
  BK :SIZE
END

; Draw the tree
TREE 50
```

### Complete Example

```logo
TO STAR :SIZE
  REPEAT 5 [FD :SIZE RT 144]
END

; Red
SETPENCOLOR "red
STAR 100

; Green, then a rotated star
SETPENCOLOR "green
LT 36
STAR 50
```

---

## PILOT

PILOT language for interactive computer-aided instruction (CAI).

### T-Units (Text Output)

```pilot
T: Welcome to the lesson
T: This is an informational message
T: Multiple T-units print sequentially
```

### A-Units (Answer Input)

A plain `A:` stores the reply in `ANSWER$`. `A:$NAME` stores it in `NAME$` and shows a `?` prompt. Put the question on a `T:` line before `A:$NAME`; a text prompt on `A:` followed by `A:$NAME` asks the user twice.

```pilot
T: What is your name?
A:$NAME
T: Hello, $NAME!
```

### M-Units (Matching)

`M:` compares the last answer with a comma-separated list of alternatives. Matching is case-insensitive. Alternatives may use `*` and `?` wildcards; plain text matches if it appears anywhere in the answer. After a match, `$LEFT`, `$MATCH`, and `$RIGHT` are set.

```pilot
T: What is the capital of France?
A:
M: PARIS
TY: Correct!
TN: Wrong. The capital is Paris.
```

Multiple alternatives:

```pilot
T: Which is a color? (red, monday)
A:
M: RED,GREEN,BLUE
TY: Yes, that is a color.
TN: No, that is not a color.
```

### Conditional Execution and Jumps

- `TY:` / `TN:` print only when the last `M:` matched / did not match.
- `JY: LABEL` / `JN: LABEL` jump to a label on a match / no match.
- `J: LABEL` always jumps. Jump targets are single labels; `J: A,B` is not supported.

Labels are written as `*NAME` on their own line.

```pilot
T: Start the lesson
J: LESSON1

*LESSON1
T: Lesson 1 content
J: LESSON2

*LESSON2
T: Lesson 2 content
E:
```

### Subroutines

`U:*LABEL` calls a labeled block; `E:` inside the block returns to the caller.

```pilot
U: *GREET
T: Back in the main program
E:

*GREET
T: Hello from the subroutine
E:
```

### Complete Example

```pilot
T: Math Quiz
T: Answer each question.

T: What is 3 * 4?
A:
M: 12
TY: Correct!
TN: Wrong. The answer is 12.

T: What is 5 + 5?
A:
M: 10
TY: Correct!
TN: Wrong. The answer is 10.

T: Quiz complete
```

---

## COMAL

COMAL-80 adds structured programming to the BASIC family: `IF … ENDIF`,
`FOR … NEXT`, `WHILE … ENDWHILE`, `REPEAT … UNTIL` and named procedures.

### Output

```comal
10 PRINT "Hello, World!"
20 PRINT "Total", 6 * 7      REM comma inserts a space
30 PRINT "Sum"; 15           REM semicolon does not
```

### Variables

```comal
10 COUNT := 0              REM := assigns
20 TITLE$ := "Report"      REM $ suffix means string
30 COUNT := COUNT + 1
```

### Selection

```comal
10 SCORE := 87
20 IF SCORE >= 90 THEN
30   PRINT "Grade A"
40 ELIF SCORE >= 80 THEN
50   PRINT "Grade B"
60 ELSE
70   PRINT "Keep going"
80 ENDIF
```

### Loops

```comal
10 FOR I := 1 TO 5
20   PRINT I
30 NEXT I

40 N := 0
50 WHILE N < 3
60   N := N + 1
70 ENDWHILE

80 REPEAT
90   PRINT N
100 UNTIL N <= 0
```

### Procedures

A `FUNC` returns a value and can be used inside an expression; a `PROC` is called for
its effect. A function body may declare its own variables with `LOCAL`:

```comal
100 FUNC DOUBLE(N)
110 LOCAL T
120 T = N * 2
130 RETURN T
140 ENDFUNC
```

`T` is the function's while it runs and the caller's afterwards whether or not the
caller had one. A `FUNC` body is statements: it may branch, loop and call another
function, and its `RETURN` leaves the body and answers the expression that called
it:

```comal
100 FUNC TOTAL(N)
110 LOCAL SUM
120 SUM = 0
130 FOR I := 1 TO N
140 SUM = SUM + I
150 NEXT I
160 RETURN SUM
170 ENDFUNC
```

A `PROC` is still the right shape for work whose point is what it prints or draws:

```comal
10 PROC GREET(NAME$)
20   PRINT "Hello, "; NAME$
30 ENDPROC
40 CALL GREET("World")
```

### Arrays and DATA

```comal
10 DIM SCORES(5)
20 DATA 12, 47, 31, 8, 25
30 FOR I := 1 TO 5
40   READ SCORES(I)
50 NEXT I
```

### Turtle graphics

```comal
10 FOR SIDE := 1 TO 4
20   MOVE 100
30   TURN 90
40 NEXT SIDE
```

**File extension:** `.comal`

---

## REXX

REXX is a typeless string-processing language: every value is a string, and
variables never need declaring.

### Output

```rexx
SAY 'Hello, World!'
SAY 6 * 7
```

### Variables and concatenation

```rexx
name = 'Time Warp'
SAY 'Hello' 'World'            /* abuttal:  HelloWorld */
SAY 'Hello' || ' ' || 'World'  /* Hello World */
```

### Branching

```rexx
IF score > 50 THEN SAY 'pass'

IF score >= 90 THEN DO
  SAY 'Grade A'
END
ELSE DO
  SAY 'Keep going'
END

SELECT
  WHEN score > 90 THEN SAY 'huge'
  OTHERWISE SAY 'small'
END
```

### Loops

```rexx
DO i = 1 TO 5
  SAY i
END

DO n = 10 TO 1 BY -3
  SAY n
END

DO WHILE count < 3
  count = count + 1
END

DO word OVER 'alpha beta gamma'
  SAY word
END
```

`LEAVE` breaks out of a loop and `ITERATE` skips to the next pass.

### Input and parsing

```rexx
PULL name
line = 'red green blue'
PARSE VAR line first second third
```

### Subroutines

```rexx
CALL factorial 6
SAY '6! =' RESULT
EXIT

factorial:
  n = ARG(1)
  result = 1
  DO i = 2 TO n
    result = result * i
  END
  RETURN result
```

### Built-in functions

`LENGTH`, `SUBSTR`, `WORD`, `WORDS`, `UPPER`, `LOWER`, `STRIP`, `SPACE`,
`LEFT`, `RIGHT`, `COPIES`, `REVERSE`, `INDEX`, `ABS`, `MAX`, `MIN`, `TRUNC`,
`SIGN`, `ARG`.

### Turtle graphics

`FORWARD`/`MOVE`, `BACK`, `RIGHT`/`TURN`, `LEFT`, `PENUP`, `PENDOWN`, `HOME`.

**File extension:** `.rexx`

---

## Scheme

Scheme is a small Lisp dialect. Operators come first and everything is a list.

### Output

```scheme
(display "Hello, World!")
(newline)
```

### Definitions and arithmetic

```scheme
(define pi 3.14159)
(define (square n) (* n n))

(display (* 6 7))
(newline)
```

### Conditionals

```scheme
(define (sign n)
  (cond ((> n 0) "positive")
        ((< n 0) "negative")
        (else "zero")))
```

### Recursion

```scheme
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
```

### Lists and higher-order procedures

```scheme
(display (car (list 1 2 3)))
(display (map (lambda (x) (* x x)) (list 1 2 3 4)))
(display (filter odd? (list 1 2 3 4 5)))
(display (apply + (list 10 20 30)))
```

### Local bindings and closures

```scheme
(display (let ((x 3) (y 4)) (+ x y)))

(define (make-adder n) (lambda (x) (+ x n)))
(define add10 (make-adder 10))
(display (add10 5))
```

### Turtle graphics

`forward`/`fd`, `backward`/`bk`, `right`/`rt`, `left`/`lt`, `goto`, `penup`,
`pendown`, `home`, `circle`, and `color`, which takes three numbers (red, green
and blue, 0–255).

```scheme
(define (square size)
  (define (loop n)
    (when (> n 0)
      (forward size)
      (right 90)
      (loop (- n 1))))
  (loop 4))
(square 100)
```

**File extension:** `.scm`

---

---

## APL

Iverson's *A Programming Language* (1962; interactive APL\360 in 1966) is the one
language here where the **shape** of the data is the program. Two of its
properties surprise everyone the first time: there is **no operator precedence**,
and most functions work on whole arrays rather than on single numbers.

Executed line by line. Each line is an expression, and its value is displayed -
which is exactly what APL's original interface did, and what makes it feel like a
calculator you can write a program with.

### Values

```apl
42             a scalar
1 2 3          a vector (a "strand")
2 3 ⍴ ⍳6       a 2-by-3 table
'HELLO'        a character vector
(1 2)(3 4)     a nested vector
```

`¯` is the negative sign, so `¯3` is minus three. `*` is **power**, not
multiplication: APL is old enough that `*` meant exponentiation before C made it
a multiplication sign. `×` is times.

### There is no precedence

A function takes the whole expression on its right. Evaluation is right to left.

```apl
2 × 3 + 4     14, not 10 - the + 4 happens first
```

### Scalar functions

Scalar functions are **pervasive**: the operation reaches into arrays, which is
why `1 2 3 + 10` and `1 2 3 + 4 5 6` both work while `1 2 3 + 4 5` is a length
error.

| Glyph | Keyword | Meaning |
| ----- | ------- | ------- |
| `+` `-` `×` `÷` | `PLUS` `MINUS` `TIMES` `DIVIDE` | Arithmetic |
| `*` `⍟` | `POWER` `LOG` | Power and logarithm |
| `⌈` `⌊` | `CEILING` / `FLOOR` | Round up or down with one argument; `MAX` / `MIN` with two |
| `\|` | `ABS` / `RESIDUE` | Absolute value, or remainder |
| `×` | `SIGNUM` | Sign: `× ¯3` is `¯1` |
| `÷` | `RECIPROCAL` | Reciprocal |
| `=` `≠` `<` `≤` `>` `≥` | `EQUAL` `NOTEQUAL` `LESS` `LESSEQ` `GREATER` `GREATEREQ` | Comparison |
| `~` `∧` `∨` `⍲` `⍱` | `NOT` `AND` `OR` `NAND` `NOR` | Logic |
| `?` | `ROLL` | Random integers |

### Structural functions

| Glyph | Keyword | Meaning |
| ----- | ------- | ------- |
| `⍳` | `IOTA` / `INDEXOF` | `⍳5` is `1 2 3 4 5`; `A ⍳ n` finds a position |
| `⍴` | `RHO` / `SHAPE` / `RESHAPE` | Shape with one argument, reshape with two |
| `⌽` | `REVERSE` / `ROTATE` | Reverse, or rotate by n |
| `↑` `↓` | `TAKE` `DROP` | Take from the front, or drop from it |
| `⍉` | `TRANSPOSE` | Transpose |
| `,` | `RAVEL` / `CATENATE` | Flatten, or join |
| `∈` | `MEMBERSHIP` | Which members of the left are in the right |
| `⍋` `⍒` | `GRADEUP` `GRADEDOWN` | Positions that sort the data |
| `⍕` | `FORMAT` | The character form of a value |

Every function has a **keyword spelling** as well as its glyph, so a keyboard
that cannot produce `⍳` is not a keyboard that cannot write APL.

### Operators

Reduce and scan take the function on their left:

```apl
+/⍳100            5050
+REDUCE 1 2 3     6         the keyword spelling of +/
+SCAN 1 2 3       1 3 6
```

APL's operator *axes* (`+/[2]`) are not implemented, so reduce and scan work
along the outermost axis.

### Defined functions

```apl
∇ R ← DOUBLE X
  R ← X × 2
∇
DOUBLE 21
```

### Turtle graphics

A **Time Warp extension** here, as for COMAL and REXX: `FD`/`FORWARD`, `BK`,
`RT`, `LT`, `PU`, `PD`, `HOME`, `CS`.

The turtle's position is read with **`TX`**, **`TY`** and **`THETA`**. Those names
are deliberate: `X`, `Y` and `H` are ordinary APL variables, and taking them from
the program to report the turtle's position would break the most natural line
anyone writes.

```apl
FD 100
RT 90
FD 100
TX
```

### What is not implemented

`∘.` outer product, `⍤` rank, the composition operators, the system functions
`⎕`, and `→` branch with labels. A program that uses one is told which symbol
stopped it rather than being given a wrong answer.

**File extension:** `.apl`, `.dyalog`

---

## FOCAL

FOCAL - **FO**rmula **CAL**culator - is DEC's line-numbered teaching language
from 1969. It was built to be taught in an afternoon: ten commands, one number
type, and labels of the form `NN.NN` so that a group of five lines can be named
as a unit.

Executed line by line, like BASIC and COMAL. Statements have a historic
single-letter form and a full name; both work.

| Command | Short | Does |
| ------- | ----- | ---- |
| `ASK` | `A` | Prompt for a value |
| `TYPE` | `T` | Print text, formats and expressions |
| `SET` | `S` | Assign |
| `IF` | `I` | Three-way branch on the sign of an expression |
| `FOR` | `F` | Loop over the rest of the line |
| `DO` | `D` | Run a group; `RETURN` hands control back to the line after the `DO` |
| `RETURN` | `R` | Leave the innermost `DO` group (an extension, not DEC's) |
| `GOTO` | `G` | Jump to a group |
| `COMMENT` | `C` | The rest of the line is text |
| `ERASE` | `E` | Forget variables: `ERASE ALL`, or `ERASE X,Y` |
| `QUIT` | `Q` | Stop |

### Line labels

Labels are `NN.NN`: a **group** number and a **line** number, each up to two
digits. The parts are compared as numbers, so `01.10` and `1.10` name the same
line, and `GOTO 9.9` reaches a label written `09.09`.

That two-part shape is easy to mistype. `01.30` is group 1 line **30**, so it is
reached as `1.30` - not `1.3`, which is group 1 line 3, a different line. A
reference to a label that does not exist is reported, which is how you find out.

```focal
01.10 T "START"
01.20 GOTO 1.40
01.30 T "NOT REACHED"
01.40 T "LANDED"
```

### TYPE

`T` prints text and expressions. `%n` sets the number of decimal places and holds
until the next `%n`; the default is two. The format belongs to the session rather
than to one statement, which is why the last line below sets it again.

```focal
01.10 T "X=",2+3          X=5.00
01.20 T %0,"R=",7/2       R=4
01.30 S X=5
01.40 T %2,"X is ",X      X is 5.00
```

`!` suppresses the newline **within one TYPE**, so `T "A"!"B"` prints `AB`. It
cannot join output across two statements: the engine logs each statement's output
separately, so the newline between them is added by the logger rather than by the
statement.

### SET and ASK

```focal
01.10 SET X=2+3
01.20 ASK "How many? " N
01.30 TYPE "You said ",N
```

`ASK` takes a quoted prompt and the name of the variable to fill. A comma before
the name is accepted, because every other FOCAL statement separates its items with
one; the historic spelling has just a space.

Undefined variables are **0**. FOCAL zeroes every variable at load. This is worth
stating because BASIC does the opposite, and echoing a name back is the failure
mode that makes a typo look like a result.

### IF branches on a sign

FOCAL has **no relational operators**. `IF (X) a,b,c` goes to `a` when X is
negative, `b` when it is zero and `c` when it is positive. An omitted target falls
through to the next line.

```focal
01.10 ASK "Number? " N
01.20 IF (N) 1.30,1.40,1.50
01.30 T "NEGATIVE"
01.35 GOTO 1.60
01.40 T "ZERO"
01.45 GOTO 1.60
01.50 T "POSITIVE"
01.60 QUIT
```

`IF (N) ,,1.50` reads "if N is positive, go to 1.50".

There is no ELSE and no fall-through control, so a three-way branch that prints
three different things needs the extra `GOTO` lines above. There is also no
statement separator: `;` separates statements **inside a FOR body only**, and
`T "A";T "B"` is an error naming the `;`.

### FOR and DO

`FOR` loops over the rest of the line, and the loop variable keeps the last value
the body saw - so after `FOR I=1,4`, I is 4 rather than the 5 that failed the
test.

```focal
01.10 FOR I=1,3;T %0,"I=",I
```

The `;` belongs to the FOR: it separates the statements that make up the body,
which is the rest of the line.

`DO` calls a group. There is no RETURN statement in FOCAL-69: a group called with
`DO` is expected to end where the program ends. A nested `DO` is bounded by a
depth limit, so a cycle reports itself instead of hanging.

```focal
01.10 DO 2.10
01.20 QUIT
02.10 T "INSIDE"
```

### Functions

`FABS`, `FSQT`, `FEXP`, `FLOG`, `FSIN`, `FCOS`, `FATN`, `FITR` (truncate),
`FRAN` (random), `FSGN` (sign) and `FPI`.

`FPI` and `FRAN` take **no parentheses, because FOCAL writes them that way**:
`T FPI` prints 3.14. `FRAN` optionally takes a seed: `FRAN(1)`.

```focal
01.10 T %5,"pi=",FPI
01.20 T "sq=",FSQT(16)
```

### Turtle graphics

A **Time Warp extension**: 1969 FOCAL had no graphics at all. `FORWARD`/`FD`,
`BACK`/`BK`, `RIGHT`/`RT`, `LEFT`/`LT`, `PENUP`/`PU`, `PENDOWN`/`PD`, `HOME`.

```focal
01.10 FOR I=1,4;FORWARD 100;RIGHT 90
```

### Deliberate differences

- **An unrecognised command is an error, not a comment.** Some FOCAL listings are
  annotated by writing text after the line number; here that is what
  `C`/`COMMENT` is for, and `TIPE` reports itself instead of printing nothing.
- **Output field widths are not a reproduction.** `%n` means "n decimal places"
  here, which is a chosen default rather than whatever a PDP-8 printed.

**File extension:** `.foc`, `.focal`

---

## CHIP-8

CHIP-8 is Joseph Weisbecker's bytecode interpreter from 1977 - the machine the
first home computers were taught with. It is not a text language: a program is a
sequence of two-byte opcodes and the display is a framebuffer of lit and unlit
pixels: 64x32 in the original mode, 128x64 in SuperChip hi-res. So this executor
is two things in one file: a small **assembler** that turns mnemonics into
opcodes, and the **machine** that runs them.

Executed whole-program. An opcode stream cannot be interpreted a line at a time,
so the listing is assembled in full and then run. For the live machine — a clock,
a keypad, an inspector and SuperChip — see
[the CHIP-8 machine manual](chip8/README.md); for every opcode and quirk, see
[the instruction set](chip8/instruction-set.md).

A `.ch8` file containing a **NUL byte** is a binary ROM rather than a listing:
its bytes are copied to `0x200` unchanged, up to the 3584 that fit before the end
of memory. The editor opens such a file read-only, because a keystroke in it
would be a corrupted program waiting to be saved.

**Tools > CHIP-8 Emulator** (Ctrl+Shift+M) runs the same bytes as a live
machine: the screen in either mode, a keypad that can be pressed while the program runs,
Run/Pause/Step/Reset, a speed control, the sound timer's buzzer, and registers,
stack and memory beside the screen. It opens on the bundled `paint.ch8` when the
current document is not a CHIP-8 program, and its **Listing** tab assembles and
runs typed source (`Ctrl+Return`) without using the main editor. The Run button
keeps the whole-program model described below, because a lesson, a gate and an
animation want different answers from the same file.

**ROM Library...** in that window browses a folder of programs the user supplies,
recursively, remembering the folder after a successful open. The studio ships no
ROMs: most CHIP-8 programs in circulation are somebody else's copyright, so the
library reads a folder the user has the right to use rather than bundling one.

```chip8
; a comment
start:  LD   V0, 0x41
        LD   I, hexfont
        LD   F, V0
        DRW  V0, V1, 5
        HALT
hexfont: DW  0x0000
```

### Numbers

Decimal unless marked, and marked either way round, because both spellings appear
in listings: `0x41` and `41h` are both 65, and a bare `41` is decimal 41.

### Directives

| Directive | Does |
| --------- | ---- |
| `DB n` | Emit one byte |
| `DW n` | Emit one word |
| `HALT` | Stop the machine |
| `; keys <digits>` | Present keypad state for the run |

### The instruction set

The complete original -- all 35 opcodes, including the arithmetic group, the
random and timing registers, the font and BCD conversions, and memory block
load/store -- plus the SuperChip 1.1 set a hi-res ROM needs. Those extensions
have no mnemonics in this assembler; write them with `DW` and a comment.

| Mnemonic | Meaning |
| -------- | ------- |
| `CLS` | Clear the display |
| `SYS addr` | Ignored; the original's RCA 1802 call |
| `JP addr` / `JP V0, addr` | Jump |
| `CALL addr` / `RET` | Subroutine call and return |
| `SE Vx, byte` / `SNE Vx, byte` | Skip if equal / not equal |
| `LD Vx, byte` / `LD Vx, Vy` / `LD I, addr` | Load |
| `LD F, Vx` / `LD B, Vx` / `LD [I], Vx` | Font, BCD and memory block |
| `ADD` `SUB` `SUBN` `OR` `AND` `XOR` | Arithmetic and logic |
| `SHR Vx` / `SHL Vx` | Shift |
| `RND Vx, byte` | Random, masked |
| `DRW Vx, Vy, n` | Draw an n-byte sprite |
| `SKP Vx` / `SKNP Vx` | Skip on key pressed / not pressed |
| `LD Vx, K` / `LD Vx, DT` / `LD ST, Vx` | Keypad and timers |
| `00FE` / `00FF` (`DW`) | Select 64x32 / 128x64; clears the screen |
| `00Cn` / `00FB` / `00FC` / `00FD` (`DW`) | Scroll down `n`, right 4, left 4; exit |
| `DXY0` | 16x16 sprite in hi-res; 8x16 with doubled columns in 64x32 |
| `Fx30` / `Fx75` / `Fx85` (`DW`) | 8x10 font address, store/load RPL flags |

### Where the original decides

This follows the 1977 interpreter rather than a later emulator, because that is
the machine the books describe:

- **`SHR`/`SHL` shift `Vx` in place.** The later convention of shifting `Vy` into
  `Vx` is not used. The y operand is accepted and ignored, so both spellings
  assemble.
- **Sprites wrap, and a collision sets `VF`.** Drawing a lit pixel onto a lit
  pixel clears it and reports the collision.
- **`LD [I], Vx` and `LD Vx, [I]` leave `I` one past the block.** CHIP-48 and
  SCHIP leave it where it was; this machine increments it. A program that needs
  the old value sets `I` again after the transfer.
- **`SYS` (`0NNN`) is ignored.** It calls a routine on the RCA 1802 that hosted
  the original interpreter, which this machine cannot run. Ignoring it keeps a
  ROM that contains one runnable, and the assembler still accepts the mnemonic.

`JP V0, addr` is the jump-table form: `Bnnn` always adds `V0`, so naming any
other register is refused by the assembler rather than assembling to an
instruction that silently ignores it.

### The display

The display is drawn in the canvas's own coordinates with filled rectangles, and
its two colours come from the canvas palette. Both modes fill a 512x256 canvas: a
64x32 screen at a scale of eight, or 128x64 at four. MegaChip's drawing planes and
audio extensions are not implemented, so `Fx01` and the MegaChip sound opcodes are
reported as unknown rather than half-drawn.

### The keypad

Presented by directive, not by typing: `; keys 13` marks keys 1 and 3 as held
down. `LD Vx, K` then reads the first held key, or 0. A program runs to completion
inside one executor call, and the engine has no way to hand input to something
that never yields control.

### Frames

A Run simulates frames rather than timing them: ten opcodes make a frame and the
timers tick once per frame, with no real clock. A fixed number of frames is the
limit, so a program that does not halt reports that it stopped rather than
hanging. The interactive emulator uses the same ten opcodes per frame by
default, but a 60 Hz clock and its speed control change how many opcodes each
frame executes; it also lets `LD Vx, K` wait for a press, where a Run can only
read the `; keys` directive.

**File extension:** `.ch8`, `.c8`

---

## Smalltalk

Smalltalk-80 is the language that made "everything is an object" a working
proposal rather than a slogan. There is **no syntax for control flow at all**:
`ifTrue:`, `whileTrue:` and `to:do:` are messages sent to objects. It fills the
one paradigm gap the lineup had - nothing else here is object-oriented.

Executed whole-program, like Scheme and CHIP-8. A file defines classes and
methods and then runs some code, and a method body is more than one line, so the
whole source is read before anything runs.

### Objects and classes

```smalltalk
Object subclass: #Counter
  instanceVariableNames: 'count'

Counter >> initialize
  count := 0

Counter >> increment
  count := count + 1

Counter >> value
  ^count

| c |
c := Counter new.
c increment.
c value
```

`Object subclass: #Counter` is not special syntax. It is the keyword message
`subclass:instanceVariableNames:` sent to the class object `Object`, exactly as in
a real file-in.

### Method lookup

A message walks the receiver's class chain looking for the selector, then stops. A
selector that reaches the top is a `doesNotUnderstand:` error, reported with the
selector and the receiver's class. A class has two method dictionaries - one for
its instances and one for the class itself - so `Counter new` and `c increment`
find their methods by the same lookup from different sides.

### Precedence

Unary, then binary, then keyword, left to right within a level. Parentheses group.

```smalltalk
2 + 3 * 4        20 - both are binary, so left to right
1 max: 2 + 3     5 - binary binds tighter than keyword
```

### Variables

In widening scope: method arguments and temporaries, then instance variables,
then globals (the class names a definition introduced). Assignment to a name that
is not one of those creates a global, which is how the workspace bar behaves.

```smalltalk
| total |
total := 0.
1 to: 10 do: [ :i | total := total + i ].
total
```

### Blocks are closures

A block captures the activation it was created in, so a block held in an instance
variable and run later still reads and writes the variables where it was written.

### Non-local return

`^` inside a block returns from the **method that created the block**, not from
the block. This is the part of Smalltalk that surprises people, and faking it -
returning from the block instead - gives a wrong answer that looks right in a
simple test.

### Cascades

`aStream show: 'x'; cr` sends `cr` to the object the *first* message went to, not
to the result of `show:`. That distinction is the entire reason `;` exists.

### Source layout

A definition is a header - `ClassName >> selector`, or `X subclass: #Y` - followed
by *indented* lines, and it ends at the next line that begins in column 0. Lines
outside a definition are top-level code, and consecutive ones form a single block
whatever their indentation. There is no `!` chunk terminator, so indentation
carries the job it does in a file-in.

### What is displayed

A chunk displays its **last value, and only if that value is not nil**. This is
the workspace's "print it" applied once per block of top-level code. Echoing every
statement would print an object for each assignment and each `do:`, and the object
you were talking to is not the answer you asked for. Real output belongs in
`Transcript show:`, as it does in Smalltalk.

### Turtle graphics

A **Time Warp extension**, as for COMAL, REXX, FOCAL and APL:

```smalltalk
| t |
t := Turtle new.
1 to: 4 do: [ :i | t forward: 100. t right: 90 ]
```

### Deliberate simplifications

- **No metaclass tower.** A class object holds two method dictionaries rather than
  being an instance of a metaclass that is an instance of a metaclass.
  `Counter class` answers the class object itself.
- **Immediate values are represented natively** - an integer is a number, a string
  is a string - and a send dispatches on the receiver's class, which is the
  standard way to get Smalltalk semantics without allocating a wrapper per
  addition.
- **No exceptions, processes, `thisContext`, `perform:` or compiler.** There is no
  `ensure:`, `on:do:` or semaphore.

**File extension:** `.st`, `.smalltalk`

---

## Forth

**Era:** 1970 · **Paradigm:** stack, dictionary, words · **Execution:** whole program
**Extensions:** `.fs`, `.fth` · **Example:** [`hello.fs`](../Examples/forth/hello.fs)

Forth has no syntax in the sense the other languages here have. A program is a
sequence of whitespace-separated **words**, and a word takes whatever it needs from
the **stack**: `2 3 +` is not `2 + 3`, it is "push two, push three, add". Every
argument is already there when the word runs, which is why the order looks
backwards until it clicks, and why `FD 100` is an error rather than a move: `FD`
runs first, and there is nothing under it. Write `100 FD`.

The dictionary is the other half. `: NAME ... ;` adds a word, and from then on it
is indistinguishable from a built-in one — which is how Forth programs grow a
vocabulary suited to their problem rather than fitting the problem to the language.

```forth
: SQUARE ( n -- n*n ) DUP * ;
: FACT ( n -- n! ) DUP 1 > IF DUP 1- RECURSE * THEN ;

9 SQUARE .        \ prints 81
6 FACT .          \ prints 720
CR
```

Forth here is whole-program, like Scheme, Logo, Smalltalk and CHIP-8: a definition
spans as many lines as it likes, so the engine has to see the whole source before it
can decide where the definition ends.

### The stack

| Word | Effect | Notes |
| --- | --- | --- |
| `DUP` | `( a -- a a )` | copies the top |
| `DROP` | `( a -- )` | discards the top |
| `SWAP` | `( a b -- b a )` | |
| `OVER` | `( a b -- a b a )` | copies the second |
| `ROT` | `( a b c -- b c a )` | rotates the third to the top |
| `-ROT` | `( a b c -- c a b )` | the other way |
| `NIP` | `( a b -- b )` | drops the second |
| `TUCK` | `( a b -- b a b )` | copies the top under the second |
| `?DUP` | `( a -- a a )` or `( 0 -- 0 )` | copies only a non-zero |
| `DEPTH` | `( -- n )` | how many items are on the stack |
| `2DUP` `2DROP` `2SWAP` | two items at a time | |
| `PICK` | `( ... a n -- ... a a )` | copies the n-th item down |
| `ROLL` | `( ... a n -- ... a )` | moves the n-th item up |
| `.` | `( a -- )` | prints the top and a space |
| `.S` | `( -- )` | prints the whole stack as `<depth> item item ...`, leaving it alone |

Stack **underflow** is an error that names the word that found it (`❌ Forth, line
1: .: stack underflow`), because a silently missing argument is the bug that takes
an afternoon to find.

### Arithmetic, comparison and bits

`+` `-` `*` `/` `MOD` `/MOD` `NEGATE` `ABS` `MIN` `MAX` `1+` `1-` all take their
operands from the stack and leave one result. `/` and `MOD` are integer division;
`/MOD` leaves the remainder under the quotient.

Comparisons — `=` `<>` `<` `>` `<=` `>=` `0=` `0<` `0>` — answer the Forth **flag**:
`-1` for true (all bits set) and `0` for false. Logic is bitwise: `AND` `OR` `XOR`
`INVERT` `LSHIFT` `RSHIFT`.

Integers are 64-bit and print in decimal; `$FF` is hexadecimal and `%1010` is binary.

### Definitions, variables and memory

```forth
: DOUBLE ( n -- n*2 ) 2 * ;
VARIABLE TOTAL
0 TOTAL !
: ADD-UP ( n -- ) 1+ 1 DO I TOTAL +! LOOP ;
10 ADD-UP
." 1 to 10 is " TOTAL @ . CR
99 CONSTANT NINETY
```

`VARIABLE NAME` makes a word that pushes the variable's address; `!` is
`( value address -- )` and `@` is `( address -- value )`, so the address is on top.
`C!` and `C@` are the byte forms, `+!` adds to what a variable holds. `CONSTANT`
pops a number once and pushes it forever. `@`/`!` address this language's own 64 KiB
data space, two bytes per cell, allocated upwards from the end of the dictionary.

`POKE` and `PEEK` are the machine's memory rather than the dictionary's:

```forth
65 32768 POKE      \ write 'A' into the first character cell of the tube
32768 PEEK .       \ read it back -- 65
```

On the HB-1 those addresses *are* the screen, so a Forth program draws text by
poking character cells (`$8000` is row 0, column 0, eighty cells to a row) exactly as
BASIC's `POKE` does, and a write into the ROM region is refused out loud rather than
accepted and dropped. Run anywhere else — in the IDE, or through `time_warp_check` —
the same words read and write a memory map of their own, so a poke and a peek still
agree.

`RECURSE` compiles a call to the word being defined. A definition is registered in
the dictionary at `:` rather than at `;`, so naming it works too — `RECURSE` is the
standard spelling and the one the tutorial teaches.

### Control structures

| Form | Meaning |
| --- | --- |
| `flag IF ... THEN` | run the body when the flag is true |
| `flag IF ... ELSE ... THEN` | either body |
| `BEGIN ... flag UNTIL` | repeat until the flag is true |
| `BEGIN ... flag WHILE ... REPEAT` | repeat while the flag is true |
| `limit start DO ... LOOP` | run with `I` from *start* to *limit-1* |
| `limit start DO ... 2 +LOOP` | step by the stack's top instead of by one |
| `... LEAVE ...` | leave the innermost `DO` loop early |
| `... EXIT` | return from the word |

`I` is the innermost loop's counter and `J` the next one out; neither consumes
anything. `LEAVE` may be written inside an `IF` within the loop body.

### Turtle words

`FD`/`FORWARD`, `BK`/`BACK`, `RT`/`RIGHT`, `LT`/`LEFT`, `PU`/`PENUP`,
`PD`/`PENDOWN`, `HOME`, `CS`/`CLEARSCREEN`, `SETPC`/`SETPENCOLOR` (a palette slot),
`SETPW` (pen width), `ST`/`SHOWTURTLE`, `HT`/`HIDETURTLE`. They are postfix like
everything else:

```forth
: SQUARE ( size -- ) 4 0 DO DUP FD RT 90 LOOP DROP ;
CS
1 SETPC
60 SQUARE
```

### Output and the rest

`." text"` prints text; the space after `."` is the delimiter rather than the first
character, so `." hello"` prints `hello`. `EMIT` prints a character from its code,
`CR` ends a line, `SPACE` and `SPACES` print one and *n* spaces, `WORDS` lists the
dictionary, and `BYE` ends the program where it stands. `(` ... `)` and a backslash
to the end of the line are comments.

### Deliberate simplifications

- **No `CREATE ... DOES>`, vocabularies, `FIND`, metacompilation or pictured
  numeric output.** Those are the parts of Forth that make it a language for
  writing Forths. A learner meeting Forth for the first time needs the stack, the
  dictionary and the loop.
- **`IMMEDIATE` is accepted and does nothing.** There are no user immediate words,
  and a listing copied from a book should not be rejected for one line.
- **No floating point.** Forth here is an integer machine, like the ones it ran on.

---

## Language Selection

To select which language your code uses:

1. **File Extension**: Save with correct extension

| Extension | Language |
| ----------- | ---------- |
| `.bas` / `.bsc` | BASIC |
| `.pilot` / `.plt` | PILOT |
| `.logo` / `.lg` | Logo |
| `.comal` / `.cml` | COMAL |
| `.rexx` / `.rex` | REXX |
| `.scm` / `.ss` / `.scheme` | Scheme |
| `.apl` / `.dyalog` | APL |
| `.foc` / `.focal` | FOCAL |
| `.ch8` / `.c8` | CHIP-8 |
| `.st` / `.smalltalk` | Smalltalk |
| `.fs` / `.fth` | Forth |
