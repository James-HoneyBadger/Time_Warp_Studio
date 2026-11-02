# Language reference

Every command the ten languages of Time Warp Studio accept, what each one does, and
the smallest program that shows it. Each language has a section of its own; the
headings are the index.

All ten run in process, in C++. There is no interpreter to install and no Python
behind them: the IDE, `./twc` and `time_warp_check` all call the same engines, so a
program behaves the same however it is run. Two shapes exist. Six languages are
line by line, and each line is a statement the interpreter hands to its executor:
BASIC, APL, PILOT, FOCAL, COMAL and REXX. Four are whole program, and the source is
read to the end before anything runs: Logo, Scheme, Smalltalk and Forth.

The ten are not variations on one language. Each answers a question the others do
not, and knowing what a language is for is more useful than knowing its command
list:

| Language | Files | Shape | What it is for |
| --- | --- | --- | --- |
| BASIC | `.bas` `.bsc` | line by line | The machine everybody recognises: numbered lines, a graphics screen, sprites, sound |
| APL | `.apl` `.dyalog` | line by line | Whole arrays as values, and an expression as the program |
| Logo | `.logo` `.lg` | whole program | Turtle drawing, and a procedure as the unit of thought |
| PILOT | `.pilot` `.plt` | line by line | Teaching: a lesson is text units, answers and matched alternatives |
| FOCAL | `.foc` `.focal` | line by line | Arithmetic at a formula calculator's prompt, in numbered groups |
| Scheme | `.scm` `.ss` `.scheme` | whole program | Lists, recursion, and procedures as values |
| COMAL | `.comal` `.cml` | line by line | Structured BASIC: blocks, procedures, functions |
| REXX | `.rexx` `.rex` | line by line | Text: every value is a string, and parsing is a statement |
| Smalltalk | `.st` `.smalltalk` | whole program | Objects, messages and classes |
| Forth | `.fs` `.fth` | whole program | The stack and the dictionary: a program that grows its own vocabulary |

To find a command, go to the language and read its headings: each section is grouped
by what the commands are for, so BASIC then Graphics covers the drawing statements.
Several spellings of one command share a row. The second column says what the command
does, and the third gives the smallest program that shows it, on one line where one
line is enough and empty where the fenced example above the table demonstrates it.
Capitals are the documented spelling, and where a language ignores case in source
— BASIC, COMAL, REXX and Forth — either form works. Runtime messages are quoted
without their leading mark character, which every diagnostic begins with.

## BASIC

Line by line, in the dialect a microcomputer of 1981 understood and a few things it
did not: `SCREEN` modes with pixels, sprites, gradients, sound. Lines may be
numbered or not; a jump needs a number, and nothing else does.

### Program control

```basic
10 GOSUB 100
20 PRINT RESULT
30 END
100 RESULT = 42
110 RETURN
```

| Command | What it does | Example |
| --- | --- | --- |
| `GOTO` | Jumps to the line numbered in the statement. | `GOTO 20` |
| `GOSUB` and `RETURN` | Calls the numbered line and comes back at the matching `RETURN`. | `GOSUB 100` |
| `IF ... THEN` and `ELSE` and `END IF` | Runs one block or the other; the block form ends at `END IF`, and `ENDIF` is the same word. | `IF X > 10 THEN` |
| a one-line `IF` | Runs the rest of the line when the test holds, and the part after `ELSE` when it does not. | `IF X > 9 THEN GOTO 40 ELSE GOTO 50` |
| `FOR ... TO ... STEP` and `NEXT` | Counts from the start to the limit inclusive, by one or by the step; written on one line, the loop repeats within that line. | `FOR I = 1 TO 3` |
| `NEXT` | Closes the loop the variable names, which is optional. | `NEXT` |
| `WHILE` and `WEND` | Repeats while the test holds. `WEND` may be on a later line or later in the same one. | `WHILE X < 3` |
| `DO` and `LOOP` | Repeats a block; the loop line may carry `WHILE` or `UNTIL`. | `DO: X = X + 1: LOOP UNTIL X >= 3` |
| `UNTIL` | The test on a `LOOP` that ends the loop when it becomes true. | `LOOP UNTIL X = 2` |
| `EXIT FOR`, `EXIT DO`, `EXIT WHILE` | Leaves that loop from inside it. | `IF I = 3 THEN EXIT FOR` |
| `SELECT CASE` and `CASE` and `END SELECT` | Chooses one body. A clause is a value, a comma-separated list of values, `a TO b`, `IS <comparison>` or `ELSE`, and the first clause that matches is the only one whose body runs. | `SELECT CASE X` |
| `SUB` and `END SUB` | A named block called with `CALL`, whose parameters are local, and whose body is on its own lines. | `SUB DOUBLE(N)` |
| `CALL` | Calls a `SUB` or a `FUNCTION` by name. | `CALL GREET("Ann")` |
| `FUNCTION` and `END FUNCTION` | A named block used in an expression; its answer is the variable named after it when `END FUNCTION` is reached. | `PRINT TWICE(21)` |
| `EXIT FUNCTION` | Returns from a `FUNCTION` early, keeping the answer so far. | `EXIT FUNCTION` |
| `DEF FN` | A one-line function of one variable. | `DEF FNA(N) = N * N: PRINT FNA(5)` |
| `TYPE` and `END TYPE` | Declares a record type; its fields are `field AS type` lines, so they cannot share the header's line. | `TYPE POINT` |
| `ON ERROR GOTO` | Diverts to a numbered line when a statement fails. | `ON ERROR GOTO 100` |
| `ON ERROR` | Removes that diversion, so the next failure is reported. | `ON ERROR` |
| `RESUME` | Returns to the numbered line given, after handling an error. | `100 RESUME 30` |
| `RESUME NEXT` | Returns to the statement after the one that failed. | `100 RESUME NEXT` |
| `END` | Ends the run where it stands. | `PRINT "A": END: PRINT "B"` |
| `RUN` | Clears the run's variables, reports `Program reset`, and ends the current run. | `RUN` |
| `SYSTEM` | Ends the run and reports `Returned to system prompt`. | `SYSTEM` |
| `REM` and `'` | A comment to the end of the line, spelled as a word or as an apostrophe. | `REM ignored` |
| `:` | Separates two statements on a line. | `PRINT "A": PRINT "B"` |

`SELECT CASE` takes the first clause that matches and runs that body alone. A clause
`CASE 2` compares the selector with one value, `CASE 2, 3` with a list of them,
`CASE 4 TO 9` with a range, `CASE IS > 10` with a comparison and `CASE ELSE` takes
whatever is left; the selector is read once for the block, when its first clause is
tested. A clause that cannot be read reports `Invalid CASE expression` and its body is
skipped, and blocks may nest. A variable may begin with a reserved word: `DOUBLE`,
`RUNNER`, `CLOSER`, `NEXTX` and `LOOPS` are ordinary names.

### Output and input

| Command | What it does | Example |
| --- | --- | --- |
| `PRINT` | Writes its items; a comma between them writes a space, a semicolon nothing. | `PRINT "HELLO WORLD"` |
| `?` | The same statement. | `? 6 * 7` |
| `PRINT USING` | Formats numbers with a mask; `#` is a digit and `.` the point. | `PRINT USING "##.##"; 3.14159` |
| `PRINT #`, `WRITE #` | Write a record to a channel; `WRITE #` keeps its quotes so `INPUT #` can read it back. | `PRINT #1, "HELLO"` |
| `LINE INPUT`, `LINE INPUT #` | Reads a whole line into a text variable, from the user or a channel. | `LINE INPUT "Name: ", N$` |
| `INPUT`, `INPUT #` | Prompts and reads one value per name, from the user or a channel. | `INPUT "Name: ", N$` |
| `INKEY$` | The next key typed at the canvas, or the empty string. A headless run has no keyboard, so it answers empty. | `IF INKEY$ = "q" THEN END` |
| `CLS`, `LOCATE`, `WIDTH` | Clears the screen; moves the text cursor; sets the text width and reports it. | `LOCATE 5, 5` |

### Variables, arrays and strings

| Command | What it does | Example |
| --- | --- | --- |
| assignment | Stores a value; the `$` on a name makes it text. | `X = 42` |
| `LET`, `MAKE` | The same statement, spelled out, or in the form BASIC dialects of the period shared. | `MAKE "X" 7` |
| `DIM`, `DIM ... AS` | Declares an array indexed from 0, or a record from a `TYPE`. | `DIM N(10): N(1) = 7` |
| `ERASE` | Discards an array, which then reads as zeroes. | `ERASE A` |
| `SWAP` | Exchanges two variables. | `SWAP A, B` |
| `CONST` | Names a value, and refuses a later assignment to that name. | `CONST SIZE = 5` |
| `DEFINT` | Makes the named variables integer, truncating on assignment. | `DEFINT X: X = 3.7` |
| `DEFLNG`, `DEFSNG`, `DEFDBL` | The same declaration for long, single and double. | `DEFDBL A-Z` |
| `DEFSTR` | Makes the named variables text. | `DEFSTR A-Z` |
| `DATA` | Stores constants for `READ`. | `DATA 3, 4` |
| `READ`, `RESTORE` | Takes the next `DATA` items; rewinds `DATA` to its first. | `READ A: READ B` |
| `LEN`, `MID$`, `CHR$`, `REPEAT$` | Length; a substring counting from 1; a character from its code; a string repeated. | `MID$("HELLO", 1, 3)` |
| `FORMAT$` | `#`-mask or printf formatting, whichever the format looks like. | `FORMAT$("#.##", 3.14159)` |
| `STR$` | A number as text. | `STR$(12)` |
| `TYPE$` | The type a name has: `DOUBLE`, `INT` or `STRING`. | `TYPE$(X)` |
| `TIME$`, `DATE$` | The clock as `HH:MM:SS`, eight characters; the date as `MM-DD-YYYY`, ten. | `A$ = TIME$` |
| `TIMER` | Seconds since local midnight. | `TIMER >= 0` |

`TIME$`, `DATE$`, `TIMER`, `INKEY$` and `TYPE$(...)` are understood as a whole
`PRINT` item and nowhere else, so `LEN(TIME$)` is 0: read the clock into a variable
first.

### Mathematics

| Command | What it does | Example |
| --- | --- | --- |
| `ABS`, `SQR` | Absolute value; square root. | `ABS(-5)` |
| `INT` | The largest integer not greater than the argument, so a negative rounds down. | `INT(-3.7)` |
| `SIN`, `COS`, `TAN` | Trigonometry in radians. | `SIN(0)` |
| `ATAN` | Arc tangent in radians. | `ATAN(1)` |
| `EXP` | e to the power of the argument. | `EXP(0)` |
| `LOG` | Natural logarithm. | `LOG(1)` |
| `RND` | A random number from 0 up to 1. | `RND(1)` |
| `RANDOMIZE` | Seeds the generator; `RANDOMIZE TIMER` seeds it from the clock. | `RANDOMIZE 42` |

Numbers print with six significant digits: `PRINT 1 / 3` gives `0.333333` and a
large pixel value prints as `1.67772e+07`. A literal may be written in exponent form
-- `1E6`, `1e-6`, `2.5E+3` -- with or without the sign, and `1E6` is the number a
million rather than 1 beside a variable called `E6`.

### Graphics

The drawing statements need a screen. `SCREEN 1` is 800 by 600 with 16 colours and
`SCREEN 1, w, h` picks another size; the modes of the period exist with their own
sizes — 0 for text, 2 at 640 by 200, 7 and 13 at 320 by 200, 9 at 640 by 350, 12 at
640 by 480 — and a number that was never a mode is refused with the list of the ones
that were: `Value 5 for screen mode is not one of 0, 1, 2, 7, 9, 12, 13`.

```basic
SCREEN 1
LINE (10, 10)-(100, 50), 4
CIRCLE (160, 100), 40
```

| Command | What it does | Example |
| --- | --- | --- |
| `SCREEN` | Chooses a mode and a size. | `SCREEN 1` |
| `COLOR` | Sets foreground and background and reports `Colors set`. | `COLOR 15, 0` |
| `PSET` | Draws one pixel in a colour. | `PSET (5, 5)` |
| `PRESET` | Draws one pixel in the background colour. | `PRESET (5, 5)` |
| `LINE` | Draws a line between two corners; a second comma and `BF` fill the box. | `LINE (10, 10)-(40, 50), 4, BF` |
| `CIRCLE` | Draws a circle from a centre and a radius. | `CIRCLE (160, 100), 40` |
| `PAINT` | Floods an area with a colour from a point inside it. | `PAINT (50, 30), 4` |
| `PALETTE CYCLE` | Rotates a range of palette slots and reports the range. | `PALETTE CYCLE 1, 15, 1` |
| `SETXY` | Moves the turtle to a point. | `SETXY 10 20` |
| `FORWARD`, `RIGHT`, `PENUP`, `PENDOWN` and the rest of Logo's turtle | A statement BASIC does not recognise is offered to the Logo executor, so turtle words work here. | `CLEARSCREEN: FORWARD 15: RIGHT 90` |
| `SPRITE` | Captures a rectangle of the screen under a name, places it, or removes it with `OFF`. | `SPRITE SHIP, 10, 10, 30, 40` |
| `SPRITEMOVE` | Moves a placed sprite by a delta. | `SPRITEMOVE SHIP, 5, -5` |
| `SPRITEHIDE` | Hides a sprite and puts back what it covered. | `SPRITEHIDE SHIP` |
| `SPRITESHOW` | Draws a hidden sprite again. | `SPRITESHOW SHIP` |
| `GRADIENT` | Fills a rectangle with a colour ramp between two inks, at an optional angle. | `GRADIENT 0, 0, 100, 50, 4, 1, 45` |
| `ELLIPSEGRADIENT` | The same ramp inside the ellipse a rectangle contains. | `ELLIPSEGRADIENT 0, 0, 60, 40, 2, 3` |
| `FRACTAL` | With no argument, lists the presets; with one, draws it. | `FRACTAL KOCH` |
| `SHAPE` | Draws a named shape, or a polygon of n sides. | `SHAPE POLYGON 6, 40` |
| `PARTICLE` | Creates a named particle effect at a point. | `PARTICLE EXPLOSION, 100, 100` |

`GRADIENT` and `ELLIPSEGRADIENT` need a screen and report an error without one, as
does `SPRITE`; placing a name that was never captured reports the statement that
would capture one.

### Sound, timing and the machine

| Command | What it does | Example |
| --- | --- | --- |
| `BEEP` | The system bell, and the report `BEEP`. | `BEEP` |
| `SOUND` | A square wave at a frequency for a number of ticks of 1/18.2 second. | `SOUND 440, 2` |
| `PLAY` | A note string: `T` tempo, `L` length, `O` octave, `>` and `<`, `A` to `G`, rests. | `PLAY "T120 O4 L8 CDEFGAB"` |
| `SAY`, `SPEAK` | Speech, which this build does not have: both report `Speech synthesis not available`. | `SAY "HELLO"` |
| `SLEEP` | Waits that many seconds, clamped to 60. | `SLEEP 1` |
| `SPEED` | Sets the turtle's speed and reports it. | `SPEED 5` |
| `ON TIMER` | Calls a numbered line every n seconds. | `ON TIMER(2) GOSUB 100` |
| `TIMER ON` | Arms that timer. | `TIMER ON` |
| `TIMER OFF` | Disarms it. | `TIMER OFF` |
| `POKE` | Writes a byte into the program's own 64 KiB map. | `POKE 1000, 7` |
| `PEEK` | Reads it back, as a statement or as a function. | `PEEK 1000` |
| `OUT` | Writes a byte to a port number. | `OUT 100, 5` |
| `IN` | Reads one back. | `IN 100` |
| `JOYINIT` | Opens the first gamepad and reports which, or that there is none. | `JOYINIT` |
| `STICK` | An axis value, and 0 until `JOYINIT` has found a pad. | `STICK(0)` |
| `STRIG` | A button, 1 while held. | `STRIG(0)` |
| `SHELL` | Refused: `SHELL commands are disabled in IDE for security`. | `SHELL "echo hi"` |

`SLEEP` is taken in two-millisecond slices, so a stop request breaks it, but the clock
is read between statements: `SLEEP 20` runs its twenty seconds first.

### Files

Every file statement takes a channel number written `#n`. The file manager confines
them to the active workspace.

| Command | What it does | Example |
| --- | --- | --- |
| `OPEN` | Opens a file for `INPUT`, `OUTPUT` or `APPEND` on a channel. | `OPEN "scores.txt" FOR OUTPUT AS #1` |
| `CLOSE` | Closes one channel, or all of them; reports which. | `CLOSE #1` |
| `PUT #`, `GET #` | Channels only: a `#` after either word means the file, and without an open channel the statement reports `File #9 is not open`. | `PUT #9, , "X"` |

### Differences worth knowing

- **A jump needs a number.** `GOTO`, `GOSUB` and `ON ERROR GOTO` take a line
  number; `GOSUB HERE` reports `Expected numeric value for line number, got 'HERE'`
  and the run carries on with the next line.
- **`FOR` counts past its limit.** The variable keeps the first value beyond the
  end: `FOR I = 1 TO 3` leaves 4, and `FOR J = 3 TO 1` has no step to count
  backwards with, so its body runs once. The limit is tested after the pass, so a
  header already past its limit does the same whether it is written on one line or
  as a block.
- **`INT` rounds down**, so `INT(-3.7)` is -4, and division by zero is an error.
  Numbers print with six significant digits: `1 / 3` is `0.333333`.
- **A failed statement is reported, not skipped quietly**: an unknown statement
  reports `Unknown BASIC command: ...` and a malformed loop reports
  `FOR requires format: var = start TO end [STEP step]`.
- **`SUB`, `FUNCTION` and `TYPE` bodies need their own lines.** A colon ends the
  header, so a definition split by colons is refused with `SUB ... needs its own
  lines: a definition cannot be split by colons` rather than run as statements.
  Loops and plain statements take colons: `WHILE X < 3 : PRINT X : X = X + 1 : WEND`
  and `FOR I = 1 TO 3 : PRINT I : NEXT` repeat inside the line, and `EXIT FOR` inside
  one leaves the loop rather than the line.

## APL

Line by line, and every line is an expression whose value is shown: the language is
a calculator you can write a program with. Arrays are values, so most functions work
on whole vectors and tables rather than on single numbers. Each function has a glyph
and a keyword, and both are accepted, so a keyboard that cannot produce `⍳` is not a
keyboard that cannot write APL.

### Values and the line

```apl
IOTA 5
2 3 RHO IOTA 6
'HELLO'
SIGNUM ¯3
```

A bare number is a scalar and numbers with spaces between them are one vector, so
`42` is a value and `1 2 3` is a value. `'HELLO'` is a character vector, shown
without its quotes. `¯` is the negative sign, written as a raised bar.

| Command | What it does | Example |
| --- | --- | --- |
| `RHO` | Reshape with two arguments, and the shape of its argument with one. | `2 3 RHO 1 2 3 4 5 6` |
| `RESHAPE`, `SHAPE` | The keyword spellings of the two-argument and one-argument forms. | `3 RHO 1` |

A table prints one row per line, so `2 3 RHO IOTA 6` answers `1 2 3` then `4 5 6` on
the next line.

### Scalar functions

Scalar functions are pervasive: they reach into an array, which is why `1 2 3 + 10`
works and `1 2 + 4 5 6` is a length error.

| Command | What it does | Example |
| --- | --- | --- |
| `TIMES` | Multiplication, and `×` is the same function. | `3 TIMES 4` |
| `DIVIDE` | Division. | `6 DIVIDE 2` |
| `POWER` | Raise to a power. | `2 POWER 10` |
| `CEILING` | Round up with one argument; the larger of two with two. | `CEILING 2.2` |
| `FLOOR` | Round down, or the smaller of two. | `FLOOR 2.8` |
| `MAX`, `MIN` | The larger and the smaller of two arguments. | `3 MAX 5` |
| `ABS` | Absolute value. | `ABS -3` |
| `RESIDUE` | The remainder of the left divided by the right. | `3 RESIDUE 10` |
| `SIGNUM` | -1, 0 or 1 by the sign. | `SIGNUM ¯3` |
| `RECIPROCAL` | One over the argument. | `RECIPROCAL 4` |
| `AND`, `OR`, `NAND`, `NOR`, `NOT` | Boolean functions on 0 and 1. | `1 NAND 1` |
| `EQUAL`, `NOTEQUAL`, `LESS`, `LESSEQ`, `GREATER`, `GREATEREQ` | Comparisons, answering 1 or 0 per element. | `1 2 3 EQUAL 1 5 3` |
| `ROLL` | Random integers below the right argument, `?` for short. | `3 ROLL 10` |

### Structural functions

| Command | What it does | Example |
| --- | --- | --- |
| `IOTA`, `INDEXOF` | The first n integers; where the right argument appears in the left. | `IOTA 5` |
| `REVERSE`, `ROTATE` | The argument backwards, or rotated left by n. | `1 ROTATE 1 2 3` |
| `TAKE`, `DROP` | The first n items, padded with zeroes; without them. | `3 TAKE 1 2` |
| `CATENATE` | The two arguments joined. | `1 2 CATENATE 3` |
| `RAVEL` | The argument flattened into a vector. | `RAVEL 2 2 RHO IOTA 4` |
| `TRANSPOSE` | Rows and columns exchanged. | `TRANSPOSE 2 3 RHO IOTA 6` |
| `GRADEUP`, `GRADEDOWN` | The positions that sort the argument, ascending or descending, rather than the sorted data. | `GRADEUP 3 1 2` |
| `MEMBERSHIP` | For each left element, whether it is in the right argument. | `1 MEMBERSHIP 1 2 3` |
| `FORMAT` | A value as the character vector that prints it. | `FORMAT 42` |

### Operators and defined functions

Reduce and scan take the function on their left. Their operator axes, as in `+/[2]`,
are not implemented, so both work along the outermost axis.

| Command | What it does | Example |
| --- | --- | --- |
| `REDUCE`, `SCAN` | Folds the function over a vector; the running fold. `+/` and `+\` are the same operators. | `+REDUCE IOTA 5` |
| `∇` | Opens and closes a function definition. The header names the result and the argument, and the body assigns the result. | see below |

```apl
∇ R ← DOUBLE X
  R ← X TIMES 2
∇
DOUBLE 21
```

Defining a function reports `APL: DOUBLE defined`; calling it displays 42.

### Turtle

| Command | What it does | Example |
| --- | --- | --- |
| `FD`, `FORWARD`, `BK`, `BACK` | Forward and back, drawing if the pen is down. | `FD 100` |
| `RT`, `RIGHT`, `LT`, `LEFT` | Turn right or left in degrees. | `RT 90` |
| `PU`, `PENUP`, `PD`, `PENDOWN` | Lift the pen, put it down. | `PU` |
| `HOME` | Back to the origin, facing up. | `HOME` |
| `TX`, `TY`, `THETA` | The turtle's position and heading, as reporters. | `FD 100` then `TY` |

Those reporter names are deliberate: `X`, `Y` and `H` are ordinary APL variables.

### Differences worth knowing

- **There is no precedence.** A function takes the whole expression on its right, so
  `2 TIMES 3 + 4` is 14, not 10.
- **Lengths must agree.** `1 2 + 4 5 6` reports `APL: length error: 2 and 3`, naming
  both shapes.
- **A name that holds nothing is an error**, not a zero, and so is an unmatched
  bracket: `APL: NOSUCHNAME has no value`, `APL: unmatched (`.
- **Not implemented**: outer product, rank, the composition operators, the system
  functions and branch with labels. Each is a read error naming the character it
  could not read, as in `APL: cannot read '.'`.

## Logo

Whole program, and the only one of the ten whose subject is drawing. A word takes its
inputs after it, `FD 50`, and a bracketed list is a body: `REPEAT 4 [FD 10 RT 90]`.
Procedures are written with `TO` and `END` and are the unit of composition.

### Turtle movement

| Command | What it does | Example |
| --- | --- | --- |
| `FD`, `FORWARD` | Forward by that many steps, drawing. | `FD 50` |
| `BK`, `BACK`, `BACKWARD` | Backwards. | `BK 50` |
| `RT`, `RIGHT` | Turn right in degrees. | `RT 90` |
| `LT`, `LEFT` | Turn left. | `LT 90` |
| `UP`, `UPWARD`, `DOWN`, `DOWNWARD` | Move up or down the screen without turning. | `UP 40` |
| `SETX`, `SETY`, `SETXY`, `SETPOS`, `SETPOSITION` | Move to an x, a y or a whole point, given as numbers or as a list. | `SETXY 10 10` |
| `SETH`, `SETHEADING` | Face an absolute heading, 0 being north. | `SETH 90` |
| `HOME` | To the origin, facing north. | `HOME` |
| `CS`, `CLEARSCREEN` | Clear and go home. | `CS` |
| `WINDOW`, `WRAP`, `FENCE` | Choose what happens at the edge, and report it: `Screen mode: WINDOW`. | `WRAP` |
| `ARC` | An arc of that angle and radius, leaving the turtle at its end. | `ARC 90 40` |
| `BEZIER` | A cubic curve from where the turtle stands, through two control points to an end point, leaving it facing along the curve. | `BEZIER 0 60 60 60 60 0` |
| `DOT` | A dot of that diameter at the turtle. | `DOT 6` |
| `STAMP`, `STAMPCLEAN` | A copy of the turtle's shape inked where it stands, and removing the stamps. | `STAMP` |
| `FILLED` | Runs a body with the area it encloses filled in that colour. | `FILLED "RED [FD 20 RT 90 FD 20 RT 90 FD 20]` |
| `LABEL` | Writes a word at the turtle. | `LABEL "HI` |
| `SETLABELHEIGHT`, `TEXTSIZE` | The height of that text. | `SETLABELHEIGHT 12` |
| `WAIT` | Pauses for that many seconds. | `WAIT 1` |

### Pen and colour

| Command | What it does | Example |
| --- | --- | --- |
| `PU`, `PENUP` | Lift the pen, so the next move leaves no ink. | `PU` |
| `PD`, `PENDOWN` | Put it back down. | `PD` |
| `SETPC`, `SETPENCOLOR`, `SETCOLOR` | The pen colour, by name in quotes or by palette slot. An unknown name is ignored. | `SETPENCOLOR "RED` |
| `SETBG`, `SETBGCOLOR` | The background colour. | `SETBG "BLUE` |
| `PENSIZE`, `SETPENSIZE`, `PENWIDTH`, `SETPENWIDTH`, `SETPW` | The width of the stroke. | `PENSIZE 3` |
| `PENSTYLE` | 0 solid, 1 dashed, 2 dotted, 3 dash-dot, 4 invisible. | `PENSTYLE 1` |
| `PENERASE`, `PE` | Draw in the background colour instead of the pen's. | `PENERASE` |
| `PENPAINT` | Back to painting. | `PENPAINT` |
| `PENREVERSE` | Draw in whatever is under the pen, inverted. | `PENREVERSE` |
| `PENMODE`, `PPT`, `PX` | The pen's mode, its paint colour, and the colour where it stands. | `PX` |
| `CLEAN`, `CLEAR` | Clears the ink, leaving the turtle where it is or sending it home. | `CLEAN` |
| `HIDETURTLE`, `HT` | Hides the turtle. | `HT` |
| `SHOWTURTLE`, `ST` | Shows it again. | `ST` |

### Procedures and control

```logo
TO SQUARE :S
REPEAT 4 [FD :S RT 90]
END
SQUARE 20
```

| Command | What it does | Example |
| --- | --- | --- |
| `TO` ... `END` | Defines a procedure, whose inputs are the `:name` words in the header. | as above |
| a procedure call | Any word that is not a primitive runs the procedure of that name. | `SQUARE 20` |
| `REPEAT` | Runs a body a number of times; `REPCOUNT` counts them. | `REPEAT 4 [FD 10 RT 90]` |
| `IF` | Runs a body when the test holds. | `IF 1 = 1 [FD 10]` |
| `IFELSE` | One body or the other. | `IFELSE 1 = 1 [PRINT "YES"] [PRINT "NO"]` |
| `WHILE` | Repeats while the first body holds. | `WHILE [:I < 3] [MAKE "I :I + 1]` |
| `FOREVER` | Repeats until the body stops the run. | `FOREVER [STOP]` |
| `FOREACH` | Runs a body once per item of a list. | `FOREACH [1 2 3] [FD 10]` |
| `REPCOUNT` | The pass number of the enclosing loop, from 1. | `REPEAT 2 [SHOW REPCOUNT]` |
| `LOCAL` | Declares a name that belongs to the procedure body: the caller's value for it goes back when the procedure returns. `LOCAL "X` and `LOCAL :X` both name `X`. | `LOCAL :X` |
| `OUTPUT`, `OP` | The value a procedure reports to its caller. | `OUTPUT :N * 2` |
| `STOP`, `BYE` | Ends the procedure, or the run. | `BYE` |
| `REM`, `;` | A comment, on its own or after a statement: the rest of the line is ignored. | `FD 10 ; and stop here` |

### A name holds one value

`MAKE` and a procedure's input both read their value the same way, so the two cannot
disagree about what a value is:

| Written | The value |
| --- | --- |
| `42`, `:N + 1` | a number |
| `"red`, `"red"` | the word `red`; the closing quote is optional |
| `"hello world"` | the text, spaces and all |
| `[1 2 3]` | a list of three items |
| `:count` | whatever `count` holds, of whatever kind |

A name holds one value, so `MAKE "X 5` followed by `MAKE "X "hello` leaves the word
and not the number beside it.

### What a procedure's inputs are

`TO SQUARE :S` declares one input. A call reads one value per input, in order, and
binds it the way `MAKE` binds a value, so `SQUARE 20`, `SQUARE :N + 1`, `SQUARE
"green` and `SQUARE [1 2 3]` each bind what they look like -- a number, a word, a
list. Fewer values than inputs reports `Procedure SQUARE requires 1 inputs`, and
words after the last input are ignored.

An input shadows whatever the caller held under that name -- a number, a word or a
list -- and the caller's value is back as soon as the procedure returns. `LOCAL` adds
another name to the same frame, so it too goes back on return.

The body stops at `STOP`, at `OUTPUT` (which is the value the caller sees) or at
`BYE`. An error ends the body and is reported, and the caller carries on. Recursion
is allowed 128 calls deep.

### Data and reporters

```logo
MAKE "X 5
PRINT :X
SHOW FIRST [1 2 3]
```

| Command | What it does | Example |
| --- | --- | --- |
| `MAKE` | Binds a name to one value, read as the table above describes: a number, a word, a text or a list. | `MAKE "X 5` |
| `THING` | The value a name holds, quoted: `THING "X`. | `SHOW THING "X` |
| `PRINT` | Writes a value. Text it cannot evaluate is written back as it stands. | `PRINT "HELLO` |
| `SHOW` | Writes a value and ends the line. | `SHOW 42` |
| `TYPE` | Writes a value. The console is a list of lines rather than a stream, so it ends the line as `PRINT` does. | `TYPE "A` |
| `FIRST`, `LAST` | The first or last item. | `SHOW FIRST [1 2 3]` |
| `BUTFIRST`, `BUTLAST` | The list without its first or last item. | `SHOW BUTFIRST [1 2 3]` |
| `ITEM` | The nth item, counting from 1. | `SHOW ITEM 2 [1 2 3]` |
| `COUNT` | The number of items in a list, or of characters in a word. | `COUNT [1 2 3]` |
| `LIST` | A list of its arguments. | `SHOW LIST 1 2` |
| `SENTENCE` | The arguments as one flat list. | `SHOW SENTENCE 1 2` |
| `SUM`, `DIFFERENCE`, `PRODUCT`, `QUOTIENT` | Arithmetic, also spelled `+ - * /`. | `SHOW SUM 1 2` |
| `RANDOM` | An integer from 0 up to one less than the argument. | `SHOW RANDOM 10` |
| `MAP`, `FILTER` | A new list of the template's answers, or the items whose template answer holds: `?` is the item. | `SHOW MAP [? * 2] [1 2]` |
| `REDUCE` | Folds a list with a template of two: `?1` is the running value and `?2` the next item. `REDUCE "SUM [1 2 3]` names a procedure instead. | `REDUCE [SUM ?1 ?2] [1 2 3]` |
| `POS` | Where the turtle stands, as a list of two. | `SHOW POS` |
| `XCOR`, `YCOR`, `HEADING` | One number of that pose: across, up, and the heading in degrees clockwise from north. | `SHOW HEADING` |

A list prints with quoted words, as a Python list would: `SHOW LIST 1 2` gives
`[1.0, 2.0]`, `SHOW BUTFIRST [1 2 3]` gives `['2', '3']`.

### Mathematics

```logo
PRINT ABS 5
PRINT SQRT 16
PRINT MAX(3, 7)
```

| Command | What it does | Example |
| --- | --- | --- |
| `ABS` | The absolute value. | `ABS(-5)` |
| `SGN` | -1, 0 or 1 according to the sign. | `SGN(-3)` |
| `INT`, `FLOOR` | The largest integer not greater than the argument, so a negative rounds down. | `INT(3.7)` |
| `CEIL` | The smallest integer not less than it. | `CEIL(1.2)` |
| `FIX` | The integer part, discarding the fraction. | `FIX(-3.7)` |
| `CINT`, `ROUND` | The nearest whole number, with a half going to the even one. | `ROUND(2.5)` |
| `SQRT`, `SQR` | The square root. | `SQRT 16` |
| `EXP`, `LOG`, `LOG10`, `LOG2` | e to the power; the natural, base-10 and base-2 logarithm. | `LOG 1` |
| `SIN`, `COS`, `TAN` | Trigonometry in radians, and `ASIN`, `ACOS`, `ATAN` the inverses. | `SIN(0)` |
| `SINH`, `COSH`, `TANH` | The hyperbolic forms. | `TANH(0)` |
| `MAX`, `MIN`, `POW` | The larger, the smaller, and one raised to the other. | `MAX(3, 7)` |

A function takes the one number written after it: `SQRT 16 + 9` is 13, because the
root is taken of 16 and the 9 is added afterwards. Anything else -- a negative
number, a longer expression, or a second argument -- needs the bracket: `ABS(-5)` is
5, `SQRT(16 + 9)` is 5, and `MAX(3, 7)` is 7. A number prints with six significant
digits: `1 / 3` is `0.333333`.

### Three dimensions

| Command | What it does | Example |
| --- | --- | --- |
| `ENABLE3D` | Turns on the three-dimensional turtle, and reports it. | `ENABLE3D` |
| `DISABLE3D`, `2DON` | Back to two dimensions. | `DISABLE3D` |
| `3DON`, `3DOFF` | The same two states again. | `3DON` |
| `PITCH` | Tips the turtle's nose up or down. | `PITCH 30` |
| `TILTUP`, `TILTDOWN` | The same by a smaller step. | `TILTUP 10` |
| `TILTROLL`, `ROLL` | Rolls about the direction of travel. | `ROLL 10` |
| `YAW` | Turns about the vertical. | `YAW 45` |

### Differences worth knowing

- **A reporter is run, whatever else the word may name.** `PRINT FIRST [1 2 3]`
  writes 1 and `PRINT COUNT [1 2 3]` writes 3: the printer knows `SUM`,
  `DIFFERENCE`, `PRODUCT`, `QUOTIENT`, `RANDOM`, `WORD`, `LIST`, `SENTENCE`, `FIRST`,
  `LAST`, `BUTFIRST`, `BUTLAST`, `ITEM`, `THING`, `COUNT`, `REPCOUNT`, `MAP`,
  `FILTER` and `REDUCE`. Text it cannot evaluate is written back as it stands.
- **A template is a value first and a command list second.** `MAP [? * 2]` and
  `FILTER [? > 7]` read the template as an expression with `?` bound to the item,
  and `REDUCE` reads `?1` and `?2`. A template that is really commands -- `FOREACH
  [1 2] [FD 10]` -- still runs as commands, and `FOREACH` does not bind `?`.
- **A closing quote in a word form is silent.** `PRINT "HI` and `PRINT "HI"` both
  write `HI`, so a bracketed body may be written `[PRINT "HI"]`, and `MAKE "X "HI"`
  binds `HI`.
- **A word argument arrives upper-cased.** Words are case-insensitive, so `show` is
  `SHOW`; a word written as an argument is read upper-cased, which is why
  `SHOW "hello` writes `HELLO` while `PRINT "hello` writes `hello` -- the printer
  reads the text of the line, and `MAKE` reads it the same way. A word handed to a
  command loses the case it was written in: `SHOW LIST "abc "def` gives
  `['ABC', 'DEF']`.
- **`TYPE` ends the line.** The console is a list of lines rather than a stream, so
  `TYPE "A` followed by `PRINT "B` shows two lines where a Logo stream would show
  `AB`.
- **Loops that overrun stop silently.** `WHILE`, `UNTIL` and `FOREVER` end after
  10,000 passes with no message, `REPEAT` has no such cap, and recursion stops at 128
  deep with `Procedure R exceeded maximum recursion depth (128)`.

## PILOT

Line by line, and the line is a teaching unit rather than a statement: a letter, a
colon, and the rest of the line is that unit's text. A lesson asks a question, reads
an answer and decides whether the answer matched. Every unit also has a long form
(`PRINT:`, `ACCEPT:`, `MATCH:`, `JUMP:`, `USE:`, `STOP:`, `PAUSE:`, `GRAPHIC:`,
`FILE:`, `DELAY:`, `HINT:`, `COMPUTE:`, `NO:`), and both spellings are accepted.

### Text and answers

| Command | What it does | Example |
| --- | --- | --- |
| `T:` | Writes the text, with `$NAME` replaced by a variable's value. | `T: Hello there` |
| `A:` | Reads an answer into `ANSWER$`. | `A:` |
| `A:$NAME` | Reads an answer into `NAME$`, showing `? ` as the prompt. | `A:$NAME` |
| `C:` | Computes; a name ending `$` stores text and any other stores a number. | `C:X = 5` |
| `H:` | A hint, written as `Hint: ...`. | `H: Try the other one` |
| `REMARK`, `R:` | A remark, ignored. | `REMARK a note` |
| `D:`, `P:` | Waits that many seconds, capped at ten; waits for Enter. | `D: 0` |

```pilot
T: What is your name?
A:$NAME
T: Hello $NAME
```

### Matching and branching

| Command | What it does | Example |
| --- | --- | --- |
| `M:` | Compares the last answer with comma-separated alternatives, ignoring case. `*` and `?` are wildcards, and plain text matches anywhere in the answer. | `M: PARIS` |
| `$LEFT`, `$MATCH`, `$RIGHT` | Set by a match: the answer before it, the part that matched, and the rest. | `T: [$MATCH]` |
| `TY:`, `TN:` | Writes the text when the last match succeeded, or when it failed. | `TY: Correct` |
| `Y:`, `N:` | Jumps when the last match succeeded; writes its text when it failed. | `Y: HIT` |
| `B:` | Jumps when the expression before the `=` is non-zero. | `B:X=*HIT` |
| `J:` and `L:` | Jumps to a label; declares one that a jump can reach, as does `*NAME` on its own line. | `J: SKIP` |
| `E:` | Ends the program, or returns from a subroutine when one is running. | `E:` |
| `U:`, `S:`, `TU` | Calls a labelled block as a subroutine, nested up to 128 deep. | `U: *SUB` |

So `T: Q`, `A:`, `M: PARIS`, `TY: Correct`, `TN: Wrong` prints `Correct` for an
answer of `PARIS` and `Wrong` for anything else. A jump to a label that does not exist
reports `Label 'NOWHERE' not found` and the lesson continues at the next line, so a
mistyped label costs a unit rather than the run.

### Graphics and files

| Command | What it does | Example |
| --- | --- | --- |
| `G:` | Runs one turtle word, so a drawing is a run of units. | `G: FD 50` |
| `G: TEXT` | Writes a label at the turtle. | `G: TEXT A label` |
| `G: FILL` | Fills the area the path encloses, in the pen colour. | `G: FILL` |
| `F: OPEN` | Opens a file for reading, or for `R`, `W` or `A` when a mode is written. | `F: OPEN notes.txt W` |
| `F: CLOSE` | Closes it. | `F: CLOSE notes.txt` |
| `F: READ` | Reads a line into a variable. | `F: READ notes.txt LINE$` |
| `F: WRITE` | Writes the rest of the unit as a line, so the text may be several words, with `$name` and `#name` replaced as they are in a `T:` unit. | `F: WRITE notes.txt total #X words` |
| `LINK` | Chains to another lesson: that file replaces this one and everything computed so far stays in scope. The lesson that linked does not carry on afterwards. | `LINK lesson.pilot` |

### Differences worth knowing

- **A plain `A:` leaves the answer in `ANSWER$`**; `A:$NAME` puts it in `NAME$`. The
  prompt is `? ` unless the unit carries text of its own.
- **`M:` matches loosely**: case is ignored, commas separate alternatives, `PAR*S` is
  a pattern, and `RI` matches `PARIS` because plain text only has to appear in the
  answer.
- **`F:` checks the mode, not the word count**: `F: OPEN notes.txt X` reports
  `F: OPEN requires mode R|W|A, got: X`, while `F: OPEN notes.txt` reads.
- **`LINK` chains rather than calls.** The second lesson starts at its first unit
  with the first lesson's variables, and a name it cannot open is reported as
  `LINK: cannot open 'name'`.
- **A headless run has no keyboard**, so the command harness answers nothing at `A:`
  and every `TN:` path is taken.

## FOCAL

Line by line, in DEC's teaching language of 1969: a handful of commands, one number
type, and line labels of the form `group.line` so that five lines can be named as a
unit. Every command has a short letter and a full name, and both are accepted.

### Labels and the commands

A label is `NN.NN`, each part up to two digits. Leading zeroes do not matter, so
`GOTO 1.40` reaches the line written `01.40`. The second part is a whole number
rather than a decimal one, so `1.3` names group 1 line 3 and not the line written
`01.30`. A reference to a label that does not exist reports
`FOCAL: no line labelled 9.9`.

| Command | What it does | Example |
| --- | --- | --- |
| `T`, `TYPE` | Writes text, formats and expressions. | `01.10 T "HELLO"` |
| `S`, `SET` | Assigns, and may assign several names at once. | `01.10 S A=1,B=2` |
| `A`, `ASK` | Prompts and reads one value. | `01.10 ASK "How many? " N` |
| `I`, `IF` | Three-way branch on the sign of an expression. | `01.20 IF (X) 1.40,1.30,1.50` |
| `F`, `FOR` | Loops over the rest of the line. | `01.10 FOR I=1,3;T I` |
| `D`, `DO` | Runs a group from its first line. | `01.10 DO 2.10` |
| `R`, `RETURN` | Leaves the innermost `DO` group. | `02.20 RETURN` |
| `G`, `GOTO` | Jumps to a group. | `01.10 GOTO 1.30` |
| `C`, `COMMENT` | The rest of the line is text. | `01.10 C ignored` |
| `E`, `ERASE` | Forgets variables: `E ALL`, or `E X,Y`. | `01.20 E ALL` |
| `Q`, `QUIT` | Stops the program. | `01.30 Q` |
| `;` | Separates two statements on a line. | `01.10 S A=1;S B=2` |

### TYPE, formats and functions

`%n` sets the number of decimal places and holds until the next `%n`; the default is
two, and a value is rounded to it. `!` suppresses the newline within one `TYPE`, so
`T "A"!"B"` writes `AB`.

| Command | What it does | Example |
| --- | --- | --- |
| `%n` | That many decimal places for the rest of the session. | `T %0,"R=",7/2` |
| `!` | No newline at that point in the statement. | `T "A"!"B"` |
| `FPI`, `FRAN` | Pi and a random number, written without parentheses; `FRAN(1)` seeds it. | `T FPI` |
| `FABS`, `FSQT`, `FEXP`, `FLOG` | Absolute value, square root, e to the power, natural log. | `T FSQT(16)` |
| `FSIN`, `FCOS`, `FATN` | Trigonometry in radians. | `T FSIN(0)` |
| `FITR`, `FSGN` | Truncate to an integer; the sign as -1, 0 or 1. | `T FITR(3.9)` |

### Turtle

| Command | What it does | Example |
| --- | --- | --- |
| `FORWARD`, `FD`, `BACK`, `BK` | Forward and back, drawing. | `01.10 FORWARD 100` |
| `RIGHT`, `RT`, `LEFT`, `LT` | Turn. | `01.10 RIGHT 90` |
| `PENUP`, `PU`, `PENDOWN`, `PD` | The pen. | `01.10 PENUP` |
| `HOME` | To the origin. | `01.10 HOME` |

### Differences worth knowing

- **`IF` has no relational operators.** `IF (X) a,b,c` goes to `a` when X is
  negative, `b` when it is zero and `c` when it is positive, so three answers need
  the `GOTO` lines a modern language would not.
- **Undefined variables are 0**, so a mistyped name prints `0.00` rather than
  reporting itself.
- **`FOR` keeps the last value it used**: after `FOR I=1,3` the variable holds 3.
- **An unrecognised command is an error, not a comment** (`FOCAL: unknown command
  'TIPE'`), a `DO` group runs on past its end unless a `RETURN` is there, and nesting
  stops at 64 deep.

## Scheme

Whole program. A small Lisp: the operator comes first, everything is a list, and a
procedure is a value. The evaluator is an explicit-stack machine, so a recursive
definition a million calls deep is a loop rather than a stack overflow.

| Command | What it does | Example |
| --- | --- | --- |
| `display` | Writes a value with no newline. | `(display "HELLO")` |
| `newline` | Ends the line. | `(newline)` |
| `define` | Binds a name, or names a procedure with a parameter list. | `(define pi 3.14159)` |
| `lambda` | A procedure as a value. | `(lambda (x) (* x x))` |
| `if` | Two-branch conditional. | `(if (= n 0) 1 (* n (fact (- n 1))))` |
| `cond` | Multi-branch conditional, with `else` as the last clause. | see below |
| `let` | Local bindings for the body. | `(let ((x 3) (y 4)) (+ x y))` |
| `car` | The first element of a list. | `(car (list 1 2 3))` |
| `list` | A list of its arguments. | `(list 1 2 3)` |
| `map` | Applies a procedure to each element. | `(map (lambda (x) (* x x)) (list 1 2 3 4))` |
| `filter` | The elements the predicate accepts. | `(filter odd? (list 1 2 3 4 5))` |
| `apply` | Calls a procedure with a list as its arguments. | `(apply + (list 10 20 30))` |
| a closure | A `lambda` that captured the bindings where it was written. | see below |
| `forward`, `fd` | Turtle forward, drawing. | `(forward 100)` |
| `right`, `left`, `back`, `bk` | Turning and reversing. | `(right 90)` |
| `goto`, `setheading`, `home` | Absolute position and heading. | `(goto 10 20)` |
| `color`, `pensize`, `penup`, `pendown` | The pen: three numbers from 0 to 255, a width, up and down. | `(color 255 0 0)` |
| `circle`, `clear-canvas`, `show-turtle`, `hide-turtle` | A circle of that radius; the canvas; the turtle. | `(circle 20)` |

```scheme
(define (sign n)
  (cond ((> n 0) "positive")
        ((< n 0) "negative")
        (else "zero")))
(display (sign -3))

(define (make-adder n) (lambda (x) (+ x n)))
(define add10 (make-adder 10))
(display (add10 5))
```

The first prints `negative` and the second prints 15; `display` ends no line, so the
two run together.

```scheme
(define (square n) (when (> n 0) (forward 50) (right 90) (square (- n 1))))
(square 4)
```

### Differences worth knowing

- **Numbers are inexact unless they are integers.** `(display pi)` writes `3.14159`,
  `(display (/ 1 3))` writes `0.3333333333333333`, and `(display 1.0)` writes 1.
- **A call with the wrong number of arguments is an error**: `(car)` reports
  `car: expected 1 argument(s), got 0`, and an unknown name reports
  `unbound variable: no-such`.
- **There are no exact rationals.** `1/2` is read as a name and reported unbound.
- **The clock is the only budget**: no step limit, no recursion limit, and a program
  that never stops is cut off after ten seconds.

## COMAL

Line by line: structured BASIC, with blocks that end in a word, `:=` for assignment
and procedures that are called rather than jumped to. Lines may be numbered or not.

| Command | What it does | Example |
| --- | --- | --- |
| `PRINT` | Writes its items; a comma between them writes a space, a semicolon none. | `PRINT "Hello, World!"` |
| assignment with `:=` | Stores a value; a `$` on the name makes it text. | `COUNT := 5` |
| `IF ... THEN ... ELSE ... ENDIF` | A block conditional. | `IF SCORE >= 90 THEN` |
| `ELIF` | An extra branch in that block. | `ELIF SCORE >= 80 THEN` |
| `FOR ... TO` and `NEXT` | Counts inclusive, and leaves the first value past the limit. | `FOR I := 1 TO 3` |
| `WHILE` and `ENDWHILE`, `REPEAT` and `UNTIL` | Repeats while the test holds; repeats until it holds, testing after the body. | `WHILE N < 3` |
| `PROC` and `ENDPROC` | A named block with parameters, called for its effect. | `PROC GREET(NAME$)` |
| `CALL` | Calls a `PROC` or a `FUNC`. | `CALL GREET("World")` |
| `FUNC` and `ENDFUNC` and `RETURN` | A named block that answers a value, usable inside an expression. | `PRINT DOUBLE(21)` |
| `LOCAL` | A name that belongs to the function body, and is put back as it was when the body returns. | `LOCAL T` |
| `DIM` | Declares an array, indexed from 0. | `DIM SCORES(5)` |
| `DATA` and `READ` | Constants, and reading them into names; one statement each, so a data line and a read are two lines. | `DATA 4, 5` |
| `MOVE` | Turtle forward, drawing. | `MOVE 100` |
| `TURN` | Turn right in degrees. | `TURN 90` |
| `PENUP`, `PENDOWN`, `PLOT` | Lift the pen, put it back, draw a point. | `PLOT` |

```comal
10 FUNC DOUBLE(N)
20 RETURN N * 2
30 ENDFUNC
40 PRINT DOUBLE(21)
```

### Differences worth knowing

- **`CALL` and an expression are different ways into a function.** `PRINT DOUBLE(21)`
  uses the answer; `CALL DOUBLE(21)` runs the body and leaves it in `RESULT`.
- **`LOCAL` is scoped to the call**: a name the caller never had reads 0 afterwards,
  and the caller's own value is put back when the body returns.
- **`FOR` leaves the first value past the limit**, so `PRINT I` after `FOR I := 1 TO
  3` writes 4.
- **An unknown statement is reported**, not skipped: `unrecognised statement: NOSUCH`.

## REXX

Line by line, and typeless: every value is a string, no variable needs declaring, and
concatenation is what happens when two values are written next to each other. A name
that has never been assigned reads as its own name in capitals, which is why `SAY n`
prints `N`.

| Command | What it does | Example |
| --- | --- | --- |
| `SAY` | Writes its arguments, abutting them. | `SAY 'Hello' 'World'` |
| `=` | Assignment. | `name = 'Ann'` |
| `\|\|` | Concatenation with nothing between. | `SAY 'Hello' \|\| ' ' \|\| 'World'` |
| `IF ... THEN` | One statement, or a `DO` block, when the test holds. | `IF score > 50 THEN SAY 'pass'` |
| `DO` ... `END` | A block; `ELSE DO` is its other half. | `IF score >= 90 THEN DO` |
| `DO i = a TO b BY n` | A counted loop, `BY` optional. | `DO n = 10 TO 1 BY -3` |
| `DO WHILE` | A loop with its test first. | `DO WHILE count < 3` |
| `DO ... OVER` | A loop over the words of a string. | `DO word OVER 'a b c'` |
| `SELECT`, `WHEN`, `OTHERWISE` | A multi-way choice. | `SELECT` |
| `LEAVE`, `ITERATE` | Leave the loop; skip to its next pass. | `LEAVE` |
| `PARSE VAR` | Splits a string's words into names; the last name takes the rest of the line. | `PARSE VAR line first rest` |
| `PULL` | Reads a line from the user, splitting it into its names the same way. | `PULL verb rest` |
| `PARSE PULL` | Reads a line into names, one word each and the rest into the last. | `PARSE PULL first rest` |
| `CALL` and `RESULT` | Calls a label as a subroutine and holds its answer. | `CALL DOUBLE 5` |
| `RETURN` and `EXIT` | Ends a subroutine with a value; ends the program. | `RETURN ARG(1) * 2` |
| `ARG` | A subroutine's nth argument. | `ARG(1)` |
| `LENGTH`, `SUBSTR`, `WORD`, `WORDS` | Length; a substring; the nth word; how many words. | `SUBSTR('HELLO', 2, 3)` |
| `UPPER`, `LOWER`, `STRIP`, `SPACE` | Case; trim; pad single spaces out to a count. | `SPACE('a b c', 3)` |
| `LEFT`, `RIGHT`, `COPIES` | Pad to a width; repeat. | `LEFT('ab', 5)` |
| `INDEX`, `SIGN`, `TRUNC`, `DATATYPE` | Position of a substring; the sign; the integer part; `NUM`, `CHAR` or neither. | `INDEX('hello', 'll')` |
| turtle words | `FORWARD`/`MOVE`, `BACK`, `RIGHT`/`TURN`, `LEFT`, `PENUP`, `PENDOWN`, `HOME`. | `FORWARD 10` |

`SPACE('a b c', 3)` writes `a   b   c`: the count is the spaces between words, not a
width, which is the opposite of `LEFT` and `RIGHT`.

### Differences worth knowing

- **Abuttal is concatenation.** `SAY 'Hi' name` writes `HiAnn`; `SAY 'a' 'b'` is
  `ab`.
- **An unassigned variable is its own name.** `SAY count` writes `COUNT`, and
  `DO WHILE count < 3` compares the string `COUNT` with 3, which is false, so the
  loop never runs.
- **`PARSE` gives the last name the rest.** `PARSE VAR line first rest` puts the
  first word in `first` and everything after it in `rest`, spaces and all;
  `PARSE PULL verb rest` splits a line the same way, so `ADD 2 3` is `verb = ADD`
  and `rest = 2 3`.
- **`LEFT` and `RIGHT` pad to a width**, and no newline is implied.

## Smalltalk

Whole program, and the one object-oriented language of the ten. There is no syntax
for control flow: `ifTrue:`, `whileTrue:` and `to:do:` are messages. A file defines
classes and methods and then runs top-level code, so the whole source is read before
anything runs. A definition is a header followed by indented lines, and it ends at
the next line that begins in column 0.

| Command | What it does | Example |
| --- | --- | --- |
| `subclass:instanceVariableNames:` | Defines a class; this is a keyword message to `Object`, not special syntax. | see below |
| `new` | An instance, whose `initialize` is sent if the class has one. | `Counter new` |
| `>>` | Adds a method to a class. | `Counter >> bump` |
| `Transcript show:` | Writes a value to the transcript. | `Transcript show: 'hello'` |
| `cr` | Ends the transcript line. | `Transcript show: 'a'; cr` |
| `;` | A cascade: the next message goes to the same receiver, not to the result. | `Transcript show: 'a'; show: 'b'; cr` |
| `super` | Sends to the class above the one that defines the method. | `^'kid and ', super talk` |
| `at:` | An element of a collection, counting from 1. | `c at: 2` |
| `first`, `last` | Its ends. | `c first` |
| `add:` | Appends to a collection. | `c add: 'a'` |
| `Turtle new` | A turtle on the canvas. | `t := Turtle new` |
| `forward:`, `back:` | Move it, drawing. | `t forward: 100` |
| `right:`, `left:` | Turn it. | `t right: 90` |
| `penUp`, `penDown` | Lift the pen, put it back. | `t penUp` |
| `home`, `clear` | To the origin; clear the canvas. | `t home` |

```smalltalk
Object subclass: #Counter
  instanceVariableNames: 'count'

Counter >> initialize
  count := 0

Counter >> bump
  count := count + 1

Counter >> value
  ^count

| c |
c := Counter new.
c bump.
c bump.
c value
```

That program writes `Counter`, `Counter >> initialize`, `Counter >> bump`,
`Counter >> value` and then 2: every definition echoes its own header, and the last
expression of the file is the value displayed. Without the `initialize` method the
instance variable is nil and the first `bump` fails, which is how `new` sends it.

### Differences worth knowing

- **Precedence is unary, then binary, then keyword.** `2 + 3 * 4` is 20, and
  `1 max: 2 + 3` is 5.
- **`^` returns from the method that made the block**, not from the block.
- **A name that is not a variable is not created silently**, and a message a
  receiver does not understand is an error: `unknown variable or global noSuchThing`,
  `3 does not understand foo`, `index 9 is out of range for a collection of 3`.
- **A runaway loop is stopped by its own guard**: `[true] whileTrue: [...]` reports
  `the loop ran for too long` after five million passes.

## Forth

Whole program, and no syntax in the sense the others have: a program is
whitespace-separated words, and each takes what it needs from the stack. `2 3 +` is
"push two, push three, add", so arguments come before the word and `FD 100` is an
error where `100 FD` moves the turtle. `: NAME ... ;` adds a word, and from then on it
is indistinguishable from a built-in one.

### The stack

| Command | What it does | Example |
| --- | --- | --- |
| `DUP`, `OVER` | Copy the top, or the second, to the top. | `7 DUP * .` |
| `DROP`, `NIP` | Discard the top, or the second. | `1 2 NIP .` |
| `SWAP`, `ROT`, `-ROT` | Exchange the top two; bring the third up; the other way. | `1 2 3 ROT . . .` |
| `TUCK` | Copy the top under the second. | `1 2 TUCK . . .` |
| `?DUP` | Copy the top only if it is not zero. | `5 ?DUP + .` |
| `DEPTH` | How many items are on the stack. | `1 2 3 DEPTH .` |
| `2DUP`, `2DROP`, `2SWAP` | The same three for two items at a time. | `1 2 2DUP + . . .` |
| `PICK` | Copy the nth item down to the top. | `10 20 30 2 PICK .` |
| `ROLL` | Move the nth item up to the top. | `10 20 30 2 ROLL .` |

### Arithmetic, comparison and bits

| Command | What it does | Example |
| --- | --- | --- |
| `+`, `-`, `*`, `/`, `MOD` | Integer arithmetic. `/` truncates towards zero, and `MOD` takes the sign of the dividend. | `7 3 MOD .` |
| `/MOD` | Both at once, remainder under quotient. | `7 3 /MOD . .` |
| `NEGATE`, `ABS`, `MIN`, `MAX`, `1+`, `1-` | Sign, absolute value, the smaller and the larger, and add or subtract one. | `-9 ABS .` |
| `=`, `<>`, `<`, `>`, `<=`, `>=`, `0=`, `0<`, `0>` | Comparisons, answering the flag -1 for true and 0 for false. | `5 5 = .` |
| `AND`, `OR`, `XOR`, `INVERT`, `LSHIFT`, `RSHIFT` | Bitwise logic and shifts. | `12 10 AND .` |

A condition is any non-zero value, and a true comparison is all bits set: `-1`.

### Output and memory

| Command | What it does | Example |
| --- | --- | --- |
| `.` | Prints the top and a space. | `42 .` |
| `.S` | The whole stack as `<depth> item item`, without changing it. | `1 2 .S` |
| `." text"` | Prints text; the space after the quote is the delimiter. | `." hello"` |
| `EMIT`, `CR` | Prints a character from its code; ends the line. | `65 EMIT` |
| `SPACE`, `SPACES`, `WORDS` | One space, n of them; the dictionary. | `3 SPACES` |
| `VARIABLE`, `CONSTANT` | A word that pushes a variable's address; a word that pushes one value. | `99 CONSTANT NINETY` |
| `!`, `@`, `C!`, `C@`, `+!` | Store at an address, fetch it back, the byte forms, and add to what a variable holds. | `7 V ! V @ .` |
| `POKE`, `PEEK` | Write and read the program's own 64 KiB map, addressed 0 to 65535. | `65 32768 POKE 32768 PEEK .` |

### Definitions and control

| Command | What it does | Example |
| --- | --- | --- |
| `:` ... `;` | A colon definition: a new word. | `: SQUARE DUP * ;` |
| `IF`, `ELSE`, `THEN` | Run a body when the flag on the stack is true. | `-1 IF ." yes" THEN` |
| `BEGIN`, `UNTIL` | Repeat until the flag is true. | `BEGIN DUP . 1+ DUP 3 = UNTIL` |
| `BEGIN`, `WHILE`, `REPEAT` | Repeat while the flag is true. | `BEGIN DUP 3 < WHILE DUP . 1+ REPEAT` |
| `DO`, `LOOP`, `+LOOP` | Run with `I` from the start up to one less than the limit, by one or by the top of the stack. | `3 0 DO I . LOOP` |
| `I`, `J` | The innermost and the next loop counter out. | `2 0 DO 2 0 DO J . LOOP LOOP` |
| `LEAVE`, `EXIT` | Leave the innermost `DO` loop; return from the word. | `2 = IF LEAVE THEN` |
| `RECURSE` | The word being defined, called again. | `: FACT DUP 1 > IF DUP 1- RECURSE * THEN ;` |
| `IMMEDIATE` | Accepted and does nothing: there are no user immediate words. | `: F 1 ; IMMEDIATE` |
| `BYE` | Ends the program where it stands. | `BYE` |
| `(` ... `)`, `\` | Comments, to the closing bracket or to the end of the line. | `( a note )` |

### Turtle

| Command | What it does | Example |
| --- | --- | --- |
| `FD`, `FORWARD` | Forward, drawing. | `100 FD` |
| `BK`, `BACK` | Backwards. | `50 BK` |
| `RT`, `RIGHT`, `LT`, `LEFT` | Turn. | `90 RT 100 FD` |
| `PU`, `PENUP`, `PD`, `PENDOWN` | The pen. | `PD PU` |
| `HOME`, `CS`, `CLEARSCREEN` | To the origin; clear the canvas. | `CS` |
| `SETPC`, `SETPENCOLOR`, `SETPW` | Pen colour as a palette slot, and pen width. | `3 SETPW 100 FD` |
| `ST`, `SHOWTURTLE`, `HT`, `HIDETURTLE` | The turtle's visibility. | `HT` |

### Differences worth knowing

- **Arguments come first and words act immediately.** `FD 100` fails where `100 FD`
  draws.
- **Division is integer division truncated towards zero**, `MOD` takes the
  dividend's sign (`-7 2 /` is -3, `7 -2 MOD` is 1), and there is no floating point.
- **Stack underflow is an error naming the word**: `Forth, line 1: .: stack
  underflow`; an unknown word reports `undefined word: NOSUCHWORD`, and a definition
  with no `;` reports `the definition of F was never closed`.
- **The hard limit is instructions, not time**: four million, and a definition that
  recurses endlessly reports `F: too deep (endless recursion?)`.

## Limits and budgets

Every language stops. What stops it differs, and the difference matters: a program cut
off by a budget is reported as cut off rather than as a clean run, which is what the
`iterationCapped` and `timedOut` flags on a run mean.

The six line-by-line languages share the interpreter loop, so they share its budget,
and a run ends at whichever of the two comes first:

| Budget | Value | What a program sees |
| --- | --- | --- |
| Statements | 2,000,000 | `Warning: Maximum iterations reached` on the last line of output |
| Wall clock | 10 seconds | `Error: Execution timeout (10.000000s exceeded)` |

BASIC, APL, PILOT, FOCAL, COMAL and REXX all count the statements the loop runs, so a
tight `GOTO` loop is cut off by the statement budget in about a second and a half, and
neither budget interrupts a statement in the middle: `SLEEP 20` in BASIC waits its
twenty seconds, and the timeout is reported on the next statement.

The four whole-program languages run their own loops and each bounds itself:

| Language | What stops it |
| --- | --- |
| Scheme | The clock: 10,000 milliseconds, reported as `the program ran for longer than 10000 ms (N steps)`. There is no step limit and no recursion limit. |
| Logo | The same 10-second clock, checked at every turtle step and loop pass; user-procedure recursion stops at 128 deep; `WHILE`, `UNTIL` and `FOREVER` stop after 10,000 passes without a message. |
| Smalltalk | Five million passes of `whileTrue:`, `whileFalse:` or a loop block, reported as `the loop ran for too long`. |
| Forth | Four million instructions, reported as `the program ran for 4000000 instructions without stopping`; a definition that recurses endlessly stops at 4,096 frames. |

Two smaller ceilings belong to one language each: a FOCAL `DO` group nests at most 64
deep, and a PILOT `D:` waits at most ten seconds.

A stop request — the IDE's Stop button, a cancelled API call — is honoured between
statements in the line loop, inside BASIC's `SLEEP` and PILOT's `D:`, and on the next
turtle step or loop pass in Logo.
