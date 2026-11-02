# Language reference

This page lists every command the ten languages accept, the effect of each, and a
program fragment that shows it. Each language has a section of its own, and the
headings form the index.

All ten engines are written in C++ and run in process. There is no separate
interpreter: the IDE, `./twc` and `time_warp_check` call the same code, so a
program behaves the same however it is started. Six languages are line by line, and
the interpreter hands each line to its executor: BASIC, APL, PILOT, FOCAL, COMAL and
REXX. Four are whole program, and the source is read to the end before anything
runs: Logo, Scheme, Smalltalk and Forth.

The languages differ in what they are designed to express:

| Language | Files | Shape | Purpose |
| --- | --- | --- | --- |
| BASIC | `.bas` `.bsc` | line by line | Numbered lines, a graphics screen, sprites and sound |
| APL | `.apl` `.dyalog` | line by line | Arrays as values; a line is an expression |
| Logo | `.logo` `.lg` | whole program | Turtle graphics; procedures as the unit of composition |
| PILOT | `.pilot` `.plt` | line by line | Teaching lessons: text units, answers, matched alternatives |
| FOCAL | `.foc` `.focal` | line by line | Arithmetic at a formula calculator, in numbered groups |
| Scheme | `.scm` `.ss` `.scheme` | whole program | Lists, recursion, procedures as values |
| COMAL | `.comal` `.cml` | line by line | Structured BASIC with blocks, procedures and functions |
| REXX | `.rexx` `.rex` | line by line | Text; every value is a string, and parsing is a statement |
| Smalltalk | `.st` `.smalltalk` | whole program | Objects, messages and classes |
| Forth | `.fs` `.fth` | whole program | The stack and the dictionary |

Commands are grouped by purpose within each language, so the BASIC graphics
statements appear together. Several spellings of one command share a row. The first
column gives the spelling, the second the effect, and the third an example. Where an
example needs more than one line, a fenced block near the table demonstrates it and the
row refers to it. Capitals are the documented spelling. BASIC, COMAL, REXX and Forth
ignore case in source, so either form works. Runtime messages are quoted without the
mark that prefixes every console line.

## BASIC

Line by line, in the dialect of a 1981 microcomputer, with additions: pixel screen
modes, sprites, gradients and sound. Lines may be numbered or unnumbered; a jump
requires a number.

### Program control

```basic
10 GOSUB 100
20 PRINT RESULT
30 END
100 RESULT = 42
110 RETURN
```

| Form | Effect | Example |
| --- | --- | --- |
| `GOTO` | Jumps to a line number. | `GOTO 20` |
| `GOSUB`, `RETURN` | Calls a numbered line and returns at the matching `RETURN`. | `GOSUB 100` |
| `IF ... THEN`, `ELSE`, `END IF` | Block conditional. `ENDIF` is the same word as `END IF`. | `IF X > 10 THEN` |
| one-line `IF` | Runs the rest of the line when the test holds, and the part after `ELSE` when it does not. | `IF X > 9 THEN GOTO 40 ELSE GOTO 50` |
| `FOR ... TO ... STEP`, `NEXT` | Counts from the start to the limit inclusive, by one or by the step. Written on one line, the loop repeats within that line. | `FOR I = 1 TO 3` |
| `NEXT` | Closes a loop and may name its variable. | `NEXT` |
| `WHILE`, `WEND` | Repeats while the test holds. `WEND` may be on a later line or later in the same line. | `WHILE X < 3` |
| `DO`, `LOOP` | Repeats a block. The `DO` line may carry `WHILE` or `UNTIL`. | `DO: X = X + 1: LOOP UNTIL X >= 3` |
| `UNTIL` | The test on a `LOOP` that ends the loop when it becomes true. | `LOOP UNTIL X = 2` |
| `EXIT FOR`, `EXIT DO`, `EXIT WHILE` | Leaves that loop from inside it. | `IF I = 3 THEN EXIT FOR` |
| `SELECT CASE`, `CASE`, `END SELECT` | Chooses one body. A clause is a value, a comma-separated list, `a TO b`, `IS` with a comparison, or `ELSE`. The first matching clause runs alone. | `SELECT CASE X` |
| `SUB`, `END SUB` | A named block called with `CALL`. Parameters are local, and the body needs its own lines. | `SUB DOUBLE(N)` |
| `CALL` | Calls a `SUB` or a `FUNCTION` by name. | `CALL GREET("Ann")` |
| `FUNCTION`, `END FUNCTION` | A named block usable in an expression. Its answer is the variable named after the function at `END FUNCTION`. | `PRINT TWICE(21)` |
| `EXIT FUNCTION` | Returns from a `FUNCTION` early with the answer so far. | `EXIT FUNCTION` |
| `DEF FN` | A one-line function of one variable. | `DEF FNA(N) = N * N: PRINT FNA(5)` |
| `TYPE`, `END TYPE` | Declares a record type. Its fields are `field AS type` lines, so they cannot share the header line. | `TYPE POINT` |
| `ON ERROR GOTO` | Diverts to a numbered line when a statement fails. | `ON ERROR GOTO 100` |
| `ON ERROR` | Removes the diversion, so the next failure is reported. | `ON ERROR` |
| `RESUME` | Continues at the numbered line given, after an error. | `100 RESUME 30` |
| `RESUME NEXT` | Continues at the statement after the one that failed. | `100 RESUME NEXT` |
| `END` | Ends the run where it stands. | `PRINT "A": END: PRINT "B"` |
| `RUN` | Clears the run's variables, reports `Program reset` and ends the run. | `RUN` |
| `SYSTEM` | Ends the run and reports `Returned to system prompt`. | `SYSTEM` |
| `REM`, `'` | A comment to the end of the line, written as a word or as an apostrophe. | `REM ignored` |
| `:` | Separates two statements on a line. | `PRINT "A": PRINT "B"` |

`SELECT CASE` evaluates the selector once, when the first clause is tested, and runs
the body of the first clause that matches. `CASE 2` compares against one value,
`CASE 2, 3` against a list, `CASE 4 TO 9` against a range, `CASE IS > 10` against a
comparison, and `CASE ELSE` matches whatever is left. A clause that cannot be read
reports `Invalid CASE expression`, and its body is skipped. Blocks may nest.

A variable may begin with a reserved word: `DOUBLE`, `RUNNER`, `CLOSER`, `NEXTX` and
`LOOPS` are ordinary names.

### Output and input

| Form | Effect | Example |
| --- | --- | --- |
| `PRINT` | Writes its items. A comma between items writes a space, a semicolon nothing. | `PRINT "HELLO WORLD"` |
| `?` | The same statement. | `? 6 * 7` |
| `PRINT USING` | Formats numbers with a mask, `#` for a digit and `.` for the point. | `PRINT USING "##.##"; 3.14159` |
| `PRINT #`, `WRITE #` | Write a record to a channel. `WRITE #` keeps its quotes so `INPUT #` can read the record back. | `PRINT #1, "HELLO"` |
| `LINE INPUT`, `LINE INPUT #` | Reads a whole line into a text variable, from the user or from a channel. | `LINE INPUT "Name: ", N$` |
| `INPUT`, `INPUT #` | Prompts and reads one value per name, from the user or from a channel. | `INPUT "Name: ", N$` |
| `INKEY$` | The next key typed at the canvas, or the empty string. A headless run has no keyboard and answers empty. | `IF INKEY$ = "q" THEN END` |
| `CLS`, `LOCATE`, `WIDTH` | Clears the screen, moves the text cursor, and sets the text width and reports it. | `LOCATE 5, 5` |

### Variables, arrays and strings

| Form | Effect | Example |
| --- | --- | --- |
| assignment | Stores a value. A `$` on the name makes it text. | `X = 42` |
| `LET`, `MAKE` | The same statement, spelled out, or in the form BASIC dialects of the period shared. | `MAKE "X" 7` |
| `DIM`, `DIM ... AS` | Declares an array indexed from 0, or a record from a `TYPE`. | `DIM N(10): N(1) = 7` |
| `ERASE` | Discards an array, which then reads as zeroes. | `ERASE A` |
| `SWAP` | Exchanges two variables. | `SWAP A, B` |
| `CONST` | Names a value and refuses a later assignment to that name. | `CONST SIZE = 5` |
| `DEFINT` | Makes the named variables integer, truncating on assignment. | `DEFINT X: X = 3.7` |
| `DEFLNG`, `DEFSNG`, `DEFDBL` | The same declaration for long, single and double. | `DEFDBL A-Z` |
| `DEFSTR` | Makes the named variables text. | `DEFSTR A-Z` |
| `DATA` | Stores constants for `READ`. | `DATA 3, 4` |
| `READ`, `RESTORE` | Takes the next `DATA` items; rewinds `DATA` to its first. | `READ A: READ B` |
| `LEN`, `MID$`, `CHR$`, `REPEAT$` | Length; a substring counting from 1; a character from its code; a string repeated. | `MID$("HELLO", 1, 3)` |
| `FORMAT$` | A `#`-mask or printf formatting, whichever the format resembles. | `FORMAT$("#.##", 3.14159)` |
| `STR$` | A number as text. | `STR$(12)` |
| `TYPE$` | The type a name has: `DOUBLE`, `INT` or `STRING`. | `TYPE$(X)` |
| `TIME$`, `DATE$` | The clock as `HH:MM:SS`, eight characters; the date as `MM-DD-YYYY`, ten. | `A$ = TIME$` |
| `TIMER` | Seconds since local midnight. | `TIMER >= 0` |

`TIME$`, `DATE$`, `TIMER`, `INKEY$` and `TYPE$(...)` are read as a whole `PRINT` item
and nowhere else, so `LEN(TIME$)` is 0. Assign the clock to a variable first.

### Mathematics

| Form | Effect | Example |
| --- | --- | --- |
| `ABS`, `SQR` | Absolute value; square root. | `ABS(-5)` |
| `INT` | The largest integer not greater than the argument, so a negative rounds down. | `INT(-3.7)` |
| `SIN`, `COS`, `TAN` | Trigonometry in radians. | `SIN(0)` |
| `ATAN` | Arc tangent in radians. | `ATAN(1)` |
| `EXP` | e to the power of the argument. | `EXP(0)` |
| `LOG` | Natural logarithm. | `LOG(1)` |
| `RND` | A random number from 0 up to 1. | `RND(1)` |
| `RANDOMIZE` | Seeds the generator; `RANDOMIZE TIMER` seeds it from the clock. | `RANDOMIZE 42` |

Numbers print to six significant digits: `PRINT 1 / 3` gives `0.333333`, and a large
pixel value prints as `1.67772e+07`. Literals may be written with an exponent, with
or without a sign: `1E6`, `1e-6`, `2.5E+3`.

### Graphics

Drawing statements need a screen. `SCREEN 1` is 800 by 600 with 16 colours, and
`SCREEN 1, w, h` selects another size. Modes 0 for text, 2 at 640 by 200, 7 and 13 at
320 by 200, 9 at 640 by 350 and 12 at 640 by 480 are also available. An unknown mode
is refused, and the report lists the valid ones:
`Value 5 for screen mode is not one of 0, 1, 2, 7, 9, 12, 13`.

```basic
SCREEN 1
LINE (10, 10)-(100, 50), 4
CIRCLE (160, 100), 40
```

| Form | Effect | Example |
| --- | --- | --- |
| `SCREEN` | Chooses a mode and a size. | `SCREEN 1` |
| `COLOR` | Sets foreground and background colours and reports `Colors set`. | `COLOR 15, 0` |
| `PSET` | Draws one pixel in a colour. | `PSET (5, 5)` |
| `PRESET` | Draws one pixel in the background colour. | `PRESET (5, 5)` |
| `LINE` | Draws between two corners. A second comma and `BF` fill the box. | `LINE (10, 10)-(40, 50), 4, BF` |
| `CIRCLE` | Draws a circle from a centre and a radius. | `CIRCLE (160, 100), 40` |
| `PAINT` | Floods an area with a colour from a point inside it. | `PAINT (50, 30), 4` |
| `PALETTE CYCLE` | Rotates a range of palette slots and reports the range. | `PALETTE CYCLE 1, 15, 1` |
| `SETXY` | Moves the turtle to a point. | `SETXY 10 20` |
| `FORWARD`, `RIGHT`, `PENUP`, `PENDOWN` and the rest of Logo's turtle | A statement BASIC does not recognise is offered to the Logo executor, so the turtle words work here. | `CLEARSCREEN: FORWARD 15: RIGHT 90` |
| `SPRITE` | Captures a rectangle of the screen under a name, places it, or removes it with `OFF`. | `SPRITE SHIP, 10, 10, 30, 40` |
| `SPRITEMOVE` | Moves a placed sprite by a delta. | `SPRITEMOVE SHIP, 5, -5` |
| `SPRITEHIDE` | Hides a sprite and restores what it covered. | `SPRITEHIDE SHIP` |
| `SPRITESHOW` | Draws a hidden sprite again. | `SPRITESHOW SHIP` |
| `GRADIENT` | Fills a rectangle with a colour ramp between two inks, at an optional angle. | `GRADIENT 0, 0, 100, 50, 4, 1, 45` |
| `ELLIPSEGRADIENT` | The same ramp inside the ellipse that the rectangle contains. | `ELLIPSEGRADIENT 0, 0, 60, 40, 2, 3` |
| `FRACTAL` | With no argument, lists the presets; with one, draws it. | `FRACTAL KOCH` |
| `SHAPE` | Draws a named shape, or a polygon of n sides. | `SHAPE POLYGON 6, 40` |
| `PARTICLE` | Creates a named particle effect at a point. | `PARTICLE EXPLOSION, 100, 100` |

`GRADIENT`, `ELLIPSEGRADIENT` and `SPRITE` need a screen and report an error without
one. Placing a sprite name that was never captured reports the statement that would
capture it.

### Sound, timing and the machine

| Form | Effect | Example |
| --- | --- | --- |
| `BEEP` | The system bell, reported as `BEEP`. | `BEEP` |
| `SOUND` | A square wave at a frequency, for a number of ticks of 1/18.2 second. | `SOUND 440, 2` |
| `PLAY` | A note string: `T` tempo, `L` length, `O` octave, `>` and `<` for octave steps, `A` to `G`, and rests. | `PLAY "T120 O4 L8 CDEFGAB"` |
| `SAY`, `SPEAK` | Speech, which this build does not provide: both report `Speech synthesis not available`. | `SAY "HELLO"` |
| `SLEEP` | Waits that many seconds, clamped to 60. | `SLEEP 1` |
| `SPEED` | Sets the turtle's speed and reports it. | `SPEED 5` |
| `ON TIMER` | Calls a numbered line every n seconds. | `ON TIMER(2) GOSUB 100` |
| `TIMER ON` | Arms that timer. | `TIMER ON` |
| `TIMER OFF` | Disarms it. | `TIMER OFF` |
| `POKE` | Writes a byte into the program's own 64 KiB map. | `POKE 1000, 7` |
| `PEEK` | Reads a byte back, as a statement or as a function. | `PEEK 1000` |
| `OUT` | Writes a byte to a port number. | `OUT 100, 5` |
| `IN` | Reads one back. | `IN 100` |
| `JOYINIT` | Opens the first gamepad and reports which, or that there is none. | `JOYINIT` |
| `STICK` | An axis value, and 0 until `JOYINIT` has found a pad. | `STICK(0)` |
| `STRIG` | A button, 1 while it is held. | `STRIG(0)` |
| `SHELL` | Refused: `SHELL commands are disabled in IDE for security`. | `SHELL "echo hi"` |

`SLEEP` is taken in two-millisecond slices, so a stop request breaks it, but the
clock is read between statements: `SLEEP 20` first waits its twenty seconds.

### Files

File statements take a channel number written `#n`. The file manager confines them to
the active workspace.

| Form | Effect | Example |
| --- | --- | --- |
| `OPEN` | Opens a file for `INPUT`, `OUTPUT` or `APPEND` on a channel. | `OPEN "scores.txt" FOR OUTPUT AS #1` |
| `CLOSE` | Closes one channel or all of them, and reports which. | `CLOSE #1` |
| `PUT #`, `GET #` | A channel is named by a `#` after either word. Without an open channel the statement reports `File #9 is not open`. | `PUT #9, , "X"` |

### Notes

- `GOTO`, `GOSUB` and `ON ERROR GOTO` take a line number. `GOSUB HERE` reports
  `Expected numeric value for line number, got 'HERE'`, and the run continues with
  the next line.
- `FOR` leaves the variable at the first value past the limit, so `FOR I = 1 TO 3`
  ends with 4. The limit is tested after the body, so `FOR J = 3 TO 1` runs its body
  once, whether the loop is written on one line or across several.
- `INT` rounds down: `INT(-3.7)` is -4. Division by zero is an error.
- An unknown statement reports `Unknown BASIC command: ...`, and a malformed loop
  reports `FOR requires format: var = start TO end [STEP step]`.
- `SUB`, `FUNCTION` and `TYPE` bodies need their own lines, since a colon ends the
  header. A definition split by colons is refused, rather than run as statements:
  `SUB ... needs its own lines: a definition cannot be split by colons`. Loops and
  plain statements accept colons: `WHILE X < 3 : PRINT X : X = X + 1 : WEND` and
  `FOR I = 1 TO 3 : PRINT I : NEXT` repeat within the line, and an `EXIT FOR` inside
  one leaves the loop.

## APL

Line by line. Every line is an expression, and its value is displayed. Arrays are
values, so most functions take vectors and tables as readily as single numbers. Each
function has a glyph and a keyword, and both are accepted, so a keyboard without `⍳`
can still write APL.

### Values and the line

```apl
IOTA 5
2 3 RHO IOTA 6
'HELLO'
SIGNUM ¯3
```

A bare number is a scalar, and numbers separated by spaces form one vector: `42` is a
value and `1 2 3` is a value. `'HELLO'` is a character vector and prints without its
quotes. `¯` is the negative sign.

| Form | Effect | Example |
| --- | --- | --- |
| `RHO` | Reshape with two arguments; the shape of its argument with one. | `2 3 RHO 1 2 3 4 5 6` |
| `RESHAPE`, `SHAPE` | The keyword spellings of the two-argument and one-argument forms. | `3 RHO 1` |

A table prints one row per line, so `2 3 RHO IOTA 6` gives `1 2 3` and then `4 5 6`.

### Scalar functions

Scalar functions apply to every element of an array: `1 2 3 + 10` works, and
`1 2 + 4 5 6` is a length error.

| Form | Effect | Example |
| --- | --- | --- |
| `TIMES` | Multiplication; `×` is the same function. | `3 TIMES 4` |
| `DIVIDE` | Division. | `6 DIVIDE 2` |
| `POWER` | Raised to a power. | `2 POWER 10` |
| `CEILING` | Rounds up with one argument; the larger of two with two. | `CEILING 2.2` |
| `FLOOR` | Rounds down, or the smaller of two. | `FLOOR 2.8` |
| `MAX`, `MIN` | The larger and the smaller of two arguments. | `3 MAX 5` |
| `ABS` | Absolute value. | `ABS -3` |
| `RESIDUE` | The remainder of the left argument divided by the right. | `3 RESIDUE 10` |
| `SIGNUM` | -1, 0 or 1 according to the sign. | `SIGNUM ¯3` |
| `RECIPROCAL` | One over the argument. | `RECIPROCAL 4` |
| `AND`, `OR`, `NAND`, `NOR`, `NOT` | Boolean functions on 0 and 1. | `1 NAND 1` |
| `EQUAL`, `NOTEQUAL`, `LESS`, `LESSEQ`, `GREATER`, `GREATEREQ` | Comparisons, answering 1 or 0 per element. | `1 2 3 EQUAL 1 5 3` |
| `ROLL` | Random integers below the right argument; `?` for short. | `3 ROLL 10` |

### Structural functions

| Form | Effect | Example |
| --- | --- | --- |
| `IOTA`, `INDEXOF` | The first n integers; where the right argument appears in the left. | `IOTA 5` |
| `REVERSE`, `ROTATE` | The argument backwards, or rotated left by n. | `1 ROTATE 1 2 3` |
| `TAKE`, `DROP` | The first n items, padded with zeroes; the items after them. | `3 TAKE 1 2` |
| `CATENATE` | The two arguments joined. | `1 2 CATENATE 3` |
| `RAVEL` | The argument flattened into a vector. | `RAVEL 2 2 RHO IOTA 4` |
| `TRANSPOSE` | Rows and columns exchanged. | `TRANSPOSE 2 3 RHO IOTA 6` |
| `GRADEUP`, `GRADEDOWN` | The positions that sort the argument, ascending or descending, rather than the sorted data. | `GRADEUP 3 1 2` |
| `MEMBERSHIP` | For each left element, whether it occurs in the right argument. | `1 MEMBERSHIP 1 2 3` |
| `FORMAT` | A value as the character vector that prints it. | `FORMAT 42` |

### Operators and defined functions

Reduce and scan take the function on their left. Their axis brackets, as in `+/[2]`,
are not implemented, so both work along the outermost axis.

| Form | Effect | Example |
| --- | --- | --- |
| `REDUCE`, `SCAN` | Folds the function over a vector; the running fold. `+/` and `+\` are the same operators. | `+REDUCE IOTA 5` |
| `∇` | Opens and closes a function definition. The header names the result and the argument; the body assigns the result. | see below |

```apl
∇ R ← DOUBLE X
  R ← X TIMES 2
∇
DOUBLE 21
```

Defining a function reports `APL: DOUBLE defined`, and calling it displays 42.

### Turtle

| Form | Effect | Example |
| --- | --- | --- |
| `FD`, `FORWARD`, `BK`, `BACK` | Forward and back, drawing while the pen is down. | `FD 100` |
| `RT`, `RIGHT`, `LT`, `LEFT` | Turn right or left in degrees. | `RT 90` |
| `PU`, `PENUP`, `PD`, `PENDOWN` | Lift the pen, and put it down. | `PU` |
| `HOME` | Back to the origin, facing up. | `HOME` |
| `TX`, `TY`, `THETA` | The turtle's position and heading, as reporters. | `FD 100` then `TY` |

`X`, `Y` and `H` are ordinary APL variables; the reporters are `TX`, `TY` and
`THETA`.

### Notes

- There is no precedence. A function takes the whole expression on its right, so
  `2 TIMES 3 + 4` is 14.
- Lengths must agree. `1 2 + 4 5 6` reports `APL: length error: 2 and 3`, naming both
  shapes.
- A name that holds nothing is an error rather than a zero, and so is an unmatched
  bracket: `APL: NOSUCHNAME has no value`, `APL: unmatched (`.
- Outer product, rank, the composition operators, the system functions and labelled
  branch are not implemented. Each is a read error naming the character it could not
  read, as in `APL: cannot read '.'`.

## Logo

Whole program. A word takes its inputs after it, as in `FD 50`, and a bracketed list
is a body, as in `REPEAT 4 [FD 10 RT 90]`. Procedures are written with `TO` and `END`
and are the unit of composition.

### Turtle movement

| Form | Effect | Example |
| --- | --- | --- |
| `FD`, `FORWARD` | Forward by that many steps, drawing. | `FD 50` |
| `BK`, `BACK`, `BACKWARD` | Backwards. | `BK 50` |
| `RT`, `RIGHT` | Turn right in degrees. | `RT 90` |
| `LT`, `LEFT` | Turn left. | `LT 90` |
| `UP`, `UPWARD`, `DOWN`, `DOWNWARD` | Move up or down the screen without turning. | `UP 40` |
| `SETX`, `SETY`, `SETXY`, `SETPOS`, `SETPOSITION` | Move to an x, a y or a point, given as numbers or as a list. | `SETXY 10 10` |
| `SETH`, `SETHEADING` | Face an absolute heading, 0 being north. | `SETH 90` |
| `HOME` | To the origin, facing north. | `HOME` |
| `CS`, `CLEARSCREEN` | Clear the canvas and go home. | `CS` |
| `WINDOW`, `WRAP`, `FENCE` | Choose what happens at the edge, and report it: `Screen mode: WINDOW`. | `WRAP` |
| `ARC` | An arc of that angle and radius, leaving the turtle at its end. | `ARC 90 40` |
| `BEZIER` | A cubic curve from the turtle through two control points to an end point, leaving it facing along the curve. | `BEZIER 0 60 60 60 60 0` |
| `DOT` | A dot of that diameter at the turtle. | `DOT 6` |
| `STAMP`, `STAMPCLEAN` | A copy of the turtle's shape inked where it stands; removing the stamps. | `STAMP` |
| `FILLED` | Runs a body with the area it encloses filled in that colour. | `FILLED "RED [FD 20 RT 90 FD 20 RT 90 FD 20]` |
| `LABEL` | Writes a word at the turtle. | `LABEL "HI` |
| `SETLABELHEIGHT`, `TEXTSIZE` | The height of that text. | `SETLABELHEIGHT 12` |
| `WAIT` | Pauses for that many seconds. | `WAIT 1` |

### Pen and colour

| Form | Effect | Example |
| --- | --- | --- |
| `PU`, `PENUP` | Lifts the pen, so the next move leaves no ink. | `PU` |
| `PD`, `PENDOWN` | Puts it back down. | `PD` |
| `SETPC`, `SETPENCOLOR`, `SETCOLOR` | The pen colour, by name in quotes or by palette slot. An unknown name is ignored. | `SETPENCOLOR "RED` |
| `SETBG`, `SETBGCOLOR` | The background colour. | `SETBG "BLUE` |
| `PENSIZE`, `SETPENSIZE`, `PENWIDTH`, `SETPENWIDTH`, `SETPW` | The width of the stroke. | `PENSIZE 3` |
| `PENSTYLE` | 0 solid, 1 dashed, 2 dotted, 3 dash-dot, 4 invisible. | `PENSTYLE 1` |
| `PENERASE`, `PE` | Draws in the background colour instead of the pen's. | `PENERASE` |
| `PENPAINT` | Back to painting. | `PENPAINT` |
| `PENREVERSE` | Draws in whatever is under the pen, inverted. | `PENREVERSE` |
| `PENMODE`, `PPT`, `PX` | The pen's mode, its paint colour, and the colour where it stands. | `PX` |
| `CLEAN`, `CLEAR` | Clears the ink, leaving the turtle in place or sending it home. | `CLEAN` |
| `HIDETURTLE`, `HT` | Hides the turtle. | `HT` |
| `SHOWTURTLE`, `ST` | Shows it again. | `ST` |

### Procedures and control

```logo
TO SQUARE :S
REPEAT 4 [FD :S RT 90]
END
SQUARE 20
```

| Form | Effect | Example |
| --- | --- | --- |
| `TO` ... `END` | Defines a procedure; the inputs are the `:name` words in the header. | as above |
| a procedure call | Any word that is not a primitive runs the procedure of that name. | `SQUARE 20` |
| `REPEAT` | Runs a body a number of times; `REPCOUNT` counts the passes. | `REPEAT 4 [FD 10 RT 90]` |
| `IF` | Runs a body when the test holds. | `IF 1 = 1 [FD 10]` |
| `IFELSE` | Runs one body or the other. | `IFELSE 1 = 1 [PRINT "YES"] [PRINT "NO"]` |
| `WHILE` | Repeats while the first body holds. | `WHILE [:I < 3] [MAKE "I :I + 1]` |
| `FOREVER` | Repeats until the body stops the run. | `FOREVER [STOP]` |
| `FOREACH` | Runs a body once per item of a list. | `FOREACH [1 2 3] [FD 10]` |
| `REPCOUNT` | The pass number of the enclosing loop, from 1. | `REPEAT 2 [SHOW REPCOUNT]` |
| `LOCAL` | Declares a name that belongs to the procedure body; the caller's value for it is restored on return. `LOCAL "X` and `LOCAL :X` both name `X`. | `LOCAL :X` |
| `OUTPUT`, `OP` | The value a procedure reports to its caller. | `OUTPUT :N * 2` |
| `STOP`, `BYE` | Ends the procedure, or the run. | `BYE` |
| `REM`, `;` | A comment, on its own or after a statement: the rest of the line is ignored. | `FD 10 ; and stop here` |

### A name holds one value

`MAKE` and a procedure input read their value the same way:

| Written | Value |
| --- | --- |
| `42`, `:N + 1` | a number |
| `"red`, `"red"` | the word `red`; the closing quote is optional |
| `"hello world"` | the text, spaces and all |
| `[1 2 3]` | a list of three items |
| `:count` | whatever `count` holds, of whatever kind |

A name holds one value, so `MAKE "X 5` followed by `MAKE "X "hello` leaves the word
and not the number beside it.

### What a procedure's inputs are

`TO SQUARE :S` declares one input. A call takes one value per input, in order, and
binds it as `MAKE` would: `SQUARE 20` passes a number, `SQUARE :N + 1` passes the
result, `SQUARE "green` passes a word and `SQUARE [1 2 3]` passes a list. Fewer values
than inputs reports `Procedure SQUARE requires 1 inputs`; words after the last input
are ignored.

An input shadows the caller's value of that name, whatever kind it is, and the
caller's value is restored when the procedure returns. `LOCAL` declares another name
on the same frame, restored in the same way.

The body ends at `STOP`, at `OUTPUT` whose value the caller receives, or at `BYE`. An
error ends the body, is reported, and the caller continues. Recursion is limited to
128 calls.

### Data and reporters

```logo
MAKE "X 5
PRINT :X
SHOW FIRST [1 2 3]
```

| Form | Effect | Example |
| --- | --- | --- |
| `MAKE` | Binds a name to one value, read as the table above describes. | `MAKE "X 5` |
| `THING` | The value a name holds, quoted: `THING "X`. | `SHOW THING "X` |
| `PRINT` | Writes a value. Text it cannot evaluate is written back as it stands. | `PRINT "HELLO` |
| `SHOW` | Writes a value and ends the line. | `SHOW 42` |
| `TYPE` | Writes a value. The console stores lines rather than a character stream, so it ends the line as `PRINT` does. | `TYPE "A` |
| `FIRST`, `LAST` | The first or last item. | `SHOW FIRST [1 2 3]` |
| `BUTFIRST`, `BUTLAST` | The list without its first or last item. | `SHOW BUTFIRST [1 2 3]` |
| `ITEM` | The nth item, counting from 1. | `SHOW ITEM 2 [1 2 3]` |
| `COUNT` | The number of items in a list, or of characters in a word. | `COUNT [1 2 3]` |
| `LIST` | A list of its arguments. | `SHOW LIST 1 2` |
| `SENTENCE` | The arguments as one flat list. | `SHOW SENTENCE 1 2` |
| `SUM`, `DIFFERENCE`, `PRODUCT`, `QUOTIENT` | Arithmetic, also spelled `+ - * /`. | `SHOW SUM 1 2` |
| `RANDOM` | An integer from 0 up to one less than the argument. | `SHOW RANDOM 10` |
| `MAP`, `FILTER` | A new list of the template's answers, or the items whose template answer holds; `?` is the item. | `SHOW MAP [? * 2] [1 2]` |
| `REDUCE` | Folds a list with a template of two, `?1` for the running value and `?2` for the next item. `REDUCE "SUM [1 2 3]` names a procedure instead. | `REDUCE [SUM ?1 ?2] [1 2 3]` |
| `POS` | Where the turtle stands, as a list of two numbers. | `SHOW POS` |
| `XCOR`, `YCOR`, `HEADING` | One number of that pose: across, up, and the heading in degrees clockwise from north. | `SHOW HEADING` |

Lists print in square brackets with their words quoted: `SHOW LIST 1 2` gives
`[1.0, 2.0]`, and `SHOW BUTFIRST [1 2 3]` gives `['2', '3']`.

### Mathematics

```logo
PRINT ABS 5
PRINT SQRT 16
PRINT MAX(3, 7)
```

| Form | Effect | Example |
| --- | --- | --- |
| `ABS` | Absolute value. | `ABS(-5)` |
| `SGN` | -1, 0 or 1 according to the sign. | `SGN(-3)` |
| `INT`, `FLOOR` | The largest integer not greater than the argument, so a negative rounds down. | `INT(3.7)` |
| `CEIL` | The smallest integer not less than the argument. | `CEIL(1.2)` |
| `FIX` | The integer part, discarding the fraction. | `FIX(-3.7)` |
| `CINT`, `ROUND` | The nearest whole number, with a half going to the even one. | `ROUND(2.5)` |
| `SQRT`, `SQR` | Square root. | `SQRT 16` |
| `EXP`, `LOG`, `LOG10`, `LOG2` | e to the power; the natural, base-10 and base-2 logarithms. | `LOG 1` |
| `SIN`, `COS`, `TAN` | Trigonometry in radians. `ASIN`, `ACOS` and `ATAN` are the inverses. | `SIN(0)` |
| `SINH`, `COSH`, `TANH` | The hyperbolic forms. | `TANH(0)` |
| `MAX`, `MIN`, `POW` | The larger, the smaller, and one raised to the other. | `MAX(3, 7)` |

A function takes the one number written after it, so `SQRT 16 + 9` is 13: the root is
taken of 16 and the 9 is added afterwards. A negative argument, a longer expression or
a second argument needs the bracket: `ABS(-5)` is 5, `SQRT(16 + 9)` is 5, and
`MAX(3, 7)` is 7. Numbers print to six significant digits, so `1 / 3` is `0.333333`.

### Three dimensions

| Form | Effect | Example |
| --- | --- | --- |
| `ENABLE3D` | Turns on the three-dimensional turtle, and reports it. | `ENABLE3D` |
| `DISABLE3D`, `2DON` | Back to two dimensions. | `DISABLE3D` |
| `3DON`, `3DOFF` | The same two states again. | `3DON` |
| `PITCH` | Tips the turtle's nose up or down. | `PITCH 30` |
| `TILTUP`, `TILTDOWN` | The same by a smaller step. | `TILTUP 10` |
| `TILTROLL`, `ROLL` | Rolls about the direction of travel. | `ROLL 10` |
| `YAW` | Turns about the vertical. | `YAW 45` |

### Notes

- `PRINT` runs a word that reports a value and writes the answer:
  `PRINT FIRST [1 2 3]` writes 1, and `PRINT COUNT [1 2 3]` writes 3. The words it
  runs are `SUM`, `DIFFERENCE`, `PRODUCT`, `QUOTIENT`, `RANDOM`, `WORD`, `LIST`,
  `SENTENCE`, `FIRST`, `LAST`, `BUTFIRST`, `BUTLAST`, `ITEM`, `THING`, `COUNT`,
  `REPCOUNT`, `MAP`, `FILTER` and `REDUCE`. Text it cannot evaluate is written back
  unchanged.
- A template is evaluated as an expression before it is run as commands. `MAP [? * 2]`
  and `FILTER [? > 7]` bind `?` to the item, and `REDUCE` binds `?1` and `?2`.
  `FOREACH` runs a command list and does not bind `?`.
- The closing quote of a word form is optional: `PRINT "HI` and `PRINT "HI"` both
  write `HI`, and `MAKE "X "HI"` binds `HI`.
- Word arguments are upper-cased. `SHOW "hello` writes `HELLO`, and
  `SHOW LIST "abc "def` gives `['ABC', 'DEF']`, while `PRINT "hello` writes `hello`,
  because the printer reads the text of the line.
- `TYPE` ends the line. The console stores lines rather than a character stream, so
  `TYPE "A` followed by `PRINT "B` shows two lines.
- `WHILE`, `UNTIL` and `FOREVER` stop silently after 10,000 passes; `REPEAT` has no
  such cap. Recursion stops at 128 calls:
  `Procedure R exceeded maximum recursion depth (128)`.

## PILOT

Line by line. A line is a teaching unit: a letter, a colon, and the text of the unit.
A lesson asks a question, reads an answer and decides whether the answer matched.
Every unit also has a long form (`PRINT:`, `ACCEPT:`, `MATCH:`, `JUMP:`, `USE:`,
`STOP:`, `PAUSE:`, `GRAPHIC:`, `FILE:`, `DELAY:`, `HINT:`, `COMPUTE:`, `NO:`), and
both spellings are accepted.

### Text and answers

| Form | Effect | Example |
| --- | --- | --- |
| `T:` | Writes the text, with `$NAME` replaced by a variable's value. | `T: Hello there` |
| `A:` | Reads an answer into `ANSWER$`. | `A:` |
| `A:$NAME` | Reads an answer into `NAME$`, showing `? ` as the prompt. | `A:$NAME` |
| `C:` | Computes. A name ending in `$` stores text; any other name stores a number. | `C:X = 5` |
| `H:` | A hint, written as `Hint: ...`. | `H: Try the other one` |
| `REMARK`, `R:` | A remark, ignored. | `REMARK a note` |
| `D:` | Waits that many seconds, capped at ten. | `D: 0` |
| `P:` | Waits for Enter. | `P:` |

```pilot
T: What is your name?
A:$NAME
T: Hello $NAME
```

### Matching and branching

| Form | Effect | Example |
| --- | --- | --- |
| `M:` | Compares the last answer with comma-separated alternatives, ignoring case. `*` and `?` are wildcards, and plain text matches anywhere in the answer. | `M: PARIS` |
| `$LEFT`, `$MATCH`, `$RIGHT` | Set by a match: the answer before it, the part that matched, and the rest. | `T: [$MATCH]` |
| `TY:`, `TN:` | Writes the text when the last match succeeded, or when it failed. | `TY: Correct` |
| `Y:`, `N:` | Jumps when the last match succeeded; writes its text when it failed. | `Y: HIT` |
| `B:` | Jumps when the expression before the `=` is non-zero. | `B:X=*HIT` |
| `J:`, `L:` | Jumps to a label; declares one that a jump can reach, as does `*NAME` on its own line. | `J: SKIP` |
| `E:` | Ends the program, or returns from a subroutine when one is running. | `E:` |
| `U:`, `S:`, `TU` | Calls a labelled block as a subroutine, nested up to 128 deep. | `U: *SUB` |

The sequence `T: Q`, `A:`, `M: PARIS`, `TY: Correct`, `TN: Wrong` prints `Correct` for
an answer of `PARIS` and `Wrong` for anything else. A jump to a label that does not
exist reports `Label 'NOWHERE' not found` and the lesson continues at the next line,
so a mistyped label costs a unit rather than the run.

### Graphics and files

| Form | Effect | Example |
| --- | --- | --- |
| `G:` | Runs one turtle word, so a drawing is a run of units. | `G: FD 50` |
| `G: TEXT` | Writes a label at the turtle. | `G: TEXT A label` |
| `G: FILL` | Fills the area the path encloses, in the pen colour. | `G: FILL` |
| `F: OPEN` | Opens a file for reading, or for `R`, `W` or `A` when a mode is given. | `F: OPEN notes.txt W` |
| `F: CLOSE` | Closes it. | `F: CLOSE notes.txt` |
| `F: READ` | Reads a line into a variable. | `F: READ notes.txt LINE$` |
| `F: WRITE` | Writes the rest of the unit as a line, with `$name` and `#name` replaced as in a `T:` unit. The text may be several words. | `F: WRITE notes.txt total #X words` |
| `LINK` | Chains to another lesson: that file replaces this one, and everything computed so far stays in scope. The lesson that linked does not continue afterwards. | `LINK lesson.pilot` |

### Notes

- A plain `A:` leaves the answer in `ANSWER$`; `A:$NAME` puts it in `NAME$`. The
  prompt is `? ` unless the unit carries text of its own.
- `M:` matches loosely. Case is ignored, commas separate alternatives, `PAR*S` is a
  pattern, and `RI` matches `PARIS`, because plain text only has to appear in the
  answer.
- `F:` checks the mode rather than the number of words: `F: OPEN notes.txt X` reports
  `F: OPEN requires mode R|W|A, got: X`, while `F: OPEN notes.txt` reads.
- `LINK` chains rather than calls. The second lesson starts at its first unit with the
  first lesson's variables, and a name it cannot open is reported:
  `LINK: cannot open 'name'`.
- A headless run has no keyboard, so the command harness answers nothing at `A:` and
  every `TN:` path is taken.

## FOCAL

Line by line, in DEC's teaching language of 1969: a handful of commands, one number
type, and labels of the form `group.line` so that five lines can be named as a unit.
Every command has a short letter and a full name, and both are accepted.

### Labels and the commands

A label is `NN.NN`, with each part up to two digits. Leading zeroes do not matter, so
`GOTO 1.40` reaches the line written `01.40`. The second part is a whole number rather
than a decimal, so `1.3` names group 1 line 3 and not the line written `01.30`. A
reference to a label that does not exist reports `FOCAL: no line labelled 9.9`.

| Form | Effect | Example |
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

`%n` sets the number of decimal places and holds until the next `%n`. The default is
two, and a value is rounded to it. `!` suppresses the newline within one `TYPE`, so
`T "A"!"B"` writes `AB`.

| Form | Effect | Example |
| --- | --- | --- |
| `%n` | That many decimal places for the rest of the session. | `T %0,"R=",7/2` |
| `!` | No newline at that point in the statement. | `T "A"!"B"` |
| `FPI`, `FRAN` | Pi, and a random number, written without parentheses. `FRAN(1)` seeds it. | `T FPI` |
| `FABS`, `FSQT`, `FEXP`, `FLOG` | Absolute value, square root, e to the power, natural log. | `T FSQT(16)` |
| `FSIN`, `FCOS`, `FATN` | Trigonometry in radians. | `T FSIN(0)` |
| `FITR`, `FSGN` | Truncates to an integer; the sign as -1, 0 or 1. | `T FITR(3.9)` |

### Turtle

| Form | Effect | Example |
| --- | --- | --- |
| `FORWARD`, `FD`, `BACK`, `BK` | Forward and back, drawing. | `01.10 FORWARD 100` |
| `RIGHT`, `RT`, `LEFT`, `LT` | Turning. | `01.10 RIGHT 90` |
| `PENUP`, `PU`, `PENDOWN`, `PD` | The pen. | `01.10 PENUP` |
| `HOME` | To the origin. | `01.10 HOME` |

### Notes

- `IF` has no relational operators. `IF (X) a,b,c` goes to `a` when X is negative,
  `b` when it is zero and `c` when it is positive, so a three-way choice needs the
  `GOTO` lines that a modern language would do without.
- Undefined variables are 0, so a mistyped name prints `0.00` rather than reporting
  itself.
- `FOR` keeps the last value it used: after `FOR I=1,3` the variable holds 3.
- An unrecognised command is an error rather than a comment:
  `FOCAL: unknown command 'TIPE'`. A `DO` group runs past its end unless a `RETURN` is
  there, and nesting stops at 64 deep.

## Scheme

Whole program. A small Lisp: the operator comes first, everything is a list, and a
procedure is a value. The evaluator uses an explicit stack, so a recursive definition
a million calls deep runs as a loop rather than overflowing the machine's stack.

| Form | Effect | Example |
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

The first program prints `negative` and the second prints 15. `display` ends no line,
so the two run together.

```scheme
(define (square n) (when (> n 0) (forward 50) (right 90) (square (- n 1))))
(square 4)
```

### Notes

- Numbers are inexact unless they are integers. `(display pi)` writes `3.14159`,
  `(display (/ 1 3))` writes `0.3333333333333333`, and `(display 1.0)` writes 1.
- A call with the wrong number of arguments is an error, as `(car)` shows:
  `car: expected 1 argument(s), got 0`. An unknown name reports
  `unbound variable: no-such`.
- There are no exact rationals. `1/2` is read as a name and reported unbound.
- The clock is the only budget: no step limit and no recursion limit, and a program
  that never stops is cut off after ten seconds.

## COMAL

Line by line. Structured BASIC, with blocks that end in a word, `:=` for assignment,
and procedures that are called rather than jumped to. Lines may be numbered or not.

| Form | Effect | Example |
| --- | --- | --- |
| `PRINT` | Writes its items. A comma between items writes a space, a semicolon nothing. | `PRINT "Hello, World!"` |
| assignment with `:=` | Stores a value. A `$` on the name makes it text. | `COUNT := 5` |
| `IF ... THEN ... ELSE ... ENDIF` | A block conditional. | `IF SCORE >= 90 THEN` |
| `ELIF` | An extra branch in that block. | `ELIF SCORE >= 80 THEN` |
| `FOR ... TO`, `NEXT` | Counts inclusively, and leaves the first value past the limit. | `FOR I := 1 TO 3` |
| `WHILE`, `ENDWHILE`, `REPEAT`, `UNTIL` | Repeats while the test holds; repeats until it holds, testing after the body. | `WHILE N < 3` |
| `PROC`, `ENDPROC` | A named block with parameters, called for its effect. | `PROC GREET(NAME$)` |
| `CALL` | Calls a `PROC` or a `FUNC`. | `CALL GREET("World")` |
| `FUNC`, `ENDFUNC`, `RETURN` | A named block that answers a value, usable inside an expression. | `PRINT DOUBLE(21)` |
| `LOCAL` | A name that belongs to the function body and is restored when the body returns. | `LOCAL T` |
| `DIM` | Declares an array indexed from 0. | `DIM SCORES(5)` |
| `DATA`, `READ` | Constants, and reading them into names. Each is one statement, so a data line and a read are two lines. | `DATA 4, 5` |
| `MOVE` | Turtle forward, drawing. | `MOVE 100` |
| `TURN` | Turn right in degrees. | `TURN 90` |
| `PENUP`, `PENDOWN`, `PLOT` | Lift the pen, put it back, draw a point. | `PLOT` |

```comal
10 FUNC DOUBLE(N)
20 RETURN N * 2
30 ENDFUNC
40 PRINT DOUBLE(21)
```

### Notes

- `CALL` and an expression are different ways into a function. `PRINT DOUBLE(21)`
  uses the answer; `CALL DOUBLE(21)` runs the body and leaves the result in `RESULT`.
- `LOCAL` is scoped to the call: a name the caller never had reads 0 afterwards, and
  the caller's own value is restored when the body returns.
- `FOR` leaves the first value past the limit, so `PRINT I` after `FOR I := 1 TO 3`
  writes 4.
- An unknown statement is reported rather than skipped:
  `unrecognised statement: NOSUCH`.

## REXX

Line by line, and typeless: every value is a string, no variable needs declaring, and
concatenation is what happens when two values are written next to each other. A name
that has never been assigned reads as its own name in capitals, so `SAY n` prints `N`.

| Form | Effect | Example |
| --- | --- | --- |
| `SAY` | Writes its arguments, abutting them. | `SAY 'Hello' 'World'` |
| `=` | Assignment. | `name = 'Ann'` |
| `\|\|` | Concatenation with nothing between. | `SAY 'Hello' \|\| ' ' \|\| 'World'` |
| `IF ... THEN` | One statement, or a `DO` block, when the test holds. | `IF score > 50 THEN SAY 'pass'` |
| `DO` ... `END` | A block. `ELSE DO` is its other half. | `IF score >= 90 THEN DO` |
| `DO i = a TO b BY n` | A counted loop; `BY` is optional. | `DO n = 10 TO 1 BY -3` |
| `DO WHILE` | A loop with its test first. | `DO WHILE count < 3` |
| `DO ... OVER` | A loop over the words of a string. | `DO word OVER 'a b c'` |
| `SELECT`, `WHEN`, `OTHERWISE` | A multi-way choice. | `SELECT` |
| `LEAVE`, `ITERATE` | Leave the loop; skip to its next pass. | `LEAVE` |
| `PARSE VAR` | Splits a string's words into names; the last name takes the rest of the line. | `PARSE VAR line first rest` |
| `PULL` | Reads a line from the user, splitting it into its names the same way. | `PULL verb rest` |
| `PARSE PULL` | Reads a line into names, one word each and the rest into the last. | `PARSE PULL first rest` |
| `CALL`, `RESULT` | Calls a label as a subroutine and holds its answer. | `CALL DOUBLE 5` |
| `RETURN`, `EXIT` | Ends a subroutine with a value; ends the program. | `RETURN ARG(1) * 2` |
| `ARG` | A subroutine's nth argument. | `ARG(1)` |
| `LENGTH`, `SUBSTR`, `WORD`, `WORDS` | Length; a substring; the nth word; how many words. | `SUBSTR('HELLO', 2, 3)` |
| `UPPER`, `LOWER`, `STRIP`, `SPACE` | Case; trim; pad single spaces out to a count. | `SPACE('a b c', 3)` |
| `LEFT`, `RIGHT`, `COPIES` | Pad to a width; repeat. | `LEFT('ab', 5)` |
| `INDEX`, `SIGN`, `TRUNC`, `DATATYPE` | Position of a substring; the sign; the integer part; `NUM`, `CHAR` or neither. | `INDEX('hello', 'll')` |
| turtle words | `FORWARD`/`MOVE`, `BACK`, `RIGHT`/`TURN`, `LEFT`, `PENUP`, `PENDOWN`, `HOME`. | `FORWARD 10` |

`SPACE('a b c', 3)` writes `a   b   c`: the count is the spaces between words, not a
width, which is the opposite of `LEFT` and `RIGHT`.

### Notes

- Abuttal is concatenation. `SAY 'Hi' name` writes `HiAnn`; `SAY 'a' 'b'` is `ab`.
- An unassigned variable is its own name. `SAY count` writes `COUNT`, and
  `DO WHILE count < 3` compares the string `COUNT` with 3, which is false, so the
  loop never runs.
- `PARSE` gives the last name the rest. `PARSE VAR line first rest` puts the first
  word in `first` and everything after it in `rest`, spaces included.
  `PARSE PULL verb rest` splits a line the same way, so `ADD 2 3` leaves
  `verb = ADD` and `rest = 2 3`.
- `LEFT` and `RIGHT` pad to a width, and no newline is implied.

## Smalltalk

Whole program, and the only object-oriented language of the ten. There is no syntax
for control flow: `ifTrue:`, `whileTrue:` and `to:do:` are messages. A file defines
classes and methods and then runs top-level code, so the whole source is read before
anything runs. A definition is a header followed by indented lines, and it ends at the
next line that begins in column 0.

| Form | Effect | Example |
| --- | --- | --- |
| `subclass:instanceVariableNames:` | Defines a class. This is a keyword message to `Object`, not special syntax. | see below |
| `new` | An instance, whose `initialize` is sent if the class has one. | `Counter new` |
| `>>` | Adds a method to a class. | `Counter >> bump` |
| `Transcript show:` | Writes a value to the transcript. | `Transcript show: 'hello'` |
| `cr` | Ends the transcript line. | `Transcript show: 'a'; cr` |
| `;` | A cascade: the next message goes to the same receiver rather than to the result. | `Transcript show: 'a'; show: 'b'; cr` |
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
`Counter >> value` and then 2. Every definition echoes its own header, and the last
expression of the file is the value displayed. Without the `initialize` method the
instance variable is nil and the first `bump` fails.

### Notes

- Precedence is unary, then binary, then keyword. `2 + 3 * 4` is 20, and
  `1 max: 2 + 3` is 5.
- `^` returns from the method that made the block, not from the block.
- A name that is not a variable is not created silently, and a message a receiver does
  not understand is an error: `unknown variable or global noSuchThing`,
  `3 does not understand foo`, `index 9 is out of range for a collection of 3`.
- A runaway loop is stopped by its own guard: `[true] whileTrue: [...]` reports
  `the loop ran for too long` after five million passes.

## Forth

Whole program, and without syntax in the sense the others have: a program is
whitespace-separated words, and each word takes what it needs from the stack. `2 3 +`
pushes two, pushes three and adds, so arguments come before the word and `FD 100` is
an error where `100 FD` moves the turtle. `: NAME ... ;` adds a word, and from then on
it is indistinguishable from a built-in one.

### The stack

| Form | Effect | Example |
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

| Form | Effect | Example |
| --- | --- | --- |
| `+`, `-`, `*`, `/`, `MOD` | Integer arithmetic. `/` truncates towards zero, and `MOD` takes the sign of the dividend. | `7 3 MOD .` |
| `/MOD` | Both at once, remainder under quotient. | `7 3 /MOD . .` |
| `NEGATE`, `ABS`, `MIN`, `MAX`, `1+`, `1-` | Sign, absolute value, the smaller and the larger, and add or subtract one. | `-9 ABS .` |
| `=`, `<>`, `<`, `>`, `<=`, `>=`, `0=`, `0<`, `0>` | Comparisons, answering the flag -1 for true and 0 for false. | `5 5 = .` |
| `AND`, `OR`, `XOR`, `INVERT`, `LSHIFT`, `RSHIFT` | Bitwise logic and shifts. | `12 10 AND .` |

A condition is any non-zero value, and a true comparison is all bits set: `-1`.

### Output and memory

| Form | Effect | Example |
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

| Form | Effect | Example |
| --- | --- | --- |
| `:` ... `;` | A colon definition: a new word. | `: SQUARE DUP * ;` |
| `IF`, `ELSE`, `THEN` | Runs a body when the flag on the stack is true. | `-1 IF ." yes" THEN` |
| `BEGIN`, `UNTIL` | Repeats until the flag is true. | `BEGIN DUP . 1+ DUP 3 = UNTIL` |
| `BEGIN`, `WHILE`, `REPEAT` | Repeats while the flag is true. | `BEGIN DUP 3 < WHILE DUP . 1+ REPEAT` |
| `DO`, `LOOP`, `+LOOP` | Runs with `I` from the start up to one less than the limit, by one or by the top of the stack. | `3 0 DO I . LOOP` |
| `I`, `J` | The innermost and the next loop counter out. | `2 0 DO 2 0 DO J . LOOP LOOP` |
| `LEAVE`, `EXIT` | Leave the innermost `DO` loop; return from the word. | `2 = IF LEAVE THEN` |
| `RECURSE` | The word being defined, called again. | `: FACT DUP 1 > IF DUP 1- RECURSE * THEN ;` |
| `IMMEDIATE` | Accepted and does nothing: there are no user immediate words. | `: F 1 ; IMMEDIATE` |
| `BYE` | Ends the program where it stands. | `BYE` |
| `(` ... `)`, `\` | Comments, to the closing bracket or to the end of the line. | `( a note )` |

### Turtle

| Form | Effect | Example |
| --- | --- | --- |
| `FD`, `FORWARD` | Forward, drawing. | `100 FD` |
| `BK`, `BACK` | Backwards. | `50 BK` |
| `RT`, `RIGHT`, `LT`, `LEFT` | Turning. | `90 RT 100 FD` |
| `PU`, `PENUP`, `PD`, `PENDOWN` | The pen. | `PD PU` |
| `HOME`, `CS`, `CLEARSCREEN` | To the origin; clear the canvas. | `CS` |
| `SETPC`, `SETPENCOLOR`, `SETPW` | Pen colour as a palette slot, and pen width. | `3 SETPW 100 FD` |
| `ST`, `SHOWTURTLE`, `HT`, `HIDETURTLE` | The turtle's visibility. | `HT` |

### Notes

- Arguments come first and words act immediately. `FD 100` fails where `100 FD`
  draws.
- Division is integer division truncated towards zero, `MOD` takes the dividend's sign
  (`-7 2 /` is -3, `7 -2 MOD` is 1), and there is no floating point.
- Stack underflow is an error naming the word: `Forth, line 1: .: stack underflow`.
  An unknown word reports `undefined word: NOSUCHWORD`, and a definition with no `;`
  reports `the definition of F was never closed`.
- The hard limit is instructions rather than time: four million, and a definition that
  recurses endlessly reports `F: too deep (endless recursion?)`.

## Limits and budgets

Every language stops eventually. The limit differs by language, and a run cut off by
one is reported as cut off rather than as a clean run; the `iterationCapped` and
`timedOut` flags on a run record that.

The six line-by-line languages share the interpreter loop and therefore its budget. A
run ends at whichever of the two limits comes first:

| Budget | Value | What a program sees |
| --- | --- | --- |
| Statements | 2,000,000 | `Warning: Maximum iterations reached` on the last line of output |
| Wall clock | 10 seconds | `Error: Execution timeout (10.000000s exceeded)` |

BASIC, APL, PILOT, FOCAL, COMAL and REXX count the statements the loop runs, so a
tight `GOTO` loop is cut off by the statement budget in about a second and a half.
Neither budget interrupts a statement in the middle: `SLEEP 20` in BASIC waits its
twenty seconds, and the timeout is reported on the next statement.

The four whole-program languages run their own loops, and each bounds itself:

| Language | What stops it |
| --- | --- |
| Scheme | The clock: 10,000 milliseconds, reported as `the program ran for longer than 10000 ms (N steps)`. There is no step limit and no recursion limit. |
| Logo | The same 10-second clock, checked at every turtle step and loop pass. User-procedure recursion stops at 128 deep. `WHILE`, `UNTIL` and `FOREVER` stop after 10,000 passes without a message. |
| Smalltalk | Five million passes of `whileTrue:`, `whileFalse:` or a loop block, reported as `the loop ran for too long`. |
| Forth | Four million instructions, reported as `the program ran for 4000000 instructions without stopping`. A definition that recurses endlessly stops at 4,096 frames. |

Two further ceilings apply to a single language each: a FOCAL `DO` group nests at
most 64 deep, and a PILOT `D:` waits at most ten seconds.

A stop request, whether from the IDE's Stop button or a cancelled API call, is
honoured between statements in the line loop, inside BASIC's `SLEEP` and PILOT's
`D:`, and on the next turtle step or loop pass in Logo.
