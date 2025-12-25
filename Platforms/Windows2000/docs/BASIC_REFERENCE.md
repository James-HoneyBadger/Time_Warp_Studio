# BASIC Language Reference

Time Warp IDE BASIC Interpreter - Complete command reference.

## Language Overview

Time Warp BASIC is a traditional BASIC dialect supporting:
- Line numbers (optional)
- Variables and arrays
- Structured programming (IF/THEN/ELSE, loops)
- Functions and procedures
- Graphics commands
- File I/O

## Data Types

### Variables
- **Numeric**: Floating-point (e.g., `X = 3.14`)
- **String**: Enclosed in quotes (e.g., `NAME$ = "Alice"`)
- **Array**: Multi-dimensional (e.g., `DIM A(10, 10)`)

### Operators
- **Arithmetic**: `+`, `-`, `*`, `/`, `^` (power), `MOD`
- **Comparison**: `=`, `<>`, `<`, `>`, `<=`, `>=`
- **Logical**: `AND`, `OR`, `NOT`, `XOR`

## Commands

### Input/Output

#### PRINT
Display text or expressions.
```basic
PRINT "Hello, World!"
PRINT X
PRINT "X ="; X
```

#### INPUT
Read user input.
```basic
INPUT "Enter your name: "; NAME$
INPUT "Enter age: "; AGE
```

#### READ/DATA
Read from data statements.
```basic
READ X, Y, Z
DATA 10, 20, 30
```

### Control Flow

#### IF...THEN...ELSE
Conditional execution.
```basic
IF X > 0 THEN PRINT "Positive"
IF X < 0 THEN PRINT "Negative" ELSE PRINT "Zero"
```

#### FOR...NEXT
Loop with counter.
```basic
FOR I = 1 TO 10
  PRINT I
NEXT I

FOR I = 10 TO 1 STEP -1
  PRINT I
NEXT I
```

#### WHILE...WEND
Loop while condition is true.
```basic
WHILE X < 100
  X = X * 2
WEND
```

#### DO...LOOP
Loop with exit conditions.
```basic
DO WHILE X < 100
  X = X + 1
LOOP

DO
  X = X + 1
LOOP UNTIL X >= 100
```

#### GOTO/GOSUB/RETURN
Branch to line number.
```basic
10 GOSUB 100
20 END
100 PRINT "Subroutine"
110 RETURN
```

### Variables & Arrays

#### LET
Assign value to variable.
```basic
LET X = 10
Y = 20    ' LET is optional
```

#### DIM
Declare array.
```basic
DIM A(10)
DIM B(5, 5)
A(3) = 42
```

### Graphics Commands

#### CLS
Clear screen/canvas.
```basic
CLS
```

#### LOCATE
Position cursor (text mode).
```basic
LOCATE 10, 5
PRINT "At row 10, column 5"
```

#### COLOR
Set foreground/background color.
```basic
COLOR 14, 1    ' Yellow on blue
```

#### PSET
Set pixel at coordinates.
```basic
PSET (100, 50), 15
```

#### LINE
Draw line.
```basic
LINE (0, 0)-(100, 100), 12
```

#### CIRCLE
Draw circle.
```basic
CIRCLE (160, 100), 50, 14
```

### Mathematical Functions

| Function | Description | Example |
|----------|-------------|---------|
| `ABS(x)` | Absolute value | `ABS(-5)` → 5 |
| `SIN(x)` | Sine (radians) | `SIN(1.57)` → 1.0 |
| `COS(x)` | Cosine (radians) | `COS(0)` → 1.0 |
| `TAN(x)` | Tangent (radians) | `TAN(0.78)` → 1.0 |
| `ATN(x)` | Arctangent | `ATN(1)` → 0.785 |
| `SQR(x)` | Square root | `SQR(16)` → 4 |
| `INT(x)` | Integer part | `INT(3.7)` → 3 |
| `RND` | Random 0-1 | `RND` → 0.423 |
| `SGN(x)` | Sign (-1/0/1) | `SGN(-5)` → -1 |

### String Functions

| Function | Description | Example |
|----------|-------------|---------|
| `LEN(s$)` | String length | `LEN("Hello")` → 5 |
| `MID$(s$,n,l)` | Substring | `MID$("Hello",2,3)` → "ell" |
| `LEFT$(s$,n)` | Left n chars | `LEFT$("Hello",2)` → "He" |
| `RIGHT$(s$,n)` | Right n chars | `RIGHT$("Hello",2)` → "lo" |
| `CHR$(n)` | Character from code | `CHR$(65)` → "A" |
| `ASC(s$)` | Code from character | `ASC("A")` → 65 |
| `STR$(x)` | Number to string | `STR$(42)` → "42" |
| `VAL(s$)` | String to number | `VAL("42")` → 42 |

### File I/O

#### OPEN
Open file for reading/writing.
```basic
OPEN "DATA.TXT" FOR INPUT AS #1
OPEN "OUTPUT.TXT" FOR OUTPUT AS #2
```

#### CLOSE
Close file.
```basic
CLOSE #1
```

#### INPUT #
Read from file.
```basic
INPUT #1, NAME$, AGE
```

#### PRINT #
Write to file.
```basic
PRINT #2, "Hello, File!"
```

## Example Programs

### Hello World
```basic
PRINT "Hello, World!"
```

### Fibonacci Sequence
```basic
10 A = 0
20 B = 1
30 FOR I = 1 TO 10
40   PRINT A
50   C = A + B
60   A = B
70   B = C
80 NEXT I
```

### Graphics Demo
```basic
10 CLS
20 FOR I = 1 TO 10
30   CIRCLE (160, 100), I * 10, I
40 NEXT I
50 FOR X = 0 TO 319 STEP 10
60   LINE (X, 0)-(X, 199), 15
70 NEXT X
```

### Simple Game
```basic
10 CLS
20 PRINT "Guess the number (1-100)"
30 N = INT(RND * 100) + 1
40 FOR T = 1 TO 10
50   INPUT "Guess: "; G
60   IF G = N THEN PRINT "Correct!": GOTO 100
70   IF G < N THEN PRINT "Too low"
80   IF G > N THEN PRINT "Too high"
90 NEXT T
100 PRINT "The number was"; N
```

## Reserved Words

```
ABS AND ASC ATN CHR CLS COLOR COS
DATA DIM DO ELSE END FOR GOSUB GOTO
IF INPUT INT LEFT LEN LET LINE LOCATE
LOOP MID MOD NEXT NOT OR PSET PRINT
READ REM RETURN RIGHT RND SGN SIN
SQR STEP STR TAN THEN TO UNTIL
VAL WEND WHILE XOR
```

## Compatibility Notes

- Line numbers are optional
- Supports modern structured programming
- Compatible with GW-BASIC and QBasic syntax
- Graphics commands target 1024×768 canvas

---

**Version**: 5.1.0  
**Platform**: Time Warp IDE for Windows 2000
