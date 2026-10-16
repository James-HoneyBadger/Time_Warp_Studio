# Language Guide - Time Warp Studio

Time Warp Studio actively implements and ships **9 programming languages**:
BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, COMAL, and REXX.

> **The reference below is complete for six of them.** BASIC, PILOT, Logo, COMAL,
> REXX and Scheme have command sections, because they have been documented since
> the beginning. **APL, FOCAL and CHIP-8 are implemented, registered and tested** -
> each has a test module and a worked example under `Examples/` - but their
> reference sections have not been written yet, so they are described here by file
> extension and example rather than by command table. `docs/ROADMAP.md` tracks the
> gap rather than leaving it to be discovered.

---

## Table of Contents

### ✅ Active Languages (supported in this release)

1. [BASIC](#basic)
2. [PILOT](#pilot)
3. [Logo](#logo)
4. [COMAL](#comal)
5. [REXX](#rexx)
6. [Scheme](#scheme)

### Newer languages (example first, reference pending)

| Language | Extensions | Start with |
| --- | --- | --- |
| **APL** | `.apl`, `.dyalog` | [`Examples/apl/arrays_and_turtle.apl`](../Examples/apl/arrays_and_turtle.apl) |
| **FOCAL** | `.foc`, `.focal` | [`Examples/focal/square.foc`](../Examples/focal/square.foc) |
| **CHIP-8** | `.ch8`, `.c8` | [`Examples/chip8/font_and_frames.ch8`](../Examples/chip8/font_and_frames.ch8) |

---

## BASIC

Classic BASIC language with Turbo BASIC graphics extensions.

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

; Pen width
SETPENWIDTH 2
```

### Shapes

```logo
; Logo has no CIRCLE, RECTANGLE, or POLYGON commands.
; Build shapes with REPEAT:
REPEAT 4 [FD 100 RT 90]
REPEAT 6 [FD 50 RT 60]
```

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
`pendown`, `home`, `circle`, `color`.

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
| `.scm` / `.ss` | Scheme |
