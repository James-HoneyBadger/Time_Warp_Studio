# Language Guide - Time Warp Studio

Complete reference for all 7 programming languages supported by Time Warp Studio.

---

## Table of Contents

1. [BASIC](#basic)
2. [Logo](#logo)
3. [PILOT](#pilot)
4. [C](#c)
5. [Pascal](#pascal)
6. [Prolog](#prolog)
7. [Forth](#forth)

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

' Arrays
DIM NUMBERS(10)
DIM GRID(10, 10)
NUMBERS(1) = 100
GRID(2, 3) = 50
```

### Input and Output

```basic
' PRINT outputs to console
PRINT "Hello, World!"
PRINT "Value:", X

' INPUT reads from user
INPUT "Enter name: ", NAME$
INPUT "Enter number: ", X
```

### Control Flow

```basic
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
' Call subroutine
GOSUB CALCULATE
PRINT RESULT

' Subroutine definition
CALCULATE:
    RESULT = X * 2
RETURN

' Functions (single line)
DEF FN SQUARE(N) = N * N
PRINT FN SQUARE(5)
```

### String Operations

```basic
' String concatenation
GREETING$ = "Hello " + NAME$

' String length
LEN$ = LEN(MESSAGE$)

' Substring
PART$ = MID$(MESSAGE$, 1, 5)

' Case conversion
UPPER$ = UCASE$(MESSAGE$)
LOWER$ = LCASE$(MESSAGE$)
```

### Graphics

```basic
' Screen modes
SCREEN 1    ' Graphics mode 320x200

' Colors
COLOR FOREGROUND, BACKGROUND
COLOR 15, 0     ' White foreground, black background

' Drawing
LINE (X1, Y1) - (X2, Y2)       ' Draw line
CIRCLE (X, Y), RADIUS          ' Draw circle
PSET (X, Y)                     ' Draw pixel
PAINT (X, Y), COLOR            ' Fill area

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
' Basic math
ABS(-5)                 ' Absolute value: 5
SQR(16)                 ' Square root: 4
INT(3.7)                ' Integer part: 3

' Trigonometry
SIN(angle)
COS(angle)
TAN(angle)
ATN(tangent)           ' Arctangent

' Other
EXP(x)                 ' e^x
LOG(x)                 ' Natural logarithm
RND                    ' Random 0 to 1
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
FORWARD 100         ' Move forward 100 units
FD 100              ' Short form

BACKWARD 50         ' Move backward 50 units
BK 50               ' Short form

RIGHT 90            ' Turn right 90 degrees
RT 90               ' Short form

LEFT 45             ' Turn left 45 degrees
LT 45               ' Short form

HOME                ' Return to center, facing up
SETPOSITION 100 50  ' Move to absolute position
SETANGLE 90         ' Set heading (0=east, 90=north)
```

### Pen Control

```logo
PENDOWN             ' Enable drawing (default)
PD                  ' Short form

PENUP               ' Disable drawing
PU                  ' Short form

SETPENCOLOR 255     ' Set pen color (0-255 = red)
SETPENWIDTH 2       ' Set pen thickness (1-10 pixels)
```

### Shapes

```logo
CIRCLE 50           ' Draw circle, radius 50

RECTANGLE 100 50    ' Draw rectangle 100x50

POLYGON 6 50        ' Draw polygon with 6 sides, size 50
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

SQUARE              ' Call procedure
```

### Recursion

```logo
TO TREE :SIZE
    IF :SIZE < 5
        FORWARD :SIZE
    ELSE
        FORWARD :SIZE / 2
        RIGHT 30
        TREE :SIZE / 2
        LEFT 60
        TREE :SIZE / 2
        RIGHT 30
        BACKWARD :SIZE / 2
    END
END

TREE 50             ' Draw tree
```

### Complete Example

```logo
TO STAR :SIZE
    REPEAT 5
        FORWARD :SIZE
        RIGHT 144
    END
END

PENCOLOR 255        ' Red
STAR 100

PENCOLOR 32768      ' Green
LEFT 36
STAR 50

PENCOLOR 255 0 0    ' RGB color
HOME
CIRCLE 30
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

### A-Units (Answer Input Validation)

```pilot
T: What is 2 + 2?
A: 4
T: Correct!
```

Answer matches exact response. Advanced matching: `A:4|four|FOUR`

### J-Units (Conditional Jumps)

```pilot
T: What is the capital of France?
A: Paris
J: CORRECT,WRONG

*CORRECT
T: You are correct!
J: END

*WRONG
T: Try again
T: The capital is Paris
```

### M-Units (Multiple Choice)

```pilot
T: Which is a color?
M: Red, Green, Blue, Monday
J: CORRECT,WRONG
```

### Labels and Jumps

```pilot
T: Start the lesson
J: LESSON1

*LESSON1
T: Lesson 1 content
J: LESSON2

*LESSON2
T: Lesson 2 content
J: END

*END
T: Lesson complete
```

### Complete Example

```pilot
T: Math Quiz
T: Get 2 correct to pass

T: What is 3 * 4?
A: 12
J: Q1CORRECT,Q1WRONG

*Q1CORRECT
T: Correct!
J: Q2

*Q1WRONG
T: Wrong. The answer is 12
J: Q2

*Q2
T: What is 5 + 5?
A: 10
J: Q2CORRECT,Q2WRONG

*Q2CORRECT
T: Correct! You passed
J: END

*Q2WRONG
T: Wrong. Study more
J: END

*END
T: Quiz complete
```

---

## C

C language with safe subset of functions.

### Variables and Types

```c
int x = 42;
float pi = 3.14;
char letter = 'A';
```

### Input and Output

```c
#include <stdio.h>

printf("Hello, World!\n");
printf("Value: %d\n", x);

scanf("%d", &x);
```

### Control Flow

```c
if (x > 10) {
    printf("Large\n");
} else {
    printf("Small\n");
}

for (int i = 0; i < 10; i++) {
    printf("%d\n", i);
}

while (x < 100) {
    x = x + 1;
}
```

### Functions

```c
int square(int n) {
    return n * n;
}

int main() {
    int result = square(5);
    printf("Result: %d\n", result);
    return 0;
}
```

### Arrays

```c
int numbers[10] = {1, 2, 3, 4, 5};

for (int i = 0; i < 5; i++) {
    printf("%d\n", numbers[i]);
}
```

---

## Pascal

Pascal language with structured programming.

### Variables and Types

```pascal
VAR
    x: INTEGER;
    name: STRING;
    pi: REAL;

BEGIN
    x := 42;
    name := 'John';
    pi := 3.14159;
END.
```

### Control Flow

```pascal
IF x > 10 THEN
    WriteLn('Large')
ELSE
    WriteLn('Small');

FOR i := 1 TO 10 DO
    WriteLn(i);

WHILE x < 100 DO
    x := x + 1;

REPEAT
    x := x - 1;
UNTIL x < 0;
```

### Procedures

```pascal
PROCEDURE PrintGreeting;
BEGIN
    WriteLn('Hello from procedure');
END;

BEGIN
    PrintGreeting;
END.
```

### Complete Example

```pascal
PROGRAM Calculator;
VAR
    x, y, result: INTEGER;

BEGIN
    WriteLn('Enter first number: ');
    ReadLn(x);
    
    WriteLn('Enter second number: ');
    ReadLn(y);
    
    result := x + y;
    WriteLn('Sum: ', result);
END.
```

---

## Prolog

Prolog language for logic programming.

### Facts

```prolog
parent(tom, bob).
parent(bob, ann).
parent(ann, mary).

male(tom).
male(bob).
female(ann).
```

### Rules

```prolog
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
grandfather(X, Z) :- grandparent(X, Z), male(X).
```

### Queries

```prolog
?- parent(tom, X).     % Who are tom's children?
?- grandparent(tom, X). % Who are tom's grandchildren?
?- father(X, Y).       % Who is a father of whom?
```

### Lists

```prolog
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

%?- member(2, [1, 2, 3]).
```

### Complete Example

```prolog
% Family relations
parent(tom, bob).
parent(bob, ann).
parent(ann, mary).

male(tom).
male(bob).
female(ann).
female(mary).

father(X, Y) :- parent(X, Y), male(X).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

---

## Forth

Forth stack-based language.

### Stack Operations

```forth
5 DUP           \ Duplicate top of stack
5 6 SWAP        \ Swap top two
5 6 7 ROT       \ Rotate
5 DROP          \ Delete top
```

### Arithmetic

```forth
5 3 +           \ Addition: 8
10 3 -          \ Subtraction: 7
4 5 *           \ Multiplication: 20
20 4 /          \ Division: 5
17 5 MOD        \ Modulo: 2
```

### Word Definitions

```forth
: SQUARE DUP * ;    \ Define word
5 SQUARE            \ Result: 25

: CUBE DUP DUP * * ;
3 CUBE              \ Result: 27
```

### Output

```forth
." Hello, World!"    \ Print string
5 .                  \ Print number (pops value)
CR                   \ Carriage return (newline)
```

### Conditionals

```forth
5 3 > IF ." 5 > 3" ELSE ." 5 <= 3" END
```

### Loops

```forth
: COUNTDOWN 10 0 DO I . LOOP ;
COUNTDOWN           \ Prints 10 9 8 7 6 5 4 3 2 1
```

### Complete Example

```forth
: FACTORIAL
    DUP 0 = IF DROP 1 ELSE DUP 1 - FACTORIAL * END
;

5 FACTORIAL .       \ Result: 120

: FIBONACCI DUP 2 < IF ELSE DUP 1 - FIBONACCI SWAP 2 - FIBONACCI + END ;
```

---

## Language Selection

To select which language your code uses:

1. **File Extension**: Save with correct extension
   - `.bas` → BASIC
   - `.logo` → Logo
   - `.pilot` → PILOT
   - `.c` → C
   - `.pas` → Pascal
   - `.pro` → Prolog
   - `.f` → Forth

2. **First Comment**: Start file with language marker
   ```basic
   ' BASIC code
   ```

3. **Menu Selection**: Run → Set Language

---

**For more help:**
- [USER_GUIDE.md](USER_GUIDE.md) - IDE usage
- [DEBUGGER_GUIDE.md](DEBUGGER_GUIDE.md) - Debugging
- [TURTLE_GRAPHICS.md](TURTLE_GRAPHICS.md) - Graphics tutorial
- [FAQ.md](FAQ.md) - Common questions
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Problem solving
