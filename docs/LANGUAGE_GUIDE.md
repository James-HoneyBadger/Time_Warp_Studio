# Language Guide - Time Warp Studio

Time Warp Studio actively implements and ships **9 programming languages**:
BASIC, PILOT, Logo, C, Pascal, Prolog, Forth, Brainfuck, and Python. These are
the only languages with working executors, syntax highlighting, examples, and
test coverage in the current release.

---

## Table of Contents

### ✅ Active Languages (supported in this release)

1. [BASIC](#basic)
2. [PILOT](#pilot)
3. [Logo](#logo)
4. [C](#c)
5. [Pascal](#pascal)
6. [Prolog](#prolog)
7. [Forth](#forth)
8. [Brainfuck](#brainfuck)
9. [Python](#python)

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
parent(tom, bob).
parent(bob, ann).
parent(ann, mary).
male(tom).
male(bob).
female(ann).
father(X, Y) :- parent(X, Y), male(X).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

?- parent(tom, X).     % Who are tom's children?   -> X=bob
?- grandparent(tom, X). % Who are tom's grandchildren? -> X=ann
?- father(X, Y).       % Who is a father of whom?  -> X=tom, Y=bob; X=bob, Y=ann
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
5 3 > IF ." 5 > 3" ELSE ." 5 <= 3" THEN
```

### Loops

```forth
: COUNTUP 10 0 DO I . LOOP ;
COUNTUP             \ Prints 0 1 2 3 4 5 6 7 8 9
```

### Complete Example

```forth
: FACTORIAL
    DUP 0 = IF DROP 1 ELSE DUP 1 - FACTORIAL * THEN
;

5 FACTORIAL .       \ Result: 120

: FIBONACCI DUP 2 < IF ELSE DUP 1 - FIBONACCI SWAP 2 - FIBONACCI + THEN ;
```

---

## Brainfuck

Esoteric Turing-complete language with 8 commands. Ideal for understanding computation fundamentals.

| Command | Meaning |
| --------- | --------- |
| `>` | Move data pointer right |
| `<` | Move data pointer left |
| `+` | Increment byte at pointer |
| `-` | Decrement byte at pointer |
| `.` | Output byte as ASCII character |
| `,` | Read one byte of input |
| `[` | Jump past matching `]` if cell is 0 |
| `]` | Jump back to matching `[` if cell is non-zero |

```brainfuck
Hello World in Brainfuck:
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
```

> **Tip:** Brainfuck programs are stopped after 10,000,000 steps to prevent runaway loops.

**File extension:** `.bf`

---

## Python

Modern multi-paradigm educational language (1991) with sandboxed stdlib access.

```python
# Hello World
print("Hello, World!")

# Variables and types
x = 42
name = "Alice"
items = [1, 2, 3, 4, 5]

# F-strings
print(f"Hello, {name}! x = {x}")

# List comprehensions
squares = [n**2 for n in range(1, 6)]
print(squares)  # [1, 4, 9, 16, 25]

# Functions
def factorial(n):
    return 1 if n <= 1 else n * factorial(n - 1)

print(factorial(6))  # 720

# Dictionaries
person = {"name": "Bob", "age": 30}
print(f"{person['name']} is {person['age']}")

# Classes
class Animal:
    def __init__(self, name):
        self.name = name
    def speak(self):
        return "..."

class Dog(Animal):
    def speak(self):
        return "Woof!"

d = Dog("Rex")
print(f"{d.name} says {d.speak()}")

# Turtle graphics (built-in integration)
forward(100)
right(90)
color("blue")
```

**File extension:** `.py`

---

## Language Selection

To select which language your code uses:

1. **File Extension**: Save with correct extension

| Extension | Language |
| ----------- | ---------- |
| `.bas` | BASIC |
| `.logo` | Logo |
| `.pilot` | PILOT |
| `.c` | C |
| `.pas` | Pascal |
| `.pro` / `.pl` / `.prolog` | Prolog |
| `.f` / `.fs` / `.forth` | Forth |
| `.bf` | Brainfuck |
| `.py` | Python |
