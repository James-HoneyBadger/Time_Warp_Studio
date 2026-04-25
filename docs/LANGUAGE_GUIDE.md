# Language Guide - Time Warp Studio

Complete reference for all **20 programming languages** supported by Time Warp Studio.

---

## Table of Contents

### Classic Educational
1. [BASIC](#basic)
2. [Logo](#logo)
3. [PILOT](#pilot)

### Systems & Structured
4. [C](#c)
5. [Pascal](#pascal)

### Functional & Declarative
6. [Prolog](#prolog)
7. [Haskell](#haskell)
8. [Scheme](#scheme)

### Scripting & General-Purpose
9. [Python](#python)
10. [JavaScript](#javascript)
11. [Lua](#lua)
12. [REXX](#rexx)
13. [Perl](#perl)
14. [Ruby](#ruby)

### Stack & Concatenative
15. [Forth](#forth)

### Object-Oriented
16. [Smalltalk](#smalltalk)

### Concurrent & Systems
17. [Erlang](#erlang)
18. [Rust](#rust)

### Event-Driven
19. [HyperTalk](#hypertalk)

### Esoteric
20. [Brainfuck](#brainfuck)

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
SETHEADING 90       ' Set heading (0=north/up, 90=east/right)
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

## Python

Modern, multi-paradigm language executed in a safe sandbox.

```python
# Variables and output
name = "Alice"
age = 30
print(f"Hello, {name}! You are {age} years old.")

# Functions
def factorial(n):
    return 1 if n <= 1 else n * factorial(n - 1)

for i in range(1, 8):
    print(f"{i}! = {factorial(i)}")

# List comprehension
squares = [x**2 for x in range(1, 11)]
print(squares)

# Classes
class Dog:
    def __init__(self, name):
        self.name = name
    def speak(self):
        return f"{self.name} says Woof!"

print(Dog("Rex").speak())
```

**File extension:** `.py`

---

## JavaScript

Web-era scripting language with first-class functions.

```javascript
// Variables
let name = "Alice";
const PI = 3.14159;
var count = 0;

console.log(`Hello, ${name}!`);
console.log("PI =", PI);

// Functions and arrow functions
function factorial(n) {
    return n <= 1 ? 1 : n * factorial(n - 1);
}

const double = x => x * 2;
const square = x => x ** 2;

for (let i = 1; i <= 6; i++) {
    console.log(`${i}! = ${factorial(i)}`);
}

// Array methods
let nums = [1, 2, 3, 4, 5];
let doubled = nums.map(double);
let evens = nums.filter(x => x % 2 === 0);
console.log("Doubled:", doubled);
console.log("Evens:", evens);

// Object
let person = { name: "Bob", age: 28, greet() { return `Hi, I'm ${this.name}`; } };
console.log(person.greet());
```

**File extension:** `.js`

---

## Lua

Lightweight, embeddable scripting language.

```lua
-- Variables and output
local name = "Alice"
local count = 5
print("Hello, " .. name .. "!")

-- Functions
local function factorial(n)
    if n <= 1 then return 1 end
    return n * factorial(n - 1)
end

for i = 1, count do
    print(i .. "! = " .. factorial(i))
end

-- Tables (arrays and dicts)
local fruits = {"apple", "banana", "cherry"}
for i, v in ipairs(fruits) do
    print(i, v)
end

local person = {name = "Bob", age = 30}
print(person.name, person.age)

-- Closures
local function make_counter()
    local n = 0
    return function() n = n + 1; return n end
end
local c = make_counter()
print(c(), c(), c())
```

**File extension:** `.lua`

---

## REXX

Classic IBM mainframe scripting language, also available on Unix.

```rexx
/* REXX program */
SAY "Hello from REXX!"
name = "Alice"
SAY "Welcome," name"!"

/* DO loop */
DO i = 1 TO 5
    SAY "Count:" i
END

/* Subroutine */
CALL Greet "World"

/* Built-in functions */
msg = "Hello World"
SAY "Length:" LENGTH(msg)
SAY "Upper:" UPPER(msg)
SAY "Words:" WORDS(msg)
EXIT

Greet:
    ARG who
    SAY "Hello," who"!"
    RETURN
```

**File extension:** `.rex`

---

## Haskell

Pure functional language with strong static typing.

```haskell
-- Functions
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- List operations
evens = filter even [1..20]
squares = map (^2) [1..10]
sumList = foldl (+) 0

-- Main
main :: IO ()
main = do
    putStrLn "Hello from Haskell!"
    print (map factorial [1..8])
    putStrLn $ "Evens up to 20: " ++ show evens
    putStrLn $ "Sum 1..100 = " ++ show (sumList [1..100])

    -- Where clause
    let result = compute 5 3
    print result
  where
    compute x y = x^2 + 2*x*y + y^2
```

**File extension:** `.hs`

---

## Scheme

Minimalist Lisp dialect emphasizing functional purity and tail calls.

```scheme
;; Define and call functions
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(display (factorial 10)) (newline)

;; Higher-order functions
(define nums '(1 2 3 4 5 6 7 8 9 10))
(display (filter odd? nums)) (newline)
(display (map (lambda (x) (* x x)) nums)) (newline)

;; Let binding and recursion
(let loop ((i 0) (a 0) (b 1))
  (when (< i 10)
    (display a) (display " ")
    (loop (+ i 1) b (+ a b))))
(newline)

;; Named let
(define (sum-to n)
  (let loop ((i n) (acc 0))
    (if (= i 0) acc (loop (- i 1) (+ acc i)))))
(display (sum-to 100)) (newline)
```

**File extension:** `.scm`

---

## Smalltalk

The original object-oriented language; everything is an object.

```smalltalk
| x coll |
Transcript showCr: 'Hello from Smalltalk!'.

"Arithmetic"
x := 6 factorial.
Transcript showCr: '6! = ' , x printString.

"Collection"
coll := OrderedCollection new.
1 to: 5 do: [:i | coll add: i * i].
Transcript showCr: 'Squares: ' , coll printString.

"Conditionals"
| n |
n := 42.
(n even)
    ifTrue:  [Transcript showCr: n printString , ' is even']
    ifFalse: [Transcript showCr: n printString , ' is odd'].

"Block as value"
| adder |
adder := [:a :b | a + b].
Transcript showCr: 'adder(3,4) = ' , (adder value: 3 value: 4) printString.
```

**File extension:** `.st`

---

## APL

Array-oriented language with unique mathematical symbols.

```apl
⍝ APL Examples
⍝ Simple arithmetic
2 + 3
10 × 5

⍝ Array operations
1 2 3 4 5
+/ 1 2 3 4 5        ⍝ Sum: 15
×/ 1 2 3 4 5        ⍝ Product: 120
⌈/ 3 1 4 1 5 9 2 6  ⍝ Max: 9
⌊/ 3 1 4 1 5 9 2 6  ⍝ Min: 1

⍝ Iota (range)
⍳10

⍝ Outer product
2 3 4 ∘.× 1 2 3
```

**File extension:** `.apl`

---

## COBOL

Business-oriented language, dominant in financial and government systems.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME       PIC X(20) VALUE 'World'.
       01 WS-COUNTER    PIC 9(3)  VALUE 0.
       01 WS-TOTAL      PIC 9(6)  VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'Hello, ' WS-NAME '!'.

           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 5
               DISPLAY 'Count: ' WS-COUNTER
           END-PERFORM.

           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 100
               ADD WS-COUNTER TO WS-TOTAL
           END-PERFORM.
           DISPLAY 'Sum 1..100 = ' WS-TOTAL.

           STOP RUN.
```

**File extension:** `.cob`

---

## Fortran

The first high-level programming language; excels at numeric computing.

```fortran
PROGRAM HELLO
    IMPLICIT NONE
    INTEGER :: i, n
    REAL :: sum, x

    PRINT *, 'Hello from Fortran!'

    ! Sum 1 to 10
    sum = 0.0
    DO i = 1, 10
        sum = sum + i
    END DO
    PRINT *, 'Sum 1..10 =', sum

    ! Factorial function
    n = 10
    PRINT *, n, '! =', factorial(n)

    CONTAINS

    INTEGER FUNCTION factorial(n)
        INTEGER, INTENT(IN) :: n
        INTEGER :: k
        factorial = 1
        DO k = 2, n
            factorial = factorial * k
        END DO
    END FUNCTION factorial

END PROGRAM HELLO
```

**File extension:** `.f90`

---

## Assembly

x86 assembly language — closest to the hardware.

```asm
; x86 Assembly - Hello World
section .data
    msg db 'Hello from Assembly!', 0x0A
    len equ $ - msg

section .text
    global _start

_start:
    ; Write to stdout
    mov eax, 4        ; sys_write
    mov ebx, 1        ; stdout
    mov ecx, msg      ; buffer
    mov edx, len      ; length
    int 0x80

    ; Exit
    mov eax, 1        ; sys_exit
    xor ebx, ebx      ; return 0
    int 0x80
```

**File extension:** `.asm`

---

## HyperTalk

Apple HyperCard's English-like scripting language (1987).

```hypertalk
-- HyperTalk Hello World
put "Hello from HyperTalk!" into greeting
answer greeting

-- Variables and arithmetic
put 10 into x
put 3 into y
put x + y into result
put "x + y = " & result

-- Repeat loop
repeat with i = 1 to 5
    put "Count: " & i
end repeat

-- If statement
if x > 5 then
    put x & " is greater than 5"
else
    put x & " is not greater than 5"
end if
```

**File extension:** `.ht`

---

## JCL

IBM Job Control Language — controls batch job execution on mainframes.

```jcl
//TWJOB01  JOB (ACCT),'HELLO WORLD',CLASS=A,MSGCLASS=X
//*
//* Simple JCL job step
//*
//STEP01   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DUMMY
//SYSUT1   DD  *
Hello from JCL!
Time Warp Studio Mainframe Demo
/*
//SYSUT2   DD  SYSOUT=*
```

Key JCL statements:
- `//jobname JOB` — defines a job
- `//stepname EXEC PGM=` — executes a program
- `//ddname DD` — defines a dataset (file)
- `SYSOUT=*` — print output to console
- `/*` — end of in-stream data

**File extension:** `.jcl`

---

## CICS

IBM CICS (Customer Information Control System) — mainframe transaction processing.

```cics
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOCICS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MSG PIC X(40) VALUE 'Hello from CICS!'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           EXEC CICS SEND TEXT
               FROM(WS-MSG)
               ERASE
           END-EXEC

           DISPLAY 'Transaction complete.'.
           EXEC CICS RETURN END-EXEC
           STOP RUN.
```

Key CICS commands:
- `EXEC CICS SEND TEXT` — send text to terminal
- `EXEC CICS RECEIVE MAP` — receive BMS map input
- `EXEC CICS READ FILE` — read VSAM record
- `EXEC CICS RETURN` — end transaction
- `EXEC CICS ASSIGN` — get system information

**File extension:** `.cics`

---

## SQL

Structured Query Language for relational databases.

```sql
-- Create and populate a table
CREATE TABLE students (
    id    INT PRIMARY KEY,
    name  VARCHAR(50),
    grade INT
);

INSERT INTO students VALUES (1, 'Alice', 95);
INSERT INTO students VALUES (2, 'Bob',   82);
INSERT INTO students VALUES (3, 'Carol', 91);
INSERT INTO students VALUES (4, 'Dave',  74);

-- Query with ORDER BY
SELECT name, grade FROM students ORDER BY grade DESC;

-- Aggregate functions
SELECT AVG(grade) AS average,
       MAX(grade) AS highest,
       MIN(grade) AS lowest
FROM students;

-- Conditional (CASE)
SELECT name,
    CASE WHEN grade >= 90 THEN 'A'
         WHEN grade >= 80 THEN 'B'
         ELSE 'C'
    END AS letter
FROM students;
```

**File extension:** `.sql`

---

## SQR

Structured Query Reporter — Oracle/PeopleSoft report-generation language.

```sqr
begin-program
    do main
end-program

begin-procedure main
    display 'Hello from SQR!'

    let $name = 'World'
    display 'Hello, ' noline
    display $name

    let #total = 0
    let #i = 1
    while #i <= 10
        let #total = #total + #i
        let #i = #i + 1
    end-while

    display 'Sum 1..10 = ' noline
    display #total

    do print-report
end-procedure

begin-procedure print-report
    print 'SQR Report' (+1, 1)
    print '----------' (+1, 1)
    print 'Time Warp Studio' (+1, 1)
end-procedure
```

**File extension:** `.sqr`

---

## Brainfuck

Esoteric Turing-complete language with 8 commands. Ideal for understanding computation fundamentals.

| Command | Meaning |
|---------|---------|
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

> **Tip:** The execution limit is 1,000,000,000 steps to accommodate complex programs.

**File extension:** `.bf`

---

## Language Selection

To select which language your code uses:

1. **File Extension**: Save with correct extension

| Extension | Language |
|-----------|----------|
| `.bas` | BASIC |
| `.logo` | Logo |
| `.pilot` | PILOT |
| `.c` | C |
| `.pas` | Pascal |
| `.pro` | Prolog |
| `.f` or `.4th` | Forth |
| `.py` | Python |
| `.js` | JavaScript |
| `.lua` | Lua |
| `.rex` | REXX |
| `.hs` | Haskell |
| `.scm` | Scheme |
| `.st` | Smalltalk |
| `.apl` | APL |
| `.cob` or `.cobol` | COBOL |
| `.f90` or `.for` | Fortran |
| `.asm` or `.s` | Assembly |
| `.jcl` | JCL |
| `.cics` | CICS |
| `.sql` | SQL |
| `.sqr` | SQR |
| `.ht` | HyperTalk |
| `.bf` | Brainfuck |

2. **Menu Selection**: Use the Language dropdown in the toolbar

---

**For more help:**
- [USER_GUIDE.md](USER_GUIDE.md) - IDE usage
- [DEBUGGER_GUIDE.md](DEBUGGER_GUIDE.md) - Debugging
- [TURTLE_GRAPHICS.md](TURTLE_GRAPHICS.md) - Graphics tutorial
- [FAQ.md](FAQ.md) - Common questions
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Problem solving
