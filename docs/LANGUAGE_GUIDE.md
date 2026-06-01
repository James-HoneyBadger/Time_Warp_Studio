# Language Guide - Time Warp Studio

Complete reference for all **24 programming languages** supported by Time Warp Studio.

---

## Table of Contents

### Classic Educational
1. [BASIC](#basic)
2. [Logo](#logo)
3. [PILOT](#pilot)

### Systems & Structured
4. [C](#c)
5. [Pascal](#pascal)
6. [6502 Assembly](#6502-assembly)

### Functional & Declarative
7. [Prolog](#prolog)
8. [LISP/Scheme](#lispscheme)
9. [Haskell](#haskell)

### Stack & Concatenative
10. [Forth](#forth)
11. [PostScript](#postscript)

### Scripting & General-Purpose
12. [JavaScript](#javascript)
13. [Lua](#lua)
14. [Python](#python)
15. [Ruby](#ruby)
16. [Perl 5](#perl-5)
17. [Tcl](#tcl)
18. [REXX](#rexx)

### Object-Oriented / Message-Passing
19. [Smalltalk](#smalltalk)

### Concurrent & Functional
20. [Erlang](#erlang)

### Event-Driven
21. [HyperTalk](#hypertalk)

### Business / Data Processing
22. [COBOL](#cobol)

### Array / Symbolic
23. [APL](#apl)

### Esoteric
24. [Brainfuck](#brainfuck)

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

## Erlang

Concurrent functional language (1986) with actor-model message passing.

```erlang
-module(hello).
-export([start/0]).

start() ->
    io:format("Hello from Erlang!~n"),
    Pid = spawn(fun worker/0),
    Pid ! {self(), "ping"},
    receive
        {_, Msg} -> io:format("Received: ~s~n", [Msg])
    end.

worker() ->
    receive
        {From, "ping"} -> From ! {self(), "pong"}
    end.
```

**File extension:** `.erl`

---

## LISP/Scheme

Classic 1958 AI / symbolic computation language. Time Warp Studio implements a Scheme-compatible subset.

```scheme
; Hello World
(display "Hello, World!")
(newline)

; Variables
(define x 42)
(define name "Alice")

; Arithmetic
(display (+ 1 2 3))      ; 6
(display (* 4 5))         ; 20

; Lambda / closures
(define square (lambda (n) (* n n)))
(display (square 7))      ; 49

; Recursion
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))
(display (factorial 6))   ; 720

; Lists
(define lst '(1 2 3 4 5))
(display (car lst))       ; 1
(display (cdr lst))       ; (2 3 4 5)
(display (length lst))    ; 5

; Map / filter
(display (map (lambda (x) (* x x)) '(1 2 3 4 5)))
; (1 4 9 16 25)
```

**File extensions:** `.lisp` / `.scm`

---

## COBOL

Business-oriented language (1959) designed for data processing and report generation.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER PIC 9(3) VALUE 0.
       01 NAME    PIC X(20) VALUE "World".

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Hello, " NAME
           PERFORM VARYING COUNTER FROM 1 BY 1
               UNTIL COUNTER > 5
               DISPLAY "Count: " COUNTER
           END-PERFORM
           STOP RUN.
```

**Key clauses:**
- `IDENTIFICATION DIVISION` — program name
- `DATA DIVISION` — variable declarations with `PIC` pictures
- `PROCEDURE DIVISION` — executable statements
- `PERFORM ... VARYING` — counted loop
- `MOVE`, `ADD`, `SUBTRACT`, `MULTIPLY`, `DIVIDE` — arithmetic verbs

**File extensions:** `.cob` / `.cobol`

---

## Tcl

Tool Command Language (1988) — a dynamic scripting and embeddable extension language.

```tcl
# Hello World
puts "Hello, World!"

# Variables
set x 42
set name "Alice"
puts "Name: $name, x = $x"

# Arithmetic
set result [expr {$x * 2 + 10}]
puts "Result: $result"

# Lists
set lst {1 2 3 4 5}
puts "Length: [llength $lst]"
puts "First: [lindex $lst 0]"

# Control flow
if {$x > 10} {
    puts "Large"
} else {
    puts "Small"
}

# Loop
for {set i 1} {$i <= 5} {incr i} {
    puts "i = $i"
}

# Procedures
proc factorial {n} {
    if {$n <= 1} { return 1 }
    return [expr {$n * [factorial [expr {$n - 1}]]}]
}
puts [factorial 6]  ;# 720
```

**File extension:** `.tcl`

---

## PostScript

Stack-based page-description language (1982) originally created by Adobe for typesetting and graphics.

```postscript
%!PS
% Hello World
/Helvetica findfont 24 scalefont setfont
100 700 moveto
(Hello, World!) show
showpage

% Stack arithmetic
3 4 add   % stack: 7
2 mul      % stack: 14

% Drawing
100 100 moveto
200 200 lineto
stroke

% Loops
1 1 5 {
  dup 72 mul  % i * 72
  100 exch moveto
  (•) show
} for
showpage
```

**Key concepts:** stack-based (operands precede operators), `/name` names, `def` for definitions, `{...}` code blocks.

**File extension:** `.ps`

---

## Ruby

Dynamic, expressive object-oriented scripting language (1995).

```ruby
# Hello World
puts "Hello, World!"

# Variables
x = 42
name = "Alice"

# String interpolation
puts "Hello, #{name}! x = #{x}"

# Arrays and ranges
arr = [1, 2, 3, 4, 5]
puts arr.map { |n| n * n }.inspect  # [1, 4, 9, 16, 25]
puts (1..5).sum                      # 15

# Hashes
person = { name: "Bob", age: 30 }
puts "#{person[:name]} is #{person[:age]}"

# Blocks and iterators
[1, 2, 3].each { |i| puts i }
5.times { |i| puts "count: #{i}" }

# Methods
def factorial(n)
  n <= 1 ? 1 : n * factorial(n - 1)
end
puts factorial(6)  # 720

# Classes
class Animal
  attr_reader :name
  def initialize(name)
    @name = name
  end
  def speak
    "..."
  end
end

class Dog < Animal
  def speak
    "Woof!"
  end
end

d = Dog.new("Rex")
puts "#{d.name} says #{d.speak}"
```

**File extension:** `.rb`

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

## Haskell

Pure, lazy functional language (1990) with strong static typing.

```haskell
-- Hello World
main = putStrLn "Hello, World!"

-- Variables (immutable bindings)
x = 42
name = "Alice"

-- Functions
square :: Int -> Int
square n = n * n

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- List operations
main = do
  print (map square [1..5])        -- [1,4,9,16,25]
  print (filter even [1..10])      -- [2,4,6,8,10]
  print (foldl (+) 0 [1..10])      -- 55
  print (factorial 6)              -- 720

-- List comprehensions
pythagorean = [(a,b,c) | c <- [1..20], a <- [1..c], b <- [a..c],
                          a^2 + b^2 == c^2]

-- Pattern matching
describeList :: [a] -> String
describeList [] = "empty"
describeList [_] = "singleton"
describeList _ = "longer list"
```

**File extension:** `.hs`

---

## 6502 Assembly

Machine-level programming for the MOS 6502 processor (1975) — the CPU in the Apple II, Commodore 64, and NES.

```asm
; Hello World - 6502 Assembly
    .org $0600

START:
    LDX #0          ; X = 0 (index)
LOOP:
    LDA MSG,X       ; load byte from MSG+X
    BEQ DONE        ; if zero, exit
    JSR PRINT_CHAR  ; print character in A
    INX             ; X++
    JMP LOOP        ; repeat
DONE:
    BRK             ; stop

MSG: .byte "Hello, World!", 0

PRINT_CHAR:
    STA $F001       ; write to output port
    RTS

; Registers: A (accumulator), X/Y (index), SP (stack pointer)
; Flags: N V - B D I Z C
; Addressing modes:
;   Immediate  LDA #$42      -- load literal
;   Zero page  LDA $10       -- fast memory access
;   Absolute   LDA $1234     -- full address
;   Indexed    LDA MSG,X     -- array indexing
;   Indirect   JMP ($FFFC)   -- vector jump
```

**Key instructions:**
- `LDA/LDX/LDY` — load register
- `STA/STX/STY` — store register
- `ADC/SBC` — add/subtract with carry
- `INX/INY/DEX/DEY` — increment/decrement index
- `JMP/JSR/RTS` — jump / call / return
- `BEQ/BNE/BCC/BCS/BMI/BPL` — branch on flag

**File extensions:** `.asm` / `.s`

---

## Perl 5

Practical scripting language (1987) with exceptional text-processing and regex capabilities.

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Hello World
print "Hello, World!\n";

# Variables
my $x = 42;
my $name = "Alice";
my @arr = (1, 2, 3, 4, 5);
my %hash = (name => "Bob", age => 30);

# String interpolation
print "Hello, $name! x = $x\n";

# Regular expressions
my $text = "The quick brown fox";
if ($text =~ /(\w+) fox/) {
    print "Adjective: $1\n";   # brown
}
(my $modified = $text) =~ s/fox/cat/;
print "$modified\n";

# Array operations
my @squares = map { $_ * $_ } @arr;
my @evens   = grep { $_ % 2 == 0 } @arr;
print join(", ", @squares), "\n";  # 1, 4, 9, 16, 25

# Subroutines
sub factorial {
    my ($n) = @_;
    return $n <= 1 ? 1 : $n * factorial($n - 1);
}
print factorial(6), "\n";  # 720

# Hash operations
print "Name: $hash{name}, Age: $hash{age}\n";
foreach my $key (sort keys %hash) {
    print "$key => $hash{$key}\n";
}
```

**File extensions:** `.pl` / `.perl`

---

## REXX

IBM Restructured eXtended eXecutor (1979) — a highly readable procedural scripting language.

```rexx
/* Hello World */
SAY "Hello, World!"

/* Variables - no declaration needed */
name = "Alice"
x = 42
SAY "Hello" name || "! x =" x

/* Arithmetic */
SAY 2 ** 10          /* 1024 */
SAY 17 // 5          /* 2 (modulo) */

/* Control flow */
IF x > 10 THEN
    SAY "Large"
ELSE
    SAY "Small"

/* DO loop */
DO i = 1 TO 5
    SAY "Count:" i
END

/* SELECT (switch) */
SELECT
    WHEN x < 0 THEN SAY "negative"
    WHEN x = 0 THEN SAY "zero"
    OTHERWISE SAY "positive"
END

/* Subroutines */
result = double(21)
SAY result           /* 42 */

double: PROCEDURE
    PARSE ARG n
    RETURN n * 2

/* String builtins */
SAY LENGTH("Hello")  /* 5 */
SAY UPPER("hello")   /* HELLO */
SAY SUBSTR("Hello, World!", 8, 5)  /* World */
SAY WORD("one two three", 2)       /* two */
```

**File extensions:** `.rexx` / `.rex` / `.rxx`

---

## Smalltalk

Smalltalk-80 (1980) — the definitive object-oriented message-passing language that inspired Java, Ruby, and Python.

```smalltalk
"Hello World"
Transcript showCr: 'Hello, World!'.

"Variables (temp declarations)"
| x name result |
x := 42.
name := 'Alice'.
Transcript showCr: 'Hello, ', name, '! x = ', x printString.

"Arithmetic"
Transcript showCr: (3 + 4) printString.       "7"
Transcript showCr: (2 raisedTo: 10) printString. "1024"
Transcript showCr: 6 factorial printString.   "720"

"Conditionals"
(x > 10)
    ifTrue: [ Transcript showCr: 'Large' ]
    ifFalse: [ Transcript showCr: 'Small' ].

"Loops"
1 to: 5 do: [ :i |
    Transcript showCr: i printString.
].

5 timesRepeat: [ Transcript showCr: 'Hello' ].

"Collections"
| col |
col := OrderedCollection new.
col add: 1.
col add: 2.
col add: 3.
Transcript showCr: col printString.

"Block closures"
| sum |
sum := 0.
#(1 2 3 4 5) do: [ :each | sum := sum + each ].
Transcript showCr: sum printString.  "15"

"select: / collect: / inject:into:"
| evens squares total |
evens   := #(1 2 3 4 5) select: [ :n | n \\ 2 = 0 ].
squares := #(1 2 3 4 5) collect: [ :n | n * n ].
total   := #(1 2 3 4 5) inject: 0 into: [ :acc :n | acc + n ].
Transcript showCr: squares printString.  "(1 4 9 16 25 )"
```

**File extension:** `.st`

---

## APL

A Programming Language (1966) — terse, symbolic, array-oriented. Right-to-left evaluation, no operator precedence.

```apl
⍝ Hello World
⎕← 'Hello, World!'

⍝ Variables (← assignment)
x ← 42
v ← 1 2 3 4 5

⍝ Arithmetic (scalar extension)
⎕← v + 10      ⍝ 11 12 13 14 15
⎕← v × v       ⍝ 1 4 9 16 25

⍝ Iota: generate sequence
⎕← ⍳5          ⍝ 1 2 3 4 5

⍝ Shape and reshape
⎕← ⍴v          ⍝ 5
m ← 2 3 ⍴ ⍳6   ⍝ 2×3 matrix
⎕← m

⍝ Reduction (fold)
⎕← +/ ⍳10      ⍝ 55  (sum 1..10)
⎕← ×/ ⍳5       ⍝ 120 (5!)
⎕← ⌈/ 3 1 4 1 5  ⍝ 5 (maximum)

⍝ Scan (running cumulative)
⎕← +\ ⍳5       ⍝ 1 3 6 10 15

⍝ Array operations
⎕← ⌽ ⍳5        ⍝ 5 4 3 2 1 (reverse)
⎕← 3 ↑ ⍳10     ⍝ 1 2 3     (take)
⎕← 7 ↓ ⍳10     ⍝ 8 9 10    (drop)

⍝ Inner product
⎕← (1 2 3) +.× (4 5 6)  ⍝ 32

⍝ Comparisons (return 0/1)
⎕← (⍳5) = 3     ⍝ 0 0 1 0 0
⎕← (⍳5) > 3     ⍝ 0 0 0 1 1

⍝ Negative literals use ¯ (high-minus)
⎕← ¯5 + 3       ⍝ ¯2
```

**Monadic functions:** `⍳` (iota), `⍴` (shape), `⌽` (reverse), `⍋`/`⍒` (grade), `~` (not), `|` (abs), `⌈` (ceiling), `⌊` (floor)

**Dyadic functions:** `⍴` (reshape), `,` (catenate), `↑`/`↓` (take/drop), `∊` (member), `∧`/`∨` (and/or), `+.×` (inner product)

**File extension:** `.apl`

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
| `.asm` / `.s` | 6502 Assembly |
| `.pro` / `.pl` / `.prolog` | Prolog |
| `.lisp` / `.scm` | LISP/Scheme |
| `.hs` | Haskell |
| `.f` / `.fs` / `.forth` | Forth |
| `.ps` | PostScript |
| `.js` | JavaScript |
| `.lua` | Lua |
| `.py` | Python |
| `.rb` | Ruby |
| `.pl` / `.perl` | Perl 5 |
| `.tcl` | Tcl |
| `.rexx` / `.rex` / `.rxx` | REXX |
| `.st` | Smalltalk |
| `.erl` | Erlang |
| `.htalk` | HyperTalk |
| `.cob` / `.cobol` | COBOL |
| `.apl` | APL |
| `.bf` | Brainfuck |

2. **Menu Selection**: Use the Language dropdown in the toolbar

---

**For more help:**
- [USER_GUIDE.md](USER_GUIDE.md) - IDE usage
- [DEBUGGER_GUIDE.md](DEBUGGER_GUIDE.md) - Debugging
- [TURTLE_GRAPHICS.md](TURTLE_GRAPHICS.md) - Graphics tutorial
- [FAQ.md](FAQ.md) - Common questions
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Problem solving
