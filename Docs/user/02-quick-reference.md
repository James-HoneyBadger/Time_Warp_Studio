# Quick Reference Guide

Fast lookup for commands, syntax, and features in all six languages.

---

## BASIC Commands

### Output
```basic
PRINT "text"              # Display text
PRINT variable            # Display variable
PRINT "x="; X             # Mix text and variables
```

### Variables
```basic
LET X = 10                # Number variable
LET NAME$ = "Alice"       # String variable ($ suffix)
INPUT "prompt"; VAR       # Get user input
```

### Math
```basic
X + Y                     # Addition
X - Y                     # Subtraction
X * Y                     # Multiplication
X / Y                     # Division
X MOD Y                   # Remainder
X ^ Y                     # Exponentiation
```

### Control Flow
```basic
IF condition THEN statement
IF X > 0 THEN PRINT "Positive"

FOR I = 1 TO 10
  PRINT I
NEXT I

FOR I = 10 TO 1 STEP -1   # Count down
  PRINT I
NEXT I

WHILE condition
  statements
WEND
```

### Subroutines
```basic
GOSUB 100                 # Call subroutine
GOTO 50                   # Jump to line
END                       # End program

100 REM Subroutine
110 PRINT "Hello"
120 RETURN
```

---

## PILOT Commands

### Text Output
```pilot
T:Text to display
T:Hello, $variable        # Display with variable
```

### Input
```pilot
A:varname                 # Accept input
A:age                     # Store in 'age'
T:You are $age years old  # Use with $
```

### Math
```pilot
C:x = 10                  # Compute/assign
C:sum = a + b
C:result = (x + y) * 2
```

### Matching (Conditionals)
```pilot
M:expected_value          # Match last input
T:Correct!
N:                        # If No match
T:Try again
```

### Jumps and Labels
```pilot
J:label                   # Jump to label
*label                    # Define label
T:This comes after jump
```

### Special
```pilot
E:                        # End program
```

---

## Logo Commands

### Turtle Movement
```logo
FORWARD 100               # Move forward (FD)
BACK 50                   # Move backward (BK)
RIGHT 90                  # Turn right (RT)
LEFT 45                   # Turn left (LT)
```

### Pen Control
```logo
PENUP                     # Lift pen (PU)
PENDOWN                   # Lower pen (PD)
PENSIZE 5                 # Set thickness
SETPENCOLOR 1             # Set color (1-16)
```

### Colors
```
1=Red    2=Green   3=Blue     4=Yellow
5=Magenta 6=Cyan   7=White    8=Black
9=Orange  10=Purple 11=Pink   12=Brown
```

### Screen
```logo
CLEARSCREEN               # Clear and reset (CS)
HOME                      # Return to center
HIDETURTLE                # Hide turtle (HT)
SHOWTURTLE                # Show turtle (ST)
```

### Loops
```logo
REPEAT 4 [FORWARD 100 RIGHT 90]
```

### Procedures
```logo
TO SQUARE
  REPEAT 4 [FORWARD 100 RIGHT 90]
END

TO SQUARE :SIZE           # With parameter
  REPEAT 4 [FORWARD :SIZE RIGHT 90]
END

SQUARE 50                 # Call with argument
```

### Variables & Math
```logo
MAKE "X 10                # Create variable
PRINT :X                  # Print variable
MAKE "Y :X + 5            # Math with variables
```

### Lists
```logo
MAKE "COLORS [RED GREEN BLUE]
PRINT FIRST :COLORS       # RED
PRINT LAST :COLORS        # BLUE
PRINT COUNT :COLORS       # 3
```

---

## Pascal Commands

### Program Structure
```pascal
program ProgramName;
var
  variable: type;
begin
  statements;
end.
```

### Variables
```pascal
var
  age: integer;
  name: string;
  price: real;
  done: boolean;
begin
  age := 25;              # Assignment uses :=
  name := 'Alice';
end.
```

### Input/Output
```pascal
write('prompt: ');        # Output without newline
writeln('text');          # Output with newline
readln(variable);         # Read input
```

### Control Flow
```pascal
if condition then
  statement
else
  statement;

case variable of
  value1: statement;
  value2: statement;
else
  statement;
end;

for i := 1 to 10 do
  statement;

for i := 10 downto 1 do   # Count down
  statement;

while condition do
  statement;

repeat
  statement;
until condition;
```

### Procedures & Functions
```pascal
procedure Name(param: type);
begin
  statements;
end;

function Name(param: type): returntype;
begin
  Name := value;          # Return value
end;
```

---

## Prolog Predicates

### Facts
```prolog
parent(tom, bob).         # Tom is parent of Bob
likes(mary, pizza).       # Mary likes pizza
```

### Rules
```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
```

### Queries
```prolog
?- parent(tom, bob).      # Is Tom parent of Bob?
yes

?- parent(tom, Who).      # Who are Tom's children?
Who = bob
Who = liz
```

### Lists
```prolog
[1, 2, 3]                 # List
[H|T]                     # Head and Tail pattern
[]                        # Empty list

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], L, L).
append([H|T], L2, [H|L3]) :- append(T, L2, L3).
```

### Built-in Predicates
```prolog
member(X, List)           # X is in List
append(L1, L2, L3)        # L3 = L1 + L2
length(List, N)           # N = length of List
is(X, Expression)         # X = result of Expression
```

---

## C Commands

### Program Structure
```c
#include <stdio.h>

int main() {
    statements;
    return 0;
}
```

### Variables
```c
int age = 25;
float price = 19.99;
char grade = 'A';
char name[50] = "Alice";
```

### Input/Output
```c
printf("text");
printf("x=%d\n", x);      # %d for int
printf("%.2f\n", f);      # %f for float
scanf("%d", &variable);   # Read input (note &)
```

### Control Flow
```c
if (condition) {
    statements;
} else {
    statements;
}

switch (variable) {
    case value1:
        statements;
        break;
    default:
        statements;
}

for (int i = 0; i < 10; i++) {
    statements;
}

while (condition) {
    statements;
}

do {
    statements;
} while (condition);
```

### Functions
```c
int functionName(int param) {
    return value;
}
```

### Arrays
```c
int numbers[5] = {1, 2, 3, 4, 5};
numbers[0]                # First element
numbers[4]                # Last element
```

---

## IDE Keyboard Shortcuts

### File Operations
- `Ctrl+N` - New file
- `Ctrl+O` - Open file
- `Ctrl+S` - Save file
- `Ctrl+Shift+S` - Save As

### Editing
- `Ctrl+Z` - Undo
- `Ctrl+Y` - Redo
- `Ctrl+X` - Cut
- `Ctrl+C` - Copy
- `Ctrl+V` - Paste
- `Ctrl+F` - Find
- `Ctrl+H` - Replace
- `Ctrl+A` - Select All

### Running
- `F5` - Run program
- `Shift+F5` - Stop execution
- `Ctrl+F5` - Run without debugging

### View
- `Ctrl++` - Zoom in
- `Ctrl+-` - Zoom out
- `Ctrl+0` - Reset zoom
- `F11` - Full screen

---

## Common Patterns

### Counting Loop
```basic
FOR I = 1 TO 10
  PRINT I
NEXT I
```

```logo
REPEAT 10 [PRINT :COUNT]
```

```pascal
for i := 1 to 10 do
  writeln(i);
```

```c
for (int i = 1; i <= 10; i++) {
    printf("%d\n", i);
}
```

### User Input
```basic
INPUT "Enter name: "; NAME$
```

```pilot
T:Enter name:
A:name
```

```pascal
write('Enter name: ');
readln(name);
```

```c
printf("Enter name: ");
scanf("%s", name);
```

### Draw Square
```logo
REPEAT 4 [FORWARD 100 RIGHT 90]
```

```basic
10 FOR I = 1 TO 4
20   GOSUB 100
30   TURN 90
40 NEXT I
50 END
100 FORWARD 100
110 RETURN
```

---

## Error Messages

### Common BASIC Errors
- `SYNTAX ERROR` - Typo or incorrect command
- `TYPE MISMATCH` - Wrong variable type
- `UNDEFINED VARIABLE` - Variable not initialized
- `DIVISION BY ZERO` - Cannot divide by zero

### Common Logo Errors
- `NOT ENOUGH INPUTS` - Missing parameter
- `TOO MANY INPUTS` - Extra parameter
- `PROCEDURE NOT DEFINED` - Unknown procedure

### Common Pascal Errors
- `UNDECLARED IDENTIFIER` - Variable not declared
- `TYPE MISMATCH` - Wrong type assignment
- `SYNTAX ERROR` - Missing semicolon or keyword

---

## Tips & Tricks

### BASIC
- Use `REM` for comments
- Line numbers in increments of 10 allow insertions
- `LET` is optional: `X = 5` works

### Logo
- Use `;` for comments
- Abbreviations: `FD`=FORWARD, `RT`=RIGHT, `PU`=PENUP
- Test procedures in immediate mode first

### Pascal
- Every statement (except before `end`) needs `;`
- Use `:=` for assignment, `=` for comparison
- Declare all variables before `begin`

### Prolog
- Facts end with `.`
- Rules use `:-`
- Variables start with uppercase
- Use `_` for "don't care" values

### C
- Remember `&` in `scanf()`
- Don't forget `break` in switch
- Array indices start at 0
- Always `return 0;` from main

---

## Next Steps

- **Full tutorials**: [Programming Guide](01-programming-guide.md)
- **Examples**: Check `examples/` folder
- **Help**: [User Manual](00-user-manual.md)

---

*Quick Reference v4.0.0 - November 2025*
