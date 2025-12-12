# Programming Guide

Learn to program in Time Warp IDE. The core languages are BASIC, PILOT, and Logo. Pascal, Prolog, and C are available as experimental modules and may not be present or fully featured in all builds.

---

## Table of Contents

1. [BASIC Programming](#basic-programming)
2. [PILOT Programming](#pilot-programming)
3. [Logo Programming](#logo-programming)
4. [Pascal Programming](#pascal-programming) (Experimental)
5. [Prolog Programming](#prolog-programming) (Experimental)
6. [C Programming](#c-programming) (Experimental)
7. [Choosing a Language](#choosing-a-language)
8. [Cross-Language Concepts](#cross-language-concepts)

---

## BASIC Programming

### Introduction to BASIC

BASIC (Beginner's All-purpose Symbolic Instruction Code) was designed in 1964 to make programming accessible to everyone. It uses English-like commands and line numbers.

### Essential Commands

**Output**
```basic
10 PRINT "Hello, World!"
20 PRINT "The answer is"; 42
```

**Variables**
```basic
10 LET X = 10
20 LET NAME$ = "Alice"
30 PRINT NAME$; " has"; X; " apples"
```

Variable names:
- End with `$` for strings: `NAME$`, `CITY$`
- Numbers don't need a suffix: `X`, `TOTAL`, `COUNT`

**Input**
```basic
10 INPUT "Enter your age: "; AGE
20 PRINT "You are"; AGE; "years old"
```

**Math Operations**
```basic
10 LET A = 5
20 LET B = 3
30 PRINT "Sum:"; A + B
40 PRINT "Difference:"; A - B
50 PRINT "Product:"; A * B
60 PRINT "Quotient:"; A / B
70 PRINT "Remainder:"; A MOD B
```

### Control Flow

**IF Statements**
```basic
10 INPUT "Enter a number: "; N
20 IF N > 0 THEN PRINT "Positive"
30 IF N < 0 THEN PRINT "Negative"
40 IF N = 0 THEN PRINT "Zero"
```

**FOR Loops**
```basic
10 REM Count to 10
20 FOR I = 1 TO 10
30   PRINT I
40 NEXT I
```

**WHILE Loops**
```basic
10 LET X = 1
20 WHILE X <= 5
30   PRINT X
40   LET X = X + 1
50 WEND
```

### Subroutines

```basic
10 GOSUB 100
20 PRINT "Back to main"
30 END
100 REM Subroutine
110 PRINT "In subroutine"
120 RETURN
```

### Complete Example: Guessing Game

```basic
10 REM Number Guessing Game
20 LET SECRET = INT(RND * 100) + 1
30 LET GUESSES = 0
40 PRINT "I'm thinking of a number between 1 and 100"
50 INPUT "Your guess: "; GUESS
60 LET GUESSES = GUESSES + 1
70 IF GUESS = SECRET THEN GOTO 120
80 IF GUESS < SECRET THEN PRINT "Too low!"
90 IF GUESS > SECRET THEN PRINT "Too high!"
100 GOTO 50
120 PRINT "Correct! You got it in"; GUESSES; "guesses!"
130 END
```

### BASIC Best Practices

1. **Use descriptive variable names**: `SCORE` instead of `S`
2. **Add comments with REM**: Explain what your code does
3. **Use consistent line numbering**: Increment by 10 (allows insertion)
4. **Group related code**: Keep subroutines together
5. **Test incrementally**: Run after adding each feature

---

## PILOT Programming

### Introduction to PILOT

PILOT (Programmed Inquiry, Learning, Or Teaching) was created in 1968 specifically for computer-aided instruction. It's designed to be readable by non-programmers.

### Essential Commands

**Text Output**
```pilot
T:Hello, World!
T:This is a second line
```

**Accepting Input**
```pilot
A:name
T:Hello, $name!
```

Variables are referenced with `$`:
```pilot
A:age
T:You are $age years old
```

**Math Operations**
```pilot
C:x = 10
C:y = 20
C:sum = x + y
T:The sum is $sum
```

### Conditional Logic

**Match Command**
```pilot
T:What is 2 + 2?
A:answer
M:4
T:Correct!
N:
T:Sorry, try again!
```

- `M:` matches if answer equals the value
- `N:` executes if previous match failed

**Yes/No Questions**
```pilot
T:Do you like programming? (Y/N)
A:response
MY:
T:Great! Let's continue learning.
MN:
T:That's okay, give it a chance!
```

### Loops and Jumps

**Labels and Jumps**
```pilot
T:Starting program
J:section2
T:This won't print
*section2
T:This will print
```

**Repeating**
```pilot
C:count = 0
*loop
T:Count is $count
C:count = count + 1
C:test = count < 5
M:test = 1
J:loop
```

### Complete Example: Quiz Program

```pilot
T:Welcome to the PILOT Quiz!
T:==========================
T:
*question1
T:Question 1: What is the capital of France?
A:answer1
M:Paris
T:✓ Correct!
C:score = score + 1
N:
T:✗ Incorrect. The answer is Paris.
T:
*question2
T:Question 2: What is 7 × 8?
A:answer2
M:56
T:✓ Correct!
C:score = score + 1
N:
T:✗ Incorrect. The answer is 56.
T:
*results
T:Your score: $score out of 2
C:percent = (score / 2) * 100
T:That's $percent%!
E:
```

### PILOT Best Practices

1. **Use clear prompts**: Tell users exactly what to enter
2. **Provide feedback**: Confirm correct answers, explain incorrect ones
3. **Label sections**: Use meaningful label names
4. **Keep it simple**: PILOT excels at straightforward tutorials
5. **Test user paths**: Try all possible answers

---

## Logo Programming

### Introduction to Logo

Logo was created in 1967 to teach programming through turtle graphics. You control a "turtle" that draws on screen as it moves.

### Turtle Basics

**Movement**
```logo
FORWARD 100    ; Move forward 100 steps
BACK 50        ; Move backward 50 steps
RIGHT 90       ; Turn right 90 degrees
LEFT 45        ; Turn left 45 degrees
```

**Pen Control**
```logo
PENUP          ; Lift pen (no drawing)
PENDOWN        ; Lower pen (draw)
PENSIZE 5      ; Set pen thickness
```

**Colors**
```logo
SETPENCOLOR 1  ; Red
SETPENCOLOR 2  ; Green
SETPENCOLOR 3  ; Blue
SETPENCOLOR 4  ; Yellow
; ... up to 16 colors
```

### Drawing Shapes

**Square**
```logo
REPEAT 4 [FORWARD 100 RIGHT 90]
```

**Triangle**
```logo
REPEAT 3 [FORWARD 100 RIGHT 120]
```

**Circle** (approximated)
```logo
REPEAT 36 [FORWARD 10 RIGHT 10]
```

**Star**
```logo
REPEAT 5 [FORWARD 100 RIGHT 144]
```

### Procedures

Define reusable commands:

```logo
TO SQUARE :SIZE
  REPEAT 4 [FORWARD :SIZE RIGHT 90]
END

; Use it:
SQUARE 50
SQUARE 100
SQUARE 150
```

**Parameters**
```logo
TO RECTANGLE :WIDTH :HEIGHT
  REPEAT 2 [
    FORWARD :WIDTH
    RIGHT 90
    FORWARD :HEIGHT
    RIGHT 90
  ]
END

RECTANGLE 150 75
```

### Variables and Math

```logo
MAKE "AGE 25
PRINT :AGE

MAKE "NEXT :AGE + 1
PRINT :NEXT
```

**List Operations**
```logo
MAKE "COLORS [RED GREEN BLUE]
PRINT FIRST :COLORS       ; RED
PRINT LAST :COLORS        ; BLUE
PRINT COUNT :COLORS       ; 3
```

### Complete Example: Colorful Spiral

```logo
TO SPIRAL :SIZE :ANGLE
  IF :SIZE > 200 [STOP]
  SETPENCOLOR RANDOM 16
  FORWARD :SIZE
  RIGHT :ANGLE
  SPIRAL :SIZE + 5 :ANGLE
END

CLEARSCREEN
SPIRAL 10 90
```

### Logo Best Practices

1. **Start simple**: Draw basic shapes before complex patterns
2. **Use procedures**: Break complex drawings into parts
3. **Experiment with values**: Small changes create interesting effects
4. **Comment your code**: Use `;` for explanations
5. **Save interesting patterns**: You might want them later

---

## Pascal Programming

### Introduction to Pascal

Pascal was created in 1970 to teach structured programming. It enforces good practices and clear code organization.

### Program Structure

```pascal
program HelloWorld;
begin
  writeln('Hello, World!');
end.
```

Every Pascal program:
- Starts with `program ProgramName;`
- Has a main `begin...end.` block
- Ends with a period `.`

### Variables and Types

```pascal
program Variables;
var
  age: integer;
  name: string;
  price: real;
  done: boolean;
begin
  age := 25;
  name := 'Alice';
  price := 19.99;
  done := false;
  
  writeln('Name: ', name);
  writeln('Age: ', age);
  writeln('Price: $', price:0:2);
end.
```

**Assignment**: Use `:=` not `=`

**Types**:
- `integer` - Whole numbers
- `real` - Decimals
- `string` - Text
- `boolean` - True/False
- `char` - Single character

### Input and Output

```pascal
program InputExample;
var
  username: string;
  age: integer;
begin
  write('Enter your name: ');
  readln(username);
  
  write('Enter your age: ');
  readln(age);
  
  writeln('Hello, ', username, '!');
  writeln('You are ', age, ' years old.');
end.
```

### Control Structures

**IF Statement**
```pascal
if age >= 18 then
  writeln('Adult')
else
  writeln('Minor');
```

**Multi-condition**
```pascal
if score >= 90 then
  writeln('A')
else if score >= 80 then
  writeln('B')
else if score >= 70 then
  writeln('C')
else
  writeln('F');
```

**CASE Statement**
```pascal
case grade of
  'A': writeln('Excellent!');
  'B': writeln('Good job!');
  'C': writeln('Average');
  'D', 'F': writeln('Needs improvement');
end;
```

### Loops

**FOR Loop**
```pascal
for i := 1 to 10 do
  writeln(i);

for i := 10 downto 1 do
  writeln(i);
```

**WHILE Loop**
```pascal
while count < 10 do
begin
  writeln(count);
  count := count + 1;
end;
```

**REPEAT-UNTIL Loop**
```pascal
repeat
  writeln('Enter password: ');
  readln(password);
until password = 'secret';
```

### Procedures and Functions

**Procedure** (no return value)
```pascal
procedure Greet(name: string);
begin
  writeln('Hello, ', name, '!');
end;

begin
  Greet('Alice');
  Greet('Bob');
end.
```

**Function** (returns a value)
```pascal
function Square(x: integer): integer;
begin
  Square := x * x;
end;

begin
  writeln(Square(5));    { Outputs: 25 }
  writeln(Square(10));   { Outputs: 100 }
end.
```

### Complete Example: Grade Calculator

```pascal
program GradeCalculator;
var
  score: integer;
  grade: char;

function CalculateGrade(points: integer): char;
begin
  if points >= 90 then
    CalculateGrade := 'A'
  else if points >= 80 then
    CalculateGrade := 'B'
  else if points >= 70 then
    CalculateGrade := 'C'
  else if points >= 60 then
    CalculateGrade := 'D'
  else
    CalculateGrade := 'F';
end;

begin
  write('Enter score (0-100): ');
  readln(score);
  
  if (score < 0) or (score > 100) then
  begin
    writeln('Invalid score!');
    halt;
  end;
  
  grade := CalculateGrade(score);
  writeln('Your grade is: ', grade);
end.
```

### Pascal Best Practices

1. **Declare all variables**: In the `var` section
2. **Use meaningful names**: `studentCount` not `sc`
3. **Indent consistently**: 2 or 4 spaces
4. **One statement per line**: Keep it readable
5. **Use procedures/functions**: Organize code logically

---

## Prolog Programming

### Introduction to Prolog

Prolog (Programming in Logic) was created in 1972. It's a declarative language based on formal logic. Instead of telling the computer how to solve a problem, you describe the problem and let Prolog find solutions.

### Facts and Rules

**Facts** state what is true:
```prolog
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).
```

This says:
- Tom is a parent of Bob
- Tom is a parent of Liz
- Etc.

**Rules** define relationships:
```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

This says: X is a grandparent of Z if X is a parent of Y and Y is a parent of Z.

### Queries

Ask Prolog questions:

```prolog
?- parent(tom, bob).
yes

?- parent(tom, ann).
no

?- grandparent(tom, ann).
yes

?- grandparent(tom, Who).
Who = ann
Who = pat
```

### Lists

```prolog
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

append([], L, L).
append([H|T], L2, [H|L3]) :- append(T, L2, L3).
```

Usage:
```prolog
?- member(2, [1, 2, 3]).
yes

?- length([a, b, c], N).
N = 3

?- append([1, 2], [3, 4], L).
L = [1, 2, 3, 4]
```

### Complete Example: Family Relationships

```prolog
% Facts about people
male(tom).
male(bob).
male(jim).
female(liz).
female(ann).
female(pat).

% Parent relationships
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Rules
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
grandmother(X, Z) :- grandparent(X, Z), female(X).
grandfather(X, Z) :- grandparent(X, Z), male(X).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y) :- sibling(X, Y), female(X).

% Queries to try:
% ?- father(tom, bob).
% ?- grandmother(Who, jim).
% ?- sibling(ann, pat).
```

### Prolog Best Practices

1. **Start with facts**: Define what you know
2. **Build rules gradually**: Test each one
3. **Use meaningful names**: Predicates should describe relationships
4. **Think declaratively**: Describe "what" not "how"
5. **Test incrementally**: Try queries as you build

---

## C Programming

### Introduction to C

C was created in 1972 for systems programming. It provides low-level control while remaining portable. Time Warp IDE supports a simplified version suitable for education.

### Basic Structure

```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

Every C program:
- Has a `main()` function
- Uses `printf()` for output
- Returns 0 for success

### Variables and Types

```c
int age = 25;
float price = 19.99;
char grade = 'A';
char name[50] = "Alice";

printf("Age: %d\n", age);
printf("Price: $%.2f\n", price);
printf("Grade: %c\n", grade);
printf("Name: %s\n", name);
```

**Format Specifiers**:
- `%d` - integer
- `%f` - float
- `%c` - character
- `%s` - string

### Input

```c
int age;
printf("Enter your age: ");
scanf("%d", &age);
printf("You are %d years old\n", age);
```

**Note**: Use `&` before variable name in `scanf()`

### Control Flow

**IF Statement**
```c
if (age >= 18) {
    printf("Adult\n");
} else {
    printf("Minor\n");
}
```

**Switch Statement**
```c
switch (grade) {
    case 'A':
        printf("Excellent!\n");
        break;
    case 'B':
        printf("Good!\n");
        break;
    default:
        printf("Keep trying!\n");
}
```

### Loops

**FOR Loop**
```c
for (int i = 1; i <= 10; i++) {
    printf("%d\n", i);
}
```

**WHILE Loop**
```c
int count = 0;
while (count < 10) {
    printf("%d\n", count);
    count++;
}
```

**DO-WHILE Loop**
```c
int num;
do {
    printf("Enter a positive number: ");
    scanf("%d", &num);
} while (num <= 0);
```

### Functions

```c
int square(int x) {
    return x * x;
}

int main() {
    int result = square(5);
    printf("5 squared is %d\n", result);
    return 0;
}
```

**With multiple parameters**:
```c
int max(int a, int b) {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}
```

### Arrays

```c
int numbers[5] = {10, 20, 30, 40, 50};

for (int i = 0; i < 5; i++) {
    printf("numbers[%d] = %d\n", i, numbers[i]);
}
```

### Complete Example: Temperature Converter

```c
#include <stdio.h>

float celsius_to_fahrenheit(float c) {
    return (c * 9.0 / 5.0) + 32.0;
}

float fahrenheit_to_celsius(float f) {
    return (f - 32.0) * 5.0 / 9.0;
}

int main() {
    int choice;
    float temp, result;
    
    printf("Temperature Converter\n");
    printf("1. Celsius to Fahrenheit\n");
    printf("2. Fahrenheit to Celsius\n");
    printf("Choose (1 or 2): ");
    scanf("%d", &choice);
    
    if (choice == 1) {
        printf("Enter temperature in Celsius: ");
        scanf("%f", &temp);
        result = celsius_to_fahrenheit(temp);
        printf("%.2f°C = %.2f°F\n", temp, result);
    } else if (choice == 2) {
        printf("Enter temperature in Fahrenheit: ");
        scanf("%f", &temp);
        result = fahrenheit_to_celsius(temp);
        printf("%.2f°F = %.2f°C\n", temp, result);
    } else {
        printf("Invalid choice!\n");
    }
    
    return 0;
}
```

### C Best Practices

1. **Initialize variables**: Avoid garbage values
2. **Check input**: Validate user data
3. **Use descriptive names**: `studentCount` not `sc`
4. **Comment complex logic**: Explain why, not what
5. **Free memory**: If you allocate, deallocate

---

## Choosing a Language

### Decision Guide

**Choose BASIC if you:**
- Are completely new to programming
- Want simple, English-like commands
- Need quick results without much setup
- Enjoy line-by-line program structure

**Choose PILOT if you:**
- Are creating educational content
- Want to build interactive tutorials
- Need clear, readable code for teaching
- Prefer straightforward question-answer programs

**Choose Logo if you:**
- Learn best visually
- Enjoy art and graphics
- Want immediate visual feedback
- Like creative, exploratory programming

**Choose Pascal if you:**
- Want to learn structured programming
- Value clear type systems
- Plan to learn languages like C++ or Java later
- Appreciate strict syntax checking

**Choose Prolog if you:**
- Are interested in logic and AI
- Like puzzle-solving
- Want to explore declarative programming
- Need to work with relationships and rules

**Choose C if you:**
- Want low-level programming experience
- Plan to do systems programming
- Need maximum control and efficiency
- Are comfortable with complexity

### Learning Path Suggestions

**Beginner Path**:
1. Start with BASIC - Learn fundamental concepts
2. Try Logo - Add visual programming
3. Explore PILOT - See teaching-focused design

**Academic Path**:
1. Start with Pascal - Learn structure
2. Try Prolog - Explore logic programming
3. Move to C - Add low-level concepts

**Creative Path**:
1. Start with Logo - Create art
2. Try BASIC - Add more control
3. Experiment with all - Mix and match!

---

## Cross-Language Concepts

These concepts apply across all languages:

### Variables
Store and manipulate data. Every language has them, just different syntax.

### Conditionals
Make decisions (if/then/else). Logic is universal.

### Loops
Repeat actions. The concept is the same, syntax differs.

### Functions/Procedures
Organize code into reusable pieces. Called different names, same idea.

### Input/Output
Get data from users, display results. Every language needs it.

### Comments
Document your code. Essential in all languages.

---

## Practice Exercises

**Beginner**:
1. Write "Hello, World!" in all six languages
2. Create a program that adds two numbers
3. Make a simple quiz with 3 questions

**Intermediate**:
1. Build a calculator with +, -, ×, ÷
2. Create a times table printer
3. Draw geometric patterns with Logo

**Advanced**:
1. Implement a guessing game with scoring
2. Build a simple text adventure
3. Create recursive turtle graphics

---

## Next Steps

- Explore **[Quick Reference](02-quick-reference.md)** for command cheat sheets
- Check **[Examples](../../examples/)** for complete programs
- Read **[FAQ](03-faq.md)** for common questions
- Join the **community** to share your work

Happy coding!
