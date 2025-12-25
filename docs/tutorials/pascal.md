# Pascal Tutorial

**Pascal** is a structured programming language that emphasizes readable, clean code. It's excellent for learning proper programming practices and has strong typing.

## Getting Started

### Hello, World! 👋

```pascal
PROGRAM HelloWorld;
BEGIN
  WriteLn('Hello, World!');
END.
```

**Output:**
```
Hello, World!
```

Key features:
- `PROGRAM name;` - Declares program
- `BEGIN...END.` - Main block (note the `.` at end)
- `WriteLn` - Prints with newline

## Variables

### Declaring Variables

```pascal
PROGRAM Variables;
VAR
  X: Integer;
  PI: Real;
  NAME: String;
BEGIN
  X := 5;
  PI := 3.14159;
  NAME := 'Alice';
  WriteLn('X is ', X);
END.
```

`VAR` section declares all variables before using them.

### Data Types

```pascal
VAR
  AGE: Integer;        { Whole numbers }
  PRICE: Real;         { Decimal numbers }
  NAME: String;        { Text }
  ACTIVE: Boolean;     { True or False }
  GRADE: Char;         { Single character }
BEGIN
  AGE := 25;
  PRICE := 19.99;
  NAME := 'John';
  ACTIVE := True;
  GRADE := 'A';
  
  WriteLn('Age: ', AGE);
  WriteLn('Price: ', PRICE:0:2);  { Format to 2 decimals }
END.
```

### Variable Naming

- Must start with letter
- Can have letters, numbers, underscore
- Case doesn't matter
- Cannot use reserved words (Program, Begin, etc.)

## Input and Output

### Writing Output

```pascal
BEGIN
  WriteLn('Hello');              { Print with newline }
  WriteLn('Line 1');
  WriteLn('Line 2');
  
  Write('No newline');           { Print without newline }
  WriteLn(' - on same line');
END.
```

**Output:**
```
Hello
Line 1
Line 2
No newline - on same line
```

### Reading Input

```pascal
PROGRAM InputExample;
VAR
  NAME: String;
  AGE: Integer;
BEGIN
  Write('Enter your name: ');
  ReadLn(NAME);
  
  Write('Enter your age: ');
  ReadLn(AGE);
  
  WriteLn('Hello ', NAME, ', age ', AGE);
END.
```

Program waits for user input and presses Enter.

## Math Operations

```pascal
PROGRAM Math;
VAR
  X, Y, RESULT: Integer;
BEGIN
  X := 10;
  Y := 3;
  
  RESULT := X + Y;        WriteLn('Sum: ', RESULT);
  RESULT := X - Y;        WriteLn('Difference: ', RESULT);
  RESULT := X * Y;        WriteLn('Product: ', RESULT);
  RESULT := X DIV Y;      WriteLn('Integer Division: ', RESULT);
  RESULT := X MOD Y;      WriteLn('Remainder: ', RESULT);
END.
```

**Output:**
```
Sum: 13
Difference: 7
Product: 30
Integer Division: 3
Remainder: 1
```

### Math Functions

```pascal
BEGIN
  WriteLn('Absolute value: ', Abs(-5));      { 5 }
  WriteLn('Square: ', Sqr(4));               { 16 }
  WriteLn('Square root: ', Sqrt(16):0:0);    { 4 }
  WriteLn('Power: ', Exp(2*Ln(3)):0:0);      { 3^2 = 9 }
END.
```

## Strings

### String Operations

```pascal
PROGRAM Strings;
VAR
  FIRST, LAST, FULL: String;
BEGIN
  FIRST := 'John';
  LAST := 'Doe';
  FULL := FIRST + ' ' + LAST;
  WriteLn(FULL);
END.
```

**Output:**
```
John Doe
```

### String Functions

```pascal
VAR
  TEXT: String;
BEGIN
  TEXT := 'Hello';
  
  WriteLn('Length: ', Length(TEXT));         { 5 }
  WriteLn('Uppercase: ', UpCase(TEXT[1]));   { H }
  WriteLn('Char 1: ', TEXT[1]);              { H }
END.
```

## Conditionals (IF/THEN/ELSE)

### Simple IF

```pascal
VAR
  X: Integer;
BEGIN
  X := 15;
  
  IF X > 10 THEN
    WriteLn('X is greater than 10');
END.
```

### IF/ELSE

```pascal
VAR
  AGE: Integer;
BEGIN
  Write('Enter age: ');
  ReadLn(AGE);
  
  IF AGE >= 18 THEN
    WriteLn('You are an adult')
  ELSE
    WriteLn('You are a minor');
END.
```

### IF/ELSE IF/ELSE

```pascal
PROGRAM Grade;
VAR
  SCORE: Integer;
BEGIN
  Write('Enter score: ');
  ReadLn(SCORE);
  
  IF SCORE >= 90 THEN
    WriteLn('Grade: A')
  ELSE IF SCORE >= 80 THEN
    WriteLn('Grade: B')
  ELSE IF SCORE >= 70 THEN
    WriteLn('Grade: C')
  ELSE IF SCORE >= 60 THEN
    WriteLn('Grade: D')
  ELSE
    WriteLn('Grade: F');
END.
```

### Comparison Operators

```pascal
IF X = 5 THEN ...        { Equal }
IF X <> 5 THEN ...       { Not equal }
IF X > 5 THEN ...        { Greater than }
IF X < 5 THEN ...        { Less than }
IF X >= 5 THEN ...       { Greater or equal }
IF X <= 5 THEN ...       { Less or equal }
```

### Logical Operators

```pascal
IF (X > 5) AND (Y < 10) THEN ...   { Both true }
IF (X > 5) OR (Y < 10) THEN ...    { Either true }
IF NOT (X = 5) THEN ...            { Not true }
```

## Loops

### FOR Loop

```pascal
PROGRAM Loops;
VAR
  I: Integer;
BEGIN
  FOR I := 1 TO 5 DO
    WriteLn('I = ', I);
END.
```

**Output:**
```
I = 1
I = 2
I = 3
I = 4
I = 5
```

**Counting backwards:**
```pascal
FOR I := 10 DOWNTO 1 DO
  WriteLn(I);
```

### WHILE Loop

```pascal
VAR
  X: Integer;
BEGIN
  X := 1;
  WHILE X <= 5 DO
  BEGIN
    WriteLn('X = ', X);
    X := X + 1
  END
END.
```

### REPEAT/UNTIL Loop

```pascal
VAR
  X: Integer;
BEGIN
  X := 1;
  REPEAT
    WriteLn('X = ', X);
    X := X + 1
  UNTIL X > 5
END.
```

Executes at least once before checking condition.

## Arrays

### Single Dimension Array

```pascal
PROGRAM Arrays;
VAR
  NUMBERS: ARRAY [1..5] OF Integer;
  I: Integer;
BEGIN
  { Fill array }
  FOR I := 1 TO 5 DO
    NUMBERS[I] := I * 10;
  
  { Print array }
  FOR I := 1 TO 5 DO
    WriteLn('NUMBERS[', I, '] = ', NUMBERS[I]);
END.
```

**Output:**
```
NUMBERS[1] = 10
NUMBERS[2] = 20
NUMBERS[3] = 30
NUMBERS[4] = 40
NUMBERS[5] = 50
```

### Two Dimensional Array

```pascal
VAR
  MATRIX: ARRAY [1..3, 1..3] OF Integer;
  I, J: Integer;
BEGIN
  FOR I := 1 TO 3 DO
    FOR J := 1 TO 3 DO
      MATRIX[I, J] := I * J;
END.
```

## Procedures

Create reusable blocks of code:

```pascal
PROGRAM Procedures;

PROCEDURE GREET(NAME: String);
BEGIN
  WriteLn('Hello, ', NAME);
  WriteLn('Welcome to Pascal');
END;

BEGIN
  GREET('Alice');
  GREET('Bob');
END.
```

**Output:**
```
Hello, Alice
Welcome to Pascal
Hello, Bob
Welcome to Pascal
```

### Procedures with Multiple Parameters

```pascal
PROCEDURE MULTIPLY(A, B: Integer);
VAR
  RESULT: Integer;
BEGIN
  RESULT := A * B;
  WriteLn(A, ' * ', B, ' = ', RESULT);
END;

BEGIN
  MULTIPLY(3, 4);
  MULTIPLY(5, 6);
END.
```

## Functions

Functions return values:

```pascal
PROGRAM Functions;

FUNCTION ADD(A, B: Integer): Integer;
BEGIN
  ADD := A + B
END;

FUNCTION GREET(NAME: String): String;
BEGIN
  GREET := 'Hello, ' + NAME
END;

BEGIN
  WriteLn(ADD(3, 5));        { 8 }
  WriteLn(GREET('Alice'));   { Hello, Alice }
END.
```

### Recursive Functions

```pascal
FUNCTION FACTORIAL(N: Integer): Integer;
BEGIN
  IF N <= 1 THEN
    FACTORIAL := 1
  ELSE
    FACTORIAL := N * FACTORIAL(N - 1)
END;

BEGIN
  WriteLn('5! = ', FACTORIAL(5));  { 120 }
END.
```

## Comments

Document your code:

```pascal
PROGRAM Comments;
{ This is a block comment
  It can span multiple lines }
BEGIN
  WriteLn('Hello');  { This is an inline comment }
  { Another comment }
END.
```

## Complete Example: Grade Calculator

```pascal
PROGRAM GradeCalc;
VAR
  SCORE1, SCORE2, SCORE3, AVERAGE: Real;
  GRADE: Char;

FUNCTION GetGrade(AVG: Real): Char;
BEGIN
  IF AVG >= 90 THEN GetGrade := 'A'
  ELSE IF AVG >= 80 THEN GetGrade := 'B'
  ELSE IF AVG >= 70 THEN GetGrade := 'C'
  ELSE IF AVG >= 60 THEN GetGrade := 'D'
  ELSE GetGrade := 'F'
END;

BEGIN
  WriteLn('Enter three test scores:');
  
  Write('Score 1: ');
  ReadLn(SCORE1);
  
  Write('Score 2: ');
  ReadLn(SCORE2);
  
  Write('Score 3: ');
  ReadLn(SCORE3);
  
  AVERAGE := (SCORE1 + SCORE2 + SCORE3) / 3;
  GRADE := GetGrade(AVERAGE);
  
  WriteLn('');
  WriteLn('Average: ', AVERAGE:0:2);
  WriteLn('Grade: ', GRADE);
END.
```

## Pascal vs BASIC

| Feature | BASIC | Pascal |
|---------|-------|--------|
| Variables | No declaration | `VAR` section |
| Types | Weak typing | Strong typing |
| Functions | Simple | Rich structures |
| Readability | Medium | High |
| Structure | Loose | Organized |

## Common Commands Reference

### Output
| Command | Example | Purpose |
|---------|---------|---------|
| `WriteLn` | `WriteLn('Hi')` | Print with newline |
| `Write` | `Write('Hi')` | Print without newline |

### Input
| Command | Example | Purpose |
|---------|---------|---------|
| `ReadLn` | `ReadLn(X)` | Read number or string |

### Control
| Command | Example | Purpose |
|---------|---------|---------|
| `IF/THEN/ELSE` | `IF X > 5 THEN ...` | Conditional |
| `FOR/DO` | `FOR I := 1 TO 10 DO ...` | Loop N times |
| `WHILE/DO` | `WHILE X < 10 DO ...` | Loop while true |
| `REPEAT/UNTIL` | `REPEAT ... UNTIL X=5` | Loop until true |

### Functions
| Command | Example | Purpose |
|---------|---------|---------|
| `PROCEDURE` | `PROCEDURE NAME;...END;` | Subroutine |
| `FUNCTION` | `FUNCTION NAME:Type;...END;` | Function with return |

## Tips for Learning Pascal

1. **Declare first** - Always use `VAR` section
2. **Use types** - Be explicit about data types
3. **Structure code** - Use procedures and functions
4. **Test types** - Pascal catches type errors
5. **Use comments** - Document your code
6. **Be precise** - Pascal enforces structure

## Common Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| `X = 5` | Wrong operator | `X := 5` |
| Missing `;` | Syntax error | Add `;` between statements |
| No `VAR` | Variable undefined | Declare in `VAR` section |
| Missing `END` | Block not closed | Add `END` |
| Wrong type | Type mismatch | Declare correct type |

## Next Steps

1. ✅ Complete tutorials above
2. 📂 Try examples from `Examples/pascal/`
3. 🔧 Build a calculator program
4. 📊 Create a data analysis tool

---

Happy coding! 💻
