# Pascal Programming with Time Warp Studio

Pascal is a structured programming language known for emphasizing clarity and structure. Time Warp Studio provides experimental Pascal support for learning procedural programming concepts.

## Quick Start

A simple Pascal program:

```pascal
program HelloWorld;
begin
  WriteLn('Hello, World!');
end.
```

Run this:
1. Select "Pascal" from the language dropdown
2. Paste the code
3. Click Run (Ctrl+R)

## Basic Syntax

### Program Structure

```pascal
program ProgramName;

var
  { Variable declarations }

begin
  { Program statements }
end.
```

### Comments

```pascal
{ This is a comment }
(* This is also a comment *)
```

### Variables and Data Types

```pascal
var
  age: Integer;
  height: Real;
  initial: Char;
  is_student: Boolean;
  name: String;
```

**Data Types:**
- `Integer` - Whole numbers
- `Real` - Decimal numbers
- `Char` - Single character
- `String` - Text
- `Boolean` - True/False

### Output: WriteLn

```pascal
program OutputDemo;
begin
  WriteLn('Hello, World!');
  WriteLn('Age: ', 25);
  WriteLn('Height: ', 5.9:3:1);  { value:width:decimals }
end.
```

### Input: ReadLn

```pascal
program InputDemo;
var
  name: String;
  age: Integer;
begin
  Write('Enter your name: ');
  ReadLn(name);
  Write('Enter your age: ');
  ReadLn(age);
  WriteLn('Hello, ', name, ' (age ', age, ')');
end.
```

## Operators

### Arithmetic

```pascal
var
  a, b, sum, diff, product, quotient: Integer;
begin
  a := 10;
  b := 3;
  
  sum := a + b;        { 13 }
  diff := a - b;       { 7 }
  product := a * b;    { 30 }
  quotient := a div b; { 3 (integer division) }
end.
```

**Note:** Use `:=` for assignment (not `=`)

### Comparison

```pascal
if x = 10 then WriteLn('Equal');
if x <> 5 then WriteLn('Not equal');
if x > 5 then WriteLn('Greater');
if x < 20 then WriteLn('Less');
if x >= 10 then WriteLn('Greater or equal');
if x <= 15 then WriteLn('Less or equal');
```

### Logical

```pascal
if (age >= 18) and (is_student) then
  WriteLn('Adult student');

if (age < 18) or (is_student) then
  WriteLn('Young or student');

if not (is_student) then
  WriteLn('Not a student');
```

## Control Flow

### if/else

```pascal
program GradeCalculator;
var
  score: Integer;
begin
  Write('Enter score: ');
  ReadLn(score);
  
  if score >= 90 then
    WriteLn('Grade: A')
  else if score >= 80 then
    WriteLn('Grade: B')
  else if score >= 70 then
    WriteLn('Grade: C')
  else
    WriteLn('Grade: F');
end.
```

### case Statement

```pascal
program CaseExample;
var
  choice: Integer;
begin
  Write('Choose (1-3): ');
  ReadLn(choice);
  
  case choice of
    1: WriteLn('Option 1');
    2: WriteLn('Option 2');
    3: WriteLn('Option 3');
    else WriteLn('Invalid option');
  end;
end.
```

### Loops

**for loop:**

```pascal
var
  i: Integer;
begin
  for i := 1 to 5 do
    WriteLn('Number: ', i);
end.
```

**Counting backwards:**

```pascal
for i := 5 downto 1 do
  WriteLn('Countdown: ', i);
```

**while loop:**

```pascal
var
  count: Integer;
begin
  count := 0;
  while count < 5 do
  begin
    WriteLn('Count: ', count);
    count := count + 1;
  end;
end.
```

**repeat...until loop:**

```pascal
var
  count: Integer;
begin
  count := 0;
  repeat
    WriteLn('Count: ', count);
    count := count + 1;
  until count >= 5;
end.
```

## Procedures and Functions

### Procedure

```pascal
procedure Greet(name: String);
begin
  WriteLn('Hello, ', name, '!');
end;

begin
  Greet('Alice');
  Greet('Bob');
end.
```

### Function

```pascal
function Add(a, b: Integer): Integer;
begin
  Add := a + b;
end;

var
  result: Integer;
begin
  result := Add(3, 5);
  WriteLn('Result: ', result);
end.
```

### Procedures with Multiple Parameters

```pascal
procedure PrintInfo(name: String; age: Integer; city: String);
begin
  WriteLn('Name: ', name);
  WriteLn('Age: ', age);
  WriteLn('City: ', city);
end;

begin
  PrintInfo('Alice', 25, 'New York');
end.
```

## Arrays

### One-Dimensional Array

```pascal
var
  numbers: array[1..5] of Integer;
  i: Integer;
begin
  numbers[1] := 10;
  numbers[2] := 20;
  numbers[3] := 30;
  
  for i := 1 to 5 do
    WriteLn('numbers[', i, '] = ', numbers[i]);
end.
```

### String Array

```pascal
var
  names: array[1..3] of String;
  i: Integer;
begin
  names[1] := 'Alice';
  names[2] := 'Bob';
  names[3] := 'Charlie';
  
  for i := 1 to 3 do
    WriteLn('Name: ', names[i]);
end.
```

## Complete Example Programs

### Simple Calculator

```pascal
program Calculator;
var
  a, b: Real;
  op: Char;
  result: Real;
begin
  WriteLn('=== Simple Calculator ===');
  Write('Enter first number: ');
  ReadLn(a);
  Write('Enter operator (+, -, *, /): ');
  ReadLn(op);
  Write('Enter second number: ');
  ReadLn(b);
  
  case op of
    '+': result := a + b;
    '-': result := a - b;
    '*': result := a * b;
    '/': begin
           if b <> 0 then
             result := a / b
           else
           begin
             WriteLn('Error: Division by zero');
             exit;
           end;
         end;
    else WriteLn('Invalid operator');
  end;
  
  WriteLn('Result: ', result:5:2);
end.
```

### Number Guessing Game

```pascal
program GuessingGame;
var
  secret, guess, attempts: Integer;
begin
  WriteLn('=== Guessing Game ===');
  secret := 42;
  attempts := 0;
  
  repeat
    Write('Guess the number (1-100): ');
    ReadLn(guess);
    attempts := attempts + 1;
    
    if guess < secret then
      WriteLn('Too low!')
    else if guess > secret then
      WriteLn('Too high!')
    else
    begin
      WriteLn('Correct! You guessed it in ', attempts, ' attempts!');
      break;
    end;
  until guess = secret;
end.
```

### Multiplication Table

```pascal
program MultiplicationTable;
var
  n, i, j: Integer;
begin
  WriteLn('=== Multiplication Table ===');
  Write('Enter table size: ');
  ReadLn(n);
  WriteLn;
  
  for i := 1 to n do
  begin
    for j := 1 to n do
      Write(i*j:4, ' ');
    WriteLn;
  end;
end.
```

### Sum and Average

```pascal
program SumAverage;
var
  numbers: array[1..10] of Integer;
  sum, count, i: Integer;
  average: Real;
begin
  WriteLn('=== Sum and Average ===');
  Write('How many numbers? ');
  ReadLn(count);
  
  for i := 1 to count do
  begin
    Write('Enter number ', i, ': ');
    ReadLn(numbers[i]);
  end;
  
  sum := 0;
  for i := 1 to count do
    sum := sum + numbers[i];
  
  average := sum / count;
  WriteLn;
  WriteLn('Sum: ', sum);
  WriteLn('Average: ', average:5:2);
end.
```

## Tips and Best Practices

1. **Use descriptive names**: `age` is better than `a`
2. **Comment complex logic**: Explain how it works
3. **Break code into procedures**: Keep procedures small
4. **Test thoroughly**: Try edge cases
5. **Use proper spacing**: Make code readable

## Common Mistakes to Avoid

```pascal
{ ❌ Wrong: Using = for assignment }
age = 25;

{ ✅ Right: Use := for assignment }
age := 25;

{ ❌ Wrong: Forgetting semicolon }
WriteLn('Hello')
WriteLn('World');

{ ✅ Right: Semicolons between statements }
WriteLn('Hello');
WriteLn('World');

{ ❌ Wrong: Array indices out of bounds }
var numbers: array[1..5] of Integer;
numbers[10] := 5;  { Error! }

{ ✅ Right: Stay within bounds }
numbers[5] := 5;
```

## Pascal vs. Other Languages

| Feature | Pascal | C | Python |
|---------|--------|---|--------|
| Assignment | `:=` | `=` | `=` |
| Output | `WriteLn` | `printf` | `print` |
| Input | `ReadLn` | `scanf` | `input` |
| Comments | `{ }` or `(* *)` | `/* */` or `//` | `#` or `""" """` |
| Procedures | `procedure` | `void function` | `def` |
| Functions | `function` | `return_type function` | `def` |

## Learning Resources

- **Start Simple**: Begin with I/O programs
- **Use Comments**: Document your learning process
- **Debug**: Add WriteLn statements to check values
- **Experiment**: Modify examples to understand concepts

## Running Pascal Programs in Time Warp Studio

1. Create a `.pas` file with your program
2. Select "Pascal" from the language dropdown
3. Paste your code or load the file
4. Click Run or press Ctrl+R
5. Interact with the program in the Output panel

## Next Steps

- Learn [C for systems programming](c.md)
- Explore [Python for general programming](python.md)
- Try [BASIC for classic programming](basic.md)
- Learn [Logo for turtle graphics](logo.md)

Happy Pascal programming!
