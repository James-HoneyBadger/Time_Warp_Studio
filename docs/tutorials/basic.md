# BASIC Programming Tutorial

Learn BASIC programming language in Time Warp Studio. BASIC is a beginner-friendly language perfect for learning programming fundamentals.

---

## Introduction to BASIC

BASIC stands for **B**eginner's **A**ll-purpose **S**ymbolic **I**nstruction **C**ode. It was designed to teach programming concepts in simple, readable syntax.

### Why BASIC?

- ✅ Easy to learn and read
- ✅ Clear, explicit syntax
- ✅ Perfect for beginners
- ✅ Great for teaching algorithms
- ✅ Fun to experiment with

---

## Getting Started

### Your First Program

```basic
PRINT "Hello, World!"
```

**Run it:**
1. Select BASIC from language dropdown
2. Type the code above
3. Press Ctrl+R or click Run

**Output:**
```
Hello, World!
```

### Simple Math

```basic
PRINT 2 + 3
PRINT 10 - 4
PRINT 5 * 6
PRINT 20 / 4
```

**Output:**
```
5
6
30
5
```

---

## Variables

Variables store data for your program.

### Declaring Variables

```basic
LET X = 5
LET NAME$ = "Alice"
LET PRICE = 19.99
```

**Rules:**
- Variable names start with letter
- Can contain letters, numbers, underscore
- String variables end with `$`
- `LET` is optional in Time Warp Studio (`X = 5` also works)

### Using Variables

```basic
LET AGE = 25
PRINT "My age is "; AGE
PRINT AGE + 10
```

**Output:**
```
My age is 25
35
```

### String Variables

```basic
LET FIRST$ = "John"
LET LAST$ = "Doe"
PRINT FIRST$; " "; LAST$
```

**Output:**
```
John Doe
```

---

## Input & Output

### PRINT Statement

Print text and numbers:

```basic
PRINT "Hello"
PRINT 42
PRINT "Age:"; 25
```

**Using semicolon (;):** Continues on same line
**Using comma (,):** Adds space

### INPUT Statement

Get data from user:

```basic
INPUT "What is your name? "; NAME$
PRINT "Hello, "; NAME$
```

When run, you'll be prompted to enter your name.

### Complete Input/Output Program

```basic
INPUT "Enter your name: "; NAME$
INPUT "Enter your age: "; AGE
LET YEAR = 2025 - AGE
PRINT "Hello "; NAME$; "!"
PRINT "You were born around "; YEAR
```

---

## Arithmetic Operations

```basic
REM Addition, subtraction, multiplication, division
PRINT 10 + 5
PRINT 10 - 5
PRINT 10 * 5
PRINT 10 / 5
REM Exponentiation (power) and modulo (remainder)
PRINT 10 ^ 2
PRINT 10 MOD 3
```

**Output:**
```
15
5
50
2
100
1
```

---

## Conditions & IF Statements

Make decisions in your program:

### Simple IF

```basic
LET AGE = 18
IF AGE >= 18 THEN PRINT "You are an adult"
```

### IF-ELSE

```basic
LET SCORE = 75
IF SCORE >= 60 THEN
  PRINT "You passed!"
ELSE
  PRINT "You failed!"
END IF
```

### Comparison Operators

| Operator | Meaning |
|----------|---------|
| = | Equal to |
| <> | Not equal to |
| < | Less than |
| > | Greater than |
| <= | Less than or equal |
| >= | Greater than or equal |

### Logical Operators

```basic
LET AGE = 30
LET STATUS$ = "student"
LET ACTIVE = 0
IF AGE >= 18 AND AGE <= 65 THEN PRINT "Working age"
IF STATUS$ = "student" OR STATUS$ = "teacher" THEN PRINT "School person"
IF NOT ACTIVE THEN PRINT "Not active"
```

---

## Loops

Repeat code multiple times:

### FOR Loop

```basic
FOR I = 1 TO 5
  PRINT I
NEXT I
```

**Output:**
```
1
2
3
4
5
```

### FOR Loop with Step

```basic
FOR I = 0 TO 10 STEP 2
  PRINT I
NEXT I
```

**Output:**
```
0
2
4
6
8
10
```

### WHILE Loop

```basic
LET X = 1
WHILE X <= 5
  PRINT X
  LET X = X + 1
WEND
```

**Output:**
```
1
2
3
4
5
```

---

## Arrays

Store multiple values in one variable:

### Creating Arrays

```basic
DIM NUMBERS(5)
DIM NAMES$(3)
```

**Note:** Array indices usually start at 0 or 1 (depends on BASIC version)

### Using Arrays

```basic
DIM NUMBERS(5)
LET NUMBERS(1) = 10
LET NUMBERS(2) = 20
LET NUMBERS(3) = 30

FOR I = 1 TO 3
  PRINT NUMBERS(I)
NEXT I
```

**Output:**
```
10
20
30
```

---

## Subroutines

Group code into reusable pieces:

### Defining Subroutines

```basic
10 GOSUB 100
20 END
100 PRINT "Welcome!"
110 INPUT "Your name: "; NAME$
120 PRINT "Hello, "; NAME$
130 RETURN
```

**How it works:**
1. Program runs from the top (line 10)
2. `GOSUB 100` jumps to line 100
3. Lines 100-120 run
4. `RETURN` sends control back to the line after GOSUB (line 20)
5. `END` on line 20 stops the program

Subroutines are called by line number. Named labels such as `GreetUser:` are not supported.

---

## Complete Example Programs

### Program 1: Multiplication Table

```basic
INPUT "Which table? "; TABLE
FOR I = 1 TO 10
  LET PRODUCT = TABLE * I
  PRINT TABLE; " x "; I; " = "; PRODUCT
NEXT I
```

### Program 2: Simple Game

```basic
LET SECRET = 5
INPUT "Guess a number (1-10): "; GUESS
IF GUESS = SECRET THEN PRINT "Correct!" ELSE IF GUESS < SECRET THEN PRINT "Too low!" ELSE PRINT "Too high!"
```

### Program 3: Sum and Average

```basic
REM Array size must be a fixed number; up to 10 values
DIM NUMBERS(10)
INPUT "How many numbers (1-10)? "; COUNT
LET SUM = 0

FOR I = 1 TO COUNT
  INPUT "Enter number: "; N
  LET NUMBERS(I) = N
  LET SUM = SUM + NUMBERS(I)
NEXT I

LET AVG = SUM / COUNT
PRINT "Sum: "; SUM
PRINT "Average: "; AVG
```

---

## Common Functions

```basic
REM Absolute value: 5
PRINT ABS(-5)
REM Integer part: 3
PRINT INT(3.7)
REM Square root: 3
PRINT SQR(9)
REM Sine and cosine of 0: 0 and 1
PRINT SIN(0)
PRINT COS(0)
REM Random number
PRINT RND(1)
```

---

## Comments

Add notes to your code:

```basic
REM This is a comment
PRINT "Hello"
```

`REM` starts a comment that runs to the end of the line. An apostrophe (') also starts a comment, but only at the start of a line. After code on the same line it is not stripped, so use `REM` there.

---

## Tips for Writing BASIC

1. **Use clear variable names** - Use `TOTAL` instead of `T`
2. **Add comments** - Explain what your code does
3. **Test often** - Run after every few lines
4. **Start simple** - Build up complexity gradually
5. **Use indentation** - Easier to read nested code
6. **Initialize variables** - Set variables before using

---

## Debugging Tips

**Program won't run?**
- Check for typos in keywords
- Ensure all IF has THEN
- Match all FOR with NEXT
- Check parentheses are balanced

**Wrong output?**
- Use PRINT to check variable values
- Trace through logic step by step
- Try with simple test data first

**Program seems stuck?**
- Check loop conditions
- Make sure loop variable changes
- Look for infinite WHILE loops

---

## Next Steps

- Try the example programs above
- Modify them slightly
- Create your own programs
- Explore [Logo](logo.md) for graphics
- Learn [Python](python.md) for modern programming

---

**Happy BASIC programming!** 🎉
