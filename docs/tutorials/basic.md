# BASIC Tutorial

**BASIC** (Beginner's All-purpose Symbolic Instruction Code) is one of the most popular languages for learning programming fundamentals.

## Getting Started

### Hello, World! 👋

```basic
PRINT "Hello, World!"
```

**Output:**
```
Hello, World!
```

## Variables

Variables store values:

```basic
X = 5
Y = 10
PRINT X + Y
```

**Output:**
```
15
```

### Variable Types

**Numbers:**
```basic
AGE = 25
PRICE = 19.99
PRINT PRICE
```

**Strings (text):**
```basic
NAME$ = "Alice"
MESSAGE$ = "Hello, " + NAME$
PRINT MESSAGE$
```

Note: String variables end with `$`

### Naming Rules

- Must start with letter
- Can contain letters, numbers, underscore
- Case doesn't matter (X = x)

## Input and Output

### Printing

```basic
PRINT "Hello"
PRINT 10
PRINT 3 + 4
```

### Input from User

```basic
PRINT "What's your name?"
INPUT NAME$
PRINT "Hello, " + NAME$
```

The program waits for user to type something and press Enter.

**Multiple inputs:**
```basic
PRINT "Enter two numbers:"
INPUT A
INPUT B
PRINT "Sum is " A + B
```

## Math Operations

```basic
PRINT 5 + 3         ; Addition
PRINT 10 - 4        ; Subtraction
PRINT 3 * 4         ; Multiplication
PRINT 10 / 2        ; Division
PRINT 2 ^ 3         ; Power (2 to the 3rd = 8)
PRINT 10 MOD 3      ; Remainder (1)
```

## Strings

### String Operations

```basic
FIRST$ = "Hello"
SECOND$ = "World"
RESULT$ = FIRST$ + " " + SECOND$
PRINT RESULT$
```

**Output:**
```
Hello World
```

### String Functions

```basic
TEXT$ = "HELLO"
PRINT LEN(TEXT$)           ; Length = 5
PRINT LOWER$(TEXT$)        ; hello
PRINT UPPER$("hello")      ; HELLO
PRINT MID$(TEXT$, 1, 3)    ; HEL (start at 1, take 3 chars)
```

## Conditionals (IF/THEN/ELSE)

### Simple IF

```basic
PRINT "Enter a number:"
INPUT X
IF X > 10 THEN PRINT "Big number!"
```

### IF/ELSE

```basic
PRINT "Enter age:"
INPUT AGE
IF AGE >= 18 THEN
  PRINT "You're an adult"
ELSE
  PRINT "You're a minor"
END IF
```

### Comparison Operators

```basic
IF X = 5 THEN ...       ; Equal
IF X <> 5 THEN ...      ; Not equal
IF X > 5 THEN ...       ; Greater than
IF X < 5 THEN ...       ; Less than
IF X >= 5 THEN ...      ; Greater or equal
IF X <= 5 THEN ...      ; Less or equal
```

### Logical Operators

```basic
IF X > 5 AND Y < 10 THEN ...     ; Both true
IF X > 5 OR Y < 10 THEN ...      ; Either true
IF NOT (X = 5) THEN ...          ; Not true
```

## Loops

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

**Counting backwards:**
```basic
FOR I = 10 DOWN TO 1
  PRINT I
NEXT I
```

**With step:**
```basic
FOR I = 0 TO 10 STEP 2
  PRINT I
NEXT I
```

Output: 0, 2, 4, 6, 8, 10

### WHILE Loop

```basic
X = 1
WHILE X <= 5
  PRINT X
  X = X + 1
WEND
```

Execute while condition is true.

### DO/UNTIL Loop

```basic
X = 1
DO
  PRINT X
  X = X + 1
UNTIL X > 5
```

## Arrays

Store multiple values:

```basic
DIM GRADES(5)
GRADES(1) = 90
GRADES(2) = 85
GRADES(3) = 92
GRADES(4) = 88
GRADES(5) = 95

PRINT GRADES(1)
```

**Output:**
```
90
```

### Arrays with LOOP

```basic
DIM NUMBERS(10)

FOR I = 1 TO 10
  NUMBERS(I) = I * 10
NEXT I

FOR I = 1 TO 10
  PRINT NUMBERS(I)
NEXT I
```

## Functions (Subroutines)

Create reusable code blocks:

```basic
SUB GREET(NAME$)
  PRINT "Hello, " + NAME$
  PRINT "Nice to meet you!"
END SUB

GREET "Alice"
GREET "Bob"
```

**With return value:**

```basic
FUNCTION ADD(A, B)
  RETURN A + B
END FUNCTION

RESULT = ADD(3, 5)
PRINT RESULT
```

**Output:**
```
8
```

## Comments

Explain your code:

```basic
REM This is a comment
PRINT "Hello"  ! This is also a comment
```

## Complete Example: Guessing Game

```basic
REM Guessing Game
SECRET = INT(RND * 100) + 1
GUESS = 0
TRIES = 0

PRINT "Welcome to the Guessing Game!"
PRINT "I'm thinking of a number 1-100"

WHILE GUESS <> SECRET
  PRINT "Take a guess:"
  INPUT GUESS
  TRIES = TRIES + 1
  
  IF GUESS = SECRET THEN
    PRINT "You got it in " TRIES " tries!"
  ELSE IF GUESS < SECRET THEN
    PRINT "Too low, try again"
  ELSE
    PRINT "Too high, try again"
  END IF
WEND
```

## Common Commands Reference

### Output
| Command | Example | What it does |
|---------|---------|--------------|
| `PRINT` | `PRINT "Hi"` | Display text or value |
| `PRINT` | `PRINT A, B` | Display multiple values |

### Input
| Command | Example | What it does |
|---------|---------|--------------|
| `INPUT` | `INPUT X` | Get number from user |
| `INPUT` | `INPUT NAME$` | Get text from user |

### Variables
| Command | Example | What it does |
|---------|---------|--------------|
| `DIM` | `DIM ARR(10)` | Create array |
| `LET` | `LET X = 5` | Assign value |

### Control
| Command | Example | What it does |
|---------|---------|--------------|
| `IF/THEN` | `IF X > 5 THEN ...` | Conditional |
| `FOR/NEXT` | `FOR I = 1 TO 10 ...` | Loop N times |
| `WHILE/WEND` | `WHILE X < 10 ...` | Loop while true |
| `DO/UNTIL` | `DO ... UNTIL X=5` | Loop until true |

### Functions
| Command | Example | What it does |
|---------|---------|--------------|
| `SUB` | `SUB GREET(N$) ... END SUB` | Create subroutine |
| `FUNCTION` | `FUNCTION ADD(A,B) RETURN ...` | Create function |

### Math
| Function | Example | Result |
|----------|---------|--------|
| `ABS` | `ABS(-5)` | 5 |
| `INT` | `INT(3.7)` | 3 |
| `SQR` | `SQR(16)` | 4 |
| `RND` | `RND` | Random 0-1 |
| `SIN`, `COS`, `TAN` | `SIN(90)` | Trigonometry |

### String
| Function | Example | Result |
|----------|---------|--------|
| `LEN` | `LEN("Hi")` | 2 |
| `UPPER$` | `UPPER$("hi")` | "HI" |
| `LOWER$` | `LOWER$("HI")` | "hi" |
| `LEFT$` | `LEFT$("Hello", 2)` | "He" |
| `RIGHT$` | `RIGHT$("Hello", 2)` | "lo" |
| `MID$` | `MID$("Hello", 2, 3)` | "ell" |

## Practice Programs

### Program 1: Addition Calculator

```basic
PRINT "Enter first number:"
INPUT A
PRINT "Enter second number:"
INPUT B
SUM = A + B
PRINT "The sum is " SUM
```

### Program 2: Times Table

```basic
PRINT "Times table for what number?"
INPUT N

FOR I = 1 TO 12
  PRINT N " x " I " = " (N * I)
NEXT I
```

### Program 3: Average Calculator

```basic
DIM SCORES(5)
SUM = 0

FOR I = 1 TO 5
  PRINT "Enter score " I ":"
  INPUT SCORES(I)
  SUM = SUM + SCORES(I)
NEXT I

AVERAGE = SUM / 5
PRINT "Average: " AVERAGE
```

## Tips for Learning BASIC

1. **Start simple** - Just PRINT and INPUT first
2. **Use variables** - Store values for reuse
3. **Test conditionals** - IF/THEN are powerful
4. **Build loops** - Automate repetition
5. **Create functions** - Reuse code blocks
6. **Comment code** - Explain what you're doing
7. **Add graphics** - Combine with Logo commands

## Common Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| `IF X = 5` | Missing THEN | `IF X = 5 THEN ...` |
| `FOR I = 1 10` | Missing TO | `FOR I = 1 TO 10` |
| `X$ = 5` | Type mismatch | `X$ = "5"` (use quotes) |
| Missing `END IF` | Syntax error | Add `END IF` |
| Wrong quote type | Syntax error | Use `"` not other quotes |

## Next Steps

1. ✅ Complete tutorials above
2. 📂 Try examples from `Examples/basic/`
3. 🎮 Build a simple game
4. 🔗 Combine with Logo for graphics

---

Happy coding! 💻
