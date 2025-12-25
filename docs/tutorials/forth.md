# Forth Tutorial

**Forth** is a stack-based language where you work with a data stack. Operations push values onto the stack and consume them in reverse order. It's elegant, fast, and great for learning how computers actually work.

## Getting Started

### Hello, World! 👋

```forth
: HELLO ." Hello, World!" ;
HELLO
```

**Output:**
```
Hello, World!
```

`:` defines a new word (function)
`." ... "` prints text

## Stack Basics

### Understanding the Stack

Forth works with a **stack** of numbers:

```forth
5          ( push 5 onto stack: [5] )
3          ( push 3 onto stack: [5, 3] )
+          ( add top two: [8] )
.          ( pop and print: prints 8 )
```

The `.` operator pops the top value and prints it.

### The Forth Stack in Action

```forth
10 20 30 .     ( prints 30, stack is [10, 20] )
.              ( prints 20, stack is [10] )
.              ( prints 10, stack is empty )
```

### Stack Manipulation

```forth
5 DUP          ( duplicate: [5, 5] )
.              ( prints 5 )
.              ( prints 5 )

10 20 SWAP     ( swap top two: [20, 10] )
.              ( prints 10 )
.              ( prints 20 )

5 10 20 DROP   ( remove top: [5, 10] )
.              ( prints 10 )
```

## Arithmetic

```forth
5 3 + .        ( 8 )
10 4 - .       ( 6 )
3 4 * .        ( 12 )
20 5 / .       ( 4 )
17 5 MOD .     ( 2 - remainder )
```

### Order Matters (Reverse Polish Notation)

```forth
( Traditional math: 3 + 4 = 7 )
( Forth: 3 4 + = 7 )

3 4 + .        ( prints 7 )
10 5 - .       ( prints 5 )
2 3 * .        ( prints 6 )
```

## Defining Words (Functions)

### Simple Definition

```forth
: SQUARE DUP * ;

5 SQUARE .     ( prints 25 )
7 SQUARE .     ( prints 49 )
```

`DUP` duplicates top of stack, `*` multiplies.

### With Multiple Operations

```forth
: CUBE DUP DUP * * ;

3 CUBE .       ( prints 27 )

: DOUBLE DUP + ;

5 DOUBLE .     ( prints 10 )
```

### Building Complex Words

```forth
: SUM3 + + ;           ( add three numbers )

1 2 3 SUM3 .           ( prints 6 )

: AVERAGE SUM3 3 / ;   ( divide by 3 - stack: [sum/3] )

10 20 30 AVERAGE .     ( prints 20 )
```

## Output

### Printing Text and Numbers

```forth
: GREET ." Hello" ;
GREET              ( prints: Hello )

: SHOW-NUM ." The number is: " . ;
42 SHOW-NUM        ( prints: The number is: 42 )
```

### Multiple Outputs

```forth
: PAIR . . ;
1 2 PAIR           ( prints 2, then 1 - order matters! )

: SHOW-TWO ." First: " . ." Second: " . ;
5 10 SHOW-TWO      ( prints: First: 10 Second: 5 )
```

## Variables and Memory

### Simple Variables

```forth
VARIABLE X
5 X !             ( store 5 into X using ! )
X @ .             ( retrieve X using @ and print )
```

**Output:**
```
5
```

`!` is "store", `@` is "fetch"

### Modifying Variables

```forth
VARIABLE COUNTER
0 COUNTER !        ( initialize to 0 )
COUNTER @ 1 + COUNTER !    ( increment )
COUNTER @ .        ( print 1 )
```

### Variables in Words

```forth
VARIABLE SUM
0 SUM !

: ADD-TO-SUM SUM @ + SUM ! ;

5 ADD-TO-SUM       ( add 5 to SUM )
10 ADD-TO-SUM      ( add 10 to SUM )
SUM @ .            ( prints 15 )
```

## Conditionals

### IF/THEN

```forth
: CHECK DUP 0= IF ." Zero!" THEN ;

0 CHECK            ( prints: Zero! )
5 CHECK            ( prints nothing )
```

`0=` tests if value is zero

### IF/ELSE/THEN

```forth
: SIGN
  DUP 0 > IF ." Positive" ELSE
  DUP 0 < IF ." Negative" ELSE
  ." Zero"
  THEN THEN
;

5 SIGN             ( prints: Positive )
-3 SIGN            ( prints: Negative )
0 SIGN             ( prints: Zero )
```

## Loops

### DO/LOOP

```forth
: COUNTDOWN 10 0 DO I . LOOP ;
COUNTDOWN
```

**Output:**
```
0 1 2 3 4 5 6 7 8 9
```

`I` gives current index, `LOOP` increments

### Counting from 1 to N

```forth
: COUNT 1+ 1 DO I . LOOP ;
5 COUNT            ( prints: 1 2 3 4 5 )
```

### Nested Loops

```forth
: MULTIPLICATION-TABLE
  10 0 DO
    CR 10 0 DO J I * . LOOP
  LOOP
;
MULTIPLICATION-TABLE
```

`J` gets outer loop index, `CR` is carriage return

## Arrays

### Creating Arrays

```forth
CREATE ARRAY 10 ALLOT   ( array of 10 cells )
0 ARRAY !              ( store 0 at index 0 )
5 ARRAY 4 + !          ( store 5 at index 1 )

ARRAY @ .              ( retrieve and print: 0 )
ARRAY 4 + @ .          ( retrieve and print: 5 )
```

### Array with Loop

```forth
CREATE SQUARES 10 ALLOT

: FILL-SQUARES
  10 0 DO
    I DUP * SQUARES I 4 * + !
  LOOP
;

FILL-SQUARES
```

## Complete Example: Factorial

```forth
: FACTORIAL
  DUP 1 > IF
    DUP 1 - RECURSIVE * 
  ELSE
    DROP 1
  THEN
;

5 FACTORIAL .      ( prints 120 )
```

## Complete Example: FizzBuzz

```forth
: FIZZBUZZ
  100 1 DO
    I DUP
    15 MOD 0= IF ." FizzBuzz " DROP ELSE
    DUP 3 MOD 0= IF ." Fizz " DROP ELSE
    DUP 5 MOD 0= IF ." Buzz " DROP ELSE
    . 
    THEN THEN THEN
  LOOP
;

FIZZBUZZ
```

## Comments

```forth
\ Single line comment

( Block comment )

: EXAMPLE
  5 3 +           \ Add them
  .               \ Print result
;
```

## Useful Words Reference

### Stack Manipulation
| Word | Stack | Purpose |
|------|-------|---------|
| `DUP` | (n -- n n) | Duplicate |
| `DROP` | (n -- ) | Remove |
| `SWAP` | (n1 n2 -- n2 n1) | Swap |
| `ROT` | (n1 n2 n3 -- n2 n3 n1) | Rotate |
| `OVER` | (n1 n2 -- n1 n2 n1) | Copy first |

### Arithmetic
| Word | Operation |
|------|-----------|
| `+` | Add |
| `-` | Subtract |
| `*` | Multiply |
| `/` | Divide |
| `MOD` | Modulo |

### Comparison
| Word | Test |
|------|------|
| `=` | Equal |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less or equal |
| `>=` | Greater or equal |
| `0=` | Is zero |
| `0<` | Is negative |
| `0>` | Is positive |

### Control
| Word | Purpose |
|------|---------|
| `IF/THEN` | Conditional |
| `IF/ELSE/THEN` | Conditional with else |
| `DO/LOOP` | Loop N times |
| `BEGIN/UNTIL` | Loop until condition |

### I/O
| Word | Purpose |
|------|---------|
| `.` | Print number |
| `."text"` | Print string |
| `CR` | New line |

### Variables
| Word | Purpose |
|------|---------|
| `VARIABLE` | Create variable |
| `!` | Store (write) |
| `@` | Fetch (read) |

## Stack Notation

Forth uses this notation to describe words:

```
(before -- after)
DUP     ( n -- n n )
DROP    ( n -- )
SWAP    ( n1 n2 -- n2 n1 )
+       ( n1 n2 -- sum )
```

Read as: what's on the left goes in, what's on the right comes out.

## Tips for Learning Forth

1. **Visualize the stack** - Draw it out on paper
2. **Test incrementally** - Define and test each word
3. **Use stack notation** - Document what your words do
4. **Build bottom-up** - Simple words first
5. **Trace execution** - Follow what happens step by step

## Common Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| Wrong order | `5 3 -` = -2, not 2 | Remember reverse Polish |
| Stack underflow | Popping empty stack | Check word needs values |
| Forgetting DUP | Using value twice | Use `DUP` to duplicate |
| Wrong word order | `5 SQUARE 3 +` | Operations apply to stack top |
| Missing semicolon | Word definition incomplete | Always end with `;` |

## Forth vs Other Languages

| Feature | Forth | BASIC | Pascal |
|---------|-------|-------|--------|
| Paradigm | Stack-based | Imperative | Imperative |
| Syntax | RPN | Keywords | Keywords |
| Memory | Explicit | Auto | Auto |
| Learning curve | Steep | Gentle | Medium |

## Next Steps

1. ✅ Learn stack operations
2. ✅ Define simple words
3. ✅ Build loops and conditionals
4. ✅ Create useful programs
5. 📂 Try examples from `Examples/forth/`

---

Happy stack programming! 📚
