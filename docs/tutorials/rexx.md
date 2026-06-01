# REXX Tutorial

## Introduction

REXX (Restructured eXtended eXecutor) is a procedural language created by Mike Cowlishaw at IBM in 1979. It was designed to be easy to learn and read, and was widely used as a macro and scripting language on IBM systems (MVS, VM/CMS, OS/2, AmigaOS).

**Key characteristics:**
- Typeless: all values are strings
- Case-insensitive keywords (by convention UPPERCASE)
- Built-in string and arithmetic operations
- Simple procedure/subroutine mechanism

## Hello World

```rexx
SAY 'Hello, World!'
```

`SAY` outputs a line of text.

## Variables and Assignment

Variables are created by assignment (no declaration needed):

```rexx
name = 'Alice'
age = 30
SAY 'Name:' name
SAY 'Age:' age
```

Uninitialised variables have their own name as their value (e.g. `X = X`).

## Arithmetic

```rexx
a = 10
b = 3
SAY a + b    /* 13 */
SAY a - b    /* 7  */
SAY a * b    /* 30 */
SAY a / b    /* 3.33... */
SAY a // b   /* 1  (modulo) */
SAY a ** b   /* 1000 (power) */
```

## String Operations

```rexx
s = 'Hello'
SAY LENGTH(s)           /* 5 */
SAY UPPER(s)            /* HELLO */
SAY SUBSTR(s, 2, 3)     /* ell */
SAY s || ', World!'     /* Hello, World! */
SAY REVERSE(s)          /* olleH */
SAY COPIES('ab', 3)     /* ababab */
SAY CENTER('hi', 10, '*') /* ****hi**** */
```

## Control Flow

### IF / THEN / ELSE

```rexx
score = 85
IF score >= 90 THEN SAY 'A'
ELSE IF score >= 80 THEN SAY 'B'
ELSE SAY 'C or below'
```

### DO (counted loop)

```rexx
DO i = 1 TO 5
  SAY i
END
```

### DO WHILE

```rexx
n = 1
DO WHILE n <= 10
  SAY n
  n = n * 2
END
```

### SELECT / WHEN

```rexx
day = 'MON'
SELECT
  WHEN day = 'MON' THEN SAY 'Monday'
  WHEN day = 'FRI' THEN SAY 'Friday'
  OTHERWISE SAY 'Other day'
END
```

## Subroutines

```rexx
CALL Greet 'World'

Greet:
  who = ARG1
  SAY 'Hello,' who
  RETURN
```

Use `RESULT` to get a return value:

```rexx
total = CALL Add 3 4
SAY RESULT    /* 7 */

Add:
  RETURN ARG1 + ARG2
```

## Turtle Graphics

```rexx
PENDOWN
COLOR 'red'
DO i = 1 TO 4
  FORWARD 50
  RIGHT 90
END
```

## Key Built-ins

| Function | Description |
|----------|-------------|
| `LENGTH(s)` | Length of string |
| `SUBSTR(s,n,l)` | Substring |
| `UPPER(s)` / `LOWER(s)` | Case conversion |
| `STRIP(s)` | Remove leading/trailing whitespace |
| `POS(needle,hay)` | Position of substring |
| `WORD(s,n)` | Nth word |
| `WORDS(s)` | Word count |
| `ABS(n)` | Absolute value |
| `MAX(a,b)` / `MIN(a,b)` | Maximum/minimum |
| `DATATYPE(s)` | Returns `NUM` or `CHAR` |
