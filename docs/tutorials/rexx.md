# REXX Tutorial

REXX (the **R**estructured **E**xtended **E**xecutor) is a typeless
string-processing language. Every value is a string that happens to look like a
number when it needs to, variables never need declaring, and the language has
only a handful of words to learn.

Time Warp Studio runs REXX line-by-line.

---

## 1. Saying hello

```rexx
SAY 'Hello, World!'
```

`SAY` displays an expression on its own line. Strings use single or double
quotes.

## 2. Variables and concatenation

Assignment needs no keyword:

```rexx
name = 'Time Warp'
count = 3
SAY name
```

Two values placed next to each other (called *abuttal*) are joined with no
space; `||` joins explicitly:

```rexx
SAY 'Hello' 'World'          →  HelloWorld
SAY 'Hello' || ' ' || 'World' →  Hello World
```

## 3. Numbers

```rexx
SAY 6 * 7            →  42
SAY 10 % 3           →  3     (integer division)
SAY 10 // 3          →  1     (remainder)
SAY 2 ** 10          →  1024
```

Comparisons produce `1` (true) or `0` (false) and compare numerically when both
sides look like numbers.

## 4. Branching

Single-line form:

```rexx
IF score > 50 THEN SAY 'pass'
```

Block form uses `DO … END`:

```rexx
IF score >= 90 THEN DO
  SAY 'Grade A'
END
ELSE DO
  SAY 'Keep going'
END
```

`SELECT` chooses between many branches:

```rexx
SELECT
  WHEN score > 90 THEN SAY 'huge'
  WHEN score > 50 THEN SAY 'medium'
  OTHERWISE SAY 'small'
END
```

## 5. Loops

```rexx
DO i = 1 TO 5
  SAY i
END
```

```rexx
DO n = 10 TO 1 BY -3      /* 10, 7, 4, 1 */
  SAY n
END
```

```rexx
DO WHILE count < 3
  count = count + 1
  SAY count
END
```

```rexx
DO UNTIL count >= 3
  count = count + 1
END
```

```rexx
DO word OVER 'alpha beta gamma'
  SAY word
END
```

Inside a loop, `LEAVE` breaks out and `ITERATE` skips to the next pass.

## 6. Input and parsing

`PULL` reads one line of input:

```rexx
PULL name
SAY 'Hello' name
```

`PARSE VAR` splits an existing string into words:

```rexx
line = 'red green blue'
PARSE VAR line first second third
SAY third
```

## 7. Subroutines

Labels end with `:`; `CALL` jumps to them and `RETURN` comes back. Arguments are
read with `ARG(n)` and results are returned in the `RESULT` variable.

```rexx
SAY 'before'
CALL factorial 6
SAY '6! =' RESULT
EXIT

factorial:
  n = ARG(1)
  result = 1
  DO i = 2 TO n
    result = result * i
  END
  RETURN result
```

## 8. Built-in functions

| Function | Result |
| --- | --- |
| `LENGTH(s)` | Number of characters |
| `SUBSTR(s, start, len)` | Slice of a string |
| `WORD(s, n)` / `WORDS(s)` | Nth word / word count |
| `UPPER(s)` / `LOWER(s)` | Change case |
| `STRIP(s)` / `SPACE(s)` | Trim / normalise whitespace |
| `LEFT(s, n)` / `RIGHT(s, n)` | First / last n characters |
| `COPIES(s, n)` / `REVERSE(s)` | Repeat / reverse |
| `INDEX(s, sub)` | 1-based position (0 = not found) |
| `ABS(n)`, `MAX(a,b)`, `MIN(a,b)`, `TRUNC(n)`, `SIGN(n)` | Numeric helpers |

```rexx
SAY UPPER('rexx')            →  REXX
SAY LENGTH('hello')          →  5
SAY SUBSTR('abcdef', 2, 3)   →  bcd
```

## 9. Turtle graphics

`FORWARD` (or `MOVE`), `BACK`, `RIGHT` (or `TURN`), `LEFT`, `PENUP`,
`PENDOWN` and `HOME` drive the drawing canvas.

```rexx
DO i = 1 TO 18
  FORWARD i * 8
  RIGHT 90
END
HOME
```

## 10. Comments

`/* … */` and `--` both start a comment.

```rexx
/* compute a total */
SAY 'done'   -- trailing note
```

---

## Quick reference

| Statement | Purpose |
| --- | --- |
| `SAY expr` | Display a value |
| `PULL var` | Read a line of input |
| `var = expr` | Assignment |
| `IF … THEN … ELSE …` | Branching |
| `SELECT / WHEN / OTHERWISE` | Multi-way branch |
| `DO … END` | Repeat / loop / block |
| `label:` / `CALL` / `RETURN` | Subroutines |
| `EXIT` | End the program |
| `LEAVE` / `ITERATE` | Break / continue a loop |
