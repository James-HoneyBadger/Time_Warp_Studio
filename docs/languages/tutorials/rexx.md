# REXX: strings, all the way down

REXX (Restructured Extended Executor) was written by Mike Cowlishaw at IBM in 1979
as a scripting language for VM/CMS, where its job was to glue other programs
together. It has one type: every value is a string, and a string that looks like a
number is treated as one when arithmetic asks. Variables are assigned by naming
them, and nothing is declared. The interpreter runs it line by line.

Press **F9** to run a document. The output blocks below are what the program prints.

## What a program looks like

```rexx
SAY 'Hello, World!'
```

```text
Hello, World!
```

`SAY` writes its expression and a newline. Strings take single or double quotes, and
a quote of the other kind inside them needs no escape. There is no keyword for the
end of a script: it ends when it runs out of lines.

## Step 1: variables and joining text

Give two names values and join them three ways.

```rexx
name = 'Time Warp'
count = 3
SAY 'Hello' || ' ' || 'World'
SAY 'count is' count
```

```text
Hello World
count is3
```

`=` assigns with no keyword. Two values written next to each other are joined with
nothing between them, which REXX calls abuttal, and `||` joins explicitly. That is
why the last line prints `count is3`, with the space written where it is wanted.
`SAY` prints one line per call, so a report is a sequence of `SAY`s.

## Step 2: loops

Count up, count down by three, and walk a list of words.

```rexx
DO i = 1 TO 3
  SAY i
END
DO n = 10 TO 1 BY -3
  SAY n
END
DO word OVER 'alpha beta gamma'
  SAY word
END
```

```text
1
2
3
10
7
4
1
alpha
beta
gamma
```

`DO var = a TO b` counts in steps of one and `BY` changes the step, negative
included. `DO word OVER '…'` walks the words of a string, which is the loop a
text-processing language is expected to have. `DO WHILE condition` and
`DO UNTIL condition` test at the top and at the foot, `LEAVE` leaves a loop and
`ITERATE` skips to its next pass.

## Step 3: decisions

Test a mark with `IF`, then with `SELECT`.

```rexx
score = 87
IF score > 50 THEN SAY 'pass'
SELECT
  WHEN score > 90 THEN SAY 'huge'
  WHEN score > 60 THEN SAY 'medium'
  OTHERWISE SAY 'small'
END
```

```text
pass
medium
```

`IF … THEN` takes one statement, and `DO … END` groups several. `SELECT` is the
multi-way form: each `WHEN` is a condition and its statement, the first true one
wins and `OTHERWISE` catches the rest. A long set of tests belongs in a `SELECT`,
where every condition lines up at the same indent.

## Step 4: text functions and parsing

Split a string into words and ask a few functions what they make of it.

```rexx
line = 'red green blue'
PARSE VAR line first second third
SAY third
SAY UPPER('rexx')
SAY SUBSTR('abcdef', 2, 3)
SAY WORD('one two three', 2)
```

```text
blue
REXX
bcd
two
```

`PARSE VAR` splits on whitespace and gives each word to the next name, which is how
a line of input is taken apart. The functions count from 1, so `SUBSTR(s, 2, 3)`
starts at the second character and takes three. `LENGTH`, `WORDS`, `INDEX`, `LEFT`,
`STRIP` and `REVERSE` are the rest of the set.

## Step 5: subroutines

Call a labelled block and read the answer from `RESULT`.

```rexx
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

```text
6! =720
```

`CALL` runs the labelled block with its arguments, and `ARG(1)` is the first of
them, so arguments arrive by the same mechanism as everything else. `RETURN value`
gives the answer back in `RESULT`, which the caller reads as a variable rather than
as the value of the call. The `EXIT` before the label is load-bearing: without it
the block runs a second time when the script walks into it.

## Step 6: the turtle

Draw a square spiral with the turtle words.

```rexx
DO i = 1 TO 18
  FORWARD i * 8
  RIGHT 90
END
HOME
```

```text
-- 19 line(s), 0 shape(s)
```

The turtle words are a Time Warp extension: `FORWARD` (or `MOVE`) goes forward,
`BACK` reverses, `RIGHT` (or `TURN`) turns clockwise, `LEFT` anticlockwise, `PENUP`
and `PENDOWN` control inking and `HOME` returns to the centre. The loop turns a
quarter each time, so the arms grow into a square spiral, and `HOME` puts the turtle
back where the next drawing expects it.

## Where it catches people out

**A label is only an address.** A script runs into its subroutines unless something
stops it, so the `EXIT` above is not tidiness.

```rexx
SAY 'before'
CALL greet
SAY 'after'

greet:
  SAY 'inside'
  RETURN
```

```text
before
inside
after
inside
```

The `RETURN` went back to the caller, and the script then walked into `greet:` on
its own. With `EXIT` before the label the block is only ever reached by `CALL`.

**A blank between terms is not a space.** Abuttal joins without one, so a number
after a word sits against it: `SAY 'count is' count` prints `count is3`. Write the
space inside the string, or join with `||`.

**`/` is real division.** `10 / 4` is `2.5`; `%` is the integer division and `//`
the remainder. Reaching for `/` and expecting `2` gives a number that then fails a
`DO` loop's test.

## Commands used here

| Word | What it does |
| --- | --- |
| `SAY expr` | Prints a value and ends the line |
| `=` | Assigns, with no keyword |
| `\|\|` | Joins two values with nothing between them |
| `DO … END` | A block; also a counted, conditional or list loop |
| `IF … THEN … ELSE` | Chooses a branch |
| `SELECT / WHEN / OTHERWISE` | Chooses between many branches |
| `PARSE VAR` | Splits a string into words |
| `CALL label` | Calls a labelled block |
| `RETURN value`, `RESULT` | Answers a value back to the caller |
| `EXIT` | Stops the script |
| `FORWARD`, `RIGHT`, `HOME` | Drive the turtle |

## Next

The [language guide](../reference.md#rexx) lists every built-in function and
the arithmetic operators.
[`Examples/rexx/strings.rexx`](../../../Examples/rexx/strings.rexx) is a tour of the
text functions, and
[`Examples/rexx/subroutines.rexx`](../../../Examples/rexx/subroutines.rexx) shows
`CALL` in a longer program.
