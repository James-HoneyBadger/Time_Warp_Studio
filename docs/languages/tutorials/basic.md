# BASIC: the home computer's first language

BASIC was written at Dartmouth College in 1964 so that students who were not
computer scientists could write a program in an afternoon. This dialect is the one
the home machines shipped in ROM: numbered lines, `PRINT`, `FOR`, `GOTO`, and a
direct mode that runs a single statement. It runs line by line. Its line numbers
are addresses for `GOTO` and `GOSUB`, not a sort order.

Press **F9** to run a document. Each output block below is what the program prints.

## What a program looks like

```basic
10 PRINT "Hello, World!"
20 END
```

```text
Hello, World!
```

Line numbers are optional. The same program without them prints the same line. They
exist so that another statement can name a line, which is how subroutines work below.
`END` is not decoration: without it the program runs off the end of the file.

## Step 1: variables and the two PRINT separators

Store three numbers and print them with both separators.

```basic
10 LET PRICE = 4.5
20 LET QUANTITY = 3
30 TOTAL = PRICE * QUANTITY
40 PRINT "Total:"; TOTAL
50 PRINT "Total:", TOTAL
60 REM the semicolon and the comma both print one space
70 PRINT "Price is"; PRICE
```

```text
Total: 13.5
Total: 13.5
Price is 4.5
```

`LET` is optional, so line 30 assigns without it. The semicolon and the comma
separate items the same way: one space, then the next item. A name ending in `$`
holds text and everything else holds a number. Names are not case sensitive, so
`total` and `TOTAL` are one variable.

## Step 2: decisions

Compare a mark against two boundaries to choose one of three grades.

```basic
10 LET MARK = 64
20 IF MARK >= 90 THEN
30   PRINT "grade A"
40 ELSE
50   IF MARK >= 60 THEN
60     PRINT "grade B"
70   ELSE
80     PRINT "grade C"
90   END IF
100 END IF
```

```text
grade B
```

A multi-line `IF` ends with `END IF`, and each `IF` needs its own. There is no
`ELSEIF`. A third branch is a second `IF` nested in the `ELSE`, so the program carries
two `END IF` lines. A single-line form exists for one action, as in
`IF MARK > 50 THEN PRINT "pass"`. The comparisons are `=`, `<>`, `<`, `>`, `<=` and
`>=`, joined with `AND`, `OR` and `NOT`.

## Step 3: counted loops

Count up with a square beside each number, then count down by four.

```basic
10 FOR I = 1 TO 5
20   PRINT I; I * I
30 NEXT I
40 FOR J = 10 TO 2 STEP -4
50   PRINT J
60 NEXT J
```

```text
1 1
2 4
3 9
4 16
5 25
10
6
2
```

`FOR` names its variable, its first value and its limit. `STEP` changes the
increment, and a negative step counts down. `NEXT` closes the loop and may name the
variable or not. The loop runs while the variable has not passed the limit, so the
second loop stops at 2: its next value, -2, is below it.

## Step 4: arrays

Fill an array from a loop and read two elements back.

```basic
10 DIM SCORES(3)
20 FOR I = 1 TO 3
30   SCORES(I) = I * 10
40 NEXT I
50 PRINT SCORES(1); SCORES(3); SCORES(0)
```

```text
10 30 0
```

`DIM SCORES(3)` sets aside four slots, 0 to 3, and the program uses 1 to 3 as the
line-numbered machines did. Element 0 exists and answers 0 until something is stored
there, so the third number is 0 and not an error. Index 4 is refused as out of bounds.
There is one dimension only, so a table is a flat array with the arithmetic done by
hand.

## Step 5: subroutines by line number

Call the same block twice and let `RETURN` bring control back after each `GOSUB`.

```basic
10 GOSUB 100
20 GOSUB 100
30 END
40 PRINT "not reached"
100 PRINT "row"; ROWS
110 ROWS = ROWS + 1
120 RETURN
```

```text
row 0
row 1
```

`GOSUB 100` records where it came from and jumps to line 100; `RETURN` goes back to
the statement after the `GOSUB`, so the second call resumes at line 20. Line 30
matters. Without `END` the program would fall into the subroutine a third time, and
that is the classic way a BASIC program prints one row too many. `ROWS` was never
assigned, and an unassigned variable counts as 0.

## Step 6: a graphics screen

Switch to graphics mode and draw five bars and a circle.

```basic
10 SCREEN 1
20 FOR BAR = 0 TO 4
30   LINE (40 + BAR * 60, 300)-(70 + BAR * 60, 300 - BAR * 45), 10, BF
40 NEXT BAR
50 CIRCLE (400, 120), 50, 14
```

```text
-- 38 line(s), 5 shape(s), screen
```

The program prints nothing of its own: the result is the picture. `SCREEN 1` gives
an 800x600 surface with (0, 0) at the top left, so the numbers are pixel positions
and y grows downwards. `LINE` takes two opposite corners, a colour number and `BF`
for "both", which fills the rectangle as well as outlining it. `CIRCLE` takes a
centre and a radius. The run report counts strokes and shapes. That is how a drawing
is checked when there is nothing to read.

## Where it catches people out

**Line numbers do not order the program.** They are addresses. The program runs in
the order the lines sit in the file.

```basic
30 PRINT "third"
10 PRINT "first"
20 PRINT "second"
```

```text
third
first
second
```

Write the lines in order, with numbers that rise down the file, and the same three
statements print in the order a reader expects.

**A trailing separator does not hold the line open.** `PRINT "a";` ends the line, so
the next `PRINT` starts a new one.

```basic
10 PRINT "a";
20 PRINT "b"
```

```text
a
b
```

To join text instead, build one string with `+`. That is what `STR$` is for when a
number is involved: `PRINT "row" + STR$(ROWS)`.

**`ELSEIF` is not a word.** It reports an error on its line and execution carries on
at the next statement, so a three-way test quietly prints the wrong branch. Nest the
second `IF` in the `ELSE` instead, with its own `END IF`, as step 2 does.

## Commands used here

| Statement | What it does |
| --- | --- |
| `PRINT` | Prints items, separated by `;` or `,`, then ends the line |
| `LET` | Assigns; the word itself is optional |
| `REM` | A comment to the end of the line |
| `IF … THEN … ELSE … END IF` | Chooses a branch |
| `FOR … TO … STEP … NEXT` | Counts a variable over a range |
| `DIM` | Sets aside an array, indexed from 0 |
| `GOSUB` / `RETURN` | Calls a numbered line and comes back |
| `END` | Stops the program |
| `SCREEN`, `LINE`, `CIRCLE` | Choose a screen and draw on it |

## Next

The [language guide](../reference.md#basic) carries the rest: `INPUT`,
`WHILE`, `SUB`, `FUNCTION`, sprites, sound and the numbered screen modes.
[`Examples/basic/star_patrol.bas`](../../../Examples/basic/star_patrol.bas) is a
whole game built on this much.
