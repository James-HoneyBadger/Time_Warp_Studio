# FOCAL: ten commands and a two-part label

FOCAL, the FOrmula CALculator, is DEC's line-numbered teaching language from 1969,
written to be learned in an afternoon: ten commands, one number type, and labels of
the form `NN.NN` so that a group of five lines can be named as a unit. Every command
has a single letter and a full name, and both work. Two things surprise a reader who
knows BASIC. Branching is on the sign of a value, and a loop runs over the rest of
its line. The interpreter runs it line by line.

Press **F9** to run a document. The output blocks below are what the program prints.

## What a program looks like

```focal
01.10 T "Hello, World!"
```

```text
Hello, World!
```

The label is part of the statement, not a separate line: `01.10` is group 1, line 10.
`T` is TYPE and its text is quoted. There is no `END` and no `RUN`. A program stops
when it runs out of lines or reaches `Q`.

## Step 1: TYPE and the number format

Print text, a calculation and a floor.

```focal
01.10 T "Hello, World!"
01.20 T "X=",2+3
01.30 T %0,"R=",7/2
01.40 T %2,"R=",7/2
```

```text
Hello, World!
X=5.00
R=4
R=3.50
```

`T` prints its items with no space between them, so `"X="` and the number sit
together. Every number prints with two decimal places by default, so the sum arrives
as `5.00`. `%0` sets the format to no decimals, `%5` would set five. The setting
belongs to the session, not to the statement: it holds until the next `%n`, so line
01.40 has to ask for two places again.

## Step 2: variables

Assign two numbers, print a third that was never assigned, then erase them.

```focal
01.10 S X=5
01.20 S Y=X*2+1
01.30 T "X=",X
01.40 T "Y=",Y
01.50 T "Z=",Z
01.60 E ALL
01.70 T "X=",X
```

```text
X=5.00
Y=11.00
Z=0.00
X=0.00
```

`S` is SET, and there is one type of value: a number. `A` is ASK, which prompts for
a number and stores it, and is the other way a variable gets a value. `Z` was never
given a value and prints `0.00`, because every variable is zero when a program loads.
That is the opposite of many languages and it is worth knowing for a practical
reason. A typo in a name prints 0 instead of reporting itself, so a number that is
unexpectedly zero is usually a spelling mistake. `E ALL` erases every variable and
sends them back to zero.

## Step 3: branching on a sign

Send a value to one of three labels by testing whether it is negative, zero or
positive.

```focal
01.10 S N=0
01.20 I (N) 1.40,1.30,1.50
01.30 T "ZERO"
01.35 GOTO 1.60
01.40 T "NEG"
01.45 GOTO 1.60
01.50 T "POS"
01.60 T "end"
```

```text
ZERO
end
```

FOCAL has no relational operators and no `IF X > 5`. `I (expression)` takes three
labels: the first when the value is negative, the second when it is zero and the third
when it is positive. A label left out falls through to the next line, so `I (N) ,,1.50`
reads "if N is positive, go to 1.50". To test for a particular value, subtract it:
`I (SIDES-4)` is zero at exactly four sides and the zero label can say so.

## Step 4: FOR over the rest of the line

Repeat the statements after the loop on the same line.

```focal
01.10 FOR I=1,3;T %0,"I=",I
01.20 T %0,"after the loop I=",I
01.30 FOR J=10,-3,1;T %0,"J=",J
01.40 T "done"
```

```text
I=1
I=2
I=3
after the loop I=3
J=10
J=7
J=4
J=1
done
```

`FOR` takes the variable and up to three numbers, and the order is start, step, limit:
`FOR I=1,3` counts up by one to 3, and `FOR J=10,-3,1` starts at 10, steps by -3 and
stops at 1. The `;` separates the loop from its body and is not a general statement
separator: `T "A";T "B"` is refused. After a loop the variable holds the last value
the body saw. `I` is 3, not the 4 that failed the test.

## Step 5: DO and RETURN

Call a group of lines and come back to the line after the call.

```focal
01.10 DO 2.10
01.20 T "back"
01.30 Q
01.40 T "not reached either"
02.10 T "inside"
02.20 RETURN
02.30 T "not reached"
```

```text
inside
back
```

`DO 2.10` runs group 2 from line 10 and `RETURN` hands control to the line after the
`DO`. `Q` is QUIT and stops the program where it stands, so line 01.40 never prints.
FOCAL-69 had no `RETURN`: a group called that way was expected to end where the
program ended, so old listings put their groups after the `Q`. `RETURN` is the one
command this implementation adds, and a `RETURN` with no group around it is reported
instead of quietly stopping the program.

## Step 6: the turtle

Draw a square with a loop that also draws.

```focal
01.10 S SIDE=70
01.20 FOR I=1,4;FD SIDE;RT 90
01.30 T "square drawn"
```

```text
square drawn
```

The turtle words `FD`/`FORWARD`, `BK`/`BACK`, `RT`/`RIGHT`, `LT`/`LEFT`, `PU`,
`PD` and `HOME` are a Time Warp extension: FOCAL in 1969 had no graphics at all. They
take their distance from a variable as readily as from a number, so `SIDE` is set
first. The run report counts four strokes here, one for each pass of the loop.

## Where it catches people out

**The middle number of a FOR is the step.** `FOR I=1,3` counts 1, 2, 3 because the
step defaults to 1. `FOR I=1,10,2` is not "from 1 to 10 by 2" in this implementation:
it starts at 1 and steps by 10 until it passes 2, which is one pass.

```focal
01.10 FOR I=1,10,2;T %0,"I=",I
```

Write the limit second and the step third, or write the step out as the middle number
when it is not 1.

**A label is two numbers.** `01.30` is group 1, line **30**, so it is reached as
`1.30`; `1.3` is group 1 line **3**, which is a different line and probably does not
exist.

```focal
01.10 GOTO 1.3
01.30 T "THIRTY"
```

The reference is reported as `no line labelled 1.3` and execution continues on the
next line, so the program prints something and looks almost right. The parts compare
as numbers, so `01.10` and `1.10` do name the same line.

**A branch falls through.** Reaching a target line does not skip the lines under it,
so a three-way branch without `GOTO` lines prints the rest of the group.

```focal
01.20 I (N) 1.40,1.30,1.50
01.30 T "ZERO"
01.40 T "NEG"
01.50 T "POS"
```

With `N` at 0 that prints `ZERO`, `NEG` and `POS`, because execution continues from
line 01.30 to the end of the program.

## Commands used here

| Command | Letter | What it does |
| --- | --- | --- |
| `TYPE` | `T` | Print text, formats and expressions |
| `SET` | `S` | Assign |
| `ASK` | `A` | Prompt for a number |
| `IF` | `I` | Three-way branch on the sign |
| `FOR` | `F` | Loop over the rest of the line |
| `DO` | `D` | Call a labelled group |
| `RETURN` | `R` | Leave the group and go back |
| `GOTO` | `G` | Jump to a label |
| `COMMENT` | `C` | The rest of the line is text |
| `ERASE` | `E` | Forget variables |
| `QUIT` | `Q` | Stop |
| `%n` | | n decimal places from here on |

## Next

The [language guide](../reference.md#focal) lists the `F` functions such as
`FSQT`, `FSIN` and `FPI`, and the `!` that joins two items inside one `TYPE`.
[`Examples/focal/square.foc`](../../../Examples/focal/square.foc) draws with a loop,
and [`Examples/focal/numerics.foc`](../../../Examples/focal/numerics.foc) covers the
arithmetic.
