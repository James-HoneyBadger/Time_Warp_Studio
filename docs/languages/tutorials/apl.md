# APL: the shape of the data is the program

APL is Kenneth Iverson's notation of 1962, made interactive as APL\360 in 1966. It is
the one language here where the array is the unit of thought: `+/⍳100` is the sum of
the first hundred numbers, with no loop and no variable. Two properties matter before
the first example. There is no operator precedence, and most functions work on whole
arrays, not on single numbers. The interpreter runs it line by line, and each line is
an expression whose value is displayed. That is how APL's original terminal behaved.

Press **F9** to run a document. Each line's value appears as the program runs.

## What a program looks like

```apl
⍳5
```

```text
1 2 3 4 5
```

A line is an expression and its value is printed. `⍳` is iota, which gives the first
five whole numbers. There are no statements in the sense of other languages. A
program is a list of expressions, and the ones a program wants silently are
assignments, which print nothing.

## Step 1: no precedence

Put two functions on one line and see which goes first.

```apl
2 × 3 + 4
(2 × 3) + 4
```

```text
14
10
```

A function takes everything to its right as its argument, so `×` receives `3 + 4` and
the sum happens first. Evaluation runs right to left, and parentheses are the only
way to change that. The same rule makes `2 3 ⍴ ⍳6` a two-by-three table instead of an
error: the `⍳6` is finished before `⍴` looks at it.

## Step 2: strands and pervasive functions

Put numbers side by side to make a vector, then add one.

```apl
1 2 3 + 10
1 2 3 + 4 5 6
```

```text
11 12 13
5 7 9
```

A row of values written with spaces between them is a strand, and a strand on the
left of a function is that function's left argument. Arithmetic is pervasive. The
`+` reaches into the array, so a scalar is added to every element and two vectors are
added element by element. Vectors of different lengths are a length error, not a
quiet truncation, and the message names both lengths.

## Step 3: shape

Ask what an array's shape is, and change it.

```apl
⍴ 1 2 3 4 5 6
2 3 ⍴ ⍳6
⍉ 2 3 ⍴ ⍳6
```

```text
6
1 2 3
4 5 6
1 4
2 5
3 6
```

A table is printed a row per line. That is the only visual cue APL gives that the
data has a shape. `⍴` with one argument answers the shape and with two reshapes:
`2 3 ⍴` fills two rows of three from the data, taking it in order. `⍉` transposes.
The count of the dimensions is not limited to two, and nothing prevents a reshape
that changes the number of elements as long as the data can be repeated or trimmed.

## Step 4: reordering and selecting

Reverse, take more than there is, and sort.

```apl
⌽ 1 2 3 4
3 ↑ 1 2
⍒ 1 3 2
```

```text
4 3 2 1
1 2 0
2 3 1
```

`⌽` reverses with one argument and rotates by a count with two. `↑` takes from the
front, and taking more than there is pads with zeros, so `3 ↑ 1 2` answers `1 2 0`.
`⍒` is grade down. It answers the positions that would sort the data, not the sorted
data itself, so `2 3 1` means the second item belongs first, then the third, then the
first. The answer is an index, and the value it points at is the caller's next step.

## Step 5: reduce and scan

Add a whole vector with one character, then keep the running totals.

```apl
+/⍳100
+SCAN 1 2 3
+ / 1 2 3 × 4 5 6
```

```text
5050
1 3 6
32
```

`/` is an operator, not a function. It takes the function on its left and inserts it
between the items of the vector, so `+/1 2 3` is `1+2+3`. `\` is the same idea keeping
each partial answer, and `SCAN` is its keyword spelling. The third line shows why the
lack of precedence is worth learning: `1 2 3 × 4 5 6` is one vector of products, and
the `+ /` in front of it adds them. Every function has a keyword spelling as well as a
glyph, so a keyboard without `⍳` can still write APL.

## Step 6: the turtle

Draw a hexagon and read the turtle's position.

```apl
FD 60
RT 60
FD 60
RT 60
FD 60
RT 60
FD 60
RT 60
FD 60
RT 60
FD 60
RT 60
TX
TY
THETA
```

```text
0
0
0
```

The turtle words `FD`, `BK`, `RT`, `LT`, `PU`, `PD`, `HOME` and `CS` are a Time Warp
extension, since APL had no graphics. They take their distance as an argument, so the
six moves above are six separate lines. `TX`, `TY` and `THETA` answer the position and
heading, and they are named that way on purpose. `X`, `Y` and `H` are ordinary APL
variables and taking them would break the most natural line anyone writes. After a
closed hexagon the turtle is back where it started, facing where it started, so the
three numbers are the starting pose.

## Where it catches people out

**`*` is power.** APL predates C, so `2 * 3` is `8`, not `6`, and `×` is the
multiplication sign. A program ported from another language gets numbers that are
wrong, not an error message.

```apl
2 * 3
2 × 3
```

**`¯` is the minus sign.** A negative number is written with a raised bar, and the
ASCII hyphen is subtraction. `¯3` is minus three; `3-5` is a calculation that
answers `¯2`.

```apl
¯3 + 1
```

**A function takes the rest of the line.** Adding a second operation to the right
changes what the first one receives, and there is no precedence rule to save the
program: `2 × 3 + 4` is 14. When the order in the head is not the order on the line,
parentheses are not optional.

**Some notation is absent.** Outer product `∘.`, rank `⍤`, the system
functions `⎕` and branch `→` are not implemented, and an indexing bracket is not
APL here either: `A[1]` is refused, since the guide's functions answer whole arrays
and the indexing step is a separate idea. A program that needs one is told which
symbol stopped it, not given a wrong answer.

## Symbols used here

| Glyph | Keyword | What it does |
| --- | --- | --- |
| `+` `-` `×` `÷` | `PLUS` `MINUS` `TIMES` `DIVIDE` | Arithmetic, element by element |
| `*` `⍟` | `POWER` `LOG` | Power and logarithm |
| `⌈` `⌊` | `CEILING` `FLOOR` | Round up, or down |
| `⍳` | `IOTA` | The first n whole numbers |
| `⍴` | `SHAPE` / `RESHAPE` | Shape with one argument, reshape with two |
| `⌽` | `REVERSE` / `ROTATE` | Reverse, or rotate |
| `↑` `↓` | `TAKE` `DROP` | Take from the front, or drop from it |
| `⍉` | `TRANSPOSE` | Transpose |
| `⍋` `⍒` | `GRADEUP` `GRADEDOWN` | Positions that sort the data |
| `/` `\` | `REDUCE` `SCAN` | Reduce to one value, or keep running values |
| `⍝` | | A comment to the end of the line |
| `FD`, `RT`, `TX` | | Turtle words and the turtle's pose |

## Next

The [language guide](../reference.md#apl) has the comparison and logic
functions, the defined-function brackets `∇ … ∇`, and what APL here deliberately
does not implement.
[`Examples/apl/arrays_and_turtle.apl`](../../../Examples/apl/arrays_and_turtle.apl) is
a worked program, and
[`Examples/apl/showcase.apl`](../../../Examples/apl/showcase.apl) is a longer one.
