# Logo: drawing by telling a turtle where to go

Logo was written in 1967 at Bolt, Beranek and Newman, and became the usual way to
introduce children to programming because its turtle turns a program into a
picture. `FORWARD 100` moves the turtle 100 units along its heading and leaves a
line behind; `RIGHT 90` turns it. Logo runs as a whole program: the file is read, its
procedures are defined in the order they appear, and then the statements run.

Press **F9** to run a document. The output blocks below are what the program prints,
and for a drawing that is the run report's count of strokes.

## What a program looks like

```logo
FORWARD 100
RIGHT 90
FORWARD 100
```

```text
-- 2 line(s), 0 shape(s)
```

The turtle starts at the centre of the canvas facing up, and the drawing is an L:
one line up from the centre and one to the right of its end. Heading 0 is north and
the heading increases clockwise, which is the opposite of the mathematical
convention, so `RIGHT` is the natural turn and `LEFT` is its opposite. The run
report counts two strokes: the turtle moved twice with the pen down, and nothing was
filled or outlined.

## Step 1: REPEAT closes the shape

Draw the same square four times over with one line instead of eight.

```logo
REPEAT 4 [FD 100 RT 90]
```

```text
-- 4 line(s), 0 shape(s)
```

`REPEAT` takes a count and a bracketed list of commands. The brackets are the
argument: without them the command list has nowhere to end, and `REPEAT 4 FD 100`
is refused. `FD` and `RT` are the short spellings of `FORWARD` and `RIGHT`, and the
whole run of drawing words has them.

## Step 2: the pen, and moving without drawing

Draw a thick red square, travel on without inking anything, then draw a thin blue
one.

```logo
SETPENCOLOR "red
SETPENWIDTH 4
REPEAT 4 [FD 60 RT 90]
PU
FD 110
PD
SETPENCOLOR "blue
SETPENWIDTH 1
REPEAT 4 [FD 60 RT 90]
```

```text
-- 8 line(s), 0 shape(s)
```

`PU` lifts the pen and `PD` lowers it, so the `FD 110` between the squares moves the
turtle across the canvas and leaves nothing. That is how a picture is made of
separate parts: the pen is the only thing that decides whether a move marks the
canvas. The first square is thick and red, the second thin and blue. A colour is
written with a quote before the name, `"red`, which is Logo's way of saying "this
word is data, not a command".

## Step 3: variables

Hold the side length in a variable and double it between squares.

```logo
MAKE "SIDE 40
REPEAT 4 [FD :SIDE RT 90]
MAKE "SIDE :SIDE * 2
REPEAT 4 [FD :SIDE RT 90]
```

```text
-- 8 line(s), 0 shape(s)
```

`MAKE` gives a name a value and the name is quoted, because it is the word being
stored rather than its value. Reading it back needs a colon: `:SIDE` is the value.
The distinction is the whole of Logo's variable syntax and the most common source of
a program that draws nothing, since a bare `SIDE` is a word Logo has never been told
about and evaluates to 0.

## Step 4: a procedure

Name a shape and call it twice at different sizes.

```logo
TO SQUARE :SIZE
  REPEAT 4 [FD :SIZE RT 90]
END

SQUARE 60
PU
FD 90
PD
SQUARE 30
```

```text
-- 8 line(s), 0 shape(s)
```

`TO` begins a definition, names it, and lists its inputs after colons; `END` closes
it. The body is indented for the reader only, since `END` is what ends it. Once the
definition has been read, `SQUARE` is a command like `FORWARD`, and `SQUARE 30` runs
the same body with `:SIZE` holding 30. A procedure is the only place a parameter
lives, so a shape that must work at two sizes is written once and called twice.

## Step 5: recursion

Let a procedure call itself on a smaller size to grow a tree.

```logo
TO TREE :SIZE
  IF :SIZE < 15 [STOP]
  FD :SIZE
  LT 30
  TREE :SIZE * 0.6
  RT 60
  TREE :SIZE * 0.6
  LT 30
  BK :SIZE
END

TREE 80
```

```text
-- 30 line(s), 0 shape(s)
```

The first line is the stopping rule: below fifteen units the procedure returns
before drawing, and `STOP` inside an `IF` block ends the call rather than the
program. Each call draws a trunk, leans left for one smaller tree, leans right for
another, and undoes both turns so the turtle ends where it started facing where it
started. The `BK :SIZE` is what makes that true, and it is the part a first attempt
leaves out: without it every call starts where the last one finished and the branches
wander across the canvas.

## Where it catches people out

**`REPEAT` needs brackets.** The count is a number and the body is a list, so the
list cannot be left implicit. This is refused with the message
`REPEAT requires [commands]`, and because the two commands after it on the line are
statements of their own they still run: one stroke and no square.

```logo
REPEAT 4 FD 100 RT 90
```

```logo
REPEAT 4 [FD 100 RT 90]
```

The second form draws the square.

**A missing colon is not an error.** `FD SIDE` is not the variable's value, so the
distance is 0: four zero-length strokes and no square, with nothing said about it.

```logo
MAKE "SIDE 40
REPEAT 4 [FD SIDE RT 90]
```

`REPEAT 4 [FD :SIDE RT 90]` is the program that draws the square. The same rule
applies inside a procedure body, where the parameter is `:SIZE`.

**A definition belongs to one run.** Logo is run whole, so the REPL bar re-runs
whatever is in its entry as a complete program: a `TO … END` typed at the bar is
defined and then thrown away with the run. Put definitions and the calls that use
them in one document.

## Commands used here

| Command | What it does |
| --- | --- |
| `FORWARD` / `FD` | Move forward along the heading, drawing |
| `RIGHT` / `RT`, `LEFT` / `LT` | Turn clockwise or anticlockwise, in degrees |
| `REPEAT n [ … ]` | Run the bracketed commands n times |
| `PU` / `PD` | Lift and lower the pen |
| `SETPENCOLOR` | The pen colour: a quoted name, or a palette slot as a number |
| `SETPENWIDTH` | Choose the pen thickness |
| `MAKE "NAME value` | Store a value under a name |
| `:NAME` | The stored value |
| `TO … END` | Define a procedure |
| `IF test [ … ]` | Run the block when the test is true |
| `STOP` | End the current procedure |

## Next

The [language guide](../reference.md#logo) has the rest of the turtle:
`SETXY`, `SETHEADING`, `SHOWTURTLE`, `PENSTYLE` and `BEZIER`.
[`Examples/logo/constellation.logo`](../../../Examples/logo/constellation.logo) is a
short program of rays, and
[`Examples/logo/mandala.logo`](../../../Examples/logo/mandala.logo) is a long one
built from the same five ideas.
