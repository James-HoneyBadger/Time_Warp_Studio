# COMAL: BASIC with a structure

COMAL-80 was written in Denmark in the late 1970s to give BASIC the shape of a
structured language: the same line numbers and `PRINT`, with `IF … ENDIF`,
`WHILE … ENDWHILE`, `REPEAT … UNTIL` and named procedures in place of `GOTO`. It
was used for teaching in Danish schools and was ported to most of the home machines.
The interpreter here runs it line by line.

Press **F9** to run a document. The output blocks below are what the program prints.

## What a program looks like

```comal
10 PRINT "Hello, World!"
```

```text
Hello, World!
```

The line number is optional and is an address rather than a sort key. `PRINT` writes
its items and ends the line, and `END` stops the program if it is reached. Nothing
else is needed: there is no header, no declaration and no semicolon to forget.

## Step 1: variables and the two PRINT separators

Store a name and a count, then print them with `,` and with `;`.

```comal
10 NAME$ := "Ada"
20 COUNT := 3
30 PRINT "Hello, ", NAME$, " - ", COUNT, " new messages"
40 PRINT "Hello, "; NAME$; " - "; COUNT; " new messages"
```

```text
Hello,  Ada  -  3  new messages
Hello, Ada - 3 new messages
```

`:=` assigns. A name ending in `$` holds text and everything else holds a number.
The two separators are not the same word: `,` prints a space of its own on each side
of the item, which is why the first line has doubled spaces where the string already
ends or starts with one, and `;` prints the item with nothing added. A sentence built
from a fixed part and a value is therefore printed piece by piece with `;` between
the pieces, since `+` does not join two strings.

## Step 2: selection

Grade a mark with the full `IF` block.

```comal
10 SCORE := 87
20 IF SCORE >= 90 THEN
30   PRINT "grade A"
40 ELIF SCORE >= 80 THEN
50   PRINT "grade B"
60 ELSE
70   PRINT "grade C"
80 ENDIF
90 PRINT "checked"
```

```text
grade B
checked
```

`ELIF` is part of the language, so one block can carry as many tests as it needs,
and `ENDIF` closes the whole chain once. Each branch is a block of lines rather than
a single statement, which is the difference from BASIC that COMAL exists for. The
single-line form `IF SCORE > 50 THEN PRINT "pass"` still works for one action.

## Step 3: counted and conditional loops

Count up with `FOR`, then repeat while a condition holds with `WHILE`.

```comal
10 FOR I := 1 TO 5
20   PRINT "I is "; I
30 NEXT I
40 N := 0
50 WHILE N < 3
60   N := N + 1
70   PRINT "N is "; N
80 ENDWHILE
```

```text
I is 1
I is 2
I is 3
I is 4
I is 5
N is 1
N is 2
N is 3
```

`FOR … NEXT` counts its variable from the first value to the last inclusive, and
`STEP` changes the increment, including downwards as in `STEP -3`. `WHILE … ENDWHILE`
tests before each pass and runs zero times if the condition is false at the start;
`REPEAT … UNTIL` is the other way round and always runs once. `EXIT` leaves a loop
early, and `LOOP … ENDLOOP` repeats until an `EXIT`.

## Step 4: procedures and functions

Compute a total with a `FUNC` and print with a `PROC`.

```comal
10 FUNC TOTAL(N)
20 LOCAL SUM
30 SUM := 0
40 FOR I := 1 TO N
50   SUM := SUM + I
60 NEXT I
70 RETURN SUM
80 ENDFUNC
90 PROC GREET(NAME$)
100   PRINT "Hello, "; NAME$
110 ENDPROC
120 PRINT "total "; TOTAL(5)
130 CALL GREET("World")
```

```text
total 15
Hello, World
```

A `FUNC` answers a value and is used inside an expression, so `TOTAL(5)` sits where
a number would; its `RETURN` leaves the body and answers the caller. A `PROC` is
called for what it does, with `CALL`, and needs no return value. `LOCAL` keeps a name
to the function while it runs, and a variable of that name in the caller is left
untouched. A `PROC` is the right shape when the work is printing or drawing.

## Step 5: the turtle

Draw a square with `MOVE` and `TURN`.

```comal
10 FOR SIDE := 1 TO 4
20   MOVE 80
30   TURN 90
40 NEXT SIDE
```

```text
-- 4 line(s), 0 shape(s)
```

The turtle words are a Time Warp extension rather than part of COMAL-80, which had
no graphics. `MOVE` goes forward along the heading, `TURN` turns clockwise and a
negative angle turns anticlockwise, `PENUP` and `PENDOWN` control inking, and `HOME`
returns the turtle to the centre. Inside a counted loop that is a square: four moves
of 80 units, each followed by a quarter turn.

## Where it catches people out

**The comma adds a space.** A string that already ends with a space, printed with
`,`, puts two spaces on the page. The wrong version reads well in the source and
wrong on the screen:

```comal
10 NAME$ := "Ada"
20 PRINT "Hello, ", NAME$
```

```text
Hello,  Ada
```

Use `;` and put the spaces in the strings, where they can be counted:

```comal
10 NAME$ := "Ada"
20 PRINT "Hello, "; NAME$
```

```text
Hello, Ada
```

**A block ends with its own word.** An `IF` without `ENDIF` reports
`unterminated block` against the `IF`'s own line and then carries on, so the
program prints its line and looks almost right.

```comal
10 IF 1 = 1 THEN
20 PRINT "yes"
```

`ENDIF`, `ENDWHILE`, `ENDPROC` and `ENDFUNC` each close one block and none of them
closes another, so the word has to match the block it ends.

**Line numbers are addresses.** They name a line for `GOTO` and `GOSUB` and do not
order the program, which runs top to bottom as written. Writing `10` after `20` in
the file runs the `10` line second, whatever the numbers say.

## Commands used here

| Statement | What it does |
| --- | --- |
| `PRINT a, b` | Prints with a space around each item |
| `PRINT a; b` | Prints the items with nothing added |
| `:=` | Assigns |
| `IF … THEN … ELIF … ELSE … ENDIF` | Chooses a branch |
| `FOR … TO … STEP … NEXT` | Counts a variable over a range |
| `WHILE … ENDWHILE` | Repeats while a condition holds |
| `REPEAT … UNTIL` | Repeats until a condition holds |
| `FUNC … ENDFUNC`, `RETURN` | Defines a function that answers a value |
| `PROC … ENDPROC`, `CALL` | Defines a procedure called for its effect |
| `LOCAL` | Keeps a name to the function that declares it |
| `MOVE`, `TURN`, `PENUP`, `PENDOWN`, `HOME` | Drive the turtle |

## Next

The [language guide](../reference.md#comal) has the rest: `DIM`, `DATA` and
`READ`, `GOTO` and `GOSUB`, and `EXIT` inside each loop.
[`Examples/comal/procedures.comal`](../../../Examples/comal/procedures.comal) puts
procedures to work, and
[`Examples/comal/turtle_art.comal`](../../../Examples/comal/turtle_art.comal) draws
with them.
