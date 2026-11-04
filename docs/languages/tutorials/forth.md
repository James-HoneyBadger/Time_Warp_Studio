# Forth: words, a stack, and no syntax

Charles Moore developed Forth in the late 1960s for programming instruments, and it
has been the same shape ever since: a program is a sequence of whitespace-separated
**words**, and a word takes whatever it needs from a **stack**. `2 3 +` is not
`2 + 3`, it is "push two, push three, add", so the order looks backwards until it
clicks. There is no grammar to learn, and the language grows: `: NAME … ;` adds a
word, and from then on it is indistinguishable from a built-in one. The interpreter
runs Forth whole-program, because a definition spans as many lines as it likes.

Press **F9** to run a document. The output blocks below are what the program prints.

## What a program looks like

```forth
2 3 + .
CR
```

```text
5 
```

Numbers push themselves onto the stack and every other word works on what is there.
`+` takes the top two items, adds them and pushes the answer; `.` takes the top item,
prints it and removes it. `.` prints a space after the number, so `CR` is what ends
the line. Nothing is implicit. A word either finds its arguments on the stack or
reports that it did not.

## Step 1: looking at the stack

Push three numbers, print the whole stack, then change it.

```forth
1 2 3 .S
CR
DROP SWAP .
CR
```

```text
<3> 1 2 3 
1 
```

`.S` prints the depth and the items without removing anything. It is the word to
reach for while learning: it shows what the next word will find. `DROP` throws the
top item away, here the 3, and `SWAP` exchanges the top two, so `.` prints 1 and
leaves 2 on the stack. `DUP`, `DROP`, `SWAP`, `OVER`, `ROT` and `NIP` are the six
that make a Forth program readable, and `DEPTH` answers how many items there are.

## Step 2: defining words

Define a square and a factorial.

```forth
: SQUARE ( n -- n*n ) DUP * ;
9 SQUARE .
CR
: FACT ( n -- n! ) DUP 1 > IF DUP 1- RECURSE * THEN ;
6 FACT .
CR
```

```text
81 
720 
```

`:` begins a definition, the name follows, and `;` ends it. The bracketed comment is a
stack effect: what the word takes and what it leaves. It is not decoration, since the
stack is the one thing a reader cannot see. `SQUARE` duplicates its argument and
multiplies the two copies. `FACT` calls itself with `RECURSE`. The `IF … THEN` around
the multiplication is what stops the recursion at 1.

## Step 3: decisions

Test a number against zero and print one of two sentences.

```forth
: SIGN ( n -- )
  DUP 0< IF ." negative" ELSE ." positive or zero" THEN DROP ;
-5 SIGN CR
5 SIGN CR
```

```text
negative
positive or zero
```

`IF` consumes a flag from the stack: zero is false and anything else is true. Every
comparison answers -1 for true and 0 for false, so a flag can be produced by `0<`,
`=`, `<` or anything else that leaves a number. `THEN` closes the branch, with `ELSE`
between the two arms, and the `." …"` prints text without the trailing space that `.`
adds. The `DUP` before `0<` is the shape to learn: the test eats a copy, so the
original survives for the `DROP` at the end.

## Step 4: loops

Print a row of asterisks, then count by two.

```forth
: STARS ( n -- ) 0 DO 42 EMIT LOOP CR ;
5 STARS
: BY-TWOS ( limit -- ) 0 DO I . 2 +LOOP CR ;
10 BY-TWOS
```

```text
*****
0 2 4 6 8 
```

`DO` takes a limit and a start, so `5 0 DO … LOOP` runs the body with `I` from 0 to 4;
inside `STARS` the 5 is the argument and the 0 is written after it. That is the
reverse of how it reads in English. `EMIT` prints the character with code 42, an
asterisk. `+LOOP` steps by whatever is on the stack instead of by one, so the body
has to leave the step there: `2 +LOOP`. `BEGIN … UNTIL` and
`BEGIN … WHILE … REPEAT` are the other two shapes, and `LEAVE` leaves a `DO` loop
early.

## Step 5: variables

Keep a running total in a variable and print it.

```forth
VARIABLE TOTAL
0 TOTAL !
: ADD-UP ( n -- ) 1+ 1 DO I TOTAL +! LOOP ;
10 ADD-UP
." 1 to 10 is " TOTAL @ . CR
```

```text
1 to 10 is 55 
```

`VARIABLE TOTAL` makes a word that pushes the variable's address. `!` is store and `@`
is fetch; both expect the address on top, so `0 TOTAL !` puts the value first and the
address second. `+!` adds to what the variable holds, so the loop accumulates without
a second variable. `CONSTANT` is the read-only version: it pops a number once and
pushes it for ever. `POKE` and `PEEK` reach the program's own 64 KiB map of memory,
with the same value-then-address order.

## Step 6: the turtle

Draw a square by looping over the two turtle words.

```forth
: SQUARE ( size -- ) 4 0 DO DUP FD RT 90 LOOP DROP ;
60 SQUARE
```

```text
-- 4 line(s), 0 shape(s)
```

The turtle words are postfix like everything else, so a distance is pushed before
`FD` and an angle before `RT`: `FD 100` would run `FD` with nothing under it. The
`DUP` inside the loop keeps the size for the next side, and the `DROP` after `LOOP`
throws away the copy the loop is done with. That is the discipline a Forth program
needs to leave the stack as it found it. `CS` clears the screen, `PU` and `PD` lift
and lower the pen, and `SETPC` takes a palette slot.

## Where it catches people out

**Nothing is infix, and arguments come first.** `FD 100` runs `FD` before the number
exists, so the word finds an empty stack and reports it.

```forth
FD 100
```

The error is `FD: stack underflow`, and it names the word that found the problem,
not the expression, because a stack has no expression to point at. `100 FD` is the
program that moves the turtle.

**`.` prints a space and does not end the line.** Four answers printed in a row
arrive on one line, separated by spaces, unless something ends it.

```forth
1 . 2 . 3 .
CR
```

```text
1 2 3 
```

`CR` is the newline. A word that prints a table is a word with a `CR` in it.

**A definition never runs by itself.** `: SQUARE … ;` compiles the word and prints
nothing, so a file of definitions produces no output at all until a line outside them
calls one. That is deliberate: the definitions are the library and the last lines are
the program.

## Words used here

| Word | Stack effect | What it does |
| --- | --- | --- |
| `.` | `( a -- )` | Print the top item and a space |
| `.S` | `( -- )` | Print the whole stack |
| `DUP`, `DROP`, `SWAP` | | Copy, discard, exchange |
| `+`, `-`, `*`, `/`, `MOD` | `( a b -- c )` | Arithmetic |
| `0<`, `=`, `<`, `>` | `( a b -- flag )` | Comparison, -1 or 0 |
| `IF … ELSE … THEN` | `( flag -- )` | Choose a branch |
| `DO … LOOP`, `I`, `+LOOP` | `( limit start -- )` | Counted loop |
| `: … ;` | | Define a word |
| `RECURSE` | | Call the word being defined |
| `VARIABLE`, `!`, `@`, `+!` | | A cell of memory and its store and fetch |
| `." text"`, `EMIT`, `CR` | | Print text, a character, a newline |
| `FD`, `RT`, `PU`, `PD` | | Drive the turtle |

## Next

The [language guide](../reference.md#forth) has the full dictionary: the
stack words, the bit operations, `PICK`, `ROLL`, `WORDS` and what this Forth leaves
out. [`Examples/forth/turtle.fs`](../../../Examples/forth/turtle.fs) draws with the
words above, and [`Examples/forth/showcase.fs`](../../../Examples/forth/showcase.fs)
is a longer program.
