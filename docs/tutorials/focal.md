# FOCAL Tutorial

FOCAL - **FO**rmula **CAL**culator - is DEC's line-numbered teaching language
from 1969. It was built to be taught in an afternoon: ten commands, one number
type, and line labels of the form `NN.NN` so that a group of five lines can be
named as a unit.

Two things about it will feel strange if you have used BASIC: **branching is by
sign, not by comparison**, and a loop changes the variable it counts.

Time Warp Studio runs FOCAL line-by-line.

---

## 1. Printing

`T` is TYPE. Text goes in quotes, expressions do not, and `,` puts the pieces
side by side.

```focal
01.10 T "Hello, World!"
01.20 T "X=",2+3
```

```text
Hello, World!
X=5.00
```

The default is **two decimal places** for every number. `%n` changes it for the
rest of that statement:

```focal
01.10 T %0,"R=",7/2         R=4
01.20 T %5,"pi=",FPI        pi=3.14159
01.30 T %2,"R=",7/2         R=3.50
```

`!` suppresses the newline inside one TYPE, so `T "A"!"B"` prints `AB` rather
than `A` and `B` on separate lines.

## 2. Variables

`S` is SET. There is one type - a number - and no declaration.

```focal
01.10 S X=2+3
01.20 T "X=",X
```

**Undefined variables are 0.** Every variable is zeroed when a program loads.
This is worth knowing: it means a typo in a name prints `0.00` rather than
reporting an error, so a number that is unexpectedly zero is usually a spelling
mistake.

## 3. Input

`A` is ASK. It prints a quoted prompt and reads a number into the variable named
after it.

```focal
01.10 A "How many? ",N
01.20 T "You said ",N
```

A comma before the name is accepted, because every other statement in the language
separates its items with one.

## 4. Branching on a sign

FOCAL has **no relational operators**. There is no `IF X > 5`. Instead `IF`
takes three targets and picks whichever matches the *sign* of an expression:

```focal
IF (expression) negative,zero,positive
```

```focal
01.10 A "Number? " N
01.20 I (N) 1.30,1.40,1.50
01.30 T "NEGATIVE"
01.35 GOTO 1.60
01.40 T "ZERO"
01.45 GOTO 1.60
01.50 T "POSITIVE"
01.60 Q
```

Leaving a target out means "fall through to the next line", so `I (N) ,,1.50`
reads "if N is positive, go to 1.50".

There is no ELSE, so a branch that prints different things on each path needs
those extra `GOTO` lines.

To test for a particular value, subtract it: `I (SIDES-4) ...` is negative when
there are fewer than four sides, zero at exactly four, and positive above.

`G` is GOTO and jumps to a label.

## 5. Labels

Labels are `NN.NN`: a **group** number and a **line** number, each up to two
digits. The parts are compared as numbers, so `01.10` and `1.10` name the same
line and `GOTO 9.9` reaches a label written `09.09`.

The two-part shape is the thing to be careful with. `01.30` is group 1 line
**30**, so it is reached as `1.30` - not `1.3`, which is group 1 line 3 and a
different line entirely. A reference to a label that does not exist is reported
rather than ignored, which is how the mistake surfaces.

```focal
01.10 T "START"
01.20 GOTO 1.40
01.30 T "NOT REACHED"
01.40 T "LANDED"
```

## 6. Loops

`F` is FOR. It repeats **the rest of the line** - there is no `NEXT` and no block.

```focal
01.10 FOR I=1,3;T %0,"I=",I
```

```text
I=1
I=2
I=3
```

After the loop, `I` keeps the last value the body saw: after `FOR I=1,4`, I is 4,
not the 5 that failed the test.

The `;` belongs to the FOR, and separates the statements that make up its body.
It is **not** a general statement separator, so `T "A";T "B"` is an error naming
the `;`.

A FOR body being the rest of the line is why the square below fits on one line:

```focal
01.10 FOR I=1,4;FD 90;RT 90
```

## 7. Subroutines

`D` is DO, and it calls a labelled group.

```focal
01.10 DO 2.10
01.20 Q
02.10 T "INSIDE"
```

There is **no RETURN statement in FOCAL-69**. A group called with DO is expected
to end where the program ends, so a DO that needs to come back simply stops. A
nested DO is bounded by a depth limit, so a cycle reports itself instead of
hanging.

## 8. Functions

| Function | Meaning |
| -------- | ------- |
| `FABS(X)` | Absolute value |
| `FSQT(X)` | Square root |
| `FEXP(X)` `FLOG(X)` | Exponential and natural logarithm |
| `FSIN(X)` `FCOS(X)` `FATN(X)` | Trigonometry, in radians |
| `FITR(X)` | Truncate to an integer |
| `FRAN` / `FRAN(X)` | Random in [0, 1), optionally seeded |
| `FSGN(X)` | Sign: -1, 0 or 1 |
| `FPI` | Pi |

`FPI` and `FRAN` take **no parentheses**, because that is how FOCAL writes them:

```focal
01.10 T "sq=",FSQT(16)      sq=4.00
01.20 T %5,"pi=",FPI        pi=3.14159
01.30 T %3,"r=",FRAN        r=0.672
```

## 9. Turtle graphics

A **Time Warp extension** - 1969 FOCAL had no graphics at all.

| Command | Short | Does |
| ------- | ----- | ---- |
| `FORWARD n` | `FD` | Move forward |
| `BACK n` | `BK` | Move backward |
| `RIGHT n` | `RT` | Turn clockwise |
| `LEFT n` | `LT` | Turn anticlockwise |
| `PENUP` / `PENDOWN` | `PU` / `PD` | Lift or lower the pen |
| `HOME` | | Return to the start |

```focal
01.10 S SIDE=90
01.20 FOR I=1,4;FD SIDE;RT 90
01.30 S SIDES=4
01.40 IF (SIDES-4) 1.60,1.50,1.60
01.50 C Four sides: the branch on the sign went to the zero target
01.55 T "That was a square, decided by branching on a sign."
01.60 Q
```

## 10. Comments and leaving

`C` makes the rest of the line text, and `Q` stops the program.

```focal
01.10 C This line does nothing
01.20 T "AFTER COMMENT"
01.30 Q
01.40 T "NOT REACHED"
```

`E` is ERASE, and it forgets variables - which sends them back to zero:

```focal
01.10 S X=1
01.20 T "X=",X
01.30 E ALL
01.40 T "X=",X
```

```text
X=1.00
X=0.00
```

An **unrecognised command is an error, not a comment**. Some FOCAL listings are
annotated by writing text after the line number; here that is what `C` is for, and
a mistyped command reports itself instead of printing nothing.

## Quick reference

| Command | Short | Does |
| ------- | ----- | ---- |
| `TYPE` | `T` | Print text, formats and expressions |
| `SET` | `S` | Assign |
| `ASK` | `A` | Prompt for a value |
| `IF` | `I` | Three-way branch on the sign |
| `FOR` | `F` | Loop over the rest of the line |
| `DO` | `D` | Call a group |
| `GOTO` | `G` | Jump to a group |
| `COMMENT` | `C` | The rest of the line is text |
| `ERASE` | `E` | Forget variables |
| `QUIT` | `Q` | Stop |
| `%n` | | n decimal places |

See also [`Examples/focal/square.foc`](../../Examples/focal/square.foc), and the
[FOCAL reference](../LANGUAGE_GUIDE.md#focal).
