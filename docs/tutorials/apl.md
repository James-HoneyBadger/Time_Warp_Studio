# APL Tutorial

APL (Iverson, 1962) is an array language, and it is the one language here where
the **shape** of the data is the program. Two properties surprise everyone the
first time:

- **There is no operator precedence.** Evaluation is right to left.
- **Most functions work on whole arrays**, not on single numbers.

`+/⍳10` is 55. There is no loop in that line and no variable, and that is the
point of having APL in the lineup.

Time Warp Studio runs APL line by line. Each line is an expression, and its value
is displayed - which is exactly what APL's original interface did, and what makes
it feel like a calculator you can write a program with.

---

## 1. Values

```apl
42            a scalar
1 2 3         a vector - a strand of three values
'HELLO'       a character vector
2 3 ⍴ ⍳6      a 2-by-3 table
(1 2)(3 4)    a nested vector
```

`¯` is the negative sign, so `¯3` is minus three. `*` is **power**, not
multiplication - APL is old enough that `*` meant exponentiation before C made it
a multiplication sign. `×` is times.

Try a line on its own and its value is shown:

```apl
⍳5
```

```text
1 2 3 4 5
```

## 2. There is no precedence

A function takes the whole expression on its right. Evaluation runs right to
left.

```apl
2 × 3 + 4      14, not 10: the + 4 happens first
(2 × 3) + 4    10, because the parentheses say so
```

## 3. Strands

Placing values side by side makes a vector. When a function has an array on its
left, **that whole strand is the left argument**:

```apl
1 2 3 + 10      11 12 13
1 2 3 + 4 5 6   5 7 9
```

This is what "pervasive" means: the `+` reaches into the arrays. Mismatched
lengths are a length error rather than a silent truncation:

```apl
1 2 3 + 4 5     ❌ length error: 3 and 2
```

## 4. Functions

Every function has a **glyph** and a **keyword spelling**, so a keyboard that
cannot produce `⍳` is not a keyboard that cannot write APL.

| Glyph | Keyword | Meaning |
| ----- | ------- | ------- |
| `+` `-` `×` `÷` | `PLUS` `MINUS` `TIMES` `DIVIDE` | Arithmetic |
| `*` `⍟` | `POWER` `LOG` | Power and logarithm |
| `⌈` `⌊` | `CEILING` `FLOOR` | Round up or down |
| `⌈` `⌊` | `MAX` `MIN` | Maximum or minimum of two arguments |
| `\|` | `ABS` `RESIDUE` | Absolute value, or remainder |
| `×` | `SIGNUM` | Sign: `× ¯3` is `¯1` |
| `÷` | `RECIPROCAL` | Reciprocal |
| `=` `≠` `<` `≤` `>` `≥` | `EQUAL` `NOTEQUAL` `LESS` `LESSEQ` `GREATER` `GREATEREQ` | Comparison |
| `~` `∧` `∨` `⍲` `⍱` | `NOT` `AND` `OR` `NAND` `NOR` | Logic |
| `?` | `ROLL` | Random integers |

Structural functions work on the shape of the data rather than on its values:

| Glyph | Keyword | Meaning |
| ----- | ------- | ------- |
| `⍳` | `IOTA` `INDEXOF` | `⍳5` is `1 2 3 4 5`; `A ⍳ n` finds a position |
| `⍴` | `RHO` `SHAPE` `RESHAPE` | Shape, or reshape |
| `⌽` | `REVERSE` `ROTATE` | Reverse, or rotate by n |
| `↑` `↓` | `TAKE` `DROP` | Take from the front, or drop from it |
| `⍉` | `TRANSPOSE` | Transpose |
| `,` | `RAVEL` `CATENATE` | Flatten, or join |
| `∈` | `MEMBERSHIP` | Which members of the left are in the right |
| `⍋` `⍒` | `GRADEUP` `GRADEDOWN` | Positions that sort the data |
| `⍕` | `FORMAT` | The character form of a value |

```apl
⍳5              1 2 3 4 5
⌽ 1 2 3 4       4 3 2 1
3 ↑ 1 2         1 2 0
⍒ 1 3 2         2 3 1
⍕ 42            42
```

`TAKE` pads with zeros when it takes more than there is, which is why `3 ↑ 1 2`
answers `1 2 0`.

## 5. Operators

Reduce and scan take the function on their left. `/` reduces a vector to one
value; `\` gives the running totals.

```apl
+/⍳100            5050
+REDUCE 1 2 3     6         the keyword spelling of +/
+SCAN 1 2 3       1 3 6
```

APL's operator *axes* (`+/[2]`) are not implemented, so reduce and scan work along
the outermost axis.

Reduction is how you write a sum without writing a loop, which is the habit APL
is teaching:

```apl
+ / 1 2 3 × 4 5 6      32: the products first, then their sum
```

## 6. Functions of your own

`∇` starts and ends a definition. The result is assigned to the name on the left
of the arrow.

```apl
∇ R ← DOUBLE X
  R ← X × 2
∇
DOUBLE 21
```

```text
42
```

```apl
∇ R ← AVERAGE X
  R ← (+/X) ÷ ⍴X
∇
AVERAGE 1 2 3 4
```

`+/X` sums the vector and `⍴X` counts it, so this is a mean written as arithmetic
rather than as a loop.

## 7. Turtle graphics

A **Time Warp extension**, not part of APL.

| Command | Does |
| ------- | ---- |
| `FD` / `FORWARD` | Move forward |
| `BK` / `BACK` | Move backward |
| `RT` / `RIGHT` | Turn clockwise |
| `LT` / `LEFT` | Turn anticlockwise |
| `PU` / `PD` | Lift or lower the pen |
| `HOME` | Return to the start |
| `CS` | Clear the screen |

The turtle's position is read with `TX`, `TY` and `THETA`. Those names are
deliberate: `X`, `Y` and `H` are ordinary APL variables, and taking them from the
program to report the turtle's position would break the most natural line anyone
writes.

```apl
⍝ A hexagon: six sides of the same two moves.
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

⍝ ... and a spoke back to the middle.
RT 30
FD 52
```

## 8. Comments

`⍝` makes the rest of the line a comment.

```apl
⍝ This line does nothing
÷ 2 4 8        0.5 0.25 0.125
```

## What is not implemented

`∘.` outer product, `⍤` rank, the composition operators, the system functions
`⎕`, and `→` branch with labels. A program that uses one is told which symbol
stopped it rather than being given a wrong answer.

## Quick reference

| Glyph | Keyword | Meaning |
| ----- | ------- | ------- |
| `⍳` | `IOTA` | Indices |
| `⍴` | `RHO` | Shape or reshape |
| `⌽` | `REVERSE` | Reverse |
| `↑` `↓` | `TAKE` `DROP` | Take or drop |
| `⍉` | `TRANSPOSE` | Transpose |
| `,` | `RAVEL` | Flatten |
| `∈` | `MEMBERSHIP` | Membership |
| `⍋` `⍒` | `GRADEUP` `GRADEDOWN` | Sort positions |
| `⍕` | `FORMAT` | Character form |
| `/` `\` | `REDUCE` `SCAN` | Reduce, scan |
| `∇` | | Define a function |
| `⍝` | | Comment |

See also
[`Examples/apl/arrays_and_turtle.apl`](../../Examples/apl/arrays_and_turtle.apl),
and the [APL reference](../LANGUAGE_GUIDE.md#apl).
