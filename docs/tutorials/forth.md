# Forth Tutorial

Forth is the language that fits in a pocket. There is no syntax to learn in the usual
sense — no parentheses around conditions, no statement keywords — only **words** and a
**stack**. A word takes what it needs from the top of the stack and leaves what it
produces there, and a program is a sequence of words.

It is the eleventh language in Time Warp Studio, and the one you can write an
interpreter for in an afternoon: that was always the point of it.

Time Warp Studio runs Forth **whole-program**: a file is read, its definitions are
built into a dictionary, and the words outside definitions run. The HB-1's ROM socket
boots it like any other ROM, with `ok` as its prompt.

---

## 1. The stack

Numbers push themselves. Everything else works on what is already there.

```forth
2 3 + .        \ pushes 2, pushes 3, + replaces them with 5, . prints it
```

`.` prints the top of the stack and removes it. `.S` prints the whole stack without
removing anything — the word you will use most while learning:

```forth
1 2 3 .S       \ <3> 1 2 3
DROP           \ 1 2
SWAP           \ 2 1
OVER           \ 2 1 2
```

`DUP` (duplicate), `DROP`, `SWAP`, `OVER`, `ROT` and `NIP` are the six you need to
read any Forth. `DEPTH` tells you how many items there are.

---

## 2. Words of your own

`:` begins a definition, `;` ends it, and the name comes straight after the colon.

```forth
: SQUARE ( n -- n*n )   DUP * ;
7 SQUARE .
```

The comment in the middle, `( n -- n2 )`, is a **stack effect**: what the word takes
and what it leaves. It is not decoration — a Forth program is read by whoever maintains
it, and the stack is the only thing they cannot see.

Words can call words, and a word can call itself with `RECURSE`:

```forth
: FACT ( n -- n! )
  DUP 1 > IF DUP 1- RECURSE * THEN ;
6 FACT .
```

---

## 3. Decisions

`IF`, `ELSE` and `THEN` compile into a definition. They consume a flag from the
stack: **zero is false, anything else is true**, and every comparison produces -1 or 0.

```forth
: SIGN ( n -- )
  DUP 0< IF ." negative" ELSE ." positive or zero" THEN DROP ;

-5 SIGN CR
```

`=` `<>` `<` `>` `<=` `>=` compare two numbers; `0=` `0<` `0>` compare one with zero.

---

## 4. Loops

Three shapes, and the first one is a *post-test* loop:

```forth
: COUNTDOWN ( n -- )
  BEGIN DUP . 1- DUP 0= UNTIL DROP ;

3 COUNTDOWN CR
```

`COUNTDOWN` prints the number, subtracts one, and loops until what is left is zero.
The `DUP` before `0=` is the shape you will write hundreds of times: the flag is
consumed by `UNTIL`, so the value has to survive it.

The counted loop is the one to learn first:

```forth
: STARS ( n -- )
  0 DO 42 EMIT LOOP CR ;

5 STARS
```

`DO` takes `limit start` — `5 0 DO ... LOOP` runs the body with `I` from 0 to 4.
`LEAVE` jumps out early, and `+LOOP` steps by whatever is on the stack instead of by
one, so the body has to put it there:

```forth
: BY-TWOS ( limit -- ) 0 DO I . 2 +LOOP CR ;
10 BY-TWOS
```

`BEGIN ... UNTIL` runs until the flag on the stack is non-zero; `BEGIN ... WHILE ...
REPEAT` runs while it is non-zero.

---

## 5. Variables and memory

```forth
VARIABLE COUNT
0 COUNT !
COUNT @ .          \ COUNT pushes its address; @ fetches the cell; ! stores one
5 COUNT +!         \ add to it in place
100 CONSTANT LIMIT
```

`@` and `!` work on two-byte cells; `C@` and `C!` on single bytes. The address space
is the language's own 64 KiB of data space, allocated upwards from the end of the
dictionary.

`POKE` and `PEEK` are how a Forth program reaches the *machine*. On the HB-1 the
character cells of the tube start at 32768, so this writes `HI` on the top line of
the screen:

```forth
72 32768 POKE
73 32769 POKE
32768 PEEK .       \ 72
```

`POKE` is `( value address -- )`, the same order as `!`. Off the machine — in the IDE
— the same words read and write a memory map of their own, so a poke and a peek still
agree.

Numbers can be written in other bases without a `BASE` word: `$FF` is hexadecimal and
`%1010` is binary.

---

## 6. The turtle

The machine has a turtle, so the ROM has turtle words — postfix, like everything else,
which makes a square read almost like English:

```forth
: SQUARE ( size -- ) 4 0 DO DUP FD RT 90 LOOP DROP ;
50 SQUARE
```

| Word | Does |
| --- | --- |
| `FD`, `BK` | forward or back by the top of the stack |
| `RT`, `LT` | turn right or left by the top of the stack |
| `PU`, `PD` | pen up, pen down |
| `HOME`, `CS` | home the turtle, clear the screen |
| `SETPC`, `SETPW` | pen colour from a palette number, pen width |

---

## 7. Words that are there

Every primitive is listed in the manual's
[language guide](../LANGUAGE_GUIDE.md#forth). The ones worth trying first:
`WORDS` (print the dictionary), `.S` (print the stack), `EMIT` (print a character),
`CR` (newline), `SPACES`, `/MOD`, `MIN`, `MAX`, `AND`, `OR`, `XOR`, `LSHIFT`.

---

## 8. What this Forth does not have

Stated plainly, because a Forth that pretends is worse than one that says:

- **No `CREATE ... DOES>`.** Defining words that define words is the cleverest part of
  Forth and the part a first program never needs.
- **No vocabularies, no `FIND`, no metacompilation.** This is one dictionary.
- **No pictured numeric output** (`<# #S #>`); `.` prints decimal and that is all.
- **No floating point.** Cells are integers, as they were on the machines this
  imitates.

Everything it accepts, it executes. A word it does not know is an error with the line
it happened on, not a silent skip.

---

## Where to go next

- [The language guide's Forth section](../LANGUAGE_GUIDE.md#forth) — every word.
- [The HB-1 manual](../hb1/README.md) — the machine whose ROM socket Forth plugs into.
- `Examples/forth/` in the repository: `hello.fs`, `turtle.fs` and `showcase.fs`.
