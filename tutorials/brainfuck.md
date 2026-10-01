# Brainfuck Programming Tutorial

Brainfuck was created by Urban Müller in 1993. It is the quintessential **esoteric language** — Turing-complete with only 8 commands. Understanding Brainfuck deepens your appreciation of computation at its most fundamental.

## The Machine Model

Brainfuck operates on an infinite tape of byte cells:

```
Tape (memory):  [ 0 | 0 | 0 | 0 | 0 | ... ]
                  ^
                  Data Pointer (dp)
```

There is also a program counter and an I/O stream.

## The 8 Commands

| Command | Meaning |
|---------|---------|
| `>` | Move data pointer right |
| `<` | Move data pointer left |
| `+` | Increment cell at dp |
| `-` | Decrement cell at dp |
| `.` | Output cell value as ASCII |
| `,` | Read one byte from input into dp |
| `[` | If cell == 0, jump to matching `]` |
| `]` | If cell != 0, jump back to matching `[` |

Everything else (spaces, letters, etc.) is a **comment**.

## Hello World

```brainfuck
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]
>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
```

This prints: `Hello World!`

The logic works by building ASCII values in tape cells using nested multiplication loops.

## How It Works: Step by Step

Let's build `A` (ASCII 65):

```brainfuck
; Put 65 in cell 0 and print it

; 65 = 13 * 5
; First, put 13 in cell 0:
+++++++++++++     13 plusses
; Now multiply by 5 using a loop:
[->+++++<]        loop: move 5 to cell 1 per iteration
; cell 1 now = 13*5 = 65
>.                move to cell 1, print it
```

Simplified: `+++++++++++++[->+++++<]>.`

## Simple Examples

```brainfuck
; Print a newline (ASCII 10)
++++++++++.

; Print ! (ASCII 33)
+++++++++++++++++++++++++++++++++.

; Print three A's (ASCII 65)
; 65 = 8 * 8 + 1
++++++++[>++++++++<-]>+.  print first A
.                          second A (value unchanged)
.                          third A
```

## Loop Pattern Reference

```brainfuck
[-]             Zero current cell
[->+<]          Move cell 0 to cell 1
[->+>+<<]       Copy cell 0 to cell 1 and cell 2
[->++<]         Double current cell into cell to the right
```

## Cat Program (echo input)

```brainfuck
,[.,]
```

Read a character; while it's not zero, print it and read next.

## Adding Two Single-Digit Numbers

```brainfuck
; Read two ASCII digits, add them, output result
,            read a
>            move right
,            read b
[-<+>]       add b into a  (b cells to 0, a gets b)
<            move to a
.            print sum (as ASCII — not numerically formatted)
```

## Tips for Writing Brainfuck

1. Draw the tape state on paper as you go
2. Use `[-]` to zero a cell before using it
3. `[->+<]` is the most common idiom: "move value right"
4. Nested loops multiply — great for building larger values
5. Comments: use any non-command character freely

## Further Reading

- [Examples/brainfuck/](../Examples/brainfuck/) — 5 Brainfuck example programs
- [Language Guide: Brainfuck](LANGUAGE_GUIDE.md#brainfuck)
