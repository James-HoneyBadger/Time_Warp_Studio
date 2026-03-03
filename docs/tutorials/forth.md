# Forth Programming Tutorial

Forth is a stack-based, concatenative programming language invented by Charles Moore in 1970. It is beloved for its simplicity, efficiency, and close relationship with the machine.

## The Stack

Everything in Forth revolves around a **data stack**. Numbers are pushed onto the stack; words (functions) pop and push values.

```forth
5 3 +      \ Push 5, push 3, add -> 8 on stack
.          \ Print top of stack: 8
```

## Basic Arithmetic

```forth
10 4 + .    \ 14
10 4 - .    \ 6
10 4 * .    \ 40
20 4 / .    \ 5
17 5 MOD .  \ 2  (remainder)
```

## Stack Manipulation

| Word | Effect | Description |
|------|--------|-------------|
| `DUP` | ( n -- n n ) | Duplicate top |
| `DROP` | ( n -- ) | Discard top |
| `SWAP` | ( a b -- b a ) | Swap top two |
| `OVER` | ( a b -- a b a ) | Copy second |
| `ROT` | ( a b c -- b c a ) | Rotate top 3 |

```forth
5 DUP  . .      \ 5 5
3 7 SWAP . .    \ 3 7  -> prints 3, then 7
2 4 OVER . . .  \ 2 4 2
```

## Defining Words

```forth
\ Simple word definition
: GREET ." Hello from Forth!" CR ;
GREET

\ Word with computation
: SQUARE DUP * ;
5 SQUARE .    \ 25

: CUBE DUP DUP * * ;
3 CUBE .      \ 27

\ Factorial (recursive)
: FACTORIAL
    DUP 1 <= IF DROP 1
    ELSE DUP 1 - FACTORIAL *
    THEN ;

6 FACTORIAL .    \ 720
```

## Control Flow

```forth
\ IF ... THEN
5 3 > IF ." 5 is greater" THEN CR

\ IF ... ELSE ... THEN
10 DUP 0 > IF ." positive" ELSE ." non-positive" THEN CR

\ DO ... LOOP (definite loop)
10 1 DO
    I .
LOOP CR

\ BEGIN ... UNTIL (indefinite loop)
1
BEGIN
    DUP .
    2 *
    DUP 100 >
UNTIL DROP CR
```

## String Output

```forth
\ Print string literals
." Hello, World!" CR
." Time Warp Studio" CR

\ CR outputs a newline
: SEPARATOR
    40 0 DO ." - " LOOP CR ;

SEPARATOR
```

## Stack Comments Convention

By convention words are documented with stack effect comments:

```forth
( before -- after )

: SQUARE ( n -- n^2 )   DUP * ;
: + ( a b -- sum )       \ built-in
```

## Working Example

```forth
\ Print a multiplication table
: .TABLE
    10 1 DO
        10 1 DO
            I J * 4 .R
        LOOP CR
    LOOP ;

.TABLE
```

## Further Reading

- [Examples/forth/](../Examples/forth/) — 5 Forth example programs
- [Language Guide: Forth](LANGUAGE_GUIDE.md#forth)
