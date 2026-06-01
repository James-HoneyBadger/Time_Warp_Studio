# Smalltalk Tutorial

## Introduction

Smalltalk is a pioneering object-oriented programming language developed at Xerox PARC in the 1970s. Smalltalk-80 introduced many ideas that are now standard: garbage collection, just-in-time compilation, the MVC design pattern, and the concept of a "live" programming environment.

**Key characteristics:**
- Everything is an object (including integers, booleans, and blocks)
- Communication happens by sending *messages* to objects
- Blocks `[...]` are first-class closures
- No keywords — only message sends

## Hello World

```smalltalk
Transcript showCr: 'Hello, World!'.
```

`Transcript` is the output object. `showCr:` is a *keyword message* that displays a string followed by a newline.

## Variables

Temporary variables are declared between `| ... |` pipes:

```smalltalk
| x y |
x := 42.
y := x * 2.
Transcript showCr: y printString.
```

`:=` is the assignment operator. `printString` converts any object to its string representation.

## Message Types

### Unary messages (no arguments)

```smalltalk
-5 abs.       "returns 5"
'hello' size. "returns 5"
10 factorial. "returns 3628800"
```

### Binary messages (one argument, operator-like)

```smalltalk
3 + 4.        "7"
'foo' , 'bar'. "foobar"
5 > 3.        "true"
```

### Keyword messages (one or more arguments)

```smalltalk
Transcript showCr: 'hello'.
10 max: 20.
arr at: 1.
coll inject: 0 into: [:acc :x | acc + x].
```

## Conditionals

```smalltalk
| x |
x := 7.
(x > 5) ifTrue: [Transcript showCr: 'big'].
(x > 5) ifTrue: [Transcript showCr: 'big'] ifFalse: [Transcript showCr: 'small'].
```

## Loops

```smalltalk
"timesRepeat:"
5 timesRepeat: [Transcript showCr: 'hello'].

"to:do:"
1 to: 5 do: [:i | Transcript showCr: i printString].

"to:by:do:"
10 to: 1 by: -2 do: [:i | Transcript showCr: i printString].

"whileTrue:"
| n |
n := 1.
[n <= 100] whileTrue: [
    Transcript showCr: n printString.
    n := n * 2].
```

## Collections

```smalltalk
| c |
c := OrderedCollection new.
c add: 10.
c add: 20.
c add: 30.
Transcript showCr: c size printString.  "3"

"Iterate"
c do: [:item | Transcript showCr: item printString].

"Filter"
| evens |
evens := c select: [:x | (x \\ 2) = 0].

"Transform"
| doubled |
doubled := c collect: [:x | x * 2].

"Reduce"
| sum |
sum := c inject: 0 into: [:acc :x | acc + x].
Transcript showCr: sum printString.  "60"
```

## Blocks

Blocks are anonymous functions (closures):

```smalltalk
| square |
square := [:n | n * n].
Transcript showCr: (square value: 5) printString.  "25"
```

Blocks close over outer variables:

```smalltalk
| count |
count := 0.
3 timesRepeat: [count := count + 1].
Transcript showCr: count printString.  "3"
```

## Turtle Graphics

```smalltalk
Turtle forward: 50.
Turtle right: 90.
Turtle forward: 50.
```

## String Messages

| Message | Result |
|---------|--------|
| `'hello' size` | `5` |
| `'hello' reversed` | `'olleh'` |
| `'hello' asUppercase` | `'HELLO'` |
| `'hello' , ' world'` | `'hello world'` |
| `'hello' copyFrom: 2 to: 4` | `'ell'` |
