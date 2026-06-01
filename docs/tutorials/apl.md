# APL Tutorial

## Introduction

APL (A Programming Language) was designed by Kenneth Iverson in 1966. It is famous for its terse, symbolic notation and powerful array-manipulation primitives. An entire algorithm can often be expressed in a single line.

**Key characteristics:**
- Array-oriented: all operations work on scalars, vectors, and matrices
- Right-to-left evaluation (no operator precedence)
- Unique symbol set: `⍳ ⍴ + × ÷ ⌈ ⌊ ⌽ ⍋ ⍒ ∊ ←`
- `¯` (high-minus) for negative literals: `¯5` not `-5`

## Hello World

```apl
⎕← 'Hello, World!'
```

`⎕←` sends output to the display.

## Variables

```apl
x ← 42
name ← 'Alice'
⎕← x
⎕← name
```

`←` is the assignment arrow.

## Numbers and Arithmetic

```apl
⎕← 3 + 4       ⍝ 7
⎕← 10 - 3      ⍝ 7
⎕← 3 × 4       ⍝ 12
⎕← 10 ÷ 4      ⍝ 2.5
⎕← 2 * 8       ⍝ 256 (power)
⎕← ¯5          ⍝ negative five
```

`⍝` starts a comment.

## Vectors

```apl
v ← 1 2 3 4 5
⎕← v
⎕← v + 10      ⍝ 11 12 13 14 15 (scalar extension)
⎕← v × v       ⍝ 1 4 9 16 25 (element-wise)
```

## Iota ⍳

Generate a sequence from 1 to N:

```apl
⎕← ⍳5          ⍝ 1 2 3 4 5
```

## Shape ⍴

Get the shape of an array, or reshape data:

```apl
v ← ⍳6
⎕← ⍴v          ⍝ 6 (length)

m ← 2 3 ⍴ ⍳6   ⍝ 2x3 matrix
⎕← m
```

## Reduction f/

Apply a function between all elements (like `fold`/`reduce`):

```apl
⎕← +/ ⍳10      ⍝ 55 (sum 1..10)
⎕← ×/ ⍳5       ⍝ 120 (5!)
⎕← ⌈/ 3 1 4 1 5  ⍝ 5 (maximum)
⎕← ⌊/ 3 1 4 1 5  ⍝ 1 (minimum)
```

## Scan f\

Running cumulative results:

```apl
⎕← +\ ⍳5       ⍝ 1 3 6 10 15
⎕← ×\ ⍳5       ⍝ 1 2 6 24 120
```

## Array Operations

```apl
a ← 1 2 3
b ← 4 5 6

⎕← a , b       ⍝ 1 2 3 4 5 6   (catenate)
⎕← 3 ↑ ⍳10    ⍝ 1 2 3         (take first 3)
⎕← 7 ↓ ⍳10    ⍝ 8 9 10        (drop first 7)
⎕← ⌽ ⍳5       ⍝ 5 4 3 2 1     (reverse)
```

## Comparisons

Comparisons return `1` (true) or `0` (false):

```apl
⎕← (⍳5) = 3     ⍝ 0 0 1 0 0
⎕← (⍳5) > 3     ⍝ 0 0 0 1 1
```

## Inner and Outer Products

Inner product (generalised dot product):

```apl
a ← 1 2 3
b ← 4 5 6
⎕← a +.× b      ⍝ 32 (1×4 + 2×5 + 3×6)
```

Outer product (all combinations):

```apl
⎕← (⍳3) ∘.× (⍳3)
⍝ 1 2 3
⍝ 2 4 6
⍝ 3 6 9
```

## Membership ∊

```apl
⎕← 3 ∊ 1 2 3 4 5    ⍝ 1 (true)
⎕← 9 ∊ 1 2 3 4 5    ⍝ 0 (false)
```

## Grade Up / Sort

`⍋` returns the indices that would sort a vector:

```apl
data ← 3 1 4 1 5
⎕← ⍋ data           ⍝ 2 4 1 3 5 (indices)
⎕← data[⍋ data]     ⍝ 1 1 3 4 5 (sorted)
```

## System Variables

```apl
⎕IO ← 0        ⍝ index origin: 0-based indexing
⎕IO ← 1        ⍝ index origin: 1-based (default)
⎕PP ← 6        ⍝ print precision: 6 significant digits
```

## Monadic Functions Reference

| Symbol | Function | Example |
|--------|----------|---------|
| `⍳N` | Iota (sequence) | `⍳5` → `1 2 3 4 5` |
| `⍴A` | Shape | `⍴2 3⍴⍳6` → `2 3` |
| `⌽A` | Reverse | `⌽1 2 3` → `3 2 1` |
| `⍋A` | Grade up | `⍋3 1 2` → `2 3 1` |
| `⍒A` | Grade down | `⍒3 1 2` → `1 3 2` |
| `~A` | Logical not | `~0 1 0` → `1 0 1` |
| `-A` | Negate | `-3` → `¯3` |
| `\|A` | Absolute value | `\|¯5` → `5` |
| `⌈A` | Ceiling | `⌈2.3` → `3` |
| `⌊A` | Floor | `⌊2.7` → `2` |
