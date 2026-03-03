# APL Programming Tutorial

APL (A Programming Language) was created by Kenneth Iverson in 1962. It uses a rich set of mathematical symbols and operates on arrays as first-class values. APL has influenced every array language since — NumPy, MATLAB, Julia, and more.

## Special Symbols

APL uses unique glyphs. Common ones:

| Glyph | Name | Meaning |
|-------|------|---------|
| `←` | Assignment | `x ← 5` |
| `⍳` | Iota | Generate sequence `⍳5 → 1 2 3 4 5` |
| `⍴` | Rho | Shape/reshape of array |
| `+/` | Reduce | Sum: `+/1 2 3 → 6` |
| `×/` | Reduce | Product: `×/⍳5 → 120` |
| `∘.×` | Outer product | Multiplication table |
| `⌈` | Ceiling / max | |
| `⌊` | Floor / min | |
| `⍉` | Transpose | |
| `⌽` | Reverse | |
| `↑` | Take | First N elements |
| `↓` | Drop | Skip N elements |

## Hello World

```apl
⎕←'Hello from APL!'
⎕←'Welcome to Time Warp Studio'
```

`⎕←` prints to the screen (quote-print).

## Arithmetic

```apl
⎕← 2 + 3          ⍝ 5
⎕← 10 - 4         ⍝ 6
⎕← 6 × 7          ⍝ 42
⎕← 10 ÷ 4         ⍝ 2.5
⎕← 2 * 10         ⍝ 1024
⎕← |¯3            ⍝ 3  (absolute value)
⎕← ⌊3.7           ⍝ 3  (floor)
⎕← ⌈3.2           ⍝ 4  (ceiling)
```

Note: `¯` (high minus) is APL's negative sign: `¯5` means -5.

## Arrays

```apl
⍝ Create a vector (1D array)
v ← 1 2 3 4 5
⎕← v
⎕← ⍴v             ⍝ shape: 5

⍝ Iota generates sequences
⎕← ⍳10            ⍝ 1 2 3 4 5 6 7 8 9 10

⍝ Array arithmetic (whole-array at once)
⎕← v × 2          ⍝ 2 4 6 8 10
⎕← v + v          ⍝ 2 4 6 8 10
⎕← v * 2          ⍝ 1 4 9 16 25 (element-wise square)

⍝ Scalar operations broadcast automatically
⎕← 100 + ⍳5       ⍝ 101 102 103 104 105
```

## Reduction

```apl
⍝ +/ sums all elements
⎕← +/ ⍳100        ⍝ 5050  (sum 1..100)

⍝ ×/ multiplies (factorial)
⎕← ×/ ⍳10         ⍝ 3628800 = 10!

⍝ ⌈/ max element, ⌊/ min element
⎕← ⌈/ 3 1 4 1 5 9 ⍝ 9
⎕← ⌊/ 3 1 4 1 5 9 ⍝ 1
```

## Outer Product

```apl
⍝ Multiplication table
⎕← (⍳5) ∘.× (⍳5)
```

Output:
```
 1  2  3  4  5
 2  4  6  8 10
 3  6  9 12 15
 4  8 12 16 20
 5 10 15 20 25
```

## Matrices

```apl
⍝ Reshape into matrix
m ← 3 3 ⍴ ⍳9          ⍝ 3×3 matrix from 1..9
⎕← m
⎕← ⍴m                  ⍝ shape: 3 3
⎕← ⍉m                  ⍝ transpose
⎕← ⌽m                  ⍝ reverse rows
⎕← ⊖m                  ⍝ reverse columns (flip)
```

## Boolean and Comparison

```apl
v ← 3 1 4 1 5 9 2 6
⎕← v > 4              ⍝ 0 0 0 0 1 1 0 1  (boolean mask)
⎕← (v > 4) / v        ⍝ 5 9 6  (compress / select)
⎕← +/ v > 4           ⍝ 3  (count elements > 4)
```

## Defined Functions

```apl
⍝ Dfn (direct function)
factorial ← {×/⍳⍵}
⎕← factorial 10      ⍝ 3628800

primes ← {
    mask ← (⍳⍵) ∘.| ⍳⍵   ⍝ outer product with remainder
    2 ↓ +/ (mask = 0) = 1
}

⍝ Traditional APL function
∇ result ← SQUARE x
    result ← x × x
∇
⎕← SQUARE 9    ⍝ 81
```

## Further Reading

- [Examples/apl/](../Examples/apl/) — 10 APL example programs
- [Language Guide: APL](LANGUAGE_GUIDE.md#apl)
