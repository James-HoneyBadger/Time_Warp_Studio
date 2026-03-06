⍝ ================================================================
⍝  APL ARRAY OPERATIONS & VECTOR PROGRAMMING SHOWCASE
⍝  Demonstrates APL's powerful array-oriented operators,
⍝  vector math, reductions, scans, sorting, and statistics.
⍝ ================================================================

⍝ ────── VECTOR CREATION ────────────────────────────────────────

⎕ ← '=== VECTOR CREATION ==='

⎕ ← 'Iota (⍳) — generate 1..10:'
⎕ ← ⍳10

v ← 42 7 19 3 55 28 11 66 1 34
⎕ ← 'Data vector v:'
⎕ ← v

⍝ ────── RESHAPE (⍴) ───────────────────────────────────────────

⎕ ← '=== RESHAPE (⍴) ==='

A ← 3 3 ⍴ 2 7 1 3 5 8 4 6 9
⎕ ← 'A ← 3 3 ⍴ 2 7 1 3 5 8 4 6 9:'
⎕ ← A

B ← 3 3 ⍴ 1 2 3 4 5 6 7 8 9
⎕ ← 'B ← 3 3 ⍴ 1 2 3 4 5 6 7 8 9:'
⎕ ← B

⍝ Reshape with cycling: 6 elements repeated into 9
C ← 3 3 ⍴ 1 0 0 0 1 0
⎕ ← 'Pseudo-identity (cycling ⍴):'
⎕ ← C

⍝ ────── ELEMENT-WISE ARITHMETIC ───────────────────────────────

⎕ ← '=== ELEMENT-WISE ARITHMETIC ==='

⎕ ← 'A + B (element-wise addition):'
⎕ ← A + B

⎕ ← 'A × B (element-wise multiplication):'
⎕ ← A × B

⎕ ← 'A - B (element-wise subtraction):'
⎕ ← A - B

⍝ ────── SCALAR EXTENSION (broadcasting) ───────────────────────

⎕ ← '=== SCALAR EXTENSION ==='

⎕ ← 'A × 3 (scale every element):'
⎕ ← A × 3

⎕ ← 'A + 10 (broadcast add):'
⎕ ← A + 10

⎕ ← 'A * 2 (square each element):'
⎕ ← A * 2

⍝ ────── COMPARISONS & BOOLEAN MASKS ──────────────────────────

⎕ ← '=== COMPARISONS ==='

⎕ ← 'A > 5 (boolean mask):'
⎕ ← A > 5

⎕ ← 'A = B (equality test):'
⎕ ← A = B

⎕ ← 'A ≥ 4:'
⎕ ← A ≥ 4

⍝ ────── REDUCTIONS ────────────────────────────────────────────

⎕ ← '=== REDUCTIONS ==='

⎕ ← 'Sum of A (+/A):'
⎕ ← +/A

⎕ ← 'Product of first 5 integers (×/⍳5):'
⎕ ← ×/⍳5

nums ← 8 3 15 1 42 6
⎕ ← 'nums ← 8 3 15 1 42 6'

⎕ ← 'Max of nums (⌈/nums):'
⎕ ← ⌈/nums

⎕ ← 'Min of nums (⌊/nums):'
⎕ ← ⌊/nums

⎕ ← 'Sum of nums (+/nums):'
⎕ ← +/nums

⎕ ← 'Product of nums (×/nums):'
⎕ ← ×/nums

⍝ ────── SCANS (running totals) ───────────────────────────────

⎕ ← '=== SCANS (cumulative) ==='

⎕ ← 'Running sum of nums (+\nums):'
⎕ ← +\nums

⎕ ← 'Running max (⌈\nums):'
⎕ ← ⌈\nums

⎕ ← 'Running product of ⍳5 (×\⍳5) = factorials:'
⎕ ← ×\⍳5

⍝ ────── SORTING & GRADING ────────────────────────────────────

⎕ ← '=== SORTING & GRADING ==='

⎕ ← 'Grade up (⍋nums) — indices for ascending:'
⎕ ← ⍋nums

⎕ ← 'Grade down (⍒nums) — indices for descending:'
⎕ ← ⍒nums

⍝ ────── CATENATION, TAKE, DROP ────────────────────────────────

⎕ ← '=== CATENATION, TAKE, DROP ==='

X ← 10 20 30 40 50
Y ← 60 70 80

⎕ ← 'X , Y (catenate):'
⎕ ← X , Y

⎕ ← '3 ↑ X (take first 3):'
⎕ ← 3 ↑ X

⎕ ← '2 ↓ X (drop first 2):'
⎕ ← 2 ↓ X

⍝ ────── REVERSE & SHAPE ──────────────────────────────────────

⎕ ← '=== REVERSE & SHAPE ==='

⎕ ← 'Reverse of ⍳8 (⌽⍳8):'
⎕ ← ⌽⍳8

⎕ ← 'Shape of A (⍴A) — element count:'
⎕ ← ⍴A

⍝ ────── MEMBERSHIP ────────────────────────────────────────────

⎕ ← '=== MEMBERSHIP ==='

⎕ ← '5 ∊ ⍳10 (is 5 in 1..10?):'
⎕ ← 5 ∊ ⍳10

⎕ ← '99 ∊ ⍳10 (is 99 in 1..10?):'
⎕ ← 99 ∊ ⍳10

⍝ ────── PRACTICAL: STATISTICS ─────────────────────────────────

⎕ ← '=== STATISTICS ON nums ==='

⎕ ← 'nums:'
⎕ ← nums
⎕ ← 'Count (#nums):'
N ← ⍴nums
⎕ ← N
⎕ ← 'Sum (+/nums):'
⎕ ← +/nums
⎕ ← 'Max (⌈/nums):'
⎕ ← ⌈/nums
⎕ ← 'Min (⌊/nums):'
⎕ ← ⌊/nums

⎕ ← '=== Array operations showcase complete! ==='
