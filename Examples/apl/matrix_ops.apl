⍝ ================================================================
⍝  APL MATRIX OPERATIONS & ARRAY PROGRAMMING SHOWCASE
⍝  Demonstrates APL's powerful array-oriented operators,
⍝  matrix math, statistics, and tacit/dfn programming style.
⍝ ================================================================

⍝ ────── MATRIX CREATION UTILITIES ──────────────────────────────

⍝ Identity matrix
identity ← {⍵ ⍵ ρ 1,⍵ρ0}     ⍝ dfn: n → n×n identity

⍝ Range vector (like APL's ⍳)
⍝ range ← ⍳ is built-in

⍝ 3×3 test matrix
A ← 3 3 ρ 2 7 1 3 5 8 4 6 9
B ← 3 3 ρ 1 2 3 4 5 6 7 8 9

⎕ ← 'Matrix A:'
⎕ ← A
⎕ ← 'Matrix B:'
⎕ ← B

⍝ ────── BASIC MATRIX OPERATIONS ────────────────────────────────

⎕ ← '--- Matrix Addition A+B ---'
⎕ ← A + B

⎕ ← '--- Element-wise Multiplication A×B ---'
⎕ ← A × B

⎕ ← '--- Matrix Multiply A+.×B ---'
⎕ ← A +.× B

⎕ ← '--- Transpose of A (⍉A) ---'
⎕ ← ⍉A

⍝ ────── SCALAR OPERATIONS (broadcast) ──────────────────────────

⎕ ← '--- A scaled by 3 ---'
⎕ ← 3 × A

⎕ ← '--- A + 10 (scalar broadcast) ---'
⎕ ← A + 10

⎕ ← '--- Boolean mask: A > 5 ---'
⎕ ← A > 5

⎕ ← '--- Elements of A greater than 5 ---'
⎕ ← (A > 5) / ,A     ⍝ compress then ravel

⍝ ────── ROW / COLUMN REDUCTIONS ─────────────────────────────────

⎕ ← '--- Row sums (+/A along axis 2) ---'
⎕ ← +/A              ⍝ sum along last axis

⎕ ← '--- Column sums (+⌿A along axis 1) ---'
⎕ ← +⌿A              ⍝ sum along first axis

⎕ ← '--- Row maxima (⌈/A) ---'
⎕ ← ⌈/A

⎕ ← '--- Column minima (⌊⌿A) ---'
⎕ ← ⌊⌿A

⎕ ← '--- Grand total ---'
⎕ ← +/+/A

⍝ ────── SORTING & GRADING ───────────────────────────────────────

v ← 42 7 19 3 55 28 11 66 1 34

⎕ ← 'Vector v:'
⎕ ← v

⎕ ← '--- Grade up (⍋v) — indices that sort ascending ---'
⎕ ← ⍋v

⎕ ← '--- Sorted ascending (v[⍋v]) ---'
⎕ ← v[⍋v]

⎕ ← '--- Sorted descending (v[⍒v]) ---'
⎕ ← v[⍒v]

⍝ ────── STATISTICS USING ARRAY OPERATORS ────────────────────────

mean  ← {(+/⍵)÷≢⍵}
vari  ← {mean←(+/⍵)÷≢⍵ ⋄ (+/(⍵-mean)*2)÷≢⍵}
stdev ← {⍵ ← ⍺⍺ ⋄ (vari ⍵)*0.5}   ⍝ operator form

data ← 23 45 12 67 34 89 56 78 43 21

⎕ ← 'Data:', data
⎕ ← 'Mean:   ', ⍕mean data
⎕ ← 'Variance:', ⍕vari data
⎕ ← 'Std Dev: ', ⍕(vari data)*0.5

⍝ Median using sort
median ← {
    s ← ⍵[⍋⍵]
    n ← ≢⍵
    0=2|n : s[⌈n÷2]          ⍝ odd length
    (+/s[(n÷2)+0 1])÷2        ⍝ even length
}

⎕ ← 'Median: ', ⍕median data

⍝ ────── STRING OPERATIONS ───────────────────────────────────────

msg ← 'Hello, APL World!'

⎕ ← '--- Reverse string ---'
⎕ ← ⌽msg

⎕ ← '--- Uppercase ---'
⎕ ← ⎕UCS (⎕UCS msg) - (('a'⍸msg)*32)    ⍝ shift lowercase letters

⎕ ← '--- Character codes ---'
⎕ ← ⎕UCS msg

⍝ ────── OUTER PRODUCT ───────────────────────────────────────────

⎕ ← '--- Multiplication table 1..5 (∘.× outer product) ---'
⎕ ← (⍳5) ∘.× (⍳5)

⎕ ← '--- Truth table AND (∘.∧) ---'
bits ← 0 1
⎕ ← bits ∘.∧ bits

⎕ ← '--- Distance matrix (∘.-) ---'
pts ← 1 3 5 7 9
⎕ ← |pts ∘.- pts

⍝ ────── FIBONACCI WITH SCAN ─────────────────────────────────────

⍝ Generate Fibonacci using +\ scan on pair sums
fib10 ← {
    v ← 0 1
    {v , (+/v↑⍨¯2), 0 ↓⍨ 1}⍣(⍵-2) ⊢ v
}

⎕ ← '--- First 10 Fibonacci numbers ---'
⎕ ← fib10 10

⍝ ────── MATRIX DETERMINANT (2×2 and 3×3) ───────────────────────

det2 ← {m←⍵ ⋄ (m[1;1]×m[2;2])-(m[1;2]×m[2;1])}

⍝ 3×3 via cofactor expansion
det3 ← {
    m←⍵
    (m[1;1]×(m[2;2]×m[3;3])-(m[2;3]×m[3;2]))
    -(m[1;2]×(m[2;1]×m[3;3])-(m[2;3]×m[3;1]))
    +(m[1;3]×(m[2;1]×m[3;2])-(m[2;2]×m[3;1]))
}

⎕ ← '--- Determinant of A ---'
⎕ ← det3 A

⍝ ────── PRIMES VIA SIEVE (APL one-liner) ────────────────────────

sieve ← {(~b\∨⌿0=(⍳⌊⍵*0.5)∘.|⍳⍵)/⍳⍵}    ⍝ classic APL sieve
⎕ ← '--- Primes up to 50 ---'
⎕ ← sieve 50

⍝ ────── ARRAY RESHAPING ─────────────────────────────────────────

flat ← ⍳24
⎕ ← '--- 24 elements reshaped to 4×6 ---'
⎕ ← 4 6 ρ flat

⎕ ← '--- Same data as 2×3×4 cube (shape) ---'
cube ← 2 3 4 ρ flat
⎕ ← ρcube

⎕ ← '--- Depth of nested array ---'
nested ← (1 2 3)(4 5)(6 7 8 9)
⎕ ← ≡nested

⍝ ────── TACIT (POINT-FREE) PROGRAMMING ─────────────────────────

⍝ Average as a fork: mean ← +/ ÷ ≢
avg ← +/ ÷ ≢
⎕ ← '--- Average of 1..20 (tacit fork) ---'
⎕ ← avg ⍳20

⍝ Sum of squares
sumsq ← +/ ∘ (*∘2)
⎕ ← '--- Sum of squares 1..10 ---'
⎕ ← sumsq ⍳10

⍝ ────── ENCODE / DECODE (BASE CONVERSION) ──────────────────────

n ← 255
⎕ ← '--- 255 in binary (8 bits) ---'
⎕ ← 8⍴2⊤n

⎕ ← '--- 255 in hex digits ---'
⎕ ← 2⍴16⊤n

⎕ ← '--- Decode 1 0 1 1 from binary ---'
⎕ ← 2⊥1 0 1 1

⎕ ← 'APL showcase complete.'
