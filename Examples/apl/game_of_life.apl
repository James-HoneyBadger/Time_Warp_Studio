⍝ APL Game of Life
⍝ Conway's Game of Life using array operations

⍝ ── Rules ────────────────────────────────────────────────────────────
⍝ A cell is alive (1) if:
⍝   - It has exactly 3 alive neighbors (birth), OR
⍝   - It is alive AND has 2 or 3 alive neighbors (survival)

life ← {
    ⍝ Count neighbors using 8-directional sum of shifted grids
    N ← ¯1 0 1
    nbr ← +/ , (⌽[1]N) ∘.+ ⌽[2]N  ⍝ alternative: sum of 8 shifts
    ⍝ Simple neighbor count via convolution
    n ← ⍺ ⌺ 3 3 ⊢ ⍵
    ⍝ Apply Life rules
    (3=n) ∨ ⍵ ∧ 2=n  
}

⍝ ── Print grid ───────────────────────────────────────────────────────
show ← {
    chars ← '·█'[1+⍵]
    ⎕← ↑{⍵}¨↓chars
}

⍝ ── Glider pattern ───────────────────────────────────────────────────
⎕← '=== Conway''s Game of Life ==='
⎕← '(· = dead, █ = alive)'
⎕← ''

⍝ 8×8 grid with glider
G ← 8 8⍴0
G[2;3] ← 1
G[3;4] ← 1
G[4;2] ← 1
G[4;3] ← 1
G[4;4] ← 1

⍝ Generation 0
⎕← 'Generation 0:'
show G

⍝ Step function (simple version without wrapping)
step ← {
    grid ← ⍵
    rows ← ⊃⍴grid
    cols ← 1↓⍴grid
    result ← rows cols ⍴ 0
    ⎕← grid  ⍝ show current
    grid
}

⍝ Run 3 generations
⎕← ''
⎕← 'Generation 1:'
G1 ← 1 life⍤2⊢G
show G1

⎕← ''
⎕← 'Generation 2:'
G2 ← 1 life⍤2⊢G1
show G2

⎕← ''
⎕← '=== Blinker (oscillator) ==='
⍝ 5x5 grid, blinker in middle
B ← 5 5⍴0
B[3;2 3 4] ← 1
⎕← 'Generation 0 (horizontal):'
show B
B1 ← 1 life⍤2⊢B
⎕← 'Generation 1 (vertical):'
show B1
