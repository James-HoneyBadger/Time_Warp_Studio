⍝ APL Mathematical Operations

⍝ Arithmetic reductions
⎕← 'Sum of 1..10:'
⎕← +/ ⍳10

⎕← 'Product of 1..5 (5!):'
⎕← ×/ ⍳5

⎕← 'Maximum of vector:'
⎕← ⌈/ 3 1 4 1 5 9 2 6

⎕← 'Minimum of vector:'
⎕← ⌊/ 3 1 4 1 5 9 2 6

⍝ Prefix scans
⎕← 'Running sum of 1..6:'
⎕← +\ ⍳6

⎕← 'Running product of 1..6:'
⎕← ×\ ⍳6

⍝ Inner product (matrix dot product)
a ← 1 2 3
b ← 4 5 6
⎕← 'Dot product 1 2 3 and 4 5 6:'
⎕← a +.× b

⍝ Outer product
⎕← 'Multiplication table 1..4 x 1..4:'
⎕← (⍳4) ∘.× (⍳4)

⍝ Ceiling and floor
v ← 2 7 1 8 2 8
⎕← 'Absolute value of ¯3 1 ¯4 1 ¯5:'
⎕← | ¯3 1 ¯4 1 ¯5
