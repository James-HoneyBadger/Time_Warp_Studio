⍝ APL Array Operations Demo
⎕IO ← 1

⍝ Iota: generate sequence
v ← ⍳10
⎕← 'Vector 1..10:'
⎕← v

⍝ Shape
⎕← 'Shape of v:'
⎕← ⍴v

⍝ Reshape into matrix
m ← 2 3 ⍴ ⍳6
⎕← '2x3 matrix:'
⎕← m

⍝ Catenate
a ← 1 2 3
b ← 4 5 6
⎕← 'Catenated:'
⎕← a , b

⍝ Take and drop
⎕← 'First 3 of ⍳10:'
⎕← 3 ↑ ⍳10
⎕← 'Drop first 7:'
⎕← 7 ↓ ⍳10

⍝ Reverse
⎕← 'Reversed:'
⎕← ⌽ 1 2 3 4 5

⍝ Grade up / sort
data ← 3 1 4 1 5 9 2 6
⎕← 'Original:'
⎕← data
⎕← 'Grade-up indices:'
⎕← ⍋ data
