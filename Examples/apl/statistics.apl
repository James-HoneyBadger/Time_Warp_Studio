⍝ APL Statistics

data ← 4 7 2 9 1 8 3 6 5 10

⍝ Count
⎕← 'Count:'
⎕← ⍴ data

⍝ Sum
⎕← 'Sum:'
⎕← +/ data

⍝ Mean
⎕← 'Mean:'
⎕← (+/ data) ÷ (⍴ data)

⍝ Maximum and minimum
⎕← 'Max:'
⎕← ⌈/ data

⎕← 'Min:'
⎕← ⌊/ data

⍝ Sorted data (via grade-up)
sorted ← data[⍋ data]
⎕← 'Sorted:'
⎕← sorted

⍝ Member test
⎕← '5 in data?'
⎕← 5 ∊ data

⎕← '11 in data?'
⎕← 11 ∊ data
