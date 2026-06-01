⍝ APL Sorting and Searching
⍝ Demonstrates grade-up, grade-down, and binary search

⍝ ── Grade-up and grade-down ───────────────────────────────────────────
v ← 64 25 12 90 3 77 44 18 55 37

⎕← '=== Sorting with ⍋ (grade-up) and ⍒ (grade-down) ==='
⎕← 'Original:   ' , ⍕v
⎕← 'Sorted ↑:   ' , ⍕v[⍋v]
⎕← 'Sorted ↓:   ' , ⍕v[⍒v]
⎕← 'Min index:  ' , ⍕⊃⍋v
⎕← 'Max index:  ' , ⍕⊃⍒v

⍝ ── Sort strings ─────────────────────────────────────────────────────
⎕← ''
⎕← '=== Sorting Words ==='
words ← 'banana' 'apple' 'cherry' 'date' 'elderberry'
⎕← 'Original: ' , ⍕words
⎕← 'Sorted:   ' , ⍕words[⍋words]

⍝ ── Matrix sorting (sort rows by first column) ───────────────────────
⎕← ''
⎕← '=== Matrix Sort ==='
M ← 3 3⍴3 9 7 1 5 8 6 2 4
⎕← 'Matrix:' ⋄ ⎕← M
sorted_M ← M[⍋M[;1];]
⎕← 'Sorted by col 1:' ⋄ ⎕← sorted_M

⍝ ── Rank (percentile position) ───────────────────────────────────────
⎕← ''
⎕← '=== Rank (position in sorted order) ==='
data ← 85 92 78 95 88 72 90
⎕← 'Data:    ' , ⍕data
⎕← 'Rank ↑:  ' , ⍕⍋⍋data   ⍝ rank from smallest
⎕← 'Rank ↓:  ' , ⍕⍒⍒data   ⍝ rank from largest

⍝ ── Membership and find ──────────────────────────────────────────────
⎕← ''
⎕← '=== Search Operations ==='
haystack ← 10 20 30 40 50 60 70 80 90
⎕← 'Array:  ' , ⍕haystack
⎕← '40∊ array: ' , ⍕40∊haystack
⎕← '99∊ array: ' , ⍕99∊haystack
⎕← 'Index of 50: ' , ⍕haystack⍳50
⎕← 'Indices of 20 50 80: ' , ⍕haystack⍳20 50 80
