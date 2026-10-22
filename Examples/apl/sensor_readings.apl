⍝ ============================================================
⍝ SENSOR READINGS
⍝ An APL program about arrays, which is what APL is: one
⍝ expression per line, no loops, and the reduction operators
⍝ doing the work a for-loop would do anywhere else. Labels are
⍝ built with ⍕ and catenated, because a line prints one value, and
⍝ each reduction in arithmetic is parenthesised: `(⌈/V)-(⌊/V)` is a
⍝ spread, while the same thing unparenthesised is not.
⍝ ============================================================

'--- 1. THE READINGS'
V ← 12 7 19 4 15 8 21 3 11 6 17 2
V
'count      : ' , ⍕ (⍴ V)
'first four : ' , ⍕ (4 ↑ V)
'last four  : ' , ⍕ (¯4 ↑ V)
'in order   : ' , ⍕ (⍋ V)

'--- 2. WHAT A WEEK OF NUMBERS IS ASKED'
'sum        : ' , ⍕ (+/ V)
'largest    : ' , ⍕ (⌈/ V)
'smallest   : ' , ⍕ (⌊/ V)
'mean       : ' , ⍕ ((+/ V) ÷ ⍴ V)
'spread     : ' , ⍕ ((⌈/ V) - (⌊/ V))

'--- 3. FUNCTIONS OF MY OWN (right to left, no precedence)'
∇ R ← AVERAGE X
  R ← (+/ X) ÷ ⍴ X
∇
∇ R ← SPREAD X
  R ← (⌈/ X) - (⌊/ X)
∇
'average    : ' , ⍕ (AVERAGE V)
'spread     : ' , ⍕ (SPREAD V)
'2 × 3 + 4  : ' , ⍕ (2 × 3 + 4)

'--- 4. RANKING: GRADE GIVES THE ORDER, NOT THE VALUES'
'grade      : ' , ⍕ (⍋ V)
'position of 19 in the list: ' , ⍕ (V ⍳ 19)

'--- 5. THE SAME TWELVE AS A 3×4 TABLE'
M ← 3 4 ⍴ V
M
'shape      : ' , ⍕ (⍴ M)
'transposed : '
⍉ M
'row totals : ' , ⍕ (+/ M)
'col totals : ' , ⍕ (+/ ⍉ M)
'grand total: ' , ⍕ (+/ ,M)
'flattened  : ' , ⍕ (,M)

'--- 6. A COMPARISON MAKES A MASK, AND A MASK COUNTS'
'above ten  : ' , ⍕ (V > 10)
'how many   : ' , ⍕ (+/ V > 10)

'--- 7. DRAWING, because a reduction is a shape too'
CS
HOME
'First the count: a twelve-sided figure, a side per reading.'
FD 30
RT 30
FD 30
RT 30
FD 30
RT 30
FD 30
RT 30
FD 30
RT 30
FD 30
RT 30
FD 30
RT 30
FD 30
RT 30
FD 30
RT 30
FD 30
RT 30
FD 30
RT 30
FD 30
RT 30
HOME
'Then the mean: a square whose side is the average, times four.'
S ← ((+/ V) ÷ (⍴ V)) × 4
FD S
RT 90
FD S
RT 90
FD S
RT 90
FD S
RT 90
HOME
'Drawn: a side per reading, and a square whose size the mean decided.'
