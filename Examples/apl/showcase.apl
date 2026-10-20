⍝ ============================================================
⍝ THE ARRAY WORKSHOP
⍝ An APL showcase: every family of function, the right-to-left
⍝ rule, a defined function, and turtle geometry.
⍝ ============================================================

'--- 1. SCALARS, VECTORS AND TABLES'
42
1 2 3
2 3 ⍴ ⍳ 6
'HELLO'
(1 2)(3 4)

'--- 2. NO PRECEDENCE: RIGHT TO LEFT'
2 × 3 + 4
(2 × 3) + 4

'--- 3. SCALAR FUNCTIONS'
⌈ 2.2
⌊ 2.8
3 ⌈ 5
3 ⌊ 5
| ¯3
3 | 10
× ¯3
÷ 4
2 * 10
⍟ 1

'--- 4. STRUCTURAL FUNCTIONS'
⍳ 5
⍴ 1 2 3
2 3 ⍴ ⍳ 6
⌽ 1 2 3 4
1 ⌽ 1 2 3
3 ↑ 1 2
1 ↓ 1 2 3
1 2 , 3
, 2 2 ⍴ ⍳ 4
⍉ 2 3 ⍴ ⍳ 6
⍋ 3 1 2
1 ∈ 1 2 3
⍕ 42

'--- 5. REDUCE AND SCAN'
+/ ⍳ 100
+SCAN 1 2 3
×/ 1 2 3 4

'--- 6. A DEFINED FUNCTION'
∇ R ← AVERAGE X
  R ← (+/X) ÷ ⍴X
∇
AVERAGE 1 2 3 4
AVERAGE 10 20 30

'--- 7. TURTLE GEOMETRY'
CS
FD 80
RT 60
FD 80
RT 60
FD 80
RT 60
FD 80
RT 60
FD 80
RT 60
FD 80
RT 60
'After the hexagon, the turtle reports its position:'
TX
TY
THETA
