⍝ APL in Time Warp Studio.
⍝
⍝ The first half is the language itself: arrays, and the two rules that make APL
⍝ unlike anything else here — evaluation is right to left with no precedence, and
⍝ a function's left argument is the whole adjacent strand of values.
⍝
⍝ The second half draws, using the turtle words this studio adds to the language.

⍳5
+/⍳100
2 3 ⍴ ⍳6
1 2 3 + 10
2 × 3 + 4
(2 × 3) + 4
+ / 1 2 3 × 4 5 6
⌽ 1 2 3 4
3 ↑ 1 2

⍝ A function of your own, and then everything above folded into one line.
∇ R ← AVERAGE X
  R ← (+/X) ÷ ⍴X
∇
AVERAGE 1 2 3 4

⍝ Turtle graphics: FD, RT, LT, PU, PD, HOME and CS are Time Warp extensions.
⍝ A hexagon, which is six sides of the same two moves.
FD 60
RT 60
FD 60
RT 60
FD 60
RT 60
FD 60
RT 60
FD 60
RT 60
FD 60
RT 60

⍝ ... and a spoke to the middle, then a smaller one inside it.
RT 30
FD 52
LT 180
FD 60
RT 90
FD 30
LT 180
FD 60
