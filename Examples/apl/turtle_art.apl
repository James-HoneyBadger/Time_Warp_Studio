⍝ APL Turtle Graphics - Polygon spiral
n ← 3
step ← 20
1 to: 18 do: [:i |
    FORWARD step
    RIGHT (360 ÷ n)
    step ← step + 5]
