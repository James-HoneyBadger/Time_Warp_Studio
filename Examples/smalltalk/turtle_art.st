"Smalltalk Turtle Graphics"
| step angle |
step := 5.
angle := 89.

"Draw a spiral"
1 to: 60 do: [:i |
    Turtle forward: step.
    Turtle right: angle.
    step := step + 3].
