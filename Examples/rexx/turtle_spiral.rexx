/* REXX turtle graphics: draw a spiral staircase */
DO i = 1 TO 18
  FORWARD i * 8
  RIGHT 90
END
PENUP
MOVE 120
PENDOWN
DO petal = 1 TO 12
  DO side = 1 TO 4
    FORWARD 40
    RIGHT 90
  END
  RIGHT 30
END
HOME
SAY 'Spiral turtle drawing complete'
