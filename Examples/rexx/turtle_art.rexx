/* REXX Turtle Graphics - Spiral Pattern */
PENDOWN
COLOR 'blue'

/* Draw a square spiral */
size = 10
DO i = 1 TO 20
  FORWARD size
  RIGHT 90
  size = size + 8
END

HOME
PENUP
