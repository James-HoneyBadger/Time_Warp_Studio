\ Turtle Art — Geometric Art Generator in Forth
\ Demonstrates stack operations, loops, and turtle graphics

: BANNER
  ." ===============================" CR
  ."   Turtle Art - Forth Style" CR
  ." ===============================" CR CR ;

: DEGREES  ;  \ angle placeholder

\ Draw a regular polygon
: POLYGON ( sides size -- )
  SWAP 0 DO
    DUP FORWARD
    360 I 1+ / RIGHT
  LOOP DROP ;

\ Draw a star shape
: STAR ( points size -- )
  SWAP 0 DO
    DUP FORWARD
    180 I 1+ 180 * I 1+ / - RIGHT
  LOOP DROP ;

\ Draw a spiral
: SPIRAL ( steps start-size increment -- )
  ROT 0 DO
    DUP FORWARD
    15 RIGHT
    OVER +
  LOOP DROP DROP ;

\ Draw concentric circles
: CIRCLES ( count spacing -- )
  PENUP HOME PENDOWN
  SWAP 0 DO
    DUP I 1+ * CIRCLE
  LOOP DROP ;

BANNER

\ === Scene 1: Colourful polygon garden ===
." Drawing polygon garden..." CR

PENUP HOME PENDOWN
2 SETPENWIDTH

\ Red triangle
255 80 80 SETPENCOLOR
PENUP -120 80 SETXY PENDOWN
3 50 POLYGON

\ Green square
80 200 80 SETPENCOLOR
PENUP 0 80 SETXY PENDOWN
4 40 POLYGON

\ Blue pentagon
80 80 255 SETPENCOLOR
PENUP 120 80 SETXY PENDOWN
5 35 POLYGON

\ Purple hexagon
180 80 255 SETPENCOLOR
PENUP 0 -20 SETXY PENDOWN
6 30 POLYGON

\ === Scene 2: Spiral pattern ===
." Drawing golden spiral..." CR
PENUP HOME PENDOWN
255 200 50 SETPENCOLOR
1 SETPENWIDTH
36 5 2 SPIRAL

\ === Scene 3: Nested squares ===
." Drawing nested squares..." CR
PENUP HOME PENDOWN
0 200 200 SETPENCOLOR
8 0 DO
  80 I 10 * - DUP 0 > IF
    4 0 DO
      DUP FORWARD 90 RIGHT
    LOOP
  THEN DROP
  5 RIGHT
LOOP

\ === Scene 4: Flower petals ===
." Drawing flower..." CR
PENUP 0 -100 SETXY PENDOWN
12 0 DO
  255 I 20 * 255 I 20 * - 100 SETPENCOLOR
  50 FORWARD
  30 RIGHT
  20 FORWARD
  150 RIGHT
  20 FORWARD
  30 RIGHT
  50 FORWARD
  180 RIGHT
LOOP

PENUP HOME HIDETURTLE

." " CR
." Turtle art complete!" CR
." Shapes drawn: triangle, square, pentagon," CR
."   hexagon, spiral, nested squares, flower" CR
