\ turtle.fs -- Forth with a turtle.
\
\ The machine's turtle is a set of words: FD, BK, RT, LT, PU, PD, HOME, CS, SETPC,
\ SETPW. They are postfix like everything else, so a square reads `DUP FD RT 90` --
\ the distance, then the word that uses it.

: SQUARE ( size -- )
  4 0 DO DUP FD RT 90 LOOP DROP ;

: PENTAGON ( size -- )
  5 0 DO DUP FD 72 RT LOOP DROP ;

: STAR ( size -- )
  5 0 DO DUP FD 144 RT LOOP DROP ;

CS
1 SETPC
." Nested squares: " CR
40 SQUARE
60 SQUARE
80 SQUARE
100 SQUARE

PU HOME 3 SETPC PD
." A pentagon: " CR
70 PENTAGON

PU HOME 2 SETPC PD
." And a star: " CR
110 STAR

PU HOME
." Done." CR
