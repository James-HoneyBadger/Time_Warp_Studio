( Loops in Forth )

: COUNTDOWN ( n -- )
  ." Counting down: " CR
  0 DO
    I . CR
  LOOP
;

." Running loop 5 times: " CR
5 COUNTDOWN
