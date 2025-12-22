( Time Warp Forth Showcase )

: STAR ( size -- )
  5 0 DO
    DUP FD
    144 RT
  LOOP
  DROP
;

: SPIRAL ( size -- )
  100 0 DO
    DUP FD
    90 RT
    5 +
  LOOP
  DROP
;

: MAIN
  CLEAN
  HOME
  ." Drawing Star... " CR
  1 PEN
  150 STAR
  
  ." Drawing Spiral... " CR
  2 PEN
  10 SPIRAL
  
  ." Done! " CR
;

MAIN
