( Graphics in Forth )
( Time Warp Extensions: FD BK RT LT PU PD HOME CLEAN PEN )

: SQUARE ( size -- )
  4 0 DO
    DUP FD
    90 RT
  LOOP
  DROP
;

: PATTERN
  CLEAN
  HOME
  36 0 DO
    100 SQUARE
    10 RT
  LOOP
;

." Drawing pattern... " CR
PATTERN
