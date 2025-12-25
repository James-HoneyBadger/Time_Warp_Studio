( LOOPS AND ITERATION IN FORTH )

." === Basic Counting Loop ===" CR

." Counting 1 to 10: "
1 10 DO
    I . ." " 
LOOP
CR

." === Reverse Counting Loop ===" CR

." Countdown 10 to 1: "
10 0 DO
    10 I - . ." " 
LOOP
CR CR

." === Multiplication Table ===" CR

." 5 times table: " CR
1 11 DO
    I . ." x 5 = " I 5 * . CR
LOOP

." === Simple Counter ===" CR

: COUNTDOWN ( n -- )
  ." Counting down from: " DUP . CR
  0 DO
    I . CR
  LOOP
;

5 COUNTDOWN
