10 REM ========================================
20 REM  Guessing Game - Time Warp BASIC
30 REM  Demonstrates: RND, INPUT, IF/THEN, GOTO
40 REM ========================================
50 LET TARGET = INT(RND(1) * 10) + 1
60 PRINT "I'm thinking of a number from 1 to 10."
70 PRINT
80 PRINT "Your guess";
90 INPUT GUESS
100 IF GUESS = TARGET THEN GOTO 130
110 PRINT "Nope! Try again."
120 GOTO 80
130 PRINT "Correct! You win!"
