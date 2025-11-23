10 REM Guessing Game
20 LET TARGET = INT(RND(1) * 10) + 1
30 PRINT "I am thinking of a number between 1 and 10"
40 PRINT "Guess my number:"
50 INPUT GUESS
60 IF GUESS = TARGET THEN GOTO 90
70 PRINT "Wrong! Try again."
80 GOTO 40
90 PRINT "Correct! You win!"
