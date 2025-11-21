10 PRINT "I'm thinking of a number between 1 and 100"
20 LET SECRET = int(rand()*100)+1
30 INPUT GUESS
40 IF GUESS = SECRET THEN 100
50 IF GUESS < SECRET THEN 70
60 PRINT "Too high"
65 GOTO 30
70 PRINT "Too low"
75 GOTO 30
100 PRINT "Correct! The number was " , SECRET
110 END
