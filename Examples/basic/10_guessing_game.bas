10 REM ================================================
20 REM GAMES AND INTERACTIVE PROGRAMS
30 REM ================================================
40 REM Higher or Lower Number Guessing Game
50 RANDOMIZE TIMER
60 LET SECRET = INT(RND * 100) + 1
70 LET TRIES = 0
80 
90 PRINT "===== GUESS THE NUMBER GAME ====="
100 PRINT "I'm thinking of a number between 1 and 100"
110 PRINT "Can you guess it?"
120 PRINT ""
130 
140 LET GUESSED = 0
150 WHILE NOT GUESSED
160     PRINT "Enter your guess: "
170     INPUT GUESS
171     TRIES = TRIES + 1
180     
190     IF GUESS = SECRET THEN
200         PRINT ""
210         PRINT "WOW! You got it right!"
220         PRINT "It took you " + STR$(TRIES) + " tries"
230         GUESSED = 1
240     ELSE
250         IF GUESS < SECRET THEN
260             PRINT "Too LOW! Try higher..."
270         ELSE
280             PRINT "Too HIGH! Try lower..."
290         END IF
300         
310         IF TRIES >= 5 AND TRIES < 10 THEN
320             PRINT "You're getting close!"
330         ELSE
340             IF TRIES >= 10 THEN
350                 PRINT "Keep trying..."
360             END IF
370         END IF
380     END IF
390     PRINT ""
400 WEND
410 
420 PRINT ""
430 PRINT "===== RATING YOUR PERFORMANCE ====="
440 IF TRIES <= 3 THEN
450     PRINT "Outstanding! You're a guessing genius!"
460 ELSE
470     IF TRIES <= 6 THEN
480         PRINT "Great job! You're pretty good at this!"
490     ELSE
500         IF TRIES <= 10 THEN
510             PRINT "Not bad! You got there eventually."
520         ELSE
530             PRINT "Keep practicing!"
540         END IF
550     END IF
560 END IF
570 
580 END
