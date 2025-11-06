10 REM INKEY$ Demo - Press keys, ESC to quit
20 PRINT "Press any key (ESC to exit)..."
30 LET K$ = INKEY$
40 IF K$ = "" THEN GOTO 30
50 PRINT "You pressed: "; K$
60 IF K$ = "Escape" THEN GOTO 100
70 GOTO 30
100 PRINT "Goodbye!"
