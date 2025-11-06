10 REM INKEY$ Interactive Game
20 REM Move a player with arrow keys
30 X = 40
40 Y = 12
50 PRINT "Use arrow keys to move @ (Q to quit)"
60 PRINT "Starting position: ("; X; ","; Y; ")"
70 REM Main game loop
80 LET K$ = INKEY$
90 IF K$ = "" THEN GOTO 80
100 REM Process arrow keys
110 IF K$ = "ArrowUp" THEN Y = Y - 1
120 IF K$ = "ArrowDown" THEN Y = Y + 1
130 IF K$ = "ArrowLeft" THEN X = X - 1
140 IF K$ = "ArrowRight" THEN X = X + 1
150 IF K$ = "q" THEN GOTO 200
160 IF K$ = "Q" THEN GOTO 200
170 PRINT "Position: ("; X; ","; Y; ") Key: "; K$
180 GOTO 80
200 PRINT "Final position: ("; X; ","; Y; ")"
210 PRINT "Game over!"
