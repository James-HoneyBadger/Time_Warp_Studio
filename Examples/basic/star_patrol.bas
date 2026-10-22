10 REM STAR PATROL -- a small game for the fixed-coordinate screen.
20 REM
30 REM What it shows: SCREEN 13 is a 320x200 screen whose pixels a program can
40 REM address and read back. GET and PUT make the ship a sprite, LINE draws the
50 REM shot and the score bar, and POINT reads the pixel the shot has reached,
60 REM which is how the game knows it hit a star.
70 REM
80 REM A/D or the arrow keys steer, SPACE fires, ESC quits. With no keyboard --
90 REM which is how the test suite runs it -- the demo still plays: it runs 240
100 REM frames and then stops, so nothing here waits for a person.
110 SCREEN 13
120 LET FRAMES = 240
130 LET SHIPX = 156
140 LET SHIPY = 180
150 LET SHOTX = 158
160 LET SHOTY = 0
170 LET SCORE = 0
180 REM -- the star field ---------------------------------------------------
190 DIM SX(24)
200 DIM SY(24)
210 FOR I = 1 TO 24
220   LET SX(I) = INT(RND * 316)
230   LET SY(I) = INT(RND * 196)
240 NEXT I
250 REM -- the ship, drawn once in the corner and captured as a sprite -------
260 FOR I = 0 TO 5
270   PSET (I, 0), 15
280   PSET (5 - I, 1), 15
290 NEXT I
300 PSET (2, 2), 15
310 PSET (3, 2), 15
320 PSET (2, 3), 15
330 PSET (3, 3), 15
340 GET (0, 0) - (5, 3), SHIP
350 REM -- and the template off the screen, so it is not part of the game ----
360 CLS
370 REM -----------------------------------------------------------------------
380 FOR FRAME = 1 TO FRAMES
390   FOR I = 1 TO 24
400     LET SY(I) = SY(I) + 1
410     IF SY(I) > 196 THEN LET SY(I) = 0
420   NEXT I
430   LET K$ = INKEY$
440   IF K$ = CHR$(27) THEN LET FRAME = FRAMES
450   IF K$ = "a" THEN LET SHIPX = SHIPX - 4
460   IF K$ = "A" THEN LET SHIPX = SHIPX - 4
470   IF K$ = "d" THEN LET SHIPX = SHIPX + 4
480   IF K$ = "D" THEN LET SHIPX = SHIPX + 4
490   IF SHIPX < 2 THEN LET SHIPX = 2
500   IF SHIPX > 312 THEN LET SHIPX = 312
510   LET SHOTX = SHIPX + 2
520   IF K$ = " " THEN LET SHOTY = SHIPY - 6
530   IF SHOTY > 0 THEN LET SHOTY = SHOTY - 6
540   REM -- redraw the frame -----------------------------------------------
550   CLS
560   FOR I = 1 TO 24
570     PSET (SX(I), SY(I)), 7
580   NEXT I
590   IF SHOTY > 0 THEN LINE (SHOTX, SHOTY) - (SHOTX, SHOTY - 5), 11
600   PUT (SHIPX, SHIPY), SHIP
610   REM -- did the shot reach a star? POINT is the question ----------------
620   LET HIT = 0
630   IF SHOTY > 0 THEN LET HIT = POINT(SHOTX, SHOTY)
640   IF HIT <> 0 THEN LET SCORE = SCORE + 1
650   IF HIT <> 0 THEN LET SHOTY = 0
660   REM -- the score, as a bar that grows in the corner --------------------
670   LINE (2, 196) - (2 + SCORE * 4, 196), 14
680   FOR P = 1 TO 30
690   NEXT P
700 NEXT FRAME
710 PRINT "Score:"; SCORE
720 PRINT "Screen:"; POINT(0, 0); " is the first pixel of the last frame"
730 REM The picture stays on the screen; run it again to play another round.
