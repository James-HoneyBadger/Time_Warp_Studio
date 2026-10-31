10 REM ============================================================
20 REM HB-1: SPRITES OUT OF A FRAMEBUFFER
30 REM A sprite on this machine is a rectangle of the canvas copied
40 REM out with GET and pasted back with PUT. So the machine can
50 REM make one out of nothing: draw it once with PSET, take it, and
60 REM then have as many as the memory allows. Three of them fly
70 REM across the screen and the tube counts the frames.
80 REM ============================================================
90 SCREEN 13
100 CLS
110 REM --- draw a shuttle once, in the corner of the screen -----
120 FOR ROW = 0 TO 7
130   PSET (ROW, ROW), 12
140   PSET (15 - ROW, ROW), 12
150   PSET (ROW, 8 - ROW), 12
160   PSET (15 - ROW, 8 - ROW), 12
170 NEXT ROW
180 FOR ROW = 6 TO 9
190   LINE (3, ROW) - (12, ROW), 12
200 NEXT ROW
210 PSET (7, 4), 15
220 PSET (8, 4), 15
230 REM --- take it, then wipe the template off the screen ------
240 GET (0, 0) - (15, 15), SHUTTLE
250 CLS
260 REM --- three of them, at three heights and three speeds -----
270 LET A = 0
280 LET B = 60
290 LET C = 120
300 FOR FRAME = 1 TO 40
310   PUT (A, 40), SHUTTLE
320   PUT (B, 110), SHUTTLE
330   PUT (C, 180), SHUTTLE
340   FOR WAIT = 1 TO 300
350   NEXT WAIT
360   LET A = (A + 7) MOD 320
370   LET B = (B + 5) MOD 320
380   LET C = (C + 3) MOD 320
390   LOCATE 2, 3
400   COLOR 15, 0
410   PRINT "FRAME "; FRAME; " OF 40";
420 NEXT FRAME
430 REM --- the canvas keeps the last frame, so say what made it -
440 LOCATE 4, 3
450 PRINT "ONE 16x16 SPRITE, TAKEN WITH GET, PUT 120 TIMES.";
460 END
