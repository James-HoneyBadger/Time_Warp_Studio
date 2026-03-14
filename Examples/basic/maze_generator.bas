10 REM ═══════════════════════════════════════════
20 REM   Maze Generator — Recursive Back-tracker
30 REM   Demonstrates arrays, subroutines, RND
40 REM ═══════════════════════════════════════════
50 LET W = 12
60 LET H = 8
70 DIM VISITED(100)
80 DIM WALLH(200)
90 DIM WALLV(200)
100 REM Initialize walls (1 = wall present)
110 FOR I = 0 TO W * H - 1
120   LET VISITED(I) = 0
130 NEXT I
140 FOR I = 0 TO (W + 1) * H - 1
150   LET WALLV(I) = 1
160 NEXT I
170 FOR I = 0 TO W * (H + 1) - 1
180   LET WALLH(I) = 1
190 NEXT I
200 REM Stack for backtracking
210 DIM STACK(200)
220 LET SP = 0
230 REM Start at cell (0, 0)
240 LET CX = 0
250 LET CY = 0
260 LET VISITED(CY * W + CX) = 1
270 LET REMAIN = W * H - 1
280 REM === Main generation loop ===
290 IF REMAIN = 0 THEN GOTO 500
300 REM Find unvisited neighbours
310 LET NC = 0
320 DIM NB(4)
330 IF CY > 0 THEN IF VISITED((CY - 1) * W + CX) = 0 THEN LET NB(NC) = 0 : LET NC = NC + 1
340 IF CX < W - 1 THEN IF VISITED(CY * W + CX + 1) = 0 THEN LET NB(NC) = 1 : LET NC = NC + 1
350 IF CY < H - 1 THEN IF VISITED((CY + 1) * W + CX) = 0 THEN LET NB(NC) = 2 : LET NC = NC + 1
360 IF CX > 0 THEN IF VISITED(CY * W + CX - 1) = 0 THEN LET NB(NC) = 3 : LET NC = NC + 1
370 IF NC = 0 THEN GOTO 450
380 REM Choose random neighbour
390 LET DIR = NB(INT(RND * NC))
400 LET STACK(SP) = CY * W + CX
410 LET SP = SP + 1
420 IF DIR = 0 THEN LET WALLH(CY * W + CX) = 0 : LET CY = CY - 1
430 IF DIR = 1 THEN LET WALLV(CY * (W + 1) + CX + 1) = 0 : LET CX = CX + 1
440 IF DIR = 2 THEN LET WALLH((CY + 1) * W + CX) = 0 : LET CY = CY + 1
441 IF DIR = 3 THEN LET WALLV(CY * (W + 1) + CX) = 0 : LET CX = CX - 1
442 LET VISITED(CY * W + CX) = 1
443 LET REMAIN = REMAIN - 1
444 GOTO 290
450 REM Backtrack
460 IF SP = 0 THEN GOTO 500
470 LET SP = SP - 1
480 LET CX = STACK(SP) - INT(STACK(SP) / W) * W
490 LET CY = INT(STACK(SP) / W)
495 GOTO 290
500 REM === Draw the maze ===
510 PRINT "Maze "; W; " x "; H
520 PRINT
530 FOR Y = 0 TO H - 1
540   REM Top wall row
550   LET L$ = ""
560   FOR X = 0 TO W - 1
570     LET L$ = L$ + "+"
580     IF WALLH(Y * W + X) = 1 THEN LET L$ = L$ + "---"
590     IF WALLH(Y * W + X) = 0 THEN LET L$ = L$ + "   "
600   NEXT X
610   PRINT L$; "+"
620   REM Side wall row
630   LET L$ = ""
640   FOR X = 0 TO W
650     IF WALLV(Y * (W + 1) + X) = 1 THEN LET L$ = L$ + "|"
660     IF WALLV(Y * (W + 1) + X) = 0 THEN LET L$ = L$ + " "
670     IF X < W THEN LET L$ = L$ + "   "
680   NEXT X
690   PRINT L$
700 NEXT Y
710 REM Bottom wall
720 LET L$ = ""
730 FOR X = 0 TO W - 1
740   LET L$ = L$ + "+---"
750 NEXT X
760 PRINT L$; "+"
770 PRINT
780 PRINT "Maze generation complete!"
