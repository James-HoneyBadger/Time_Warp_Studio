10 REM =============================================
20 REM  BASIC Comprehensive Demo - Time Warp Studio
30 REM =============================================
40 PRINT "===== HELLO WORLD ====="
50 PRINT "Welcome to BASIC!"
60 PRINT ""
70 REM --- Variables and Arithmetic ---
80 PRINT "===== VARIABLES & MATH ====="
90 LET X = 10
100 LET Y = 3
110 PRINT "X ="; X
120 PRINT "Y ="; Y
130 PRINT "X + Y ="; X + Y
140 PRINT "X * Y ="; X * Y
150 PRINT "X / Y ="; X / Y
160 PRINT "X MOD Y ="; X MOD Y
170 PRINT ""
180 REM --- String Variables ---
190 PRINT "===== STRINGS ====="
200 A$ = "Hello"
210 B$ = "World"
220 PRINT A$
230 PRINT B$
240 PRINT LEN("BASIC")
250 PRINT LEFT("Hello", 3)
260 PRINT RIGHT("Hello", 3)
270 PRINT MID("Hello", 2, 3)
280 PRINT ""
290 REM --- FOR Loop ---
300 PRINT "===== FOR LOOP ====="
310 FOR I = 1 TO 5
320     PRINT I
330 NEXT I
340 PRINT ""
350 REM --- FOR with STEP ---
360 PRINT "===== STEP LOOP ====="
370 FOR I = 2 TO 10 STEP 2
380     PRINT I
390 NEXT I
400 PRINT ""
410 REM --- WHILE loop ---
420 PRINT "===== WHILE LOOP ====="
430 X = 1
440 WHILE X <= 5
450     PRINT X
460     X = X + 1
470 WEND
480 PRINT ""
490 REM --- IF/THEN ---
500 PRINT "===== CONDITIONALS ====="
510 X = 7
520 IF X > 5 THEN PRINT "X is greater than 5"
530 IF X < 3 THEN PRINT "X is less than 3"
540 X = 10
550 IF X = 10 THEN PRINT "X equals 10"
560 PRINT ""
570 REM --- GOSUB/RETURN ---
580 PRINT "===== SUBROUTINES ====="
590 GOSUB 900
600 PRINT "Back from subroutine"
610 PRINT ""
620 REM --- DATA/READ ---
630 PRINT "===== DATA AND READ ====="
640 DATA 10, 20, 30
650 READ A
660 READ B
670 READ C
680 PRINT A
690 PRINT B
700 PRINT C
710 PRINT ""
720 REM --- Arrays ---
730 PRINT "===== ARRAYS ====="
740 DIM ARR(5)
750 ARR(0) = 100
760 ARR(1) = 200
770 ARR(2) = 300
780 PRINT ARR(0)
790 PRINT ARR(1)
800 PRINT ARR(2)
810 PRINT ""
820 REM --- Multi-statement Lines ---
830 PRINT "===== MULTI-STATEMENT ====="
840 A = 1 : B = 2 : C = A + B
850 PRINT C
860 PRINT ""
870 PRINT "===== DONE ====="
880 END
890 REM --- Subroutine ---
900 PRINT "Inside subroutine"
910 RETURN
