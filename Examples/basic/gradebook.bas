10 REM ============================================================
20 REM THE GRADEBOOK
30 REM A BASIC program that does a job: read a class's marks, rank
40 REM them, print a report a teacher can read, and draw it. Two
50 REM parallel arrays, GOSUB subroutines that leave their answer
60 REM in a named variable, PRINT USING for the columns, and LINE
70 REM BF for the chart.
80 REM ============================================================
90 CONST PUPILS = 6
100 CONST PASS = 50
110 DIM NAMES$(6)
120 DIM MARKS(6)
130 DIM SORTED(6)

140 PRINT "T H E   G R A D E B O O K"
150 PRINT "=============================================="

160 REM --- 1. The class, read from DATA ----------------------------
170 DATA "Ada Lovelace", 94
180 DATA "Grace Hopper", 88
190 DATA "Alan Turing", 76
200 DATA "Katherine J", 91
210 DATA "Edsger D", 63
220 DATA "Barbara L", 48
230 FOR N = 1 TO PUPILS
240   READ PUPIL$
250   READ SCORE
260   NAMES$(N) = PUPIL$
270   MARKS(N) = SCORE
280 NEXT N

290 REM --- 2. Summary figures -------------------------------------
300 TOTAL = 0
310 FOR N = 1 TO PUPILS
320   MARK = MARKS(N)
330   TOTAL = TOTAL + MARK
340 NEXT N
350 MEAN = TOTAL / PUPILS
360 HIGH = MARKS(1)
370 LOW = MARKS(1)
380 PASSING = 0
390 FOR N = 1 TO PUPILS
400   MARK = MARKS(N)
410   IF MARK > HIGH THEN HIGH = MARK
420   IF MARK < LOW THEN LOW = MARK
430   IF MARK >= PASS THEN PASSING = PASSING + 1
440 NEXT N

450 PRINT
460 PRINT "Marked  : " + STR$(PUPILS) + " papers"
470 PRINT "Mean    : " + FORMAT$("#.##", MEAN)
480 PRINT "Highest : " + STR$(HIGH)
490 PRINT "Lowest  : " + STR$(LOW)
500 PRINT "Passed  : " + STR$(PASSING) + " of " + STR$(PUPILS)

510 REM --- 3. Ranked, by an exchange sort on the marks -------------
520 FOR N = 1 TO PUPILS - 1
530   FOR M = N + 1 TO PUPILS
540     A = MARKS(N)
550     B = MARKS(M)
560     IF B > A THEN GOSUB 886
570   NEXT M
580 NEXT N

590 PRINT
600 PRINT "  POS  MARK  GRADE  NAME"
610 PRINT "  ---  ----  -----  ------------------------------"
620 FOR N = 1 TO PUPILS
630   MARK = MARKS(N)
640   GOSUB 901
650   PRINT "   " + FORMAT$("##", N) + "   " + FORMAT$("###", MARK) + "     " + G$ + "     " + NAMES$(N)
670 NEXT N

680 REM --- 4. What a teacher would ask next ------------------------
690 INBAND = 0
700 FOR N = 1 TO PUPILS
710   MARK = MARKS(N)
720   IF MARK >= 80 AND MARK < 90 THEN INBAND = INBAND + 1
730 NEXT N
740 PRINT
750 PRINT "At or above pass : " + STR$(PASSING)
760 PRINT "Below pass       : " + STR$(PUPILS - PASSING)
770 PRINT "Papers in the 80s: " + STR$(INBAND)
780 IF PASSING < PUPILS THEN
790   W = PUPILS - PASSING
795   PRINT "ACTION: " + STR$(W) + " student(s) need a conversation."
796 ELSE
797   PRINT "ACTION: every paper passed. Set a harder paper next term."
798 END IF

810 REM --- 5. The same class, drawn -------------------------------
820 PRINT
830 PRINT "The class, tallest mark first:"
840 FOR N = 1 TO PUPILS
850   MARK = MARKS(N)
860   LENGTH = MARK * 2
870   LINE (40, N * 18) - (40 + LENGTH, N * 18 + 12), 1, BF
871 NEXT N
880 PRINT "Drawn: one bar per paper, in rank order."
885 END

886 REM --- the swap, as a subroutine -------------------------------
887 REM Two parallel arrays mean two swaps and a temporary, which is
888 REM exactly the kind of thing a GOSUB is for in BASIC.
890 HOLD$ = NAMES$(N)
895 HOLD = MARKS(N)
896 NAMES$(N) = NAMES$(M)
897 MARKS(N) = MARKS(M)
898 NAMES$(M) = HOLD$
899 MARKS(M) = HOLD
900 RETURN

901 REM --- the grade letter, as a subroutine -----------------------
910 IF MARK >= 90 THEN G$ = "A" : RETURN
920 IF MARK >= 80 THEN G$ = "B" : RETURN
930 IF MARK >= 70 THEN G$ = "C" : RETURN
940 IF MARK >= 50 THEN G$ = "D" : RETURN
950 G$ = "F"
960 RETURN

910 REM --- the swap, as a subroutine -------------------------------
920 REM Two parallel arrays mean two swaps and a temporary, which is
930 REM exactly the kind of thing a GOSUB is for in BASIC.
940 HOLD$ = NAMES$(N)
950 HOLD = MARKS(N)
960 NAMES$(N) = NAMES$(M)
970 MARKS(N) = MARKS(M)
980 NAMES$(M) = HOLD$
990 MARKS(M) = HOLD
1000 RETURN

1010 REM --- the grade letter, as a subroutine -----------------------
1020 IF MARK >= 90 THEN G$ = "A" : RETURN
1030 IF MARK >= 80 THEN G$ = "B" : RETURN
1040 IF MARK >= 70 THEN G$ = "C" : RETURN
1050 IF MARK >= 50 THEN G$ = "D" : RETURN
1060 G$ = "F"
1070 RETURN
