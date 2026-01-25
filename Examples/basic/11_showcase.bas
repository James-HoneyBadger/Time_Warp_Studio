10 REM ================================================
20 REM COMPREHENSIVE SHOWCASE PROGRAM
30 REM All major BASIC features combined
40 REM ================================================
50 PRINT "╔════════════════════════════════════════════╗"
60 PRINT "║  TIME WARP STUDIO - BASIC SHOWCASE PROGRAM   ║"
61 PRINT "║        Demonstrating All Features         ║"
62 PRINT "╚════════════════════════════════════════════╝"
63 PRINT ""
64 
70 REM SECTION 1: VARIABLES AND TYPES
80 PRINT "1. VARIABLES AND TYPES"
90 PRINT "─────────────────────"
100 LET INT_VAR = 100
110 LET STR_VAR$ = "Time Warp Studio"
120 LET BOOL_VAR = 1
130 PRINT "Integer: " + STR$(INT_VAR)
140 PRINT "String: " + STR_VAR$
150 PRINT "Boolean: " + STR$(BOOL_VAR)
160 PRINT ""
170 
180 REM SECTION 2: ARITHMETIC
190 PRINT "2. ARITHMETIC OPERATIONS"
200 PRINT "───────────────────────"
210 LET A = 50
220 LET B = 12
230 PRINT "50 + 12 = " + STR$(A + B)
240 PRINT "50 - 12 = " + STR$(A - B)
250 PRINT "50 * 12 = " + STR$(A * B)
260 PRINT "50 / 12 = " + STR$(INT(A / B))
270 PRINT "50 MOD 12 = " + STR$(A MOD B)
280 PRINT ""
290 
300 REM SECTION 3: STRINGS
310 PRINT "3. STRING OPERATIONS"
320 PRINT "────────────────────"
330 LET S1$ = "Hello"
340 LET S2$ = "World"
350 LET S3$ = S1$ + " " + S2$
360 PRINT "Concatenation: " + S3$
370 PRINT "Length: " + STR$(LEN(S3$))
380 PRINT "Substring: " + LEFT$(S3$, 5)
390 PRINT ""
400 
410 REM SECTION 4: ARRAYS
420 PRINT "4. ARRAYS"
430 PRINT "─────────"
440 DIM DATA(5)
450 FOR I = 1 TO 5
460     DATA(I) = I * 20
470 NEXT I
480 
490 LET TOTAL = 0
500 FOR I = 1 TO 5
510     PRINT "Data(" + STR$(I) + ") = " + STR$(DATA(I))
520     TOTAL = TOTAL + DATA(I)
530 NEXT I
540 PRINT "Total: " + STR$(TOTAL)
550 PRINT ""
560 
570 REM SECTION 5: CONDITIONALS
580 PRINT "5. CONDITIONAL LOGIC"
590 PRINT "────────────────────"
600 FOR N = 1 TO 7
610     IF N MOD 2 = 0 THEN
620         PRINT STR$(N) + " is EVEN"
630     ELSE
640         PRINT STR$(N) + " is ODD"
650     END IF
660 NEXT N
670 PRINT ""
680 
690 REM SECTION 6: LOOPS
700 PRINT "6. LOOPS"
710 PRINT "────────"
720 PRINT "For loop (1 to 5):"
730 FOR I = 1 TO 5
740     PRINT "  " + STR$(I);
750 NEXT I
760 PRINT ""
770 PRINT ""
780 
790 REM SECTION 7: MATHEMATICAL FUNCTIONS
800 PRINT "7. MATHEMATICAL FUNCTIONS"
810 PRINT "────────────────────────"
820 PRINT "Square Root of 16: " + STR$(SQR(16))
830 PRINT "Absolute of -25: " + STR$(ABS(-25))
840 PRINT "Integer of 3.7: " + STR$(INT(3.7))
850 PRINT ""
860 
870 REM SECTION 8: SUBROUTINES
880 PRINT "8. SUBROUTINES"
890 PRINT "──────────────"
900 GOSUB 1500
910 PRINT ""
920 
930 REM SECTION 9: SUMMARY
940 PRINT "9. PROGRAM COMPLETE"
950 PRINT "───────────────────"
960 PRINT "All BASIC features demonstrated successfully!"
970 PRINT "Happy coding with Time Warp Studio!"
980 
990 END
1000 
1500 REM Simple subroutine
1510 FOR I = 1 TO 3
1520     PRINT "Subroutine call #" + STR$(I)
1530 NEXT I
1540 RETURN
