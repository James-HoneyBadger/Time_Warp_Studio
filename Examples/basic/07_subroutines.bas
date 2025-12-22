10 REM ================================================
20 REM SUBROUTINES AND FUNCTIONS
30 REM ================================================
40 GOSUB 1000
50 PRINT ""
60 GOSUB 2000
70 PRINT ""
80 GOSUB 3000
90 END
100 
200 REM ================================================
210 REM SUBROUTINE: PRINT SEPARATOR
220 REM ================================================
1000 PRINT "===== SIMPLE SUBROUTINE ====="
1010 PRINT "This is a reusable subroutine"
1020 PRINT "It can be called multiple times"
1030 RETURN
1040 
1100 REM ================================================
1110 REM SUBROUTINE: DISPLAY NUMBERS 1-5
1120 REM ================================================
2000 PRINT "===== SUBROUTINE WITH LOOP ====="
2010 FOR I = 1 TO 5
2020     GOSUB 3500
2030 NEXT I
2040 RETURN
2050 
2100 REM ================================================
2110 REM SUBROUTINE: CALCULATIONS
2120 REM ================================================
3000 PRINT "===== SUBROUTINE WITH CALCULATIONS ====="
3010 LET NUM = 12
3020 GOSUB 3100
3030 LET NUM = 25
3040 GOSUB 3100
3050 LET NUM = 144
3060 GOSUB 3100
3070 RETURN
3080 
3100 REM Subroutine to print info about a number
3110 PRINT "Number: " + STR$(NUM)
3120 PRINT "  Square: " + STR$(NUM * NUM)
3130 PRINT "  Square Root: " + STR$(INT(SQR(NUM) * 100) / 100)
3140 PRINT "  Is Divisible by 3: ";
3150 IF NUM MOD 3 = 0 THEN PRINT "Yes" ELSE PRINT "No"
3160 RETURN
3170 
3500 REM Simple loop subroutine
3510 PRINT "  Item " + STR$(I)
3520 RETURN
