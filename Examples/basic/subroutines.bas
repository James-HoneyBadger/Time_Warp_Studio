10 REM ========================================
20 REM  Subroutines - Time Warp BASIC
30 REM  Demonstrates: GOSUB, RETURN, END
40 REM ========================================
50 PRINT "Main program start"
60 GOSUB 200
70 PRINT "Back in main"
80 GOSUB 200
90 PRINT "Main program end"
100 END
200 REM --- Subroutine ---
210 PRINT "  >> Inside subroutine"
220 RETURN
