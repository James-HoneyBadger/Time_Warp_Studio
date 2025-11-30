10 REM Subroutine Demonstration
20 PRINT "Main Program Starting"
30 GOSUB 100
40 PRINT "Back in Main Program"
50 GOSUB 100
60 PRINT "Done"
70 END
100 REM Subroutine
110 PRINT "  Inside Subroutine"
120 RETURN
