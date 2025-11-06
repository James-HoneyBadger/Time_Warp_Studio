10 PRINT "Multiplication Table Generator"
20 PRINT "Enter a number (1-12):"
30 INPUT N
40 IF N < 1 THEN 90
50 IF N > 12 THEN 90
60 PRINT "Multiplication table for", N
70 FOR I = 1 TO 10
80 PRINT N, "x", I, "=", N*I
85 NEXT I
88 GOTO 100
90 PRINT "Please enter a number between 1 and 12"
95 GOTO 20
100 END
