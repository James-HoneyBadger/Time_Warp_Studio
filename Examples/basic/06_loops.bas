10 REM ================================================
20 REM LOOPS (FOR, WHILE, DO)
30 REM ================================================
40 PRINT "===== FOR LOOP ====="
50 PRINT "Counting 1 to 10:"
60 FOR I = 1 TO 10
70     PRINT STR$(I) + " ";
80 NEXT I
90 PRINT ""
100 
110 PRINT "===== FOR LOOP WITH STEP ====="
120 PRINT "Even numbers 0 to 20:"
130 FOR I = 0 TO 20 STEP 2
140     PRINT STR$(I) + " ";
150 NEXT I
160 PRINT ""
170 
180 PRINT "===== NESTED FOR LOOPS (MULTIPLICATION TABLE) ====="
190 PRINT "     | 1  2  3  4  5"
200 PRINT "-----+--+---------"
210 FOR I = 1 TO 5
220     PRINT STR$(I) + "    | ";
230     FOR J = 1 TO 5
240         PRINT STR$(I * J) + " ";
250     NEXT J
260     PRINT ""
270 NEXT I
280 
290 PRINT ""
300 PRINT "===== WHILE LOOP ====="
310 LET COUNT = 1
320 WHILE COUNT <= 5
330     PRINT "Count: " + STR$(COUNT)
340     COUNT = COUNT + 1
350 WEND
360 
370 PRINT ""
380 PRINT "===== LOOP WITH EXIT ====="
390 FOR I = 1 TO 100
400     IF I = 11 THEN EXIT FOR
410     PRINT STR$(I) + " ";
410 NEXT I
420 PRINT ""
430 PRINT "(Exited at 11)"
440 
450 PRINT ""
460 PRINT "===== FACTORIAL CALCULATION ====="
470 FOR N = 1 TO 6
480     LET FACT = 1
490     FOR I = 1 TO N
500         FACT = FACT * I
510     NEXT I
520     PRINT STR$(N) + "! = " + STR$(FACT)
530 NEXT N
540 
550 END
