10 REM ================================================
20 REM ARRAY OPERATIONS
30 REM ================================================
40 DIM NUMBERS(10)
50 DIM NAMES$(5)
60 DIM MATRIX(5, 5)
70 
80 PRINT "===== FILLING AND READING ARRAYS ====="
90 FOR I = 1 TO 10
100     NUMBERS(I) = I * I
110 NEXT I
120 
130 FOR I = 1 TO 10
140     PRINT "NUMBERS(" + STR$(I) + ") = " + STR$(NUMBERS(I))
150 NEXT I
160 
170 PRINT ""
180 PRINT "===== STRING ARRAYS ====="
190 NAMES$(1) = "Alice"
200 NAMES$(2) = "Bob"
210 NAMES$(3) = "Charlie"
220 NAMES$(4) = "Diana"
230 NAMES$(5) = "Eve"
240 
250 FOR I = 1 TO 5
260     PRINT STR$(I) + ": " + NAMES$(I)
270 NEXT I
280 
290 PRINT ""
300 PRINT "===== 2D ARRAYS (MATRIX) ====="
310 FOR I = 1 TO 5
320     FOR J = 1 TO 5
330         MATRIX(I, J) = I * J
331     NEXT J
332 NEXT I
340 
350 PRINT "Multiplication Table:"
360 FOR I = 1 TO 5
370     FOR J = 1 TO 5
380         PRINT STR$(MATRIX(I, J)) + " ";
390     NEXT J
400     PRINT ""
410 NEXT I
420 
430 PRINT ""
440 PRINT "===== ARRAY OPERATIONS ====="
450 LET SUM = 0
460 LET MAX = NUMBERS(1)
470 
480 FOR I = 1 TO 10
490     SUM = SUM + NUMBERS(I)
500     IF NUMBERS(I) > MAX THEN MAX = NUMBERS(I)
510 NEXT I
520 
530 PRINT "Sum of squares (1 to 10): " + STR$(SUM)
540 PRINT "Maximum value: " + STR$(MAX)
550 
560 END
