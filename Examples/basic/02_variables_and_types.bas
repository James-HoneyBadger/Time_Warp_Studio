10 REM ================================================
20 REM VARIABLES AND DATA TYPES
30 REM ================================================
40 REM Integer variables
50 LET X = 42
60 LET Y = -100
70 LET Z = 0
80 
90 REM String variables
100 LET NAME$ = "Alice"
110 LET CITY$ = "New York"
120 LET GREETING$ = "Hello"
130 
140 REM Floating point (stored as integers with scaling)
150 LET PI = 31416
160 LET E = 27183
170 
180 PRINT "===== INTEGERS ====="
190 PRINT "X = "; X
200 PRINT "Y = "; Y
210 PRINT "Z = "; Z
220 PRINT "SUM = "; X + Y + Z
230 
240 PRINT "===== STRINGS ====="
250 PRINT NAME$; " lives in "; CITY$
260 PRINT GREETING$; " "; NAME$; "!"
270 
280 PRINT "===== ARRAYS ====="
290 REM Declare arrays
300 DIM SCORES(10)
310 DIM NAMES$(10)
320 
330 REM Fill array
340 FOR I = 1 TO 5
350     SCORES(I) = I * 10
360     NAMES$(I) = "PLAYER" + STR$(I)
370 NEXT I
380 
390 REM Print array contents
400 FOR I = 1 TO 5
410     PRINT "Player " + STR$(I) + ": " + NAMES$(I) + " Score: " + STR$(SCORES(I))
420 NEXT I
430 
440 END
