10 REM ================================================
20 REM STRING OPERATIONS
30 REM ================================================
40 PRINT "===== STRING CONCATENATION ====="
50 LET FIRST$ = "John"
60 LET LAST$ = "Smith"
70 LET FULL$ = FIRST$ + " " + LAST$
80 PRINT "Full Name: " + FULL$
90 
100 PRINT ""
110 PRINT "===== STRING FUNCTIONS ====="
120 LET TEXT$ = "Hello World"
130 PRINT "Original: " + TEXT$
140 PRINT "Length: " + STR$(LEN(TEXT$))
150 PRINT "First 5 chars: " + LEFT$(TEXT$, 5)
160 PRINT "Last 5 chars: " + RIGHT$(TEXT$, 5)
170 PRINT "Middle (6-11): " + MID$(TEXT$, 6, 5)
180 
190 PRINT ""
200 PRINT "===== STRING SEARCH ====="
210 LET SENTENCE$ = "The quick brown fox"
220 LET POS = INSTR(1, SENTENCE$, "brown")
230 IF POS > 0 THEN
240     PRINT "Found 'brown' at position " + STR$(POS)
250 END IF
260 
270 PRINT ""
280 PRINT "===== CASE CONVERSION ====="
290 LET LOWER$ = "hello world"
300 LET UPPER$ = "HELLO WORLD"
310 PRINT "Lowercase: " + LOWER$
320 PRINT "Uppercase: " + UPPER$
330 
340 PRINT ""
350 PRINT "===== STRING TO NUMBER AND BACK ====="
360 LET NUM = 42
370 LET STR_NUM$ = STR$(NUM)
380 PRINT "Number as string: " + STR_NUM$
390 PRINT "String length: " + STR$(LEN(STR_NUM$))
400 
410 PRINT ""
420 PRINT "===== BUILD FORMATTED STRING ====="
430 FOR I = 1 TO 5
440     LET LINE$ = "Item #" + STR$(I) + ": " + STR$(I * 100) + " units"
450     PRINT LINE$
460 NEXT I
470 
480 END
