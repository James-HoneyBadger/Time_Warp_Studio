10 REM ================================================
20 REM INPUT AND OUTPUT OPERATIONS
30 REM ================================================
40 PRINT "===== BASIC OUTPUT ====="
50 PRINT "This is a simple print statement"
60 PRINT "Multiple values:"; 10; 20; 30
70 PRINT "String concatenation: " + "Hello" + " " + "World"
80 
90 PRINT ""
100 PRINT "===== FORMATTED OUTPUT ====="
110 FOR I = 1 TO 5
120     PRINT "Line " + STR$(I) + ": Value = " + STR$(I * 100)
130 NEXT I
140 
150 PRINT ""
160 PRINT "===== INPUT DEMONSTRATION ====="
170 PRINT "Enter your name (default: Programmer):"
180 INPUT NAME$
190 IF NAME$ = "" THEN NAME$ = "Programmer"
200 
210 PRINT "Hello, " + NAME$ + "!"
220 
230 PRINT "Enter a number (default: 10):"
240 INPUT NUM
250 IF NUM = 0 THEN NUM = 10
260 
270 PRINT "You entered: " + STR$(NUM)
280 PRINT "Doubled: " + STR$(NUM * 2)
290 
300 PRINT ""
310 PRINT "===== FORMATTED TABLE ====="
320 PRINT "NUM  | SQUARE | CUBE  | SQRT"
330 PRINT "-----+--------+-------+-----"
340 FOR I = 1 TO 10
350     LET SQ = I * I
360     LET CU = I * I * I
370     LET RT = INT(SQR(I) * 10) / 10
371     PRINT STR$(I) + "    | " + STR$(SQ) + "    | " + STR$(CU) + "    | " + STR$(RT)
380 NEXT I
390 
400 END
