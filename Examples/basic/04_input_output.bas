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
170 PRINT "Enter your name (or press Enter for 'Friend'):"
180 INPUT NAME$
190 IF NAME$ = "" THEN NAME$ = "Friend"
200 PRINT "Hello, " + NAME$ + "!"
210 
220 PRINT ""
230 PRINT "Enter a number (1-10):"
240 INPUT NUM
250 
260 PRINT ""
270 PRINT "===== SIMPLE CALCULATIONS ====="
280 PRINT "Number: " + STR$(NUM)
290 PRINT "Squared: " + STR$(NUM * NUM)
300 PRINT "Cubed: " + STR$(NUM * NUM * NUM)
310 
320 PRINT ""
330 PRINT "===== FORMATTED TABLE ====="
340 PRINT "NUM  | SQUARE | CUBE"
350 PRINT "-----+--------+------"
360 FOR I = 1 TO 5
370     LET SQ = I * I
380     LET CU = I * I * I
390     PRINT STR$(I) + "    | " + STR$(SQ) + "    | " + STR$(CU)
400 NEXT I
410 
420 PRINT ""
430 PRINT "End of program"
440 END
