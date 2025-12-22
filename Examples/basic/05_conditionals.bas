10 REM ================================================
20 REM CONDITIONAL STATEMENTS (IF/THEN/ELSE)
30 REM ================================================
40 PRINT "===== SIMPLE IF STATEMENT ====="
50 LET X = 50
60 IF X > 0 THEN PRINT "X is positive"
70 IF X < 0 THEN PRINT "X is negative"
80 IF X = 0 THEN PRINT "X is zero"
90 
100 PRINT ""
110 PRINT "===== IF/ELSE STATEMENTS ====="
120 LET AGE = 25
130 IF AGE >= 18 THEN
140     PRINT AGE; " years old: You are an adult"
150 ELSE
160     PRINT AGE; " years old: You are a minor"
170 END IF
180 
190 PRINT ""
200 PRINT "===== MULTIPLE CONDITIONS ====="
210 LET GRADE = 85
220 IF GRADE >= 90 THEN
230     PRINT "Grade A (Excellent)"
240 ELSE
250     IF GRADE >= 80 THEN
260         PRINT "Grade B (Good)"
270     ELSE
280         IF GRADE >= 70 THEN
290             PRINT "Grade C (Fair)"
300         ELSE
310             PRINT "Grade F (Needs Improvement)"
320         END IF
330     END IF
340 END IF
350 
360 PRINT ""
370 PRINT "===== LOGICAL OPERATORS ====="
380 LET X = 50
390 LET Y = 30
400 
410 IF X > 40 AND Y > 20 THEN PRINT "Both conditions true"
420 IF X > 40 OR Y < 10 THEN PRINT "At least one condition true"
430 IF NOT (X < 40) THEN PRINT "X is NOT less than 40"
440 
450 PRINT ""
460 PRINT "===== COMPARISON EXAMPLES ====="
470 FOR I = 1 TO 10
480     IF I MOD 2 = 0 THEN
490         PRINT STR$(I) + " is even"
500     ELSE
510         PRINT STR$(I) + " is odd"
520     END IF
530 NEXT I
540 
550 END
