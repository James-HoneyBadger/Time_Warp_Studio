10 REM ================================================
20 REM ARITHMETIC OPERATIONS
30 REM ================================================
40 PRINT "===== BASIC ARITHMETIC ====="
50 LET A = 100
60 LET B = 25
70 
80 PRINT "A = "; A
90 PRINT "B = "; B
100 PRINT "A + B = "; A + B
110 PRINT "A - B = "; A - B
120 PRINT "A * B = "; A * B
130 PRINT "A / B = "; A / B
140 PRINT "A MOD B = "; A MOD B
150 
160 PRINT ""
170 PRINT "===== OPERATOR PRECEDENCE ====="
180 PRINT "2 + 3 * 4 = "; 2 + 3 * 4
190 PRINT "(2 + 3) * 4 = "; (2 + 3) * 4
200 PRINT "10 - 5 - 2 = "; 10 - 5 - 2
210 PRINT "100 / 10 / 2 = "; 100 / 10 / 2
220 
230 PRINT ""
240 PRINT "===== MATHEMATICAL FUNCTIONS ====="
250 PRINT "ABS(-42) = "; ABS(-42)
260 PRINT "INT(3.7) = "; INT(3.7)
270 PRINT "SQR(16) = "; SQR(16)
280 PRINT "SIN(0) = "; SIN(0)
290 PRINT "COS(0) = "; COS(0)
300 
310 PRINT ""
320 PRINT "===== COMPLEX CALCULATIONS ====="
310 LET RADIUS = 5
320 LET AREA = 314 * RADIUS * RADIUS / 100
330 PRINT "Circle area (r=5): "; AREA
340 
350 PRINT ""
360 PRINT "===== ACCUMULATION ====="
370 LET SUM = 0
380 FOR I = 1 TO 100
390     SUM = SUM + I
400 NEXT I
410 PRINT "Sum 1 to 100: "; SUM
420 
430 END
