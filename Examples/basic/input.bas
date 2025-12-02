10 REM ========================================
20 REM  Input Demo - Time Warp BASIC
30 REM  Demonstrates: INPUT, string variables
40 REM ========================================
50 PRINT "What is your name";  
60 INPUT NAME$
70 PRINT "Hello, "; NAME$
80 PRINT
90 PRINT "How old are you";  
100 INPUT AGE
110 LET NEXTAGE = AGE + 1
120 PRINT "Next year you'll be "; NEXTAGE
