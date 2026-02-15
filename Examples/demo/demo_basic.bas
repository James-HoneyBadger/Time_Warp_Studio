10 REM ====================================
20 REM     BASIC LANGUAGE DEMO
30 REM  Showcasing Turbo BASIC Features
40 REM ====================================
50 SCREEN 1
60 COLOR 15, 0
70 PRINT "Time Warp Studio - BASIC Demo"
75 PRINT
80 REM --- GRAPHICS MODE ---
90 PRINT "Drawing graphics..."
100 LINE (50,50)-(750,50),15
110 LINE (50,50)-(50,550),14
120 LINE (750,50)-(750,550),12
130 LINE (50,550)-(750,550),11
140 REM Box outline
150 LINE (80,80)-(200,200),15,B
160 REM Filled box
170 LINE (250,80)-(370,200),2,BF
180 REM Circles
190 CIRCLE (150,350),40,14
200 CIRCLE (300,350),50,5
210 REM Pixels
220 FOR I = 0 TO 10
230   PSET (100+I*5, 450), 15
240   PSET (100+I*5, 460), 14
250 NEXT I
260 REM Variables
270 PRINT
280 PRINT "Variables and Math:"
290 X = 2
300 Y = 3
310 Z = X + Y
320 PRINT "X="; X; " Y="; Y; " Z="; Z
330 REM Loops
340 PRINT
345 PRINT "Sum of 1-5:"
350 S = 0
360 FOR I = 1 TO 5
370   S = S + I
380 NEXT I
390 PRINT "Sum="; S
400 REM Strings
410 PRINT
420 NAME$ = "BASIC"
430 PRINT "Language: "; NAME$
440 PRINT
450 PRINT "Demo complete! All features working."
460 END
