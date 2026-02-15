10 REM Test Turbo BASIC Graphics Features
20 SCREEN 1
30 COLOR 15, 0
40 REM Draw lines with different styles
50 LINE (10,10)-(100,10),15
60 LINE (10,20)-(100,20),4
70 REM Draw rectangles (B = box outline, BF = box filled)
80 LINE (10,30)-(80,70),15,B
90 LINE (100,30)-(150,70),2,BF
100 REM Draw circles with color
110 CIRCLE (50,100),20,14
120 CIRCLE (120,100),15,5,0,3.14159
130 REM Place pixels
140 PSET (30,140),12
150 PSET (35,140),13
160 PSET (40,140),14
170 PSET (45,140),15
180 PSET (50,140),4
190 REM (Optional) Paint a region
200 REM The PAINT command fills the last shape
210 PAINT (120,100),1
220 END
