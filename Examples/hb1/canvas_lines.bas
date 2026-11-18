10 REM ============================================================
20 REM HB-1: THE OTHER SURFACE, AS A BAR CHART
30 REM The tube is a character display; the canvas beside it is a
40 REM framebuffer with coordinates. The same program reaches both:
50 REM POKE and PRINT go to the tube, LINE and CIRCLE address the
60 REM canvas. SCREEN 13 is the fixed-coordinate screen -- 320 by
70 REM 200 -- that the drawing statements are measured in.
80 REM ============================================================
90 SCREEN 13
100 CLS
110 DIM HEIGHT(8)
120 FOR I = 1 TO 8
130   LET HEIGHT(I) = 20 + (I MOD 4) * 30 + I * 6
140 NEXT I
150 REM --- the axes: one across, one down ------------------------
160 LINE (30, 20) - (30, 170), 15
170 LINE (30, 170) - (310, 170), 15
180 REM --- eight bars, each one LINE with the BF fill ------------
190 FOR I = 1 TO 8
200   LET X = 40 + (I - 1) * 34
210   LET TOP = 170 - HEIGHT(I)
220   LINE (X, TOP) - (X + 26, 169), I, BF
230   REM --- and a tick above each one, so the scale is readable --
240   LINE (X + 13, TOP - 4) - (X + 13, TOP - 1), 15
250 NEXT I
260 REM --- a circle for a title block, so the shape is not all bars
270 CIRCLE (70, 45), 26, 14
280 CIRCLE (70, 45), 16, 7
290 REM --- the tube says what the canvas is showing --------------
300 LOCATE 20, 3
310 COLOR 15, 0
320 PRINT "CANVAS 320x200: AXES, EIGHT BARS, TWO CIRCLES";
330 LOCATE 22, 3
340 PRINT "EVERY ONE OF THEM A LINE STATEMENT, ON A FRAMEBUFFER.";
350 END
