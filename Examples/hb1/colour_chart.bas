10 REM ============================================================
20 REM HB-1: SIXTEEN STEPS OF PHOSPHOR
30 REM The tube is monochrome, so a colour number is a brightness:
40 REM 0 is the background and 15 the brightest ink. The attribute
50 REM byte at 36864 holds a pair of them -- ink in the high nibble,
60 REM paper in the low one -- and each cell is written directly, so
70 REM the picture is in memory before it is on the glass.
80 REM ============================================================
90 CLS
100 REM --- the left ladder: sixteen cells, ink 0 to 15 on black ---
110 FOR STEP = 0 TO 15
120   POKE 32768 + (4 + STEP) * 80 + 8, 128
130   POKE 36864 + (4 + STEP) * 80 + 8, STEP * 16
140 NEXT STEP
150 REM --- and the same sixteen the other way up: ink 0 on paper ---
160 FOR STEP = 0 TO 15
170   POKE 32768 + (4 + STEP) * 80 + 20, 128
180   POKE 36864 + (4 + STEP) * 80 + 20, STEP
190 NEXT STEP
200 REM --- a strip of the sixteen side by side, along row 22 -----
210 FOR STEP = 0 TO 15
220   POKE 32768 + 18 * 80 + (8 + STEP * 2), 128
230   POKE 36864 + 18 * 80 + (8 + STEP * 2), STEP * 16
240 NEXT STEP
250 REM --- what the reader is looking at, in the tube's own voice -
260 FOR I = 1 TO 30
270   POKE 32768 + 2 * 80 + (2 + I), 45
280 NEXT I
290 LOCATE 2, 4
300 COLOR 15, 0
310 PRINT "PHOSPHOR: EACH CELL IS ONE BYTE OF BRIGHTNESS";
320 LOCATE 16, 4
330 PRINT "INK 0..15 ON BLACK, THEN PAPER 0..15 UNDER BLACK INK:";
340 LOCATE 24, 4
350 PRINT "THE SAME BYTE THE POKE WRITES IS THE PICTURE.";
360 END
