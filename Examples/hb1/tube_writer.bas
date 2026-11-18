10 REM ============================================================
20 REM HB-1: THE TUBE IS MEMORY
30 REM Every character below is written into the screen cells at
40 REM $8000 (32768), so the tube shows it and PEEK reads it back.
50 REM Each cell also has a colour byte at $9000 (36864):
60 REM the ink in the high nibble, the paper in the low one.
70 REM Run this on the HB-1 to paint its tube; the same program
80 REM runs in the IDE, where the turtle part still draws.
90 REM ============================================================
100 CLS
110 MESSAGE$ = "HB-1  SCREEN MEMORY"
120 ROW = 2
130 COL = 4
140 REM --- one POKE per character, at an address the program computes
150 FOR I = 1 TO LEN(MESSAGE$)
160   CODE = ASC(MID$(MESSAGE$, I, 1))
170   POKE 32768 + ROW * 80 + COL + I - 1, CODE
180 NEXT I
190 REM --- the same cells, colourised: ink 10 on paper 0 ----------
200 FOR I = 0 TO LEN(MESSAGE$) - 1
210   POKE 36864 + ROW * 80 + COL + I, 160
220 NEXT I
230 REM --- an underline drawn a row below, in the screen itself -----
240 FOR I = 0 TO LEN(MESSAGE$) - 1
250   POKE 32768 + (ROW + 1) * 80 + COL + I, 45
260 NEXT I
270 REM --- read the first cell back: the tube and PEEK agree -------
280 PEEK 32768 + ROW * 80 + COL
290 REM --- the cursor is an I/O register, so LOCATE is a write too
300 LOCATE 5, 4
310 PRINT "Written by POKE, read by PEEK."
320 REM --- border colour is register two of the I/O page ----------
330 POKE 40962, 9
340 REM --- and the canvas takes a turtle frame round the message --
350 SCREEN 1
360 SETPENCOLOR 90 190 255
370 SETPENWIDTH 2
380 FOR SIDE = 1 TO 4
390   FORWARD 130
400   RIGHT 90
410 NEXT SIDE
420 PRINT "Tube and canvas both agree."
