10 REM ============================================================
20 REM HB-1: A SOUND BOARD IN EIGHT NOTES
30 REM SOUND takes a frequency in hertz and a length in ticks of
40 REM 1/18.2 of a second, which is the machine's own unit. Each
50 REM note also POKEs a block into the tube, so what is heard and
60 REM what is seen are the same event.
70 REM ============================================================
80 CLS
90 PRINT "HB-1 SOUND BOARD"
100 PRINT "----------------"
110 PRINT ""
120 DIM NOTE(8)
130 LET NOTE(1) = 262
140 LET NOTE(2) = 294
150 LET NOTE(3) = 330
160 LET NOTE(4) = 349
170 LET NOTE(5) = 392
180 LET NOTE(6) = 440
190 LET NOTE(7) = 494
200 LET NOTE(8) = 523
210 REM --- the scale, one block per note, left to right -----------
220 FOR I = 1 TO 8
230   LET FREQ = NOTE(I)
240   SOUND FREQ, 2
250   LET BAR = 4
260   FOR R = 1 TO BAR
270     POKE 32768 + (8 + R) * 80 + 10 + I * 3, 128
280   NEXT R
290   LOCATE 18, 10
300   PRINT "NOTE "; I; " = "; FREQ; " HZ";
310   LET BAR = BAR + 1
320 NEXT I
330 REM --- and a chord: three notes, one after another ------------
340 PRINT ""
350 PRINT "A CHORD, ONE NOTE AT A TIME:"
360 SOUND 262, 3
370 SOUND 330, 3
380 SOUND 392, 4
390 PRINT "C, E, G - THE SAME THREE BLOCKS, TALLER.";
400 REM --- silence, said out loud ---------------------------------
410 SOUND OFF
420 COLOR 15, 0
430 LOCATE 24, 10
440 PRINT "THE TICKS ARE THE TUBE'S, NOT THE SPEAKER'S.";
450 END
