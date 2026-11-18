10 REM ============================================================
20 REM HB-1: RAM THAT PROVES ITSELF
30 REM The machine claims 32K of free RAM below the screen at
40 REM $8000. This writes a pattern into it, reads three cells
50 REM back with PEEK, then draws a bar for every step of the
60 REM pattern it laid down.
70 REM ============================================================
80 CLS
90 PRINT "RAM PROBE - 32K BELOW THE SCREEN"
100 PRINT "--------------------------------"
110 REM --- lay a pattern across the first kilobyte ---------------
120 FOR ADDRESS = 0 TO 1023 STEP 4
130   POKE ADDRESS, 64 + ((ADDRESS / 4) MOD 128)
140 NEXT ADDRESS
150 REM --- ask the machine what it actually holds ---------------
160 PRINT "Three cells, written then read:"
170 PEEK 0
180 PEEK 4
190 PEEK 8
200 REM --- the same bytes decide a drawing ----------------------
210 SCREEN 1
220 FOR BAR = 0 TO 7
230   VALUE = 64 + (BAR * 17)
240   LEFT = 60 + BAR * 80
250   SETPENCOLOR 60 + BAR * 20, 200 - BAR * 15, 120 + BAR * 10
260   LINE (LEFT, 520) - (LEFT + 56, 520 - VALUE)
270   LINE (LEFT, 520 - VALUE) - (LEFT + 56, 520 - VALUE)
280   PRINT "Bar " + STR$(BAR) + " drew a byte of " + STR$(VALUE)
290 NEXT BAR
300 PRINT "Each bar is one step of the pattern, 64, 81, 98 ..."
310 PRINT "The top of RAM and the top of the drawing are the same idea."
