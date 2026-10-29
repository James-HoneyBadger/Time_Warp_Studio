10 REM ============================================================
20 REM HB-1: TWO CLOCKS, ONE READABLE AND ONE THAT INTERRUPTS
30 REM Registers six and seven of the I/O page are a 16-bit frame
40 REM counter at $A006/$A007 (40966/40967), sixty ticks a second.
50 REM ON TIMER is the other clock: TIMER ON arms it, and its
60 REM handler is called between statements while the program works.
70 REM ============================================================
80 CLS
90 PRINT "HB-1 CLOCK WORK"
100 PRINT "---------------"
110 PRINT "Frame counter, low byte then high byte:"
120 PEEK 40966
130 PEEK 40967
140 REM --- the interrupt clock -----------------------------------
150 TICKS = 0
160 ON TIMER(0.01) GOSUB 400
170 TIMER ON
180 FOR STEP = 1 TO 30000
190   WORK = STEP * 2
200 NEXT STEP
210 TIMER OFF
220 PRINT "The handler ran " + STR$(TICKS) + " time(s) inside the loop."
230 REM --- the count becomes a drawing ---------------------------
240 SCREEN 1
250 CIRCLE (320, 260), 110
260 CIRCLE (320, 260), 8
270 FOR TICK = 0 TO TICKS
280   ANGLE = TICK * 20
290   X = 320 + 100 * SIN(ANGLE)
300   Y = 260 - 100 * COS(ANGLE)
310   SETPENCOLOR 80 + TICK * 15, 200, 255 - TICK * 15
320   LINE (320, 260) - (X, Y)
330 NEXT TICK
340 PRINT "Each spoke is one interrupt; the hub is the dial."
350 PRINT "The frame counter above is the clock a program can read."
360 END
400 REM --- the interrupt handler -----------------------------------
410 TICKS = TICKS + 1
420 RETURN
