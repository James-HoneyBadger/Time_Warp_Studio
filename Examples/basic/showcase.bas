10 REM ============================================================
20 REM THE WEATHER STATION
30 REM A BASIC showcase: values, strings, arrays, control flow,
40 REM procedures, error handling, turtle graphics and sound.
50 REM ============================================================
60 PRINT "W E A T H E R   S T A T I O N"
70 PRINT "----------------------------------------"
80 PRINT ""
90 REM --- 1. Values, strings and formatting ----------------------
100 NAME$ = "Fair Oak Farm"
110 STATION$ = MID$(NAME$, 1, 4)
120 PRINT "Station  : " + UCASE$(STATION$)
130 PRINT "Place    : " + NAME$
140 PRINT "Name is  : " + STR$(LEN(NAME$)) + " characters"
150 PRINT "Initials : " + MID$(NAME$, 1, 1) + MID$(NAME$, 6, 1) + MID$(NAME$, 10, 1)
160 PRINT ""
170 REM --- 2. Constants, DATA, READ and an array ------------------
180 CONST DAYS = 7
190 DIM RAIN(7)
200 DATA 12, 0, 4, 18, 7, 0, 21
210 READ R1, R2, R3, R4, R5, R6, R7
215 REM READ fills the scalars; copying them in makes the array numeric.
220 RAIN(1) = R1
221 RAIN(2) = R2
222 RAIN(3) = R3
223 RAIN(4) = R4
224 RAIN(5) = R5
225 RAIN(6) = R6
226 RAIN(7) = R7
230 PRINT "Rainfall for " + STR$(DAYS) + " days (mm):"
240 FOR DAY = 1 TO DAYS
250   VALUE = RAIN(DAY)
260   PRINT "  day " + STR$(DAY) + " : " + STR$(VALUE)
270 NEXT DAY
280 PRINT ""
290 REM --- 3. Accumulators and formatting -------------------------
300 TOTAL = 0
310 FOR DAY = 1 TO DAYS
320   VALUE = RAIN(DAY)
330   TOTAL = TOTAL + VALUE
340 NEXT DAY
350 MEAN = TOTAL / DAYS
360 PRINT "Total    : " + FORMAT$("%.2f", TOTAL) + " mm"
370 PRINT "Mean     : " + FORMAT$("%.2f", MEAN) + " mm"
380 WET = 0
390 DAY = 1
400 WHILE DAY <= DAYS
410   VALUE = RAIN(DAY)
420   IF VALUE > 0 THEN WET = WET + 1
430   DAY = DAY + 1
440 WEND
450 PRINT "Wet days : " + STR$(WET)
460 DRY = 0
470 DAY = 1
480 DO
490   VALUE = RAIN(DAY)
500   IF VALUE = 0 THEN DRY = DRY + 1
510   DAY = DAY + 1
520 LOOP UNTIL DAY > DAYS
530 PRINT "Dry days : " + STR$(DRY)
540 EVENS = 0
550 FOR DAY = 2 TO DAYS STEP 2
560   VALUE = RAIN(DAY)
570   EVENS = EVENS + VALUE
580 NEXT DAY
590 PRINT "Even days: " + STR$(EVENS) + " mm"
600 PRINT ""
610 REM --- 4. Selection ------------------------------------------
620 IF MEAN > 10 THEN
630   PRINT "Verdict  : a wet week."
640 ELSE
650   IF MEAN > 5 THEN
660     PRINT "Verdict  : a typical week."
670   ELSE
680     PRINT "Verdict  : a dry week."
690   END IF
700 END IF
710 REM --- 5. User-defined functions and the math library ----------
720 DEF FNC(F) = (F - 32) * 5 / 9
730 CELSIUS = FNC(68)
740 PRINT "68 Fahrenheit is " + FORMAT$("%.2f", CELSIUS) + " Celsius"
750 ROOT = SQR(144)
760 MAG = ABS(-9)
770 WHOLE = INT(7.9)
780 PRINT "SQR(144) = " + STR$(ROOT) + ", ABS(-9) = " + STR$(MAG)
790 PRINT "INT(7.9) = " + STR$(WHOLE)
800 SINE = SIN(0)
810 COSINE = COS(0)
820 SLOPE = ATAN(1)
830 EZERO = EXP(0)
840 LONE = LOG(1)
850 PRINT "SIN(0) = " + STR$(SINE) + ", COS(0) = " + STR$(COSINE)
860 PRINT "ATAN(1) = " + FORMAT$("%.4f", SLOPE)
870 PRINT "EXP(0) = " + STR$(EZERO) + ", LOG(1) = " + STR$(LONE)
880 PRINT ""
890 REM --- 6. Subroutines and named subprograms -------------------
900 GOSUB 1200
910 CALL BANNER("SUB BANNER", TOTAL)
920 PRINT ""
930 REM --- 7. Error handling --------------------------------------
940 ON ERROR GOTO 1270
950 RISKY = RAIN(1) / (RAIN(2) + 0)
960 PRINT "This line is skipped by RESUME NEXT"
970 PRINT "The run continued after the error."
980 PRINT ""
990 REM --- 8. Graphics, turtle, shapes and sound ------------------
1000 SCREEN 1
1010 COLOR 15, 0
1020 PRINT "Drawing the week as a bar chart..."
1030 FOR DAY = 1 TO DAYS
1040   VALUE = RAIN(DAY)
1050   X = 40 + DAY * 30
1060   LINE (X, 200) - (X + 20, 200 - VALUE * 6)
1070 NEXT DAY
1080 CIRCLE (300, 120), 40
1090 PSET (10, 10)
1100 SETPENCOLOR "YELLOW"
1110 SETPENWIDTH 2
1120 HOME
1130 FOR SPOKE = 1 TO 8
1140   FORWARD 40
1150   BACK 40
1160   RIGHT 45
1170 NEXT SPOKE
1180 SHAPE STAR, 30
1190 PARTICLE EXPLOSION, 300, 120
1191 BEEP
1192 SOUND 660, 4
1193 PLAY "T160 O5 L8 C E G"
1195 PRINT ""
1196 PRINT "Report complete."
1197 END
1200 REM --- the subroutine the GOSUB calls -------------------------
1210 SUMCHECK = 0
1220 FOR DAY = 1 TO DAYS
1230   VALUE = RAIN(DAY)
1240   SUMCHECK = SUMCHECK + VALUE
1250 NEXT DAY
1260 PRINT "GOSUB recomputed the total: " + STR$(SUMCHECK) + " mm"
1265 RETURN
1270 REM --- the error handler ---------------------------------------
1280 PRINT "Caught a division by zero; continuing with RESUME NEXT."
1290 RESUME NEXT
1300 REM --- a SUB with parameters -----------------------------------
1310 SUB BANNER(TITLE$, VALUE)
1320 PRINT "[" + TITLE$ + "] " + FORMAT$("%.2f", VALUE) + " mm"
1330 END SUB
