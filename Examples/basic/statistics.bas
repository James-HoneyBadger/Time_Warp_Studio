10  REM ================================================
20  REM  STATISTICS CALCULATOR - Science Tool
30  REM  Enter a data set; get mean, median, mode,
40  REM  standard deviation, quartiles, histogram.
50  REM  A genuinely useful statistical analysis tool.
60  REM ================================================
70  DIM DATA(100)
80  N = 0
90  PRINT "╔══════════════════════════════════╗"
100 PRINT "║    STATISTICS CALCULATOR v1.0    ║"
110 PRINT "║   Mean · Median · Mode · StdDev  ║"
120 PRINT "╚══════════════════════════════════╝"
130 PRINT ""
140 PRINT "Enter data values one at a time."
150 PRINT "Type 0 when finished (or press Enter with nothing)."
160 PRINT ""
170 MORE = 1
180 WHILE MORE = 1
190   PRINT "Value "; N+1; ": ";
200   INPUT V
210   IF V = 0 AND N > 0 THEN MORE = 0 : GOTO 240
220   N = N + 1
230   DATA(N) = V
240 WEND
250 IF N < 2 THEN PRINT "Need at least 2 values." : END
260 GOSUB 9000
270 PRINT ""
280 PRINT "╔══════════════════════════════════════════╗"
290 PRINT "║            ANALYSIS RESULTS              ║"
300 PRINT "╚══════════════════════════════════════════╝"
310 PRINT ""
320 PRINT "  Count:        "; N
330 PRINT "  Sum:          "; SUM_VAL
340 PRINT "  Mean:         "; MEAN_VAL
350 PRINT "  Minimum:      "; MIN_VAL
360 PRINT "  Maximum:      "; MAX_VAL
370 PRINT "  Range:        "; MAX_VAL - MIN_VAL
380 PRINT "  Median:       "; MEDIAN_VAL
390 PRINT "  Std Deviation:"; STDDEV_VAL
390 PRINT "  Variance:     "; VARIANCE_VAL
400 PRINT "  Q1 (25th):    "; Q1_VAL
410 PRINT "  Q3 (75th):    "; Q3_VAL
420 PRINT "  IQR:          "; Q3_VAL - Q1_VAL
430 PRINT ""
440 GOSUB 5000
450 PRINT ""
460 GOSUB 6000
470 END

1000 REM Sort array using bubble sort
1010 FOR I = 1 TO N - 1
1020   FOR J = 1 TO N - I
1030     IF DATA(J) > DATA(J+1) THEN TEMP = DATA(J) : DATA(J) = DATA(J+1) : DATA(J+1) = TEMP
1040   NEXT J
1050 NEXT I
1060 RETURN

5000 REM ============================
5010 REM  ASCII HISTOGRAM
5020 REM ============================
5030 PRINT "  HISTOGRAM:"
5040 PRINT "  ──────────"
5050 BINS = 5
5060 BIN_W = (MAX_VAL - MIN_VAL) / BINS
5070 IF BIN_W = 0 THEN BIN_W = 1
5080 FOR B = 0 TO BINS - 1
5090   BIN_LO = MIN_VAL + B * BIN_W
5100   BIN_HI = BIN_LO + BIN_W
5110   COUNT = 0
5120   FOR I = 1 TO N
5130     IF DATA(I) >= BIN_LO AND DATA(I) < BIN_HI THEN COUNT = COUNT + 1
5140   NEXT I
5150   REM Last bin is inclusive
5160   IF B = BINS - 1 THEN FOR I = 1 TO N : IF DATA(I) = MAX_VAL THEN COUNT = COUNT + 1 : NEXT I
5170   BARS$ = ""
5180   FOR K = 1 TO COUNT : BARS$ = BARS$ + "█" : NEXT K
5190   PRINT "  "; INT(BIN_LO); "-"; INT(BIN_HI); " |"; BARS$; " "; COUNT
5200 NEXT B
5210 RETURN

6000 REM ============================
6010 REM  FREQUENCY / MODE
6020 REM ============================
6030 PRINT "  FREQUENCY TABLE (top values):"
6040 PRINT "  Value   Frequency"
6050 PRINT "  ────────────────"
6060 MAX_FREQ = 0
6070 FOR I = 1 TO N
6080   IF DATA(I) = -999 THEN GOTO 6130
6090   FREQ = 1
6100   FOR J = I+1 TO N
6110     IF DATA(J) = DATA(I) THEN FREQ = FREQ + 1
6120   NEXT J
6130   PRINT "  "; DATA(I); "        "; FREQ
6140   IF FREQ > MAX_FREQ THEN MAX_FREQ = FREQ : MODE_VAL = DATA(I)
6150   REM Mark processed duplicates
6160   FOR J = I+1 TO N
6170     IF DATA(J) = DATA(I) THEN DATA(J) = -999
6180   NEXT J
6190 NEXT I
6200 PRINT ""
6210 PRINT "  Mode: "; MODE_VAL; " (appears "; MAX_FREQ; " times)"
6220 RETURN

9000 REM ============================
9010 REM  COMPUTE ALL STATS
9020 REM ============================
9030 REM Sort first
9040 GOSUB 1000
9050 REM Sum and Min/Max
9060 SUM_VAL = 0
9070 MIN_VAL = DATA(1)
9080 MAX_VAL = DATA(1)
9090 FOR I = 1 TO N
9100   SUM_VAL = SUM_VAL + DATA(I)
9110   IF DATA(I) < MIN_VAL THEN MIN_VAL = DATA(I)
9120   IF DATA(I) > MAX_VAL THEN MAX_VAL = DATA(I)
9130 NEXT I
9140 MEAN_VAL = SUM_VAL / N
9150 REM Median
9160 IF N MOD 2 = 1 THEN MEDIAN_VAL = DATA(INT(N/2) + 1)
9170 IF N MOD 2 = 0 THEN MEDIAN_VAL = (DATA(N/2) + DATA(N/2 + 1)) / 2
9180 REM Variance / StdDev
9190 SS = 0
9200 FOR I = 1 TO N
9210   SS = SS + (DATA(I) - MEAN_VAL) ^ 2
9220 NEXT I
9230 VARIANCE_VAL = SS / N
9240 STDDEV_VAL = SQR(VARIANCE_VAL)
9250 REM Quartiles
9260 Q1_IDX = INT(N * 0.25) + 1
9270 Q3_IDX = INT(N * 0.75) + 1
9280 Q1_VAL = DATA(Q1_IDX)
9290 Q3_VAL = DATA(Q3_IDX)
9300 RETURN
