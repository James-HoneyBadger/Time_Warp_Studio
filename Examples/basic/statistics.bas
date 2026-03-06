10  REM ================================================
20  REM  STATISTICS CALCULATOR - Science Tool
30  REM  Computes mean, median, mode, standard deviation,
40  REM  quartiles, histogram, and frequency table.
50  REM ================================================
60  DIM NUMS(100)
70  PRINT "╔══════════════════════════════════╗"
80  PRINT "║    STATISTICS CALCULATOR v1.0    ║"
90  PRINT "║   Mean · Median · Mode · StdDev  ║"
100 PRINT "╚══════════════════════════════════╝"
110 PRINT ""
120 REM ── Load sample data set ──
130 NUMS(1) = 85 : NUMS(2) = 92 : NUMS(3) = 78
140 NUMS(4) = 90 : NUMS(5) = 85 : NUMS(6) = 67
150 NUMS(7) = 95 : NUMS(8) = 72 : NUMS(9) = 88
160 NUMS(10) = 85 : NUMS(11) = 91 : NUMS(12) = 76
170 NUMS(13) = 83 : NUMS(14) = 69 : NUMS(15) = 94
180 N = 15
190 PRINT "  Sample data set ("; N; " values):"
200 FOR I = 1 TO N
210   PRINT "    "; NUMS(I);
220 NEXT I
230 PRINT ""
240 PRINT ""
250 GOSUB 9000
260 PRINT "╔══════════════════════════════════════════╗"
270 PRINT "║            ANALYSIS RESULTS              ║"
280 PRINT "╚══════════════════════════════════════════╝"
290 PRINT ""
300 PRINT "  Count:          "; N
310 PRINT "  Sum:            "; SUM_VAL
320 PRINT "  Mean:           "; MEAN_VAL
330 PRINT "  Minimum:        "; MIN_VAL
340 PRINT "  Maximum:        "; MAX_VAL
350 PRINT "  Range:          "; MAX_VAL - MIN_VAL
360 PRINT "  Median:         "; MEDIAN_VAL
370 PRINT "  Std Deviation:  "; STDDEV_VAL
380 PRINT "  Variance:       "; VARIANCE_VAL
390 PRINT "  Q1 (25th):      "; Q1_VAL
400 PRINT "  Q3 (75th):      "; Q3_VAL
410 PRINT "  IQR:            "; Q3_VAL - Q1_VAL
420 PRINT ""
430 GOSUB 5000
440 PRINT ""
450 GOSUB 6000
460 END
1000 REM ── Bubble sort ──
1010 FOR I = 1 TO N - 1
1020   FOR J = 1 TO N - I
1030     IF NUMS(J) > NUMS(J+1) THEN TEMP = NUMS(J) : NUMS(J) = NUMS(J+1) : NUMS(J+1) = TEMP
1040   NEXT J
1050 NEXT I
1060 RETURN
5000 REM ── ASCII Histogram ──
5010 PRINT "  HISTOGRAM:"
5020 PRINT "  ──────────"
5030 BINS = 5
5040 BIN_W = (MAX_VAL - MIN_VAL) / BINS
5050 IF BIN_W = 0 THEN BIN_W = 1
5060 FOR B = 0 TO BINS - 1
5070   BIN_LO = MIN_VAL + B * BIN_W
5080   BIN_HI = BIN_LO + BIN_W
5090   COUNT = 0
5100   FOR I = 1 TO N
5110     IF NUMS(I) >= BIN_LO AND NUMS(I) < BIN_HI THEN COUNT = COUNT + 1
5120   NEXT I
5130   IF B = BINS - 1 THEN FOR I = 1 TO N : IF NUMS(I) = MAX_VAL THEN COUNT = COUNT + 1 : NEXT I
5140   BARS$ = ""
5150   FOR K = 1 TO COUNT : BARS$ = BARS$ + "█" : NEXT K
5160   PRINT "  "; INT(BIN_LO); "-"; INT(BIN_HI); " |"; BARS$; " "; COUNT
5170 NEXT B
5180 RETURN
6000 REM ── Frequency / Mode ──
6010 PRINT "  FREQUENCY TABLE:"
6020 PRINT "  Value   Frequency"
6030 PRINT "  ────────────────"
6040 MAX_FREQ = 0
6050 FOR I = 1 TO N
6060   IF NUMS(I) = -999 THEN GOTO 6120
6070   FREQ = 1
6080   FOR J = I+1 TO N
6090     IF NUMS(J) = NUMS(I) THEN FREQ = FREQ + 1
6100   NEXT J
6110   PRINT "  "; NUMS(I); "        "; FREQ
6120   IF FREQ > MAX_FREQ THEN MAX_FREQ = FREQ : MODE_VAL = NUMS(I)
6130   FOR J = I+1 TO N
6140     IF NUMS(J) = NUMS(I) THEN NUMS(J) = -999
6150   NEXT J
6160 NEXT I
6170 PRINT ""
6180 PRINT "  Mode: "; MODE_VAL; " (appears "; MAX_FREQ; " times)"
6190 RETURN
9000 REM ── Compute all stats ──
9010 GOSUB 1000
9020 SUM_VAL = 0
9030 MIN_VAL = NUMS(1)
9040 MAX_VAL = NUMS(1)
9050 FOR I = 1 TO N
9060   SUM_VAL = SUM_VAL + NUMS(I)
9070   IF NUMS(I) < MIN_VAL THEN MIN_VAL = NUMS(I)
9080   IF NUMS(I) > MAX_VAL THEN MAX_VAL = NUMS(I)
9090 NEXT I
9100 MEAN_VAL = SUM_VAL / N
9110 IF N MOD 2 = 1 THEN MEDIAN_VAL = NUMS(INT(N/2) + 1)
9120 IF N MOD 2 = 0 THEN MEDIAN_VAL = (NUMS(N/2) + NUMS(N/2 + 1)) / 2
9130 SS = 0
9140 FOR I = 1 TO N
9150   SS = SS + (NUMS(I) - MEAN_VAL) ^ 2
9160 NEXT I
9170 VARIANCE_VAL = SS / N
9180 STDDEV_VAL = SQR(VARIANCE_VAL)
9190 Q1_IDX = INT(N * 0.25) + 1
9200 Q3_IDX = INT(N * 0.75) + 1
9210 Q1_VAL = NUMS(Q1_IDX)
9220 Q3_VAL = NUMS(Q3_IDX)
9230 RETURN
