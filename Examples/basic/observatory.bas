10 REM THE NIGHT WATCH
20 REM Five orbit radii, read from DATA and drawn as a scale model.
30 PRINT "NIGHT WATCH - ORBITAL DISTANCES"
40 PRINT "--------------------------------"
50 SCREEN 1
60 FOR RING = 1 TO 5
70   READ RADIUS
80   PRINT "Orbit " + STR$(RING) + " radius " + STR$(RADIUS)
90   SETPENCOLOR 35 * RING, 255 - 30 * RING, 90 + 25 * RING
100  CIRCLE (320, 220), RADIUS
110 NEXT RING
120 DATA 28, 54, 82, 112, 145
130 PRINT "Five paths around one quiet star."
