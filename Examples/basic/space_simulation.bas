10 REM ============================================================
20 REM SOLAR SYSTEM EXPLORER — Time Warp Studio BASIC Showcase
30 REM Orbital mechanics, turtle graphics, space calculations
40 REM ============================================================
50 PRINT "╔══════════════════════════════════════════════════╗"
60 PRINT "║        SOLAR SYSTEM EXPLORER                    ║"
70 PRINT "║   Physics * Turtle Graphics * Astronomy         ║"
80 PRINT "╚══════════════════════════════════════════════════╝"
90 PRINT ""

100 REM --- Planet data: name, AU distance, year length (days), radius km
110 DIM PNAME$(9)
120 DIM PDIST(9)
130 DIM PYEAR(9)
140 DIM PRAD(9)
150 DIM PMASS(9)
160 DIM PCOLOR$(9)

170 PNAME$(1) = "Mercury"  : PDIST(1) = 0.39  : PYEAR(1) = 88    : PRAD(1) = 2440   : PCOLOR$(1) = "GRAY"
180 PNAME$(2) = "Venus"    : PDIST(2) = 0.72  : PYEAR(2) = 225   : PRAD(2) = 6052   : PCOLOR$(2) = "ORANGE"
190 PNAME$(3) = "Earth"    : PDIST(3) = 1.00  : PYEAR(3) = 365   : PRAD(3) = 6371   : PCOLOR$(3) = "BLUE"
200 PNAME$(4) = "Mars"     : PDIST(4) = 1.52  : PYEAR(4) = 687   : PRAD(4) = 3390   : PCOLOR$(4) = "RED"
210 PNAME$(5) = "Jupiter"  : PDIST(5) = 5.20  : PYEAR(5) = 4333  : PRAD(5) = 69911  : PCOLOR$(5) = "ORANGE"
220 PNAME$(6) = "Saturn"   : PDIST(6) = 9.58  : PYEAR(6) = 10759 : PRAD(6) = 58232  : PCOLOR$(6) = "YELLOW"
230 PNAME$(7) = "Uranus"   : PDIST(7) = 19.2  : PYEAR(7) = 30688 : PRAD(7) = 25362  : PCOLOR$(7) = "CYAN"
240 PNAME$(8) = "Neptune"  : PDIST(8) = 30.1  : PYEAR(8) = 60182 : PRAD(8) = 24622  : PCOLOR$(8) = "BLUE"

250 NPLANETS = 8

260 REM --- Section 1: Planet Table
270 PRINT "[ 1 ] PLANETARY DATA"
280 PRINT ""
290 PRINT "  Planet      AU Distance  Year(days)  Radius(km)  Moons"
300 PRINT "  ----------  -----------  ----------  ----------  -----"

310 DIM MOONS(9)
320 MOONS(1) = 0 : MOONS(2) = 0 : MOONS(3) = 1 : MOONS(4) = 2
330 MOONS(5) = 95 : MOONS(6) = 146 : MOONS(7) = 28 : MOONS(8) = 16

340 FOR P = 1 TO NPLANETS
350   PRINT "  " + PNAME$(P) + "       " + STR$(PDIST(P)) + "          " + STR$(PYEAR(P)) + "     " + STR$(PRAD(P)) + "    " + STR$(MOONS(P))
360 NEXT P

370 PRINT ""

380 REM --- Section 2: Kepler's Third Law
390 PRINT "[ 2 ] KEPLER'S THIRD LAW  (T^2 = a^3)"
400 PRINT "  Predicted vs actual orbital periods:"
410 PRINT ""
420 PRINT "  Planet      Predicted(days)  Actual(days)  Error%"
430 PRINT "  ----------  ---------------  ------------  ------"

440 FOR P = 1 TO NPLANETS
450   TPRED = 365.25 * (PDIST(P) ^ 1.5)
460   ERR = ABS(TPRED - PYEAR(P)) * 100 / PYEAR(P)
470   PRINT "  " + PNAME$(P) + "       " + STR$(INT(TPRED)) + "          " + STR$(PYEAR(P)) + "       " + STR$(INT(ERR * 10) / 10) + "%"
480 NEXT P

490 PRINT ""

500 REM --- Section 3: Light travel time from Sun
510 PRINT "[ 3 ] LIGHT TRAVEL TIME FROM SUN"
510 PRINT "  Speed of light = 299,792 km/s"
520 PRINT "  1 AU = 149,597,871 km"
530 PRINT ""

540 AU_KM = 149597871
550 LIGHT_KMS = 299792

560 FOR P = 1 TO NPLANETS
570   DIST_KM = PDIST(P) * AU_KM
580   SECS = DIST_KM / LIGHT_KMS
590   IF SECS < 60 THEN
600     PRINT "  " + PNAME$(P) + ": " + STR$(INT(SECS)) + " seconds"
610   ELSE
620     MINS = INT(SECS / 60)
630     PRINT "  " + PNAME$(P) + ": " + STR$(MINS) + " minutes " + STR$(INT(SECS - MINS * 60)) + " seconds"
640   END IF
650 NEXT P

660 PRINT ""

670 REM --- Section 4: Escape velocity
680 PRINT "[ 4 ] ESCAPE VELOCITY FROM EACH PLANET"
690 PRINT "  Formula: v = SQR(2 * G * M / R)"
700 PRINT "  (scaled to Earth = 1.0 relative)"
710 PRINT ""

720 DIM PGRAV(9)
730 PGRAV(1) = 3.7 : PGRAV(2) = 8.87 : PGRAV(3) = 9.81 : PGRAV(4) = 3.72
740 PGRAV(5) = 24.79 : PGRAV(6) = 10.44 : PGRAV(7) = 8.87 : PGRAV(8) = 11.15

750 FOR P = 1 TO NPLANETS
760   VESC = SQR(2 * PGRAV(P) * PRAD(P) * 1000) / 1000
770   BAR = INT(VESC / 2)
780   BARSTR = ""
790   FOR B = 1 TO BAR
800     BARSTR = BARSTR + "█"
810   NEXT B
820   PRINT "  " + PNAME$(P) + ":  " + STR$(INT(VESC * 10) / 10) + " km/s  " + BARSTR
830 NEXT P

840 PRINT ""

850 REM --- Section 5: Turtle Graphics - Solar System
860 PRINT "[ 5 ] DRAWING SOLAR SYSTEM (Turtle Graphics)"
870 PRINT ""

880 CLEARSCREEN
890 SETXY 0 0

900 REM Draw Sun
910 SETPENCOLOR "YELLOW"
920 FORWARD 15
930 RIGHT 90
940 FORWARD 15
950 LEFT 90

960 REM Draw orbital paths and planets
970 FOR P = 1 TO NPLANETS
980   REM Draw orbital circle
990   ORBIT_R = INT(PDIST(P) * 25)
1000  IF ORBIT_R > 280 THEN ORBIT_R = 280
1010  SETPENCOLOR "DARKGRAY"
1020  PENUP
1030  SETXY 0 (ORBIT_R * -1)
1040  PENDOWN
1050  FOR ANGLE = 1 TO 360
1060    X = SIN(ANGLE * 3.14159 / 180) * ORBIT_R
1070    Y = COS(ANGLE * 3.14159 / 180) * ORBIT_R
1080    SETXY X Y
1090  NEXT ANGLE
1100
1110  REM Place planet at current position in orbit
1120  PHASE = P * 37
1130  PX = SIN(PHASE * 3.14159 / 180) * ORBIT_R
1140  PY = COS(PHASE * 3.14159 / 180) * ORBIT_R
1150  PENUP
1160  SETXY PX PY
1170  PENDOWN
1180  SETPENCOLOR PCOLOR$(P)
1190  PRINT "  Drawing " + PNAME$(P) + " at (" + STR$(INT(PX)) + ", " + STR$(INT(PY)) + ")"
1200  GOSUB 1400
1210 NEXT P

1220 REM Draw Sun at center last (on top)
1230 PENUP
1240 SETXY 0 0
1250 PENDOWN
1260 SETPENCOLOR "YELLOW"
1270 FOR SR = 1 TO 12
1280   ANGLE2 = SR * 30
1290   SETXY SIN(ANGLE2 * 3.14159 / 180) * 12, COS(ANGLE2 * 3.14159 / 180) * 12
1300 NEXT SR

1310 PRINT ""
1320 PRINT "[ 6 ] FUN FACTS"
1330 PRINT "  The Sun contains 99.86% of all mass in the solar system"
1340 PRINT "  Jupiter alone has 2.5x more mass than all other planets combined"
1350 PRINT "  One day on Venus is longer than one year on Venus"
1360 PRINT "  Saturn would float in water (density = 0.69 g/cm^3)"
1370 PRINT "  Light from the Sun takes ~8 min 20 sec to reach Earth"
1380 PRINT ""
1390 END

1400 REM --- Subroutine: draw small dot at current position
1410 FOR DR = 0 TO 6
1420   SETXY PX + DR PY
1430   SETXY PX - DR PY
1440   SETXY PX PY + DR
1450   SETXY PX PY - DR
1460 NEXT DR
1470 RETURN
