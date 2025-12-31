10 REM ============================================
20 REM   HONEY BADGER SAYS THANK YOU!
30 REM   Colorful ASCII Art Animation
40 REM ============================================
50 CLS
60 COLOR 14, 1
70 GOSUB 1000
80 END

1000 REM ===== MAIN ANIMATION ROUTINE =====
1010 PRINT
1020 PRINT "                    *** THANK YOU! ***"
1030 PRINT
1040 PRINT "            Welcome to Time Warp Studio!"
1050 PRINT
1060 GOSUB 2000
1070 PRINT
1080 PRINT "    ┌─────────────────────────────────────┐"
1090 PRINT "    │   Meet: Honey Badger of Gratitude   │"
1100 PRINT "    └─────────────────────────────────────┘"
1110 PRINT
1120 GOSUB 3000
1130 PRINT
1140 GOSUB 4000
1150 PRINT
1160 PRINT "     Press ENTER to continue..."
1170 INPUT X$
1180 RETURN

2000 REM ===== PRINT DECORATIVE BORDER =====
2010 PRINT "     ╔═════════════════════════════════════╗"
2020 PRINT "     ║         🦡  HONEY BADGER  🦡        ║"
2030 PRINT "     ║                                     ║"
2040 PRINT "     ║    ★  THANK YOU!  ★  GRACIAS  ★    ║"
2050 PRINT "     ║           MERCI!   谢谢            ║"
2060 PRINT "     ║                                     ║"
2070 PRINT "     ╚═════════════════════════════════════╝"
2080 RETURN

3000 REM ===== DRAW THE BADGER =====
3010 PRINT "              ╭─────────────╮"
3020 PRINT "              │  🦡 ▓▓ ▓▓   │  Head"
3030 PRINT "              │   ◉ ◉ •    │  (Eyes & Nose)"
3040 PRINT "              ╰─────────────╯"
3050 PRINT
3060 PRINT "            ┌────────────────────┐"
3070 PRINT "         ◂──┤  ■■■■■■■■■■■■■■■■  ├──▸"
3080 PRINT "            │  Body with stripes │"
3090 PRINT "            └────────────────────┘"
3100 PRINT "                    ╲"
3110 PRINT "                     ╲  Tail"
3120 PRINT "                      ◥"
3130 RETURN

4000 REM ===== DISPLAY MESSAGE =====
4010 PRINT "╔══════════════════════════════════════════╗"
4020 PRINT "║                                          ║"
4030 PRINT "║  With sincere gratitude for:             ║"
4040 PRINT "║                                          ║"
4050 PRINT "║  ✓ Your support & collaboration          ║"
4060 PRINT "║  ✓ Making Time Warp Studio amazing       ║"
4070 PRINT "║  ✓ Building the future of coding         ║"
4080 PRINT "║                                          ║"
4090 PRINT "║            Happy New Year 2026!          ║"
4100 PRINT "║                                          ║"
4110 PRINT "╚══════════════════════════════════════════╝"
4120 RETURN
