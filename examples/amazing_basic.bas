REM =================================================================
REM   TIME WARP DASHBOARD - AMAZING BASIC DEMO
REM   Showcases: Graphics, Math, Loops, Input, Subroutines, Color
REM =================================================================

REM --- Setup Screen ---
CLS
SCREEN 1
COLOR "CYAN", "BLACK"
WIDTH 80

REM --- Main Loop ---
GOSUB 1000 : REM Draw Interface Frame
GOSUB 2000 : REM Draw Radar
GOSUB 3000 : REM System Check
GOSUB 4000 : REM User Login

REM --- Final Animation ---
LOCATE 22, 25
PRINT "ACCESS GRANTED. INITIATING WARP DRIVE..."
FOR I = 1 TO 50
    COLOR "YELLOW"
    CIRCLE 400, 300, I * 5
    COLOR "RED"
    CIRCLE 400, 300, I * 5 + 2
NEXT I

LOCATE 24, 30
PRINT "WELCOME TO TIME WARP IDE!"
END

REM =================================================================
REM   SUBROUTINE: Draw Interface Frame
REM =================================================================
1000 REM Draw outer border
     COLOR "BLUE"
     LINE 10, 10, 790, 10
     LINE 790, 10, 790, 590
     LINE 790, 590, 10, 590
     LINE 10, 590, 10, 10
     
     REM Draw title box
     LINE 10, 60, 790, 60
     LOCATE 2, 30
     COLOR "WHITE"
     PRINT "=== TIME WARP SYSTEM CONTROL ==="
     
     REM Draw panels
     LINE 260, 60, 260, 590 : REM Vertical divider
     LINE 260, 300, 790, 300 : REM Horizontal divider right
     RETURN

REM =================================================================
REM   SUBROUTINE: Draw Radar (Math & Graphics)
REM =================================================================
2000 LOCATE 6, 45
     COLOR "GREEN"
     PRINT "RADAR SCAN"
     
     REM Radar center at 525, 180, Radius 100
     CIRCLE 525, 180, 100
     CIRCLE 525, 180, 70
     CIRCLE 525, 180, 40
     
     REM Sweep animation
     FOR A = 0 TO 360 STEP 10
         REM Calculate end point
         LET R = 100
         LET RAD = A * 3.14159 / 180
         LET X = 525 + R * COS(RAD)
         LET Y = 180 + R * SIN(RAD)
         
         COLOR "GREEN"
         LINE 525, 180, X, Y
         
         REM Simulate "blip"
         IF A = 120 THEN
             COLOR "RED"
             CIRCLE 480, 140, 5
             LOCATE 5, 35
             PRINT "TARGET DETECTED!"
         END IF
         
         REM Erase line (simple animation)
         COLOR "BLACK"
         LINE 525, 180, X, Y
     NEXT A
     
     REM Redraw radar structure
     COLOR "GREEN"
     LINE 525, 180, 525 + 100 * COS(2.09), 180 + 100 * SIN(2.09)
     RETURN

REM =================================================================
REM   SUBROUTINE: System Check (Text & Progress)
REM =================================================================
3000 LOCATE 6, 5
     COLOR "MAGENTA"
     PRINT "SYSTEM DIAGNOSTICS"
     
     REM List systems
     LOCATE 8, 5 : PRINT "CPU CORE:"
     LOCATE 10, 5 : PRINT "MEMORY:"
     LOCATE 12, 5 : PRINT "GRAPHICS:"
     LOCATE 14, 5 : PRINT "NETWORK:"
     
     REM Animate checks
     FOR S = 1 TO 4
         LOCATE 6 + S * 2, 15
         PRINT "CHECKING..."
         REM Simple delay loop
         FOR D = 1 TO 500 : NEXT D
         LOCATE 6 + S * 2, 15
         COLOR "GREEN"
         PRINT "ONLINE    "
         COLOR "MAGENTA"
     NEXT S
     RETURN

REM =================================================================
REM   SUBROUTINE: User Login (Input)
REM =================================================================
4000 LOCATE 20, 40
     COLOR "CYAN"
     PRINT "LOGIN REQUIRED"
     
     LOCATE 22, 40
     INPUT "ENTER USERNAME: ", USER$
     
     LOCATE 24, 40
     PRINT "HELLO, "; USER$
     RETURN
