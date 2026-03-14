C     ══════════════════════════════════════
C     ⚛️  Physics Simulation: Projectile Motion
C     Demonstrates: DO loops, WRITE formatting,
C     arrays, functions, subroutines
C     ══════════════════════════════════════
      PROGRAM PHYSICS
      IMPLICIT NONE

      INTEGER I, NSTEPS
      REAL G, V0, ANGLE, RAD, VX, VY
      REAL DT, T, X, Y, MAXH, RANGE
      REAL TMAX, PI
      PARAMETER (PI = 3.14159265)
      PARAMETER (G = 9.81)
      PARAMETER (NSTEPS = 20)

C     Arrays to store trajectory
      REAL XPOS(100), YPOS(100), TVAL(100)

      WRITE(*,*) '=================================='
      WRITE(*,*) ' Projectile Motion Simulator'
      WRITE(*,*) '=================================='
      WRITE(*,*)

C     Initial conditions
      V0 = 50.0
      ANGLE = 45.0

      WRITE(*,100) V0
  100 FORMAT(' Initial velocity: ', F6.1, ' m/s')
      WRITE(*,110) ANGLE
  110 FORMAT(' Launch angle:     ', F6.1, ' degrees')
      WRITE(*,*)

C     Convert to radians and compute components
      RAD = ANGLE * PI / 180.0
      VX = V0 * COS(RAD)
      VY = V0 * SIN(RAD)

      WRITE(*,120) VX, VY
  120 FORMAT(' Vx = ', F6.2, ' m/s   Vy = ', F6.2, ' m/s')

C     Time of flight and range
      TMAX = 2.0 * VY / G
      RANGE = VX * TMAX
      MAXH = VY * VY / (2.0 * G)

      WRITE(*,*)
      WRITE(*,130) TMAX
  130 FORMAT(' Time of flight: ', F6.2, ' seconds')
      WRITE(*,140) RANGE
  140 FORMAT(' Horizontal range: ', F8.2, ' metres')
      WRITE(*,150) MAXH
  150 FORMAT(' Maximum height:   ', F8.2, ' metres')

C     Compute trajectory points
      DT = TMAX / REAL(NSTEPS)

      WRITE(*,*)
      WRITE(*,*) '──── Trajectory Table ────'
      WRITE(*,200)
  200 FORMAT(3X,'Step',4X,'Time(s)',4X,'X(m)',6X,'Y(m)',
     &       5X,'Speed(m/s)')
      WRITE(*,*) '  ────────────────────────────────────'
     &           // '──────────'

      DO 300 I = 0, NSTEPS
          T = REAL(I) * DT
          X = VX * T
          Y = VY * T - 0.5 * G * T * T

C         Don't go below ground
          IF (Y .LT. 0.0) Y = 0.0

C         Store for later use
          XPOS(I+1) = X
          YPOS(I+1) = Y
          TVAL(I+1) = T

C         Current speed
          CALL SPEED(VX, VY, G, T, V0)

          WRITE(*,250) I, T, X, Y, V0
  250     FORMAT(3X,I4,3X,F7.3,2X,F8.2,2X,F8.2,3X,F7.2)
  300 CONTINUE

C     Print ASCII trajectory plot
      WRITE(*,*)
      WRITE(*,*) '──── Trajectory Plot ────'
      CALL PLOTXY(XPOS, YPOS, NSTEPS+1, RANGE, MAXH)

C     Energy analysis
      WRITE(*,*)
      WRITE(*,*) '──── Energy Analysis ────'
      CALL ENERGY(V0, G, XPOS, YPOS, TVAL, NSTEPS+1)

      WRITE(*,*)
      WRITE(*,*) 'Simulation complete!'
      STOP
      END

C     ── Subroutine: compute speed ──
      SUBROUTINE SPEED(VX, VY0, G, T, SPD)
      REAL VX, VY0, G, T, SPD, VYCUR
      VYCUR = VY0 - G * T
      SPD = SQRT(VX*VX + VYCUR*VYCUR)
      RETURN
      END

C     ── Subroutine: ASCII plot ──
      SUBROUTINE PLOTXY(X, Y, N, XMAX, YMAX)
      INTEGER N, I, J, COL, ROW
      REAL X(N), Y(N), XMAX, YMAX
      INTEGER PROWS, PCOLS
      PARAMETER (PROWS = 12, PCOLS = 50)
      CHARACTER*52 LINE

      DO 500 ROW = PROWS, 0, -1
          LINE = '                                                    '
          LINE(1:1) = '|'
          DO 400 I = 1, N
              COL = INT(X(I) / XMAX * REAL(PCOLS)) + 2
              IF (COL .LT. 2) COL = 2
              IF (COL .GT. 51) COL = 51
              J = INT(Y(I) / YMAX * REAL(PROWS))
              IF (J .EQ. ROW) THEN
                  LINE(COL:COL) = '*'
              END IF
  400     CONTINUE
          WRITE(*,'(A)') LINE
  500 CONTINUE
      WRITE(*,'(A)') '+' // '─────────────────────────────────'
     &           // '──────────────────>'
      RETURN
      END

C     ── Subroutine: energy analysis ──
      SUBROUTINE ENERGY(V0, G, X, Y, T, N)
      INTEGER N, I
      REAL V0, G, X(N), Y(N), T(N)
      REAL KE, PE, TOTAL, MASS, VY, VX2

      MASS = 1.0
      VX2 = X(2) / T(2)
      VX2 = VX2 * VX2

      WRITE(*,600)
  600 FORMAT(3X,'Point',3X,'KE(J)',6X,'PE(J)',6X,'Total(J)')
      WRITE(*,*) '  ──────────────────────────────────'

      DO 700 I = 1, N, 4
          VY = (V0 * SIN(ATAN2(Y(2),X(2))) - G * T(I))
          KE = 0.5 * MASS * (VX2 + VY*VY)
          PE = MASS * G * Y(I)
          TOTAL = KE + PE
          WRITE(*,650) I, KE, PE, TOTAL
  650     FORMAT(3X,I5,2X,F9.2,2X,F9.2,2X,F9.2)
  700 CONTINUE
      RETURN
      END
