C     ================================================================
C     NUMERICAL METHODS IN FORTRAN
C     Demonstrates: Newton's Method, Bisection, Numerical Integration,
C     statistics, and array operations using classic Fortran style.
C     ================================================================
      PROGRAM NUMETHODS
      IMPLICIT NONE

C     ── WORKING VARIABLES ──
      REAL X, XN, FX, DFX, A, B, MID, FA, FM
      REAL H, INTEGRAL, TOL
      REAL DATA(10), MEAN, VARIANCE, STDDEV, SSUM
      REAL SMIN, SMAX
      INTEGER I, J, N, ITER, NDATA, TEMP

      WRITE(*,*) '=============================='
      WRITE(*,*) ' FORTRAN NUMERICAL METHODS'
      WRITE(*,*) '=============================='

C     ============================================================
C     1. NEWTON-RAPHSON METHOD — Find root of f(x) = x^3 - 2x - 5
C        f(x)  = x**3 - 2*x - 5
C        f'(x) = 3*x**2 - 2
C     ============================================================
      WRITE(*,*) ' '
      WRITE(*,*) '1. NEWTON-RAPHSON METHOD'
      WRITE(*,*) '   f(x) = x^3 - 2x - 5 = 0'

      X   = 2.0
      TOL = 0.00001
      ITER = 0

      DO 100 I = 1, 50
          FX  = X**3 - 2.0*X - 5.0
          DFX = 3.0*X**2 - 2.0
          IF (ABS(FX) .LT. TOL) GOTO 101
          X = X - FX / DFX
          ITER = ITER + 1
  100 CONTINUE
  101 CONTINUE

      WRITE(*,*) '   Converged after iterations:'
      WRITE(*,*) ITER
      WRITE(*,*) '   Root found:'
      WRITE(*,*) X
      FX = X**3 - 2.0*X - 5.0
      WRITE(*,*) '   f(root) residual:'
      WRITE(*,*) FX

C     ============================================================
C     2. BISECTION METHOD — Same function, cross-check
C     ============================================================
      WRITE(*,*) ' '
      WRITE(*,*) '2. BISECTION METHOD'

      A = 2.0
      B = 3.0
      ITER = 0

      DO 200 I = 1, 100
          MID = (A + B) / 2.0
          FM = MID**3 - 2.0*MID - 5.0
          IF (ABS(FM) .LT. TOL) GOTO 201
          FA = A**3 - 2.0*A - 5.0
          IF (FA * FM .LT. 0.0) THEN
              B = MID
          ELSE
              A = MID
          ENDIF
          ITER = ITER + 1
  200 CONTINUE
  201 CONTINUE

      WRITE(*,*) '   Root found:'
      WRITE(*,*) MID
      WRITE(*,*) '   Iterations:'
      WRITE(*,*) ITER

C     ============================================================
C     3. NUMERICAL INTEGRATION — Simpson's Rule
C        Integrate f(x) = x^2 from 0 to 1 (exact = 1/3)
C     ============================================================
      WRITE(*,*) ' '
      WRITE(*,*) '3. SIMPSONS RULE INTEGRATION'
      WRITE(*,*) '   Integral of x^2 from 0 to 1'

      N = 100
      A = 0.0
      B = 1.0
      H = (B - A) / N
      INTEGRAL = A**2 + B**2

      DO 300 I = 1, N - 1
          X = A + I * H
          IF (MOD(I, 2) .EQ. 0) THEN
              INTEGRAL = INTEGRAL + 2.0 * X**2
          ELSE
              INTEGRAL = INTEGRAL + 4.0 * X**2
          ENDIF
  300 CONTINUE
      INTEGRAL = INTEGRAL * H / 3.0

      WRITE(*,*) '   Result (should be 0.333):'
      WRITE(*,*) INTEGRAL

C     ============================================================
C     4. DESCRIPTIVE STATISTICS
C     ============================================================
      WRITE(*,*) ' '
      WRITE(*,*) '4. DESCRIPTIVE STATISTICS'

      NDATA = 10
      DATA(1)  = 23.0
      DATA(2)  = 17.0
      DATA(3)  = 45.0
      DATA(4)  = 31.0
      DATA(5)  = 8.0
      DATA(6)  = 52.0
      DATA(7)  = 19.0
      DATA(8)  = 37.0
      DATA(9)  = 11.0
      DATA(10) = 42.0

      WRITE(*,*) '   Data set:'
      DO 400 I = 1, NDATA
          WRITE(*,*) DATA(I)
  400 CONTINUE

C     Mean
      SSUM = 0.0
      DO 410 I = 1, NDATA
          SSUM = SSUM + DATA(I)
  410 CONTINUE
      MEAN = SSUM / NDATA

      WRITE(*,*) '   Mean:'
      WRITE(*,*) MEAN

C     Min and Max
      SMIN = DATA(1)
      SMAX = DATA(1)
      DO 420 I = 2, NDATA
          IF (DATA(I) .LT. SMIN) SMIN = DATA(I)
          IF (DATA(I) .GT. SMAX) SMAX = DATA(I)
  420 CONTINUE

      WRITE(*,*) '   Min:'
      WRITE(*,*) SMIN
      WRITE(*,*) '   Max:'
      WRITE(*,*) SMAX
      WRITE(*,*) '   Range:'
      WRITE(*,*) SMAX - SMIN

C     Variance and Std Dev
      VARIANCE = 0.0
      DO 430 I = 1, NDATA
          VARIANCE = VARIANCE + (DATA(I) - MEAN)**2
  430 CONTINUE
      VARIANCE = VARIANCE / NDATA
      STDDEV = SQRT(VARIANCE)

      WRITE(*,*) '   Variance:'
      WRITE(*,*) VARIANCE
      WRITE(*,*) '   Std Dev:'
      WRITE(*,*) STDDEV

C     ============================================================
C     5. FIBONACCI SEQUENCE
C     ============================================================
      WRITE(*,*) ' '
      WRITE(*,*) '5. FIBONACCI SEQUENCE (first 15)'

      DATA(1) = 1.0
      DATA(2) = 1.0
      DO 500 I = 3, 10
          DATA(I) = DATA(I-1) + DATA(I-2)
  500 CONTINUE
      DO 510 I = 1, 10
          WRITE(*,*) DATA(I)
  510 CONTINUE

C     ============================================================
C     6. BUBBLE SORT
C     ============================================================
      WRITE(*,*) ' '
      WRITE(*,*) '6. BUBBLE SORT'

      DATA(1) = 64.0
      DATA(2) = 34.0
      DATA(3) = 25.0
      DATA(4) = 12.0
      DATA(5) = 22.0
      DATA(6) = 11.0
      DATA(7) = 90.0
      N = 7

      WRITE(*,*) '   Before:'
      DO 600 I = 1, N
          WRITE(*,*) DATA(I)
  600 CONTINUE

      DO 620 I = 1, N - 1
          DO 610 J = 1, N - I
              IF (DATA(J) .GT. DATA(J+1)) THEN
                  X = DATA(J)
                  DATA(J) = DATA(J+1)
                  DATA(J+1) = X
              ENDIF
  610     CONTINUE
  620 CONTINUE

      WRITE(*,*) '   After:'
      DO 630 I = 1, N
          WRITE(*,*) DATA(I)
  630 CONTINUE

C     ============================================================
C     7. POWER TABLE
C     ============================================================
      WRITE(*,*) ' '
      WRITE(*,*) '7. POWERS TABLE (n, n^2, n^3)'

      DO 700 I = 1, 8
          WRITE(*,*) I, I**2, I**3
  700 CONTINUE

      WRITE(*,*) ' '
      WRITE(*,*) '=============================='
      WRITE(*,*) ' ALL METHODS COMPLETE'
      WRITE(*,*) '=============================='

      END
