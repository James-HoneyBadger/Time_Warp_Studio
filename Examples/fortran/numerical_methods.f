C     ================================================================
C     NUMERICAL METHODS IN FORTRAN
C     Demonstrates: Newton's Method, Bisection, Gaussian Elimination,
C     Simpson's Rule integration, Euler/RK4 ODE solvers,
C     statistics, and matrix operations.
C     ================================================================
      PROGRAM NUMETHODS
      IMPLICIT NONE

C     ── EXTERNAL FUNCTION DECLARATIONS ──
      EXTERNAL FUNC1, DFUNC1, FUNC2
      DOUBLE PRECISION FUNC1, DFUNC1, FUNC2

C     ── WORKING VARIABLES ──
      DOUBLE PRECISION X, ROOT, H, SUM, A, B
      DOUBLE PRECISION Y1, Y2, K1, K2, K3, K4
      DOUBLE PRECISION MATRIX(4,5), SOL(4)
      DOUBLE PRECISION DATA(20)
      INTEGER I, J, N, ITER, NDATA
      DOUBLE PRECISION TOL, MEAN, VARIANCE, STDDEV

      WRITE(*,*) '=============================='
      WRITE(*,*) ' FORTRAN NUMERICAL METHODS'
      WRITE(*,*) '=============================='

C     ============================================================
C     1. NEWTON-RAPHSON METHOD — Find root of f(x) = x^3 - 2x - 5
C     ============================================================
      WRITE(*,*) ''
      WRITE(*,*) '1. NEWTON-RAPHSON METHOD'
      WRITE(*,*) '   f(x) = x^3 - 2x - 5 = 0'

      X    = 2.0D0
      TOL  = 1.0D-10
      ITER = 0

      DO WHILE (ABS(FUNC1(X)) .GT. TOL .AND. ITER .LT. 100)
          X    = X - FUNC1(X) / DFUNC1(X)
          ITER = ITER + 1
      END DO

      WRITE(*,'(A,F16.10,A,I3,A)')
     +    '   Root: ', X, '   (', ITER, ' iterations)'
      WRITE(*,'(A,E12.4)') '   f(root) = ', FUNC1(X)

C     ============================================================
C     2. BISECTION METHOD — Same function, cross-check
C     ============================================================
      WRITE(*,*) ''
      WRITE(*,*) '2. BISECTION METHOD'

      A = 2.0D0
      B = 3.0D0

      DO I = 1, 100
          X = (A + B) / 2.0D0
          IF (ABS(FUNC1(X)) .LT. TOL) GOTO 10
          IF (FUNC1(A) * FUNC1(X) .LT. 0.0D0) THEN
              B = X
          ELSE
              A = X
          END IF
      END DO
   10 CONTINUE
      WRITE(*,'(A,F16.10)') '   Root: ', X
      WRITE(*,'(A,E12.4)')  '   f(root) = ', FUNC1(X)

C     ============================================================
C     3. GAUSSIAN ELIMINATION — Solve 3x3 linear system
C        2x + y - z = 8
C        -3x - y + 2z = -11
C        -2x + y + 2z = -3
C     Solution: x=2, y=3, z=-1
C     ============================================================
      WRITE(*,*) ''
      WRITE(*,*) '3. GAUSSIAN ELIMINATION'
      WRITE(*,*) '   2x+y-z=8  -3x-y+2z=-11  -2x+y+2z=-3'

      MATRIX(1,1) =  2.0D0
      MATRIX(1,2) =  1.0D0
      MATRIX(1,3) = -1.0D0
      MATRIX(1,4) =  8.0D0
      MATRIX(2,1) = -3.0D0
      MATRIX(2,2) = -1.0D0
      MATRIX(2,3) =  2.0D0
      MATRIX(2,4) = -11.0D0
      MATRIX(3,1) = -2.0D0
      MATRIX(3,2) =  1.0D0
      MATRIX(3,3) =  2.0D0
      MATRIX(3,4) = -3.0D0

      CALL GAUSS_ELIM(MATRIX, SOL, 3)

      WRITE(*,'(A,3F8.2)') '   Solution (x,y,z): ',
     +    SOL(1), SOL(2), SOL(3)

C     ============================================================
C     4. SIMPSON'S RULE — Integrate sin(x) from 0 to pi
C        Exact answer = 2.0
C     ============================================================
      WRITE(*,*) ''
      WRITE(*,*) '4. SIMPSONS RULE INTEGRATION'
      WRITE(*,*) '   Integral of sin(x) from 0 to pi (exact=2.0)'

      A = 0.0D0
      B = 4.0D0 * ATAN(1.0D0)   ! pi
      N = 1000
      H = (B - A) / N

      SUM = FUNC2(A) + FUNC2(B)
      DO I = 1, N-1
          X = A + I * H
          IF (MOD(I,2) .EQ. 0) THEN
              SUM = SUM + 2.0D0 * FUNC2(X)
          ELSE
              SUM = SUM + 4.0D0 * FUNC2(X)
          END IF
      END DO
      SUM = SUM * H / 3.0D0

      WRITE(*,'(A,F18.14)') '   Computed: ', SUM
      WRITE(*,'(A,E12.4)')  '   Error:    ', ABS(SUM - 2.0D0)

C     ============================================================
C     5. RUNGE-KUTTA 4TH ORDER — Solve dy/dx = y - x^2 + 1
C        y(0) = 0.5, approximate y(2)
C        Exact: y = (x+1)^2 - 0.5*e^x
C     ============================================================
      WRITE(*,*) ''
      WRITE(*,*) '5. RUNGE-KUTTA 4TH ORDER ODE SOLVER'
      WRITE(*,*) '   dy/dx = y - x^2 + 1   y(0)=0.5'

      X  = 0.0D0
      Y1 = 0.5D0
      H  = 0.1D0
      N  = 20

      DO I = 1, N
          K1 = H * (Y1         - X**2       + 1.0D0)
          K2 = H * ((Y1+K1/2)  - (X+H/2)**2 + 1.0D0)
          K3 = H * ((Y1+K2/2)  - (X+H/2)**2 + 1.0D0)
          K4 = H * ((Y1+K3)    - (X+H)**2   + 1.0D0)
          Y1 = Y1 + (K1 + 2*K2 + 2*K3 + K4) / 6.0D0
          X  = X  + H
      END DO

      Y2 = (X+1.0D0)**2 - 0.5D0*EXP(X)  ! Exact solution
      WRITE(*,'(A,F10.6,A,F10.6)') '   y(2.0) RK4=', Y1,
     +    '  exact=', Y2
      WRITE(*,'(A,E12.4)') '   Error: ', ABS(Y1 - Y2)

C     ============================================================
C     6. STATISTICS ON A DATASET
C     ============================================================
      WRITE(*,*) ''
      WRITE(*,*) '6. DESCRIPTIVE STATISTICS'

      NDATA = 12
      DATA(1)  =  23.5D0
      DATA(2)  =  31.0D0
      DATA(3)  =  28.7D0
      DATA(4)  =  19.2D0
      DATA(5)  =  35.4D0
      DATA(6)  =  27.1D0
      DATA(7)  =  24.9D0
      DATA(8)  =  30.2D0
      DATA(9)  =  22.8D0
      DATA(10) =  29.5D0
      DATA(11) =  26.3D0
      DATA(12) =  33.7D0

      MEAN = 0.0D0
      DO I = 1, NDATA
          MEAN = MEAN + DATA(I)
      END DO
      MEAN = MEAN / NDATA

      VARIANCE = 0.0D0
      DO I = 1, NDATA
          VARIANCE = VARIANCE + (DATA(I) - MEAN)**2
      END DO
      VARIANCE = VARIANCE / (NDATA - 1)
      STDDEV = SQRT(VARIANCE)

      WRITE(*,'(A,F8.3)') '   Mean:     ', MEAN
      WRITE(*,'(A,F8.3)') '   Variance: ', VARIANCE
      WRITE(*,'(A,F8.3)') '   Std Dev:  ', STDDEV

      WRITE(*,*) ''
      WRITE(*,*) '>>> FORTRAN Numerical Methods complete!'
      STOP
      END

C     ============================================================
      DOUBLE PRECISION FUNCTION FUNC1(X)
      DOUBLE PRECISION X
      FUNC1 = X**3 - 2.0D0*X - 5.0D0
      RETURN
      END

      DOUBLE PRECISION FUNCTION DFUNC1(X)
      DOUBLE PRECISION X
      DFUNC1 = 3.0D0*X**2 - 2.0D0
      RETURN
      END

      DOUBLE PRECISION FUNCTION FUNC2(X)
      DOUBLE PRECISION X
      FUNC2 = SIN(X)
      RETURN
      END

C     ============================================================
      SUBROUTINE GAUSS_ELIM(A, SOL, N)
      INTEGER N
      DOUBLE PRECISION A(4,5), SOL(4)
      INTEGER I, J, K
      DOUBLE PRECISION FACTOR, SUM

      DO J = 1, N
          DO I = J+1, N
              FACTOR = A(I,J) / A(J,J)
              DO K = J, N+1
                  A(I,K) = A(I,K) - FACTOR * A(J,K)
              END DO
          END DO
      END DO

      SOL(N) = A(N,N+1) / A(N,N)
      DO I = N-1, 1, -1
          SUM = A(I,N+1)
          DO J = I+1, N
              SUM = SUM - A(I,J) * SOL(J)
          END DO
          SOL(I) = SUM / A(I,I)
      END DO
      RETURN
      END
