      PROGRAM DEMO
C     =============================================
C      FORTRAN Comprehensive Demo - Time Warp Studio
C     =============================================

C     --- Variables ---
      INTEGER X, Y, I, N, FACT
      REAL PI, AREA, RADIUS
      CHARACTER*20 NAME

      X = 10
      Y = 3
      PI = 3.14159
      NAME = 'FORTRAN'

C     --- Hello World ---
      WRITE(*,*) '===== HELLO WORLD ====='
      WRITE(*,*) 'Welcome to FORTRAN!'
      WRITE(*,*) ' '

C     --- Variables and Types ---
      WRITE(*,*) '===== VARIABLES ====='
      WRITE(*,*) X
      WRITE(*,*) Y
      WRITE(*,*) PI
      WRITE(*,*) NAME
      WRITE(*,*) ' '

C     --- Arithmetic ---
      WRITE(*,*) '===== ARITHMETIC ====='
      WRITE(*,*) X + Y
      WRITE(*,*) X - Y
      WRITE(*,*) X * Y
      WRITE(*,*) X / Y
      WRITE(*,*) 2 ** 10
      WRITE(*,*) ' '

C     --- Conditionals ---
      WRITE(*,*) '===== CONDITIONALS ====='
      IF (X .GT. 5) THEN
        WRITE(*,*) 'X is greater than 5'
      ELSE
        WRITE(*,*) 'X is not greater than 5'
      ENDIF

      IF (Y .EQ. 3) THEN
        WRITE(*,*) 'Y equals 3'
      ENDIF
      WRITE(*,*) ' '

C     --- DO Loop ---
      WRITE(*,*) '===== DO LOOP ====='
      DO 10 I = 1, 5
        WRITE(*,*) I
   10 CONTINUE
      WRITE(*,*) ' '

C     --- Computed Values ---
      WRITE(*,*) '===== COMPUTATION ====='
      RADIUS = 5.0
      AREA = PI * RADIUS * RADIUS
      WRITE(*,*) 'Radius:', RADIUS
      WRITE(*,*) 'Area:', AREA
      WRITE(*,*) ' '

C     --- Factorial ---
      WRITE(*,*) '===== FACTORIAL ====='
      FACT = 1
      DO 20 I = 1, 5
        FACT = FACT * I
   20 CONTINUE
      WRITE(*,*) '5! =', FACT
      WRITE(*,*) ' '

C     --- Subroutine ---
      WRITE(*,*) '===== SUBROUTINE ====='
      CALL GREET
      WRITE(*,*) ' '

      WRITE(*,*) '===== DONE ====='
      STOP
      END

      SUBROUTINE GREET
      WRITE(*,*) 'Hello from subroutine!'
      RETURN
      END
