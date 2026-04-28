C HELLO WORLD --- FORTRAN 77 Language Demo
C Welcome to FORTRAN programming!
C Time Warp Studio --- FORTRAN Language Demo

      PROGRAM HELLO
C ===== HELLO WORLD =====
      WRITE(*,*) 'HELLO WORLD'
      WRITE(*,*) 'Welcome to FORTRAN'

C ===== VARIABLES =====
      WRITE(*,*) 'VARIABLES'
      INTEGER I, J
      REAL X, Y
      I = 42
      J = 100
      X = 3.14
      Y = 2.718
      WRITE(*,*) 'Integer I = ', I
      WRITE(*,*) 'Integer J = ', J
      WRITE(*,*) 'Real X = ', X
      WRITE(*,*) 'Real Y = ', Y

C ===== ARITHMETIC =====
      WRITE(*,*) 'ARITHMETIC'
      INTEGER SUM, DIFF, PROD
      SUM = I + J
      DIFF = J - I
      PROD = I * 2
      WRITE(*,*) 'Sum = ', SUM
      WRITE(*,*) 'Difference = ', DIFF
      WRITE(*,*) 'Product = ', PROD

C ===== SUBROUTINE =====
      CALL GREET
      WRITE(*,*) 'DONE'
      END

      SUBROUTINE GREET
      WRITE(*,*) 'SUBROUTINE'
      WRITE(*,*) 'Hello from subroutine'
      RETURN
      END
