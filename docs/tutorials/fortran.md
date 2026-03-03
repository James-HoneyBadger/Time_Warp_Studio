# Fortran Programming Tutorial

Fortran (Formula Translation) was created by John Backus at IBM in 1957 — the first high-level programming language. It remains the gold standard for numerical and scientific computing.

## Hello World

```fortran
PROGRAM hello
    PRINT *, "Hello from Fortran!"
    PRINT *, "Welcome to Time Warp Studio"
END PROGRAM hello
```

## Variables and Types

```fortran
PROGRAM types_demo
    IMPLICIT NONE

    INTEGER   :: count = 0
    REAL      :: pi    = 3.14159
    REAL(8)   :: e     = 2.71828182845904523536d0
    CHARACTER(LEN=20) :: name = "Alice"
    LOGICAL   :: flag  = .TRUE.

    PRINT *, "Integer:", count
    PRINT *, "Real:   ", pi
    PRINT *, "Double: ", e
    PRINT *, "String: ", name
    PRINT *, "Logical:", flag
END PROGRAM types_demo
```

## Arithmetic

```fortran
PROGRAM arithmetic
    IMPLICIT NONE
    REAL :: a = 10.0, b = 3.0

    PRINT *, "Add:     ", a + b
    PRINT *, "Sub:     ", a - b
    PRINT *, "Mul:     ", a * b
    PRINT *, "Div:     ", a / b
    PRINT *, "Power:   ", a ** b
    PRINT *, "Abs:     ", ABS(-42.0)
    PRINT *, "Sqrt:    ", SQRT(144.0)
    PRINT *, "Floor:   ", FLOOR(3.7)
    PRINT *, "Ceiling: ", CEILING(3.2)
    PRINT *, "Mod:     ", MOD(17, 5)
END PROGRAM arithmetic
```

## Control Flow

```fortran
PROGRAM control
    IMPLICIT NONE
    INTEGER :: i, n, score

    ! IF / ELSE IF / END IF
    score = 85
    IF (score >= 90) THEN
        PRINT *, "Grade A"
    ELSE IF (score >= 80) THEN
        PRINT *, "Grade B"
    ELSE
        PRINT *, "Grade C or below"
    END IF

    ! DO loop (definite)
    DO i = 1, 10
        WRITE(*, "(I3)", ADVANCE='NO') i
    END DO
    PRINT *

    ! DO WHILE
    n = 1
    DO WHILE (n <= 1024)
        WRITE(*, "(I6)", ADVANCE='NO') n
        n = n * 2
    END DO
    PRINT *

    ! SELECT CASE
    SELECT CASE (score / 10)
        CASE (10, 9)
            PRINT *, "A"
        CASE (8)
            PRINT *, "B"
        CASE (7)
            PRINT *, "C"
        CASE DEFAULT
            PRINT *, "Below C"
    END SELECT
END PROGRAM control
```

## Arrays

```fortran
PROGRAM arrays
    IMPLICIT NONE
    INTEGER, DIMENSION(5) :: a = [1, 2, 3, 4, 5]
    REAL, DIMENSION(3,3)  :: matrix
    INTEGER :: i, j

    ! Array operations (whole-array)
    PRINT *, a
    PRINT *, a * 2
    PRINT *, SUM(a), PRODUCT(a), MAXVAL(a), MINVAL(a)

    ! 2D array
    matrix = 0.0
    DO i = 1, 3
        DO j = 1, 3
            IF (i == j) matrix(i,j) = 1.0    ! identity
        END DO
    END DO
    DO i = 1, 3
        PRINT *, matrix(i, :)
    END DO
END PROGRAM arrays
```

## Subroutines and Functions

```fortran
PROGRAM math_demo
    IMPLICIT NONE
    REAL :: r

    r = circle_area(5.0)
    PRINT *, "Area of circle r=5:", r
    CALL print_table(5)
    CONTAINS

    PURE FUNCTION circle_area(r) RESULT(area)
        REAL, INTENT(IN) :: r
        REAL :: area
        area = 3.14159265 * r * r
    END FUNCTION circle_area

    SUBROUTINE print_table(n)
        INTEGER, INTENT(IN) :: n
        INTEGER :: i
        DO i = 1, n
            PRINT *, i, i*i, i*i*i
        END DO
    END SUBROUTINE print_table

END PROGRAM math_demo
```

## Further Reading

- [Examples/fortran/](../Examples/fortran/) — 10 Fortran example programs
- [Language Guide: Fortran](LANGUAGE_GUIDE.md#fortran)
