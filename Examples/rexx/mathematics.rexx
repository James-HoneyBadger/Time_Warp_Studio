/* REXX - Mathematical Functions */
/* Demonstrates REXX numeric operations and built-in math */

SAY '=== REXX Mathematics ==='
SAY ''

/* Basic arithmetic */
a = 15; b = 4
SAY 'a = 'a'  b = 'b
SAY 'a + b = '  (a + b)
SAY 'a - b = '  (a - b)
SAY 'a * b = '  (a * b)
SAY 'a / b = '  (a / b)
SAY 'a % b = '  (a % b)    /* integer divide */
SAY 'a // b = ' (a // b)   /* remainder */
SAY 'a ** b = ' (a ** b)   /* power */
SAY ''

/* REXX numeric precision */
NUMERIC DIGITS 20
SAY '=== High Precision ==='
SAY 'PI  ≈ 3.14159265358979323846'
pi  = 3.14159265358979323846
SAY 'Circle area (r=5): ' FORMAT(pi * 5 * 5, , 4)
SAY ''

/* Fibonacci */
SAY '=== Fibonacci Sequence ==='
a = 0; b = 1
fibs = a b
DO 12
    c = a + b
    fibs = fibs c
    a = b; b = c
END
SAY fibs
SAY ''

/* GCD via Euclidean algorithm */
SAY '=== GCD Examples ==='
DO WHILE line \= ''
    /* Manual test pairs */
END

nums = '48 18 100 75 252 105 17 13'
DO WHILE nums \= ''
    PARSE VAR nums x y nums
    orig_x = x; orig_y = y
    DO WHILE y \= 0
        temp = y
        y = x // y
        x = temp
    END
    SAY '  GCD('orig_x', 'orig_y') = 'x
END

/* Prime sieve */
SAY ''
SAY '=== Primes up to 50 ==='
limit = 50
DO i = 2 TO limit
    composite.i = 0
END
DO i = 2 TO limit
    IF composite.i = 0 THEN DO
        j = i * i
        DO WHILE j <= limit
            composite.j = 1
            j = j + i
        END
    END
END
primes = ''
DO i = 2 TO limit
    IF composite.i = 0 THEN primes = primes i
END
SAY STRIP(primes)
