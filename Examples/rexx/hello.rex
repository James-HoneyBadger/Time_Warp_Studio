/* =============================================
    REXX Comprehensive Demo - Time Warp Studio
   ============================================= */

/* --- Hello World --- */
SAY "===== HELLO WORLD ====="
SAY "Welcome to REXX!"
SAY ""

/* --- Variables --- */
SAY "===== VARIABLES ====="
x = 10
y = 3
name = "REXX"
SAY x
SAY y
SAY name
SAY ""

/* --- Arithmetic --- */
SAY "===== ARITHMETIC ====="
SAY x + y
SAY x - y
SAY x * y
SAY x / y
SAY x // y
SAY x % y
SAY 2 ** 10
SAY ""

/* --- String Builtins --- */
SAY "===== STRINGS ====="
SAY LENGTH("Hello")
SAY SUBSTR("Hello World", 7, 5)
SAY REVERSE("Hello")
SAY COPIES("ab", 3)
SAY UPPER("hello")
SAY WORD("Hello World Foo", 2)
SAY WORDS("Hello World Foo")
SAY LEFT("Hello", 3)
SAY RIGHT("Hello", 3)
SAY POS("lo", "Hello")
SAY ""

/* --- Conditionals --- */
SAY "===== CONDITIONALS ====="
IF x > 5 THEN
  SAY "x is greater than 5"
END

IF y = 3 THEN
  SAY "y equals 3"
ELSE
  SAY "y does not equal 3"
END
SAY ""

/* --- DO Loop --- */
SAY "===== DO LOOP ====="
DO i = 1 TO 5
  SAY i
END
SAY ""

/* --- DO WHILE --- */
SAY "===== DO WHILE ====="
n = 1
DO WHILE n <= 3
  SAY n
  n = n + 1
END
SAY ""

/* --- DO UNTIL --- */
SAY "===== DO UNTIL ====="
m = 1
DO UNTIL m > 3
  SAY m
  m = m + 1
END
SAY ""

/* --- DO FOREVER with LEAVE --- */
SAY "===== DO FOREVER ====="
count = 0
DO FOREVER
  IF count >= 3 THEN LEAVE
  SAY count
  count = count + 1
END
SAY ""

/* --- Numeric Builtins --- */
SAY "===== NUMERIC ====="
SAY ABS(-5)
SAY MAX(3, 7)
SAY MIN(3, 7)
SAY TRUNC(3.7)
SAY ""

/* --- Procedures --- */
SAY "===== PROCEDURES ====="
CALL greet
SAY ""

SAY "===== DONE ====="
EXIT

greet:
  SAY "Hello from procedure!"
  RETURN
