/* REXX Subroutines and Functions */

/* Call a subroutine */
CALL Greet 'World'
CALL Greet 'REXX'

/* Function returning a value */
result = CALL Factorial 6
SAY '6! =' RESULT

/* Recursive function */
fib10 = CALL Fibonacci 10
SAY 'Fibonacci(10) =' RESULT

/* Multiple arguments */
CALL Stats 4 7 2 9 1

EXIT

/* --- Subroutine definitions --- */

Greet:
  who = ARG1
  SAY 'Hello,' who || '!'
  RETURN

Factorial:
  n = ARG1
  IF n <= 1 THEN RETURN 1
  prev = CALL Factorial n - 1
  RETURN n * RESULT

Fibonacci:
  n = ARG1
  IF n <= 1 THEN RETURN n
  a = CALL Fibonacci n - 1
  v1 = RESULT
  b = CALL Fibonacci n - 2
  RETURN v1 + RESULT

Stats:
  total = ARG1 + ARG2 + ARG3 + ARG4 + ARG5
  avg = total / 5
  SAY 'Sum:' total
  SAY 'Average:' avg
  RETURN
