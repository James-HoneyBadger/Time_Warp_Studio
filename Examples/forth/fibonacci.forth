\ Fibonacci sequence in Forth
\ Demonstrates recursion via iterative stack manipulation

\ Iterative Fibonacci
: fib ( n -- fib_n )
  dup 1 <= if drop 1 exit then
  0 1 rot            \ stack: prev curr n
  1 - 0 ?do
    over + swap drop \ sum, keep running total
  loop
  drop
;

\ Print first N Fibonacci numbers
: print-fibs ( n -- )
  ." First " dup . ." Fibonacci numbers:" cr
  0 do
    i fib . ."  "
  loop
  cr
;

\ Golden ratio approximation via consecutive Fibonacci numbers
: golden-ratio ( n -- )
  ." Golden ratio via fib(n+1)/fib(n):" cr
  5 15 do
    i fib i 1+ fib    \ fib(i) fib(i+1)
    swap              \ fib(i+1) fib(i)
    ." fib(" i . .") / fib(" i . ."): "
    100 * /           \ integer approximation * 100
    ." ~" . ." /100" cr
  loop
;

15 print-fibs
