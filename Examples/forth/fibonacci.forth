\ Fibonacci sequence in Forth
\ Demonstrates loops, stack juggling and integer ratios

\ Iterative Fibonacci: fib(0)=0 fib(1)=1 fib(2)=1 ...
: fib ( n -- fib_n )
  0 1 rot            \ a b n
  0 ?do
    swap over +      \ b a+b
  loop
  drop
;

\ Print the first N Fibonacci numbers on one line
: print-fibs ( n -- )
  ." First " dup . ." Fibonacci numbers:" cr
  0 do
    i fib .
  loop
  cr
;

\ Golden ratio approximation: fib(i+1) * 1000 / fib(i)
: golden-ratio ( -- )
  ." Golden ratio x1000 via fib(i+1)/fib(i):" cr
  16 5 do
    ." i=" i . ." ratio=" i 1+ fib 1000 * i fib / . cr
  loop
;

15 print-fibs
golden-ratio
