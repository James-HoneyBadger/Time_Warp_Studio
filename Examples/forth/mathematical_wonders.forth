\ ============================================================
\ MATHEMATICAL WONDERS — Forth Language Showcase
\ Primes * Fibonacci * Pi * Towers of Hanoi * Turtle Art
\ Time Warp Studio — Forth Language Demo
\ ============================================================

\ ===== UTILITY WORDS =====

: SPACE5   5 SPACES ;
: LINE     CR 60 0 DO ASCII - EMIT LOOP CR ;
: HEADER   ( addr n -- ) CR LINE TYPE CR LINE ;

\ Absolute value
: ABS-INT  ( n -- |n| ) DUP 0 < IF NEGATE THEN ;

\ Max of two
: MAX2  ( a b -- max ) 2DUP < IF SWAP THEN DROP ;

\ ===== PRIME SIEVE =====
\ Uses a VARIABLE array to sieve primes up to 200

CREATE SIEVE 201 ALLOT
VARIABLE SIEVE-LIMIT

: SIEVE-CLEAR   ( -- )
  201 0 DO  1 SIEVE I + C!  LOOP
  0 SIEVE C!
  0 SIEVE 1 + C! ;

: SIEVE-MARK   ( p -- )
  DUP * BEGIN DUP SIEVE-LIMIT @ <= WHILE
    0 OVER SIEVE + C!
    OVER +
  REPEAT
  2DROP ;

: BUILD-SIEVE   ( limit -- )
  SIEVE-LIMIT !
  SIEVE-CLEAR
  2 BEGIN DUP DUP * SIEVE-LIMIT @ <= WHILE
    SIEVE OVER + C@ IF SIEVE-MARK ELSE DROP THEN
    1+
  REPEAT DROP ;

: PRINT-PRIMES   ( limit -- )
  BUILD-SIEVE
  CR ." Primes up to " SIEVE-LIMIT @ . ." :" CR
  VARIABLE PCNT  0 PCNT !
  2 BEGIN DUP SIEVE-LIMIT @ <= WHILE
    SIEVE OVER + C@ IF
      DUP . SPACE
      1 PCNT +!
      PCNT @ 10 MOD 0= IF CR THEN
    THEN
    1+
  REPEAT DROP
  CR ." Total primes found: " PCNT @ . CR ;

\ ===== FIBONACCI =====

: FIB   ( n -- fib(n) )
  DUP 1 <= IF EXIT THEN
  DUP
  1-  RECURSE
  SWAP
  2-  RECURSE
  + ;

: FIB-TABLE   ( -- )
  CR ." Fibonacci sequence F(0)..F(15):" CR
  16 0 DO
    ." F(" I . ." ) = " I FIB . CR
  LOOP ;

\ Efficient iterative Fibonacci (no recursion)
: FIB-ITER   ( n -- fib(n) )
  DUP 1 <= IF EXIT THEN
  0 1 ROT 1- 0 DO
    OVER + SWAP
  LOOP
  SWAP DROP ;

\ Golden ratio via consecutive terms
: GOLDEN-RATIO   ( -- )
  CR ." Golden ratio approximation F(n+1)/F(n):" CR
  20 2 DO
    I FIB-ITER S>F
    I 1- FIB-ITER S>F
    FDUP F0= IF FDROP FDROP ELSE
      F/ CR ."   F(" I . ." )/F(" I 1- . ." ) = " F. THEN
  LOOP ;

\ ===== PI APPROXIMATION (LEIBNIZ SERIES) =====

\ pi/4 = 1 - 1/3 + 1/5 - 1/7 + 1/9 - ...
: LEIBNIZ-PI   ( n -- )
  CR ." Pi via Leibniz series (more terms = more accuracy):" CR
  VARIABLE TERMS
  TERMS !
  1.0E0 FVARIABLE PI-ACCUM
  1.0E0 PI-ACCUM F!
  VARIABLE TERM-N
  3 TERM-N !
  VARIABLE SIGN
  -1 SIGN !
  TERMS @ 1 DO
    1.0E0 TERM-N @ S>F F/
    SIGN @ 0 < IF FNEGATE THEN
    PI-ACCUM F@ F+ PI-ACCUM F!
    -1 SIGN @ * SIGN !
    2 TERM-N +!
  LOOP
  PI-ACCUM F@ 4.0E0 F*
  ." pi ~ " F. CR
  ." Actual: 3.14159265358979" CR ;

\ ===== TOWERS OF HANOI =====

VARIABLE HANOI-MOVES

: HANOI   ( n from to via -- )
  OVER 0 = IF
    2DROP 2DROP EXIT
  THEN
  4DUP ( n from to via | n from to via )
  ROT  ( n from via | n from to via -> n from via to )
  SWAP ( n from to | --> rearrange for recursive call )
  \ This is complex, let's use a simpler approach
  2DROP 2DROP DROP ;

\ Cleaner Towers of Hanoi using named pegs
: HANOI2   ( n from to via -- )
  ROT DUP 0 = IF
    DROP
    CR ."   Move disk from peg " . ."  to peg " . ."  (via " . ." )"
    1 HANOI-MOVES +!
  ELSE
    4DUP 1- ROT ROT SWAP HANOI2  \ n-1 from via to
    CR ."   Move disk " . ."  from peg " . ."  to peg " .
    1 HANOI-MOVES +!
    1- SWAP ROT HANOI2            \ n-1 via to from
  THEN ;

: HANOI-DEMO   ( n -- )
  0 HANOI-MOVES !
  CR ." Towers of Hanoi with " DUP . ." disks:" CR
  ." (Pegs: 1=Source, 2=Target, 3=Auxiliary)" CR
  DUP 4 > IF
    CR ." [Showing first few moves for " . ." disks]" CR
    ." Total moves required = 2^n - 1 = " 3 . CR
  ELSE
    1 2 3 HANOI2
    CR ." Total moves: " HANOI-MOVES @ . CR
    ." Formula: 2^" DUP . ." - 1 = " 1 SWAP 0 DO 2 * LOOP 1- . CR
  THEN ;

\ ===== STACK OPERATIONS SHOWCASE =====

: STACK-DEMO   ( -- )
  CR ." Stack manipulation showcase:" CR
  CR ." Initial stack: 1 2 3 4 5" CR
  1 2 3 4 5
  ." DUP (copy top):      " DUP . CR
  ." DROP (discard top):  " DROP . . . . CR
  2 3 4 5
  ." SWAP (swap top 2):   " SWAP . . ."  (then " . . ." )" CR
  1 2 3
  ." OVER (copy 2nd):     " OVER . . . CR
  1 2 3 4
  ." ROT (rotate top 3):  " ROT . . . ."  (then " . ." )" CR ;

\ ===== TURTLE STAR PATTERN =====

: STAR-ARM   ( len -- )
  FD SETPENWIDTH 2 ;

: DRAW-STAR  ( -- )
  HOME CLEAN
  SETPENCOLOR 0 255 200
  36 0 DO
    150 FD
    170 RT
    70 FD
    170 RT
  LOOP ;

: SPIRAL-STAR   ( -- )
  HOME CLEAN
  SETPENCOLOR 255 180 0
  SETPENWIDTH 2
  200 0 DO
    I 1+ FD
    91 RT
  LOOP ;

: COLOR-BURST   ( -- )
  HOME CLEAN
  12 0 DO
    I 20 * 0 SETPENCOLOR
    SETPENWIDTH 3
    160 FD
    HOME
    30 RT
  LOOP ;

\ ===== MATHEMATICAL FACTS =====

: MATH-FACTS   ( -- )
  CR ." Mathematical Properties Explored:" CR
  CR ." Perfect Numbers (sum of divisors = itself):" CR
  500 2 DO
    0 \ accumulate sum
    I 1- 2 DO
      J I MOD 0= IF J + THEN
    LOOP
    1 +   \ add 1 (always a divisor)
    I = IF ."   " I . ." is perfect" CR THEN
  LOOP
  CR ." Triangular Numbers T(n) = n*(n+1)/2:" CR
  ." T(1..12): "
  12 1 DO I I 1+ * 2 / . SPACE LOOP CR
  CR ." Harmonic Series H(n) = 1 + 1/2 + 1/3...:" CR
  1.0E0 FVARIABLE HARMONIC  1.0E0 HARMONIC F!
  15 2 DO
    1.0E0 I S>F F/ HARMONIC F@ F+ HARMONIC F!
    ." H(" I . ." ) = " HARMONIC F@ F. CR
  LOOP ;

\ ===== MAIN PROGRAM =====

CR
." ============================================================" CR
." MATHEMATICAL WONDERS — Forth Stack-Based Computing" CR
." Primes * Fibonacci * Pi * Hanoi * Visual Art" CR
." ============================================================" CR

CR ." [ 1 ] PRIME SIEVE (Sieve of Eratosthenes, limit=100)" CR
100 PRINT-PRIMES

CR ." [ 2 ] FIBONACCI SEQUENCE (Recursive)" CR
FIB-TABLE

CR ." [ 3 ] GOLDEN RATIO CONVERGENCE" CR
GOLDEN-RATIO

CR ." [ 4 ] PI APPROXIMATION (Leibniz, 1000 terms)" CR
1000 LEIBNIZ-PI

CR ." [ 5 ] TOWERS OF HANOI (4 disks)" CR
4 HANOI-DEMO

CR ." [ 6 ] MATHEMATICAL PROPERTIES" CR
MATH-FACTS

CR ." [ 7 ] TURTLE GRAPHICS — Spiral Star" CR
SPIRAL-STAR

CR ." [ 8 ] TURTLE GRAPHICS — Color Burst" CR
COLOR-BURST

CR
." ============================================================" CR
." Forth Showcase Complete!" CR
." Stack-based computing at its most elegant!" CR
." ============================================================" CR
