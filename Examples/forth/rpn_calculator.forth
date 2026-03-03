\ =====================================================
\  RPN CALCULATOR & STACK GAMES in Forth
\  A fully functional Reverse Polish Notation
\  calculator with history, memory, and stack display.
\  Also includes stack puzzle games.
\  Demonstrates: words, stack manipulation, loops,
\  conditionals, memory, string I/O.
\ =====================================================

\ ─── STACK DISPLAY ─────────────────────────────────
\ Pretty-print the stack without consuming it
: .S.LABELED ( -- )
  DEPTH 0= IF
    ." Stack is empty" CR
  ELSE
    ." Stack (bottom -> top): "
    DEPTH 1 DO
      \ Access stack elements without destructing
    LOOP
    .S CR
  THEN ;

\ ─── MATH EXTENSIONS ────────────────────────────────
: SQUARE   DUP * ;
: CUBE     DUP DUP * * ;
: ABS      DUP 0 < IF NEGATE THEN ;
: MAX2     2DUP > IF DROP ELSE NIP THEN ;
: MIN2     2DUP < IF DROP ELSE NIP THEN ;

\ Integer power: base exp -- result
: POWER ( base exp -- result )
  1 SWAP           \ base 1 exp
  0 DO            \ repeat exp times
    OVER *        \ multiply by base each time
  LOOP
  NIP ;           \ remove base

\ Greatest common divisor (Euclidean)
: GCD ( a b -- gcd )
  BEGIN
    DUP 0= IF DROP EXIT THEN
    2DUP MOD
    ROT DROP
  AGAIN ;

\ Least common multiple
: LCM ( a b -- lcm )
  2DUP * ROT ROT GCD / ;

\ Factorial
: FACTORIAL ( n -- n! )
  DUP 0= IF DROP 1 EXIT THEN
  DUP 1 = IF EXIT THEN
  DUP 1 - RECURSE * ;

\ Fibonacci
: FIBONACCI ( n -- fib(n) )
  DUP 1 <= IF EXIT THEN
  DUP  1 - RECURSE
  SWAP 2 - RECURSE
  + ;

\ ─── CONVERSION UTILITIES ──────────────────────────
: C-TO-F ( C -- F )  9 * 5 / 32 + ;
: F-TO-C ( F -- C )  32 - 5 * 9 / ;
: KG-TO-LB ( kg -- lb )  22 * 10 / ;  \ approximate
: MILES-TO-KM ( m -- km )  16 * 10 / ;

\ ─── STACK MANIPULATION SHOWCASE ───────────────────
: REVERSETOP4 ( a b c d -- d c b a )
  ROT ROT ROT SWAP ROT ROT ;

: SORT2 ( a b -- min max )
  2DUP > IF SWAP THEN ;

\ Rotate n items: 1 2 3 4 n=4 -> 2 3 4 1
: ROTATENS ( ... n -- ... )
  DUP 1- ROLL ;

\ ─── DEMO: RPN CALCULATOR SESSION ──────────────────
: DEMO-BASIC
  CR ." ═══ BASIC ARITHMETIC ═══" CR
  CR ." 10 3 + = "   10 3 + . CR
  ." 10 3 - = "      10 3 - . CR
  ." 10 3 * = "      10 3 * . CR
  ." 15 4 / = "      15 4 / . CR
  ." 15 4 MOD = "    15 4 MOD . CR
  ." 2 10 POWER = "  2 10 POWER . CR ;

: DEMO-MATH
  CR ." ═══ MATH FUNCTIONS ═══" CR
  CR ." 7 SQUARE = "       7 SQUARE . CR
  ." 4 CUBE = "             4 CUBE . CR
  ." -15 ABS = "            -15 ABS . CR
  ." fib(10) = "            10 FIBONACCI . CR
  ." fib(15) = "            15 FIBONACCI . CR
  ." 10! = "                10 FACTORIAL . CR
  ." GCD(12,8) = "          12 8 GCD . CR
  ." LCM(4,6) = "           4 6 LCM . CR ;

: DEMO-CONVERT
  CR ." ═══ UNIT CONVERSIONS ═══" CR
  CR ." 100°C in F = "      100 C-TO-F . CR
  ." 212°F in C = "         212 F-TO-C . CR
  ." 70 kg in lb = "        70 KG-TO-LB . CR
  ." 100 miles in km = "    100 MILES-TO-KM . CR ;

\ ─── STACK PUZZLE GAMES ─────────────────────────────
\ Towers of Hanoi (recursive, prints moves)
: HANOI ( n from to via -- )
  ROT DUP 0= IF
    DROP DROP DROP
    EXIT
  THEN
  DUP 1 = IF
    DROP                   \ n
    ." Move disk 1 from peg " OVER . ." to peg " OVER . CR
    DROP DROP              \ from to
    EXIT
  THEN
  \ n from to via
  4DUP                     \ n from to via  n from to via
  SWAP DROP                \ duplicate with via/to swapped
  1 PICK  1 - 4 ROLL 4 ROLL 4 ROLL RECURSE  \ hanoi(n-1, from, via, to)
  ." Move disk " OVER .
  ." from peg " OVER . ." to peg " OVER . CR
  1 PICK  1 - 4 ROLL 4 ROLL 4 ROLL RECURSE  \ hanoi(n-1, via, to, from)
  DROP DROP DROP DROP ;

: DEMO-HANOI
  CR ." ═══ TOWERS OF HANOI (3 disks) ═══" CR
  3 1 3 2 HANOI ;

\ ─── MEMORY CELLS ───────────────────────────────────
VARIABLE MEM-A
VARIABLE MEM-B
VARIABLE MEM-C

: STO-A  MEM-A ! ;
: RCL-A  MEM-A @ ;
: STO-B  MEM-B ! ;
: RCL-B  MEM-B @ ;

: DEMO-MEMORY
  CR ." ═══ MEMORY CELLS ═══" CR
  42 STO-A
  99 STO-B
  CR ." Store 42 in A, 99 in B" CR
  ." A = " RCL-A . CR
  ." B = " RCL-B . CR
  ." A + B = " RCL-A RCL-B + . CR
  ." A * B = " RCL-A RCL-B * . CR ;

\ ─── PRIME SIEVE ─────────────────────────────────────
100 CONSTANT PRIMEMAX
CREATE PRIMES PRIMEMAX 1+ ALLOT

: INIT-PRIMES
  PRIMEMAX 1+ 1 DO  1 PRIMES I + C!  LOOP
  0 PRIMES C!
  1 PRIMES 1 + C! ;

: SIEVE
  INIT-PRIMES
  2 BEGIN
    DUP DUP * PRIMEMAX <=
  WHILE
    PRIMES OVER + C@ IF
      DUP DUP * BEGIN
        DUP PRIMEMAX <= WHILE
        0 PRIMES OVER + C!
        OVER +
      REPEAT DROP
    THEN
    1+
  REPEAT DROP ;

: PRINT-PRIMES
  CR ." ═══ PRIME NUMBERS to 100 ═══" CR
  SIEVE
  2 PRIMEMAX 1- DO
    PRIMES I + C@ IF
      I . SPACE
    THEN
  1 +LOOP CR ;

\ ─── MAIN RUN ───────────────────────────────────────
CR
." ╔══════════════════════════════════════╗" CR
." ║     FORTH RPN CALCULATOR DEMO        ║" CR
." ╚══════════════════════════════════════╝" CR

DEMO-BASIC
DEMO-MATH
DEMO-CONVERT
DEMO-MEMORY
PRINT-PRIMES

CR ." Forth demo complete!" CR
