\ ============================================================
\  THE STACK WORKSHOP
\  A Forth showcase: the stack, definitions, control structures,
\  variables, memory, recursion, and turtle geometry.
\ ============================================================

." --- 1. THE STACK IS THE ARGUMENT LIST" CR
2 3 + . CR
10 4 - . CR
6 7 * . CR
100 7 /MOD . . CR
1 2 3 .S CR

." --- 2. DUP, SWAP AND OVER" CR
7 DUP * . CR
1 2 SWAP . . CR
1 2 OVER + + . CR

." --- 3. A WORD IS A VERB YOU INVENT" CR
: SQUARE ( n -- n*n ) DUP * ;
9 SQUARE . CR
: CUBE ( n -- n*n*n ) DUP DUP * * ;
4 CUBE . CR

." --- 4. DECISIONS" CR
: SIGN ( n -- ) DUP 0< IF ." negative" ELSE
                     DUP 0> IF ." positive" ELSE ." zero" THEN THEN DROP ;
-5 SIGN SPACE 0 SIGN SPACE 5 SIGN CR

." --- 5. LOOPS" CR
: COUNTDOWN ( n -- ) BEGIN DUP . 1- DUP 0= UNTIL DROP ;
4 COUNTDOWN CR
: TABLES ( -- ) 4 1 DO 3 1 DO I J * . LOOP LOOP ;
TABLES CR
: THREES ( -- ) 10 1 DO I . 3 +LOOP ;
THREES CR

." --- 6. VARIABLES AND MEMORY" CR
VARIABLE TOTAL
0 TOTAL !
: ADD-UP ( n -- ) 1+ 1 DO I TOTAL +! LOOP ;
10 ADD-UP
." 1 to 10 sums to " TOTAL @ . CR
VARIABLE BYTE
65 BYTE C!
." BYTE holds the letter " BYTE C@ EMIT CR

." --- 7. RECURSION" CR
: FACT ( n -- n! ) DUP 1 > IF DUP 1- RECURSE * THEN ;
6 FACT . CR
: FIB ( n -- fib ) DUP 2 < IF EXIT THEN DUP 1- RECURSE SWAP 2 - RECURSE + ;
10 FIB . CR

." --- 8. TURTLE GEOMETRY" CR
CS
: POLY8 ( n -- ) 8 0 DO DUP FD 45 RT LOOP DROP ;
2 SETPC
150 POLY8
: STAR ( n -- ) 5 0 DO DUP FD 144 RT LOOP DROP ;
12 SETPC
PU HOME 135 RT 120 FD PD
55 STAR
: SPIRAL ( -- ) 16 0 DO I 3 * FD 60 RT LOOP ;
1 SETPC
PU HOME PD
SPIRAL
PU HOME PD

." --- 9. DONE" CR
." Stack: " .S CR
