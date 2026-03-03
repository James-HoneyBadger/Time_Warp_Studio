\ =============================================
\  Forth Comprehensive Demo - Time Warp Studio
\ =============================================

\ --- Hello World ---
." ===== HELLO WORLD =====" CR
." Welcome to Forth!" CR
CR

\ --- Stack Operations ---
." ===== STACK OPS =====" CR
5 DUP . .          ( DUP: duplicate top )
CR
1 2 SWAP . .       ( SWAP: exchange top two )
CR
1 2 OVER . . .     ( OVER: copy second to top )
CR
1 2 3 ROT . . .    ( ROT: rotate third to top )
CR
CR

\ --- Arithmetic ---
." ===== ARITHMETIC =====" CR
3 4 + .  CR
10 3 - .  CR
6 7 * .  CR
20 4 / .  CR
17 5 MOD .  CR
5 NEGATE .  CR
-7 ABS .  CR
CR

\ --- Comparison ---
." ===== COMPARISON =====" CR
5 5 = .  CR
5 3 = .  CR
7 3 > .  CR
3 7 < .  CR
CR

\ --- Word Definitions ---
." ===== WORD DEFINITIONS =====" CR
: SQUARE DUP * ;
5 SQUARE .  CR
8 SQUARE .  CR

: CUBE DUP DUP * * ;
3 CUBE .  CR

: GREET ." Hello from a Forth word!" ;
GREET CR
CR

\ --- Variables and Constants ---
." ===== VARIABLES =====" CR
VARIABLE MYVAR
42 MYVAR !
MYVAR @ .  CR

99 MYVAR !
MYVAR @ .  CR

42 CONSTANT ANSWER
ANSWER .  CR
CR

\ --- Conditionals ---
." ===== CONDITIONALS =====" CR
: CHECK-SIGN
  DUP 0 > IF
    ." positive"
  ELSE
    DUP 0 < IF
      ." negative"
    ELSE
      ." zero"
    THEN
  THEN
  DROP
;
5 CHECK-SIGN CR
-3 CHECK-SIGN CR
0 CHECK-SIGN CR
CR

\ --- DO Loops ---
." ===== DO LOOPS =====" CR
: COUNT-UP 5 0 DO I . LOOP ;
COUNT-UP CR

: EVENS 10 0 DO I . 2 +LOOP ;
EVENS CR
CR

\ --- Emit / Character Output ---
." ===== CHARACTERS =====" CR
65 EMIT CR    ( prints 'A' )
72 EMIT 105 EMIT 33 EMIT CR   ( prints 'Hi!' )
CR

\ --- Stack Inspection ---
." ===== STACK DEPTH =====" CR
1 2 3 DEPTH .  CR
DROP DROP DROP
CR

\ --- Floating Point ---
." ===== FLOATING POINT =====" CR
3.14 F.  CR
2.5 3.5 F+ F.  CR
CR

\ --- Base Conversion ---
." ===== BASE CONVERSION =====" CR
HEX FF DECIMAL .  CR
CR

." ===== DONE =====" CR
