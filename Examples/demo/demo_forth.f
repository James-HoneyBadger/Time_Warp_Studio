\ Time Warp Studio - Forth Demo

\ Stack-based Programming

\ Basic arithmetic
2 3 + .    \ Outputs: 5
CR
5 3 - .    \ Outputs: 2
CR

\ Stack operations
5 DUP .    \ Duplicate 5 on stack
CR

\ Simple loops
10 0 DO I . LOOP
CR

\ Definitions
: SQUARE DUP * ;

5 SQUARE .    \ Outputs: 25
CR

: DOUBLE DUP + ;

10 DOUBLE .   \ Outputs: 20
CR

\ Simple output
.\" Forth Demo Complete!\" CR
