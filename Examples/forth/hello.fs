\ hello.fs -- the first Forth program.
\
\ A Forth program is words separated by spaces, and a word takes what it needs from
\ the stack. None of this is syntax: `.` prints the top of the stack, `CR` is a
\ newline, and `." ..."` prints the text between the quotes.

: FACT ( n -- n! )
  DUP 1 > IF DUP 1- RECURSE * THEN ;

: COUNTUP ( n -- )
  0 DO I . LOOP CR ;

: STARS ( n -- )
  0 DO 42 EMIT LOOP CR ;

." *** HB-1 FORTH ***" CR
." The eleventh language, in a socket." CR
CR

." Two and three make " 2 3 + . CR
." Seven squared is " 7 DUP * . CR
." Six factorial is " 6 FACT . CR
CR

." Counted to ten: " 10 COUNTUP
." Nine stars:      " 9 STARS
CR
." Stack at the end: " .S CR
