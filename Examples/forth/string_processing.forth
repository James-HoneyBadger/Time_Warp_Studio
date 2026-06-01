\ Forth String Processing
\ Demonstrates counted strings, parsing, and text manipulation

\ -- Count vowels in a string --
: is-vowel? ( char -- flag )
  dup [char] a = swap
  dup [char] e = swap
  dup [char] i = swap
  dup [char] o = swap
  [char] u =
  or or or or
;

: count-vowels ( addr len -- count )
  0 -rot          \ count addr len
  0 do
    over i + c@   \ count addr char
    32 or         \ lowercase
    is-vowel? if
      rot 1+ -rot
    then
  loop
  drop drop
;

\ -- Reverse a string --
: reverse-str ( addr len -- )
  2dup + 1-      \ addr len addr+len-1
  rot            \ len addr addr+end
  swap           \ len addr+end addr
  0 do           \ for i=0 to len/2
    2dup          \ end start end start
    c@ swap c@   \ end start c@end c@start
    rot c! swap c!  \ swap chars
    swap 1+      \ increment start
    rot 1-       \ decrement end
    rot
  2 +loop
  drop drop
;

\ -- Demo --
." === Forth String Processing ===" cr cr

: demo-count-vowels
  s" Hello World" 2dup type
  ."  — vowels: "
  count-vowels . cr
;

: demo-words
  ." Words in Forth: counted strings" cr
  ." s\" text\" pushes addr and len" cr
  ." Common string words:" cr
  ."   TYPE   — print string" cr
  ."   COUNT  — get length" cr
  ."   CMOVE  — copy memory" cr
  ."   COMPARE — compare strings" cr
;

demo-count-vowels

s" racecar" dup type
." " 2 / reverse-str
cr

demo-words
