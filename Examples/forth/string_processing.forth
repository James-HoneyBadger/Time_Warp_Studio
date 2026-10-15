\ Forth String Processing
\ Demonstrates S" strings, character access with C@, and loops over text

\ -- Is a character a vowel? --
: is-vowel? ( char -- flag )
  dup [char] a = swap
  dup [char] e = swap
  dup [char] i = swap
  dup [char] o = swap
  [char] u =
  or or or or
;

\ -- Count vowels in a string --
: count-vowels ( addr len -- count )
  0 swap 0 ?do          \ addr count
    over i + c@         \ addr count char
    32 or               \ fold to lowercase
    is-vowel? if 1+ then
  loop
  nip
;

\ -- Print a string backwards --
: type-reversed ( addr len -- )
  begin dup while
    1- 2dup + c@ emit
  repeat
  2drop
;

\ -- Count characters equal to a target character --
variable target
: count-char ( addr len char -- count )
  target !
  0 swap 0 ?do          \ addr count
    over i + c@ target @ = if 1+ then
  loop
  nip
;

." === Forth String Processing ===" cr cr

s" Hello World" 2dup type ."  has vowels: " count-vowels . cr
s" racecar" 2dup type ."  reversed: " type-reversed cr
s" Time Warp Studio" 2dup type ."  reversed: " type-reversed cr
s" mississippi" 2dup type ."  has letter s: " [char] s count-char . cr
cr
." Common string words:" cr
."   S( text )  pushes address and length" cr
."   TYPE       prints a string" cr
."   C@         fetches one character" cr
."   EMIT       prints one character" cr
