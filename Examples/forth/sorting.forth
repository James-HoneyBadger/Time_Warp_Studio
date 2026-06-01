\ Sorting algorithms in Forth
\ Bubble sort and selection sort on arrays

\ Simple array stored in variables
VARIABLE arr-size
CREATE arr 20 CELLS ALLOT

\ Store values into array
: arr! ( val idx -- )  cells arr + ! ;
: arr@ ( idx -- val )  cells arr + @ ;

\ Fill array with values
: fill-array ( -- )
  8 arr-size !
  64 0 arr!
  25 1 arr!
  12 2 arr!
  90 3 arr!
  3  4 arr!
  77 5 arr!
  44 6 arr!
  18 7 arr!
;

\ Print the array
: print-array ( -- )
  arr-size @ 0 do
    i arr@ . ."  "
  loop cr
;

\ Swap two array elements
: swap-arr ( i j -- )
  over arr@    ( i j val-i )
  over arr@    ( i j val-i val-j )
  rot          ( i val-i val-j j )
  arr!         ( i val-i )
  swap arr!
;

\ Bubble sort
: bubble-sort ( -- )
  arr-size @ 1- 0 do
    arr-size @ 1- 0 do
      i arr@ i 1+ arr@ > if
        i i 1+ swap-arr
      then
    loop
  loop
;

\ Demo
." === Forth Sorting Demo ===" cr cr

fill-array
." Unsorted: " print-array

bubble-sort
." Sorted:   " print-array
