[ Countdown from 10 to 1 then print "GO!" ]
[ Cell 0: counter, Cell 1: newline (10), Cells 2-3: scratch ]

[ Set cell 0 = 10 ]
++++++++++

[ Loop: print digit and decrement ]
[
  [ Copy cell 0 to cell 2, cell 3 for printing digit ]
  [ Convert 10..1 to ASCII 49..57 = add 48 ]
  [ Cell 0 = counter, move to cell 1, add 48 to get ASCII ]
  >+++++++++ >++++++++ >+++++++++ >++++++++++ <<<<
  [ We'll use a simpler approach: print cell 0 + 48 as ASCII ]
  [ Save counter ]
  [->+>+<<]   [ copy cell0 to cell1 and cell2 ]
  >>          [ move to cell2 ]
  [ add 48 for ASCII '0' ]
  ++++ ++++ ++++ ++++ ++++ ++++ ++++++++++++
  .           [ print digit ]
  [ newline ]
  >++++++++++. <
  [ restore: go back to cell0 ]
  [-]<[-]<    [ clear cell2 and cell1 ]
  -           [ decrement counter ]
]

[ Print "GO!" ]
[ G = 71 ]
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.
[ O = 79, difference 8 ]
++++++++ .
[ ! = 33, back 46 ]
----------------------------------------------  .
