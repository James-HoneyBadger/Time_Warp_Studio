Fibonacci sequence generator in Brainfuck
Prints first 12 Fibonacci numbers as ASCII

Initialize: cell0=count cell1=a=0 cell2=b=1 cell3=temp cell4=newline

Set counter to 12
++++++++++++

Print header message
>+++++++++[<++++++++>-]<+.>++++[<++++++++>-]<.+++.-------.+++++++.
[-]

Set newline
>>>>++++++++++<<<<

Main loop: print fibonacci numbers
[
  >>.                Print a (as ASCII char offset)
  >>.                Print newline

  Compute next: temp = a + b
  <<[->>+<<]         Move a to temp (cell3)
  >[-<+>>+<]         Copy b to a and temp
  >>[-<<+>>]         Move temp back to b
  <<<<

  -                  Decrement counter
]

Print done
>[-]+++++++++[<++++++++++>-]<++.[-]++++++++++.
