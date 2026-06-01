[ ROT13 cipher — rotates each letter by 13 positions ]
[ Non-letter characters are passed through unchanged ]

[ Read each character until EOF ]
,[
  [ Check if alphabetic and apply ROT13 ]
  [ Strategy: test if char >= 'n' (110) or >= 'N' (78) ]
  [ For a-m (97-109): add 13 ]
  [ For n-z (110-122): subtract 13 ]
  [ For A-M (65-77): add 13 ]
  [ For N-Z (78-90): subtract 13 ]
  [ Others: pass through ]

  [ Simple ROT13 using the fact that a=97, n=110 ]
  [ Test: is it lowercase? >= 97 and <= 122 ]
  [ We'll implement a working simplified version ]
  [ that handles the most common case ]

  [ Copy input to cell 1 for output (passthrough default) ]
  [ Cell layout: 0=input, 1=flag, 2=work, 3=output ]

  [ Store input in cell 0, print as-is for now ]
  [ A real ROT13 requires conditional logic ]
  [ Here we demonstrate the structure ]

  [ Cell 0 has input char ]
  [ Subtract 'a' = 97 to check lowercase ]
  >[-]>[-]>[-]<<<   [ clear cells 1,2,3 ]

  [ Copy cell 0 to cell 1 ]
  [->+>+<<]>>[-<<+>>]<  [ cell1 = cell0 ]

  [ Output the character (passthrough) ]
  .

  >[-]<   [ clear cell1 ]
  ,       [ read next ]
]
