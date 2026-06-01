[ Addition calculator: reads two single-digit numbers and prints sum ]
[ Input: two ASCII digits (e.g. "3" and "5") ]
[ Output: the ASCII sum character ]

[ Read first digit, subtract 48 to get numeric value ]
,
++++++++++++++++++++++++++++++++++++++++++++++++++++[-]  [ scratch ]
,                  [ read first digit into cell 0 ]
>                  [ move to cell 1 ]
,                  [ read second digit into cell 1 ]

[ Convert both from ASCII to numeric ]
< -------------------------------- [ subtract 48 from cell 0 ]
> -------------------------------- [ subtract 48 from cell 1 ]

[ Add cell 1 into cell 0 ]
[-<+>]

[ Now cell 0 has the sum, add 48 to print as ASCII digit ]
< ++++++++++++++++++++++++++++++++++++++++++++++++  [ add 48 ]
.                  [ print result ]

[ Print newline ]
>++++++++++.
