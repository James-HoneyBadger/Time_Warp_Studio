( ARITHMETIC IN FORTH )
( Reverse Polish Notation (RPN) examples )
( In Forth, operators come AFTER the operands )

." === Basic Arithmetic ===" CR

." 2 + 3 = " 
2 3 + . CR

." 10 - 4 = "
10 4 - . CR

." 10 * 5 = "
10 5 * . CR

." 20 / 4 = "
20 4 / . CR

." === Complex Expressions ===" CR

." (20 + 5) * 2 = "
20 5 + 2 * . CR

." (100 - 20) / 4 = "
100 20 - 4 / . CR

." === Powers and Squares ===" CR

." 3 squared = "
3 3 * . CR

." 5 squared = "
5 5 * . CR
