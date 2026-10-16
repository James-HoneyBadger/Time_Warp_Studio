/* REXX string handling: concatenation, PARSE and built-ins */
name = 'Time Warp'
SAY 'Hello' name                 /* abuttal concatenation */
SAY 'Hello' || ' ' || name       /* explicit || concatenation */
SAY 'Length :' LENGTH(name)
SAY 'Upper  :' UPPER(name)
SAY 'Lower  :' LOWER(name)
SAY 'Reverse:' REVERSE(name)
SAY 'Words  :' WORDS('one two three')

line = 'red green blue'
PARSE VAR line first second third
SAY 'Second colour is' second
SAY 'Substring      :' SUBSTR('abcdefgh', 3, 4)
