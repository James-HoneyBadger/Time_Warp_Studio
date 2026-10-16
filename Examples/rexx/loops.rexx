/* REXX looping constructs */
SAY 'DO i = 1 TO 5'
DO i = 1 TO 5
  SAY 'square of' i 'is' i * i
END

SAY 'DO countdown BY -2'
DO n = 10 TO 2 BY -2
  SAY n
END

SAY 'DO WHILE'
fib = 0
next = 1
DO WHILE next < 50
  SAY next
  temp = fib + next
  fib = next
  next = temp
END

SAY 'DO OVER a list of words'
DO word OVER 'alpha beta gamma'
  SAY word
END
