/* REXX subroutines with CALL and RETURN */
SAY 'Program starts'
CALL banner 'Report'
CALL banner 'Details'
CALL factorial 6
SAY 'Factorial of 6 is' RESULT
SAY 'Program ends'
EXIT

banner:
  SAY '----------' ARG(1) '----------'
  RETURN

factorial:
  n = ARG(1)
  result = 1
  DO i = 2 TO n
    result = result * i
  END
  RETURN result
