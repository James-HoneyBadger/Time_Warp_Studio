/* REXX Loops and Control Flow */

/* Counted DO loop */
SAY '--- Counted loop ---'
DO i = 1 TO 5
  SAY 'Count:' i
END

/* DO loop with step */
SAY '--- Step loop (evens) ---'
DO n = 2 TO 10 BY 2
  SAY n
END

/* DO WHILE */
SAY '--- Fibonacci sequence ---'
a = 0
b = 1
DO WHILE b <= 100
  SAY b
  temp = b
  b = a + b
  a = temp
END

/* SELECT / WHEN */
SAY '--- Grade classifier ---'
score = 85
SELECT
  WHEN score >= 90 THEN SAY 'Grade: A'
  WHEN score >= 80 THEN SAY 'Grade: B'
  WHEN score >= 70 THEN SAY 'Grade: C'
  OTHERWISE SAY 'Grade: D or below'
END

/* Nested loops with LEAVE */
SAY '--- Search example ---'
target = 7
DO row = 1 TO 3
  DO col = 1 TO 3
    val = row * col
    IF val = target THEN DO
      SAY 'Found' target 'at row' row 'col' col
      LEAVE row
    END
  END
END
