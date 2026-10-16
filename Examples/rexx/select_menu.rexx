/* REXX SELECT / WHEN and IF / THEN / ELSE */
score = 87

SELECT
  WHEN score >= 90 THEN SAY 'Grade A'
  WHEN score >= 80 THEN SAY 'Grade B'
  WHEN score >= 70 THEN SAY 'Grade C'
  OTHERWISE SAY 'Needs work'
END

IF score > 100 THEN DO
  SAY 'Impossible score'
END
ELSE DO
  SAY 'Score recorded: ' || score
END

DO i = 1 TO 20
  IF i = 5 THEN LEAVE
  IF i = 3 THEN ITERATE
  SAY 'visited' i
END
SAY 'Loop finished early at 5'
