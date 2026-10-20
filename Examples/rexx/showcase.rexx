/* ============================================================ */
/* THE TEXT ANALYST                                             */
/* A REXX showcase: typeless values, concatenation, selection,   */
/* every kind of DO loop, PARSE, built-in functions, internal    */
/* subroutines and turtle graphics.                              */
/* ============================================================ */

SAY 'T H E   T E X T   A N A L Y S T'
SAY '----------------------------------------'
SAY ''

/* --- 1. Values, arithmetic and concatenation ---------------- */
sentence = 'the quick brown fox jumps over the lazy dog'
SAY 'The sentence is:'
SAY sentence
SAY 'Characters: ' LENGTH(sentence)
SAY 'Words: ' WORDS(sentence)
SAY 'Six times seven is ' 6 * 7
SAY 'Seven divided by two is ' 7 / 2
SAY 'Abuttal joins: ' 'Hello' 'World'
SAY 'Explicit join: ' 'Hello' || ' ' || 'World'
SAY ''

/* --- 2. PARSE splits a sentence ----------------------------- */
PARSE VAR sentence first second third rest
SAY 'First word: ' first
SAY 'Second word: ' second
SAY 'Third word: ' third
SAY 'The rest: ' rest
SAY ''

/* --- 3. Every kind of DO loop ------------------------------- */
SAY 'Counting by threes to ten:'
DO n = 1 TO 10 BY 3
  SAY n
END
SAY 'Words one at a time:'
DO word OVER sentence
  SAY word
END
count = 0
DO WHILE count < 3
  count = count + 1
END
SAY 'DO WHILE stopped at ' count
SAY 'Skipping the third and leaving at the fifth:'
DO i = 1 TO 6
  IF i = 3 THEN ITERATE
  IF i = 5 THEN LEAVE
  SAY i
END
SAY ''

/* --- 4. Selection ------------------------------------------- */
score = 87
IF score >= 90 THEN DO
  SAY 'Grade: A'
END
ELSE DO
  SAY 'Grade: B or lower'
END
SELECT
  WHEN score >= 85 THEN SAY 'SELECT says: strong pass'
  WHEN score >= 70 THEN SAY 'SELECT says: pass'
  OTHERWISE SAY 'SELECT says: keep going'
END
SAY ''

/* --- 5. Built-in functions ---------------------------------- */
SAY 'UPPER  :' UPPER(sentence)
SAY 'REVERSE:' REVERSE('stressed')
SAY 'SUBSTR :' SUBSTR(sentence, 5, 5)
SAY 'WORD 4 :' WORD(sentence, 4)
SAY 'LEFT 3 :' LEFT(sentence, 3)
SAY 'RIGHT 3:' RIGHT(sentence, 3)
SAY 'STRIP  :' STRIP('   padded   ')
SAY 'COPIES :' COPIES('ab', 3)
SAY 'INDEX  :' INDEX(sentence, 'brown')
SAY 'MAX/MIN:' MAX(3, 7) MIN(3, 7)
SAY 'TRUNC  :' TRUNC(3.7)
SAY 'ABS    :' ABS(-5)
SAY 'SIGN   :' SIGN(-5)
SAY 'DATATYPE:' DATATYPE('42')
SAY ''

/* --- 6. An internal subroutine ------------------------------ */
CALL SUMMARISE
SAY 'The subroutine reported:' RESULT
SAY ''

/* --- 7. Turtle graphics ------------------------------------- */
SAY 'Drawing a square spiral, then a star.'
DO step = 1 TO 12
  FORWARD step * 6
  RIGHT 90
END
PENUP
HOME
PENDOWN
RIGHT 18
DO point = 1 TO 5
  FORWARD 60
  RIGHT 144
END
HOME
SAY 'Drawing complete.'
EXIT

SUMMARISE:
  total = 0
  DO word OVER sentence
    total = total + LENGTH(word)
  END
  RETURN total
