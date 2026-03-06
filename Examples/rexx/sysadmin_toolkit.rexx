/* ================================================================
   REXX SYSTEM ADMINISTRATION TOOLKIT
   Demonstrates REXX string handling, arithmetic, loops,
   conditionals, built-in functions, and subroutines.
   ================================================================ */

SAY "======================================"
SAY "  REXX SYSTEM ADMINISTRATION TOOLKIT"
SAY "======================================"
SAY ""

/* ── 1. String Processing ──────────────────────────────────────── */
SAY "--- 1. STRING PROCESSING ---"

greeting = "Hello, World!"
SAY greeting
SAY LENGTH(greeting)
SAY REVERSE(greeting)
SAY SUBSTR(greeting, 1, 5)
SAY COPIES("=-", 20)
SAY ""

/* ── 2. Arithmetic ─────────────────────────────────────────────── */
SAY "--- 2. ARITHMETIC ---"

a = 256
b = 17
SAY a + b
SAY a - b
SAY a * b
SAY a / b
SAY a // b
SAY a % b
SAY 2 ** 8
SAY ""

/* ── 3. DO Loop — Fibonacci ────────────────────────────────────── */
SAY "--- 3. FIBONACCI SEQUENCE ---"

f1 = 0
f2 = 1
DO i = 1 TO 12
  SAY f1
  f3 = f1 + f2
  f1 = f2
  f2 = f3
END
SAY ""

/* ── 4. DO Loop — Factorial ────────────────────────────────────── */
SAY "--- 4. FACTORIALS ---"

fact = 1
DO n = 1 TO 10
  fact = fact * n
  SAY fact
END
SAY ""

/* ── 5. Conditionals ───────────────────────────────────────────── */
SAY "--- 5. CONDITIONALS ---"

temp = 72
IF temp > 90 THEN SAY "Hot"
IF temp > 70 THEN SAY "Warm"
IF temp > 50 THEN SAY "Mild"
IF temp <= 50 THEN SAY "Cool"
SAY ""

/* ── 6. String Functions ───────────────────────────────────────── */
SAY "--- 6. STRING FUNCTIONS ---"

phrase = "The quick brown fox jumps over the lazy dog"
SAY LENGTH(phrase)
SAY WORDS(phrase)
SAY WORD(phrase, 4)
SAY WORD(phrase, 1)
SAY LEFT(phrase, 15)
SAY RIGHT(phrase, 8)
SAY POS("fox", phrase)
SAY ""

/* ── 7. Powers of Two ──────────────────────────────────────────── */
SAY "--- 7. POWERS OF TWO ---"

DO p = 0 TO 12
  SAY 2 ** p
END
SAY ""

/* ── 8. Subroutine Call ────────────────────────────────────────── */
SAY "--- 8. SUBROUTINE DEMO ---"

CALL show_banner
CALL compute_stats
SAY ""

SAY "======================================"
SAY "  TOOLKIT COMPLETE"
SAY "======================================"
EXIT

/* ── Subroutines ───────────────────────────────────────────────── */
show_banner:
  SAY "  ** Subroutine called successfully **"
  RETURN

compute_stats:
  SAY "  Computing sum 1..100:"
  total = 0
  DO k = 1 TO 100
    total = total + k
  END
  SAY total
  SAY "  Computing sum of squares 1..10:"
  sq = 0
  DO k = 1 TO 10
    sq = sq + k * k
  END
  SAY sq
  RETURN
