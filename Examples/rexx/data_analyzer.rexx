/* ============================================================ */
/* DATA ANALYZER — REXX Language Showcase                       */
/* Word Frequency * Text Stats * Numeric Analysis * Transforms  */
/* Time Warp Studio — REXX Language Demo                        */
/* ============================================================ */

SAY "============================================================"
SAY "  DATA ANALYZER — REXX String Processing Showcase"
SAY "  Word Frequency | Statistics | Text Transform | Cipher"
SAY "============================================================"
SAY ""

/* ===== SECTION 1: WORD FREQUENCY ANALYSIS ===== */
SAY "[ 1 ] WORD FREQUENCY ANALYSIS"
SAY ""

text = "To be or not to be that is the question whether tis nobler",
     "in the mind to suffer the slings and arrows of outrageous",
     "fortune or to take arms against a sea of troubles"

/* Clean and split into words */
DO i = 1 TO LENGTH(text)
    c = SUBSTR(text, i, 1)
    IF c = "," THEN text = OVERLAY(" ", text, i)
END

text = TRANSLATE(text, COPIES(" ", 26), "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

/* Count words */
total_words = 0
unique_words = 0
DROP word_list. word_count.

DO WHILE text \= ""
    PARSE VAR text word text
    word = STRIP(word)
    IF word = "" THEN ITERATE
    word = TRANSLATE(word)   /* uppercase */
    total_words = total_words + 1
    IF SYMBOL("word_count." || word) = "LIT" THEN DO
        word_count.word = 0
        unique_words = unique_words + 1
        word_list.unique_words = word
    END
    word_count.word = word_count.word + 1
END

SAY "  Text: Shakespeare's Hamlet (excerpt)"
SAY "  Total words: " total_words
SAY "  Unique words:" unique_words
SAY ""
SAY "  Word frequencies (all):"

/* Find max count for sorting display */
max_count = 0
DO i = 1 TO unique_words
    w = word_list.i
    IF word_count.w > max_count THEN max_count = word_count.w
END

/* Print by frequency (simple selection sort display) */
shown = ""
DO cnt = max_count TO 1 BY -1
    DO i = 1 TO unique_words
        w = word_list.i
        IF word_count.w = cnt THEN DO
            IF POS("|" || w || "|", shown) = 0 THEN DO
                bar = COPIES("█", cnt)
                SAY "    " LEFT(w, 12) " " cnt "  " bar
                shown = shown || "|" || w || "|"
            END
        END
    END
END

SAY ""

/* ===== SECTION 2: TEXT STATISTICS ===== */
SAY "[ 2 ] TEXT STATISTICS ENGINE"
SAY ""

long_text = "The quick brown fox jumps over the lazy dog. ",
           "Pack my box with five dozen liquor jugs. ",
           "How valiantly the brave knight fought the fearsome dragon. ",
           "Programming in REXX is elegant and powerful!"

SAY "  Analysing:"
SAY "  " long_text
SAY ""

total_chars = LENGTH(long_text)
letters = 0
vowels = 0
consonants = 0
digits = 0
spaces = 0
sentences = 0
upper_count = 0
lower_count = 0

vowel_set = "AEIOU"

DO i = 1 TO total_chars
    c = SUBSTR(long_text, i, 1)
    cu = TRANSLATE(c)
    IF cu >= "A" & cu <= "Z" THEN DO
        letters = letters + 1
        IF POS(cu, vowel_set) > 0 THEN vowels = vowels + 1
        ELSE consonants = consonants + 1
        IF c >= "A" & c <= "Z" THEN upper_count = upper_count + 1
        ELSE lower_count = lower_count + 1
    END
    IF c >= "0" & c <= "9" THEN digits = digits + 1
    IF c = " " THEN spaces = spaces + 1
    IF c = "." | c = "!" | c = "?" THEN sentences = sentences + 1
END

words_approx = spaces + 1

SAY "  Total characters: " total_chars
SAY "  Letters:          " letters
SAY "  Vowels:           " vowels
SAY "  Consonants:       " consonants
SAY "  Digits:           " digits
SAY "  Spaces:           " spaces
SAY "  Uppercase:        " upper_count
SAY "  Lowercase:        " lower_count
SAY "  Sentences:        " sentences
SAY "  Approx words:     " words_approx
SAY "  Avg word length:  " FORMAT(letters / words_approx, 4, 2)
SAY ""

/* ===== SECTION 3: NUMERIC DATA ANALYSIS ===== */
SAY "[ 3 ] NUMERIC DATA ANALYSIS"
SAY ""

/* Dataset: annual rainfall in mm for 20 cities */
data = "450 1200 890 320 1450 670 2100 540 980 1100",
      "1750 280 1380 760 490 1620 430 1050 850 1900"

/* Parse into array */
n = 0
DO WHILE data \= ""
    PARSE VAR data val data
    val = STRIP(val)
    IF val \= "" THEN DO
        n = n + 1
        arr.n = val
    END
END

/* Sort (bubble sort) */
DO i = 1 TO n - 1
    DO j = 1 TO n - i
        IF arr.j > arr.(j+1) THEN DO
            tmp = arr.j
            arr.j = arr.(j+1)
            arr.(j+1) = tmp
        END
    END
END

/* Calculate stats */
total = 0
DO i = 1 TO n
    total = total + arr.i
END
mean_val = total / n

/* Variance */
sum_sq = 0
DO i = 1 TO n
    diff = arr.i - mean_val
    sum_sq = sum_sq + diff * diff
END
variance = sum_sq / n
std_dev = SQRT(variance)

/* Median */
IF n // 2 = 0 THEN
    median_val = (arr.(n/2) + arr.(n/2+1)) / 2
ELSE
    median_val = arr.((n+1)/2)

/* IQR */
q1 = arr.(n/4 + 1)
q3 = arr.(3*n/4 + 1)
iqr = q3 - q1

SAY "  Dataset: annual rainfall (mm) for" n "cities"
SAY "  Min:      " arr.1 " mm"
SAY "  Max:      " arr.n " mm"
SAY "  Range:    " arr.n - arr.1 " mm"
SAY "  Mean:     " FORMAT(mean_val, 6, 1) " mm"
SAY "  Median:   " FORMAT(median_val, 6, 1) " mm"
SAY "  Std Dev:  " FORMAT(std_dev, 6, 1) " mm"
SAY "  Q1:       " q1 " mm"
SAY "  Q3:       " q3 " mm"
SAY "  IQR:      " iqr " mm"
SAY ""
SAY "  Sorted: "
line = "  "
DO i = 1 TO n
    line = line || arr.i || " "
END
SAY line
SAY ""

/* Histogram (4 buckets) */
bucket_size = (arr.n - arr.1) / 4 + 1
SAY "  Distribution histogram:"
DO b = 0 TO 3
    lo = arr.1 + b * bucket_size
    hi = lo + bucket_size - 1
    cnt = 0
    DO i = 1 TO n
        IF arr.i >= lo & arr.i <= hi THEN cnt = cnt + 1
    END
    bar = COPIES("█", cnt * 2)
    SAY "    " RIGHT(lo, 5) "-" RIGHT(hi, 5) " | " bar " (" cnt ")"
END
SAY ""

/* ===== SECTION 4: STRING TRANSFORMATION TOOLKIT ===== */
SAY "[ 4 ] STRING TRANSFORMATION TOOLKIT"
SAY ""

sample = "  Hello, World! This is REXX v5.0  "

SAY "  Original: '" || sample || "'"
SAY "  STRIP:    '" || STRIP(sample) || "'"
SAY "  UPPER:    '" || TRANSLATE(STRIP(sample)) || "'"
SAY "  LOWER:    '" || TRANSLATE(STRIP(sample),,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",,
        "abcdefghijklmnopqrstuvwxyz") || "'"
SAY "  REVERSE:  '" || REVERSE(STRIP(sample)) || "'"
SAY "  LENGTH:   " LENGTH(STRIP(sample))
SAY "  LEFT(15): '" || LEFT(STRIP(sample), 15) || "'"
SAY "  RIGHT(15):'" || RIGHT(STRIP(sample), 15) || "'"

word_s = STRIP(sample)
SAY "  WORDS:    " WORDS(word_s)
SAY "  WORD(2):  '" || WORD(word_s, 2) || "'"
SAY "  WORD(4):  '" || WORD(word_s, 4) || "'"
SAY ""

/* Pattern extraction with PARSE */
date_str = "2026-04-15 14:30:00"
PARSE VAR date_str year "-" month "-" day " " hour ":" min ":" sec
SAY "  Parsing date: '" || date_str || "'"
SAY "    Year:" year "  Month:" month "  Day:" day
SAY "    Hour:" hour "  Min:" min "  Sec:" sec
SAY ""

/* ===== SECTION 5: ROT13 CIPHER IN REXX ===== */
SAY "[ 5 ] ROT13 CIPHER"
SAY ""

CALL rot13_subr "Hello, World!"
CALL rot13_subr "The Quick Brown Fox"
CALL rot13_subr "REXX is powerful!"
SAY ""

/* ===== SECTION 6: FIBONACCI & PRIMES ===== */
SAY "[ 6 ] FIBONACCI SEQUENCE & PRIMES"
SAY ""

SAY "  Fibonacci (F0..F15):"
a = 0; b = 1
line = "  "
DO i = 0 TO 15
    line = line || a || " "
    tmp = a + b; a = b; b = tmp
END
SAY line
SAY ""

SAY "  Primes up to 100 (trial division):"
line = "  "
cnt = 0
DO n_p = 2 TO 100
    is_prime = 1
    DO d = 2 TO n_p - 1
        IF n_p // d = 0 THEN DO
            is_prime = 0
            LEAVE
        END
    END
    IF is_prime THEN DO
        line = line || n_p || " "
        cnt = cnt + 1
    END
END
SAY line
SAY "  Count:" cnt
SAY ""

SAY "============================================================"
SAY "  REXX Data Analyzer Complete!"
SAY "  PARSE VAR | String ops | DO loops | Subroutines | Math"
SAY "============================================================"
EXIT

/* ===== SUBROUTINES ===== */

rot13_subr:
    PARSE ARG instr
    outstr = ""
    DO i = 1 TO LENGTH(instr)
        c = SUBSTR(instr, i, 1)
        IF c >= "A" & c <= "Z" THEN DO
            n = XRANGE("A", "Z")
            pos_c = POS(c, n)
            outstr = outstr || SUBSTR(n, (pos_c + 12 - 1) // 26 + 1, 1)
        END
        ELSE IF c >= "a" & c <= "z" THEN DO
            n = XRANGE("a", "z")
            pos_c = POS(c, n)
            outstr = outstr || SUBSTR(n, (pos_c + 12 - 1) // 26 + 1, 1)
        END
        ELSE outstr = outstr || c
    END
    SAY "  ROT13: '" || instr || "' -> '" || outstr || "'"
    RETURN

SQRT: PROCEDURE
    /* Newton's method square root */
    PARSE ARG x
    IF x <= 0 THEN RETURN 0
    guess = x / 2
    DO 50
        guess = (guess + x / guess) / 2
    END
    RETURN guess
