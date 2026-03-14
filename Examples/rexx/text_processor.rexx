/* REXX Text Processor — String manipulation toolkit
   Demonstrates: PARSE, built-in functions, stem variables, I/O */

SAY "╔══════════════════════════════════════╗"
SAY "║   📝 REXX Text Processor            ║"
SAY "╚══════════════════════════════════════╝"
SAY ""

/* ── Word frequency counter ── */
SAY "━━━ Word Frequency Analysis ━━━"
text = "the quick brown fox jumps over the lazy dog the fox"
SAY "Input:" text
SAY ""

/* Count words using stem variable */
word_count. = 0
num_words = WORDS(text)
DO i = 1 TO num_words
    w = TRANSLATE(WORD(text, i))  /* uppercase */
    word_count.w = word_count.w + 1
END

/* Display unique words and counts */
seen. = 0
DO i = 1 TO num_words
    w = TRANSLATE(WORD(text, i))
    IF seen.w = 0 THEN DO
        SAY "  " LEFT(w, 10) ":" word_count.w "time(s)"
        seen.w = 1
    END
END
SAY ""

/* ── Caesar cipher ── */
SAY "━━━ Caesar Cipher ━━━"
plain = "HELLO WORLD"
shift = 3
cipher = ""
DO i = 1 TO LENGTH(plain)
    ch = SUBSTR(plain, i, 1)
    IF DATATYPE(ch, 'U') THEN DO
        pos = C2D(ch) - C2D('A')
        new_pos = (pos + shift) // 26
        cipher = cipher || D2C(new_pos + C2D('A'))
    END
    ELSE
        cipher = cipher || ch
END
SAY "  Plain:   " plain
SAY "  Shift:   " shift
SAY "  Cipher:  " cipher

/* Decrypt */
decoded = ""
DO i = 1 TO LENGTH(cipher)
    ch = SUBSTR(cipher, i, 1)
    IF DATATYPE(ch, 'U') THEN DO
        pos = C2D(ch) - C2D('A')
        new_pos = (pos - shift + 26) // 26
        decoded = decoded || D2C(new_pos + C2D('A'))
    END
    ELSE
        decoded = decoded || ch
END
SAY "  Decoded: " decoded
SAY ""

/* ── String statistics ── */
SAY "━━━ String Statistics ━━━"
sample = "The Time Warp Studio is an educational IDE for 24 languages!"
SAY "  Text:" sample
SAY "  Length:     " LENGTH(sample)
SAY "  Words:      " WORDS(sample)
SAY "  Uppercase:  " TRANSLATE(sample)
SAY "  Reversed:   " REVERSE(sample)
SAY "  Left 20:    " LEFT(sample, 20)
SAY "  Right 20:   " RIGHT(sample, 20)

/* Count vowels */
vowels = 0
DO i = 1 TO LENGTH(sample)
    ch = TRANSLATE(SUBSTR(sample, i, 1))
    IF POS(ch, 'AEIOU') > 0 THEN vowels = vowels + 1
END
SAY "  Vowels:     " vowels
SAY ""

/* ── PARSE demonstration ── */
SAY "━━━ PARSE Template Demo ━━━"
date_str = "2025-03-15 14:30:00"
PARSE VAR date_str year '-' month '-' day ' ' hour ':' min ':' sec
SAY "  Input:  " date_str
SAY "  Year:   " year
SAY "  Month:  " month
SAY "  Day:    " day
SAY "  Hour:   " hour
SAY "  Minute: " min
SAY "  Second: " sec
SAY ""

/* ── ROT13 ── */
SAY "━━━ ROT13 Encoder ━━━"
msg = "REXX IS POWERFUL"
rot13 = ""
DO i = 1 TO LENGTH(msg)
    ch = SUBSTR(msg, i, 1)
    IF DATATYPE(ch, 'U') THEN DO
        pos = C2D(ch) - C2D('A')
        new_pos = (pos + 13) // 26
        rot13 = rot13 || D2C(new_pos + C2D('A'))
    END
    ELSE
        rot13 = rot13 || ch
END
SAY "  Original:" msg
SAY "  ROT13:   " rot13
SAY ""

SAY "✅ Text processing complete!"
