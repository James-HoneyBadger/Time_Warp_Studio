/* ============================================================ */
/* NUMBER SYSTEMS — REXX Language Showcase                      */
/* Binary, octal, hex, Roman numerals, base conversion          */
/* Time Warp Studio — REXX Language Demo                        */
/* ============================================================ */

SAY "============================================================"
SAY "  NUMBER SYSTEMS — REXX Showcase"
SAY "  Base Conversion, Roman Numerals, Bit Operations"
SAY "============================================================"
SAY ""

/* ============================================================ */
/* SECTION 1: DECIMAL TO BINARY                                 */
/* ============================================================ */

SAY "SECTION 1: DECIMAL TO BINARY"
SAY "------------------------------------------------------------"

DO n = 0 TO 20
    CALL DecToBin n
    SAY "  " || n || " = " || result
END

SAY ""
SAY "  Powers of 2:"
DO k = 0 TO 16
    p = 2 ** k
    CALL DecToBin p
    SAY "  2^" || k || " = " || p || " = " || result || "b"
END
SAY ""

/* ============================================================ */
/* SECTION 2: DECIMAL TO OCTAL AND HEX                         */
/* ============================================================ */

SAY "SECTION 2: DECIMAL TO OCTAL AND HEXADECIMAL"
SAY "------------------------------------------------------------"

SAY "  Dec     Octal     Hex"
SAY "  ------  --------  --------"

DO n = 0 TO 255 BY 16
    CALL DecToBase n, 8
    oct = result
    CALL DecToBase n, 16
    hex = result
    SAY "  " || RIGHT(n, 4) || "    " || RIGHT(oct, 6) || "o    " || RIGHT(hex, 4) || "h"
END

SAY ""
SAY "  Notable values:"
SAY "  255 (8 bits all 1s) = " || D2X(255) || " hex = " || D2O(255) || " oct"
SAY "  1024 = " || D2X(1024) || " hex = " || D2O(1024) || " oct"
SAY "  65535 (16 bits all 1s) = " || D2X(65535) || " hex"
SAY "  16777215 (24 bits all 1s) = " || D2X(16777215) || " hex"
SAY ""

/* ============================================================ */
/* SECTION 3: BASE CONVERSION MATRIX                           */
/* ============================================================ */

SAY "SECTION 3: BASE CONVERSION TABLE (select numbers)"
SAY "------------------------------------------------------------"

SAY "  Dec   Binary             Octal    Hex    Base36"
SAY "  ---   -----------------  -------  -----  ------"

special = "0 1 2 8 10 16 32 42 64 100 127 128 255 256 1000 1024 4096 65536"
DO i = 1 TO WORDS(special)
    n = WORD(special, i)
    CALL DecToBin n
    bin = result
    CALL DecToBase n, 36
    b36 = result
    SAY "  " || RIGHT(n, 5) || "  " || RIGHT(bin, 17) || ,
        "  " || RIGHT(D2O(n), 7) || "  " || RIGHT(D2X(n), 5) || ,
        "  " || b36
END
SAY ""

/* ============================================================ */
/* SECTION 4: ROMAN NUMERALS                                    */
/* ============================================================ */

SAY "SECTION 4: ROMAN NUMERALS"
SAY "------------------------------------------------------------"

SAY "  Decimal -> Roman:"
numbers = "1 4 5 9 10 14 40 49 50 90 99 100 400 499 500 900 999 1000 1776 1999 2024 3999"
DO i = 1 TO WORDS(numbers)
    n = WORD(numbers, i)
    CALL ToRoman n
    SAY "  " || RIGHT(n, 5) || " = " || result
END

SAY ""
SAY "  Roman -> Decimal:"
romans = "I IV V IX X XL L XC C CD D CM M MCMXCIX MMXXIV MMMCMXCIX"
DO i = 1 TO WORDS(romans)
    r = WORD(romans, i)
    CALL FromRoman r
    SAY "  " || LEFT(r, 10) || " = " || result
END
SAY ""

/* ============================================================ */
/* SECTION 5: BITWISE OPERATIONS                               */
/* ============================================================ */

SAY "SECTION 5: BITWISE OPERATIONS"
SAY "------------------------------------------------------------"

SAY "  AND, OR, XOR (decimal; using binary representation):"
SAY ""

DO i = 1 TO 5
    a = i * 17 + 3
    b = i * 11 + 7
    CALL BitwiseAND a, b
    andResult = result
    CALL BitwiseOR a, b
    orResult = result
    CALL BitwiseXOR a, b
    xorResult = result
    CALL BitwiseNOT a, 8
    notResult = result

    CALL DecToBin a
    binA = result
    CALL DecToBin b
    binB = result

    SAY "  A=" || RIGHT(a, 3) || " (" || RIGHT(binA,8) || "b) "
    SAY "  B=" || RIGHT(b, 3) || " (" || RIGHT(binB,8) || "b) "
    SAY "  AND=" || andResult || "  OR=" || orResult || "  XOR=" || xorResult || "  NOT A=" || notResult
    SAY ""
END

/* ============================================================ */
/* SECTION 6: PALINDROMIC NUMBERS                              */
/* ============================================================ */

SAY "SECTION 6: PALINDROMIC NUMBERS IN DIFFERENT BASES"
SAY "------------------------------------------------------------"
SAY "  (A number is a palindrome if its digits read same forward and back)"
SAY ""

SAY "  Decimal palindromes (1-200):"
palList = ""
DO n = 1 TO 200
    CALL DecToBase n, 10
    r = result
    IF r = REVERSE(r) THEN palList = palList || n || " "
END
SAY "  " || STRIP(palList)
SAY ""

SAY "  Numbers that are palindromes in BOTH decimal AND binary:"
SAY "  (n <= 200)"
DO n = 1 TO 200
    CALL DecToBase n, 10
    decDigits = result
    CALL DecToBin n
    binDigits = result
    IF decDigits = REVERSE(decDigits) & binDigits = REVERSE(binDigits) THEN
        SAY "    " || n || " (dec: " || decDigits || ", bin: " || binDigits || ")"
END
SAY ""

/* ============================================================ */
/* SECTION 7: NUMBER BASE CURIOSITIES                          */
/* ============================================================ */

SAY "SECTION 7: INTERESTING BASE CONVERSIONS"
SAY "------------------------------------------------------------"

SAY "  1/3 in different bases (as repeating fraction approx):"
SAY "  Base 10: 0.333..."
SAY "  Base 3:  0.1 (exact!)"
SAY "  Base 12: 0.4 (exact!)"
SAY ""

SAY "  'Beautiful' numbers in hex (hex digit sum = 15):"
cnt = 0
DO n = 256 TO 65535
    hexStr = D2X(n)
    total = 0
    DO j = 1 TO LENGTH(hexStr)
        ch = SUBSTR(hexStr, j, 1)
        SELECT
            WHEN ch = 'A' THEN total = total + 10
            WHEN ch = 'B' THEN total = total + 11
            WHEN ch = 'C' THEN total = total + 12
            WHEN ch = 'D' THEN total = total + 13
            WHEN ch = 'E' THEN total = total + 14
            WHEN ch = 'F' THEN total = total + 15
            OTHERWISE total = total + ch
        END
    END
    IF total = 15 & cnt < 8 THEN DO
        SAY "    " || n || " = 0x" || hexStr || " (digit sum=" || total || ")"
        cnt = cnt + 1
    END
END

SAY ""
SAY "============================================================"
SAY "  Number Systems complete!"
SAY "  Binary * Octal * Hex * Roman * Bitwise * Palindromes"
SAY "============================================================"

EXIT

/* ============================================================ */
/* SUBROUTINES                                                   */
/* ============================================================ */

DecToBin: PROCEDURE EXPOSE result
    ARG n
    IF n = 0 THEN DO
        result = "0"
        RETURN
    END
    result = ""
    tmp = n
    DO WHILE tmp > 0
        result = (tmp // 2) || result
        tmp = tmp % 2
    END
    RETURN

DecToBase: PROCEDURE EXPOSE result
    ARG n, base
    IF n = 0 THEN DO
        result = "0"
        RETURN
    END
    digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    result = ""
    tmp = n
    DO WHILE tmp > 0
        d = (tmp // base) + 1
        result = SUBSTR(digits, d, 1) || result
        tmp = tmp % base
    END
    RETURN

ToRoman: PROCEDURE EXPOSE result
    ARG n
    vals = "1000 900 500 400 100 90 50 40 10 9 5 4 1"
    syms = "M CM D CD C XC L XL X IX V IV I"
    result = ""
    DO i = 1 TO WORDS(vals)
        v = WORD(vals, i)
        s = WORD(syms, i)
        DO WHILE n >= v
            result = result || s
            n = n - v
        END
    END
    RETURN

FromRoman: PROCEDURE EXPOSE result
    ARG r
    vals. = 0
    vals.I = 1
    vals.V = 5
    vals.X = 10
    vals.L = 50
    vals.C = 100
    vals.D = 500
    vals.M = 1000
    result = 0
    DO i = 1 TO LENGTH(r) - 1
        cur  = vals.VALUE(SUBSTR(r, i,   1))
        next = vals.VALUE(SUBSTR(r, i+1, 1))
        IF cur < next THEN result = result - cur
        ELSE               result = result + cur
    END
    result = result + vals.VALUE(SUBSTR(r, LENGTH(r), 1))
    RETURN

BitwiseAND: PROCEDURE EXPOSE result
    ARG a, b
    result = BITOR(a, b)  /* REXX BITAND */
    result = BITAND(a, b)
    RETURN

BitwiseOR: PROCEDURE EXPOSE result
    ARG a, b
    result = BITOR(a, b)
    RETURN

BitwiseXOR: PROCEDURE EXPOSE result
    ARG a, b
    result = BITXOR(a, b)
    RETURN

BitwiseNOT: PROCEDURE EXPOSE result
    ARG a, bits
    /* NOT = XOR with all-1 mask */
    mask = 2 ** bits - 1
    CALL BitwiseXOR a, mask
    RETURN

D2O: PROCEDURE
    ARG n
    CALL DecToBase n, 8
    RETURN result
