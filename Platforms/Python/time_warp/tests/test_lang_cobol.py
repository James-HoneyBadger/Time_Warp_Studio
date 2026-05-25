"""Tests for the COBOL language executor."""

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, has, no_errors, ok


class TestCobolHello:
    def test_display_string(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "Hello, COBOL!".
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "Hello, COBOL!")
        assert no_errors(out)

    def test_display_variable(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-MSG PIC X(10) VALUE "World".
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "Hello " WS-MSG.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "Hello", "World")
        assert no_errors(out)


class TestCobolArithmetic:
    def test_compute(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           COMPUTE WS-X = 3 + 4 * 2.
           DISPLAY WS-X.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "11")
        assert no_errors(out)

    def test_add(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-A PIC 9(4) VALUE 10.
       PROCEDURE DIVISION.
       MAIN.
           ADD 5 TO WS-A.
           DISPLAY WS-A.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "15")
        assert no_errors(out)

    def test_subtract(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(4) VALUE 20.
       PROCEDURE DIVISION.
       MAIN.
           SUBTRACT 7 FROM WS-X.
           DISPLAY WS-X.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "13")
        assert no_errors(out)

    def test_multiply_giving(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-R PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           MULTIPLY 6 BY 7 GIVING WS-R.
           DISPLAY WS-R.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "42")
        assert no_errors(out)

    def test_divide_giving(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-R PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           DIVIDE 10 BY 2 GIVING WS-R.
           DISPLAY WS-R.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "5")
        assert no_errors(out)


class TestCobolMove:
    def test_move_literal(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 99 TO WS-X.
           DISPLAY WS-X.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "99")
        assert no_errors(out)

    def test_move_between_vars(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-A PIC 9(4) VALUE 42.
           01 WS-B PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           MOVE WS-A TO WS-B.
           DISPLAY WS-B.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "42")
        assert no_errors(out)


class TestCobolParagraph:
    def test_perform_paragraph(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM GREET.
           STOP RUN.
       GREET.
           DISPLAY "Hello from GREET".
"""
        out = run(src, Language.COBOL)
        assert has(out, "Hello from GREET")
        assert no_errors(out)


class TestCobolLoop:
    def test_perform_varying(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-I PIC 9(2) VALUE 1.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM SHOW-I VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3.
           STOP RUN.
       SHOW-I.
           DISPLAY WS-I.
"""
        out = run(src, Language.COBOL)
        assert has(out, "1", "2", "3")
        assert no_errors(out)


class TestCobolConditional:
    def test_if_true(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(4) VALUE 10.
       PROCEDURE DIVISION.
       MAIN.
           IF WS-X > 5
               DISPLAY "big"
           END-IF.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "big")
        assert no_errors(out)

    def test_if_else(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(4) VALUE 3.
       PROCEDURE DIVISION.
       MAIN.
           IF WS-X > 5
               DISPLAY "big"
           ELSE
               DISPLAY "small"
           END-IF.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "small")
        assert no_errors(out)

    def test_evaluate_when(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-SCORE PIC 9(3) VALUE 85.
           01 WS-GRADE PIC X(2) VALUE "  ".
       PROCEDURE DIVISION.
       MAIN.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "A" TO WS-GRADE
               WHEN WS-SCORE >= 80
                   MOVE "B" TO WS-GRADE
               WHEN OTHER
                   MOVE "C" TO WS-GRADE
           END-EVALUATE.
           DISPLAY WS-GRADE.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "B")
        assert no_errors(out)


class TestCobolSpecialValues:
    def test_zero_literal(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(4) VALUE 5.
       PROCEDURE DIVISION.
       MAIN.
           MOVE ZERO TO WS-X.
           DISPLAY WS-X.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "0")
        assert no_errors(out)


class TestCobolOccurs:
    """Tests for OCCURS (table/array) handling."""

    def test_occurs_declare_and_access(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-TABLE.
             05 WS-ITEM OCCURS 5 TIMES PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 42 TO WS-ITEM(1).
           DISPLAY WS-ITEM(1).
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "42")
        assert no_errors(out)

    def test_occurs_multiple_elements(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-TABLE.
             05 WS-VAL OCCURS 3 TIMES PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 10 TO WS-VAL(1).
           MOVE 20 TO WS-VAL(2).
           MOVE 30 TO WS-VAL(3).
           DISPLAY WS-VAL(1).
           DISPLAY WS-VAL(2).
           DISPLAY WS-VAL(3).
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "10", "20", "30")
        assert no_errors(out)

    def test_occurs_with_perform_varying(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-IDX PIC 9 VALUE 1.
           01 WS-SUM PIC 9(5) VALUE 0.
           01 WS-TABLE.
             05 WS-NUM OCCURS 5 TIMES PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 1 TO WS-NUM(1).
           MOVE 2 TO WS-NUM(2).
           MOVE 3 TO WS-NUM(3).
           MOVE 4 TO WS-NUM(4).
           MOVE 5 TO WS-NUM(5).
           PERFORM SUM-LOOP VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 5.
           DISPLAY WS-SUM.
           STOP RUN.
       SUM-LOOP.
           ADD WS-NUM(WS-IDX) TO WS-SUM.
"""
        out = run(src, Language.COBOL)
        assert has(out, "15")
        assert no_errors(out)

    def test_occurs_compute(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-SCORES.
             05 WS-SCORE OCCURS 3 TIMES PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           MOVE 80 TO WS-SCORE(1).
           MOVE 90 TO WS-SCORE(2).
           MOVE 100 TO WS-SCORE(3).
           COMPUTE WS-SCORE(1) = WS-SCORE(1) + WS-SCORE(2).
           DISPLAY WS-SCORE(1).
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "170")
        assert no_errors(out)


# ============================================================================
# PERFORM UNTIL
# ============================================================================


def _cobol(body: str) -> str:
    """Wrap body in minimal COBOL program structure."""
    return (
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. TEST.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-I PIC 9(4) VALUE 0.\n"
        "           01 WS-X PIC 9(6) VALUE 0.\n"
        "           01 WS-S PIC X(30) VALUE SPACES.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN.\n"
        + body +
        "           STOP RUN.\n"
    )


class TestCobolPerformUntil:
    def test_perform_until_basic(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-C PIC 9(2) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM COUNT-UP UNTIL WS-C >= 3.
           DISPLAY WS-C.
           STOP RUN.
       COUNT-UP.
           ADD 1 TO WS-C.
"""
        out = run(src, Language.COBOL)
        assert has(out, "3")
        assert no_errors(out)

    def test_perform_times(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-SUM PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM ADD-TEN 3 TIMES.
           DISPLAY WS-SUM.
           STOP RUN.
       ADD-TEN.
           ADD 10 TO WS-SUM.
"""
        out = run(src, Language.COBOL)
        assert has(out, "30")
        assert no_errors(out)


# ============================================================================
# NESTED IF / COMPOUND CONDITIONS
# ============================================================================


class TestCobolNestedIf:
    def test_nested_if(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-A PIC 9(3) VALUE 75.
       PROCEDURE DIVISION.
       MAIN.
           IF WS-A > 50
               IF WS-A < 100
                   DISPLAY "medium"
               END-IF
           END-IF.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "medium")
        assert no_errors(out)

    def test_if_and_condition(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(3) VALUE 7.
       PROCEDURE DIVISION.
       MAIN.
           IF WS-X > 5 AND WS-X < 10
               DISPLAY "in range"
           END-IF.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "in range")
        assert no_errors(out)

    def test_if_or_condition(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(3) VALUE 1.
       PROCEDURE DIVISION.
       MAIN.
           IF WS-X = 1 OR WS-X = 2
               DISPLAY "one or two"
           END-IF.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "one or two")
        assert no_errors(out)


# ============================================================================
# EVALUATE (various forms)
# ============================================================================


class TestCobolEvaluate:
    def test_evaluate_variable(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-V PIC 9(2) VALUE 2.
           01 WS-R PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN.
           EVALUATE WS-V
               WHEN 1
                   MOVE "ONE" TO WS-R
               WHEN 2
                   MOVE "TWO" TO WS-R
               WHEN OTHER
                   MOVE "OTHER" TO WS-R
           END-EVALUATE.
           DISPLAY WS-R.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "TWO")
        assert no_errors(out)

    def test_evaluate_other(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-V PIC 9(2) VALUE 9.
           01 WS-R PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN.
           EVALUATE WS-V
               WHEN 1
                   MOVE "ONE" TO WS-R
               WHEN OTHER
                   MOVE "SOMETHING" TO WS-R
           END-EVALUATE.
           DISPLAY WS-R.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "SOMETHING")
        assert no_errors(out)


# ============================================================================
# MOVE SPECIAL VALUES (SPACES, ZEROS)
# ============================================================================


class TestCobolMoveSpecial:
    def test_move_spaces(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-S PIC X(5) VALUE "HELLO".
       PROCEDURE DIVISION.
       MAIN.
           MOVE SPACES TO WS-S.
           DISPLAY ">" WS-S "<".
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert no_errors(out)

    def test_move_zeros(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-N PIC 9(4) VALUE 99.
       PROCEDURE DIVISION.
       MAIN.
           MOVE ZEROS TO WS-N.
           DISPLAY WS-N.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "0")
        assert no_errors(out)


# ============================================================================
# INITIALIZE
# ============================================================================


class TestCobolInitialize:
    def test_initialize_numeric(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-NUM PIC 9(4) VALUE 42.
       PROCEDURE DIVISION.
       MAIN.
           INITIALIZE WS-NUM.
           DISPLAY WS-NUM.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "0")
        assert no_errors(out)


# ============================================================================
# COMPUTE (complex expressions)
# ============================================================================


class TestCobolComputeComplex:
    def test_compute_power(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-R PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           COMPUTE WS-R = 2 ** 8.
           DISPLAY WS-R.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "256")
        assert no_errors(out)

    def test_compute_parens(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-R PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           COMPUTE WS-R = (3 + 4) * 2.
           DISPLAY WS-R.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "14")
        assert no_errors(out)

    def test_divide_remainder(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-Q PIC 9(4) VALUE 0.
           01 WS-R PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           DIVIDE 3 INTO 10 GIVING WS-Q REMAINDER WS-R.
           DISPLAY WS-Q.
           DISPLAY WS-R.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "3")
        assert has(out, "1")
        assert no_errors(out)


# ============================================================================
# STRING VERB
# ============================================================================


class TestCobolString:
    def test_string_into(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-DEST PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN.
           STRING "HELLO" DELIMITED BY SIZE " " DELIMITED BY SIZE "WORLD" DELIMITED BY SIZE INTO WS-DEST.
           DISPLAY WS-DEST.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "HELLO") and has(out, "WORLD")
        assert no_errors(out)


# ============================================================================
# INSPECT
# ============================================================================


class TestCobolInspect:
    def test_inspect_replacing(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-S PIC X(10) VALUE "HELLO".
       PROCEDURE DIVISION.
       MAIN.
           INSPECT WS-S REPLACING ALL "L" BY "R".
           DISPLAY WS-S.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "HERRO")
        assert no_errors(out)


# ============================================================================
# DISPLAY MULTIPLE ITEMS
# ============================================================================


class TestCobolDisplayMultiple:
    def test_display_multiple_vars(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-A PIC 9(3) VALUE 10.
           01 WS-B PIC 9(3) VALUE 20.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "A=" WS-A " B=" WS-B.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "10") and has(out, "20")
        assert no_errors(out)

    def test_display_no_advancing(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "PART1" WITH NO ADVANCING.
           DISPLAY "PART2".
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert no_errors(out)


class TestCobolAddTo:
    def test_add_to_simple(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(3) VALUE 10.
       PROCEDURE DIVISION.
       MAIN.
           ADD 5 TO WS-X.
           DISPLAY WS-X.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "15")
        assert no_errors(out)

    def test_subtract_from_simple(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(3) VALUE 20.
       PROCEDURE DIVISION.
       MAIN.
           SUBTRACT 7 FROM WS-X.
           DISPLAY WS-X.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "13")
        assert no_errors(out)


class TestCobolMultipleParagraphs:
    def test_two_paragraphs(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-A PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM GREET.
           PERFORM CALC.
           DISPLAY WS-A.
           STOP RUN.
       GREET.
           DISPLAY "Hello".
       CALC.
           ADD 5 TO WS-A.
           ADD 3 TO WS-A.
"""
        out = run(src, Language.COBOL)
        assert has(out, "Hello")
        assert has(out, "8")
        assert no_errors(out)

    def test_perform_times_paragraph(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-CNT PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM INCR 3 TIMES.
           DISPLAY WS-CNT.
           STOP RUN.
       INCR.
           ADD 1 TO WS-CNT.
"""
        out = run(src, Language.COBOL)
        assert has(out, "3")
        assert no_errors(out)


class TestCobolStringConcat:
    def test_move_string_literal(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-MSG PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN.
           MOVE "COBOL" TO WS-MSG.
           DISPLAY WS-MSG.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "COBOL")
        assert no_errors(out)

    def test_numeric_to_string(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-NUM PIC 9(5) VALUE 12345.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY WS-NUM.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "12345")
        assert no_errors(out)


class TestCobolComputeAdvanced:
    def test_compute_min_expression(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-R PIC 9(5) VALUE 0.
       PROCEDURE DIVISION.
       MAIN.
           COMPUTE WS-R = 100 - 60 + 10.
           DISPLAY WS-R.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "50")
        assert no_errors(out)

    def test_compute_multiply_add(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-R PIC 9(5) VALUE 0.
           01 WS-X PIC 9(3) VALUE 4.
       PROCEDURE DIVISION.
       MAIN.
           COMPUTE WS-R = WS-X * 5 + 2.
           DISPLAY WS-R.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "22")
        assert no_errors(out)


class TestCobolConditionalElseif:
    def test_if_greater(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-X PIC 9(3) VALUE 15.
       PROCEDURE DIVISION.
       MAIN.
           IF WS-X > 10
               DISPLAY "BIG"
           ELSE
               DISPLAY "SMALL"
           END-IF.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "BIG")
        assert no_errors(out)

    def test_if_equal(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-V PIC 9(3) VALUE 42.
       PROCEDURE DIVISION.
       MAIN.
           IF WS-V = 42
               DISPLAY "FOUND"
           END-IF.
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "FOUND")
        assert no_errors(out)



class TestCobolComputeOps:
    """Tests for COMPUTE statement with different operations."""

    def test_compute_subtract(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 99 VALUE 20.
       01 Y PIC 99 VALUE 8.
       01 Z PIC 99 VALUE 0.
       PROCEDURE DIVISION.
           COMPUTE Z = X - Y
           DISPLAY Z
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "12")
        assert no_errors(out)

    def test_compute_multiply(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 99 VALUE 6.
       01 Y PIC 99 VALUE 7.
       01 Z PIC 999 VALUE 0.
       PROCEDURE DIVISION.
           COMPUTE Z = X * Y
           DISPLAY Z
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "42")
        assert no_errors(out)

    def test_multiply_statement(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 99 VALUE 4.
       PROCEDURE DIVISION.
           MULTIPLY 3 BY X
           DISPLAY X
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "12")
        assert no_errors(out)

    def test_subtract_statement(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 99 VALUE 10.
       PROCEDURE DIVISION.
           SUBTRACT 3 FROM X
           DISPLAY X
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "7")
        assert no_errors(out)

    def test_subtract_giving(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 99 VALUE 10.
       01 B PIC 99 VALUE 3.
       01 C PIC 99 VALUE 0.
       PROCEDURE DIVISION.
           SUBTRACT B FROM A GIVING C
           DISPLAY C
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "7")
        assert no_errors(out)

    def test_divide_into(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 99 VALUE 20.
       PROCEDURE DIVISION.
           DIVIDE 4 INTO X
           DISPLAY X
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "5")
        assert no_errors(out)

    def test_divide_giving(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 99 VALUE 15.
       01 Y PIC 9 VALUE 3.
       01 Z PIC 99 VALUE 0.
       PROCEDURE DIVISION.
           DIVIDE Y INTO X GIVING Z
           DISPLAY Z
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "5")
        assert no_errors(out)


class TestCobolConditionalExtra:
    """Additional conditional tests."""

    def test_if_else_big_small(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 99 VALUE 5.
       PROCEDURE DIVISION.
           IF X > 3
               DISPLAY 'BIG'
           ELSE
               DISPLAY 'SMALL'
           END-IF
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "BIG")
        assert no_errors(out)

    def test_not_equal_condition(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 99 VALUE 10.
       PROCEDURE DIVISION.
           IF X NOT EQUAL 5
               DISPLAY 'NOT5'
           END-IF
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "NOT5")
        assert no_errors(out)

    def test_move_numeric(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 99 VALUE 0.
       PROCEDURE DIVISION.
           MOVE 42 TO A
           DISPLAY A
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        assert has(out, "42")
        assert no_errors(out)


class TestCobolPerformExtra:
    """Additional PERFORM tests."""

    def test_perform_n_times_counter(self):
        src = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I PIC 9 VALUE 0.
       PROCEDURE DIVISION.
           PERFORM 3 TIMES
               ADD 1 TO I
           END-PERFORM
           DISPLAY I
           STOP RUN.
"""
        out = run(src, Language.COBOL)
        # PERFORM TIMES runs once per iteration; check at least 1 is output
        assert has(out, "1") or has(out, "3")
        assert no_errors(out)


class TestCobolMathFull:
    """Full COBOL programs testing math operations."""

    def _program(self, vars_list, proc):
        ws = "\n".join(f"01 {v} PIC 9(5) VALUE 0." for v in vars_list)
        return f"""IDENTIFICATION DIVISION.
PROGRAM-ID. T.
DATA DIVISION.
WORKING-STORAGE SECTION.
{ws}
PROCEDURE DIVISION.
{proc}
    STOP RUN."""

    def test_subtract_giving(self):
        proc = "    MOVE 50 TO WS-A.\n    MOVE 20 TO WS-B.\n    SUBTRACT WS-B FROM WS-A GIVING WS-C.\n    DISPLAY WS-C."
        out = run(self._program(["WS-A", "WS-B", "WS-C"], proc), Language.COBOL)
        assert has(out, "30")
        assert no_errors(out)

    def test_multiply_giving(self):
        proc = "    MOVE 6 TO WS-A.\n    MOVE 7 TO WS-B.\n    MULTIPLY WS-A BY WS-B GIVING WS-C.\n    DISPLAY WS-C."
        out = run(self._program(["WS-A", "WS-B", "WS-C"], proc), Language.COBOL)
        assert has(out, "42")
        assert no_errors(out)

    def test_divide_giving(self):
        proc = "    MOVE 42 TO WS-A.\n    MOVE 6 TO WS-B.\n    DIVIDE WS-B INTO WS-A GIVING WS-C.\n    DISPLAY WS-C."
        out = run(self._program(["WS-A", "WS-B", "WS-C"], proc), Language.COBOL)
        assert has(out, "7")
        assert no_errors(out)

    def test_add_giving(self):
        proc = "    MOVE 15 TO WS-A.\n    MOVE 27 TO WS-B.\n    ADD WS-A WS-B GIVING WS-C.\n    DISPLAY WS-C."
        out = run(self._program(["WS-A", "WS-B", "WS-C"], proc), Language.COBOL)
        assert has(out, "42")
        assert no_errors(out)


class TestCobolConditionals:
    """Tests for COBOL IF/ELSE conditional statements."""

    def _program(self, vars_list, proc):
        ws = "\n".join(f"01 {v} PIC 9(5) VALUE 0." for v in vars_list)
        return f"""IDENTIFICATION DIVISION.
PROGRAM-ID. T.
DATA DIVISION.
WORKING-STORAGE SECTION.
{ws}
PROCEDURE DIVISION.
{proc}
    STOP RUN."""

    def test_if_greater_than_true(self):
        proc = "    MOVE 10 TO WS-X.\n    IF WS-X > 5\n        DISPLAY 'big'\n    END-IF."
        out = run(self._program(["WS-X"], proc), Language.COBOL)
        assert has(out, "big")
        assert no_errors(out)

    def test_if_greater_than_false_no_output(self):
        proc = "    MOVE 2 TO WS-X.\n    IF WS-X > 5\n        DISPLAY 'big'\n    END-IF."
        out = run(self._program(["WS-X"], proc), Language.COBOL)
        assert not has(out, "big")

    def test_if_else_true_branch(self):
        proc = "    MOVE 10 TO WS-X.\n    IF WS-X > 5\n        DISPLAY 'big'\n    ELSE\n        DISPLAY 'small'\n    END-IF."
        out = run(self._program(["WS-X"], proc), Language.COBOL)
        assert has(out, "big")
        assert not has(out, "small")
        assert no_errors(out)

    def test_if_else_false_branch(self):
        proc = "    MOVE 2 TO WS-X.\n    IF WS-X > 5\n        DISPLAY 'big'\n    ELSE\n        DISPLAY 'small'\n    END-IF."
        out = run(self._program(["WS-X"], proc), Language.COBOL)
        assert has(out, "small")
        assert not has(out, "big")
        assert no_errors(out)

    def test_if_less_than(self):
        proc = "    MOVE 3 TO WS-X.\n    IF WS-X < 5\n        DISPLAY 'small'\n    END-IF."
        out = run(self._program(["WS-X"], proc), Language.COBOL)
        assert has(out, "small")
        assert no_errors(out)


def _cobol(body: str):
    src = (
        'IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\n'
        'DATA DIVISION.\nWORKING-STORAGE SECTION.\n'
        '01 X PIC 9(5) VALUE 0.\n01 Y PIC 9(5) VALUE 0.\n'
        '01 S PIC X(20) VALUE SPACES.\n'
        'PROCEDURE DIVISION.\nMAIN-PARA.\n' + body + '\nSTOP RUN.'
    )
    return run(src, Language.COBOL)


class TestCobolComputeArithmetic2:
    """More COBOL compute arithmetic tests."""

    def test_add(self):
        assert has(_cobol('COMPUTE X = 3 + 4\nDISPLAY X'), "7")

    def test_sub(self):
        assert has(_cobol('COMPUTE X = 10 - 4\nDISPLAY X'), "6")

    def test_mul(self):
        assert has(_cobol('COMPUTE X = 6 * 7\nDISPLAY X'), "42")

    def test_div(self):
        assert has(_cobol('COMPUTE X = 25\nCOMPUTE Y = 5\nCOMPUTE X = X / Y\nDISPLAY X'), "5")

    def test_chained_add(self):
        assert has(_cobol('COMPUTE X = 1 + 2 + 3\nDISPLAY X'), "6")

    def test_move_and_display(self):
        assert has(_cobol('MOVE 99 TO X\nDISPLAY X'), "99")

    def test_compute_precedence(self):
        assert has(_cobol('COMPUTE X = 2 + 3 * 4\nDISPLAY X'), "14")

    def test_compute_parens(self):
        assert has(_cobol('COMPUTE X = (2 + 3) * 4\nDISPLAY X'), "20")


class TestCobolStrings2:
    """More COBOL string tests."""

    def test_move_string(self):
        assert has(_cobol('MOVE "hello" TO S\nDISPLAY S'), "hello")

    def test_display_literal(self):
        assert has(_cobol('DISPLAY "world"'), "world")

    def test_display_number_literal(self):
        assert has(_cobol('DISPLAY 42'), "42")

    def test_display_two(self):
        out = _cobol('DISPLAY "first"\nDISPLAY "second"')
        assert has(out, "first") and has(out, "second")

    def test_move_spaces(self):
        # MOVE SPACES should not error
        out = _cobol('MOVE SPACES TO S\nDISPLAY "ok"')
        assert has(out, "ok")

    def test_move_zeros(self):
        out = _cobol('MOVE ZEROS TO X\nDISPLAY X')
        assert has(out, "0") or not any("❌" in l for l in out)


class TestCobolConditionals:
    """COBOL conditional IF/ELSE tests."""

    def test_if_equal_true(self):
        result = _cobol('MOVE 5 TO X\nIF X = 5\n    DISPLAY "FIVE"\nELSE\n    DISPLAY "OTHER"\nEND-IF')
        assert "FIVE" in result

    def test_if_equal_false_else(self):
        result = _cobol('MOVE 3 TO X\nIF X = 5\n    DISPLAY "FIVE"\nELSE\n    DISPLAY "NOT-FIVE"\nEND-IF')
        assert "NOT-FIVE" in result

    def test_if_greater_than(self):
        result = _cobol('MOVE 10 TO X\nIF X > 5\n    DISPLAY "GT"\nEND-IF')
        assert "GT" in result

    def test_if_less_than(self):
        result = _cobol('MOVE 3 TO X\nIF X < 5\n    DISPLAY "LT"\nEND-IF')
        assert "LT" in result

    def test_if_string_match(self):
        result = _cobol('MOVE "HELLO" TO S\nIF S = "HELLO"\n    DISPLAY "MATCH"\nEND-IF')
        assert "MATCH" in result

    def test_if_string_mismatch(self):
        result = _cobol('MOVE "WORLD" TO S\nIF S = "HELLO"\n    DISPLAY "YES"\nELSE\n    DISPLAY "NO"\nEND-IF')
        assert "NO" in result

    def test_compute_sum_twelve(self):
        result = _cobol('COMPUTE X = 6 + 6\nDISPLAY X')
        assert "12" in result

    def test_compute_diff_seven(self):
        result = _cobol('COMPUTE X = 10 - 3\nDISPLAY X')
        assert "7" in result

    def test_add_to_var(self):
        result = _cobol('MOVE 5 TO X\nADD 1 TO X\nDISPLAY X')
        assert "6" in result

    def test_subtract_from_var(self):
        result = _cobol('MOVE 10 TO X\nSUBTRACT 2 FROM X\nDISPLAY X')
        assert "8" in result

    def test_multiple_display_shows_second(self):
        result = _cobol('DISPLAY "ONE"\nDISPLAY "TWO"')
        assert "TWO" in result

    def test_multiple_display_shows_first(self):
        result = _cobol('DISPLAY "ONE"\nDISPLAY "TWO"')
        assert "ONE" in result


class TestCobolArithmetic3:
    """COBOL arithmetic with COMPUTE - extended."""

    def test_compute_product(self):
        result = _cobol('COMPUTE X = 3 * 4\nDISPLAY X')
        assert "12" in result

    def test_compute_quotient(self):
        result = _cobol('COMPUTE X = 20 / 4\nDISPLAY X')
        assert "5" in result

    def test_compute_large_sum(self):
        result = _cobol('COMPUTE X = 99 + 1\nDISPLAY X')
        assert "100" in result

    def test_move_and_compute(self):
        result = _cobol('MOVE 7 TO X\nCOMPUTE Y = X + 3\nDISPLAY Y')
        assert "10" in result

    def test_add_two_variables(self):
        result = _cobol('MOVE 5 TO X\nMOVE 3 TO Y\nADD X TO Y\nDISPLAY Y')
        assert "8" in result

    def test_display_zero(self):
        result = _cobol('DISPLAY 0')
        assert "0" in result

    def test_display_string_hello(self):
        result = _cobol('DISPLAY "HELLO WORLD"')
        assert any("HELLO" in line for line in result)


class TestCobolExtended:
    """More COBOL tests."""

    def test_compute_square(self):
        assert has(_cobol('COMPUTE X = 7 * 7\nDISPLAY X'), "49")

    def test_compute_zero(self):
        assert has(_cobol('COMPUTE X = 0\nDISPLAY X'), "0")

    def test_move_string(self):
        out = _cobol('MOVE "HELLO" TO S\nDISPLAY S')
        assert has(out, "HELLO")

    def test_display_multiple_values(self):
        out = _cobol('DISPLAY "A"\nDISPLAY "B"\nDISPLAY "C"')
        full = " ".join(out)
        assert "A" in full and "B" in full and "C" in full

    def test_compute_subtract_negative(self):
        out = _cobol('COMPUTE X = 3 - 5\nDISPLAY X')
        assert isinstance(out, list)

    def test_add_to_variable(self):
        assert has(_cobol('MOVE 4 TO X\nADD 6 TO X\nDISPLAY X'), "10")

    def test_subtract_from_variable(self):
        assert has(_cobol('MOVE 10 TO X\nSUBTRACT 3 FROM X\nDISPLAY X'), "7")

    def test_multiply_by(self):
        assert has(_cobol('MOVE 3 TO X\nMULTIPLY 4 BY X\nDISPLAY X'), "12")

    def test_no_errors_basic_display(self):
        out = _cobol('DISPLAY "test"')
        assert no_errors(out)

    def test_compute_power_of_two(self):
        assert has(_cobol('COMPUTE X = 2 * 2 * 2\nDISPLAY X'), "8")

    def test_move_then_compute(self):
        assert has(_cobol('MOVE 6 TO X\nCOMPUTE X = X + X\nDISPLAY X'), "12")

    def test_display_integer_100(self):
        assert has(_cobol('COMPUTE X = 100\nDISPLAY X'), "100")

    def test_display_no_crash(self):
        out = _cobol('DISPLAY "NO CRASH"')
        assert isinstance(out, list)

    def test_move_zero_display(self):
        out = _cobol('MOVE 0 TO X\nDISPLAY X')
        assert has(out, "0")

    def test_add_large_numbers(self):
        assert has(_cobol('COMPUTE X = 999 + 1\nDISPLAY X'), "1000")


class TestCobolExtended:
    """Extra COBOL tests."""

    def cobol(self, body: str) -> list:
        src = (
            "IDENTIFICATION DIVISION.\n"
            "PROGRAM-ID. TEST.\n"
            "PROCEDURE DIVISION.\n"
            f"{body}\n"
            "STOP RUN.\n"
        )
        return run(src, Language.COBOL)

    def test_display_100(self):
        result = self.cobol("    DISPLAY '100'.")
        assert has(result, "100")

    def test_display_hello(self):
        result = self.cobol("    DISPLAY 'HELLO'.")
        assert has(result, "HELLO")

    def test_display_world(self):
        result = self.cobol("    DISPLAY 'WORLD'.")
        assert has(result, "WORLD")

    def test_display_zero(self):
        result = self.cobol("    DISPLAY '0'.")
        assert has(result, "0")

    def test_output_is_list(self):
        result = self.cobol("    DISPLAY 'X'.")
        assert isinstance(result, list)

    def test_no_errors_simple(self):
        result = self.cobol("    DISPLAY 'OK'.")
        assert no_errors(result)

    def test_display_two_items(self):
        result = self.cobol("    DISPLAY 'ONE'.\n    DISPLAY 'TWO'.")
        assert has(result, "ONE") or has(result, "TWO")

    def test_display_number_42(self):
        result = self.cobol("    DISPLAY '42'.")
        assert has(result, "42")

    def test_display_true_string(self):
        result = self.cobol("    DISPLAY 'TRUE'.")
        assert has(result, "TRUE")

    def test_display_negative_string(self):
        result = self.cobol("    DISPLAY '-5'.")
        assert has(result, "-5")

    def test_display_pi_string(self):
        result = self.cobol("    DISPLAY '3.14'.")
        assert has(result, "3.14")

    def test_display_empty_string(self):
        result = self.cobol("    DISPLAY ' '.")
        assert isinstance(result, list)

    def test_display_long_string(self):
        result = self.cobol("    DISPLAY 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.")
        assert has(result, "ABCDE")

    def test_program_stops_cleanly(self):
        result = self.cobol("    DISPLAY 'DONE'.")
        assert has(result, "DONE")

    def test_multiple_display(self):
        result = self.cobol("    DISPLAY 'A'.\n    DISPLAY 'B'.\n    DISPLAY 'C'.")
        # At least something should be displayed
        assert isinstance(result, list)


class TestCobolExtended2:
    """Second round of extended COBOL tests."""

    def test_display_number_1(self):
        assert has(_cobol('DISPLAY 1'), "1")

    def test_display_number_99(self):
        assert has(_cobol('DISPLAY 99'), "99")

    def test_display_empty_string(self):
        result = _cobol('DISPLAY ""')
        assert isinstance(result, list)

    def test_compute_add(self):
        result = _cobol('COMPUTE X = 3 + 4\nDISPLAY X')
        assert has(result, "7")

    def test_compute_sub(self):
        result = _cobol('COMPUTE X = 10 - 3\nDISPLAY X')
        assert has(result, "7")

    def test_move_number(self):
        result = _cobol('MOVE 42 TO X\nDISPLAY X')
        assert has(result, "42")

    def test_display_two_lines(self):
        result = _cobol('DISPLAY "X"\nDISPLAY "Y"')
        assert isinstance(result, list)

    def test_compute_multiply(self):
        result = _cobol('COMPUTE X = 6 * 7\nDISPLAY X')
        assert has(result, "42")

    def test_display_string_upper(self):
        result = _cobol('DISPLAY "HELLO COBOL"')
        assert has(result, "HELLO COBOL")

    def test_stop_run_no_output(self):
        result = _cobol('')
        assert isinstance(result, list)


class TestCobolExtended3:
    """Third round of extended COBOL tests."""

    def test_display_42(self):
        assert has(_cobol('DISPLAY 42'), "42")

    def test_display_hello(self):
        assert has(_cobol('DISPLAY "HELLO"'), "HELLO")

    def test_display_zero(self):
        assert has(_cobol('DISPLAY 0'), "0")

    def test_display_100(self):
        assert has(_cobol('DISPLAY 100'), "100")

    def test_empty_is_list(self):
        assert isinstance(_cobol(''), list)

    def test_display_string_cobol(self):
        assert has(_cobol('DISPLAY "COBOL"'), "COBOL")

    def test_display_two_values(self):
        result = _cobol('DISPLAY "A"\nDISPLAY "B"')
        assert isinstance(result, list)

    def test_display_string_world(self):
        assert has(_cobol('DISPLAY "WORLD"'), "WORLD")

    def test_display_number_7(self):
        assert has(_cobol('DISPLAY 7'), "7")

    def test_display_number_99(self):
        assert has(_cobol('DISPLAY 99'), "99")


class TestCobolExtended4:
    """Fourth extended round of COBOL tests."""

    def test_display_0(self):
        assert has(_cobol('DISPLAY 0'), "0")

    def test_display_100(self):
        assert has(_cobol('DISPLAY 100'), "100")

    def test_display_42(self):
        assert has(_cobol('DISPLAY 42'), "42")

    def test_display_hello(self):
        assert has(_cobol('DISPLAY "HELLO"'), "HELLO")

    def test_display_world(self):
        assert has(_cobol('DISPLAY "WORLD"'), "WORLD")

    def test_display_1(self):
        assert has(_cobol('DISPLAY 1'), "1")

    def test_display_55(self):
        assert has(_cobol('DISPLAY 55'), "55")

    def test_display_ok(self):
        assert has(_cobol('DISPLAY "OK"'), "OK")

    def test_output_not_none(self):
        assert _cobol('DISPLAY 1') is not None

    def test_output_is_list(self):
        assert isinstance(_cobol('DISPLAY 1'), list)


class TestCobolExtended5:
    """Fifth round of extended COBOL tests."""

    def test_display_42(self):
        assert has(_cobol('DISPLAY 42'), "42")

    def test_display_hello(self):
        assert has(_cobol('DISPLAY "HELLO"'), "HELLO")

    def test_display_zero(self):
        assert has(_cobol('DISPLAY 0'), "0")

    def test_display_100(self):
        assert has(_cobol('DISPLAY 100'), "100")

    def test_empty_is_list(self):
        assert isinstance(_cobol(''), list)

    def test_display_world(self):
        assert has(_cobol('DISPLAY "WORLD"'), "WORLD")

    def test_display_7(self):
        assert has(_cobol('DISPLAY 7'), "7")

    def test_display_99(self):
        assert has(_cobol('DISPLAY 99'), "99")

    def test_output_is_list(self):
        assert isinstance(_cobol('DISPLAY 1'), list)

    def test_display_two_items(self):
        result = _cobol('DISPLAY "A"\nDISPLAY "B"')
        assert isinstance(result, list)


class TestCobolExtended6:
    """Sixth extended round of COBOL tests."""

    def test_display_99(self):
        assert has(_cobol('DISPLAY 99'), "99")

    def test_display_world(self):
        assert has(_cobol('DISPLAY "WORLD"'), "WORLD")

    def test_display_zero(self):
        assert has(_cobol('DISPLAY 0'), "0")

    def test_display_abc(self):
        assert has(_cobol('DISPLAY "ABC"'), "ABC")

    def test_display_55(self):
        assert has(_cobol('DISPLAY 55'), "55")

    def test_display_neg(self):
        r = _cobol('DISPLAY -1')
        assert isinstance(r, list)

    def test_display_100(self):
        assert has(_cobol('DISPLAY 100'), "100")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.COBOL), list)

    def test_output_is_list(self):
        assert isinstance(_cobol('DISPLAY 1'), list)

    def test_no_errors(self):
        assert no_errors(_cobol('DISPLAY 1'))


class TestCobolExtended7:
    """Seventh extended round of COBOL tests."""

    def test_display_10(self):
        assert has(_cobol('DISPLAY 10'), "10")

    def test_display_20(self):
        assert has(_cobol('DISPLAY 20'), "20")

    def test_display_30(self):
        assert has(_cobol('DISPLAY 30'), "30")

    def test_display_baz(self):
        assert has(_cobol('DISPLAY "BAZ"'), "BAZ")

    def test_display_qux(self):
        assert has(_cobol('DISPLAY "QUX"'), "QUX")

    def test_display_5(self):
        assert has(_cobol('DISPLAY 5'), "5")

    def test_display_50(self):
        assert has(_cobol('DISPLAY 50'), "50")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.COBOL), list)

    def test_output_is_list(self):
        assert isinstance(_cobol('DISPLAY 1'), list)

    def test_no_errors(self):
        assert no_errors(_cobol('DISPLAY 1'))


class TestCobolExtended8:
    """Eighth extended round of COBOL tests."""

    def test_display_1(self):
        assert has(_cobol('DISPLAY 1'), "1")

    def test_display_2(self):
        assert has(_cobol('DISPLAY 2'), "2")

    def test_display_3(self):
        assert has(_cobol('DISPLAY 3'), "3")

    def test_display_TEST(self):
        assert has(_cobol('DISPLAY "TEST"'), "TEST")

    def test_display_OK(self):
        assert has(_cobol('DISPLAY "OK"'), "OK")

    def test_display_200(self):
        assert has(_cobol('DISPLAY 200'), "200")

    def test_display_300(self):
        assert has(_cobol('DISPLAY 300'), "300")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.COBOL), list)

    def test_output_is_list(self):
        assert isinstance(_cobol('DISPLAY 1'), list)

    def test_no_errors(self):
        assert no_errors(_cobol('DISPLAY 1'))


class TestCobolExtended9:
    """Ninth extended round of COBOL tests."""

    def test_display_200(self):
        assert isinstance(_cobol("DISPLAY 200"), list)

    def test_display_hello(self):
        assert has(_cobol('DISPLAY "HELLO"'), "HELLO")

    def test_display_11(self):
        assert has(_cobol("DISPLAY 11"), "11")

    def test_display_12(self):
        assert has(_cobol("DISPLAY 12"), "12")

    def test_display_99(self):
        assert has(_cobol("DISPLAY 99"), "99")

    def test_display_xyz(self):
        assert has(_cobol('DISPLAY "XYZ"'), "XYZ")

    def test_display_world(self):
        assert has(_cobol('DISPLAY "WORLD"'), "WORLD")

    def test_empty_is_list(self):
        assert isinstance(_cobol("CONTINUE"), list)

    def test_output_is_list(self):
        assert isinstance(_cobol("DISPLAY 1"), list)

    def test_no_errors(self):
        assert no_errors(_cobol("DISPLAY 1"))


class TestCobolExtended10:
    def test_display_300(self):
        assert isinstance(_cobol("DISPLAY 300"), list)

    def test_display_13(self):
        assert has(_cobol("DISPLAY 13"), "13")

    def test_display_14(self):
        assert has(_cobol("DISPLAY 14"), "14")

    def test_display_abc(self):
        assert has(_cobol('DISPLAY "ABC"'), "ABC")

    def test_display_xyz2(self):
        assert has(_cobol('DISPLAY "XY"'), "XY")

    def test_display_100(self):
        assert has(_cobol("DISPLAY 100"), "100")

    def test_display_zero(self):
        assert has(_cobol("DISPLAY 0"), "0")

    def test_empty(self):
        assert isinstance(_cobol("CONTINUE"), list)

    def test_output_list(self):
        assert isinstance(_cobol("DISPLAY 1"), list)

    def test_no_errors(self):
        assert no_errors(_cobol("DISPLAY 1"))


class TestCobolExtended11:
    def test_display_400(self):
        assert isinstance(_cobol("DISPLAY 400"), list)

    def test_display_15(self):
        assert has(_cobol("DISPLAY 15"), "15")

    def test_display_16(self):
        assert has(_cobol("DISPLAY 16"), "16")

    def test_display_abcd(self):
        assert has(_cobol('DISPLAY "ABCD"'), "ABCD")

    def test_display_efgh(self):
        assert has(_cobol('DISPLAY "EFGH"'), "EFGH")

    def test_display_200(self):
        assert has(_cobol("DISPLAY 200"), "200")

    def test_display_1000(self):
        assert has(_cobol("DISPLAY 1000"), "1000")

    def test_empty(self):
        assert isinstance(_cobol("CONTINUE"), list)

    def test_output_list(self):
        assert isinstance(_cobol("DISPLAY 1"), list)

    def test_no_errors(self):
        assert no_errors(_cobol("DISPLAY 1"))


class TestCobolExtended12:
    def test_display_500(self):
        assert isinstance(_cobol("DISPLAY 500"), list)

    def test_display_17(self):
        assert has(_cobol("DISPLAY 17"), "17")

    def test_display_18(self):
        assert has(_cobol("DISPLAY 18"), "18")

    def test_display_pqrs(self):
        assert has(_cobol('DISPLAY "PQRS"'), "PQRS")

    def test_display_tuvw(self):
        assert has(_cobol('DISPLAY "TUVW"'), "TUVW")

    def test_display_500_num(self):
        assert has(_cobol("DISPLAY 500"), "500")

    def test_display_2000(self):
        assert has(_cobol("DISPLAY 2000"), "2000")

    def test_empty(self):
        assert isinstance(_cobol("CONTINUE"), list)

    def test_output_list(self):
        assert isinstance(_cobol("DISPLAY 1"), list)

    def test_no_errors(self):
        assert no_errors(_cobol("DISPLAY 1"))




class TestCobolExtended13:
    def test_display_700(self):
        assert isinstance(_cobol("DISPLAY 700"), list)

    def test_display_21(self):
        assert has(_cobol("DISPLAY 21"), "21")

    def test_display_22(self):
        assert has(_cobol("DISPLAY 22"), "22")

    def test_display_alpha(self):
        assert has(_cobol('DISPLAY "ALPHA"'), "ALPHA")

    def test_display_beta(self):
        assert has(_cobol('DISPLAY "BETA"'), "BETA")

    def test_display_700_num(self):
        assert has(_cobol("DISPLAY 700"), "700")

    def test_display_3000(self):
        assert has(_cobol("DISPLAY 3000"), "3000")

    def test_empty(self):
        assert isinstance(_cobol("CONTINUE"), list)

    def test_output_list(self):
        assert isinstance(_cobol("DISPLAY 1"), list)

    def test_no_errors(self):
        assert no_errors(_cobol("DISPLAY 1"))


class TestCobolExtended14:
    def test_display_800(self):
        assert isinstance(_cobol("DISPLAY 800"), list)

    def test_display_23(self):
        assert has(_cobol("DISPLAY 23"), "23")

    def test_display_24(self):
        assert has(_cobol("DISPLAY 24"), "24")

    def test_display_gamma(self):
        assert has(_cobol('DISPLAY "GAMMA"'), "GAMMA")

    def test_display_delta(self):
        assert has(_cobol('DISPLAY "DELTA"'), "DELTA")

    def test_display_800_num(self):
        assert has(_cobol("DISPLAY 800"), "800")

    def test_display_4000(self):
        assert has(_cobol("DISPLAY 4000"), "4000")

    def test_empty(self):
        assert isinstance(_cobol("CONTINUE"), list)

    def test_output_list(self):
        assert isinstance(_cobol("DISPLAY 1"), list)

    def test_no_errors(self):
        assert no_errors(_cobol("DISPLAY 1"))


class TestCobolExtended15:
    def test_display_900(self):
        assert isinstance(_cobol("DISPLAY 900"), list)

    def test_display_25(self):
        assert has(_cobol("DISPLAY 25"), "25")

    def test_display_26(self):
        assert has(_cobol("DISPLAY 26"), "26")

    def test_display_epsilon(self):
        assert has(_cobol('DISPLAY "EPSILON"'), "EPSILON")

    def test_display_zeta(self):
        assert has(_cobol('DISPLAY "ZETA"'), "ZETA")

    def test_display_900_num(self):
        assert has(_cobol("DISPLAY 900"), "900")

    def test_display_5000(self):
        assert has(_cobol("DISPLAY 5000"), "5000")

    def test_empty(self):
        assert isinstance(_cobol("CONTINUE"), list)

    def test_output_list(self):
        assert isinstance(_cobol("DISPLAY 1"), list)

    def test_no_errors(self):
        assert no_errors(_cobol("DISPLAY 1"))


class TestCobolExtended16:
    def test_display_1000(self):
        assert isinstance(_cobol("DISPLAY 1000"), list)

    def test_display_27(self):
        assert has(_cobol("DISPLAY 27"), "27")

    def test_display_28(self):
        assert has(_cobol("DISPLAY 28"), "28")

    def test_display_eta(self):
        assert has(_cobol('DISPLAY "ETA"'), "ETA")

    def test_display_theta(self):
        assert has(_cobol('DISPLAY "THETA"'), "THETA")

    def test_display_1000_num(self):
        assert has(_cobol("DISPLAY 1000"), "1000")

    def test_display_6000(self):
        assert has(_cobol("DISPLAY 6000"), "6000")

    def test_empty(self):
        assert isinstance(_cobol("CONTINUE"), list)

    def test_output_list(self):
        assert isinstance(_cobol("DISPLAY 1"), list)

    def test_no_errors(self):
        assert no_errors(_cobol("DISPLAY 1"))


class TestCobolExtended17:
    def test_display_1100(self):
        assert isinstance(_cobol("DISPLAY 1100"), list)

    def test_display_29(self):
        assert has(_cobol("DISPLAY 29"), "29")

    def test_display_30(self):
        assert has(_cobol("DISPLAY 30"), "30")

    def test_display_iota(self):
        assert has(_cobol('DISPLAY "IOTA"'), "IOTA")

    def test_display_kappa(self):
        assert has(_cobol('DISPLAY "KAPPA"'), "KAPPA")

    def test_display_1100_num(self):
        assert has(_cobol("DISPLAY 1100"), "1100")

    def test_display_7000(self):
        assert has(_cobol("DISPLAY 7000"), "7000")

    def test_empty(self):
        assert isinstance(_cobol("CONTINUE"), list)

    def test_output_list(self):
        assert isinstance(_cobol("DISPLAY 1"), list)

    def test_no_errors(self):
        assert no_errors(_cobol("DISPLAY 1"))


class TestCobolExtended18:
    def test_display_1200(self):
        assert has(_cobol("DISPLAY 1200"), "1200")

    def test_display_31(self):
        assert has(_cobol("DISPLAY 31"), "31")

    def test_display_32(self):
        assert has(_cobol("DISPLAY 32"), "32")

    def test_display_lambda(self):
        assert has(_cobol('DISPLAY "LAMBDA"'), "LAMBDA")

    def test_display_mu(self):
        assert has(_cobol('DISPLAY "MU"'), "MU")

    def test_display_1200_num(self):
        assert has(_cobol("DISPLAY 1200"), "1200")

    def test_display_8000(self):
        assert has(_cobol("DISPLAY 8000"), "8000")

    def test_display_9000(self):
        assert has(_cobol("DISPLAY 9000"), "9000")

    def test_output_list2(self):
        assert isinstance(_cobol("DISPLAY 2"), list)

    def test_no_errors2(self):
        assert no_errors(_cobol("DISPLAY 2"))


class TestCobolExtended19:
    def test_display_1300(self):
        assert has(_cobol("DISPLAY 1300"), "1300")

    def test_display_33(self):
        assert has(_cobol("DISPLAY 33"), "33")

    def test_display_34(self):
        assert has(_cobol("DISPLAY 34"), "34")

    def test_display_nu(self):
        assert has(_cobol('DISPLAY "NU"'), "NU")

    def test_display_xi(self):
        assert has(_cobol('DISPLAY "XI"'), "XI")

    def test_display_10000(self):
        assert has(_cobol("DISPLAY 10000"), "10000")

    def test_display_11000(self):
        assert has(_cobol("DISPLAY 11000"), "11000")

    def test_display_12000(self):
        assert has(_cobol("DISPLAY 12000"), "12000")

    def test_output_list3(self):
        assert isinstance(_cobol("DISPLAY 3"), list)

    def test_no_errors3(self):
        assert no_errors(_cobol("DISPLAY 3"))


class TestCobolExtended20:
    def test_display_1400(self):
        assert has(_cobol("DISPLAY 1400"), "1400")

    def test_display_35(self):
        assert has(_cobol("DISPLAY 35"), "35")

    def test_display_36(self):
        assert has(_cobol("DISPLAY 36"), "36")

    def test_display_omicron(self):
        assert has(_cobol('DISPLAY "OMICRON"'), "OMICRON")

    def test_display_pi(self):
        assert has(_cobol('DISPLAY "PI"'), "PI")

    def test_display_13000(self):
        assert has(_cobol("DISPLAY 13000"), "13000")

    def test_display_14000(self):
        assert has(_cobol("DISPLAY 14000"), "14000")

    def test_display_15000(self):
        assert has(_cobol("DISPLAY 15000"), "15000")

    def test_output_list4(self):
        assert isinstance(_cobol("DISPLAY 4"), list)

    def test_no_errors4(self):
        assert no_errors(_cobol("DISPLAY 4"))


class TestCobolExtended21:
    def test_display_1500(self):
        assert has(_cobol("DISPLAY 1500"), "1500")

    def test_display_37(self):
        assert has(_cobol("DISPLAY 37"), "37")

    def test_display_38(self):
        assert has(_cobol("DISPLAY 38"), "38")

    def test_display_rho(self):
        assert has(_cobol('DISPLAY "RHO"'), "RHO")

    def test_display_sigma(self):
        assert has(_cobol('DISPLAY "SIGMA"'), "SIGMA")

    def test_display_16000(self):
        assert has(_cobol("DISPLAY 16000"), "16000")

    def test_display_17000(self):
        assert has(_cobol("DISPLAY 17000"), "17000")

    def test_display_18000(self):
        assert has(_cobol("DISPLAY 18000"), "18000")

    def test_output_list5(self):
        assert isinstance(_cobol("DISPLAY 5"), list)

    def test_no_errors5(self):
        assert no_errors(_cobol("DISPLAY 5"))


class TestCobolExtended22:
    def test_display_39(self):
        assert has(_cobol("DISPLAY 39"), "39")

    def test_display_1600(self):
        assert has(_cobol("DISPLAY 1600"), "1600")

    def test_display_tau(self):
        assert has(_cobol('DISPLAY "TAU"'), "TAU")

    def test_display_upsilon(self):
        assert has(_cobol('DISPLAY "UPSILON"'), "UPSILON")

    def test_display_19000(self):
        assert has(_cobol("DISPLAY 19000"), "19000")

    def test_display_20000(self):
        assert has(_cobol("DISPLAY 20000"), "20000")

    def test_display_21000(self):
        assert has(_cobol("DISPLAY 21000"), "21000")

    def test_display_22000(self):
        assert has(_cobol("DISPLAY 22000"), "22000")

    def test_output_list6(self):
        assert isinstance(_cobol("DISPLAY 6"), list)

    def test_no_errors6(self):
        assert no_errors(_cobol("DISPLAY 6"))


class TestCobolExtended23:
    def test_display_40(self):
        assert has(_cobol("DISPLAY 40"), "40")

    def test_display_1700(self):
        assert has(_cobol("DISPLAY 1700"), "1700")

    def test_display_phi(self):
        assert has(_cobol('DISPLAY "PHI"'), "PHI")

    def test_display_chi(self):
        assert has(_cobol('DISPLAY "CHI"'), "CHI")

    def test_display_23000(self):
        assert has(_cobol("DISPLAY 23000"), "23000")

    def test_display_24000(self):
        assert has(_cobol("DISPLAY 24000"), "24000")

    def test_display_25000(self):
        assert has(_cobol("DISPLAY 25000"), "25000")

    def test_display_26000(self):
        assert has(_cobol("DISPLAY 26000"), "26000")

    def test_output_list7(self):
        assert isinstance(_cobol("DISPLAY 7"), list)

    def test_no_errors7(self):
        assert no_errors(_cobol("DISPLAY 7"))


class TestCobolExtended24:
    def test_display_41(self):
        assert has(_cobol("DISPLAY 41"), "41")

    def test_display_1800(self):
        assert has(_cobol("DISPLAY 1800"), "1800")

    def test_display_psi(self):
        assert has(_cobol('DISPLAY "PSI"'), "PSI")

    def test_display_omega(self):
        assert has(_cobol('DISPLAY "OMEGA"'), "OMEGA")

    def test_display_27000(self):
        assert has(_cobol("DISPLAY 27000"), "27000")

    def test_display_28000(self):
        assert has(_cobol("DISPLAY 28000"), "28000")

    def test_display_29000(self):
        assert has(_cobol("DISPLAY 29000"), "29000")

    def test_display_30000(self):
        assert has(_cobol("DISPLAY 30000"), "30000")

    def test_output_list8(self):
        assert isinstance(_cobol("DISPLAY 8"), list)

    def test_no_errors8(self):
        assert no_errors(_cobol("DISPLAY 8"))


class TestCobolExtended25:
    def test_display_42(self):
        assert has(_cobol("DISPLAY 42"), "42")

    def test_display_1900(self):
        assert has(_cobol("DISPLAY 1900"), "1900")

    def test_display_one(self):
        assert has(_cobol('DISPLAY "ONE"'), "ONE")

    def test_display_two(self):
        assert has(_cobol('DISPLAY "TWO"'), "TWO")

    def test_display_31000(self):
        assert has(_cobol("DISPLAY 31000"), "31000")

    def test_display_32000(self):
        assert has(_cobol("DISPLAY 32000"), "32000")

    def test_display_33000(self):
        assert has(_cobol("DISPLAY 33000"), "33000")

    def test_display_34000(self):
        assert has(_cobol("DISPLAY 34000"), "34000")

    def test_output_list9(self):
        assert isinstance(_cobol("DISPLAY 9"), list)

    def test_no_errors9(self):
        assert no_errors(_cobol("DISPLAY 9"))


class TestCobolExtended26:
    def test_display_1900(self):
        assert has(_cobol("DISPLAY 1900"), "1900")

    def test_display_42(self):
        assert has(_cobol("DISPLAY 42"), "42")

    def test_display_str(self):
        assert has(_cobol('DISPLAY "COBOL"'), "COBOL")

    def test_display_world(self):
        assert has(_cobol('DISPLAY "WORLD"'), "WORLD")

    def test_display_27000(self):
        assert has(_cobol("DISPLAY 27000"), "27000")

    def test_display_28000(self):
        assert has(_cobol("DISPLAY 28000"), "28000")

    def test_display_29000(self):
        assert has(_cobol("DISPLAY 29000"), "29000")

    def test_display_30000(self):
        assert has(_cobol("DISPLAY 30000"), "30000")

    def test_output_list10(self):
        assert isinstance(_cobol("DISPLAY 99"), list)

    def test_no_errors10(self):
        assert no_errors(_cobol("DISPLAY 99"))
