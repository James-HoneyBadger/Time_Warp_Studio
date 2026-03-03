"""Comprehensive tests for the COBOL language executor."""

import pytest

from time_warp.core.interpreter import Language

from .conftest_lang import run, ok, has, no_errors, first_error

L = Language.COBOL


def cob(source: str, **kw) -> list[str]:
    """Shortcut: run a COBOL program."""
    return run(source, L, **kw)


# ============================================================================
# DISPLAY
# ============================================================================


class TestDisplay:
    def test_hello_world(self):
        out = cob('DISPLAY "Hello World".')
        assert has(out, "Hello World")

    def test_display_number(self):
        out = cob("DISPLAY 42.")
        assert has(out, "42")

    def test_display_variable(self):
        out = cob(
            '01 WS-NAME PIC X(10) VALUE "Alice".\n'
            "DISPLAY WS-NAME."
        )
        assert has(out, "Alice")

    def test_display_multiple(self):
        out = cob('DISPLAY "A".\nDISPLAY "B".')
        assert has(out, "A") and has(out, "B")


# ============================================================================
# DATA DECLARATIONS
# ============================================================================


class TestDataDeclarations:
    def test_pic_x(self):
        out = cob(
            '01 WS-STR PIC X(5) VALUE "Hello".\n'
            "DISPLAY WS-STR."
        )
        assert has(out, "Hello")

    def test_pic_9(self):
        out = cob(
            "01 WS-NUM PIC 9(3) VALUE 42.\n"
            "DISPLAY WS-NUM."
        )
        assert has(out, "42")

    def test_pic_sv9(self):
        out = cob(
            "01 WS-DEC PIC 9V99 VALUE 3.14.\n"
            "DISPLAY WS-DEC."
        )
        assert no_errors(out)


# ============================================================================
# MOVE
# ============================================================================


class TestMove:
    def test_move_literal(self):
        out = cob(
            "01 WS-X PIC 9(3).\n"
            "MOVE 42 TO WS-X.\n"
            "DISPLAY WS-X."
        )
        assert has(out, "42")

    def test_move_string(self):
        out = cob(
            '01 WS-S PIC X(10).\n'
            'MOVE "Hello" TO WS-S.\n'
            "DISPLAY WS-S."
        )
        assert has(out, "Hello")

    def test_move_variable(self):
        out = cob(
            "01 WS-A PIC 9(3) VALUE 10.\n"
            "01 WS-B PIC 9(3).\n"
            "MOVE WS-A TO WS-B.\n"
            "DISPLAY WS-B."
        )
        assert has(out, "10")


# ============================================================================
# ARITHMETIC (ADD / SUBTRACT / MULTIPLY / DIVIDE / COMPUTE)
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = cob(
            "01 WS-A PIC 9(3) VALUE 10.\n"
            "01 WS-B PIC 9(3) VALUE 20.\n"
            "01 WS-C PIC 9(3).\n"
            "ADD WS-A TO WS-B GIVING WS-C.\n"
            "DISPLAY WS-C."
        )
        assert has(out, "30")

    def test_subtract(self):
        out = cob(
            "01 WS-A PIC 9(3) VALUE 10.\n"
            "01 WS-B PIC 9(3) VALUE 30.\n"
            "01 WS-C PIC 9(3).\n"
            "SUBTRACT WS-A FROM WS-B GIVING WS-C.\n"
            "DISPLAY WS-C."
        )
        assert has(out, "20")

    def test_multiply(self):
        out = cob(
            "01 WS-A PIC 9(3) VALUE 6.\n"
            "01 WS-B PIC 9(3) VALUE 7.\n"
            "01 WS-C PIC 9(3).\n"
            "MULTIPLY WS-A BY WS-B GIVING WS-C.\n"
            "DISPLAY WS-C."
        )
        assert has(out, "42")

    def test_divide(self):
        out = cob(
            "01 WS-A PIC 9(3) VALUE 20.\n"
            "01 WS-B PIC 9(3) VALUE 4.\n"
            "01 WS-C PIC 9(3).\n"
            "DIVIDE WS-A BY WS-B GIVING WS-C.\n"
            "DISPLAY WS-C."
        )
        assert has(out, "5")

    def test_compute(self):
        out = cob(
            "01 WS-X PIC 9(3).\n"
            "COMPUTE WS-X = 2 + 3 * 4.\n"
            "DISPLAY WS-X."
        )
        assert no_errors(out)

    def test_add_simple(self):
        out = cob(
            "01 WS-A PIC 9(3) VALUE 10.\n"
            "ADD 5 TO WS-A.\n"
            "DISPLAY WS-A."
        )
        assert has(out, "15")


# ============================================================================
# IF / EVALUATE
# ============================================================================


class TestConditional:
    def test_if_true(self):
        out = cob(
            "01 WS-X PIC 9(3) VALUE 5.\n"
            'IF WS-X > 3\n  DISPLAY "yes"\nEND-IF.'
        )
        assert has(out, "yes")

    def test_if_else(self):
        out = cob(
            "01 WS-X PIC 9(3) VALUE 1.\n"
            'IF WS-X > 3\n  DISPLAY "big"\nELSE\n  DISPLAY "small"\nEND-IF.'
        )
        assert has(out, "small")

    def test_evaluate(self):
        out = cob(
            "01 WS-X PIC 9(3) VALUE 2.\n"
            "EVALUATE WS-X\n"
            '  WHEN 1 DISPLAY "one"\n'
            '  WHEN 2 DISPLAY "two"\n'
            '  WHEN OTHER DISPLAY "other"\n'
            "END-EVALUATE."
        )
        assert has(out, "two")


# ============================================================================
# PERFORM (LOOP)
# ============================================================================


class TestPerform:
    def test_perform_times(self):
        out = cob('PERFORM 3 TIMES\n  DISPLAY "X"\nEND-PERFORM.')
        assert has(out, "X")

    def test_perform_varying(self):
        out = cob(
            "01 WS-I PIC 9(3).\n"
            "PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3\n"
            "  DISPLAY WS-I\n"
            "END-PERFORM."
        )
        assert has(out, "1") and has(out, "3")

    def test_perform_paragraph(self):
        out = cob(
            "PERFORM SHOW-MSG.\nSTOP RUN.\n"
            'SHOW-MSG.\n  DISPLAY "hello".'
        )
        assert has(out, "hello")


# ============================================================================
# STRING OPERATIONS
# ============================================================================


class TestStringOps:
    def test_string_concat(self):
        out = cob(
            '01 WS-A PIC X(5) VALUE "Hello".\n'
            '01 WS-B PIC X(5) VALUE "World".\n'
            "01 WS-C PIC X(10).\n"
            "STRING WS-A DELIMITED SIZE WS-B DELIMITED SIZE INTO WS-C.\n"
            "DISPLAY WS-C."
        )
        assert has(out, "Hello") and has(out, "World")

    def test_inspect_tallying(self):
        out = cob(
            '01 WS-STR PIC X(10) VALUE "ABCABCAB".\n'
            "01 WS-CNT PIC 9(3) VALUE 0.\n"
            'INSPECT WS-STR TALLYING WS-CNT FOR ALL "A".\n'
            "DISPLAY WS-CNT."
        )
        assert has(out, "3")


# ============================================================================
# STOP RUN
# ============================================================================


class TestStopRun:
    def test_stop_run(self):
        out = cob('DISPLAY "before".\nSTOP RUN.\nDISPLAY "after".')
        assert has(out, "before") and not has(out, "after")


# ============================================================================
# FULL PROGRAM STRUCTURE
# ============================================================================


class TestProgramStructure:
    def test_divisions(self):
        out = cob(
            "IDENTIFICATION DIVISION.\n"
            "PROGRAM-ID. TEST1.\n"
            "DATA DIVISION.\n"
            "WORKING-STORAGE SECTION.\n"
            "01 WS-X PIC 9(3) VALUE 42.\n"
            "PROCEDURE DIVISION.\n"
            "DISPLAY WS-X.\n"
            "STOP RUN."
        )
        assert has(out, "42")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_undefined_variable(self):
        out = cob("DISPLAY WS-UNDEFINED.")
        # May show undefined or error
        assert len(out) > 0
