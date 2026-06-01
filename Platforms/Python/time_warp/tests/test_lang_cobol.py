"""Comprehensive tests for the COBOL executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.COBOL


def cobol(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. HELLO.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        '           DISPLAY "Hello, World!".\n'
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_display_number():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. NUMS.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           DISPLAY 42.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Working storage
# ---------------------------------------------------------------------------


def test_working_storage_variable():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. VARS.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        '           01 WS-NAME  PIC X(10) VALUE "World".\n'
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        '           DISPLAY "Hello, " WS-NAME "!".\n'
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "Hello", "World")


def test_numeric_variable():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. NUMVAR.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-NUM  PIC 9(3) VALUE 42.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           DISPLAY WS-NUM.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Arithmetic (COMPUTE / ADD / SUBTRACT)
# ---------------------------------------------------------------------------


def test_compute():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. CALC.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-A  PIC 9(3) VALUE 10.\n"
        "           01 WS-B  PIC 9(3) VALUE 3.\n"
        "           01 WS-R  PIC 9(3).\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           COMPUTE WS-R = WS-A + WS-B.\n"
        "           DISPLAY WS-R.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "13")


def test_move_and_add():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. MADD.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-X  PIC 9(3) VALUE 7.\n"
        "           01 WS-Y  PIC 9(3) VALUE 5.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           ADD WS-Y TO WS-X.\n"
        "           DISPLAY WS-X.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "12")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_then():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. COND.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-X  PIC 9 VALUE 7.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           IF WS-X > 5\n"
        "               DISPLAY 'BIG'\n"
        "           END-IF.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "BIG")


def test_perform_varying():
    # COBOL loop using MOVE + DISPLAY sequence
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. LOOP.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-I  PIC 9 VALUE 0.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           MOVE 1 TO WS-I.\n"
        "           DISPLAY WS-I.\n"
        "           MOVE 2 TO WS-I.\n"
        "           DISPLAY WS-I.\n"
        "           MOVE 3 TO WS-I.\n"
        "           DISPLAY WS-I.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_string_variable():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. STRTEST.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-NAME  PIC X(20) VALUE 'HELLO'.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           DISPLAY WS-NAME.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "HELLO")


def test_add_to():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. ADDTO.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-A  PIC 9(3) VALUE 0.\n"
        "           01 WS-B  PIC 9(3) VALUE 0.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           ADD 10 TO WS-A.\n"
        "           ADD 20 TO WS-B.\n"
        "           DISPLAY WS-A.\n"
        "           DISPLAY WS-B.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "10", "20")


def test_if_greater():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. IFTEST.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-X  PIC 9(3) VALUE 42.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           IF WS-X > 10\n"
        "               DISPLAY 'BIG'\n"
        "           ELSE\n"
        "               DISPLAY 'SMALL'\n"
        "           END-IF.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "BIG")


def test_subtract():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SUB.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-A  PIC 9(3) VALUE 15.\n"
        "           01 WS-B  PIC 9(3) VALUE 6.\n"
        "           01 WS-R  PIC 9(3).\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           SUBTRACT WS-B FROM WS-A GIVING WS-R.\n"
        "           DISPLAY WS-R.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "9")


def test_display_literal():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. LIT.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           DISPLAY 'Hello World'.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "Hello World")


def test_move_variable():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. MOV.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-A  PIC 9(3) VALUE 0.\n"
        "           01 WS-B  PIC 9(3) VALUE 99.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           MOVE WS-B TO WS-A.\n"
        "           DISPLAY WS-A.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "99")


def test_subtract_from():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. T.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 X PIC 9(3) VALUE 20.\n"
        "           01 Y PIC 9(3) VALUE 5.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           SUBTRACT Y FROM X.\n"
        "           DISPLAY X.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "15")


def test_multiply_giving():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. T.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 X PIC 9(3) VALUE 6.\n"
        "           01 Y PIC 9(3) VALUE 7.\n"
        "           01 Z PIC 9(3) VALUE 0.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           MULTIPLY X BY Y GIVING Z.\n"
        "           DISPLAY Z.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "42")


def test_display_string_literal():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. T.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           DISPLAY 'HELLO COBOL'.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "HELLO COBOL")


def test_divide_giving():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. T.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 X PIC 9(3) VALUE 20.\n"
        "           01 Y PIC 9(3) VALUE 4.\n"
        "           01 Z PIC 9(3) VALUE 0.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           DIVIDE X BY Y GIVING Z.\n"
        "           DISPLAY Z.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "5")


def test_display_number():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. T.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           DISPLAY 42.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_evaluate_when():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. EVALTEST.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-GRADE  PIC 9 VALUE 2.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           EVALUATE WS-GRADE\n"
        "               WHEN 1\n"
        "                   DISPLAY 'ONE'\n"
        "               WHEN 2\n"
        "                   DISPLAY 'TWO'\n"
        "               WHEN OTHER\n"
        "                   DISPLAY 'OTHER'\n"
        "           END-EVALUATE.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "TWO")


def test_evaluate_true():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. EVALTEST2.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-SCORE  PIC 9(3) VALUE 85.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           EVALUATE TRUE\n"
        "               WHEN WS-SCORE >= 90\n"
        "                   DISPLAY 'A'\n"
        "               WHEN WS-SCORE >= 80\n"
        "                   DISPLAY 'B'\n"
        "               WHEN OTHER\n"
        "                   DISPLAY 'C'\n"
        "           END-EVALUATE.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "B")


# ---------------------------------------------------------------------------
# Additional arithmetic
# ---------------------------------------------------------------------------


def test_subtract():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. SUB.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-A  PIC 9(3) VALUE 15.\n"
        "           01 WS-B  PIC 9(3) VALUE 4.\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           SUBTRACT WS-B FROM WS-A.\n"
        "           DISPLAY WS-A.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "11")


def test_multiply():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. MUL.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-A  PIC 9(3) VALUE 6.\n"
        "           01 WS-B  PIC 9(3) VALUE 7.\n"
        "           01 WS-R  PIC 9(3).\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           MULTIPLY WS-A BY WS-B GIVING WS-R.\n"
        "           DISPLAY WS-R.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "42")


def test_divide():
    out = cobol(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. DIV.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "           01 WS-A  PIC 9(3) VALUE 20.\n"
        "           01 WS-B  PIC 9(3) VALUE 4.\n"
        "           01 WS-R  PIC 9(3).\n"
        "       PROCEDURE DIVISION.\n"
        "       MAIN-PARA.\n"
        "           DIVIDE WS-A BY WS-B GIVING WS-R.\n"
        "           DISPLAY WS-R.\n"
        "           STOP RUN."
    )
    assert no_errors(out)
    assert has(out, "5")


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "cobol" / "hello.cob"
    ).read_text()
    out = cobol(src)
    assert no_errors(out)
    assert ok(out)
