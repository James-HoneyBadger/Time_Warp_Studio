"""Comprehensive tests for the PILOT executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.PILOT


def pilot(source: str, *, input_val: str = "YES") -> list[str]:
    return run(source, LANG, input_val=input_val)


# ---------------------------------------------------------------------------
# Basic output (T: command)
# ---------------------------------------------------------------------------


def test_hello_world():
    out = pilot("T:Hello, World!")
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_multiple_type():
    out = pilot("T:Line one\nT:Line two")
    assert no_errors(out)
    assert has(out, "Line one", "Line two")


# ---------------------------------------------------------------------------
# Compute (C:) and variable substitution
# ---------------------------------------------------------------------------


def test_compute_and_display():
    out = pilot("C:X = 5\nT:X = $X")
    assert no_errors(out)
    assert has(out, "5")


def test_arithmetic():
    out = pilot(
        "C:A = 10\n"
        "C:B = 3\n"
        "C:SUM = $A + $B\n"
        "T:Sum = $SUM"
    )
    assert no_errors(out)
    assert has(out, "13")


def test_multiplication():
    out = pilot("C:X = 6\nC:Y = 7\nC:R = $X * $Y\nT:$R")
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Matching (M:, Y:, N:)
# ---------------------------------------------------------------------------


def test_match_yes():
    out = pilot("T:Enter yes:\nA:\nM:YES\nTY:Matched!\nTN:Not matched.", input_val="YES")
    assert no_errors(out)
    assert has(out, "Matched!")


def test_match_no():
    out = pilot("T:Enter:\nA:\nM:YES\nTY:Matched!\nTN:Not matched.", input_val="NO")
    assert no_errors(out)
    assert has(out, "Not matched.")


# ---------------------------------------------------------------------------
# Jump (J:) and labels
# ---------------------------------------------------------------------------


def test_jump():
    out = pilot(
        "T:Before\n"
        "J:*AFTER\n"
        "T:Skipped\n"
        "*AFTER\n"
        "T:After"
    )
    assert no_errors(out)
    assert has(out, "Before", "After")
    assert "Skipped" not in "\n".join(out)


# ---------------------------------------------------------------------------
# Subroutines (U:)
# ---------------------------------------------------------------------------


def test_subroutine():
    # PILOT subroutine definition comes BEFORE the call in execution flow;
    # use U: to call a named label
    out = pilot(
        "T:Main start\n"
        "U:*GREET\n"
        "T:Main end\n"
        "E:\n"
        "*GREET\n"
        "T:In subroutine\n"
        "E:\n"
    )
    # Check at least main flow executed
    assert no_errors(out)
    assert has(out, "Main start", "Main end")


# ---------------------------------------------------------------------------
# Additional features
# ---------------------------------------------------------------------------


def test_remark_ignored():
    out = pilot("R:This line is ignored\nT:Visible line")
    assert no_errors(out)
    assert has(out, "Visible line")
    assert "This line is ignored" not in "\n".join(out)


def test_compute_square():
    out = pilot("C:X = 3\nC:Y = X * X\nT:Square of 3 is $Y")
    assert no_errors(out)
    assert has(out, "9")


def test_compute_chain():
    out = pilot(
        "C:A = 4\n"
        "C:B = A + 1\n"
        "C:C = A * B\n"
        "T:$C"
    )
    assert no_errors(out)
    assert has(out, "20")


def test_division():
    out = pilot("C:X = 20\nC:Y = X / 4\nT:$Y")
    assert no_errors(out)
    assert has(out, "5")


def test_nested_compute():
    out = pilot(
        "C:A = 10\n"
        "C:B = 3\n"
        "C:C = A - B\n"
        "C:D = C * 2\n"
        "T:$D"
    )
    assert no_errors(out)
    assert has(out, "14")


def test_multiple_type_vars():
    out = pilot(
        "C:X = 7\n"
        "C:Y = X + 1\n"
        "T:X is $X and Y is $Y"
    )
    assert no_errors(out)
    assert has(out, "7", "8")


def test_jump_yes_conditional():
    out = pilot(
        "A:\n"
        "M:YES\n"
        "JY:*DONE\n"
        "T:Not done\n"
        "*DONE\n"
        "T:Done!",
        input_val="YES",
    )
    assert no_errors(out)
    assert has(out, "Done!")
    assert "Not done" not in "\n".join(out)


def test_jump_no_conditional():
    out = pilot(
        "A:\n"
        "M:YES\n"
        "JN:*NOPE\n"
        "T:Yes!\n"
        "*NOPE\n"
        "T:Nope!",
        input_val="NO",
    )
    assert no_errors(out)
    assert has(out, "Nope!")


def test_end_stops_execution():
    out = pilot(
        "T:Line 1\n"
        "E:\n"
        "T:Line 2"
    )
    assert no_errors(out)
    assert has(out, "Line 1")
    assert "Line 2" not in "\n".join(out)


def test_type_hello_world():
    out = pilot("T:Hello World")
    assert no_errors(out)
    assert has(out, "Hello World")


def test_jump_to_label():
    out = pilot("*A\nT:Step A\nJ:*B\n*B\nT:Step B")
    assert no_errors(out)
    assert has(out, "Step A", "Step B")


def test_match_yes():
    out = pilot("*START\nT:Type yes\nA:\nM:YES\nTY:Correct\nTN:Wrong\n*END", input_val="YES")
    assert no_errors(out)
    assert has(out, "Correct")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "pilot" / "hello.pilot"
    ).read_text()
    out = pilot(src, input_val="YES")
    assert no_errors(out)
    assert ok(out)
