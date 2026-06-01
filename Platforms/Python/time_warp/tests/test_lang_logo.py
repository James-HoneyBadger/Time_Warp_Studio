"""Comprehensive tests for the Logo executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.LOGO


def logo(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    # Logo outputs string words with leading " and uppercased
    out = logo('PRINT "Hello')
    assert no_errors(out)
    assert has(out, "HELLO")


def test_print_number():
    out = logo("PRINT 42")
    assert no_errors(out)
    assert has(out, "42")


def test_print_expression():
    # Logo does not evaluate inline arithmetic in PRINT; use MAKE to compute
    out = logo('MAKE "R 7\nPRINT :R')
    assert no_errors(out)
    assert has(out, "7")


# ---------------------------------------------------------------------------
# Variables (MAKE / :var)
# ---------------------------------------------------------------------------


def test_make_variable():
    out = logo('MAKE "X 10\nPRINT :X')
    assert no_errors(out)
    assert has(out, "10")


def test_variable_arithmetic():
    # Arithmetic must be computed via MAKE then printed
    out = logo('MAKE "A 6\nMAKE "B 7\nMAKE "C :A * :B\nPRINT :C')
    assert no_errors(out)
    assert has(out, "42")


def test_string_variable():
    # Logo uppercases word output
    out = logo('MAKE "NAME "World\nPRINT :NAME')
    assert no_errors(out)
    assert has(out, "WORLD")


# ---------------------------------------------------------------------------
# Turtle graphics (basic — just check no errors)
# ---------------------------------------------------------------------------


def test_forward():
    out = logo("FORWARD 100")
    assert no_errors(out)


def test_right_turn():
    out = logo("RIGHT 90")
    assert no_errors(out)


def test_penup_pendown():
    out = logo("PENUP\nFORWARD 50\nPENDOWN\nFORWARD 50")
    assert no_errors(out)


def test_home():
    out = logo("FORWARD 50\nHOME")
    assert no_errors(out)


def test_clearscreen():
    out = logo("CLEARSCREEN")
    assert no_errors(out)


def test_repeat():
    out = logo("REPEAT 4 [FORWARD 50 RIGHT 90]")
    assert no_errors(out)


# ---------------------------------------------------------------------------
# Procedures
# ---------------------------------------------------------------------------


def test_to_procedure():
    out = logo(
        "TO SQUARE :S\n"
        "  REPEAT 4 [FORWARD :S RIGHT 90]\n"
        "END\n"
        "SQUARE 50"
    )
    assert no_errors(out)


# ---------------------------------------------------------------------------
# String functions
# ---------------------------------------------------------------------------


def test_first_last():
    # FIRST returns first letter, LAST returns last letter (uppercased)
    out = logo('PRINT FIRST "Hello\nPRINT LAST "Hello')
    assert no_errors(out)
    assert has(out, "H", "O")


def test_if_condition():
    out = logo('MAKE "X 10\nIF :X > 5 [PRINT 1]')
    assert no_errors(out)
    assert has(out, "1")


def test_ifelse_true():
    out = logo('MAKE "N 8\nIFELSE :N > 5 [PRINT 1] [PRINT 0]')
    assert no_errors(out)
    assert has(out, "1")


def test_ifelse_false():
    out = logo('MAKE "N 2\nIFELSE :N > 5 [PRINT 1] [PRINT 0]')
    assert no_errors(out)
    assert has(out, "0")


def test_make_variable_float():
    out = logo('make "x 5\nprint :x')
    assert no_errors(out)
    assert has(out, "5")


def test_word_concat():
    out = logo('print word "hello "world')
    assert no_errors(out)
    assert has(out, "HELLOWORLD")


def test_repeat_accumulate():
    out = logo('make "total 0\nrepeat 3 [make "total :total + 10]\nprint :total')
    assert no_errors(out)
    assert has(out, "30")


def test_sum_builtin():
    out = logo('print sum 2 3')
    assert no_errors(out)
    assert has(out, "5")


def test_product_builtin():
    out = logo('print product 4 5')
    assert no_errors(out)
    assert has(out, "20")


def test_difference_builtin():
    out = logo('print difference 10 3')
    assert no_errors(out)
    assert has(out, "7")


def test_product():
    out = logo('print product 6 7')
    assert no_errors(out)
    assert has(out, "42")


def test_quotient():
    out = logo('print quotient 12 3')
    assert no_errors(out)
    assert has(out, "4")


def test_sum_large():
    out = logo('print sum 100 200')
    assert no_errors(out)
    assert has(out, "300")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "logo" / "hello.logo"
    ).read_text()
    out = logo(src)
    assert no_errors(out)
    assert ok(out)
