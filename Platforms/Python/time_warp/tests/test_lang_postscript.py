"""Comprehensive tests for the PostScript executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.POSTSCRIPT


def ps(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = ps("(Hello, World!) =")
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_print_string():
    out = ps("(Welcome to PostScript!) =")
    assert no_errors(out)
    assert has(out, "Welcome to PostScript!")


def test_multiple_output():
    out = ps("(Alpha) =\n(Beta) =")
    assert no_errors(out)
    assert has(out, "Alpha", "Beta")


# ---------------------------------------------------------------------------
# Stack operations and arithmetic
# ---------------------------------------------------------------------------


def test_add():
    out = ps("3 4 add =")
    assert no_errors(out)
    assert has(out, "7")


def test_sub():
    out = ps("10 3 sub =")
    assert no_errors(out)
    assert has(out, "7")


def test_mul():
    out = ps("6 7 mul =")
    assert no_errors(out)
    assert has(out, "42")


def test_dup():
    out = ps("5 dup add =")
    assert no_errors(out)
    assert has(out, "10")


def test_exch():
    out = ps("1 2 exch = =")
    assert no_errors(out)
    assert has(out, "1", "2")


# ---------------------------------------------------------------------------
# Variables (def)
# ---------------------------------------------------------------------------


def test_def_variable():
    out = ps("/x 42 def\nx =")
    assert no_errors(out)
    assert has(out, "42")


def test_def_procedure():
    out = ps(
        "/double { 2 mul } def\n"
        "5 double ="
    )
    assert no_errors(out)
    assert has(out, "10")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_true():
    out = ps("true { (YES) = } if")
    assert no_errors(out)
    assert has(out, "YES")


def test_ifelse():
    out = ps("5 3 gt { (BIG) = } { (SMALL) = } ifelse")
    assert no_errors(out)
    assert has(out, "BIG")


def test_repeat():
    out = ps("3 { (x) = } repeat")
    assert no_errors(out)
    assert has(out, "x")


def test_for_loop():
    out = ps("1 1 3 { = } for")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


# ---------------------------------------------------------------------------
# Math
# ---------------------------------------------------------------------------


def test_sqrt():
    out = ps("16 sqrt =")
    assert no_errors(out)
    assert has(out, "4")


def test_mod():
    out = ps("17 5 mod =")
    assert no_errors(out)
    assert has(out, "2")


def test_abs():
    out = ps("-42 abs =")
    assert no_errors(out)
    assert has(out, "42")


def test_div_real():
    out = ps("10 4 div =")
    assert no_errors(out)
    assert has(out, "2.5")


def test_dup_mul():
    out = ps("10 dup mul =")
    assert no_errors(out)
    assert has(out, "100")


def test_variable_mul():
    out = ps("/x 5 def\n/y 3 def\nx y mul =")
    assert no_errors(out)
    assert has(out, "15")


def test_string_length():
    out = ps("(Hello) length =")
    assert no_errors(out)
    assert has(out, "5")


def test_add_integers():
    out = ps("3 4 add =")
    assert no_errors(out)
    assert has(out, "7")


def test_divide():
    out = ps("10 2 div =")
    assert no_errors(out)
    assert has(out, "5")


def test_exponent():
    out = ps("2 10 exp =")
    assert no_errors(out)
    assert has(out, "1024")


def test_negation():
    out = ps("10 neg =")
    assert no_errors(out)
    assert has(out, "-10")


def test_absolute_value():
    out = ps("-5 abs =")
    assert no_errors(out)
    assert has(out, "5")


def test_subtract():
    out = ps("10 3 sub =")
    assert no_errors(out)
    assert has(out, "7")


def test_multiply():
    out = ps("6 7 mul =")
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "postscript" / "hello.ps"
    ).read_text()
    out = ps(src)
    assert no_errors(out)
    assert ok(out)
