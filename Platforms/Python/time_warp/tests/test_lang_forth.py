"""Comprehensive tests for the Forth executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.FORTH


def forth(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = forth('." Hello, World!" CR')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_dot_print():
    out = forth("42 . CR")
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Stack operations
# ---------------------------------------------------------------------------


def test_dup():
    out = forth("5 DUP . . CR")
    assert no_errors(out)
    assert has(out, "5")


def test_swap():
    out = forth("1 2 SWAP . . CR")
    assert no_errors(out)
    assert has(out, "2", "1")


def test_drop():
    out = forth("1 2 DROP . CR")
    assert no_errors(out)
    assert has(out, "1")


def test_over():
    out = forth("1 2 OVER . CR")
    assert no_errors(out)
    assert has(out, "1")


# ---------------------------------------------------------------------------
# Arithmetic
# ---------------------------------------------------------------------------


def test_addition():
    out = forth("3 4 + . CR")
    assert no_errors(out)
    assert has(out, "7")


def test_subtraction():
    out = forth("10 3 - . CR")
    assert no_errors(out)
    assert has(out, "7")


def test_multiplication():
    out = forth("6 7 * . CR")
    assert no_errors(out)
    assert has(out, "42")


def test_division():
    out = forth("10 2 / . CR")
    assert no_errors(out)
    assert has(out, "5")


def test_modulo():
    out = forth("10 3 MOD . CR")
    assert no_errors(out)
    assert has(out, "1")


# ---------------------------------------------------------------------------
# Word definitions
# ---------------------------------------------------------------------------


def test_define_word():
    out = forth(": DOUBLE DUP + ;\n5 DOUBLE . CR")
    assert no_errors(out)
    assert has(out, "10")


def test_nested_definition():
    out = forth(
        ": SQUARE DUP * ;\n"
        ": CUBE DUP SQUARE * ;\n"
        "3 CUBE . CR"
    )
    assert no_errors(out)
    assert has(out, "27")


# ---------------------------------------------------------------------------
# Conditionals
# ---------------------------------------------------------------------------


def test_if_else():
    out = forth('5 3 > IF ." BIG" ELSE ." SMALL" THEN CR')
    assert no_errors(out)
    assert has(out, "BIG")


# ---------------------------------------------------------------------------
# Loops
# ---------------------------------------------------------------------------


def test_do_loop():
    # Forth DO..LOOP: 'limit start DO ... LOOP' — stack order: limit on NOS, start on TOS
    # '4 1 DO I . LOOP' iterates I = 1, 2, 3
    out = forth("4 1 DO I . LOOP CR")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_greater_than():
    # In Forth, -1 is TRUE, 0 is FALSE
    out = forth("5 3 > .")
    assert no_errors(out)
    assert has(out, "-1")


def test_equal():
    out = forth("7 7 = .")
    assert no_errors(out)
    assert has(out, "-1")


def test_variable():
    out = forth("VARIABLE X\n10 X !\nX @ .")
    assert no_errors(out)
    assert has(out, "10")


def test_dup_stack():
    out = forth("5 dup . .")
    assert no_errors(out)
    assert has(out, "5")


def test_modulo():
    out = forth("10 3 mod .")
    assert no_errors(out)
    assert has(out, "1")


def test_do_loop():
    out = forth("5 0 do i . loop")
    assert no_errors(out)
    assert has(out, "0", "1", "2", "3", "4")


def test_swap():
    out = forth("3 5 swap . .")
    assert no_errors(out)
    assert has(out, "3", "5")


def test_abs_negative():
    out = forth("-7 abs .")
    assert no_errors(out)
    assert has(out, "7")


def test_multiply_add():
    # (2 + 3) * 4 = 20
    out = forth("2 3 + 4 * .")
    assert no_errors(out)
    assert has(out, "20")


def test_dup_add():
    out = forth("5 DUP + .")
    assert no_errors(out)
    assert has(out, "10")


def test_swap_top():
    out = forth("3 4 SWAP .")
    assert no_errors(out)
    assert has(out, "3")


def test_rot_top():
    out = forth("1 2 3 ROT .")
    assert no_errors(out)
    assert has(out, "1")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "forth" / "hello.f"
    ).read_text()
    out = forth(src)
    assert no_errors(out)
    assert ok(out)
