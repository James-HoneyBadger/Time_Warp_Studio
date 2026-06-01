"""Tests for the REXX language executor."""
from __future__ import annotations
import pytest
from .conftest_lang import run, ok, has, no_errors
from ..core.interpreter import Language

REXX = Language.REXX


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------

def test_hello_world():
    out = run("SAY 'Hello, World!'", REXX)
    assert no_errors(out) and has(out, "Hello, World!")


def test_say_number():
    out = run("SAY 42", REXX)
    assert no_errors(out) and has(out, "42")


def test_say_empty():
    out = run("SAY ''", REXX)
    assert no_errors(out)


# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------

def test_variable_assignment():
    out = run("x = 'REXX'\nSAY x", REXX)
    assert no_errors(out) and has(out, "REXX")


def test_uninit_variable():
    """Uninitialised variable evaluates to its own name (uppercase)."""
    out = run("SAY NOVAR", REXX)
    assert no_errors(out) and has(out, "NOVAR")


def test_variable_reassign():
    out = run("x = 10\nx = x + 5\nSAY x", REXX)
    assert no_errors(out) and has(out, "15")


# ---------------------------------------------------------------------------
# Arithmetic
# ---------------------------------------------------------------------------

def test_addition():
    out = run("x = 3 + 4\nSAY x", REXX)
    assert no_errors(out) and has(out, "7")


def test_subtraction():
    out = run("x = 10 - 3\nSAY x", REXX)
    assert no_errors(out) and has(out, "7")


def test_multiplication():
    out = run("x = 6 * 7\nSAY x", REXX)
    assert no_errors(out) and has(out, "42")


def test_division():
    out = run("x = 10 / 2\nSAY x", REXX)
    assert no_errors(out) and has(out, "5")


def test_integer_division():
    out = run("x = 10 % 3\nSAY x", REXX)
    assert no_errors(out) and has(out, "3")


def test_remainder():
    out = run("x = 10 // 3\nSAY x", REXX)
    assert no_errors(out) and has(out, "1")


def test_power():
    out = run("x = 2 ** 8\nSAY x", REXX)
    assert no_errors(out) and has(out, "256")


# ---------------------------------------------------------------------------
# String operations
# ---------------------------------------------------------------------------

def test_string_concat():
    out = run("a = 'Hello'\nb = ' World'\nSAY a || b", REXX)
    assert no_errors(out) and has(out, "Hello World")


def test_length():
    out = run("SAY LENGTH('Hello')", REXX)
    assert no_errors(out) and has(out, "5")


def test_upper():
    out = run("SAY UPPER('hello')", REXX)
    assert no_errors(out) and has(out, "HELLO")


def test_lower():
    out = run("SAY LOWER('HELLO')", REXX)
    assert no_errors(out) and has(out, "hello")


def test_substr():
    out = run("SAY SUBSTR('Hello World', 7, 5)", REXX)
    assert no_errors(out) and has(out, "World")


def test_reverse():
    out = run("SAY REVERSE('abc')", REXX)
    assert no_errors(out) and has(out, "cba")


def test_copies():
    out = run("SAY COPIES('ab', 3)", REXX)
    assert no_errors(out) and has(out, "ababab")


def test_strip():
    out = run("SAY STRIP('  hello  ')", REXX)
    assert no_errors(out) and has(out, "hello")


def test_pos():
    out = run("SAY POS('ll', 'Hello')", REXX)
    assert no_errors(out) and has(out, "3")


# ---------------------------------------------------------------------------
# Conditionals
# ---------------------------------------------------------------------------

def test_if_true():
    out = run("x = 5\nIF x > 3 THEN SAY 'big'", REXX)
    assert no_errors(out) and has(out, "big")


def test_if_false():
    out = run("x = 1\nIF x > 3 THEN SAY 'big'\nSAY 'done'", REXX)
    assert no_errors(out) and has(out, "done")
    assert not has(out, "big")


def test_if_else():
    out = run("x = 1\nIF x > 3 THEN SAY 'big' ELSE SAY 'small'", REXX)
    assert no_errors(out) and has(out, "small")


# ---------------------------------------------------------------------------
# DO loops
# ---------------------------------------------------------------------------

def test_do_counted():
    out = run("DO i = 1 TO 5\n  SAY i\nEND", REXX)
    assert no_errors(out) and has(out, "1", "5")


def test_do_counted_by():
    out = run("DO i = 0 TO 10 BY 2\n  SAY i\nEND", REXX)
    assert no_errors(out) and has(out, "0", "10")


def test_do_while():
    out = run("x = 1\nDO WHILE x <= 3\n  SAY x\n  x = x + 1\nEND", REXX)
    assert no_errors(out) and has(out, "1", "3")


def test_do_until():
    out = run("x = 1\nDO UNTIL x > 3\n  SAY x\n  x = x + 1\nEND", REXX)
    assert no_errors(out) and has(out, "1", "3")


def test_leave():
    out = run("DO i = 1 TO 10\n  IF i = 5 THEN LEAVE\n  SAY i\nEND", REXX)
    assert no_errors(out) and has(out, "4")
    assert not has(out, "5")


def test_iterate():
    out = run("DO i = 1 TO 5\n  IF i = 3 THEN ITERATE\n  SAY i\nEND", REXX)
    assert no_errors(out) and has(out, "1", "2", "4", "5")
    assert not has(out, "3")


# ---------------------------------------------------------------------------
# SELECT / WHEN
# ---------------------------------------------------------------------------

def test_select_when():
    src = """\
x = 2
SELECT
  WHEN x = 1 THEN SAY 'one'
  WHEN x = 2 THEN SAY 'two'
  WHEN x = 3 THEN SAY 'three'
  OTHERWISE SAY 'other'
END
"""
    out = run(src, REXX)
    assert no_errors(out) and has(out, "two")


def test_select_otherwise():
    src = """\
x = 99
SELECT
  WHEN x = 1 THEN SAY 'one'
  OTHERWISE SAY 'other'
END
"""
    out = run(src, REXX)
    assert no_errors(out) and has(out, "other")


# ---------------------------------------------------------------------------
# Subroutines
# ---------------------------------------------------------------------------

def test_subroutine_call():
    src = """\
CALL GREET
EXIT

GREET:
SAY 'Hello from subroutine!'
RETURN
"""
    out = run(src, REXX)
    assert no_errors(out) and has(out, "Hello from subroutine!")


def test_subroutine_return_value():
    src = """\
CALL DOUBLE 5
SAY RESULT

DOUBLE:
n = ARG1
RETURN n * 2
"""
    out = run(src, REXX)
    assert no_errors(out) and has(out, "10")


# ---------------------------------------------------------------------------
# Built-in functions
# ---------------------------------------------------------------------------

def test_abs():
    out = run("SAY ABS(-42)", REXX)
    assert no_errors(out) and has(out, "42")


def test_max_min():
    out = run("SAY MAX(3, 7, 2)\nSAY MIN(3, 7, 2)", REXX)
    assert no_errors(out) and has(out, "7", "2")


def test_word_words():
    out = run("s = 'one two three'\nSAY WORDS(s)\nSAY WORD(s, 2)", REXX)
    assert no_errors(out) and has(out, "3", "two")


def test_datatype_number():
    out = run("SAY DATATYPE('42', 'N')", REXX)
    assert no_errors(out) and has(out, "1")


def test_datatype_not_number():
    out = run("SAY DATATYPE('abc', 'N')", REXX)
    assert no_errors(out) and has(out, "0")


def test_center():
    out = run("SAY CENTER('Hi', 10, '-')", REXX)
    assert no_errors(out) and has(out, "----Hi----")


def test_sign():
    out = run("SAY SIGN(-5)\nSAY SIGN(0)\nSAY SIGN(3)", REXX)
    assert no_errors(out) and has(out, "-1", "0", "1")


def test_changestr():
    out = run("SAY CHANGESTR('o', 'Hello World', '0')", REXX)
    assert no_errors(out) and has(out, "Hell0 W0rld")
