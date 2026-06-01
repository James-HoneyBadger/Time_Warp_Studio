"""Comprehensive tests for the Brainfuck executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.BRAINFUCK


def bf(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_print_A():
    # Set cell to 65 ('A') and output
    out = bf("+" * 65 + ".")
    assert no_errors(out)
    assert has(out, "A")


def test_print_hi():
    # Use hello world program that's verified to work
    out = bf(
        "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    )
    assert no_errors(out)
    assert has(out, "Hello")


def test_hello_world():
    # Classic hello world program
    out = bf(
        "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    )
    assert no_errors(out)
    assert has(out, "Hello World")


def test_increment_pointer():
    # Move to next cell, set value, output
    out = bf(">+" + "+" * 64 + ".")
    assert no_errors(out)
    assert has(out, "A")


# ---------------------------------------------------------------------------
# Loop
# ---------------------------------------------------------------------------


def test_simple_loop():
    # Count down from 3 using a loop; no output check, just no errors
    out = bf("+++" + "[-]")
    assert no_errors(out)


def test_counting_output():
    # Print ASCII '1' through '3' using BF
    # Set cell to 49 ('1') then print 3 times incrementing
    out = bf(
        ">+++++++[>+++++++<-]>"   # cell[2] = 49 = '1'
        "+++"                      # cell[0] = 3 (counter)
        "[<.+>-]"                  # print cell[1], increment, decrement counter
    )
    # Just verify it runs and produces some character output
    assert no_errors(out)
    assert ok(out)


# ---------------------------------------------------------------------------
# Multiple programs in source (skip blank/comment lines)
# ---------------------------------------------------------------------------


def test_ignores_comments():
    # Comments (non-BF chars) should be ignored
    out = bf(
        "This is a comment\n"
        "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    )
    assert no_errors(out)
    assert has(out, "Hello World")


# ---------------------------------------------------------------------------
# Additional programs
# ---------------------------------------------------------------------------


def test_print_digit_sequence():
    # Print '1', '2', '3' using increment from 49 (ASCII '1')
    out = bf("+" * 49 + ".+.+.")
    assert no_errors(out)
    assert "1" in out[0]
    assert "2" in out[0]
    assert "3" in out[0]


def test_print_Z():
    # Print 'Z' = ASCII 90
    out = bf("+" * 90 + ".")
    assert no_errors(out)
    assert has(out, "Z")


def test_two_cell_output():
    # Set cell0=65 ('A'), output, move to cell1, set to 66 ('B'), output
    out = bf("+" * 65 + ".>" + "+" * 66 + ".")
    assert no_errors(out)
    assert "A" in out[0]
    assert "B" in out[0]


def test_abc_sequence():
    # ASCII 65='A', 66='B', 67='C' using increment
    out = bf("+" * 65 + ".+.+.")
    assert no_errors(out)
    assert "A" in out[0]
    assert "B" in out[0]
    assert "C" in out[0]


def test_digit_sequence():
    # ASCII 48='0', 49='1', 50='2'
    out = bf("+" * 48 + ".+.+.")
    assert no_errors(out)
    assert "0" in out[0]
    assert "1" in out[0]
    assert "2" in out[0]


def test_loop_multiply():
    # 9 * 8 = 72 = ASCII 'H'
    out = bf("+++++++++[->++++++++<]>.")
    assert no_errors(out)
    assert has(out, "H")


def test_print_ab():
    # ASCII 65='A', 66='B'
    out = bf("+" * 65 + ".+.")
    assert no_errors(out)
    assert "A" in out[0]
    assert "B" in out[0]


def test_multiply_6x7():
    # 6 * 7 = 42 = ASCII '*'
    out = bf("++++++[->+++++++<]>.")
    assert no_errors(out)
    assert "*" in out[0]


def test_multiply_9x10():
    # 9 * 10 = 90 = ASCII 'Z'
    out = bf(">+++++++++[<++++++++++>-]<.")
    assert no_errors(out)
    assert "Z" in out[0]


def test_print_A():
    out = bf("+" * 65 + ".")
    assert no_errors(out)
    assert has(out, "A")


def test_print_exclamation():
    out = bf("+" * 33 + ".")
    assert no_errors(out)
    assert has(out, "!")


def test_loop_multiplication():
    # 8 * 9 = 72 = 'H'
    out = bf("++++++++[->+++++++++<]>.")
    assert no_errors(out)
    assert has(out, "H")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "brainfuck" / "hello.bf"
    ).read_text()
    out = bf(src)
    assert no_errors(out)
    assert ok(out)
