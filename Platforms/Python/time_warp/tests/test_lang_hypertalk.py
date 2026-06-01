"""Comprehensive tests for the HyperTalk executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.HYPERTALK


def ht(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = ht('answer "Hello, World!"')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_multiple_output():
    out = ht('answer "Alpha"\nanswer "Beta"')
    assert no_errors(out)
    assert has(out, "Alpha", "Beta")


# ---------------------------------------------------------------------------
# Variables (put ... into)
# ---------------------------------------------------------------------------


def test_put_variable():
    out = ht('put 42 into x\nanswer x')
    assert no_errors(out)
    assert has(out, "42")


def test_put_string():
    out = ht('put "World" into name\nanswer name')
    assert no_errors(out)
    assert has(out, "World")


def test_arithmetic():
    out = ht(
        "put 10 into a\n"
        "put 3 into b\n"
        "answer a + b\n"
        "answer a * b\n"
        "answer a mod b\n"
    )
    assert no_errors(out)
    assert has(out, "13", "30", "1")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_then():
    out = ht(
        "put 7 into x\n"
        "if x > 5 then answer \"BIG\""
    )
    assert no_errors(out)
    assert has(out, "BIG")


def test_if_then_else():
    out = ht(
        "put 3 into x\n"
        "if x > 5 then\n"
        "  answer \"BIG\"\n"
        "else\n"
        "  answer \"SMALL\"\n"
        "end if"
    )
    assert no_errors(out)
    assert has(out, "SMALL")


def test_repeat_times():
    out = ht(
        "put 0 into count\n"
        "repeat 3 times\n"
        "  put count + 1 into count\n"
        "  answer count\n"
        "end repeat"
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


# ---------------------------------------------------------------------------
# Handlers (on ... end)
# ---------------------------------------------------------------------------


def test_on_handler():
    out = ht(
        'on greet\n'
        '  answer "Hello from handler!"\n'
        'end greet\n'
        'greet'
    )
    assert no_errors(out)
    assert has(out, "Hello from handler!")


# ---------------------------------------------------------------------------
# Additional features
# ---------------------------------------------------------------------------


def test_repeat_while():
    out = ht(
        "put 0 into x\n"
        "repeat while x < 3\n"
        "  put x + 1 into x\n"
        "  put x\n"
        "end repeat"
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_math_functions():
    out = ht(
        "put abs(-5)\n"
        "put sqrt(16)\n"
    )
    assert no_errors(out)
    assert has(out, "5")
    assert has(out, "4")


def test_string_concatenation():
    out = ht('put "Hello" && "World"')
    assert no_errors(out)
    assert has(out, "Hello World")


def test_repeat_times():
    out = ht(
        "put 0 into counter\n"
        "repeat 4 times\n"
        "  put counter + 1 into counter\n"
        "end repeat\n"
        "put counter"
    )
    assert no_errors(out)
    assert has(out, "4")


def test_item_of():
    out = ht('put item 2 of "alpha,beta,gamma"')
    assert no_errors(out)
    assert has(out, "beta")


def test_modulo():
    out = ht("put 17 mod 5")
    assert no_errors(out)
    assert has(out, "2")


def test_chartonum():
    out = ht("put charToNum('A')")
    assert no_errors(out)
    assert has(out, "65")


def test_if_then_else():
    out = ht(
        "put 7 into x\n"
        "if x > 5 then\n"
        "  put \"big\"\n"
        "else\n"
        "  put \"small\"\n"
        "end if"
    )
    assert no_errors(out)
    assert has(out, "big")


def test_number_of_items():
    out = ht(
        'put "a,b,c,d" into myList\n'
        "put the number of items in myList into n\n"
        "put n"
    )
    assert no_errors(out)
    assert has(out, "4")


def test_numtochar():
    out = ht("put numToChar(65)")
    assert no_errors(out)
    assert has(out, "A")


def test_string_concat():
    out = ht('put "Hello" & " " & "World"')
    assert no_errors(out)
    assert has(out, "Hello World")


def test_repeat_with():
    out = ht("repeat with i = 1 to 3\n  put i\nend repeat")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_string_length_fn():
    out = ht('put length("Hello World") into x\nput x')
    assert no_errors(out)
    assert has(out, "11")


def test_number_of_words():
    out = ht('put the number of words of "one two three" into n\nput n')
    assert no_errors(out)
    assert has(out, "3")


def test_ampersand_concat():
    out = ht('put "Hello" & " " & "World" into s\nput s')
    assert no_errors(out)
    assert has(out, "Hello World")


def test_put_arithmetic():
    out = ht("put 3 + 4 into x\nput x")
    assert no_errors(out)
    assert has(out, "7")


def test_repeat_times():
    out = ht("repeat 3 times\nput \"hi\"\nend repeat")
    assert no_errors(out)
    assert has(out, "hi")


def test_if_then_else():
    out = ht("put 10 into x\nif x > 5 then\nput \"big\"\nelse\nput \"small\"\nend if")
    assert no_errors(out)
    assert has(out, "big")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "hypertalk" / "hello.htalk"
    ).read_text()
    out = ht(src)
    assert no_errors(out)
    assert ok(out)
