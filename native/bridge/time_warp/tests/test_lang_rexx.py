"""Comprehensive tests for the REXX executor."""

from __future__ import annotations

from .conftest_lang import first_error, has, no_errors, run

from time_warp.core.interpreter import Language

LANG = Language.REXX


def rexx(source: str, input_val: str = "4") -> list[str]:
    return run(source, LANG, input_val=input_val)


def test_say_string_literal():
    out = rexx("SAY 'Hello, World!'\n")
    assert out == ["Hello, World!"]
    assert no_errors(out)


def test_say_arithmetic():
    assert rexx("SAY 6 * 7\n") == ["42"]


def test_assignment_and_concat_operator():
    assert rexx("x = 5\nSAY 'x=' || x\n") == ["x=5"]


def test_abuttal_concatenation():
    assert rexx("SAY 'AB' 'CD'\n") == ["ABCD"]
    assert rexx("x = 5\nSAY 'x=' x\n") == ["x=5"]


def test_do_loop_counts():
    assert rexx("DO i = 1 TO 5\n  SAY i\nEND\n") == ["1", "2", "3", "4", "5"]


def test_do_loop_with_by_step():
    assert rexx("DO i = 10 TO 1 BY -3\n  SAY i\nEND\n") == ["10", "7", "4", "1"]


def test_do_while():
    assert rexx("x = 0\nDO WHILE x < 3\n  x = x + 1\n  SAY x\nEND\n") == [
        "1",
        "2",
        "3",
    ]


def test_do_until():
    assert rexx("x = 0\nDO UNTIL x >= 3\n  x = x + 1\n  SAY x\nEND\n") == [
        "1",
        "2",
        "3",
    ]


def test_do_over_words():
    assert rexx("DO w OVER 'a b c'\n  SAY w\nEND\n") == ["a", "b", "c"]


def test_if_then_else_block():
    src = "IF 1 > 2 THEN DO\n  SAY 'a'\nEND\nELSE DO\n  SAY 'b'\nEND\n"
    assert rexx(src) == ["b"]


def test_if_single_line():
    assert rexx("IF 2 > 1 THEN SAY 'yes'\n") == ["yes"]


def test_select_when_otherwise():
    src = (
        "x = 42\nSELECT\n  WHEN x > 100 THEN SAY 'huge'\n"
        "  WHEN x > 10 THEN SAY 'big'\n  OTHERWISE SAY 'small'\nEND\n"
    )
    assert rexx(src) == ["big"]


def test_builtin_string_functions():
    assert rexx("SAY LENGTH('hello')\n") == ["5"]
    assert rexx("SAY UPPER('rexx')\n") == ["REXX"]
    assert rexx("SAY LOWER('REXX')\n") == ["rexx"]
    assert rexx("SAY SUBSTR('abcdef', 2, 3)\n") == ["bcd"]
    assert rexx("SAY REVERSE('abc')\n") == ["cba"]
    assert rexx("SAY WORD('one two three', 2)\n") == ["two"]
    assert rexx("SAY WORDS('one two three')\n") == ["3"]


def test_subroutine_call_and_return():
    src = (
        "SAY 'before'\nCALL greet\nSAY 'after'\nEXIT\n"
        "greet:\n  SAY 'hi'\n  RETURN\n"
    )
    assert rexx(src) == ["before", "hi", "after"]


def test_leave_breaks_out_of_loop():
    assert rexx("DO i = 1 TO 10\n  IF i = 3 THEN LEAVE\n  SAY i\nEND\n") == [
        "1",
        "2",
    ]


def test_iterate_continues_loop():
    src = "DO i = 1 TO 4\n  IF i = 2 THEN ITERATE\n  SAY i\nEND\n"
    assert rexx(src) == ["1", "3", "4"]


def test_comments_and_blank_lines():
    assert rexx("/* c */ SAY 'x'\n-- another\nSAY 'y'\n") == ["x", "y"]


def test_pull_reads_input():
    out = run("PULL name\nSAY 'hi' name\n", LANG, input_val="Ada")
    assert out == ["hiAda"]
    spaced = run(
        "PULL name\nSAY 'hi' || ' ' || name\n", LANG, input_val="Ada"
    )
    assert spaced == ["hi Ada"]


def test_parse_var_splits_words():
    src = (
        "line = 'one two three'\nPARSE VAR line a b c\n"
        "SAY c || ' ' || b || ' ' || a\n"
    )
    assert rexx(src) == ["three two one"]


def test_unknown_statement_reports_error():
    assert first_error(rexx("FROBULATE 3\n")) is not None


# -- regressions -------------------------------------------------------------


def test_space_honours_its_count_and_pad() -> None:
    """`SPACE(string, n, pad)` puts n pad characters between the words.

    The count was ignored, so SPACE('a b c', 0) answered 'a b c' — a plausible
    wrong answer rather than a missing one.
    """
    assert has(run("SAY SPACE('a b c', 3)", LANG), "a   b   c")
    assert has(run("SAY SPACE('a b c', 0)", LANG), "abc")
    assert has(run("SAY SPACE('a b c', 1, '.')", LANG), "a.b.c")
