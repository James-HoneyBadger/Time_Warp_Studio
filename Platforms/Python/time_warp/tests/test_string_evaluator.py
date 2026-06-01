"""Tests for time_warp.utils.string_evaluator module."""

import pytest
from time_warp.utils.string_evaluator import StringExpressionEvaluator


def ev(expr: str, **vars_):
    """Helper: create evaluator with optional string/numeric variables."""
    string_vars = {k: v for k, v in vars_.items() if isinstance(v, str)}
    numeric_vars = {k: v for k, v in vars_.items() if isinstance(v, (int, float))}
    return StringExpressionEvaluator(
        string_variables=string_vars,
        numeric_variables=numeric_vars,
    ).evaluate(expr)


# ---------------------------------------------------------------------------
# String functions
# ---------------------------------------------------------------------------


def test_len_literal():
    assert ev('LEN("hello")') == "5"


def test_len_empty_string():
    assert ev('LEN("")') == "0"


def test_left_function():
    assert ev('LEFT("HELLO", 3)') == "HEL"


def test_right_function():
    assert ev('RIGHT("HELLO", 3)') == "LLO"


def test_mid_function():
    assert ev('MID("HELLO", 2, 3)') == "ELL"


def test_upper_function():
    assert ev('UPPER("hello world")') == "HELLO WORLD"


def test_lower_function():
    assert ev('LOWER("HELLO WORLD")') == "hello world"


def test_trim_function():
    assert ev('TRIM("  hello  ")') == "hello"


def test_str_function():
    assert ev("STR(42)") == "42"


def test_val_function():
    assert ev('VAL("42")') == "42"


def test_instr_function():
    # "WORLD" starts at position 7 in "HELLO WORLD"
    assert ev('INSTR("HELLO WORLD", "WORLD")') == "7"


# ---------------------------------------------------------------------------
# Variable access
# ---------------------------------------------------------------------------


def test_string_variable_substitution():
    result = StringExpressionEvaluator(
        string_variables={"A$": "HELLO"}
    ).evaluate("A$")
    assert result == "HELLO"


def test_string_concatenation():
    result = StringExpressionEvaluator(
        string_variables={"A$": "HELLO", "B$": "WORLD"}
    ).evaluate('A$ + " " + B$')
    assert result == "HELLO WORLD"


def test_len_of_variable():
    result = StringExpressionEvaluator(
        string_variables={"A$": "HELLO"}
    ).evaluate("LEN(A$)")
    assert result == "5"


def test_upper_of_variable():
    result = StringExpressionEvaluator(
        string_variables={"S$": "hello"}
    ).evaluate("UPPER(S$)")
    assert result == "HELLO"


def test_left_of_variable():
    result = StringExpressionEvaluator(
        string_variables={"S$": "ABCDEF"}
    ).evaluate("LEFT(S$, 4)")
    assert result == "ABCD"


def test_right_of_variable():
    result = StringExpressionEvaluator(
        string_variables={"S$": "ABCDEF"}
    ).evaluate("RIGHT(S$, 3)")
    assert result == "DEF"


def test_mid_of_variable():
    result = StringExpressionEvaluator(
        string_variables={"S$": "ABCDEF"}
    ).evaluate("MID(S$, 2, 3)")
    assert result == "BCD"


def test_lower_of_variable():
    result = StringExpressionEvaluator(
        string_variables={"S$": "Hello World"}
    ).evaluate("LOWER(S$)")
    assert result == "hello world"


def test_trim_of_variable():
    result = StringExpressionEvaluator(
        string_variables={"S$": "  hello  "}
    ).evaluate("TRIM(S$)")
    assert result == "hello"


def test_concatenation_self():
    result = StringExpressionEvaluator(
        string_variables={"A$": "hello"}
    ).evaluate("A$ + A$")
    assert result == "hellohello"
