"""Tests for the ExpressionEvaluator utility."""

from __future__ import annotations

import math
import pytest
from time_warp.utils.expression_evaluator import ExpressionEvaluator


def ev(expr: str, **variables) -> float:
    """Evaluate expression with optional variables."""
    e = ExpressionEvaluator(variables=variables)
    return e.evaluate(expr)


# ---------------------------------------------------------------------------
# Basic arithmetic
# ---------------------------------------------------------------------------


def test_addition():
    assert ev("2 + 3") == 5.0


def test_subtraction():
    assert ev("10 - 4") == 6.0


def test_multiplication():
    assert ev("3 * 7") == 21.0


def test_division():
    assert ev("10 / 4") == 2.5


def test_modulo():
    assert ev("17 % 5") == 2.0


def test_operator_precedence():
    assert ev("2 + 3 * 4") == 14.0


def test_parentheses():
    assert ev("(2 + 3) * 4") == 20.0


def test_nested_parentheses():
    assert ev("((2 + 3) * (4 - 1))") == 15.0


# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------


def test_variable_in_expr():
    assert ev("2 + 3 * X", X=5) == 17.0


def test_multiple_variables():
    assert ev("(X + Y) * 2", X=5, Y=3) == 16.0


def test_variable_times_itself():
    assert ev("X * X", X=6) == 36.0


# ---------------------------------------------------------------------------
# Built-in functions
# ---------------------------------------------------------------------------


def test_abs_negative():
    assert ev("abs(-42)") == 42.0


def test_abs_positive():
    assert ev("abs(7)") == 7.0


def test_sqrt():
    assert ev("sqrt(16)") == 4.0


def test_max():
    assert ev("max(X, Y)", X=5, Y=3) == 5.0


def test_min():
    assert ev("min(X, Y)", X=5, Y=3) == 3.0


def test_floor():
    result = ev("floor(3.7)")
    assert result == 3.0


def test_ceil():
    result = ev("ceil(3.2)")
    assert result == 4.0


def test_round_function():
    result = ev("round(3.5)")
    assert result == 4.0


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------


def test_negative_literal():
    assert ev("-5 + 3") == -2.0


def test_float_literal():
    assert ev("1.5 + 2.5") == 4.0


def test_variable_modulo():
    assert ev("X % Y", X=17, Y=5) == 2.0


def test_power_operator():
    assert ev("2 ^ 3") == 8.0


def test_sin_function():
    import math
    assert ev("sin(0)") == 0.0


def test_cos_function():
    import math
    assert abs(ev("cos(0)") - 1.0) < 1e-9


def test_tan_function():
    assert ev("tan(0)") == 0.0


def test_int_truncation():
    assert ev("int(3.7)") == 3.0


def test_complex_expression_with_variables():
    assert ev("(X + Y) * 2", X=5, Y=3) == 16.0


def test_variable_power():
    assert ev("X ^ 2", X=5) == 25.0


def test_negated_variable():
    assert ev("-X", X=5) == -5.0


def test_abs_function():
    assert ev("abs(-7)") == 7.0


def test_modulo_operator():
    assert ev("10 % 3") == 1.0


def test_power_operator():
    assert ev("2 ^ 8") == 256.0


def test_chain_multiplication():
    assert ev("2 * 3 * 4") == 24.0


def test_nested_parentheses():
    assert ev("((2 + 3) * (4 - 1))") == 15.0


def test_floor_function():
    assert ev("floor(3.7)") == 3.0


def test_ceil_function():
    assert ev("ceil(3.2)") == 4.0


def test_log_of_one():
    assert ev("log(1)") == 0.0


def test_exp_of_zero():
    assert ev("exp(0)") == 1.0
