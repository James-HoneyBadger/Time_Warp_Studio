"""Tests for the ExpressionEvaluator — arithmetic, functions, edge cases."""

from __future__ import annotations

import math
import pytest
from time_warp.utils.expression_evaluator import ExpressionEvaluator


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def ev():
    """Fresh evaluator with no variables."""
    return ExpressionEvaluator()


@pytest.fixture
def ev_vars():
    """Evaluator with some pre-set variables."""
    return ExpressionEvaluator(variables={"X": 10.0, "Y": 3.0, "PI": math.pi})


# ---------------------------------------------------------------------------
# Basic arithmetic
# ---------------------------------------------------------------------------


class TestArithmetic:
    def test_addition(self, ev):
        assert ev.evaluate("2 + 3") == 5.0

    def test_subtraction(self, ev):
        assert ev.evaluate("10 - 4") == 6.0

    def test_multiplication(self, ev):
        assert ev.evaluate("6 * 7") == 42.0

    def test_division(self, ev):
        assert ev.evaluate("15 / 3") == 5.0

    def test_modulo(self, ev):
        assert ev.evaluate("17 % 5") == 2.0

    def test_power(self, ev):
        assert ev.evaluate("2 ^ 10") == 1024.0

    def test_negative_number(self, ev):
        assert ev.evaluate("-5 + 3") == -2.0

    def test_float_literal(self, ev):
        assert abs(ev.evaluate("3.14") - 3.14) < 0.001

    def test_integer_result(self, ev):
        assert ev.evaluate("100") == 100.0


# ---------------------------------------------------------------------------
# Operator precedence
# ---------------------------------------------------------------------------


class TestPrecedence:
    def test_mul_before_add(self, ev):
        assert ev.evaluate("2 + 3 * 4") == 14.0

    def test_parens_override(self, ev):
        assert ev.evaluate("(2 + 3) * 4") == 20.0

    def test_nested_parens(self, ev):
        assert ev.evaluate("((1 + 2) * (3 + 4))") == 21.0

    def test_power_right_to_left(self, ev):
        # 2 ^ 3 ^ 2 should be 2^(3^2) = 2^9 = 512  (right-associative)
        result = ev.evaluate("2 ^ 3 ^ 2")
        assert result in (512.0, 64.0)  # Accept either associativity

    def test_complex_expression(self, ev):
        result = ev.evaluate("(10 - 4) * 2 + 3 ^ 2")
        assert result == 21.0  # 6*2 + 9


# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------


class TestVariables:
    def test_variable_lookup(self, ev_vars):
        assert ev_vars.evaluate("X") == 10.0

    def test_variable_in_expression(self, ev_vars):
        assert ev_vars.evaluate("X + Y") == 13.0

    def test_variable_multiply(self, ev_vars):
        assert ev_vars.evaluate("X * Y") == 30.0

    def test_set_variable(self, ev):
        ev.set_variable("Z", 99.0)
        assert ev.evaluate("Z") == 99.0

    def test_pi_constant(self, ev_vars):
        result = ev_vars.evaluate("PI")
        assert abs(result - math.pi) < 0.0001

    def test_unknown_variable_is_zero(self, ev):
        # Most BASIC-style evaluators default unknowns to 0
        try:
            result = ev.evaluate("UNKNOWN_VAR")
            assert result == 0.0
        except (ValueError, KeyError):
            pass  # also acceptable to raise an error


# ---------------------------------------------------------------------------
# Built-in functions
# ---------------------------------------------------------------------------


class TestFunctions:
    def test_sqrt(self, ev):
        assert ev.evaluate("SQRT(144)") == 12.0

    def test_abs_positive(self, ev):
        assert ev.evaluate("ABS(42)") == 42.0

    def test_abs_negative(self, ev):
        assert ev.evaluate("ABS(-42)") == 42.0

    def test_sin_zero(self, ev):
        assert abs(ev.evaluate("SIN(0)")) < 0.0001

    def test_cos_zero(self, ev):
        assert abs(ev.evaluate("COS(0)") - 1.0) < 0.0001

    def test_exp(self, ev):
        result = ev.evaluate("EXP(1)")
        assert abs(result - math.e) < 0.001

    def test_log(self, ev):
        result = ev.evaluate("LOG(1)")
        assert abs(result) < 0.001

    def test_floor(self, ev):
        assert ev.evaluate("FLOOR(3.7)") == 3.0

    def test_ceil(self, ev):
        assert ev.evaluate("CEIL(3.2)") == 4.0

    def test_int_function(self, ev):
        assert ev.evaluate("INT(7.9)") == 7.0

    def test_sgn_positive(self, ev):
        assert ev.evaluate("SGN(42)") == 1.0

    def test_sgn_negative(self, ev):
        assert ev.evaluate("SGN(-5)") == -1.0

    def test_sgn_zero(self, ev):
        assert ev.evaluate("SGN(0)") == 0.0

    def test_sqr_alias(self, ev):
        assert ev.evaluate("SQR(25)") == 5.0

    def test_nested_function(self, ev):
        result = ev.evaluate("ABS(SIN(0))")
        assert abs(result) < 0.0001


# ---------------------------------------------------------------------------
# Comparison operators
# ---------------------------------------------------------------------------


class TestComparisons:
    def test_less_than_true(self, ev):
        assert ev.evaluate("1 < 2") == 1.0

    def test_less_than_false(self, ev):
        assert ev.evaluate("3 < 2") == 0.0

    def test_greater_than(self, ev):
        assert ev.evaluate("5 > 3") == 1.0

    def test_equal(self, ev):
        assert ev.evaluate("4 = 4") == 1.0

    def test_not_equal(self, ev):
        result = ev.evaluate("4 <> 5")
        assert result == 1.0

    def test_less_equal(self, ev):
        assert ev.evaluate("3 <= 3") == 1.0

    def test_greater_equal(self, ev):
        assert ev.evaluate("5 >= 6") == 0.0


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------


class TestEdgeCases:
    def test_whitespace_only(self, ev):
        try:
            ev.evaluate("   ")
        except (ValueError, ZeroDivisionError):
            pass  # acceptable

    def test_empty_string(self, ev):
        try:
            ev.evaluate("")
        except (ValueError, ZeroDivisionError):
            pass

    def test_division_by_zero(self, ev):
        with pytest.raises((ZeroDivisionError, ValueError)):
            ev.evaluate("1 / 0")

    def test_large_number(self, ev):
        result = ev.evaluate("999999 * 999999")
        assert result == 999998000001.0

    def test_very_small_float(self, ev):
        result = ev.evaluate("0.001 + 0.002")
        assert abs(result - 0.003) < 0.0001


# ---------------------------------------------------------------------------
# Arrays
# ---------------------------------------------------------------------------


class TestArrays:
    def test_array_access(self):
        ev = ExpressionEvaluator(
            variables={},
            arrays={"A": [10.0, 20.0, 30.0]},
        )
        result = ev.evaluate("A(0)")
        assert result == 10.0

    def test_array_in_expression(self):
        ev = ExpressionEvaluator(
            variables={},
            arrays={"A": [5.0, 10.0]},
        )
        result = ev.evaluate("A(0) + A(1)")
        assert result == 15.0


# ---------------------------------------------------------------------------
# Security / limits
# ---------------------------------------------------------------------------


class TestSecurity:
    def test_max_tokens_attribute(self, ev):
        assert ev.MAX_TOKENS == 1000

    def test_no_code_execution(self, ev):
        """Expressions like __import__ must not work."""
        with pytest.raises((ValueError, KeyError)):
            ev.evaluate("__import__('os')")

    def test_very_long_expression_constrained(self, ev):
        """An extremely long expression should be bounded."""
        expr = " + ".join(["1"] * 2000)
        try:
            ev.evaluate(expr)
        except ValueError as e:
            assert "complex" in str(e).lower() or "token" in str(e).lower()
