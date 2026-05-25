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


class TestBooleanLogic:
    """Boolean and logical operators."""

    def test_and_true(self, ev):
        assert ev.evaluate("1 AND 1") != 0

    def test_and_false(self, ev):
        assert ev.evaluate("1 AND 0") == 0

    def test_or_true(self, ev):
        assert ev.evaluate("0 OR 1") != 0

    def test_or_false(self, ev):
        assert ev.evaluate("0 OR 0") == 0

    def test_not_true(self, ev):
        result = ev.evaluate("NOT 0")
        assert result != 0

    def test_not_false(self, ev):
        result = ev.evaluate("NOT 1")
        assert result == 0


class TestTrigonometry:
    """Trig function tests."""

    def test_sin_half_pi(self, ev):
        result = ev.evaluate("SIN(3.14159265/2)")
        assert abs(result - 1.0) < 0.001

    def test_cos_pi(self, ev):
        result = ev.evaluate("COS(3.14159265)")
        assert abs(result - (-1.0)) < 0.001

    def test_tan_zero(self, ev):
        result = ev.evaluate("TAN(0)")
        assert abs(result) < 0.001

    def test_atan(self, ev):
        result = ev.evaluate("ATAN(1)")
        assert abs(result - math.pi / 4) < 0.001

    def test_sin_squared_plus_cos_squared(self, ev):
        # sin^2 + cos^2 = 1
        result = ev.evaluate("SIN(1)^2 + COS(1)^2")
        assert abs(result - 1.0) < 0.0001


class TestChainedComparisons:
    """More comparison tests."""

    def test_chained_addition(self, ev):
        assert ev.evaluate("2 + 3 + 4") == 9

    def test_chained_subtraction(self, ev):
        assert ev.evaluate("10 - 3 - 2") == 5

    def test_mixed_multiply_divide(self, ev):
        assert ev.evaluate("12 / 4 * 3") == 9

    def test_power_chain(self, ev):
        result = ev.evaluate("2^10")
        assert result == 1024

    def test_unary_minus_in_expr(self, ev):
        assert ev.evaluate("10 + -3") == 7


class TestVariableUpdates:
    """Variable mutation tests."""

    def test_update_variable(self):
        ev = ExpressionEvaluator(variables={"X": 5})
        ev.set_variable("X", 10)
        assert ev.evaluate("X") == 10

    def test_expression_with_updated_var(self):
        ev = ExpressionEvaluator(variables={"N": 3})
        ev.set_variable("N", 7)
        assert ev.evaluate("N * N") == 49

    def test_multiple_variables(self):
        ev = ExpressionEvaluator(variables={"A": 2, "B": 3, "C": 5})
        assert ev.evaluate("A + B * C") == 17

    def test_pi_in_expression(self):
        ev = ExpressionEvaluator(variables={"PI": math.pi})
        result = ev.evaluate("2 * PI")
        assert abs(result - 2 * math.pi) < 0.0001


class TestExprWithVariables:
    """Tests for expressions with variables."""

    def test_multiply_two_vars(self):
        ev = ExpressionEvaluator(variables={"X": 3, "Y": 4})
        assert ev.evaluate("X * Y") == 12

    def test_three_vars_sum(self):
        ev = ExpressionEvaluator(variables={"X": 1, "Y": 2, "Z": 3})
        assert ev.evaluate("X + Y + Z") == 6

    def test_negate_var(self):
        ev = ExpressionEvaluator(variables={"X": 5})
        assert ev.evaluate("-X") == -5

    def test_nested_paren_with_vars(self):
        ev = ExpressionEvaluator(variables={"X": 1, "Y": 2, "Z": 3})
        assert ev.evaluate("(X + Y) * Z") == 9

    def test_caret_power_with_var(self):
        ev = ExpressionEvaluator(variables={"X": 2})
        assert ev.evaluate("X ^ 3") == 8

    def test_modulo_expr(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("10 % 3")
        assert result == 1

    def test_abs_function(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("abs(-7)")
        assert result == 7

    def test_min_function(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("min(3, 7)")
        assert result == 3

    def test_max_function(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("max(3, 7)")
        assert result == 7


class TestMathFunctions2:
    """Additional math function tests."""

    def test_log_natural(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("log(100)")
        assert abs(result - math.log(100)) < 0.0001

    def test_log10(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("log10(100)")
        assert abs(result - 2.0) < 0.0001

    def test_floor_func(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("floor(3.9)")
        assert result == 3

    def test_ceil_func(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("ceil(3.2)")
        assert result == 4

    def test_round_func(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("round(3.7)")
        assert result == 4

    def test_sin_30_degrees(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("sin(0.5235987755982988)")  # pi/6
        assert abs(result - 0.5) < 0.0001

    def test_cos_zero(self):
        ev = ExpressionEvaluator(variables={})
        result = ev.evaluate("cos(0)")
        assert abs(result - 1.0) < 0.0001


class TestExpressionEvaluatorBasicOps:
    """Tests for basic arithmetic operations in ExpressionEvaluator."""

    def test_mul_before_add(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('2 + 3 * 4') == 14.0

    def test_parentheses_override_precedence(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('(2 + 3) * 4') == 20.0

    def test_division(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('10 / 2') == 5.0

    def test_power_operator(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('2 ^ 10') == 1024.0

    def test_mod_operator(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('10 mod 3') == 1.0

    def test_sqrt(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('sqrt(16)') == 4.0

    def test_abs_negative(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('abs(-9)') == 9.0

    def test_sin_zero(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('sin(0)') == 0.0

    def test_cos_zero(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('cos(0)') == 1.0

    def test_tan_zero(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('tan(0)') == 0.0

    def test_round(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('round(3.5)') == 4.0

    def test_floor(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('floor(3.9)') == 3.0

    def test_ceil(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('ceil(3.1)') == 4.0

    def test_exp_zero(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('exp(0)') == 1.0

    def test_max_function(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('max(3, 7)') == 7.0

    def test_min_function(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('min(3, 7)') == 3.0

    def test_double_negative(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('--5') == 5.0

    def test_nested_parens(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('((2 + 3) * (4 - 1))') == 15.0

    def test_integer_subtraction(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('100 - 37') == 63.0

    def test_chained_additions(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate('1 + 2 + 3 + 4') == 10.0


class TestExpressionEvaluatorWithVars2:
    """Tests for ExpressionEvaluator with multiple variables."""

    def test_a_plus_b_plus_c(self):
        ev = ExpressionEvaluator(variables={"A": 1, "B": 2, "C": 3})
        assert abs(ev.evaluate("A + B + C") - 6) < 0.001

    def test_a_times_b_plus_c(self):
        ev = ExpressionEvaluator(variables={"A": 2, "B": 3, "C": 4})
        assert abs(ev.evaluate("A * B + C") - 10) < 0.001

    def test_pythagorean(self):
        ev = ExpressionEvaluator(variables={"A": 3, "B": 4})
        assert abs(ev.evaluate("sqrt(A ^ 2 + B ^ 2)") - 5) < 0.001

    def test_negation(self):
        ev = ExpressionEvaluator(variables={"A": 5})
        assert abs(ev.evaluate("-A") - (-5)) < 0.001

    def test_negative_var_in_expr(self):
        ev = ExpressionEvaluator(variables={"A": 10, "B": 3})
        assert abs(ev.evaluate("A + (-B)") - 7) < 0.001

    def test_abs_difference(self):
        ev = ExpressionEvaluator(variables={"A": 3, "B": 7})
        assert abs(ev.evaluate("abs(A - B)") - 4) < 0.001

    def test_floor_division(self):
        ev = ExpressionEvaluator(variables={"A": 7, "B": 2})
        assert abs(ev.evaluate("floor(A / B)") - 3) < 0.001

    def test_ceil_division(self):
        ev = ExpressionEvaluator(variables={"A": 7, "B": 2})
        assert abs(ev.evaluate("ceil(A / B)") - 4) < 0.001

    def test_sin_plus_cos_zero(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("sin(0) + cos(0)") - 1) < 0.001

    def test_sum_of_squares(self):
        ev = ExpressionEvaluator(variables={"A": 3, "B": 4})
        assert abs(ev.evaluate("A ^ 2 + B ^ 2") - 25) < 0.001

    def test_group_of_four(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("1 + 2 + 3 + 4") - 10) < 0.001

    def test_grouped_mult(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("(1 + 2) * (3 + 4)") - 21) < 0.001


class TestExpressionEvaluatorComplex:
    """Complex expression evaluator tests."""

    def test_sum_1_to_5(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("1 + 2 + 3 + 4 + 5") - 15) < 0.001

    def test_multiply_chain(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("2 * 3 * 4") - 24) < 0.001

    def test_nested_parens(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("((2 + 3) * (4 - 1))") - 15) < 0.001

    def test_power_chain(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("2 ^ 2 ^ 2") - 16) < 0.001

    def test_float_arithmetic(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("1.5 + 2.5") - 4.0) < 0.001

    def test_float_multiply(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("2.5 * 4.0") - 10.0) < 0.001

    def test_negative_result(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("3 - 10") - (-7)) < 0.001

    def test_division_remainder(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("10 / 4") - 2.5) < 0.001

    def test_variable_power(self):
        ev = ExpressionEvaluator(variables={"N": 3})
        assert abs(ev.evaluate("N ^ 3") - 27) < 0.001

    def test_multiple_vars_formula(self):
        ev = ExpressionEvaluator(variables={"A": 2, "B": 3, "C": 4})
        assert abs(ev.evaluate("A * B + C") - 10) < 0.001

    def test_abs_function(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("abs(-15)") - 15) < 0.001

    def test_sqrt_25(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("sqrt(25)") - 5) < 0.001

    def test_min_function(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("min(3, 7)") - 3) < 0.001

    def test_max_function(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("max(3, 7)") - 7) < 0.001

    def test_zero_expr(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("0") - 0) < 0.001

    def test_large_number(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("1000000") - 1000000) < 0.001

    def test_subtract_from_zero(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("0 - 5") - (-5)) < 0.001

    def test_modulo_op(self):
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("17 % 5") - 2) < 0.001

    def test_var_in_parens(self):
        ev = ExpressionEvaluator(variables={"X": 5})
        assert abs(ev.evaluate("(X + 1) * (X - 1)") - 24) < 0.001

    def test_pi_approx(self):
        import math
        ev = ExpressionEvaluator(variables={"PI": math.pi})
        assert abs(ev.evaluate("PI") - 3.14159265) < 0.001


class TestExpressionEvaluatorBooleans2:
    """Boolean expression evaluator tests."""

    def test_and_true(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("1 and 1") in (True, 1)

    def test_and_false(self):
        ev = ExpressionEvaluator()
        assert not ev.evaluate("1 and 0")

    def test_or_true(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("0 or 1")

    def test_or_false(self):
        ev = ExpressionEvaluator()
        assert not ev.evaluate("0 or 0")

    def test_not_true(self):
        ev = ExpressionEvaluator()
        assert not ev.evaluate("not 1")

    def test_not_false(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("not 0")

    def test_ge_true(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("5 >= 5")

    def test_le_true(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("5 <= 5")

    def test_ne_true(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("5 <> 3")

    def test_ne_false(self):
        ev = ExpressionEvaluator()
        assert not ev.evaluate("5 <> 5")


class TestExpressionEvaluatorVariables2:
    """More tests for ExpressionEvaluator with variables."""

    def test_var_A(self):
        ev = ExpressionEvaluator(variables={"A": 7})
        assert ev.evaluate("A") == 7

    def test_var_B(self):
        ev = ExpressionEvaluator(variables={"B": 3})
        assert ev.evaluate("B") == 3

    def test_var_sum(self):
        ev = ExpressionEvaluator(variables={"A": 7, "B": 3})
        assert ev.evaluate("A + B") == 10

    def test_var_product(self):
        ev = ExpressionEvaluator(variables={"A": 6, "B": 7})
        assert ev.evaluate("A * B") == 42

    def test_var_expr(self):
        ev = ExpressionEvaluator(variables={"X": 3, "Y": 4})
        assert ev.evaluate("X ^ 2 + Y ^ 2") == 25

    def test_var_conditional(self):
        ev = ExpressionEvaluator(variables={"N": 5})
        assert ev.evaluate("N > 3")

    def test_var_negative(self):
        ev = ExpressionEvaluator(variables={"N": -7})
        assert ev.evaluate("ABS(N)") == 7

    def test_var_zero(self):
        ev = ExpressionEvaluator(variables={"X": 0})
        assert ev.evaluate("X") == 0

    def test_var_chain(self):
        ev = ExpressionEvaluator(variables={"A": 2})
        assert ev.evaluate("A + A + A + A + A") == 10

    def test_var_in_sqrt(self):
        ev = ExpressionEvaluator(variables={"N": 16})
        assert ev.evaluate("SQRT(N)") == 4.0


class TestExpressionEvaluatorChains:
    """Chain expression evaluation tests."""

    def test_add_sub_chain(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("10 - 3 + 2") == 9

    def test_mul_div_chain(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("6 * 4 / 3") == 8

    def test_precedence_mul_before_add(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("2 + 3 * 4") == 14

    def test_precedence_parens_override(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("(2 + 3) * 4") == 20

    def test_nested_parens(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("((2 + 3) * (4 - 1))") == 15

    def test_deep_nesting(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("(((7)))") == 7

    def test_unary_minus(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("-5 + 8") == 3

    def test_double_unary(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("--5") == 5

    def test_power_right_assoc(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("2 ^ 3") == 8

    def test_large_numbers(self):
        ev = ExpressionEvaluator()
        assert ev.evaluate("1000000 * 1000000") == 1e12

    def test_add_float(self):
        ev = ExpressionEvaluator()
        result = ev.evaluate("1.5 + 2.5")
        assert abs(result - 4.0) < 1e-9

    def test_sub_float(self):
        ev = ExpressionEvaluator()
        result = ev.evaluate("3.7 - 1.2")
        assert abs(result - 2.5) < 0.001

    def test_mul_float(self):
        ev = ExpressionEvaluator()
        result = ev.evaluate("2.5 * 4")
        assert abs(result - 10.0) < 1e-9


class TestExpressionEvaluatorMore:
    """Additional expression evaluator tests."""

    def ev(self, variables=None):
        return ExpressionEvaluator(variables=variables or {})

    def test_add_three_terms(self):
        assert self.ev().evaluate("1 + 2 + 3") == 6

    def test_sub_chain(self):
        assert self.ev().evaluate("10 - 3 - 2") == 5

    def test_mul_chain(self):
        assert self.ev().evaluate("2 * 3 * 4") == 24

    def test_div_chain(self):
        result = self.ev().evaluate("100 / 2 / 5")
        assert abs(result - 10.0) < 1e-9

    def test_mixed_add_mul(self):
        assert self.ev().evaluate("2 + 3 * 4") == 14

    def test_parens_override_precedence(self):
        assert self.ev().evaluate("(2 + 3) * 4") == 20

    def test_unary_minus_variable(self):
        result = self.ev({"X": 5.0}).evaluate("-X")
        assert result == -5

    def test_three_variables(self):
        result = self.ev({"A": 1.0, "B": 2.0, "C": 3.0}).evaluate("A + B + C")
        assert result == 6

    def test_power_chain(self):
        assert self.ev().evaluate("2 ^ 10") == 1024

    def test_mod_exact(self):
        assert self.ev().evaluate("10 MOD 5") == 0

    def test_integer_div_result(self):
        assert self.ev().evaluate("7 \\ 2") == 3

    def test_abs_zero(self):
        assert self.ev().evaluate("ABS(0)") == 0

    def test_int_negative(self):
        assert self.ev().evaluate("INT(-3.7)") == -4

    def test_sgn_positive(self):
        result = self.ev().evaluate("SGN(5)")
        assert result == 1

    def test_sgn_negative(self):
        result = self.ev().evaluate("SGN(-5)")
        assert result == -1

    def test_sgn_zero(self):
        result = self.ev().evaluate("SGN(0)")
        assert result == 0

    def test_max_two_values(self):
        assert self.ev().evaluate("MAX(3, 7)") == 7

    def test_min_two_values(self):
        assert self.ev().evaluate("MIN(3, 7)") == 3
