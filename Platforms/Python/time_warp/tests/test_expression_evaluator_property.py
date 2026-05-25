"""Property-based tests for ExpressionEvaluator using Hypothesis.

These tests verify algebraic identities and safety invariants that should hold
across a wide range of automatically-generated inputs.
"""

from __future__ import annotations

import math

import pytest
from hypothesis import assume, given, settings
from hypothesis import strategies as st

from time_warp.utils.expression_evaluator import ExpressionEvaluator  # type: ignore[import-not-found]

# ---------------------------------------------------------------------------
# Strategy helpers
# ---------------------------------------------------------------------------

# Finite floats in a safe range (avoids overflow and near-zero divisions)
# Use integers cast to float to avoid scientific notation in f-strings
safe_floats = st.floats(
    min_value=-999.0,
    max_value=999.0,
    allow_nan=False,
    allow_infinity=False,
).filter(lambda x: abs(x) < 1e6 and (x == 0.0 or abs(x) >= 1e-4))

# Integers expressed as floats for cleaner arithmetic tests
safe_ints = st.integers(min_value=-500, max_value=500).map(float)

# Single uppercase letters usable as variable names
var_names = st.sampled_from(list("ABCDFGHIJKLMNOPQRSTUVWXYZ"))  # exclude E to avoid e-notation clash


def fmt(x: float) -> str:
    """Format a float for the evaluator (no scientific notation)."""
    return f"{x:.6f}"


def ev(variables: dict | None = None) -> ExpressionEvaluator:
    return ExpressionEvaluator(variables=variables or {})


# ---------------------------------------------------------------------------
# Arithmetic identity properties
# ---------------------------------------------------------------------------


class TestArithmeticIdentities:
    """Algebraic laws that the evaluator must respect."""

    @given(x=safe_floats)
    def test_addition_zero_identity(self, x: float) -> None:
        """x + 0 == x for any finite float."""
        result = ev().evaluate(f"{fmt(x)} + 0")
        assert math.isclose(result, x, rel_tol=1e-6, abs_tol=1e-6)

    @given(x=safe_floats)
    def test_multiplication_one_identity(self, x: float) -> None:
        """x * 1 == x for any finite float."""
        result = ev().evaluate(f"{fmt(x)} * 1")
        assert math.isclose(result, x, rel_tol=1e-6, abs_tol=1e-6)

    @given(x=safe_floats)
    def test_subtraction_self_is_zero(self, x: float) -> None:
        """x - x == 0 for any finite float."""
        result = ev().evaluate(f"{fmt(x)} - {fmt(x)}")
        assert math.isclose(result, 0.0, abs_tol=1e-6)

    @given(a=safe_ints, b=safe_ints)
    def test_commutativity_addition(self, a: float, b: float) -> None:
        """a + b == b + a."""
        lhs = ev().evaluate(f"{fmt(a)} + {fmt(b)}")
        rhs = ev().evaluate(f"{fmt(b)} + {fmt(a)}")
        assert math.isclose(lhs, rhs, abs_tol=1e-9)

    @given(a=safe_ints, b=safe_ints)
    def test_commutativity_multiplication(self, a: float, b: float) -> None:
        """a * b == b * a."""
        lhs = ev().evaluate(f"{fmt(a)} * {fmt(b)}")
        rhs = ev().evaluate(f"{fmt(b)} * {fmt(a)}")
        assert math.isclose(lhs, rhs, abs_tol=1e-9)

    @given(a=safe_ints, b=safe_ints, c=safe_ints)
    def test_associativity_addition(self, a: float, b: float, c: float) -> None:
        """(a + b) + c == a + (b + c)."""
        lhs = ev().evaluate(f"({fmt(a)} + {fmt(b)}) + {fmt(c)}")
        rhs = ev().evaluate(f"{fmt(a)} + ({fmt(b)} + {fmt(c)})")
        assert math.isclose(lhs, rhs, abs_tol=1e-9)

    @given(a=safe_ints, b=safe_ints)
    @settings(max_examples=200)
    def test_distributivity(self, a: float, b: float) -> None:
        """2 * (a + b) == 2*a + 2*b."""
        lhs = ev().evaluate(f"2 * ({fmt(a)} + {fmt(b)})")
        rhs = ev().evaluate(f"2 * {fmt(a)} + 2 * {fmt(b)}")
        assert math.isclose(lhs, rhs, abs_tol=1e-9)

    @given(x=safe_floats)
    def test_double_negation(self, x: float) -> None:
        """-(- x) == x."""
        result = ev().evaluate(f"-(-{fmt(x)})")
        assert math.isclose(result, x, rel_tol=1e-6, abs_tol=1e-6)


# ---------------------------------------------------------------------------
# Variable substitution properties
# ---------------------------------------------------------------------------


class TestVariableSubstitution:
    """Variables should behave exactly like their numeric values."""

    @given(name=var_names, value=safe_floats)
    def test_variable_lookup(self, name: str, value: float) -> None:
        """Evaluating a variable returns its assigned value."""
        result = ev({name: value}).evaluate(name)
        assert math.isclose(result, value, rel_tol=1e-6, abs_tol=1e-6)

    @given(name=var_names, a=safe_ints, b=safe_ints)
    def test_variable_in_expression(self, name: str, a: float, b: float) -> None:
        """VAR + b computes the same as a + b when VAR=a."""
        assume(abs(a + b) < 1e9)  # avoid overflow
        result = ev({name: a}).evaluate(f"{name} + {fmt(b)}")
        expected = a + b
        assert math.isclose(result, expected, rel_tol=1e-6, abs_tol=1e-6)

    @given(a=safe_ints, b=safe_ints)
    def test_two_variables_sum(self, a: float, b: float) -> None:
        """X + Y evaluates correctly with two distinct variables."""
        result = ev({"X": a, "Y": b}).evaluate("X + Y")
        assert math.isclose(result, a + b, abs_tol=1e-9)


# ---------------------------------------------------------------------------
# Division safety
# ---------------------------------------------------------------------------


class TestDivisionSafety:
    """Division by zero must raise, not silently produce NaN/Inf."""

    def test_literal_division_by_zero(self) -> None:
        with pytest.raises((ValueError, ZeroDivisionError)):
            ev().evaluate("1 / 0")

    def test_literal_mod_zero(self) -> None:
        with pytest.raises((ValueError, ZeroDivisionError)):
            ev().evaluate("5 MOD 0")

    @given(x=safe_floats)
    def test_nonzero_division_is_finite(self, x: float) -> None:
        """Dividing by a non-zero float always yields a finite result."""
        assume(abs(x) > 1e-4)
        result = ev().evaluate(f"1 / {fmt(x)}")
        assert math.isfinite(result)


# ---------------------------------------------------------------------------
# Math function properties
# ---------------------------------------------------------------------------


class TestMathFunctions:
    """Built-in functions (SIN, COS, SQRT, ABS, LOG) must satisfy identities."""

    @given(x=safe_floats)
    def test_abs_nonnegative(self, x: float) -> None:
        """ABS always returns a non-negative value."""
        result = ev().evaluate(f"ABS({fmt(x)})")
        assert result >= 0.0

    @given(x=safe_floats)
    def test_abs_of_negative_is_positive(self, x: float) -> None:
        """ABS(-x) == ABS(x)."""
        lhs = ev().evaluate(f"ABS(-({fmt(x)}))")
        rhs = ev().evaluate(f"ABS({fmt(x)})")
        assert math.isclose(lhs, rhs, abs_tol=1e-9)

    @given(x=st.floats(min_value=0.0, max_value=1000.0, allow_nan=False, allow_infinity=False).filter(lambda v: v >= 1e-4))
    def test_sqrt_squared_is_identity(self, x: float) -> None:
        """SQRT(x)^2 ≈ x for non-negative x."""
        result = ev().evaluate(f"SQR({fmt(x)}) ^ 2")
        assert math.isclose(result, x, rel_tol=1e-5, abs_tol=1e-5)

    @given(x=safe_floats)
    def test_sin_range(self, x: float) -> None:
        """SIN always returns a value in [-1, 1]."""
        result = ev().evaluate(f"SIN({fmt(x)})")
        assert -1.0 - 1e-9 <= result <= 1.0 + 1e-9

    @given(x=safe_floats)
    def test_cos_range(self, x: float) -> None:
        """COS always returns a value in [-1, 1]."""
        result = ev().evaluate(f"COS({fmt(x)})")
        assert -1.0 - 1e-9 <= result <= 1.0 + 1e-9

    @given(x=safe_floats)
    def test_sin_squared_plus_cos_squared(self, x: float) -> None:
        """SIN(x)^2 + COS(x)^2 == 1 (Pythagorean identity)."""
        result = ev().evaluate(f"SIN({fmt(x)}) ^ 2 + COS({fmt(x)}) ^ 2")
        assert math.isclose(result, 1.0, abs_tol=1e-9)


# ---------------------------------------------------------------------------
# Comparison operator properties
# ---------------------------------------------------------------------------


class TestComparisonProperties:
    """Comparison operators must be consistent with numeric ordering."""

    @given(a=safe_ints, b=safe_ints)
    def test_less_than_antisymmetry(self, a: float, b: float) -> None:
        """If a < b then NOT (b < a)."""
        assume(a != b)
        ab = ev().evaluate(f"{fmt(a)} < {fmt(b)}")
        ba = ev().evaluate(f"{fmt(b)} < {fmt(a)}")
        # Exactly one should be non-zero (true)
        assert (ab != 0) != (ba != 0)

    @given(a=safe_ints)
    def test_equal_reflexivity(self, a: float) -> None:
        """a == a should always be true (1)."""
        result = ev().evaluate(f"{fmt(a)} = {fmt(a)}")
        assert result == 1.0

    @given(a=safe_ints, b=safe_ints)
    def test_not_equal_consistency(self, a: float, b: float) -> None:
        """(a <> b) should equal NOT (a = b)."""
        assume(a != b)
        ne = ev().evaluate(f"{fmt(a)} <> {fmt(b)}")
        eq = ev().evaluate(f"{fmt(a)} = {fmt(b)}")
        assert ne != 0 and eq == 0


# ---------------------------------------------------------------------------
# No-crash safety net
# ---------------------------------------------------------------------------


class TestNoCrash:
    """Arbitrary valid expressions should never crash with an unhandled exception."""

    @given(
        a=safe_ints,
        b=safe_ints,
        op=st.sampled_from(["+", "-", "*"]),
    )
    def test_basic_operators_never_crash(self, a: float, b: float, op: str) -> None:
        """Basic arithmetic never raises unexpected exceptions."""
        try:
            result = ev().evaluate(f"{fmt(a)} {op} {fmt(b)}")
            assert math.isfinite(result)
        except (ValueError, ZeroDivisionError):
            pass  # acceptable


class TestExpressionEvaluatorExtended:
    """Direct (non-property) tests for ExpressionEvaluator."""

    def test_simple_add(self):
        assert ev().evaluate("3 + 4") == 7

    def test_simple_sub(self):
        assert ev().evaluate("10 - 3") == 7

    def test_simple_mul(self):
        assert ev().evaluate("6 * 7") == 42

    def test_simple_div(self):
        assert ev().evaluate("10 / 2") == 5

    def test_variable_substitution(self):
        assert ev({"X": 5}).evaluate("X + 3") == 8

    def test_two_variables(self):
        assert ev({"A": 2, "B": 3}).evaluate("A * B") == 6

    def test_nested_parens(self):
        assert ev().evaluate("(2 + 3) * 4") == 20

    def test_abs_function(self):
        assert ev().evaluate("ABS(-5)") == 5

    def test_sqr_function(self):
        assert ev().evaluate("SQR(9)") == 3

    def test_int_function(self):
        assert ev().evaluate("INT(3.7)") == 3

    def test_equal_operator(self):
        assert ev().evaluate("5 = 5") != 0

    def test_not_equal_operator(self):
        assert ev().evaluate("5 <> 6") != 0

    def test_less_than_operator(self):
        assert ev().evaluate("3 < 5") != 0

    def test_greater_than_operator(self):
        assert ev().evaluate("5 > 3") != 0

    def test_zero_result(self):
        assert ev().evaluate("0 * 999") == 0

    def test_negative_result(self):
        assert ev().evaluate("3 - 10") == -7


class TestExpressionEvaluatorExtended2:
    """More expression evaluator tests."""

    def test_nested_parens(self):
        assert ev().evaluate("((2 + 3))") == 5

    def test_unary_minus_literal(self):
        assert ev().evaluate("-7") == -7

    def test_chain_three_adds(self):
        assert ev().evaluate("1 + 2 + 3") == 6

    def test_chain_three_muls(self):
        assert ev().evaluate("2 * 3 * 4") == 24

    def test_mixed_sub_add(self):
        assert ev().evaluate("10 - 3 + 2") == 9

    def test_sgn_of_pos(self):
        assert ev().evaluate("SGN(5)") == 1

    def test_sgn_of_neg(self):
        assert ev().evaluate("SGN(-5)") == -1

    def test_sgn_of_zero(self):
        assert ev().evaluate("SGN(0)") == 0

    def test_abs_of_positive(self):
        assert ev().evaluate("ABS(3)") == 3

    def test_max_function(self):
        assert ev().evaluate("MAX(3, 7)") == 7

    def test_min_function(self):
        assert ev().evaluate("MIN(3, 7)") == 3

    def test_variable_times_two(self):
        assert ev({"X": 6}).evaluate("X * 2") == 12

    def test_two_variables_sum(self):
        assert ev({"A": 3, "B": 4}).evaluate("A + B") == 7

    def test_power_two(self):
        result = ev().evaluate("2 ^ 10")
        assert result == 1024

    def test_expression_returns_float_or_int(self):
        result = ev().evaluate("5 + 3")
        assert isinstance(result, (int, float))


class TestExpressionEvaluatorExtended3:
    """Third round of ExpressionEvaluator tests."""

    def test_add_three_numbers(self):
        assert ev().evaluate("1 + 2 + 3") == 6

    def test_multiply_subtract(self):
        result = ev().evaluate("5 * 4 - 10")
        assert result == 10

    def test_nested_parens(self):
        result = ev().evaluate("(2 + 3) * (4 - 1)")
        assert result == 15

    def test_float_result(self):
        result = ev().evaluate("10 / 4")
        assert result == 2.5

    def test_variable_in_expression(self):
        result = ev({"N": 7}).evaluate("N + 3")
        assert result == 10

    def test_two_vars_product(self):
        result = ev({"A": 5, "B": 6}).evaluate("A * B")
        assert result == 30

    def test_unary_minus(self):
        result = ev({"X": 3}).evaluate("-X")
        assert result == -3

    def test_abs_function(self):
        result = ev().evaluate("ABS(-10)")
        assert result == 10

    def test_sqrt_function(self):
        result = ev().evaluate("SQRT(16)")
        assert result == pytest.approx(4.0)

    def test_int_function(self):
        result = ev().evaluate("INT(3.7)")
        assert result == 3


class TestExpressionEvaluatorExtended4:
    """Fourth round of ExpressionEvaluator tests."""

    def test_add_floats(self):
        result = ev().evaluate("1.5 + 2.5")
        assert result == pytest.approx(4.0)

    def test_subtract_floats(self):
        result = ev().evaluate("5.0 - 2.5")
        assert result == pytest.approx(2.5)

    def test_multiply_floats(self):
        result = ev().evaluate("2.5 * 4.0")
        assert result == pytest.approx(10.0)

    def test_divide_floats(self):
        result = ev().evaluate("9.0 / 3.0")
        assert result == pytest.approx(3.0)

    def test_power_of_two(self):
        result = ev().evaluate("2 ^ 10")
        assert result == pytest.approx(1024.0)

    def test_three_variable_expression(self):
        result = ev({"A": 2, "B": 3, "C": 4}).evaluate("A + B * C")
        assert result == pytest.approx(14.0)

    def test_nested_abs(self):
        result = ev().evaluate("ABS(-10)")
        assert result == pytest.approx(10.0)

    def test_sqrt_of_9(self):
        result = ev().evaluate("SQRT(9)")
        assert result == pytest.approx(3.0)

    def test_min_of_two(self):
        result = ev().evaluate("MIN(5, 3)")
        assert result == pytest.approx(3.0)

    def test_max_of_two(self):
        result = ev().evaluate("MAX(5, 3)")
        assert result == pytest.approx(5.0)


class TestExpressionEvaluatorExtended5:
    """Fifth round of expression evaluator tests."""

    def test_add_three_numbers(self):
        result = ev().evaluate("1 + 2 + 3")
        assert result == pytest.approx(6.0)

    def test_subtract_zero(self):
        result = ev().evaluate("5 - 0")
        assert result == pytest.approx(5.0)

    def test_multiply_by_one(self):
        result = ev().evaluate("7 * 1")
        assert result == pytest.approx(7.0)

    def test_divide_by_self(self):
        result = ev().evaluate("9 / 9")
        assert result == pytest.approx(1.0)

    def test_negative_number(self):
        result = ev().evaluate("-5 + 5")
        assert result == pytest.approx(0.0)

    def test_variable_subtraction(self):
        result = ev({"A": 10, "B": 3}).evaluate("A - B")
        assert result == pytest.approx(7.0)

    def test_nested_parentheses(self):
        result = ev().evaluate("((2 + 3) * (4 - 1))")
        assert result == pytest.approx(15.0)

    def test_float_division(self):
        result = ev().evaluate("7 / 2")
        assert result == pytest.approx(3.5)

    def test_power_expression(self):
        result = ev().evaluate("2 ^ 8")
        assert result == pytest.approx(256.0)

    def test_modulo_expression(self):
        result = ev().evaluate("10 % 3")
        assert result == pytest.approx(1.0)


class TestExpressionEvaluatorExtended6:
    """Sixth round of expression evaluator tests."""

    def test_add_integers(self):
        result = ev().evaluate("100 + 200")
        assert result == pytest.approx(300.0)

    def test_subtract_negative(self):
        result = ev().evaluate("5 - 10")
        assert result == pytest.approx(-5.0)

    def test_multiply_zero(self):
        result = ev().evaluate("0 * 99")
        assert result == pytest.approx(0.0)

    def test_divide_by_two(self):
        result = ev().evaluate("8 / 2")
        assert result == pytest.approx(4.0)

    def test_simple_variable(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev2 = ExpressionEvaluator(variables={"N": 10})
        assert ev2.evaluate("N") == pytest.approx(10.0)

    def test_variable_addition(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev2 = ExpressionEvaluator(variables={"A": 3, "B": 7})
        assert ev2.evaluate("A + B") == pytest.approx(10.0)

    def test_parens_priority(self):
        result = ev().evaluate("(1 + 2) * 4")
        assert result == pytest.approx(12.0)

    def test_chained_multiply(self):
        result = ev().evaluate("2 * 3 * 4")
        assert result == pytest.approx(24.0)

    def test_float_result(self):
        result = ev().evaluate("1 / 4")
        assert result == pytest.approx(0.25)

    def test_large_number(self):
        result = ev().evaluate("1000 + 2000")
        assert result == pytest.approx(3000.0)


class TestExpressionEvaluatorExtended7:
    """Seventh round of expression evaluator tests."""

    def test_simple_one(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("1") == 1

    def test_simple_ten(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("10") == 10

    def test_add_floats(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("1.5 + 1.5") - 3.0) < 0.001

    def test_subtract_floats(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("5.5 - 0.5") - 5.0) < 0.001

    def test_multiply_float(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("2.5 * 4") - 10.0) < 0.001

    def test_divide_float(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert abs(ev.evaluate("7 / 2") - 3.5) < 0.001

    def test_variable_used_twice(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator(variables={"X": 5})
        assert ev.evaluate("X + X") == 10

    def test_two_variables(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator(variables={"A": 3, "B": 4})
        assert ev.evaluate("A + B") == 7

    def test_nested_parens(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("(2 + 3) * (1 + 1)") == 10

    def test_chained_add(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("1 + 2 + 3 + 4") == 10


class TestExpressionEvaluatorExtended8:
    """Eighth round of expression evaluator tests."""

    def test_evaluate_42(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("42") == 42

    def test_evaluate_zero(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("0") == 0

    def test_add_integers(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("3 + 4") == 7

    def test_subtract_integers(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("10 - 3") == 7

    def test_multiply_integers(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("3 * 4") == 12

    def test_divide_integers(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("10 / 2") == 5

    def test_variable_simple(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator(variables={"X": 7})
        assert ev.evaluate("X") == 7

    def test_variable_in_expr(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator(variables={"N": 5})
        assert ev.evaluate("N + 5") == 10

    def test_parens(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("(2 + 3) * 2") == 10

    def test_negative_number(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("-5 + 10") == 5


class TestExpressionEvaluatorExtended9:
    """Ninth round of expression evaluator tests."""

    def test_one_plus_one(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("1 + 1") == 2

    def test_ten_minus_three(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("10 - 3") == 7

    def test_three_times_four(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("3 * 4") == 12

    def test_ten_div_two(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("10 / 2") == 5

    def test_parens(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("(2 + 3) * 4") == 20

    def test_variable_x(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator(variables={"X": 7})
        assert ev.evaluate("X") == 7

    def test_variable_add(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator(variables={"X": 3})
        assert ev.evaluate("X + 2") == 5

    def test_zero(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("0") == 0

    def test_negative_literal(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("-3 + 3") == 0

    def test_large_number(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        ev = ExpressionEvaluator()
        assert ev.evaluate("100 * 100") == 10000


class TestExpressionEvaluatorExtended10:
    """Tenth extended round of expression evaluator tests."""

    def _ev(self, **kwargs):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        return ExpressionEvaluator(variables=kwargs)

    def test_add(self):
        assert self._ev().evaluate("5 + 5") == 10

    def test_subtract(self):
        assert self._ev().evaluate("10 - 4") == 6

    def test_multiply(self):
        assert self._ev().evaluate("7 * 6") == 42

    def test_divide(self):
        assert self._ev().evaluate("20 / 4") == 5

    def test_parens(self):
        assert self._ev().evaluate("(2 + 3) * 4") == 20

    def test_variable(self):
        assert self._ev(N=7).evaluate("N * 6") == 42

    def test_zero(self):
        assert self._ev().evaluate("0") == 0

    def test_negative(self):
        assert self._ev().evaluate("-5 + 10") == 5

    def test_large(self):
        assert self._ev().evaluate("200 * 200") == 40000

    def test_nested_parens(self):
        assert self._ev().evaluate("((1 + 2) * (3 + 4))") == 21


class TestExpressionEvaluatorExtended11:
    """Eleventh extended round of expression evaluator tests."""

    def test_add_200(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator()
        assert e.evaluate("100 + 100") == 200

    def test_sub_190(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator()
        assert e.evaluate("200 - 10") == 190

    def test_mul_200(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator()
        assert e.evaluate("20 * 10") == 200

    def test_div_10(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator()
        assert e.evaluate("100 / 10") == 10

    def test_var_add(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator(variables={"A": 100, "B": 100})
        assert e.evaluate("A + B") == 200

    def test_var_mul(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator(variables={"X": 20, "Y": 10})
        assert e.evaluate("X * Y") == 200

    def test_three_term(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator()
        assert e.evaluate("10 + 20 + 30") == 60

    def test_parens(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator()
        assert e.evaluate("(2 + 3) * 4") == 20

    def test_mod(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator()
        assert e.evaluate("10 MOD 3") == 1

    def test_neg_number(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator()
        result = e.evaluate("5 - 10")
        assert result == -5


class TestExpressionEvaluatorExtended12:
    def _e(self, **kw):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        return ExpressionEvaluator(variables=kw)

    def test_add_300(self):
        assert self._e().evaluate("150 + 150") == 300

    def test_sub_290(self):
        assert self._e().evaluate("300 - 10") == 290

    def test_mul_300(self):
        assert self._e().evaluate("30 * 10") == 300

    def test_div_15(self):
        assert self._e().evaluate("30 / 2") == 15

    def test_var_a_b(self):
        assert self._e(A=150, B=150).evaluate("A + B") == 300

    def test_nested_parens(self):
        assert self._e().evaluate("(1 + 2) * (3 + 4)") == 21

    def test_minus_result(self):
        assert self._e().evaluate("10 - 20") == -10

    def test_zero(self):
        assert self._e().evaluate("0") == 0

    def test_simple_100(self):
        assert self._e().evaluate("100") == 100

    def test_var_single(self):
        assert self._e(X=42).evaluate("X") == 42
