"""
Tests for ExpressionEvaluator and StringEvaluator — core parsing utilities
used by the BASIC executor and other language executors.

These run without a QApplication; they are pure-Python unit tests.
"""

import math
import pytest

from time_warp.utils.expression_evaluator import ExpressionEvaluator  # type: ignore[import-not-found]
from time_warp.utils.string_evaluator import StringExpressionEvaluator as StringEvaluator  # type: ignore[import-not-found]


# ---------------------------------------------------------------------------
# ExpressionEvaluator — numeric arithmetic
# ---------------------------------------------------------------------------

class TestExpressionEvaluatorBasic:
    def eval(self, expr, variables=None):
        return ExpressionEvaluator(variables=variables or {}).evaluate(expr)

    def test_addition(self):
        assert self.eval("2 + 3") == 5.0

    def test_subtraction(self):
        assert self.eval("10 - 4") == 6.0

    def test_multiplication(self):
        assert self.eval("3 * 4") == 12.0

    def test_division(self):
        assert self.eval("9 / 2") == 4.5

    def test_integer_division_truncates_toward_zero(self):
        # BASIC \ operator: 7 \ 2 = 3, 7 \ -2 = -3 (NOT -4)
        assert self.eval("7 \\ 2") == 3.0
        assert self.eval("7 \\ -2") == -3.0
        assert self.eval("-7 \\ 2") == -3.0

    def test_modulo(self):
        assert self.eval("10 MOD 3") == 1.0

    def test_power(self):
        assert self.eval("2 ^ 10") == 1024.0

    def test_parentheses(self):
        assert self.eval("(2 + 3) * 4") == 20.0

    def test_variable_substitution(self):
        assert self.eval("X + 1", {"X": 5.0}) == 6.0
        assert self.eval("A * B", {"A": 3.0, "B": 4.0}) == 12.0

    def test_negative_number(self):
        assert self.eval("-5") == -5.0

    def test_nested_parentheses(self):
        assert self.eval("((2 + 3) * (4 - 1))") == 15.0


class TestExpressionEvaluatorFunctions:
    def eval(self, expr, variables=None):
        return ExpressionEvaluator(variables=variables or {}).evaluate(expr)

    def test_abs(self):
        assert self.eval("ABS(-5)") == 5.0

    def test_sqrt(self):
        assert self.eval("SQRT(4)") == 2.0

    def test_sqr_alias(self):
        assert self.eval("SQR(9)") == 3.0

    def test_int_floors_toward_negative_infinity(self):
        assert self.eval("INT(4.9)") == 4.0
        assert self.eval("INT(-4.1)") == -5.0

    def test_fix_truncates_toward_zero(self):
        assert self.eval("FIX(4.9)") == 4.0
        assert self.eval("FIX(-4.9)") == -4.0

    def test_cint_rounds(self):
        assert self.eval("CINT(4.4)") == 4.0
        assert self.eval("CINT(4.6)") == 5.0
        assert self.eval("CINT(-4.6)") == -5.0
        # CINT uses Python round() which uses banker's rounding on .5
        assert self.eval("CINT(4.5)") in (4.0, 5.0)

    def test_sgn(self):
        assert self.eval("SGN(10)") == 1.0
        assert self.eval("SGN(-5)") == -1.0
        assert self.eval("SGN(0)") == 0.0

    def test_log(self):
        assert abs(self.eval("LOG(1)")) < 1e-10

    def test_log10(self):
        assert abs(self.eval("LOG10(100)") - 2.0) < 1e-10

    def test_log2(self):
        assert abs(self.eval("LOG2(8)") - 3.0) < 1e-10

    def test_sin_cos(self):
        assert abs(self.eval("SIN(0)")) < 1e-10
        assert abs(self.eval("COS(0)") - 1.0) < 1e-10

    def test_atan(self):
        assert abs(self.eval("ATAN(1)") - math.atan(1)) < 1e-10

    def test_atan2(self):
        result = ExpressionEvaluator().evaluate("ATAN2(1, 1)")
        assert abs(result - math.atan2(1, 1)) < 1e-10

    def test_val(self):
        # VAL with a string variable substituted via string_variables
        ev = ExpressionEvaluator(variables={}, string_variables={"A$": "42"})
        # Direct float via FUNCTIONS
        assert ev.evaluate("VAL(3)") == 3.0

    def test_exp(self):
        assert abs(self.eval("EXP(0)") - 1.0) < 1e-10

    def test_min_max(self):
        result = ExpressionEvaluator().evaluate("MIN(3, 7)")
        assert result == 3.0
        result2 = ExpressionEvaluator().evaluate("MAX(3, 7)")
        assert result2 == 7.0

    def test_pow(self):
        result = ExpressionEvaluator().evaluate("POW(2, 8)")
        assert result == 256.0

    def test_unknown_function_raises(self):
        with pytest.raises(ValueError, match="Unknown function"):
            ExpressionEvaluator().evaluate("BOGUS(1)")


class TestExpressionEvaluatorEdgeCases:
    def eval(self, expr, variables=None):
        return ExpressionEvaluator(variables=variables or {}).evaluate(expr)

    def test_zero_division_raises(self):
        with pytest.raises((ZeroDivisionError, ValueError)):
            self.eval("5 / 0")

    def test_boolean_and(self):
        assert self.eval("1 AND 1") != 0
        assert self.eval("1 AND 0") == 0

    def test_boolean_or(self):
        assert self.eval("0 OR 1") != 0
        assert self.eval("0 OR 0") == 0

    def test_comparison_true(self):
        assert self.eval("3 > 2") != 0

    def test_comparison_false(self):
        assert self.eval("3 < 2") == 0

    def test_equality(self):
        assert self.eval("5 = 5") != 0
        assert self.eval("5 = 6") == 0


# ---------------------------------------------------------------------------
# StringEvaluator — BASIC string functions
# ---------------------------------------------------------------------------

class TestStringEvaluator:
    def se(self, svars=None, nvars=None):
        return StringEvaluator(
            string_variables=svars or {},
            numeric_variables=nvars or {},
        )

    def eval(self, expr, svars=None, nvars=None):
        return self.se(svars, nvars).evaluate(expr)

    def test_len(self):
        assert self.eval('LEN("hello")') == "5"

    def test_left(self):
        assert self.eval('LEFT("hello", 3)') == "hel"

    def test_right(self):
        assert self.eval('RIGHT("hello", 3)') == "llo"

    def test_mid(self):
        assert self.eval('MID("hello", 2, 3)') == "ell"

    def test_mid_no_length(self):
        assert self.eval('MID("hello", 3)') == "llo"

    def test_instr_found(self):
        assert self.eval('INSTR("hello world", "world")') == "7"

    def test_instr_not_found(self):
        assert self.eval('INSTR("hello", "xyz")') == "0"

    def test_upper(self):
        assert self.eval('UPPER("hello")') == "HELLO"

    def test_lower(self):
        assert self.eval('LOWER("HELLO")') == "hello"

    def test_trim(self):
        assert self.eval('TRIM("  hi  ")') == "hi"

    def test_ltrim(self):
        assert self.eval('LTRIM("  hi  ")') == "hi  "

    def test_rtrim(self):
        assert self.eval('RTRIM("  hi  ")') == "  hi"

    def test_space(self):
        assert self.eval("SPACE(5)") == "     "

    def test_str(self):
        assert self.eval("STR(42)") == "42"

    def test_chr(self):
        assert self.eval("CHR(65)") == "A"

    def test_asc(self):
        assert self.eval('ASC("A")') == "65"

    def test_val(self):
        assert self.eval('VAL("123")') == "123"
        assert self.eval('VAL("")') == "0"
        assert self.eval('VAL("3.14abc")') == "3.14"

    def test_spc(self):
        assert self.eval("SPC(3)") == "   "

    def test_string_fn(self):
        assert self.eval('STRING(4, "x")') == "xxxx"
        assert self.eval("STRING(3, 65)") == "AAA"

    def test_string_variable(self):
        assert self.eval("A$", svars={"A$": "hello"}) == "hello"

    def test_concatenation_ampersand(self):
        assert self.eval('"hello" & " world"') == "hello world"

    def test_unknown_function_raises(self):
        with pytest.raises(ValueError, match="Unknown string function"):
            self.eval("BOGUS(1)")
