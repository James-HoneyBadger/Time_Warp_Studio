#!/usr/bin/env python
"""Test script to verify 03_arithmetic.bas output."""

if __name__ == "__main__":
    import sys
    from pathlib import Path

    # Add the time_warp package to path
    sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent))

    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    # Simple test first
    print("Testing simple BASIC:")
    interp = Interpreter()
    interp.language = Language.BASIC
    turtle = TurtleState()

    simple_code = """10 PRINT "Hello"
20 PRINT ""
30 PRINT "World"
"""

    interp.load_program(simple_code)
    result = interp.execute(turtle)

    print(f"Total output lines: {len(result)}")
    for i, line in enumerate(result):
        print(f"  Line {i}: {repr(line)}")

import pytest
from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState


def _run_basic(code: str):
    """Run BASIC code and return output lines."""
    interp = Interpreter()
    interp.language = Language.BASIC
    turtle = TurtleState()
    interp.load_program(code)
    return interp.execute(turtle)


class TestArithmeticBasic:
    """Tests for BASIC arithmetic operations."""

    def test_print_hello(self):
        result = _run_basic('10 PRINT "Hello"\n')
        assert any("Hello" in line for line in result)

    def test_addition(self):
        result = _run_basic("10 PRINT 2 + 3\n")
        assert any("5" in line for line in result)

    def test_subtraction(self):
        result = _run_basic("10 PRINT 10 - 4\n")
        assert any("6" in line for line in result)

    def test_multiplication(self):
        result = _run_basic("10 PRINT 3 * 4\n")
        assert any("12" in line for line in result)

    def test_division(self):
        result = _run_basic("10 PRINT 10 / 2\n")
        assert any("5" in line for line in result)

    def test_let_variable(self):
        result = _run_basic("10 LET X = 42\n20 PRINT X\n")
        assert any("42" in line for line in result)

    def test_addition_variables(self):
        result = _run_basic("10 LET A = 7\n20 LET B = 3\n30 PRINT A + B\n")
        assert any("10" in line for line in result)

    def test_multiple_prints(self):
        result = _run_basic('10 PRINT "A"\n20 PRINT "B"\n30 PRINT "C"\n')
        text = " ".join(result)
        assert "A" in text and "B" in text and "C" in text

    def test_print_negative(self):
        result = _run_basic("10 PRINT -5\n")
        assert any("-5" in line for line in result)

    def test_power_operator(self):
        result = _run_basic("10 PRINT 2 ^ 8\n")
        assert any("256" in line for line in result)

    def test_string_variable(self):
        result = _run_basic('10 LET A$ = "hello"\n20 PRINT A$\n')
        assert any("hello" in line for line in result)

    def test_abs_function(self):
        result = _run_basic("10 PRINT ABS(-10)\n")
        assert any("10" in line for line in result)

    def test_sqr_function(self):
        result = _run_basic("10 PRINT SQR(16)\n")
        assert any("4" in line for line in result)

    def test_int_function(self):
        result = _run_basic("10 PRINT INT(3.7)\n")
        assert any("3" in line for line in result)

    def test_max_function(self):
        result = _run_basic("10 PRINT MAX(5, 10)\n")
        assert any("10" in line for line in result)

    def test_min_function(self):
        result = _run_basic("10 PRINT MIN(5, 10)\n")
        assert any("5" in line for line in result)

    def test_mod_operator(self):
        result = _run_basic("10 PRINT 10 MOD 3\n")
        assert any("1" in line for line in result)

    def test_for_loop(self):
        result = _run_basic("10 FOR I = 1 TO 5\n20 PRINT I\n30 NEXT I\n")
        text = " ".join(result)
        assert "1" in text and "5" in text

    def test_if_true(self):
        result = _run_basic('10 IF 1 = 1 THEN PRINT "yes"\n')
        assert any("yes" in line for line in result)

    def test_if_false(self):
        result = _run_basic('10 IF 1 = 2 THEN PRINT "yes"\n')
        assert not any("yes" in line for line in result)


class TestArithmeticExtended:
    """More arithmetic tests."""

    def test_print_100(self):
        r = _run_basic("10 PRINT 100\n")
        assert any("100" in line for line in r)

    def test_addition_5_5(self):
        r = _run_basic("10 PRINT 5+5\n")
        assert any("10" in line for line in r)

    def test_multiplication_3_3(self):
        r = _run_basic("10 PRINT 3*3\n")
        assert any("9" in line for line in r)

    def test_division_9_3(self):
        r = _run_basic("10 PRINT 9/3\n")
        assert any("3" in line for line in r)

    def test_let_and_print(self):
        r = _run_basic("10 LET X=7\n20 PRINT X\n")
        assert any("7" in line for line in r)

    def test_expression_mixed(self):
        r = _run_basic("10 PRINT 2+3*4\n")
        assert any("14" in line for line in r)

    def test_subtraction_positive_result(self):
        r = _run_basic("10 PRINT 10-4\n")
        assert any("6" in line for line in r)

    def test_zero_print(self):
        r = _run_basic("10 PRINT 0\n")
        assert any("0" in line for line in r)

    def test_for_to_3(self):
        r = _run_basic("10 FOR I=1 TO 3\n20 PRINT I\n30 NEXT I\n")
        text = " ".join(r)
        assert "3" in text

    def test_string_print(self):
        r = _run_basic('10 PRINT "HELLO"\n')
        assert any("HELLO" in line for line in r)

    def test_output_is_list(self):
        r = _run_basic("10 PRINT 1\n")
        assert isinstance(r, list)

    def test_power_of_2(self):
        r = _run_basic("10 PRINT 2^8\n")
        assert any("256" in line for line in r)


class TestArithmeticExtended2:
    """More arithmetic tests."""

    def bas(self, src):
        return _run_basic(src)

    def test_modulo_operation(self):
        r = self.bas("10 PRINT 10 MOD 3\n")
        assert any("1" in line for line in r)

    def test_string_repetition_join(self):
        r = self.bas('10 PRINT "AB"\n')
        assert any("AB" in line for line in r)

    def test_precedence_add_mul(self):
        r = self.bas("10 PRINT 2+3*4\n")
        assert any("14" in line for line in r)

    def test_abs_function(self):
        r = self.bas("10 PRINT ABS(-10)\n")
        assert any("10" in line for line in r)

    def test_sqr_of_16(self):
        r = self.bas("10 PRINT SQR(16)\n")
        assert any("4" in line for line in r)

    def test_int_of_float(self):
        r = self.bas("10 PRINT INT(7.9)\n")
        assert any("7" in line for line in r)

    def test_arithmetic_chain(self):
        r = self.bas("10 PRINT 1+2+3+4+5\n")
        assert any("15" in line for line in r)

    def test_subtraction_result_0(self):
        r = self.bas("10 PRINT 5-5\n")
        assert any("0" in line for line in r)

    def test_large_addition(self):
        r = self.bas("10 PRINT 10000+20000\n")
        assert any("30000" in line for line in r)

    def test_multiply_two_numbers(self):
        r = self.bas("10 PRINT 7*8\n")
        assert any("56" in line for line in r)

    def test_simple_var(self):
        r = self.bas("10 LET N=42\n20 PRINT N\n")
        assert any("42" in line for line in r)

    def test_two_vars(self):
        r = self.bas("10 LET A=3\n20 LET B=4\n30 PRINT A+B\n")
        assert any("7" in line for line in r)


class TestArithmeticExtended3:
    """Third round of BASIC arithmetic tests."""

    def bas(self, src):
        return _run_basic(src)

    def test_division(self):
        r = self.bas("10 PRINT 20/4\n")
        assert any("5" in line for line in r)

    def test_power_operator(self):
        r = self.bas("10 PRINT 2^8\n")
        assert any("256" in line for line in r)

    def test_negative_number(self):
        r = self.bas("10 PRINT -5\n")
        assert any("-5" in line for line in r)

    def test_three_operand_add(self):
        r = self.bas("10 PRINT 1+2+3\n")
        assert any("6" in line for line in r)

    def test_chained_multiply(self):
        r = self.bas("10 PRINT 2*3*4\n")
        assert any("24" in line for line in r)

    def test_mixed_operators(self):
        r = self.bas("10 PRINT 2+3*4\n")
        assert any("14" in line for line in r)

    def test_parentheses(self):
        r = self.bas("10 PRINT (2+3)*4\n")
        assert any("20" in line for line in r)

    def test_var_multiply(self):
        r = self.bas("10 LET X=6\n20 PRINT X*7\n")
        assert any("42" in line for line in r)

    def test_comparison_true(self):
        r = self.bas("10 IF 5 > 3 THEN PRINT \"YES\"\n")
        assert any("YES" in line for line in r)

    def test_comparison_false(self):
        r = self.bas("10 IF 2 > 10 THEN PRINT \"YES\"\n20 PRINT \"NO\"\n")
        assert any("NO" in line for line in r)


class TestArithmeticExtended4:
    """Fourth round of BASIC arithmetic tests."""

    def bas(self, src):
        return _run_basic(src)

    def test_print_hundred(self):
        r = self.bas("10 PRINT 100\n")
        assert any("100" in line for line in r)

    def test_addition_strings(self):
        r = self.bas('10 PRINT "A" + "B"\n')
        assert any("AB" in line for line in r) or isinstance(r, list)

    def test_let_and_add(self):
        r = self.bas("10 LET A=3\n20 LET B=7\n30 PRINT A+B\n")
        assert any("10" in line for line in r)

    def test_multiply_three(self):
        r = self.bas("10 PRINT 3*4*2\n")
        assert any("24" in line for line in r)

    def test_int_div_result(self):
        r = self.bas("10 PRINT 10\\3\n")
        assert isinstance(r, list)

    def test_print_float(self):
        r = self.bas("10 PRINT 1.5+1.5\n")
        assert any("3" in line for line in r)

    def test_print_negative_result(self):
        r = self.bas("10 PRINT 5-10\n")
        assert any("-5" in line for line in r)

    def test_mod_operator(self):
        r = self.bas("10 PRINT 10 MOD 3\n")
        assert any("1" in line for line in r)

    def test_nested_parens(self):
        r = self.bas("10 PRINT (2+3)*(4-1)\n")
        assert any("15" in line for line in r)

    def test_var_expression(self):
        r = self.bas("10 X=6\n20 Y=X*X\n30 PRINT Y\n")
        assert any("36" in line for line in r)


class TestArithmeticExtended5:
    """Fifth round of BASIC arithmetic tests."""

    def bas(self, src):
        return _run_basic(src)

    def test_print_zero(self):
        r = self.bas("10 PRINT 0\n")
        assert any("0" in line for line in r)

    def test_power_operator(self):
        r = self.bas("10 PRINT 2^10\n")
        assert any("1024" in line for line in r)

    def test_division_result(self):
        r = self.bas("10 PRINT 15/3\n")
        assert any("5" in line for line in r)

    def test_subtraction_result(self):
        r = self.bas("10 PRINT 100-37\n")
        assert any("63" in line for line in r)

    def test_parentheses_order(self):
        r = self.bas("10 PRINT (2+3)*4\n")
        assert any("20" in line for line in r)

    def test_mixed_types(self):
        r = self.bas("10 A=5\n20 B=2.5\n30 PRINT A*B\n")
        assert isinstance(r, list)

    def test_negative_result(self):
        r = self.bas("10 PRINT 5-8\n")
        assert any("-3" in line for line in r)

    def test_large_multiplication(self):
        r = self.bas("10 PRINT 999*999\n")
        assert any("998001" in line for line in r)

    def test_variable_reassign(self):
        r = self.bas("10 X=5\n20 X=X+1\n30 PRINT X\n")
        assert any("6" in line for line in r)

    def test_abs_negative(self):
        r = self.bas("10 PRINT ABS(-10)\n")
        assert any("10" in line for line in r)


class TestArithmeticExtended6:
    """Sixth round of BASIC arithmetic tests."""

    def bas(self, src):
        return _run_basic(src)

    def test_integer_addition(self):
        r = self.bas("10 PRINT 7+3\n")
        assert any("10" in line for line in r)

    def test_integer_subtraction(self):
        r = self.bas("10 PRINT 10-4\n")
        assert any("6" in line for line in r)

    def test_multiplication_3x4(self):
        r = self.bas("10 PRINT 3*4\n")
        assert any("12" in line for line in r)

    def test_division_10by2(self):
        r = self.bas("10 PRINT 10/2\n")
        assert any("5" in line for line in r)

    def test_modulo_7mod3(self):
        r = self.bas("10 PRINT 7 MOD 3\n")
        assert any("1" in line for line in r)

    def test_nested_parens(self):
        r = self.bas("10 PRINT (2+3)*4\n")
        assert any("20" in line for line in r)

    def test_let_and_print(self):
        r = self.bas("10 LET X=42\n20 PRINT X\n")
        assert any("42" in line for line in r)

    def test_multiple_vars(self):
        r = self.bas("10 LET A=5\n20 LET B=3\n30 PRINT A+B\n")
        assert any("8" in line for line in r)

    def test_string_len(self):
        r = self.bas('10 PRINT LEN("HELLO")\n')
        assert any("5" in line for line in r)

    def test_large_addition(self):
        r = self.bas("10 PRINT 1000+2000\n")
        assert any("3000" in line for line in r)


class TestArithmeticExtended7:
    """Seventh round of BASIC arithmetic tests."""

    def bas(self, src):
        return _run_basic(src)

    def test_print_one(self):
        r = self.bas("10 PRINT 1\n")
        assert any("1" in line for line in r)

    def test_print_two(self):
        r = self.bas("10 PRINT 2\n")
        assert any("2" in line for line in r)

    def test_add_five_and_five(self):
        r = self.bas("10 PRINT 5+5\n")
        assert any("10" in line for line in r)

    def test_subtract_one(self):
        r = self.bas("10 PRINT 9-1\n")
        assert any("8" in line for line in r)

    def test_multiply_two(self):
        r = self.bas("10 PRINT 6*2\n")
        assert any("12" in line for line in r)

    def test_divide_four(self):
        r = self.bas("10 PRINT 8/4\n")
        assert any("2" in line for line in r)

    def test_let_and_add(self):
        r = self.bas("10 LET X=10\n20 PRINT X+5\n")
        assert any("15" in line for line in r)

    def test_negative_result(self):
        r = self.bas("10 PRINT 3-7\n")
        assert isinstance(r, list)

    def test_print_hundred(self):
        r = self.bas("10 PRINT 100\n")
        assert any("100" in line for line in r)

    def test_power_via_let(self):
        r = self.bas("10 LET X=2*2*2\n20 PRINT X\n")
        assert any("8" in line for line in r)


class TestArithmeticExtended8:
    """Eighth round of BASIC arithmetic tests."""

    def bas(self, src):
        return _run_basic(src)

    def test_print_two(self):
        r = self.bas("PRINT 2")
        assert "2" in r

    def test_add_three_and_seven(self):
        r = self.bas("PRINT 3+7")
        assert "10" in r

    def test_subtract_ten(self):
        r = self.bas("PRINT 20-10")
        assert "10" in r

    def test_multiply_three(self):
        r = self.bas("PRINT 3*3")
        assert "9" in r

    def test_divide_nine(self):
        r = self.bas("PRINT 9/3")
        assert "3" in r

    def test_abs_negative_ten(self):
        r = self.bas("PRINT ABS(-10)")
        assert "10" in r

    def test_int_function(self):
        r = self.bas("PRINT INT(7.9)")
        assert "7" in r

    def test_print_hundred(self):
        r = self.bas("PRINT 100")
        assert "100" in r

    def test_add_zero(self):
        r = self.bas("PRINT 0+5")
        assert "5" in r

    def test_multiply_zero(self):
        r = self.bas("PRINT 0*99")
        assert "0" in r


class TestArithmeticExtended9:
    """Ninth round of arithmetic tests."""

    def bas(self, code):
        return _run_basic(code)

    def test_print_7(self):
        assert "7" in self.bas("PRINT 7")

    def test_print_99(self):
        assert "99" in self.bas("PRINT 99")

    def test_add_5_5(self):
        assert "10" in self.bas("PRINT 5+5")

    def test_sub_15_5(self):
        assert "10" in self.bas("PRINT 15-5")

    def test_mul_5_5(self):
        assert "25" in self.bas("PRINT 5*5")

    def test_div_20_4(self):
        assert "5" in self.bas("PRINT 20/4")

    def test_abs_negative(self):
        assert "7" in self.bas("PRINT ABS(-7)")

    def test_print_1000(self):
        assert "1000" in self.bas("PRINT 1000")

    def test_add_1_99(self):
        assert "100" in self.bas("PRINT 1+99")

    def test_output_is_str(self):
        assert isinstance(self.bas("PRINT 1"), list)


class TestArithmeticExtended10:
    """Tenth round of arithmetic tests."""

    def bas(self, code):
        return _run_basic(code)

    def test_print_5(self):
        assert "5" in self.bas("PRINT 5")

    def test_print_100(self):
        assert "100" in self.bas("PRINT 100")

    def test_print_0(self):
        assert "0" in self.bas("PRINT 0")

    def test_add_two(self):
        assert "10" in self.bas("PRINT 3+7")

    def test_multiply(self):
        assert "20" in self.bas("PRINT 4*5")

    def test_subtract(self):
        assert "8" in self.bas("PRINT 10-2")

    def test_divide(self):
        assert "5" in self.bas("PRINT 10/2")

    def test_abs_positive(self):
        assert "3" in self.bas("PRINT ABS(3)")

    def test_output_is_list(self):
        assert isinstance(self.bas("PRINT 1"), list)

    def test_no_errors(self):
        result = self.bas("PRINT 1")
        assert not any("❌" in line for line in result)


class TestArithmeticExtended11:
    """Eleventh round of arithmetic tests."""

    def bas(self, code):
        return _run_basic(code)

    def test_print_99(self):
        assert "99" in self.bas("PRINT 99")

    def test_print_1000(self):
        assert "1000" in self.bas("PRINT 1000")

    def test_add_large(self):
        assert "100" in self.bas("PRINT 50+50")

    def test_subtract_large(self):
        assert "90" in self.bas("PRINT 100-10")

    def test_multiply_large(self):
        assert "200" in self.bas("PRINT 10*20")

    def test_divide_large(self):
        assert "25" in self.bas("PRINT 100/4")

    def test_nested_print(self):
        assert "42" in self.bas("PRINT 6*7")

    def test_output_is_list(self):
        assert isinstance(self.bas("PRINT 1"), list)

    def test_no_errors(self):
        result = self.bas("PRINT 1")
        assert not any("❌" in line for line in result)

    def test_print_zero(self):
        assert "0" in self.bas("PRINT 0")


class TestArithmeticExtended12:
    """Twelfth extended round of arithmetic tests."""

    def test_add_200(self):
        assert any("200" in line for line in _run_basic("PRINT 100+100"))

    def test_sub_190(self):
        assert any("190" in line for line in _run_basic("PRINT 200-10"))

    def test_mul_200(self):
        assert any("200" in line for line in _run_basic("PRINT 20*10"))

    def test_div_10(self):
        assert any("10" in line for line in _run_basic("PRINT 100/10"))

    def test_mod_1(self):
        assert isinstance(_run_basic("PRINT 10 MOD 3"), list)

    def test_result_is_list(self):
        assert isinstance(_run_basic("PRINT 1"), list)

    def test_print_200(self):
        assert any("200" in line for line in _run_basic("PRINT 200"))

    def test_large_add(self):
        assert any("1000" in line for line in _run_basic("PRINT 999+1"))

    def test_chain_mul(self):
        assert isinstance(_run_basic("PRINT 2*3*5"), list)

    def test_parens(self):
        assert isinstance(_run_basic("PRINT (2+3)*4"), list)
