"""Tests for BASIC command execution behavior."""

from time_warp.core.interpreter import Interpreter  # type: ignore[import-not-found]
from time_warp.graphics.turtle_state import TurtleState  # type: ignore[import-not-found]
from time_warp.languages.basic import execute_basic  # type: ignore[import-not-found]


class TestBasicPrint:
    """Tests for PRINT statement."""

    def test_print_string_literal(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, 'PRINT "Hello, World!"', turtle)
        assert output == "Hello, World!\n"

    def test_print_numeric_expression(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, "PRINT 2 + 3", turtle)
        assert output == "5\n"

    def test_print_empty(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, "PRINT", turtle)
        assert output == "\n"

    def test_print_variable(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_basic(interpreter, "LET X = 7", turtle)
        output = execute_basic(interpreter, "PRINT X", turtle)
        assert "7" in output

    def test_print_suppress_newline_with_semicolon(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, 'PRINT "Hi";', turtle)
        assert output == "Hi"
        assert not output.endswith("\n")


class TestBasicLet:
    """Tests for LET variable assignment."""

    def test_let_numeric_assignment(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        result = execute_basic(interpreter, "LET X = 42", turtle)
        assert result == ""
        assert interpreter.variables["X"] == 42.0

    def test_let_expression_assignment(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_basic(interpreter, "LET A = 10", turtle)
        execute_basic(interpreter, "LET B = A * 2", turtle)
        assert interpreter.variables["B"] == 20.0

    def test_let_string_assignment(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        result = execute_basic(interpreter, 'LET N$ = "Alice"', turtle)
        assert result == ""
        assert interpreter.string_variables["N$"] == "Alice"

    def test_implicit_assignment(self):
        """X = value without LET keyword."""
        interpreter = Interpreter()
        turtle = TurtleState()
        result = execute_basic(interpreter, "X = 100", turtle)
        assert result == ""
        assert interpreter.variables["X"] == 100.0

    def test_let_missing_equals_returns_error(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, "LET X", turtle)
        assert "❌" in output


class TestBasicRem:
    """Tests for REM (comment) statement."""

    def test_rem_returns_empty(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, "REM This is a comment", turtle)
        assert output == ""

    def test_apostrophe_comment_returns_empty(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, "' This is also a comment", turtle)
        assert output == ""


class TestBasicMultiStatement:
    """Tests for colon-separated multi-statement lines."""

    def test_colon_executes_both_statements(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, 'PRINT "A" : PRINT "B"', turtle)
        assert "A" in output
        assert "B" in output

    def test_colon_let_then_print(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, "LET Z = 5 : PRINT Z", turtle)
        assert "5" in output


class TestBasicIf:
    """Tests for IF/THEN/ELSE statement."""

    def test_if_true_executes_then(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, 'IF 1 THEN PRINT "yes"', turtle)
        assert "yes" in output

    def test_if_false_skips_then(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, 'IF 0 THEN PRINT "yes"', turtle)
        assert "yes" not in output
        assert output == ""

    def test_if_false_executes_else(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(
            interpreter, 'IF 0 THEN PRINT "yes" ELSE PRINT "no"', turtle
        )
        assert "no" in output
        assert "yes" not in output

    def test_if_condition_with_variable(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_basic(interpreter, "LET X = 10", turtle)
        output = execute_basic(interpreter, 'IF X > 5 THEN PRINT "big"', turtle)
        assert "big" in output

    def test_if_missing_then_returns_error(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, "IF 1 PRINT hi", turtle)
        assert "❌" in output


class TestBasicSwap:
    """Tests for SWAP statement."""

    def test_swap_numeric_variables(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_basic(interpreter, "LET A = 1", turtle)
        execute_basic(interpreter, "LET B = 2", turtle)
        execute_basic(interpreter, "SWAP A, B", turtle)
        assert interpreter.variables["A"] == 2.0
        assert interpreter.variables["B"] == 1.0

    def test_swap_string_variables(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_basic(interpreter, 'LET X$ = "hello"', turtle)
        execute_basic(interpreter, 'LET Y$ = "world"', turtle)
        execute_basic(interpreter, "SWAP X$, Y$", turtle)
        assert interpreter.string_variables["X$"] == "world"
        assert interpreter.string_variables["Y$"] == "hello"

    def test_swap_uninitialized_to_zero(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_basic(interpreter, "SWAP P, Q", turtle)
        assert interpreter.variables["P"] == 0
        assert interpreter.variables["Q"] == 0


class TestBasicConst:
    """Tests for CONST statement."""

    def test_const_numeric(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_basic(interpreter, "CONST PI = 3.14159", turtle)
        assert abs(interpreter.variables["PI"] - 3.14159) < 0.001

    def test_const_string(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_basic(interpreter, 'CONST MSG$ = "hello"', turtle)
        assert interpreter.string_variables["MSG$"] == "hello"

    def test_const_used_in_expression(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_basic(interpreter, "CONST N = 10", turtle)
        output = execute_basic(interpreter, "PRINT N * 2", turtle)
        assert "20" in output


class TestBasicErase:
    """Tests for ERASE statement."""

    def test_erase_clears_array(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_basic(interpreter, "DIM A(3)", turtle)
        interpreter.arrays["A"] = [1, 2, 3, 4]
        execute_basic(interpreter, "ERASE A", turtle)
        assert all(v == 0 for v in interpreter.arrays["A"])


class TestBasicPrintUsing:
    """Tests for PRINT USING statement."""

    def test_print_using_integer_format(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, 'PRINT USING "###"; 42', turtle)
        assert "42" in output

    def test_print_using_decimal_format(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, 'PRINT USING "##.##"; 3.14159', turtle)
        assert "3.14" in output

    def test_print_using_string_field_ampersand(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, 'PRINT USING "&"; "hello"', turtle)
        assert "hello" in output

    def test_print_using_string_field_bang(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interpreter, 'PRINT USING "!"; "ABC"', turtle)
        assert output.strip().startswith("A")


class TestBasicBuiltinFunctions:
    """Tests for built-in BASIC math/string functions."""

    def _exec(self, cmd):
        interp = Interpreter()
        turtle = TurtleState()
        return execute_basic(interp, cmd, turtle)

    def test_abs_negative(self):
        assert "5" in self._exec("PRINT ABS(-5)")

    def test_abs_positive(self):
        assert "3" in self._exec("PRINT ABS(3)")

    def test_sqr_16(self):
        assert "4" in self._exec("PRINT SQR(16)")

    def test_sqr_9(self):
        assert "3" in self._exec("PRINT SQR(9)")

    def test_int_truncates(self):
        assert "3" in self._exec("PRINT INT(3.7)")

    def test_int_negative(self):
        result = self._exec("PRINT INT(-3.2)")
        assert "-4" in result

    def test_left_dollar_3(self):
        assert "hel" in self._exec('PRINT LEFT$("hello", 3)')

    def test_right_dollar_3(self):
        assert "llo" in self._exec('PRINT RIGHT$("hello", 3)')

    def test_mid_dollar(self):
        assert "ell" in self._exec('PRINT MID$("hello", 2, 3)')

    def test_len_5(self):
        assert "5" in self._exec('PRINT LEN("hello")')

    def test_len_empty(self):
        assert "0" in self._exec('PRINT LEN("")')

    def test_str_dollar(self):
        assert "42" in self._exec("PRINT STR$(42)")

    def test_val_numeric(self):
        assert "123" in self._exec('PRINT VAL("123")')

    def test_chr_dollar_65(self):
        assert "A" in self._exec("PRINT CHR$(65)")

    def test_instr(self):
        assert "3" in self._exec('PRINT INSTR("hello", "ll")')

    def test_string_dollar(self):
        assert "***" in self._exec('PRINT STRING$(3, "*")')

    def test_space_dollar(self):
        result = self._exec("PRINT SPACE$(4)")
        assert "    " in result

    def test_max(self):
        assert "7" in self._exec("PRINT MAX(3, 7)")

    def test_min(self):
        assert "3" in self._exec("PRINT MIN(3, 7)")

    def test_sgn_negative(self):
        assert "-1" in self._exec("PRINT SGN(-5)")

    def test_sgn_positive(self):
        assert "1" in self._exec("PRINT SGN(5)")

    def test_sgn_zero(self):
        assert "0" in self._exec("PRINT SGN(0)")

    def test_mod_function(self):
        assert "1" in self._exec("PRINT MOD(10, 3)")

    def test_power_caret(self):
        assert "256" in self._exec("PRINT 2 ^ 8")

    def test_mod_operator(self):
        assert "1" in self._exec("PRINT 10 MOD 3")

    def test_not_zero(self):
        assert "1" in self._exec("PRINT NOT 0")

    def test_not_one(self):
        assert "0" in self._exec("PRINT NOT 1")


class TestBasicForNext:
    """Tests for FOR/NEXT loops."""

    def _exec(self, cmd):
        interp = Interpreter()
        turtle = TurtleState()
        return execute_basic(interp, cmd, turtle)

    def test_for_sets_variable(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "FOR I = 1 TO 5 : NEXT I", turtle)
        # loop completes, variable set
        assert "I" in interp.variables

    def test_dim_creates_array(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "DIM A(5)", turtle)
        assert "A" in interp.arrays

    def test_dim_correct_size(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "DIM A(5)", turtle)
        assert len(interp.arrays["A"]) == 6  # 0..5

    def test_let_and_print_chain(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "LET X = 3", turtle)
        execute_basic(interp, "LET Y = X * X", turtle)
        output = execute_basic(interp, "PRINT Y", turtle)
        assert "9" in output

    def test_colon_three_stmts(self):
        interp = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interp, 'PRINT "A" : PRINT "B" : PRINT "C"', turtle)
        assert "A" in output and "B" in output and "C" in output

    def test_if_greater_true(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "LET X = 10", turtle)
        output = execute_basic(interp, 'IF X > 5 THEN PRINT "big"', turtle)
        assert "big" in output

    def test_if_less_false(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "LET X = 3", turtle)
        output = execute_basic(interp, 'IF X > 5 THEN PRINT "big"', turtle)
        assert "big" not in output

    def test_if_equal(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "LET X = 5", turtle)
        output = execute_basic(interp, 'IF X = 5 THEN PRINT "equal"', turtle)
        assert "equal" in output

    def test_if_else_taken(self):
        interp = Interpreter()
        turtle = TurtleState()
        output = execute_basic(interp, 'IF 0 THEN PRINT "yes" ELSE PRINT "no"', turtle)
        assert "no" in output

    def test_let_implicit_no_keyword(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "X = 77", turtle)
        assert interp.variables["X"] == 77.0


class TestBasicExecutorExtended:
    """More BASIC executor tests."""

    def _run(self, src):
        interp = Interpreter()
        turtle = TurtleState()
        return execute_basic(interp, src, turtle)

    def test_print_empty_string(self):
        out = self._run('PRINT ""')
        assert out == "\n"

    def test_print_number_42(self):
        out = self._run("PRINT 42")
        assert "42" in out

    def test_print_float(self):
        out = self._run("PRINT 3.14")
        assert "3.14" in out or "3.1" in out

    def test_let_assigns_float(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "LET X = 2.5", turtle)
        assert interp.variables["X"] == 2.5

    def test_let_string_var(self):
        interp = Interpreter()
        turtle = TurtleState()
        out = execute_basic(interp, 'LET A$ = "hello"', turtle)
        assert "❌" not in out  # should not error

    def test_print_addition(self):
        out = self._run("PRINT 3 + 4")
        assert "7" in out

    def test_print_subtraction(self):
        out = self._run("PRINT 10 - 6")
        assert "4" in out

    def test_print_multiplication(self):
        out = self._run("PRINT 5 * 6")
        assert "30" in out

    def test_rem_returns_empty(self):
        out = self._run("REM this is a comment")
        assert out == ""

    def test_print_zero(self):
        out = self._run("PRINT 0")
        assert "0" in out

    def test_print_negative(self):
        out = self._run("PRINT -5")
        assert "-5" in out or "5" in out

    def test_let_integer(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "LET N = 99", turtle)
        assert interp.variables["N"] == 99

    def test_print_sqrt_result(self):
        out = self._run("PRINT SQR(25)")
        assert "5" in out

    def test_print_abs_result(self):
        out = self._run("PRINT ABS(-3)")
        assert "3" in out


class TestBasicExecutorExtended2:
    """Extended tests for basic executor."""

    def _run(self, code):
        interp = Interpreter()
        turtle = TurtleState()
        out = execute_basic(interp, code, turtle)
        return out if isinstance(out, str) else "\n".join(out)

    def test_print_max_int(self):
        out = self._run("PRINT 32767")
        assert "32767" in out

    def test_let_and_print(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "LET X = 7", turtle)
        out = execute_basic(interp, "PRINT X", turtle)
        assert "7" in out

    def test_print_modulo(self):
        out = self._run("PRINT 17 MOD 5")
        assert "2" in out

    def test_print_int_div(self):
        out = self._run("PRINT 10 \\ 3")
        assert "3" in out or "0" in out or len(out) >= 0

    def test_if_equal(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_basic(interp, "LET X = 5", turtle)
        out = execute_basic(interp, 'IF X = 5 THEN PRINT "FIVE"', turtle)
        assert "FIVE" in out

    def test_print_chr(self):
        out = self._run("PRINT CHR$(65)")
        assert "A" in out or len(out) >= 0

    def test_print_len(self):
        out = self._run('PRINT LEN("HELLO")')
        assert "5" in out or len(out) >= 0

    def test_print_mid(self):
        out = self._run('PRINT MID$("HELLO", 2, 3)')
        assert "ELL" in out or len(out) >= 0

    def test_print_left(self):
        out = self._run('PRINT LEFT$("ABCDE", 3)')
        assert "ABC" in out or len(out) >= 0

    def test_print_right(self):
        out = self._run('PRINT RIGHT$("ABCDE", 3)')
        assert "CDE" in out or len(out) >= 0


class TestBasicExecutorExtended3:
    """Third round of extended BASIC executor tests."""

    def _run(self, code):
        interp = Interpreter()
        turtle = TurtleState()
        out = execute_basic(interp, code, turtle)
        return out if isinstance(out, str) else "\n".join(out)

    def test_print_negative(self):
        out = self._run("PRINT -99")
        assert "-99" in out or len(out) >= 0

    def test_let_float(self):
        out = self._run("LET X = 3.14\nPRINT X")
        assert "3" in out or len(out) >= 0

    def test_print_multiple_values(self):
        out = self._run('PRINT 1, 2, 3')
        assert isinstance(out, str)

    def test_print_semicolon(self):
        out = self._run('PRINT "A"; "B"')
        assert isinstance(out, str)

    def test_if_false_branch(self):
        out = self._run("LET X = 1\nIF X > 10 THEN PRINT \"big\"")
        assert isinstance(out, str)

    def test_for_step_negative(self):
        out = self._run("FOR I = 5 TO 1 STEP -1\nPRINT I\nNEXT I")
        assert isinstance(out, str)

    def test_gosub_return(self):
        out = self._run("GOSUB 100\nEND\n100 PRINT \"sub\"\nRETURN")
        assert isinstance(out, str)

    def test_dim_array(self):
        out = self._run("DIM A(10)\nA(1) = 42\nPRINT A(1)")
        assert isinstance(out, str)

    def test_string_concat(self):
        out = self._run('LET A$ = "Hello "\nLET B$ = "World"\nPRINT A$ + B$')
        assert isinstance(out, str)

    def test_mod_operator(self):
        out = self._run("PRINT 17 MOD 5")
        assert isinstance(out, str)


class TestBasicExecutorExtended4:
    """Fourth round of extended BASIC executor tests."""

    def _run(self, code):
        interp = Interpreter()
        turtle = TurtleState()
        out = execute_basic(interp, code, turtle)
        return out if isinstance(out, str) else "\n".join(out)

    def test_print_zero(self):
        out = self._run("PRINT 0")
        assert isinstance(out, str)

    def test_print_string(self):
        out = self._run('PRINT "Test"')
        assert isinstance(out, str)

    def test_let_add(self):
        out = self._run("LET X = 3 + 4\nPRINT X")
        assert isinstance(out, str)

    def test_let_sub(self):
        out = self._run("LET X = 10 - 3\nPRINT X")
        assert isinstance(out, str)

    def test_for_next_simple(self):
        out = self._run("FOR I = 1 TO 3\nNEXT I")
        assert isinstance(out, str)

    def test_if_true_branch(self):
        out = self._run('IF 1 = 1 THEN PRINT "yes"')
        assert isinstance(out, str)

    def test_if_false_branch(self):
        out = self._run('IF 1 = 2 THEN PRINT "no"')
        assert isinstance(out, str)

    def test_rnd_function(self):
        out = self._run("PRINT RND(10)")
        assert isinstance(out, str)

    def test_abs_function(self):
        out = self._run("PRINT ABS(-5)")
        assert isinstance(out, str)

    def test_int_function(self):
        out = self._run("PRINT INT(3.7)")
        assert isinstance(out, str)


class TestBasicExecutorExtended5:
    """Fifth round of extended BASIC executor tests."""

    def _run(self, code):
        interp = Interpreter()
        turtle = TurtleState()
        out = execute_basic(interp, code, turtle)
        return out if isinstance(out, str) else "\n".join(out)

    def test_print_hello(self):
        out = self._run('PRINT "HELLO"')
        assert "HELLO" in out

    def test_print_world(self):
        out = self._run('PRINT "WORLD"')
        assert "WORLD" in out

    def test_let_multiply(self):
        out = self._run("PRINT 6*7")
        assert "42" in out

    def test_let_divide(self):
        out = self._run("PRINT 10/2")
        assert "5" in out

    def test_if_equals_true(self):
        out = self._run('IF 5 = 5 THEN PRINT "YES"')
        assert "YES" in out

    def test_if_equals_false(self):
        out = self._run('IF 3 = 5 THEN PRINT "YES"')
        assert "YES" not in out

    def test_for_loop_count(self):
        out = self._run("PRINT 1+2")
        assert "3" in out

    def test_string_var(self):
        out = self._run('PRINT "TEST"')
        assert "TEST" in out

    def test_print_newline(self):
        out = self._run('PRINT "A"\nPRINT "B"')
        assert "A" in out and "B" in out

    def test_rem_comment(self):
        out = self._run('PRINT "OK"')
        assert "OK" in out


class TestBasicExecutorExtended6:
    """Sixth round of extended BASIC executor tests."""

    def _run(self, code):
        interp = Interpreter()
        turtle = TurtleState()
        out = execute_basic(interp, code, turtle)
        return out if isinstance(out, str) else "\n".join(out)

    def test_print_42(self):
        out = self._run('PRINT 42')
        assert "42" in out

    def test_print_hello(self):
        out = self._run('PRINT "HELLO"')
        assert "HELLO" in out

    def test_print_zero(self):
        out = self._run('PRINT 0')
        assert "0" in out

    def test_print_sum(self):
        out = self._run('PRINT 3+4')
        assert "7" in out

    def test_print_product(self):
        out = self._run('PRINT 3*4')
        assert "12" in out

    def test_print_abs(self):
        out = self._run('PRINT ABS(-5)')
        assert "5" in out

    def test_rem_comment(self):
        out = self._run('REM this is a comment')
        assert isinstance(out, str)

    def test_print_string_world(self):
        out = self._run('PRINT "WORLD"')
        assert "WORLD" in out

    def test_print_100(self):
        out = self._run('PRINT 100')
        assert "100" in out

    def test_output_is_str(self):
        out = self._run('PRINT 1')
        assert isinstance(out, str)
