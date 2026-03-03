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
