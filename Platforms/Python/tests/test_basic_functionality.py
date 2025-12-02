#!/usr/bin/env python3
"""
Core interpreter smoke tests for PILOT, BASIC, and Logo.

Validates language execution, expression evaluation, and error hint system
without requiring a GUI. Run directly or via pytest.
"""

import os
import sys
import traceback

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)))

# pylint: disable=wrong-import-position,import-error,no-name-in-module
from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState
from time_warp.utils.error_hints import check_syntax_mistakes, suggest_command
from time_warp.utils.expression_evaluator import ExpressionEvaluator


def test_pilot():
    """PILOT: text output, compute, and variable interpolation."""
    interp = Interpreter()
    turtle = TurtleState()

    program = """
T:Hello from PILOT
C:X = 5 + 3
U:X
T:Result is *X*
E:
"""
    interp.load_program(program, language=Language.PILOT)
    output = interp.execute(turtle)
    assert output, "PILOT should produce output"
    print("✅ PILOT test passed")


def test_basic():
    """BASIC: PRINT, LET, and line-numbered execution."""
    interp = Interpreter()
    turtle = TurtleState()

    program = """
10 PRINT "Hello from BASIC"
20 LET X = 10
30 PRINT "X = "; X
40 END
"""
    interp.load_program(program)
    output = interp.execute(turtle)
    assert output, "BASIC should produce output"
    print("✅ BASIC test passed")


def test_logo():
    """Logo: turtle movement and HOME command."""
    interp = Interpreter()
    turtle = TurtleState()

    program = """
PRINT "Hello from Logo"
FORWARD 50
RIGHT 90
FORWARD 50
HOME
"""
    interp.load_program(program, language=Language.LOGO)
    interp.execute(turtle)
    assert turtle.x == 0 and turtle.y == 0, "HOME should return to origin"
    print("✅ Logo test passed")


def test_expression_evaluator():
    """Math expressions with variables and functions."""
    evaluator = ExpressionEvaluator({"X": 5, "Y": 3})

    tests = [
        ("2 + 3", 5.0),
        ("X * 2", 10.0),
        ("X + Y", 8.0),
        ("2 ^ 3", 8.0),
        ("sqrt(16)", 4.0),
        ("sin(0)", 0.0),
    ]

    for expr, expected in tests:
        result = evaluator.evaluate(expr)
        assert abs(result - expected) < 0.001, f"Failed: {expr}"

    print("✅ Expression evaluator test passed")


def test_error_hints():
    """Typo suggestions and syntax error detection."""
    suggestions = [("PRITN", "PRINT"), ("FORWAD", "FORWARD"), ("GOTP", "GOTO")]
    for typo, expected in suggestions:
        assert suggest_command(typo) == expected

    syntax_tests = [
        ('PRINT "hello', "Unclosed string literal"),
        ("IF X > 5", "IF statement missing THEN"),
        ("FOR I = 1", "FOR loop missing TO"),
    ]
    for code, expected_msg in syntax_tests:
        result = check_syntax_mistakes(code)
        assert result and expected_msg.lower() in result.lower()

    print("✅ Error hints test passed")


def main():
    """Run all smoke tests."""
    print("=" * 60)
    print("Time Warp IDE - Core Interpreter Tests")
    print("=" * 60)

    try:
        test_pilot()
        test_basic()
        test_logo()
        test_expression_evaluator()
        test_error_hints()
        print("\n✅ ALL TESTS PASSED!")
        return 0
    except Exception as e:  # pylint: disable=broad-exception-caught
        print(f"\n❌ TEST FAILED: {e}")
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
