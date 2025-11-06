#!/usr/bin/env python3
"""
Simple test script to verify Time Warp Python port basic functionality.
Tests PILOT, BASIC, and Logo interpreters.
"""

import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)))

from time_warp.core.interpreter import Interpreter
from time_warp.graphics.turtle_state import TurtleState


def test_pilot():
    """Test PILOT language execution"""
    print("Testing PILOT...")
    
    interp = Interpreter()
    turtle = TurtleState()
    
    program = """
T:Hello from PILOT
C:X = 5 + 3
U:X
T:Result is *X*
E:
"""
    
    interp.load_program(program)
    output = interp.execute(turtle)
    
    print(f"Output: {output}")
    assert len(output) > 0, "PILOT should produce output"
    print("✅ PILOT test passed\n")


def test_basic():
    """Test BASIC language execution"""
    print("Testing BASIC...")
    
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
    
    print(f"Output: {output}")
    assert len(output) > 0, "BASIC should produce output"
    print("✅ BASIC test passed\n")


def test_logo():
    """Test Logo language execution"""
    print("Testing Logo...")
    
    interp = Interpreter()
    turtle = TurtleState()
    
    program = """
PRINT "Hello from Logo"
FORWARD 50
RIGHT 90
FORWARD 50
HOME
"""
    
    interp.load_program(program)
    output = interp.execute(turtle)
    
    print(f"Output: {output}")
    print(f"Turtle position: ({turtle.x}, {turtle.y})")
    print(f"Turtle lines drawn: {len(turtle.lines)}")
    assert turtle.x == 0 and turtle.y == 0, "HOME should return to origin"
    print("✅ Logo test passed\n")


def test_expression_evaluator():
    """Test expression evaluator"""
    print("Testing expression evaluator...")
    
    from time_warp.utils.expression_evaluator import ExpressionEvaluator
    
    evaluator = ExpressionEvaluator({'X': 5, 'Y': 3})
    
    tests = [
        ('2 + 3', 5.0),
        ('X * 2', 10.0),
        ('X + Y', 8.0),
        ('2 ^ 3', 8.0),
        ('sqrt(16)', 4.0),
        ('sin(0)', 0.0),
    ]
    
    for expr, expected in tests:
        result = evaluator.evaluate(expr)
        print(f"  {expr} = {result} (expected {expected})")
        assert abs(result - expected) < 0.001, f"Failed: {expr}"
    
    print("✅ Expression evaluator test passed\n")


def test_error_hints():
    """Test error hint system"""
    print("Testing error hints...")
    
    from time_warp.utils.error_hints import suggest_command, check_syntax_mistakes
    
    # Test typo suggestions
    suggestions = [
        ('PRITN', 'PRINT'),
        ('FORWAD', 'FORWARD'),
        ('GOTP', 'GOTO'),
    ]
    
    for typo, expected in suggestions:
        result = suggest_command(typo)
        print(f"  {typo} -> {result}")
        assert result == expected, f"Expected {expected}, got {result}"
    
    # Test syntax checking
    syntax_tests = [
        ('PRINT "hello', "Unclosed string literal"),
        ('IF X > 5', "IF statement missing THEN"),
        ('FOR I = 1', "FOR loop missing TO"),
    ]
    
    for code, expected_msg in syntax_tests:
        result = check_syntax_mistakes(code)
        print(f"  '{code}' -> {result}")
        assert result and expected_msg.lower() in result.lower(), \
            f"Expected message containing '{expected_msg}'"
    
    print("✅ Error hints test passed\n")


def main():
    """Run all tests"""
    print("=" * 60)
    print("Time Warp IDE - Python Port - Basic Functionality Tests")
    print("=" * 60)
    print()
    
    try:
        test_pilot()
        test_basic()
        test_logo()
        test_expression_evaluator()
        test_error_hints()
        
        print("=" * 60)
        print("✅ ALL TESTS PASSED!")
        print("=" * 60)
        return 0
        
    except Exception as e:
        print(f"\n❌ TEST FAILED: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())
