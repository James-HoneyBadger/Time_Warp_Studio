#!/usr/bin/env python
"""Test script to verify 03_arithmetic.bas output."""

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState

# Simple test first
print("Testing simple BASIC:")
interp = Interpreter(Language.BASIC)
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
