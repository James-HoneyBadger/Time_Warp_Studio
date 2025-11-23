#!/usr/bin/env python3
"""Test BASIC variable handling"""

import sys

sys.path.insert(0, "/home/james/Time_Warp/platforms/python")

from time_warp.core.interpreter import Interpreter
from time_warp.graphics.turtle_state import TurtleState

# Create interpreter
interp = Interpreter()
turtle = TurtleState()

# Load program
program = """10 REM Input Demonstration
20 PRINT "What is your name?"
30 INPUT NAME$
40 PRINT "Hello, "; NAME$
50 PRINT "How old are you?"
60 INPUT AGE
70 LET NEXTAGE = AGE + 1
80 PRINT "Next year you will be "; NEXTAGE
"""

print("=== Loading program ===")
interp.load_program(program)

print(f"\nProgram lines: {len(interp.program_lines)}")
for i, (line_num, cmd) in enumerate(interp.program_lines):
    print(f"  {i}: [{line_num}] {cmd}")

print("\n=== Simulating execution with manual input ===")

# Execute first few lines
print("\n--- Line 0 (REM) ---")
interp._execute_line(interp.program_lines[0][1], turtle)
interp.current_line += 1

print("\n--- Line 1 (PRINT question) ---")
result = interp._execute_line(interp.program_lines[1][1], turtle)
print(f"Output: {repr(result)}")
print(f"Interpreter output buffer: {interp.output}")
interp.current_line += 1

print("\n--- Line 2 (INPUT NAME$) ---")
result = interp._execute_line(interp.program_lines[2][1], turtle)
print(f"Pending input: {interp.pending_input}")
if interp.pending_input:
    print(f"  Prompt: {interp.pending_input.prompt}")
    print(f"  Var name: {interp.pending_input.var_name}")
    print(f"  Is numeric: {interp.pending_input.is_numeric}")

    # Simulate providing input
    print("\n  Simulating input: 'John'")
    interp.provide_input("John")
    print(f"  String variables: {interp.string_variables}")
    print(f"  Numeric variables: {interp.variables}")

print("\n--- Line 3 (PRINT Hello) ---")
result = interp._execute_line(interp.program_lines[3][1], turtle)
print(f"Output: {repr(result)}")
print(f"Interpreter output buffer: {interp.output}")
interp.current_line += 1

print("\n--- Line 4 (PRINT age question) ---")
result = interp._execute_line(interp.program_lines[4][1], turtle)
print(f"Output: {repr(result)}")
interp.current_line += 1

print("\n--- Line 5 (INPUT AGE) ---")
result = interp._execute_line(interp.program_lines[5][1], turtle)
if interp.pending_input:
    print(f"Pending input: {interp.pending_input}")
    print(f"  Var name: {interp.pending_input.var_name}")

    # Simulate providing input
    print("\n  Simulating input: '25'")
    interp.provide_input("25")
    print(f"  String variables: {interp.string_variables}")
    print(f"  Numeric variables: {interp.variables}")

print("\n--- Line 6 (LET NEXTAGE) ---")
result = interp._execute_line(interp.program_lines[6][1], turtle)
print(f"Output: {repr(result)}")
print(f"Numeric variables: {interp.variables}")
interp.current_line += 1

print("\n--- Line 7 (PRINT next year) ---")
result = interp._execute_line(interp.program_lines[7][1], turtle)
print(f"Output: {repr(result)}")
print(f"Interpreter output buffer: {interp.output}")
