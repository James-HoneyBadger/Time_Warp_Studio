#!/usr/bin/env python3
"""BASIC variable and INPUT statement tests."""

import sys
from pathlib import Path

# pylint: disable=protected-access,wrong-import-position,import-error,no-name-in-module
p = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(p))

from time_warp.core.interpreter import Interpreter  # noqa: E402

# Load program used by the demo/test
PROGRAM = """10 REM Input Demonstration
20 PRINT "What is your name?"
30 INPUT NAME$
40 PRINT "Hello, "; NAME$
50 PRINT "How old are you?"
60 INPUT AGE
70 LET NEXTAGE = AGE + 1
80 PRINT "Next year you will be "; NEXTAGE
"""


def test_program_loaded():
    """Smoke test: loading the demo program produces program lines."""

    itp = Interpreter()
    itp.load_program(PROGRAM)
    assert len(itp.program_lines) == 8


if __name__ == "__main__":
    # Demo mode: exercise the program when executed directly.
    # Import inside __main__ so pytest import-time static checks don't require
    # the GUI/graphics library at collection time.
    # The graphics module is imported from the in-tree package; silence
    # static import errors from external analyzers.
    # pylint: disable=import-error
    from time_warp.graphics import turtle_state as _ts

    TurtleState = _ts.TurtleState

    interp = Interpreter()
    turtle = TurtleState()

    interp.load_program(PROGRAM)

    print(f"\nProgram lines: {len(interp.program_lines)}")
    for i, (line_num, cmd) in enumerate(interp.program_lines):
        print(f"  {i}: [{line_num}] {cmd}")

    print("\n=== Simulating execution with manual input ===")

    # Execute first few lines (demo — not run during pytest import)
    interp._execute_line(interp.program_lines[0][1], turtle)
    interp.current_line += 1

    result = interp._execute_line(interp.program_lines[1][1], turtle)
    print(f"Output: {repr(result)}")
    print(f"Interpreter output buffer: {interp.output}")
    interp.current_line += 1

    result = interp._execute_line(interp.program_lines[2][1], turtle)
    print(f"Pending input: {interp.pending_input}")
    if interp.pending_input:
        # Simulate providing input
        interp.provide_input("John")
        print(f"  String variables: {interp.string_variables}")
        print(f"  Numeric variables: {interp.variables}")

    result = interp._execute_line(interp.program_lines[3][1], turtle)
    print(f"Output: {repr(result)}")
    print(f"Interpreter output buffer: {interp.output}")
    interp.current_line += 1

    result = interp._execute_line(interp.program_lines[4][1], turtle)
    print(f"Output: {repr(result)}")
    interp.current_line += 1

    result = interp._execute_line(interp.program_lines[5][1], turtle)
    if interp.pending_input:
        interp.provide_input("25")
        print(f"  String variables: {interp.string_variables}")
        print(f"  Numeric variables: {interp.variables}")

    result = interp._execute_line(interp.program_lines[6][1], turtle)
    print(f"Output: {repr(result)}")
    print(f"Numeric variables: {interp.variables}")
    interp.current_line += 1

    result = interp._execute_line(interp.program_lines[7][1], turtle)
    print(f"Output: {repr(result)}")
    print(f"Interpreter output buffer: {interp.output}")
