#!/usr/bin/env python3
"""Simple headless runner to exercise the Interpreter and TurtleState.

Usage:
  ./run_interpreter_example.py ../../Examples/basic/hello_world.bas
"""
import sys
from pathlib import Path

from time_warp.core.interpreter import Interpreter
from time_warp.graphics.turtle_state import TurtleState


def main():
    if len(sys.argv) < 2:
        print("Usage: run_interpreter_example.py <program-file>")
        sys.exit(2)

    file_path = Path(sys.argv[1])
    if not file_path.exists():
        print(f"File not found: {file_path}")
        sys.exit(2)

    code = file_path.read_text(encoding="utf-8")

    interp = Interpreter()
    interp.reset()
    interp.load_program(code)

    turtle = TurtleState()

    # Capture outputs via the interpreter's output list or callback
    outputs = interp.execute(turtle)

    print("=== Execution Output ===")
    if outputs:
        for line in outputs:
            print(line)
    else:
        print("(no text output)")

    # Show turtle lines summary
    print("\n=== Turtle State ===")
    print(f"Position: ({turtle.x:.2f}, {turtle.y:.2f}), heading={turtle.heading:.2f}")
    print(f"Lines drawn: {len(turtle.lines)}")
    for i, l in enumerate(turtle.lines[:10]):
        print(
            f"  {i}: ({l.start_x:.1f},{l.start_y:.1f}) -> ({l.end_x:.1f},{l.end_y:.1f}), color={l.color}, width={l.width}"
        )


if __name__ == "__main__":
    main()
