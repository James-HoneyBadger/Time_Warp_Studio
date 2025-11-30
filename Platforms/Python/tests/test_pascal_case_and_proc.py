"""Pascal tests for case statements and nested procedures.

These tests run in-tree and need the Platforms/Python package on sys.path
so they intentionally add the in-repo package path at runtime.
"""

import os
import sys

# Add platform path so tests import the in-tree package correctly.
sys.path.append(os.path.join(os.getcwd(), "platforms/python"))

# Imports below must remain after sys.path modification so the in-repo
# package is importable during test collection.
# pylint: disable=wrong-import-position
# These imports require the in-repo package; silence import-error warnings.
# pylint: disable=import-error
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


def run(code: str) -> str:
    """Helper to load and execute Pascal source and return output as string."""
    interp = Interpreter()
    interp.load_program(code, Language.PASCAL)
    out = interp.execute(TurtleState())
    return "".join(out)


def test_pascal_case_of_basic():
    """Case statement selects the correct branch and outputs expected text."""
    code = """
    program demo;
    var x: integer;
    begin
      x := 2;
      case x of
        1:
        begin
          writeln('A');
        end;
        2:
        begin
          writeln('B');
        end;
      else:
        begin
          writeln('Z');
        end;
      end
    end.
    """
    assert run(code) == "B\n"


def test_pascal_case_single_line_branch():
    """Handle case branches written as single-line statements."""
    code = """
    program demo;
    var x: integer;
    begin
      x := 1;
      case x of
        1:
          writeln('A');
        2:
          writeln('B');
      else:
          writeln('Z');
      end
    end.
    """
    assert run(code) == "A\n"
